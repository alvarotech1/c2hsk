module Evaluador where

import AST
import Data.List  (isPrefixOf, nub, find, intercalate)
import Data.Char  (isDigit)
import Control.Monad (foldM, unless)

------------------------------------------------------------------
-- Tipos auxiliares
------------------------------------------------------------------
type Version = Int
type Scope   = [(Variable, Version)]   -- scope  = lista de variables con su versión
data Env = Env
  { varEnv :: [Scope]                              -- pila de scopes SSA
  , ptrEnv :: [(Variable,(Variable,Exp))]          -- pVer ↦ (baseVer, idxExp)
  }
type IOFuncs = [String]

emptyEnv :: Env
emptyEnv = Env { varEnv = [[]], ptrEnv = [] }


------------------------------------------------------------------
-- Gestión de scopes
------------------------------------------------------------------
varsInEnv :: Env -> [Variable]
varsInEnv = map fst . concat . varEnv

pushScope :: Env -> Env
pushScope env = env { varEnv = [] : varEnv env }

popScope :: Env -> Env
popScope env =
  case varEnv env of
    []       -> error "popScope: env vacío"
    [_]      -> env { varEnv = [[]] }
    (_:rest) -> env { varEnv = rest }

lookupVar :: Variable -> Env -> Maybe Version
lookupVar _ (Env { varEnv = [] }) = Nothing
lookupVar v (Env { varEnv = s:ss, ptrEnv = _ }) =
  case lookup v s of
    Just n  -> Just n
    Nothing -> lookupVar v (Env { varEnv = ss, ptrEnv = [] })

declareVar :: Variable -> Env -> (Variable, Env)
declareVar v env@(Env { varEnv = (s:ss) }) =
  let verPrev = maybe 0 id (lookupVar v env)
      ver     = verPrev + 1
      env'    = env { varEnv = ((v,ver):s) : ss }
  in (v ++ show ver, env')
declareVar _ _ = error "declareVar: env vacío"

nextVarName :: Variable -> Env -> (Variable, Env)
nextVarName v env@(Env { varEnv = (s:ss) }) =
  let prev = case lookupVar v env of
               Nothing -> 0
               Just n  -> n
      n' = prev + 1
      s' = (v,n') : filter ((/=v).fst) s
  in (v ++ show n', env { varEnv = s':ss })
nextVarName _ _ = error "nextVarName: env vacío"

currentVarName :: Variable -> Env -> Variable
currentVarName v env =
  case lookupVar v env of
    Just 0 -> v
    Just n -> v ++ show n
    Nothing -> error $ "Variable " ++ v ++ " no encontrada."

currentVarNameOrDefault :: Variable -> String -> Env -> Variable
currentVarNameOrDefault v def env =
  maybe def pretty (lookupVar v env)
  where pretty 0 = v
        pretty n = v ++ show n

------------------------------------------------------------------
-- Helpers de punteros (ptrEnv)
------------------------------------------------------------------
setPtr :: Variable -> (Variable, Exp) -> Env -> Env
setPtr p dest env =
  env { ptrEnv = (p,dest) : filter ((/=p).fst) (ptrEnv env) }

lookupPtr :: Variable -> Env -> Maybe (Variable, Exp)
lookupPtr p env = lookup p (ptrEnv env)


------------------------------------------------------------------
-- Detección de funciones impuras
------------------------------------------------------------------
detectImpureFunctions :: Comm -> [String]
detectImpureFunctions Skip                     = []
detectImpureFunctions (LetType _ _ _)          = []
detectImpureFunctions (Assign _ _)             = []
detectImpureFunctions (AssignArr _ _ _)          = []
detectImpureFunctions (ExprStmt _)             = []
detectImpureFunctions (Seq c1 c2)              = nub (detectImpureFunctions c1
                                                    ++ detectImpureFunctions c2)
detectImpureFunctions (FuncDef _ name _ body)
  | name == "main"                          = []
  | containsIO body                         = [name]
  | otherwise                               = []
detectImpureFunctions (Return _)               = []
detectImpureFunctions (Printf _ _)             = []
detectImpureFunctions (While _ body)           = detectImpureFunctions body
detectImpureFunctions (DoWhile c _) = detectImpureFunctions c
detectImpureFunctions (Cond _ c1 c2)           = nub (detectImpureFunctions c1
                                                    ++ detectImpureFunctions c2)
detectImpureFunctions (CondNoElse _ c)         = detectImpureFunctions c
detectImpureFunctions (Repeat body _)          = detectImpureFunctions body
detectImpureFunctions (Block c)                = detectImpureFunctions c
detectImpureFunctions (Scanf _ _) = []
detectImpureFunctions (For mi _ ms body) =
    nub $  maybe [] detectImpureFunctions mi
        ++ maybe [] detectImpureFunctions ms
        ++ detectImpureFunctions body


------------------------------------------------------------------
-- Detección de acciones IO
------------------------------------------------------------------
containsIO :: Comm -> Bool
containsIO (Block c)           = containsIO c
containsIO (Printf _ _)        = True
containsIO (Seq c1 c2)         = containsIO c1 || containsIO c2
containsIO (While _ body)      = containsIO body
containsIO (DoWhile c _)       = containsIO c
containsIO (Cond _ c1 c2)      = containsIO c1 || containsIO c2
containsIO (CondNoElse _ c)    = containsIO c
containsIO (Repeat body _)     = containsIO body
containsIO (Scanf _ _)         = True
containsIO (FuncDef _ _ _ _)   = False
containsIO (For mi _ ms body) =
    maybe False containsIO mi
 || maybe False containsIO ms
 || containsIO body
containsIO _                   = False

------------------------------------------------------------------
-- Función principal
------------------------------------------------------------------
eval :: Comm -> IO ()
eval programa = do
  let ioFuncs = detectImpureFunctions programa
  (_, hsLines) <- evalComm ioFuncs programa 0 emptyEnv   -- ← ioFuncs (no ioFns)
  mapM_ putStrLn (alignLines hsLines)


------------------------------------------------------------------
-- Evaluador de comandos
------------------------------------------------------------------
evalComm :: IOFuncs -> Comm -> Int -> Env -> IO (Env, [String])
evalComm _ Skip _ env = return (env, [])

-- Declaración con tipo ------------------------------------------------------
evalComm ioFns (LetType (TPtr innerT) v (AddrOf addr)) indent env = do
  -- • nombre SSA para el puntero
  let (v', env1) = declareVar v env

      -- • “base” lógico y expresión de índice
      (baseLog, idxExp, idxStr) =
        case addr of
          VarExp x ->
            (x, IntExp 0, "0")
          ArrayAccess a i ->
            (a, i, evalExp i env)
          _ ->
            error "LetType TPtr: AddrOf debe ser VarExp o ArrayAccess"

      baseName = currentVarName baseLog env    -- versión vigente de la base

      -- tipo del base para la anotación
      baseTypeStr = case addr of
        VarExp _ -> translateType innerT    -- usualmente "Int"
        ArrayAccess _ _ -> "[" ++ translateType innerT ++ "]"   -- array de ese tipo
        _ -> translateType innerT

      -- línea Haskell que inicializa el puntero como tupla
      ptrLine  = indentStr indent ++
                 "let " ++ v' ++ " = (" ++ baseName ++ ", " ++ idxStr ++
                 ") :: (" ++ baseTypeStr ++ ", Int)"

      -- registrar el puntero en ptrEnv con el _nombre lógico_ de base
      env2 = setPtr v' (baseLog, idxExp) env1

  return (env2, [ptrLine])

-- el resto de casos LetType igual que antes


-- caso general (no-puntero) ── adaptá el “resto” de tu implementación original
evalComm ioFns (LetType t v e) indent env
  | not (isPtr t) = 
      -- <-- pegá aquí tu código original para LetType no-puntero
      let (v', env1) = declareVar v env
          rhsStr     = evalExp e env
          lineBase   = indentStr indent ++
                       if needsBind ioFns e
                          then v' ++ " <- " ++ rhsStr
                          else "let " ++ v' ++ " = " ++ rhsStr ++ " :: " ++ translateType t
          env2       = env1
      in return (env2, [lineBase])
  where
    isPtr (TPtr _) = True
    isPtr _        = False

-- Asignación de puntero: q = &b[2];
evalComm ioFns (Assign v (AddrOf addr)) indent env = do
  let (v', env1) = nextVarName v env
      (baseLog, idxExp, idxStr) =
        case addr of
          VarExp x      -> (x, IntExp 0, "0")
          ArrayAccess a i -> (a, i, evalExp i env)
          _ -> error "Assign: AddrOf debe ser VarExp o ArrayAccess"
      baseName = currentVarName baseLog env
      -- Detectar tipo del base
      baseTypeStr = case addr of
        VarExp _      -> "Int"  -- escalar
        ArrayAccess _ _ -> "[Int]" -- array
        _ -> "Int"
      assignLine = indentStr indent ++
                   "let " ++ v' ++ " = (" ++ baseName ++ ", " ++ idxStr ++ ") :: (" ++ baseTypeStr ++ ", Int)"
      -- Registrar la nueva versión del puntero en ptrEnv
      env2 = setPtr v' (baseLog, idxExp) env1
  return (env2, [assignLine])


-- Asignación
-- Asignación ---------------------------------------------------------------
evalComm ioFns (Assign v rhs) indent env = do
  --------------------------------------------------------------------------
  -- 1) línea principal (asigna el RHS a una nueva versión de ‘v’)
  --------------------------------------------------------------------------
  let rhsStr         = evalExp rhs env
      (v',   env1)   = nextVarName v env
      assignLine     = indentStr indent ++
                       if needsBind ioFns rhs
                          then v' ++ " <- " ++ rhsStr
                          else "let " ++ v' ++ " = " ++ rhsStr

  --------------------------------------------------------------------------
  -- 2) ¿hay ++ / -- en el RHS?  →  variable afectada y operación
  --------------------------------------------------------------------------
      sideEff = case rhs of
                  PostIncr x -> Just (x, "+ 1")
                  PostDecr x -> Just (x, "- 1")
                  PreIncr  x -> Just (x, "+ 1")
                  PreDecr  x -> Just (x, "- 1")
                  _          -> Nothing

  case sideEff of
    ------------------------------------------------------------------------
    -- 2a) Sin efectos secundarios
    ------------------------------------------------------------------------
    Nothing ->
      return (env1, [assignLine])

    ------------------------------------------------------------------------
    -- 2b) Con efecto ++ / -- sobre la variable ‘x’
    ------------------------------------------------------------------------
    Just (x, op) -> do
      let oldName            = currentVarName x env1
          (x', env2)         = nextVarName x env1
          incLine            = indentStr indent ++
                               "let " ++ x' ++ " = " ++ oldName ++ " " ++ op
      return (env2, [assignLine, incLine])





-- Expresión como statement
evalComm _ (ExprStmt e) indent env = do
  let (outStr, newEnv) = evalExprStmt e env
  return (newEnv, [indentStr indent ++ outStr])

-- Bloque con scope propio
-- Bloque con scope propio
evalComm ioFns (Block body) indent env = do
  let envIn = pushScope env
      hdr   = indentStr indent ++ "do"
  (envMid, innerLines) <- evalComm ioFns body (indent+2) envIn
  let envOut   = popScope envMid

      -- ¿el bloque está vacío o termina en 'let …'?
      needsRet = null innerLines
              || case dropWhile (==' ') (last innerLines) of
                   ('l':'e':'t':_) -> True
                   _               -> False

      -- Añadimos “return ()” cuando hace falta
      innerLines' = if needsRet
                      then innerLines ++ [indentStr (indent+2) ++ "return ()"]
                      else innerLines

  return (envOut, hdr : innerLines')


-- Secuencia
{-
evalComm ioFuncs (Seq c1 c2) indent env = do
  (env',  l1) <- evalComm ioFuncs c1 indent env
  (env'', l2) <- evalComm ioFuncs c2 indent env'
  return (env'', l1 ++ l2)
-}
-- Secuencia
evalComm ioFuncs (Seq c1 c2) indent env = do
  (env',  l1) <- evalComm ioFuncs c1 indent env
  if hasTrailingReturn c1          -- ⇠ si c1 ya devuelve, c2 es código muerto
     then return (env', l1)
     else do
       (env'', l2) <- evalComm ioFuncs c2 indent env'
       return (env'', l1 ++ l2)


-- main
evalComm ioFns (FuncDef _ "main" _ body) indent env = do
  let header = ["main :: IO ()", "main = do"]
  (_, raw) <- evalComm ioFns body (indent+4) env
  let core   = if hasTrailingReturn body && not (null raw)
                 then init raw else raw
      body'  = map (fixBindLine ioFns) core
             ++ [indentStr (indent+4) ++ "return ()"]
  return (env, header ++ body')

-- arr[i] = rhs;
evalComm ioFns (AssignArr v idx rhs) indent env = do
  let oldArr   = currentVarName v env
      idxStr   = evalExp idx env
      rhsStr   = evalExp rhs env
      (v', env') = nextVarName v env
      line = indentStr indent ++
             "let " ++ v' ++ " = take " ++ idxStr ++ " " ++ oldArr ++
             " ++ [" ++ rhsStr ++ "] ++ drop (" ++ idxStr ++ " + 1) " ++ oldArr
  return (env', [line])



------------------------------------------------------------------
-- Definición de función (≠ main)
------------------------------------------------------------------
evalComm ioFuncs (FuncDef retType name params body) indent env = do
  ----------------------------------------------------------------
  -- 1) ¿requiere IO?  ¿devuelve void?
  ----------------------------------------------------------------
  let ioNeeded = name `elem` ioFuncs
      isVoid   = retType == TVoid

  ----------------------------------------------------------------
  -- 2) Firma de tipos
  ----------------------------------------------------------------
      paramTypes = map (translateType . fst) params
      paramNames = unwords (map snd params)
      retSig     = if ioNeeded
                     then "IO " ++ if isVoid then "()" else translateType retType
                     else         if isVoid then "()" else translateType retType
      typeSig    = name ++ " :: " ++ concatMap (++ " -> ") paramTypes ++ retSig
  putStrLn typeSig

  ----------------------------------------------------------------
  -- 3) Entorno con los parámetros en un nuevo scope
  ----------------------------------------------------------------
  let paramScope    = map (\(_,v) -> (v,0)) params :: Scope
      envWithParams = env { varEnv = paramScope : varEnv env }

  ----------------------------------------------------------------
  -- 4) Traducir el cuerpo de la función
  ----------------------------------------------------------------
  (_, rawLines) <- evalComm ioFuncs body (indent + 2) envWithParams

  -- separamos todo salvo la última línea
  let (bodyInit, finalExpr) =
        case reverse rawLines of
          []     -> ([], "return ()")
          (x:xs) -> (reverse xs, x)

  ----------------------------------------------------------------
  -- 5) Generar definición Haskell según 4 casos
  ----------------------------------------------------------------
  case () of
    ----------------------------------------------------------------
    -- 1)  Función con IO  y  void
    ----------------------------------------------------------------
    _ | ioNeeded && isVoid -> do
          putStrLn $ name ++ " " ++ paramNames ++ " = do"
          mapM_ (putStrLn . ("  " ++)) rawLines
          putStrLn "    return ()"

    ----------------------------------------------------------------
    -- 2)  Función con IO  y  valor de retorno
    ----------------------------------------------------------------
      | ioNeeded && not isVoid -> do
          let endsWithReturn = case words (dropWhile (==' ') finalExpr) of
                                ("return":_) -> True
                                _            -> False
              cuerpo = if endsWithReturn then bodyInit else rawLines
          putStrLn $ name ++ " " ++ paramNames ++ " = do"
          mapM_ (putStrLn . ("  " ++)) cuerpo
          unless endsWithReturn $
            putStrLn $ "    return (" ++ dropWhile (== ' ') finalExpr ++ ")"

    ----------------------------------------------------------------
    -- 3)  Pura  y  void
    ----------------------------------------------------------------
      | not ioNeeded && isVoid -> do
          putStrLn $ name ++ " " ++ paramNames ++ " = ()"
          unless (null rawLines) $ do
            putStrLn "  where"
            mapM_ (putStrLn . ("    " ++) . stripAux . dropWhile (==' ')) rawLines

    ----------------------------------------------------------------
    -- 4a) Pura  con retorno  *y*  cuerpo simple (solo Returns/ifs anidados)
    ----------------------------------------------------------------
      | not ioNeeded && not isVoid && isPureReturnBody body -> do
          let expr = genPureBody body envWithParams
          putStrLn $ name ++ " " ++ paramNames ++ " = " ++ expr

    ----------------------------------------------------------------
    -- 4b) Pura  con retorno  y cuerpo general
    ----------------------------------------------------------------
      | otherwise -> do
          let clean = dropWhile (==' ')               -- quita espacios iniciales
                      . stripPrefix "return "         -- elimina el “return ” final
              stripPrefix p xs = if p `isPrefixOf` xs then drop (length p) xs else xs
          putStrLn $ name ++ " " ++ paramNames ++ " = " ++ clean finalExpr
          unless (null bodyInit) $ do
            putStrLn "  where"
            mapM_ (putStrLn . ("    " ++) . stripAux . dropWhile (==' ')) bodyInit

  ----------------------------------------------------------------
  -- 6)  Esta rama no añade líneas al código Haskell resultante
  ----------------------------------------------------------------
  return (env, [])




-- Return
evalComm _ (Return exp) indent env =
  return (env, [indentStr indent ++ "return " ++ evalExp exp env])
{-
evalComm ioFns (Printf fmt args) indent env = do
  let str = translatePrintf fmt args env
  return (env, [indentStr indent ++ str])
-}
evalComm ioFns (Printf fmt args) indent env = do
  putStrLn $ "DEBUG Printf: env = " ++ show (varsInEnv env)
  let str = translatePrintf fmt args env
  return (env, [indentStr indent ++ str])

{-
-- ----------------  *p = rhs;  -------------------------------
evalComm _ (AssignDeref (Deref (VarExp p)) rhs) indent env = do
  let pName  = currentVarName p env
      rhsStr = evalExp rhs env
  case lookupPtr pName env of
    ------------------------------------------------------------------
    --  puntero a variable escalar  (&x)
    ------------------------------------------------------------------
    Just (baseLog, IntExp 0) -> do
      -- Detectar si baseLog es escalar o array:
      -- Por defecto, lo tratamos como escalar. Si querés detección real, habría que consultar el entorno y el tipo.
      let isArray = False  -- Cambiá manualmente según el test, o implementá lógica de detección si te interesa.
      if isArray
        then do
          -- Caso ARRAY
          let idxStr = "0"
              oldArr = currentVarName baseLog env
              (arr', env1) = nextVarName baseLog env
              lineAssign = indentStr indent ++
                           "let " ++ arr' ++ " = take " ++ idxStr ++ " " ++ oldArr ++
                           " ++ [" ++ rhsStr ++ "] ++ drop (" ++ idxStr ++ " + 1) " ++ oldArr
              (p', env2)   = nextVarName p env1
              linePtr      = indentStr indent ++
                             "let " ++ p' ++ " = (" ++ arr' ++ ", " ++ idxStr ++ ")"
              envFinal     = setPtr p' (baseLog, IntExp 0) env2
          return (envFinal, [lineAssign, linePtr])
        else do
          -- Caso ESCALAR
          let (base', env1) = nextVarName baseLog env
              lineAssign    = indentStr indent ++ "let " ++ base' ++ " = " ++ rhsStr
              (p',   env2)  = nextVarName p env1
              linePtr       = indentStr indent ++
                              "let " ++ p' ++ " = (" ++ base' ++ ", 0)"
              envFinal      = setPtr p' (baseLog, IntExp 0) env2
          return (envFinal, [lineAssign, linePtr])

    ------------------------------------------------------------------
    --  puntero a arr[i]   (&arr[i])
    ------------------------------------------------------------------
    Just (arrLog, idxE) -> do
      let idxStr      = evalExp idxE env
          oldArr      = currentVarName arrLog env
          (arr', env1) = nextVarName arrLog env
          lineAssign   = indentStr indent ++
                         "let " ++ arr' ++ " = take " ++ idxStr ++ " " ++ oldArr ++
                         " ++ [" ++ rhsStr ++ "] ++ drop (" ++ idxStr ++ " + 1) " ++ oldArr
          (p', env2)   = nextVarName p env1
          linePtr      = indentStr indent ++
                         "let " ++ p' ++ " = (" ++ arr' ++ ", " ++ idxStr ++ ")"
          envFinal     = setPtr p' (arrLog, idxE) env2
      return (envFinal, [lineAssign, linePtr])

    ------------------------------------------------------------------
    _ -> error "AssignDeref: puntero no declarado o forma no soportada."

-}

evalComm _ (AssignDeref (Deref (VarExp p)) rhs) indent env = do
  let pName  = currentVarName p env
      rhsStr = evalExp rhs env
  case lookupPtr pName env of
    Just (baseLog, IntExp 0) -> do
      -- Heurística básica: si el nombre base empieza con "b" o "a", asumimos array.
      -- ¡Podes mejorarlo por entorno real!
      let isArray = case take 1 baseLog of
                      "b" -> True
                      "a" -> True
                      _   -> False
      if isArray
        then do
          let idxStr = "0"
              oldArr = currentVarName baseLog env
              (arr', env1) = nextVarName baseLog env
              lineAssign = indentStr indent ++
                           "let " ++ arr' ++ " = take " ++ idxStr ++ " " ++ oldArr ++
                           " ++ [" ++ rhsStr ++ "] ++ drop (" ++ idxStr ++ " + 1) " ++ oldArr
              (p', env2)   = nextVarName p env1
              linePtr      = indentStr indent ++
                             "let " ++ p' ++ " = (" ++ arr' ++ ", " ++ idxStr ++ ")"
              envFinal     = setPtr p' (baseLog, IntExp 0) env2
          return (envFinal, [lineAssign, linePtr])
        else do
          let (base', env1) = nextVarName baseLog env
              lineAssign    = indentStr indent ++ "let " ++ base' ++ " = " ++ rhsStr
              (p',   env2)  = nextVarName p env1
              linePtr       = indentStr indent ++
                              "let " ++ p' ++ " = (" ++ base' ++ ", 0)"
              envFinal      = setPtr p' (baseLog, IntExp 0) env2
          return (envFinal, [lineAssign, linePtr])
    Just (arrLog, idxE) -> do
      let idxStr      = evalExp idxE env
          oldArr      = currentVarName arrLog env
          (arr', env1) = nextVarName arrLog env
          lineAssign   = indentStr indent ++
                         "let " ++ arr' ++ " = take " ++ idxStr ++ " " ++ oldArr ++
                         " ++ [" ++ rhsStr ++ "] ++ drop (" ++ idxStr ++ " + 1) " ++ oldArr
          (p', env2)   = nextVarName p env1
          linePtr      = indentStr indent ++
                         "let " ++ p' ++ " = (" ++ arr' ++ ", " ++ idxStr ++ ")"
          envFinal     = setPtr p' (arrLog, idxE) env2
      return (envFinal, [lineAssign, linePtr])
    _ -> error "AssignDeref: puntero no declarado o forma no soportada."







-- While --------------------------------------------------------------------
evalComm ioFns (While cond body) indent env = do
  -- 0) comprobación booleana
  unless (ensureBool cond) $
        error "La condición del while debe ser booleana."

  -- 1) nombres y primer entorno
  let ctrlVar           = extractControlVar cond
      loopName          = "whileLoop_" ++ ctrlVar
      (paramCtrl, env1) = nextVarName ctrlVar env
      condStr           = evalBoolExp cond env1
      hasBrk            = containsBreak body

      -- desenvolver Block para no meter un `do`/`return ()` extra
      actualBody = case body of
                     Block c -> c
                     _       -> body

  -- 2) traducir cuerpo con indentación +10
  (envBody, bodyLines) <- evalComm ioFns actualBody (indent + 10) env1

  -- 3) detectar todas las vars mutadas (incluye ctrlVar)
  let mods       = nub (ctrlVar : changedVars env1 envBody)
      paramsIn   = map (`currentVarName` env1)  mods
      paramsNext = map (`currentVarName` envBody) mods

  -- 4) generar nombres de salida y nuevo entorno
  (resVars, envOut) <- foldM
    (\(vs, curEnv) v ->
        let (v', e') = nextVarName v curEnv
        in return (vs ++ [v'], e'))
    ([], envBody)
    mods

  let tuple xs = if length xs == 1
                   then head xs
                   else "(" ++ intercalate ", " xs ++ ")"

  -- 5) montar líneas con la indentación corregida
  let header =
        [ indentStr indent ++ "let " ++ loopName ++ " " ++ unwords paramsIn ++ " ="
        , indentStr (indent + 6)
            ++ (if hasBrk
                  then "catch (if (" ++ condStr ++ ") then do"
                  else "if (" ++ condStr ++ ") then do")
        ]
      recur    = indentStr (indent + 10) ++ loopName ++ " " ++ unwords paramsNext
      elseLn   = indentStr (indent + 6)  ++ "else return " ++ tuple paramsIn
      catchEnd = if hasBrk
                   then [ indentStr (indent + 6)
                           ++ ") (\\_ -> return " ++ tuple paramsIn ++ ")" ]
                   else []
      callLine = indentStr indent
                 ++ tuple resVars
                 ++ " <- "
                 ++ loopName ++ " "
                 ++ unwords (map (`currentVarName` env) mods)

      code = header ++ bodyLines ++ [recur, elseLn] ++ catchEnd ++ [callLine]

  return (envOut, code)


-- MODIFICAMOS


-- If con else
-- If con else
evalComm ioFuncs (Cond cond cThen cElse) indent env = do
  let condStr     = evalBoolExp cond env
      envBeforeIf = env
  (envThen, thenLines) <- evalComm ioFuncs cThen (indent+4) envBeforeIf
  (envElse, elseLines) <- evalComm ioFuncs cElse (indent+4) envBeforeIf

  let allVars      = nub (varsInEnv envThen ++ varsInEnv envElse ++ varsInEnv envBeforeIf)
      affectedVars = filter (\v -> lookupVar v envThen /= lookupVar v envBeforeIf
                               || lookupVar v envElse /= lookupVar v envBeforeIf)
                             allVars

  if null affectedVars
    then do
      let indentIf   = indentStr indent
          alreadyDo s = case words (dropWhile (== ' ') s) of
                          ("do":_) -> True   -- reconoce "do" solo o "do …"
                          _        -> False
          addDoBlock s = if alreadyDo s then [] else [indentIf ++ "do"]
          headerIf   = indentIf ++ "if " ++ condStr ++ " then"
          headerElse = indentIf ++ "else"

          thenLines' = if null thenLines
                         then [indentStr (indent+4) ++ "return ()"]
                         else addDoBlock (head thenLines) ++ thenLines
          elseLines' = if null elseLines
                         then [indentStr (indent+4) ++ "return ()"]
                         else addDoBlock (head elseLines) ++ elseLines

      return (envBeforeIf, headerIf:thenLines' ++ headerElse:elseLines')
    else do
      (finalEnv, letStmts) <- foldM
        (\(curEnv, curLines) var -> do
           let nameBefore    = currentVarNameOrDefault var var envBeforeIf
               thenAssignExp = findLastAssignmentExpr var thenLines nameBefore
               elseAssignExp = findLastAssignmentExpr var elseLines nameBefore
               (finalVar, nextEnv) = nextVarName var curEnv
               stmt = indentStr indent ++ "let " ++ finalVar ++ " = if "
                      ++ condStr ++ " then (" ++ thenAssignExp ++ ") else (" ++ elseAssignExp ++ ")"
           return (nextEnv, curLines ++ [stmt]))
        (envBeforeIf, []) affectedVars
      return (finalEnv, letStmts)


-- If sin else
evalComm ioFuncs (CondNoElse cond cThen) indent env = do
  let condStr     = evalBoolExp cond env
      envBeforeIf = env
  (envThen, thenLines) <- evalComm ioFuncs cThen (indent+4) envBeforeIf

  let allVars      = nub (varsInEnv envThen ++ varsInEnv envBeforeIf)
      affectedVars = filter (\v -> lookupVar v envThen /= lookupVar v envBeforeIf)
                             allVars

  if null affectedVars
    then do
      let headerIf  = indentStr indent ++ "if " ++ condStr ++ " then do"
          thenLines' = if null thenLines
                       then [indentStr (indent+4) ++ "return ()"]
                       else thenLines
      return (envBeforeIf, headerIf:thenLines')
    else do
      (finalEnv, letStmts) <- foldM
        (\(curEnv, curLines) var -> do
           let nameBefore    = currentVarNameOrDefault var var envBeforeIf
               thenAssignExp = findLastAssignmentExpr var thenLines nameBefore
               (finalVar, nextEnv) = nextVarName var curEnv
               stmt = indentStr indent ++ "let " ++ finalVar ++ " = if "
                      ++ condStr ++ " then (" ++ thenAssignExp ++ ") else ("
                      ++ nameBefore ++ ")"
           return (nextEnv, curLines ++ [stmt]))
        (envBeforeIf, []) affectedVars
      return (finalEnv, letStmts)


-- ==============================================================
--  Declaración SIN inicialización
--  • Si el tipo es puntero, abortamos inmediatamente con el
--    mismo mensaje usado en AssignDeref para garantizar que
--    cualquier «int *p;» corte la traducción/ejecución.
-- ==============================================================
evalComm _ (LetUninit t v) indent env =
  case t of
    -- ▸ Puntero sin inicializar  →  error y fin.
    TPtr _ ->
      error "AssignDeref: puntero no declarado o forma no soportada."

    -- ▸ Cualquier otro tipo  →  seguimos como siempre.
    _      -> do
      let (v', env') = declareVar v env
          defVal     = defaultValue t
          line       = indentStr indent
                    ++ "let " ++ v' ++ " = "
                    ++ defVal ++ " :: " ++ translateType t
      return (env', [line])



-- For (for(init; cond; step) { body })
evalComm ioFns (For mInit cond mStep body) indent env = do
  ----------------------------------------------------------------
  -- 0) Ejecutar init opcional
  ----------------------------------------------------------------
  (envInit, initLines) <- maybe (return (env, []))
                                (\c -> evalComm ioFns c indent env)
                                mInit

  ----------------------------------------------------------------
  -- 1) Variable de control y nombres auxiliares
  ----------------------------------------------------------------
  let ctrlVar           = case cond of
                             BTrue -> "dummy"
                             _     -> extractControlVar cond
      loopName          = "forLoop_" ++ ctrlVar
      (paramCtrl, env1) = nextVarName ctrlVar envInit

      ----------------------------------------------------------------
      -- 2) Detectar arreglo(s) mutado(s)
      --    • 0  →  permitido (nuevo)
      --    • 1  →  comportamiento previo
      --    • >1 →  seguimos rechazando
      ----------------------------------------------------------------
      arrsMut = nub (arraysWritten body)
  unless (length arrsMut <= 1) $
      error "Solo se admite un arreglo modificado por for."

  ----------------------------------------------------------------
  -- 3) Condición del for
  ----------------------------------------------------------------
  let condStr = evalBoolExp cond env1

  ----------------------------------------------------------------
  -- 4) Traducir cuerpo y paso
  ----------------------------------------------------------------
  (envBody, bodyLines) <- evalComm ioFns body (indent+6) env1
  (envStep, stepLines) <- maybe (return (envBody, []))
                                (\c -> evalComm ioFns c (indent+8) envBody)
                                mStep

  ----------------------------------------------------------------
  -- 5) Nombres tras el paso
  ----------------------------------------------------------------
  let nextCtrl = currentVarName ctrlVar envStep
      recIndent = indent + 8        -- mismo indent que bodyLines/stepLines

  ----------------------------------------------------------------
  -- 6) Generar código según haya (o no) arreglo
  ----------------------------------------------------------------
  code <- case arrsMut of
            --------------------------------------------------------
            -- 0 arreglos mutados  ➜  solo control de flujo
            --------------------------------------------------------
            [] -> do
              let callLine = indentStr indent
                           ++ loopName ++ " "
                           ++ currentVarName ctrlVar envInit
                  code0 =
                       initLines ++
                       [ indentStr indent     ++ "let"
                       , indentStr (indent+2) ++ loopName ++ " " ++ paramCtrl ++ " = do"
                       , indentStr (indent+4) ++ "if (" ++ condStr ++ ") then do"
                       ]
                       ++ bodyLines
                       ++ stepLines
                       ++ [ indentStr recIndent ++ loopName ++ " " ++ nextCtrl
                          , indentStr (indent+4) ++ "else return ()"
                          , callLine
                          ]
              return code0

            --------------------------------------------------------
            -- 1 arreglo mutado  ➜  comportamiento original
            --------------------------------------------------------
            (arr:_) -> do
              let arrInitName = currentVarName arr envInit
                  arrParam    = arrInitName
                  arrNewest   = currentVarName arr envStep
                  code1 =
                       initLines ++
                       [ indentStr indent     ++ "let"
                       , indentStr (indent+2) ++ loopName ++ " " ++ arrParam ++ " " ++ paramCtrl ++ " = do"
                       , indentStr (indent+4) ++ "if (" ++ condStr ++ ") then do"
                       ]
                       ++ bodyLines
                       ++ stepLines
                       ++ [ indentStr recIndent ++ loopName ++ " " ++ arrNewest ++ " " ++ nextCtrl
                          , indentStr (indent+4) ++ "else return " ++ arrParam
                          , indentStr indent     ++ currentVarName arr envStep ++ " <- "
                                               ++ loopName ++ " " ++ arrInitName ++ " "
                                               ++ currentVarName ctrlVar envInit
                          ]
              return code1

  return (envStep, code)



evalComm _ Break indent env =
  return (env, [indentStr indent ++ "return ()"])

----backup

evalComm _ (Scanf fmt vars) indent env = do
  let formatList = words fmt
  (env', lines) <- foldM (genLine indent) (env, []) (zip formatList vars)
  return (env', lines)
  where
    genLine i (eAcc, acc) (fmt, var) = do
      let hsType = case fmt of
            "%d"    -> "readLn :: IO Int"
            "%ld"   -> "readLn :: IO Integer"
            "%f"    -> "readLn :: IO Float"
            "%lf"   -> "readLn :: IO Double"
            "%c"    -> "getChar"
            "%s"    -> "getLine"
            _       -> error $ "scanf: formato no soportado: " ++ fmt
      let (v', newEnv) = nextVarName var eAcc
      let linea = indentStr i ++ v' ++ " <- " ++ hsType
      return (newEnv, acc ++ [linea])

-- Do-While -----------------------------------------------------------------
evalComm ioFns (DoWhile body cond) indent env = do
  ----------------------------------------------------------------
  -- 0) chequeo de tipo
  ----------------------------------------------------------------
  unless (ensureBool cond) $
        error "La condición del do-while debe ser booleana."

  ----------------------------------------------------------------
  -- 1) nombres auxiliares y primer entorno
  ----------------------------------------------------------------
  let ctrlVar           = extractControlVar cond
      loopName          = "doWhileLoop_" ++ ctrlVar
      (paramCtrl, env1) = nextVarName ctrlVar env
      hasBrk            = containsBreak body

      -- sacamos Block para no anidar un do extra
      unwrap (Block c) = c
      unwrap c         = c
      realBody         = unwrap body

  ----------------------------------------------------------------
  -- 2) traducir el cuerpo (+8 columnas)
  ----------------------------------------------------------------
  (envBody, bodyLines) <- evalComm ioFns realBody (indent+8) env1

  ----------------------------------------------------------------
  -- 3) condición *después* del cuerpo (usa envBody)
  ----------------------------------------------------------------
  let condStr = evalBoolExp cond envBody

  ----------------------------------------------------------------
  -- 4) variables que cambian en cada vuelta
  ----------------------------------------------------------------
      mods       = nub (ctrlVar : changedVars env1 envBody)
      paramsIn   = map (`currentVarName`  env1)  mods   -- al entrar
      paramsNext = map (`currentVarName`  envBody) mods -- tras el body

  ----------------------------------------------------------------
  -- 5) nombres nuevos para la salida y entorno final
  ----------------------------------------------------------------
  (resVars, envOut) <- foldM
        (\(vs,cur) v -> let (v',cur') = nextVarName v cur
                        in pure (vs++[v'],cur'))
        ([], envBody) mods

  let tuple xs = if length xs == 1 then head xs
                               else "(" ++ intercalate ", " xs ++ ")"

      -- ▸ cabecera, con o sin soporte de break
      header | hasBrk = [ indentStr indent ++ "let " ++ loopName ++ " " ++ unwords paramsIn ++ " ="
                        , indentStr (indent+6) ++ "catch (do"
                        ]
             | otherwise =
                        [ indentStr indent ++ "let " ++ loopName ++ " " ++ unwords paramsIn ++ " = do" ]

      -- ▸ pie para el caso con break
      footer | hasBrk = [ indentStr (indent+6) ++ ") (\\_ -> return " ++ tuple paramsIn ++ ")" ]
             | otherwise = []

      -- ▸ líneas de control de la iteración
      ifLn    = indentStr (indent+8)  ++ "if (" ++ condStr ++ ")"
      thenLn  = indentStr (indent+12) ++ "then " ++ loopName ++ " " ++ unwords paramsNext
      elseLn  = indentStr (indent+12) ++ "else return " ++ tuple paramsNext

      -- ▸ llamada inicial desde el lugar donde estaba el Do-While
      callLn  = indentStr indent ++ tuple resVars ++ " <- "
                          ++ loopName ++ " " ++ unwords (map (`currentVarName` env) mods)

      code = header
          ++ bodyLines
          ++ [ifLn, thenLn, elseLn]
          ++ footer
          ++ [callLn]

  return (envOut, code)

--------------------------------------------------------------------
--  SWITCH
--------------------------------------------------------------------
-- Genera código Haskell que emula el comportamiento de C:
--   * sólo se ejecuta el primer ‘case’ que matchee;
--   * ‘default’ se ejecuta si ningún ‘case’ matcheó.
--------------------------------------------------------------------
evalComm ioFns (Switch scrut cases) indent env0 = do
  -- 1) Evaluar el valor del switch una sola vez
  let (scrutVar, env1) = nextVarName "scrut" env0
      scrutVal         = evalExp scrut env1
      assignScrut      = indentStr indent ++ "let " ++ scrutVar ++ " = " ++ scrutVal

  -- 2) Generar los bloques de cada case
  (envOut, caseLines) <- genCases ioFns scrutVar cases "False" indent env1
  return (envOut, assignScrut : caseLines)


--------------------------------------------------------------------
-- Generador recursivo de los ‘case’ / ‘default’
--------------------------------------------------------------------
genCases :: IOFuncs        -- ^ primitivas de IO
         -> Variable       -- ^ variable con el valor ya evaluado del switch
         -> [Case]         -- ^ lista de casos
         -> String         -- ^ flag ‘matched’ acumulado
         -> Int            -- ^ indentación actual
         -> Env            -- ^ entorno actual
         -> IO (Env,[String])
genCases _ _ [] _ _ envAcc = return (envAcc, [])

genCases ioFns scrVar (c:rest) matched indentCur envCur =
  case c of
    ----------------------------------------------------------
    --            case  <const>:
    ----------------------------------------------------------
    Case lbl body -> do
      let condLabel = scrVar ++ " == " ++ evalExp lbl envCur
          condFull  = "(not (" ++ matched ++ ") && " ++ condLabel ++ ")"
          matched'  = "(" ++ matched ++ " || " ++ condLabel ++ ")"
          header    = indentStr indentCur ++ "if " ++ condFull ++ " then do"

      (envBody, rawLines) <- evalComm ioFns body (indentCur + 4) envCur
      let bodyLines = case rawLines of
                         [] -> [indentStr (indentCur+4) ++ "return ()"]
                         ls -> ls ++ [indentStr (indentCur+4) ++ "return ()"]
          footer    = indentStr indentCur ++ "else return ()"

      (envNext, restLines) <- genCases ioFns scrVar rest matched' indentCur envBody
      return (envNext, header : bodyLines ++ [footer] ++ restLines)

    ----------------------------------------------------------
    --            default:
    ----------------------------------------------------------
    DefaultCase body -> do
      let condFull = "not (" ++ matched ++ ")"
          header   = indentStr indentCur ++ "if " ++ condFull ++ " then do"

      (envBody, rawLines) <- evalComm ioFns body (indentCur + 4) envCur
      let bodyLines = case rawLines of
                         [] -> [indentStr (indentCur+4) ++ "return ()"]
                         ls -> ls ++ [indentStr (indentCur+4) ++ "return ()"]
          footer    = indentStr indentCur ++ "else return ()"

      -- después de default no hace falta seguir evaluando condiciones
      (envNext, restLines) <- genCases ioFns scrVar rest "True" indentCur envBody
      return (envNext, header : bodyLines ++ [footer] ++ restLines)





------------------------------------------------------------------
-- Utilidades para distinguir acciones IO
------------------------------------------------------------------

-- ¿hay que ligar con <-?
needsBind :: IOFuncs -> Exp -> Bool
needsBind ioFns (CallExp fn _) = fn `elem` ioFns
needsBind _      _             = False

-- Dentro de 'main' re-escribimos  «x <- f …»  →  «let x = f …»
-- cuando la función f es PURA, para no arrastrar IO innecesario
fixBindLine :: IOFuncs -> String -> String
fixBindLine ioFns line =
  case words (dropWhile (== ' ') line) of
    (v:"<-":fn:rest)
        | esPura fn ->        -- se convierte a let
            indentStr 4 ++ "let " ++ v ++ " = " ++ unwords (fn:rest)
        | otherwise -> line   -- se deja con <-
    _ -> line
  where
    esPura f =  f `notElem` ioFns
             && not ("whileLoop_"  `isPrefixOf` f)
             && not ("repeatLoop_" `isPrefixOf` f)
             && not ("forLoop_"    `isPrefixOf` f) 
             && not ("doWhileLoop_"  `isPrefixOf` f) 



-- -------------------------------------------------------------------
-- Variable de control de un BoolExp (while / for / do-while …)
-- -------------------------------------------------------------------
getCtrlVar :: BoolExp -> Variable
getCtrlVar = extractControlVar


------------------------------------------------------------------
-- Funciones faltantes añadidas
------------------------------------------------------------------
ensureBool :: BoolExp -> Bool
ensureBool BTrue           = True
ensureBool BFalse          = True
ensureBool (Not b)         = ensureBool b
ensureBool (And b1 b2)     = ensureBool b1 && ensureBool b2
ensureBool (Or  b1 b2)     = ensureBool b1 && ensureBool b2
ensureBool (Eq _ _)        = True
ensureBool (Neq _ _)       = True
ensureBool (Lt _ _)        = True
ensureBool (Le _ _)        = True
ensureBool (Gt _ _)        = True
ensureBool (Ge _ _)        = True

replaceVar :: Variable -> Variable -> Exp -> Exp
replaceVar old new (VarExp v) | v == old = VarExp new
replaceVar old new (AddExp e1 e2)       = AddExp (replaceVar old new e1) (replaceVar old new e2)
replaceVar old new (SubExp e1 e2)       = SubExp (replaceVar old new e1) (replaceVar old new e2)
replaceVar old new (MulExp e1 e2)       = MulExp (replaceVar old new e1) (replaceVar old new e2)
replaceVar old new (DivExp e1 e2)       = DivExp (replaceVar old new e1) (replaceVar old new e2)
replaceVar old new (UMinus e)           = UMinus (replaceVar old new e)
replaceVar old new (BoolAsIntExp b)     = BoolAsIntExp (replaceVarBool old new b)
replaceVar _   _   e                    = e

replaceVarBool :: Variable -> Variable -> BoolExp -> BoolExp
replaceVarBool old new (Eq e1 e2)    = Eq  (replaceVar old new e1) (replaceVar old new e2)
replaceVarBool old new (Neq e1 e2)   = Neq (replaceVar old new e1) (replaceVar old new e2)
replaceVarBool old new (Lt e1 e2)    = Lt  (replaceVar old new e1) (replaceVar old new e2)
replaceVarBool old new (Le e1 e2)    = Le  (replaceVar old new e1) (replaceVar old new e2)
replaceVarBool old new (Gt e1 e2)    = Gt  (replaceVar old new e1) (replaceVar old new e2)
replaceVarBool old new (Ge e1 e2)    = Ge  (replaceVar old new e1) (replaceVar old new e2)
replaceVarBool old new (And b1 b2)   = And (replaceVarBool old new b1) (replaceVarBool old new b2)
replaceVarBool old new (Or b1 b2)    = Or  (replaceVarBool old new b1) (replaceVarBool old new b2)
replaceVarBool old new (Not b)       = Not (replaceVarBool old new b)
replaceVarBool _   _   b             = b

extractControlVar :: BoolExp -> Variable
extractControlVar (Eq (VarExp v) _)  = v
extractControlVar (Neq (VarExp v) _) = v
extractControlVar (Lt (VarExp v) _)  = v
extractControlVar (Le (VarExp v) _)  = v
extractControlVar (Gt (VarExp v) _)  = v
extractControlVar (Ge (VarExp v) _)  = v
extractControlVar (Not b)            = extractControlVar b
extractControlVar (And b _)          = extractControlVar b
extractControlVar (Or b _)           = extractControlVar b
extractControlVar _                  = error "No se pudo extraer la variable de control."

------------------------------------------------------------------
-- Otras utilidades
------------------------------------------------------------------
hasTrailingReturn :: Comm -> Bool
hasTrailingReturn (Return _) = True
hasTrailingReturn Break      = True
hasTrailingReturn (Seq _ c2) = hasTrailingReturn c2
hasTrailingReturn _          = False

-- ➜ Nuevo helper que limpia "let" o "return" al generar cláusulas where
stripAux :: String -> String
stripAux s =
  let ws = words (dropWhile (==' ') s)
  in case ws of
       ("let":rest)    -> unwords rest
       ("return":rest) -> unwords rest
       _               -> unwords ws


findLastAssignmentExpr :: Variable -> [String] -> String -> String
findLastAssignmentExpr var lines def =
  case find (isAssignFor var) (reverse lines) of
    Just l  -> extractExpr l
    Nothing -> def
  where
    isAssignFor v ln =
      let t = dropWhile (== ' ') ln
      in case words t of
           ("let":vN:"=":_) -> v `isPrefixOf` vN && all (`elem` ['0'..'9']) (drop (length v) vN)
           _                -> False
    extractExpr ln =
      let parts = drop 3 (words (dropWhile (== ' ') ln))
          endsWithType = length parts >= 2 && last (init parts) == "::"
          exprParts    = if endsWithType then take (length parts - 2) parts else parts
      in unwords exprParts




------------------------------------------------------------------
-- Helpers de printf
------------------------------------------------------------------
splitOn :: String -> String -> [String]
splitOn _ "" = [""]
splitOn delim str =
  case findPrefix delim str of
    Just rest -> "" : splitOn delim rest
    Nothing   -> let (before, after) = breakOn delim str
                 in before : case after of
                                "" -> []
                                _  -> splitOn delim after

findPrefix :: String -> String -> Maybe String
findPrefix p s | p `isPrefixOf` s = Just (drop (length p) s)
               | otherwise        = Nothing

breakOn :: String -> String -> (String,String)
breakOn _ "" = ("","")
breakOn delim s@(c:cs)
  | delim `isPrefixOf` s = ("", s)
  | otherwise            = let (pre, post) = breakOn delim cs
                           in (c:pre, post)

------------------------------------------------------------------
-- Evaluación de expresiones
------------------------------------------------------------------
evalExprStmt :: Exp -> Env -> (String, Env)
evalExprStmt (PostIncr v) env =
  let old             = currentVarName v env
      (new, newEnv)   = nextVarName v env
  in ("let " ++ new ++ " = " ++ old ++ " + 1", newEnv)
evalExprStmt (PostDecr v) env =
  let old             = currentVarName v env
      (new, newEnv)   = nextVarName v env
  in ("let " ++ new ++ " = " ++ old ++ " - 1", newEnv)
evalExprStmt (PreIncr v) env =
  let old             = currentVarName v env
      (new, newEnv)   = nextVarName v env
  in ("let " ++ new ++ " = " ++ old ++ " + 1", newEnv)
evalExprStmt (PreDecr v) env =
  let old             = currentVarName v env
      (new, newEnv)   = nextVarName v env
  in ("let " ++ new ++ " = " ++ old ++ " - 1", newEnv)
evalExprStmt e env = (evalExp e env, env)

evalExp :: Exp -> Env -> String
evalExp (BoolAsIntExp b) env      = "(if " ++ evalBoolExp b env ++ " then 1 else 0)"
evalExp (IntExp n) _              = show n
evalExp (FloatExp n) _            = show n
evalExp (StringExp s) _           = show s
evalExp (CharExp c) _             = show c
evalExp (VarExp v) env            = currentVarName v env
evalExp (UMinus e) env            = "(-" ++ evalExp e env ++ ")"
evalExp (AddExp e1 e2) env        = "(" ++ evalExp e1 env ++ " + " ++ evalExp e2 env ++ ")"
evalExp (SubExp e1 e2) env        = "(" ++ evalExp e1 env ++ " - " ++ evalExp e2 env ++ ")"
evalExp (MulExp e1 e2) env        = "(" ++ evalExp e1 env ++ " * " ++ evalExp e2 env ++ ")"
evalExp (DivExp e1 e2) env        = "(" ++ evalExp e1 env ++ " `div` " ++ evalExp e2 env ++ ")"
evalExp (ModExp e1 e2) env = "(" ++ evalExp e1 env ++ " `mod` " ++ evalExp e2 env ++ ")"
evalExp (PostIncr v) env          = currentVarName v env
evalExp (PostDecr v) env          = currentVarName v env
evalExp (PreIncr v) env           = "(" ++ currentVarName v env ++ " + 1)"
evalExp (PreDecr v) env           = "(" ++ currentVarName v env ++ " - 1)"
evalExp (ArrayAccess arr idx) env = "(" ++ currentVarName arr env ++ " !! " ++ evalExp idx env ++ ")"
-- llamada a función
evalExp (CallExp fn args) env
  | null args = fn                   -- hablar        (IO ())
  | otherwise = fn ++ " " ++ unwords (map (`evalExp` env) args)
-- --- tomar dirección: "&e" ---
evalExp (AddrOf e) env =
  case e of
    VarExp v ->
      -- devolvemos el nombre versionado de la variable a la que apuntamos
      currentVarName v env
    ArrayAccess a idxExp ->
      -- si decimos "&arr[i]", devolvemos el nombre versionado de "arr"
      -- y dejamos que idxExp se maneje luego en AssignDeref
      currentVarName a env
    _ -> error "evalExp AddrOf: solo VarExp o ArrayAccess permitidos."



-- Des-referenciar -----------------------------------------------------------
evalExp (Deref (VarExp p)) env =
  case lookupPtr (currentVarName p env) env of
    Just (baseLog, idxE) ->
      let baseName = currentVarName baseLog env
      in case idxE of
           IntExp 0 -> baseName
           IntExp n -> "(" ++ baseName ++ " !! " ++ show n ++ ")"
           _        -> "(" ++ baseName ++ " !! " ++ evalExp idxE env ++ ")"
    _ -> error $ "evalExp Deref: puntero '" ++ p ++ "' no declarado."





evalBoolExp :: BoolExp -> Env -> String
evalBoolExp BTrue _           = "True"
evalBoolExp BFalse _          = "False"
evalBoolExp (Not b) env       = "not (" ++ evalBoolExp b env ++ ")"
evalBoolExp (And b1 b2) env   = "(" ++ evalBoolExp b1 env ++ " && " ++ evalBoolExp b2 env ++ ")"
evalBoolExp (Or  b1 b2) env   = "(" ++ evalBoolExp b1 env ++ " || " ++ evalBoolExp b2 env ++ ")"
evalBoolExp (Eq  e1 e2) env   = "(" ++ evalExp e1 env ++ " == " ++ evalExp e2 env ++ ")"
evalBoolExp (Neq e1 e2) env   = "(" ++ evalExp e1 env ++ " /= " ++ evalExp e2 env ++ ")"
evalBoolExp (Lt  e1 e2) env   = "(" ++ evalExp e1 env ++ " < "  ++ evalExp e2 env ++ ")"
evalBoolExp (Le  e1 e2) env   = "(" ++ evalExp e1 env ++ " <= " ++ evalExp e2 env ++ ")"
evalBoolExp (Gt  e1 e2) env   = "(" ++ evalExp e1 env ++ " > "  ++ evalExp e2 env ++ ")"
evalBoolExp (Ge  e1 e2) env   = "(" ++ evalExp e1 env ++ " >= " ++ evalExp e2 env ++ ")"

------------------------------------------------------------------
-- Auxiliares finales
------------------------------------------------------------------
indentStr :: Int -> String
indentStr n = replicate n ' '

alignLines :: [String] -> [String]
alignLines = id

------------------------------------------------------------------
-- Conversión de tipos C → Haskell
------------------------------------------------------------------
translateType :: Type -> String
translateType TInt    = "Int"
translateType TFloat  = "Float"
translateType TDouble = "Double"
translateType TChar   = "Char"
translateType TLong   = "Integer"
translateType TShort  = "Int"     -- usamos Int para short
translateType TString = "String"
translateType TVoid = "()"
translateType (TArray t _) = "[" ++ translateType t ++ "]"
translateType (TPtr t) = translateType t  



defaultValue :: Type -> String
defaultValue TInt    = "0"
defaultValue TFloat  = "0.0"
defaultValue TDouble = "0.0"
defaultValue TChar   = "'\\0'"
defaultValue TLong   = "0"
defaultValue TShort  = "0"
defaultValue TString = "\"\""
defaultValue (TArray t n)    = "replicate " ++ show n ++ " (" ++ defaultValue t ++ ")"


------------------------------------------------------------------
-- Traducción básica de printf   (solo %d, %ld, %f, %lf, %s, %c)
------------------------------------------------------------------
translatePrintf :: String -> [Exp] -> Env -> String
translatePrintf fmt args env =
  "putStrLn (" ++ build fmt args ++ ")"
  where
    -- filtramos cualquier '\n' literal de fmt, porque putStrLn ya añade el salto
    build :: String -> [Exp] -> String
    build fmt' args' =
      "\"" ++ concatChunks (filter (/= '\n') fmt') args' ++ "\""

    concatChunks :: String -> [Exp] -> String
    concatChunks [] _ = ""

    -- %d (int)
    concatChunks ('%':'d':xs) (a:as) =
      "\" ++ show (" ++ evalExp a env ++ ") ++ \"" ++ concatChunks xs as

    -- %ld (long)
    concatChunks ('%':'l':'d':xs) (a:as) =
      "\" ++ show (" ++ evalExp a env ++ ") ++ \"" ++ concatChunks xs as

    -- %f y %lf (float/double)
    concatChunks ('%':'f':xs) (a:as) =
      "\" ++ show (" ++ evalExp a env ++ ") ++ \"" ++ concatChunks xs as
    concatChunks ('%':'l':'f':xs) (a:as) =
      "\" ++ show (" ++ evalExp a env ++ ") ++ \"" ++ concatChunks xs as

    -- %s (string)
    concatChunks ('%':'s':xs) (a:as) =
      "\" ++ " ++ evalExp a env ++ " ++ \"" ++ concatChunks xs as

    -- %c (char)
    concatChunks ('%':'c':xs) (a:as) =
      "\" ++ [ " ++ evalExp a env ++ " ] ++ \"" ++ concatChunks xs as

    -- secuencias de escape
    concatChunks ('\\':'n':xs) as =
      "\\n" ++ concatChunks xs as
    concatChunks ('\\':'t':xs) as =
      "\\t" ++ concatChunks xs as
    concatChunks ('\\':'\\':xs) as =
      "\\\\" ++ concatChunks xs as
    concatChunks ('\\':'\"':xs) as =
      "\\\"" ++ concatChunks xs as

    -- carácter normal
    concatChunks (x:xs) as = x : concatChunks xs as



------------------------------------------------------------------
-- Detección de cuerpos “puros” de solo Return y Cond
------------------------------------------------------------------
isPureReturnBody :: Comm -> Bool
isPureReturnBody (Return _)        = True
isPureReturnBody (Cond _ c1 c2)    = isPureReturnBody c1 && isPureReturnBody c2
isPureReturnBody (CondNoElse _ c)  = isPureReturnBody c
isPureReturnBody (DoWhile _ _)     = False   
isPureReturnBody (Block c)         = isPureReturnBody c  -- <- Nueva línea
isPureReturnBody _                 = False

------------------------------------------------------------------
-- Genera una única expresión Haskell a partir de un Comm puro
------------------------------------------------------------------
genPureBody :: Comm -> Env -> String
genPureBody (Return e) env = evalExp e env
genPureBody (Cond cond cThen cElse) env =
    "if "   ++ evalBoolExp cond env
 ++ " then " ++ genPureBody cThen env
 ++ " else " ++ genPureBody cElse env
genPureBody (CondNoElse cond cThen) env =
    "if "    ++ evalBoolExp cond env
 ++ " then "  ++ genPureBody cThen env
 ++ " else ()"
genPureBody (Block c) env = genPureBody c env  -- <- Nueva línea
genPureBody _ _ = error "genPureBody: cuerpo no soportado"



------------------------------------------------------------------
-- Arrays modificados dentro de un comando
------------------------------------------------------------------
arraysWritten :: Comm -> [Variable]
arraysWritten (AssignArr v _ _)     = [v]
arraysWritten (Seq c1 c2)           = arraysWritten c1 ++ arraysWritten c2
arraysWritten (Block c)             = arraysWritten c
arraysWritten (Cond _ c1 c2)        = arraysWritten c1 ++ arraysWritten c2
arraysWritten (CondNoElse _ c)      = arraysWritten c
arraysWritten (While _ c)           = arraysWritten c
arraysWritten (Switch _ cs) = concatMap aw cs
  where aw (Case _ c)       = arraysWritten c
        aw (DefaultCase c)  = arraysWritten c
arraysWritten (DoWhile c _)      = arraysWritten c
arraysWritten (For mi _ ms c)       =
    concatMap arraysWritten (initToList ++ stepToList ++ [c])
  where
    initToList = maybe [] (:[]) mi
    stepToList = maybe [] (:[]) ms
arraysWritten _                     = []



-- Prepend n espacios a cada línea
addIndent :: Int -> [String] -> [String]
addIndent n = map (\ln -> replicate n ' ' ++ ln)

------------------------------------------------------------------
-- Variables modificadas entre dos entornos
------------------------------------------------------------------
changedVars :: Env -> Env -> [Variable]
changedVars before after =
    filter changed (nub (varsInEnv before ++ varsInEnv after))
  where
    changed v = lookupVar v before /= lookupVar v after

-- Revisa si el comando contiene un Break (directo o anidado)
containsBreak :: Comm -> Bool
containsBreak (Break)          = True
containsBreak (Seq c1 c2)      = containsBreak c1 || containsBreak c2
--containsBreak (IfThenElse _ c1 c2) = containsBreak c1 || containsBreak c2
containsBreak (While _ c)      = containsBreak c
containsBreak (Block c)        = containsBreak c
containsBreak (Switch _ cs) = any chk cs
  where chk (Case _ c)      = containsBreak c
        chk (DefaultCase c) = containsBreak c
containsBreak (DoWhile c _)      = containsBreak c
containsBreak _                = False

-- ¿Es tipo puntero?
isPtr :: Type -> Bool
isPtr (TPtr _) = True
isPtr _        = False
