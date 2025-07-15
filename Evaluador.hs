module Evaluador where

import AST
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Control.Monad.State (StateT, get, gets, modify, put, runStateT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array.IO (IOArray)
import Data.Array.IO qualified as A
import Data.IORef
import Data.Int (Int32)
import Data.List (intercalate, isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER ENV
--------------------------------------------------------------------------------
-- Cada variable lógica se asocia al nombre de la IORef que la contiene
type Binding = String

type Gen = StateT Env (Writer [String]) -- Gen a  ≡  Env -> (a, Env, [String])

type Scope = Map Variable Binding

data LoopCtrl a = Continue | LoopBreak | LoopReturn a
  deriving (Show, Eq)

data Env = Env
  { scopes :: [Scope], -- pila de scopes anidados
    counter :: Int, -- para nombres de IORef
    tmpCounter :: Int, -- para nombres de temporales
    typeInfo :: Map Variable Type, -- tipos de cada var lógica
    retSlot :: Maybe String, -- ⇐ *** NUEVO ***  IORef (Maybe a) p/ return
    currentFnType :: Maybe Type,
    breakStack :: [String] -- ← pila de IORefs Bool, uno por bucle
  }

emptyEnv :: Env
emptyEnv = Env [M.empty] 0 0 M.empty Nothing Nothing []

-- | Devuelve la IORef donde se almacena el valor de ‘return’, si existe.
getRetSlot :: Gen (Maybe String)
getRetSlot = gets retSlot

emit :: String -> Gen ()
emit ln = tell [ln]

continue_ :: Gen (LoopCtrl ())
continue_ = return Continue

-- Helpers BREAK
pushBreakRef :: String -> Gen ()
pushBreakRef r = modify $ \e -> e {breakStack = r : breakStack e}

popBreakRef :: Gen ()
popBreakRef = modify $ \e -> e {breakStack = tail (breakStack e)}

getBreakRef :: Gen String -- falla si hay ‘break’ fuera de bucle
getBreakRef = do
  stk <- gets breakStack
  case stk of
    (r : _) -> pure r
    [] -> error "‘break’ fuera de cualquier bucle"

-- | Devuelve el IORef (como String) que representa un lvalue
getRefOfLValue :: Exp -> Int -> Gen String
getRefOfLValue (VarExp v) _ = lookupVarM v

getRefOfLValue (Deref e) ind = evalExp e ind   -- ¡ya ES la IORef base!

getRefOfLValue (ArrayAccess arr idxExp) ind = do
  arrRef <- lookupVarM arr
  idxTok <- evalExp idxExp ind
  tmpArr <- freshTmp
  tmpVal <- freshTmp
  emit (indentStr ind ++ tmpArr ++ " <- readIORef " ++ arrRef)
  emit (indentStr ind ++ tmpVal ++ " <- A.readArray " ++ tmpArr ++ " (" ++ idxTok ++ ")")
  -- creamos una nueva IORef con ese valor
  tmpRef <- freshTmp
  emit (indentStr ind ++ tmpRef ++ " <- newIORef " ++ tmpVal)
  pure tmpRef

getRefOfLValue other _ =
  error $ "getRefOfLValue: expresión no válida como lvalue → " ++ show other


-- Helper para no poner guardia delante de una declaracion
-- ¿El comando sólo declara variables (sin ejecutar código luego)?
isDecl :: Comm -> Bool
isDecl (LetType (TArray _ _) _ _) = True
isDecl (LetType _ _ _) = True
isDecl (LetUninit (TArray _ _) _) = True
isDecl (LetUninit _ _) = True
isDecl (Seq a b) = isDecl a && isDecl b
isDecl _ = False

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER HELPER ENV
--------------------------------------------------------------------------------
pushScope, popScope :: Gen ()
pushScope = modify $ \e -> e {scopes = M.empty : scopes e}
popScope = modify $ \e ->
  case scopes e of
    [] -> error "popScope: pila vacía"
    [_] -> e
    (_ : xs) -> e {scopes = xs}

lookupVar :: Variable -> Env -> Maybe Binding
lookupVar v = foldr (\m acc -> M.lookup v m <|> acc) Nothing . scopes

lookupVarM :: Variable -> Gen String
lookupVarM v = do
  mb <- gets (lookupVar v)
  case mb of
    Nothing -> error $ "Variable no declarada: " ++ v
    Just ref -> pure ref

freshName :: Variable -> Gen String
freshName v = do
  n <- gets counter
  modify $ \e -> e {counter = n + 1}
  pure (v ++ "_ref" ++ show n)

declareVar :: Variable -> Type -> Gen String
declareVar v t = do
  ref <- freshName v
  modify $ \e ->
    let (top : rest) = scopes e
     in e
          { scopes = M.insert v ref top : rest,
            typeInfo = M.insert v t (typeInfo e)
          }
  pure ref

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER EVAL
--------------------------------------------------------------------------------
{-
eval :: Comm -> IO ()
eval ast = do
  let ((_, _finalEnv), hsLines) =
        runWriter (runStateT (evalComm ast 0) emptyEnv)
  mapM_ putStrLn hsLines

alignLines :: [String] -> [String]
alignLines = id          -- por ahora no tocamos el alineador sofisticado
-}

eval :: Comm -> IO ()
eval ast = do
  let ((_, _finalEnv), raw) =
        runWriter (runStateT (evalComm ast 0) emptyEnv)

  mapM_ putStrLn (alignLines raw)

alignLines :: [String] -> [String]
alignLines lns =
  let (imports, others) = spanImports lns
   in imports ++ [""] ++ others -- dejamos un salto de línea estético
  where
    spanImports =
      foldr
        ( \ln (imps, rest) ->
            if "import " `isPrefixOf` ln
              then (ln : imps, rest)
              else (imps, ln : rest)
        )
        ([], [])

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER EVALCOMM
--------------------------------------------------------------------------------
evalComm :: Comm -> Int -> Gen (LoopCtrl ())
-- 4.1  skip
evalComm Skip _ = continue_
-- -------------------------------------------------------------------
--  Definición de main  (con slot de retorno unificado)
-- -------------------------------------------------------------------
evalComm (FuncDef retT "main" _ body) ind = do
  -- imports principales (una sola vez)
  emit ""
  emit "import Data.IORef"
  emit "import Control.Monad (when)"
  emit "import qualified Data.Array.IO as A"
  emit "import Data.Maybe (isJust)"
  emit "import Data.Int (Int32)"
  emit ""
  emit "main :: IO ()"
  emit "main = do"

  -- 0) registrar el tipo de la función actual
  oldTy <- gets currentFnType
  modify $ \e -> e {currentFnType = Just retT}

  -- 1) slot de retorno (bandera)
  retRef <- freshTmp
  emit
    ( indentStr (ind + 2)
        ++ retRef
        ++ " <- newIORef (Nothing :: Maybe "
        ++ translateType retT
        ++ ")"
    )

  -- 2) enlazar el slot en el entorno
  oldSlot <- gets retSlot
  modify $ \e -> e {retSlot = Just retRef}

  -- 3) cuerpo real del programa
  _ <- evalComm body (ind + 2)

  -- 4) restaurar entorno
  modify $ \e -> e {retSlot = oldSlot, currentFnType = oldTy}

  -- 5) fin
  emit (indentStr (ind + 2) ++ "return ()")
  continue_

-- -------------------------------------------------------------------
-- Funciones definidas por el usuario  (no main)
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--  Funciones definidas por el usuario  (≠ main)
-- -------------------------------------------------------------------
evalComm (FuncDef retT name params body) ind
  | name /= "main" = do
      -- 1) firma de tipo y cabecera
      let paramTys = map (translateType . fst) params
          retTyStr = if retT == TVoid then "()" else translateType retT
          typeSig =
            name
              ++ " :: "
              ++ concatMap (++ " -> ") paramTys
              ++ "IO "
              ++ retTyStr
      emit ""
      emit typeSig
      emit (name ++ " " ++ unwords (map snd params) ++ " = do")

      -- 2) scope léxico propio
      pushScope

      -- 3) parámetros en IORefs
      forM_ params $ \(t, v) -> do
        ref <- declareVar v t
        emit (indentStr (ind + 2) ++ ref ++ " <- newIORef " ++ v)

      -- 4) preparar slot de retorno (siempre, también en void)
      oldSlot <- gets retSlot
      oldTy <- gets currentFnType
      modify $ \e -> e {currentFnType = Just retT}

      retRef <- freshTmp
      let slotTy = if retT == TVoid then "()" else translateType retT
      emit
        ( indentStr (ind + 2)
            ++ retRef
            ++ " <- newIORef (Nothing :: Maybe "
            ++ slotTy
            ++ ")"
        )
      modify $ \e -> e {retSlot = Just retRef}

      -- 5) cuerpo de la función
      resBody <- evalComm body (ind + 2)

      -- 6) epílogo: devolver valor o unit
      case retT of
        TVoid -> emit (indentStr (ind + 2) ++ "return ()")
        _ -> do
          emit (indentStr (ind + 2) ++ "mVal <- readIORef " ++ retRef)
          emit (indentStr (ind + 2) ++ "case mVal of")
          emit (indentStr (ind + 4) ++ "Just v  -> return v")
          emit
            ( indentStr (ind + 4)
                ++ "Nothing -> error \"\\\""
                ++ name
                ++ "\\\" terminó sin ejecutar return\""
            )

      -- 7) restaurar entorno y cerrar scope
      modify $ \e -> e {retSlot = oldSlot, currentFnType = oldTy}
      popScope

      -- 8) propagar control
      case resBody of
        LoopReturn _ -> continue_
        LoopBreak -> error "break fuera de un bucle"
        Continue -> continue_
evalComm (LetType (TArray t size) v (InitList xs)) ind = do
  ref <- declareVar v t

  -- completamos la lista con ceros si es más corta
  let paddExps = replicate (size - length xs) (defaultExp t)
      allExps = xs ++ paddExps

  -- obtenemos los tokens literales, sin tipo
  toks <- mapM (`evalExp` ind) allExps

  -- lista literal SÓLO con [1,2,3,4,5], sin ":: [Int]"
  let listTok = "[" ++ intercalate "," toks ++ "]"

  tmpArr <- freshTmp
  emit
    ( indentStr ind
        ++ tmpArr
        ++ " <- (A.newListArray ((0 :: Int32),"
        ++ show (size - 1)
        ++ " :: Int32) "
        ++ listTok
        ++ " :: IO (A.IOArray Int32 "
        ++ translateType t
        ++ "))"
    )
  emit (indentStr ind ++ ref ++ " <- newIORef " ++ tmpArr)

  continue_

-- int arr[5] = 7;
evalComm (LetType (TArray t size) v rhs) ind = do
  ref <- declareVar v t
  rhsTok <- evalExp rhs ind
  arrTmp <- genNewArray t size rhsTok ind
  emit (indentStr ind ++ ref ++ " <- newIORef " ++ arrTmp)
  continue_

-- 4.2  declaración con inicializador:  int x = 5;
evalComm (LetType t v rhs) ind = do
  ref <- declareVar v t
  rhsT <- evalExp rhs ind
  emit (indentStr ind ++ ref ++ " <- newIORef (" ++ rhsT ++ ")")
  continue_

-- int arr[5];   (sin inicializador)
evalComm (LetUninit (TArray t size) v) ind = do
  ref <- declareVar v t
  let zeroVal = defaultInit t -- «0 :: Int», etc.
  arrTmp <- genNewArray t size zeroVal ind
  emit (indentStr ind ++ ref ++ " <- newIORef " ++ arrTmp)
  continue_

-- int x; o char nombre[100];  (sin inicializador)
evalComm (LetUninit t v) ind = do
  let t' = case t of
        TArray TChar _ -> TString -- Si es char[], lo tomamos como string
        _ -> t
  ref <- declareVar v t'
  let initVal = defaultInit t'
  emit (indentStr ind ++ ref ++ " <- newIORef " ++ initVal)
  continue_
evalComm (Assign v rhs) ind = do
  ref <- lookupVarM v
  rhsT <- evalExp rhs ind
  emit (indentStr ind ++ "writeIORef " ++ ref ++ " (" ++ rhsT ++ ")")
  continue_
evalComm (AssignArr arrVar idxExp rhsExp) ind = do
  refTok <- lookupVarM arrVar
  idxTok <- evalExp idxExp ind
  rhsTok <- evalExp rhsExp ind
  tmpArr <- freshTmp
  emit (indentStr ind ++ tmpArr ++ " <- readIORef " ++ refTok)
  emit
    ( indentStr ind
        ++ "A.writeArray "
        ++ tmpArr
        ++ " ("
        ++ idxTok
        ++ ")"
        ++ " ("
        ++ rhsTok
        ++ ")"
    )
  continue_
evalComm (ExprStmt e) ind = case e of
  CallExp fn args -> do
    toks <- mapM (\a -> evalExp a ind) args
    emit (indentStr ind ++ "_ <- " ++ unwords (fn : toks))
    continue_
  _ -> do
    _ <- evalExp e ind
    continue_

-- -------------------------------------------------------------------
--  Return expr  –  sólo escribe cuando todavía no hay valor guardado
-- -------------------------------------------------------------------
evalComm (Return expr) ind = do
  tok <- evalExp expr ind -- se evalúa siempre (efectos)
  mSlot <- getRetSlot -- IORef donde guardamos el flag
  mTy <- gets currentFnType

  case mSlot of
    Nothing ->
      -- impos-tile (return fuera de función)
      emit (indentStr ind ++ "error \"return fuera de contexto\"")
    Just ref -> do
      -- ¿qué guardamos: valor o ()?
      let valTok = case mTy of
            Just TVoid -> "()" -- función void → sólo flag
            _ -> tok
      emit (indentStr ind ++ "mAlready <- readIORef " ++ ref)
      emit (indentStr ind ++ "case mAlready of")
      emit (indentStr (ind + 2) ++ "Just _  -> pure ()")
      emit
        ( indentStr (ind + 2)
            ++ "Nothing -> writeIORef "
            ++ ref
            ++ " (Just ("
            ++ valTok
            ++ "))"
        )

  -- Propagamos el control para que callers sepan que hubo ‘return’
  return (LoopReturn ())

-- -------------------------------------------------------------------
--  Return   (sin valor)   ⇒   escribe () en el slot, si corresponde
-- -------------------------------------------------------------------
evalComm ReturnVoid ind = do
  mSlot <- getRetSlot
  case mSlot of
    ------------------------------------------------------------------
    -- return; FUERA de cualquier función (ilegal en C).
    -- Generamos “error …” para abortar en tiempo de ejecución.
    ------------------------------------------------------------------
    Nothing -> do
      emit (indentStr ind ++ "error \"return fuera de contexto\"")
      -- avisamos al caller que hubo return (aunque nunca debería volver)
      pure (LoopReturn ())

    ------------------------------------------------------------------
    -- return; DENTRO de una función
    ------------------------------------------------------------------
    Just ref -> do
      emit (indentStr ind ++ "mAlready <- readIORef " ++ ref)
      emit (indentStr ind ++ "case mAlready of")
      emit (indentStr (ind + 2) ++ "Just _  -> pure ()")
      emit
        ( indentStr (ind + 2)
            ++ "Nothing -> writeIORef "
            ++ ref
            ++ " (Just ())"
        )
      -- propagamos el control: LoopReturn lleva siempre un valor (aquí ())
      pure (LoopReturn ())

--------------------------------------------------------------------
-- c1 ; c2   – ejecuta c2 sólo si no hubo break ni return en runtime
--------------------------------------------------------------------
evalComm (Seq c1 c2) ind = do
  -- 1)  genera c1 y memoriza su control estático
  r1 <- evalComm c1 ind

  -- 2)  refs para checkear break / return en runtime
  mBrk <- gets breakStack -- [] si no estamos dentro de un bucle
  mRet <- getRetSlot -- Nothing si función void / en main

  -- 3)  cabecera del guardia
  let guardHeader = case (mBrk, mRet) of
        (b : _, Just r) ->
          -- break + return
          [ indentStr ind ++ "quit <- readIORef " ++ b,
            indentStr ind ++ "done <- fmap isJust (readIORef " ++ r ++ ")",
            indentStr ind ++ "when (not quit && not done) $ do"
          ]
        (b : _, Nothing) ->
          -- sólo break
          [ indentStr ind ++ "quit <- readIORef " ++ b,
            indentStr ind ++ "when (not quit) $ do"
          ]
        (_, Just r) ->
          -- sólo return
          [ indentStr ind ++ "done <- fmap isJust (readIORef " ++ r ++ ")",
            indentStr ind ++ "when (not done) $ do"
          ]
        _ -> [] -- nada que chequear
      inner = ind + 2 -- indent interno
      -- NO ponemos la guardia si alguno de los dos lados es solo declaración
      needGuard = (not . null) guardHeader && not (isDecl c2) && not (isSkip c2)

  -- 4)  abre guardia si hace falta
  when needGuard $
    mapM_ emit guardHeader

  -- 5) genera c2 …
  r2 <- evalComm c2 (if needGuard then inner else ind)

  -- 6)  cierra guardia
  when needGuard $
    emit (indentStr ind ++ "-- fin guardia")

  -- 7)  combina controles estáticos
  return (mergeCtrl r1 r2)
  where
    mergeCtrl (LoopReturn _) _ = LoopReturn ()
    mergeCtrl LoopBreak (LoopReturn _) = LoopReturn ()
    mergeCtrl LoopBreak _ = LoopBreak
    mergeCtrl _ LoopBreak = LoopBreak
    mergeCtrl _ _ = Continue

-- ---------------------------------------------------------------
-- break;            (sólo válido dentro de un while / for / do…)
-- ---------------------------------------------------------------
evalComm Break ind = do
  brkRef <- getBreakRef -- IORef Bool del bucle actual
  emit (indentStr ind ++ "writeIORef " ++ brkRef ++ " True")
  return LoopBreak -- ← avisa al while exterior

-- Scanf
evalComm (Scanf _ exps) ind = do
  tmp <- freshTmp
  emit (indentStr ind ++ tmp ++ " <- getLine")
  emit (indentStr ind ++ "let ws = words " ++ tmp)
  forM_ (zip exps [0 ..]) $ \(exp, i) -> case exp of
    VarExp v -> do
      ref <- lookupVarM v
      mty <- gets (M.lookup v . typeInfo)
      let expr = case mty of
            Just TChar -> "head (ws !! " ++ show i ++ ")"
            Just TInt -> "read (ws !! " ++ show i ++ ") :: Int32"
            Just TFloat -> "read (ws !! " ++ show i ++ ") :: Float"
            Just TDouble -> "read (ws !! " ++ show i ++ ") :: Double"
            Just TString -> "ws !! " ++ show i
            _ -> "read (ws !! " ++ show i ++ ") :: Int32"
      emit (indentStr ind ++ "writeIORef " ++ ref ++ " (" ++ expr ++ ")")
    ArrayAccess arr idxExp -> do
      refArr <- lookupVarM arr
      idxTok <- evalExp idxExp ind
      tmpArr <- freshTmp
      emit (indentStr ind ++ tmpArr ++ " <- readIORef " ++ refArr)
      -- Por ahora asumimos que el array es de Int, pero podrías ajustar por tipo si querés
      emit
        ( indentStr ind
            ++ "A.writeArray "
            ++ tmpArr
            ++ " "
            ++ idxTok
            ++ " (read (ws !! "
            ++ show i
            ++ ") :: Int32)"
        )
    _ -> error "scanf: solo soporta variables simples o acceso tipo arr[i]"
  continue_

-- 4.5  bloque con scope { … }
evalComm (Block body) ind = do
  pushScope
  res <- evalComm body ind
  popScope
  return res -- propaga lo que venga de adentro (Continue, LoopBreak o LoopReturn)

-- Printf
evalComm (Printf fmt args) ind = do
  argChunks <-
    mapM
      ( \a -> do
          tok <- evalExp a ind
          pres <- gets (const []) -- evalExp ya emite sus pres con 'emit'; no devuelve nada
          pure ([], tok)
      )
      args -- pres vacíos porque los emitió evalExp
  let (presLines, putLn) = translatePrintf ind fmt argChunks
  mapM_ emit presLines
  emit putLn
  continue_

-- evalComm COND
evalComm (Cond cond cThen Skip) ind = do
  tok <- evalBoolExp cond ind
  emit $ indStr ++ "when (" ++ tok ++ ") $ do"
  pushScope
  _ <- evalComm cThen (ind + 2) -- ← ignoramos el resultado
  popScope
  continue_ -- ← SIEMPRE Continue
  where
    indStr = indentStr ind

-- A) if … con else  (cElse NO es Skip)
evalComm (Cond cond cThen cElse) ind | not (isSkip cElse) = do
  tok <- evalBoolExp cond ind
  emit $ indStr ++ "if " ++ tok ++ " then do"
  pushScope
  rThen <- evalComm cThen (ind + 2)
  popScope
  emit $ indStr ++ "else do"
  pushScope
  rElse <- evalComm cElse (ind + 2)
  popScope
  -- propagá lo que corresponda
  case (rThen, rElse) of
    (LoopReturn _, _) -> return (LoopReturn ())
    (_, LoopReturn _) -> return (LoopReturn ())
    (LoopBreak, _) -> return LoopBreak
    (_, LoopBreak) -> return LoopBreak
    _ -> continue_
  where
    indStr = indentStr ind

--------------------------------------------------------------------
-- while (cond) { body }   – controla ‘return’ y ‘break’
--------------------------------------------------------------------
evalComm (While cond body) ind = do
  loop <- freshTmp -- nombre del closure recursivo
  brkRef <- freshTmp -- IORef Bool  ← flag de break
  mRet <- getRetSlot -- IORef (Maybe a) o Nothing

  -- 0) flag break inicial a False
  emit (indentStr ind ++ brkRef ++ " <- newIORef False")
  pushBreakRef brkRef -- lo empujamos al stack

  -- 1) let loop = do
  let header = "let " ++ loop ++ " = do"
  emit (indentStr ind ++ header)
  let inner = ind + length header + 1 -- +1 por espacio
      deep x = x + 2

  -- 2) chequeos previos
  emit (indentStr inner ++ "quit <- readIORef " ++ brkRef)
  case mRet of
    Just r -> emit (indentStr inner ++ "done <- fmap isJust (readIORef " ++ r ++ ")")
    Nothing -> emit (indentStr inner ++ "let done = False")

  -- 3) condición del while
  cTok <- evalBoolExp cond inner
  emit
    ( indentStr inner
        ++ "when (not quit && not done && ("
        ++ cTok
        ++ ")) $ do"
    )

  -- 4) cuerpo
  pushScope
  res <- evalComm body (deep inner) -- ⬅  bind en ‘res’
  popScope

  -- 5) actuar según el resultado del cuerpo
  case res of
    LoopReturn _ -> pure () -- corta todo
    _ -> emit (indentStr (deep inner) ++ loop)

  -- 6) lanzar la primera llamada y salir del stack
  emit (indentStr ind ++ loop)
  popBreakRef
  continue_

--------------------------------------------------------------------
-- for (init; cond; step) { body }   – motor exacto, break/return
--------------------------------------------------------------------
evalComm (For mInit cond mStep body) ind = do
  -- 0) scope propio del for
  pushScope

  -- 1) ejecutamos ‘init’ si existe
  _ <- maybe continue_ (\c -> evalComm c ind) mInit

  -- 2) preámbulo break/return
  loop <- freshTmp
  brkRef <- freshTmp
  mRet <- getRetSlot
  emit (indentStr ind ++ brkRef ++ " <- newIORef False")
  pushBreakRef brkRef

  let header = "let " ++ loop ++ " = do"
  emit (indentStr ind ++ header)
  let inner = ind + length header + 1
      deep = inner + 2

  -- 3) chequeos quit/done y condición
  emit (indentStr inner ++ "quit <- readIORef " ++ brkRef)
  case mRet of
    Just r ->
      emit
        ( indentStr inner
            ++ "done <- fmap isJust (readIORef "
            ++ r
            ++ ")"
        )
    Nothing -> emit (indentStr inner ++ "let done = False")

  cTok <- evalBoolExp cond inner
  emit
    ( indentStr inner
        ++ "when (not quit && not done && ("
        ++ cTok
        ++ ")) $ do"
    )

  -- 4) body + step + recursión, todos dentro del guardia
  pushScope
  resBody <- evalComm body (deep)
  popScope

  case resBody of
    LoopReturn _ -> pure () -- corta todo
    LoopBreak -> pure () -- quit=True ya marcado
    Continue -> do
      _ <- maybe continue_ (\s -> evalComm s deep) mStep
      emit (indentStr deep ++ loop) -- recursión

  -- 5) primera llamada + limpieza
  emit (indentStr ind ++ loop)
  popBreakRef
  popScope
  continue_

--------------------------------------------------------------------
-- do { body } while (cond);        – controla break / return
--------------------------------------------------------------------
evalComm (DoWhile body cond) ind = do
  loop <- freshTmp -- nombre del cierre recursivo  (tmpN)
  brkRef <- freshTmp -- IORef Bool  ← flag de break
  mRet <- getRetSlot -- IORef (Maybe a) o Nothing

  -- 0) flag break inicial a False
  emit (indentStr ind ++ brkRef ++ " <- newIORef False")
  pushBreakRef brkRef -- lo subimos al stack

  -- 1) let loop = do …
  let header = "let " ++ loop ++ " = do"
  emit (indentStr ind ++ header)
  let inner = ind + length header + 1 -- indent dentro del let
      deep = inner + 2 -- indent para cuerpo

  -- 2) chequeos quit / done antes de ejecutar el cuerpo
  emit (indentStr inner ++ "quit <- readIORef " ++ brkRef)
  case mRet of
    Just r ->
      emit
        ( indentStr inner
            ++ "done <- fmap isJust (readIORef "
            ++ r
            ++ ")"
        )
    Nothing -> emit (indentStr inner ++ "let done = False")

  emit (indentStr inner ++ "when (not quit && not done) $ do")

  -- 3) cuerpo del do-while
  pushScope
  res <- evalComm body deep -- ← puede devolver LoopBreak / LoopReturn
  popScope

  -- 4) si no hubo ‘return’ se evalúa la condición y, si procede, se recurre
  case res of
    LoopReturn _ -> pure () -- nada más: se sale del bucle
    _ -> do
      emit (indentStr deep ++ "quit <- readIORef " ++ brkRef)
      case mRet of
        Just r ->
          emit
            ( indentStr deep
                ++ "done <- fmap isJust (readIORef "
                ++ r
                ++ ")"
            )
        Nothing -> emit (indentStr deep ++ "let done = False")
      cTok <- evalBoolExp cond deep
      emit
        ( indentStr deep
            ++ "when (not quit && not done && ("
            ++ cTok
            ++ ")) $ do"
        )
      emit (indentStr (deep + 2) ++ loop) -- recursión

  -- 5) primera llamada al cierre y limpieza
  emit (indentStr ind ++ loop)
  popBreakRef
  continue_

--------------------------------------------------------
--  switch (expr) { … }
--------------------------------------------------------
evalComm (Switch scrut sections) ind = do
  ------------------------------------------------------------------
  -- 0)  Evaluamos la expresión discriminante  **una sola vez**
  ------------------------------------------------------------------
  scrTok <- evalExp scrut ind -- token con el valor

  ------------------------------------------------------------------
  -- 1)  IORefs internas
  --     • brkRef    → flag de break
  --     • matched   → ya hubo match (para fall-through)
  --     • mRet      → IORef (Maybe a) si la función puede ‘return’
  ------------------------------------------------------------------
  brkRef <- freshTmp
  matchedRef <- freshTmp
  emit (indentStr ind ++ brkRef ++ " <- newIORef False")
  emit (indentStr ind ++ matchedRef ++ " <- newIORef False")
  pushBreakRef brkRef

  mRet <- getRetSlot -- Nothing en ‘main’ o funciones void

  ------------------------------------------------------------------
  -- 2)  Generador de una única sección  (Case / DefaultCase)
  ------------------------------------------------------------------
  let genCase :: Case -> Gen (LoopCtrl ())
      --------------------------------------------------------------
      genCase (Case lbl body) = do
        lblTok <- evalExp lbl ind
        emit (indentStr ind ++ "quit <- readIORef " ++ brkRef)

        -- Re-leemos ‘done’ antes de CADA sección
        case mRet of
          Just r ->
            emit
              ( indentStr ind
                  ++ "done <- fmap isJust (readIORef "
                  ++ r
                  ++ ")"
              )
          Nothing -> emit (indentStr ind ++ "let done = False")

        emit (indentStr ind ++ "matched <- readIORef " ++ matchedRef)
        emit
          ( indentStr ind
              ++ "when (not quit && not done && (matched || ("
              ++ scrTok
              ++ " == "
              ++ lblTok
              ++ "))) $ do"
          )
        emit (indentStr (ind + 2) ++ "writeIORef " ++ matchedRef ++ " True")

        pushScope
        rc <- evalComm body (ind + 2) -- puede devolver LoopReturn
        popScope
        pure rc
      --------------------------------------------------------------
      genCase (DefaultCase body) = do
        emit (indentStr ind ++ "quit <- readIORef " ++ brkRef)

        case mRet of
          Just r ->
            emit
              ( indentStr ind
                  ++ "done <- fmap isJust (readIORef "
                  ++ r
                  ++ ")"
              )
          Nothing -> emit (indentStr ind ++ "let done = False")

        emit (indentStr ind ++ "matched <- readIORef " ++ matchedRef)
        emit
          ( indentStr ind
              ++ "when (not quit && not done && not matched) $ do"
          )

        pushScope
        rc <- evalComm body (ind + 2)
        popScope
        pure rc

      --------------------------------------------------------------
      -- 3)  Recorremos todas las secciones.
      --     allRet == True  ↔  todas las ramas vistas terminan con return
      --------------------------------------------------------------
      walk :: [Case] -> Bool -> Gen (LoopCtrl ())
      walk [] allRet =
        if allRet then pure (LoopReturn ()) else continue_
      walk (c : rest) allRet = do
        rc <- genCase c
        let allRet' =
              allRet && case rc of
                LoopReturn _ -> True
                _ -> False
        walk rest allRet'

  res <- walk sections True

  ------------------------------------------------------------------
  -- 4)  Limpieza y retorno
  ------------------------------------------------------------------
  popBreakRef
  return res

evalComm (AssignDeref lhs rhs) ind = do
  -- obtener referencia al lvalue (que puede ser *p o arr[i])
  ref <- getRefOfLValue lhs ind
  rhsTok <- evalExp rhs ind
  emit (indentStr ind ++ "writeIORef " ++ ref ++ " (" ++ rhsTok ++ ")")
  continue_



-- 4.6  cualquier otra construcción (if, while, etc.) → pendiente
evalComm other _ =
  error $ "evalComm: caso aún no implementado → " ++ show other

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER EVALEXP
--------------------------------------------------------------------------------

evalExp :: Exp -> Int -> Gen String
-- literales
evalExp (IntExp n) _ = pure ("(" ++ show n ++ " :: Int32)")
evalExp (FloatExp f) _ = pure (show f)
evalExp (StringExp s) _ = pure (show s)
evalExp (CharExp c) _ = pure (show c)
-- variable (ya implementado)
evalExp (VarExp v) ind = do
  ref <- lookupVarM v
  tmp <- freshTmp
  emit (indentStr ind ++ tmp ++ " <- readIORef " ++ ref)
  pure tmp

-- booleano como entero   (bool ? 1 : 0)
evalExp (BoolAsIntExp b) ind = do
  tok <- evalBoolExp b ind
  pure $ "(if " ++ tok ++ " then 1 else 0)"

-- unario -
evalExp (UMinus e) ind = do
  t <- evalExp e ind
  pure $ "(-" ++ t ++ ")"

-- aritmética binaria
evalExp (AddExp e1 e2) ind = binArith "+" e1 e2 ind
evalExp (SubExp e1 e2) ind = binArith "-" e1 e2 ind
evalExp (MulExp e1 e2) ind = binArith "*" e1 e2 ind
-- división   e1 / e2
evalExp (DivExp e1 e2) ind = do
  t1 <- evalExp e1 ind
  t2 <- evalExp e2 ind
  int1 <- isIntExp e1
  int2 <- isIntExp e2
  let cast b tok = if b then "fromIntegral " ++ tok else tok
  if int1 && int2
    then pure $ "(" ++ t1 ++ " `div` " ++ t2 ++ ")" -- caso 100 % Int
    else pure $ "(" ++ cast int1 t1 ++ " / " ++ cast int2 t2 ++ ")"
evalExp (ModExp e1 e2) ind = binArith "`mod`" e1 e2 ind
-- -------------------------------------------------------------
--  sqrt(e)          pow(base, exp)          log2(e)
-- -------------------------------------------------------------
evalExp (Sqrt e) ind = do
  t <- evalExp e ind
  isI <- isIntExp e
  let arg = if isI then "(fromIntegral " ++ t ++ ")" else t
  pure $ "(sqrt " ++ arg ++ ")"
evalExp (Pow b e) ind = do
  tb <- evalExp b ind
  te <- evalExp e ind
  ib <- isIntExp b
  ie <- isIntExp e
  let cast b tok = if b then "(fromIntegral " ++ tok ++ ")" else tok
  pure $ "(" ++ cast ib tb ++ " ** " ++ cast ie te ++ ")"
evalExp (Log2 e) ind = do
  t <- evalExp e ind
  isI <- isIntExp e
  let arg = if isI then "(fromIntegral " ++ t ++ ")" else t
  pure $ "(logBase 2 " ++ arg ++ ")"

-- ++ / --
evalExp (PostIncr v) ind = mutateVar v "+ 1" False ind
evalExp (PostDecr v) ind = mutateVar v "- 1" False ind
evalExp (PreIncr v) ind = mutateVar v "+ 1" True ind
evalExp (PreDecr v) ind = mutateVar v "- 1" True ind
evalExp (ArrayAccess arrVar idxExp) ind = do
  refTok <- lookupVarM arrVar
  idxTok <- evalExp idxExp ind
  tmpArr <- freshTmp
  valTmp <- freshTmp
  emit (indentStr ind ++ tmpArr ++ " <- readIORef " ++ refTok)
  emit
    ( indentStr ind
        ++ valTmp
        ++ " <- A.readArray "
        ++ tmpArr
        ++ " "
        ++ idxTok
    )
  pure valTmp
evalExp (InitList xs) ind = do
  toks <- mapM (`evalExp` ind) xs -- evalúa cada literal
  tmpV <- freshTmp
  let len = length xs
      body = "[" ++ intercalate "," toks ++ "]"
  emit
    ( indentStr ind
        ++ tmpV
        ++ " <- newIORef =<< A.newListArray (0,"
        ++ show (len - 1)
        ++ ") "
        ++ body
    )
  pure tmpV

-- llamada a función (aún sin IO puro/impuro)
evalExp (CallExp fn args) ind = do
  toks <- mapM (\a -> evalExp a ind) args
  tmp <- freshTmp
  emit (indentStr ind ++ tmp ++ " <- " ++ unwords (fn : toks))
  pure tmp


-- Evaluador.hs  · evalExp
evalExp (AddrOf e) ind = do
  -- devolvemos directamente la IORef del l-value,
  -- SIN envolverla en otro newIORef
  getRefOfLValue e ind


-- Evaluador.hs  · evalExp
evalExp (Deref e) ind = do
  ptr <- evalExp e ind              -- IORef base
  tmp <- freshTmp
  emit (indentStr ind ++ tmp ++ " <- readIORef " ++ ptr)
  pure tmp


-- cualquier otra expresión aún no implementada
evalExp other _ =
  error $ "evalExp: constructor aún no soportado → " ++ show other

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER EVALBOOLEXP
--------------------------------------------------------------------------------

evalBoolExp :: BoolExp -> Int -> Gen String
evalBoolExp BTrue _ = pure "True"
evalBoolExp BFalse _ = pure "False"
evalBoolExp (Not b) ind = do
  tok <- evalBoolExp b ind
  pure $ "(not " ++ tok ++ ")"

-- Operadores binarios
evalBoolExp (And b1 b2) ind = binOp "&&" b1 b2 ind
evalBoolExp (Or b1 b2) ind = binOp "||" b1 b2 ind
evalBoolExp (Eq e1 e2) ind = cmp "==" e1 e2 ind
evalBoolExp (Neq e1 e2) ind = cmp "/=" e1 e2 ind
evalBoolExp (Lt e1 e2) ind = cmp "<" e1 e2 ind
evalBoolExp (Le e1 e2) ind = cmp "<=" e1 e2 ind
evalBoolExp (Gt e1 e2) ind = cmp ">" e1 e2 ind
evalBoolExp (Ge e1 e2) ind = cmp ">=" e1 e2 ind

-- helpers
binOp op b1 b2 ind = do
  t1 <- evalBoolExp b1 ind
  t2 <- evalBoolExp b2 ind
  pure $ "(" ++ t1 ++ " " ++ op ++ " " ++ t2 ++ ")"

cmp op e1 e2 ind = do
  t1 <- evalExp e1 ind
  t2 <- evalExp e2 ind
  pure $ "(" ++ t1 ++ " " ++ op ++ " " ++ t2 ++ ")"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER HELPER EVALEXP
--------------------------------------------------------------------------------

-- | Operadores binarios aritméticos  (+ – * / mod)
binArith :: String -> Exp -> Exp -> Int -> Gen String
binArith op e1 e2 ind = do
  t1 <- evalExp e1 ind
  t2 <- evalExp e2 ind
  pure $ "(" ++ t1 ++ " " ++ op ++ " " ++ t2 ++ ")"

-- | (++ / --)   escriben la IORef y devuelven tmp con el valor viejo o nuevo
mutateVar ::
  -- | variable a mutar
  Variable ->
  -- | operador ("+ 1"  o  "- 1")
  String ->
  -- | True → pre (devuelve nuevo); False → post (devuelve viejo)
  Bool ->
  -- | indent
  Int ->
  Gen String
mutateVar v delta isPre ind = do
  ref <- lookupVarM v
  oldT <- freshTmp
  emit (indentStr ind ++ oldT ++ " <- readIORef " ++ ref)
  emit (indentStr ind ++ "writeIORef " ++ ref ++ " (" ++ oldT ++ " " ++ delta ++ ")")
  pure $
    if isPre
      then "(" ++ oldT ++ " " ++ delta ++ ")"
      else oldT

------------------------------------------------------------
-- ¿Podemos asegurar que la expresión es entera? (≈ TInt)
------------------------------------------------------------
bothInt :: Exp -> Exp -> Gen Bool
bothInt a b = (&&) <$> isIntExp a <*> isIntExp b

----------------------------------

isIntExp :: Exp -> Gen Bool
isIntExp (IntExp _) = pure True
isIntExp (VarExp v) = do
  mt <- gets (M.lookup v . typeInfo)
  pure (mt == Just TInt)
isIntExp (AddExp a b) = bothInt a b
isIntExp (SubExp a b) = bothInt a b
isIntExp (MulExp a b) = bothInt a b
isIntExp (ModExp _ _) = pure True -- “a mod b” siempre usa enteros
isIntExp _ = pure False -- caso conservador

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER HELPER PRINTF
--------------------------------------------------------------------------------

-- Traducción básica de printf (solo %d, %ld, %f, %lf, %s, %c)

-- | Genera el código Haskell correspondiente a un printf.
--   • ind   = nivel de indentación (en espacios)
--   • fmt   = literal de formato de C (ej. "El valor es %d\n")
--   • chunks= lista de pares (líneas previas, token) para cada argumento ya evaluado
--   Devuelve: (líneas previas totales, línea putStrLn lista)
translatePrintf ::
  -- | indentación
  Int ->
  -- | formato printf
  String ->
  -- | (pres, token) por argumento
  [([String], String)] ->
  -- | (pres totales, línea final)
  ([String], String)
translatePrintf ind fmt chunks =
  let pres = concatMap fst chunks
      toks = map snd chunks
      fmtClean = filter (/= '\n') fmt --  quitamos '\n'
      body = "\"" ++ build fmtClean toks --  abrimos la comilla
      ioLn = indentStr ind ++ "putStrLn (" ++ body ++ ")" -- ③ cerramos paréntesis
   in (pres, ioLn)
  where
    build :: String -> [String] -> String
    build [] _ = "\"" --  cerramos la comilla
    build ('%' : 'd' : xs) (t : ts) =
      "\" ++ show (" ++ t ++ ") ++ \"" ++ build xs ts
    build ('%' : 'l' : 'd' : xs) (t : ts) =
      "\" ++ show (" ++ t ++ ") ++ \"" ++ build xs ts
    build ('%' : 'f' : xs) (t : ts) =
      "\" ++ show (" ++ t ++ ") ++ \"" ++ build xs ts
    build ('%' : 'l' : 'f' : xs) (t : ts) =
      "\" ++ show (" ++ t ++ ") ++ \"" ++ build xs ts
    build ('%' : 's' : xs) (t : ts) =
      "\" ++ " ++ t ++ " ++ \"" ++ build xs ts
    build ('%' : 'c' : xs) (t : ts) =
      "\" ++ [ " ++ t ++ " ] ++ \"" ++ build xs ts
    build (c : cs) ts = c : build cs ts

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------VER HELPER EXTRAS
--------------------------------------------------------------------------------

-- Funcion usada en Main.hs, sirve para que el codigo generado al runearlo se ponga en un archivo .hs
generateCode :: Comm -> [String]
generateCode ast = alignLines raw
  where
    ((_, _), raw) = runWriter (runStateT (evalComm ast 0) emptyEnv)

indentStr :: Int -> String
indentStr n = replicate n ' '

freshTmp :: Gen String
freshTmp = do
  n <- gets tmpCounter
  modify $ \e -> e {tmpCounter = n + 1}
  pure ("tmp" ++ show n)

{-    estaria piola que sea como este, era mas lindo este:
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
-}

isSkip :: Comm -> Bool
isSkip Skip = True
isSkip _ = False

translateType :: Type -> String
translateType TInt = "Int32"
translateType TFloat = "Float"
translateType TDouble = "Double"
translateType TChar = "Char"
translateType TLong = "Integer"
translateType TShort = "Int32"
translateType TString = "String"
translateType TVoid = "()"
translateType (TPtr t) = "IORef (" ++ translateType t ++ ")"
translateType (TArray t _) = "A.IOArray Int32 " ++ translateType t

-- | Crea (A.newArray (0,size-1) initTok) asegurando que la
--   anotación de tipo no quede duplicada.
genNewArray ::
  -- | tipo base del array
  Type ->
  -- | tamaño
  Int ->
  -- | token del valor inicial (puede llevar :: …)
  String ->
  -- | indent
  Int ->
  Gen String
genNewArray t size initTok ind = do
  tmpArr <- freshTmp
  emit $
    indentStr ind
      ++ tmpArr
      ++ " <- (A.newArray (0,"
      ++ show (size - 1)
      ++ ") ("
      ++ initTok
      ++ ") :: IO (A.IOArray Int32 "
      ++ translateType t
      ++ "))"
  pure tmpArr

containsReturn :: Comm -> Bool -- ● nuevo
containsReturn (Return _) = True
containsReturn ReturnVoid = True
containsReturn (Seq c1 c2) = containsReturn c1 || containsReturn c2
containsReturn (Block b) = containsReturn b
containsReturn (Cond _ t e) = containsReturn t || containsReturn e
containsReturn _ = False

-- Valor “cero” SIN anotación de tipo

-- | Valor “cero” con anotación de tipo incluida
defaultInit :: Type -> String
defaultInit TInt = "(0 :: Int32)"
defaultInit TFloat = "(0.0 :: Float)"
defaultInit TDouble = "(0.0 :: Double)"
defaultInit TChar = "('\\0' :: Char)"
defaultInit TShort = "(0 :: Int32)"
defaultInit _ = "undefined"

-- Literal de lista, para rellenar inicializaciones parciales
defaultExp :: Type -> Exp
defaultExp TInt = IntExp 0
defaultExp TFloat = FloatExp 0.0
defaultExp TDouble = FloatExp 0.0
defaultExp TChar = CharExp '\0'
defaultExp _ = IntExp 0
