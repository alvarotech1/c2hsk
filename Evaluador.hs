module Evaluador where

import AST
import Data.List (isPrefixOf)

-- Función principal para traducir comandos
eval :: Comm -> IO ()
eval comando = do
    evalComm comando 4  -- Indentamos en 4 espacios para simular "do"

--------------------------------------------
-- Traducción de comandos a código Haskell --
--------------------------------------------

evalComm :: Comm -> Int -> IO ()
evalComm Skip _ = return ()

-- let <var> = <exp> :: <tipo>
evalComm (LetType t var exp) indent = do
    putStrLn (indentStr indent 
        ++ "let " ++ var 
        ++ " = " ++ evalExp exp 
        ++ " :: " ++ translateType t)

-- Secuencia de comandos
evalComm (Seq c1 c2) indent = do
    evalComm c1 indent
    evalComm c2 indent

-- If-then-else
evalComm (Cond cond c1 c2) indent = do
    putStrLn (indentStr indent ++ "if (" ++ evalBoolExp cond ++ ") then")
    evalComm c1 (indent + 2)
    putStrLn (indentStr indent ++ "else")
    evalComm c2 (indent + 2)

-- repeat ... until
evalComm (Repeat c cond) indent = do
    putStrLn (indentStr indent ++ "-- repeat")
    evalComm c (indent + 2)
    putStrLn (indentStr indent ++ "-- until " ++ evalBoolExp cond)

evalComm (FuncDef retType "main" params body) indent = do
    -- Caso especial: la función se llama main
    putStrLn "main :: IO ()"
    putStrLn "main = do"
    evalComm body (indent + 2)

evalComm (FuncDef retType name params body) indent = do
    -- Caso general: funciones que no sean main
    let typeSig = makeTypeSignature name params retType
    putStrLn typeSig
    let paramNames = unwords (map snd params)
    -- Para funciones puras, no uses `do` si no manejan IO
    putStrLn (name ++ " " ++ paramNames ++ " = " ++ processBody body)


-- Return
evalComm (Return exp) indent = do
    putStrLn (indentStr indent ++ "return " ++ evalExp exp)

-- Asignación
evalComm (Assign var expr) indent = do
    putStrLn (indentStr indent 
       ++ "let " ++ var 
       ++ " = " ++ evalExp expr)

-- Printf
evalComm (Printf s exps) indent = do
    let hsPrintf = translatePrintf s exps
    putStrLn (indentStr indent ++ "putStrLn (" ++ hsPrintf ++ ")")


evalComm (While cond cuerpo) indent = do
    -- Primero, chequeamos que la condición sea booleana.
    if not (ensureBool cond)
       then error "La condición del while debe ser booleana."
       else return ()
    -- Extraemos la variable de control de la condición (se asume que la variable está en la parte izquierda)
    let controlVar = extractControlVar cond
    -- Generamos un nombre de función (único según la variable y la indentación, de manera provisional)
    let loopName = "whileLoop_" ++ controlVar ++ "_" ++ show indent
    -- Definimos la función recursiva
    putStrLn (indentStr indent ++ "let " ++ loopName ++ " " ++ controlVar ++ " =")
    putStrLn (indentStr (indent+2) ++ "if " ++ translateLoopCond cond ++ " then do")
    -- Traducimos el cuerpo del while (se espera que en el cuerpo se haga la actualización sobre controlVar)
    evalComm (substituteVar controlVar cuerpo) (indent+4)
    -- Se asume que hay una asignación que actualiza la variable de control;
    -- la buscamos y, de no hallarla, se conserva el mismo valor.
    putStrLn (indentStr (indent+4) ++ loopName ++ " " ++ updateExpr controlVar cuerpo)
    putStrLn (indentStr (indent+2) ++ "else return ()")
    putStrLn (indentStr indent ++ loopName)
    -- Llamada inicial al loop (se pasa el valor original, en el código generado se habrá declarado la variable)
    putStrLn (indentStr indent ++ loopName ++ " " ++ controlVar)



------------------------------------------------------------
-- Funciones auxiliares para la traducción a código Haskell --
------------------------------------------------------------
-- Chequea recursivamente que la expresión booleana tenga sentido.
-- Aquí se asume que los operadores básicos (Eq, Le, Lt, Ge, Gt, Neq) producen valores booleanos.
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

-- Intenta extraer la variable de control, es decir, el operando izquierdo de la comparación
extractControlVar :: BoolExp -> String
extractControlVar (Eq (VarExp v) _)  = v
extractControlVar (Neq (VarExp v) _) = v
extractControlVar (Le (VarExp v) _)  = v
extractControlVar (Lt (VarExp v) _)  = v
extractControlVar (Ge (VarExp v) _)  = v
extractControlVar (Gt (VarExp v) _)  = v
-- Para operadores compuestos, se recorre recursivamente (por ejemplo, en Not, And, Or)
extractControlVar (Not b)          = extractControlVar b
extractControlVar (And b1 _)       = extractControlVar b1
extractControlVar (Or b1 _)        = extractControlVar b1
extractControlVar _ = error "No se pudo extraer la variable de control de la condición."

-- Transforma la condición del while a código Haskell (usa evalBoolExp para formar la condición)
translateLoopCond :: BoolExp -> String
translateLoopCond cond = evalBoolExp cond

-- Sustituye (si es necesario) ocurrencias de la variable de control en el cuerpo.
-- En esta versión simple asumimos que ya se usa de forma directa, así que no hacemos sustitución.
substituteVar :: String -> Comm -> Comm
substituteVar _ cuerpo = cuerpo

-- Busca (recursivamente) en el cuerpo alguna asignación que actualice la variable de control.
-- Se asume que las actualizaciones vienen en la forma: Assign controlVar (AddExp (VarExp controlVar) (IntExp n))
findUpdate :: String -> Comm -> Maybe String
findUpdate v (Assign v' expr) | v == v' =
    case expr of
      AddExp (VarExp v'') (IntExp n) | v == v'' -> Just ("(" ++ v ++ " + " ++ show n ++ ")")
      SubExp (VarExp v'') (IntExp n) | v == v'' -> Just ("(" ++ v ++ " - " ++ show n ++ ")")
      _ -> Nothing
findUpdate v (Seq c1 c2) =
    case findUpdate v c2 of
      Just upd -> Just upd
      Nothing  -> findUpdate v c1
findUpdate _ _ = Nothing

-- Devuelve la expresión de actualización o, si no se encuentra, la misma variable (sin cambio)
updateExpr :: String -> Comm -> String
updateExpr v cuerpo =
    case findUpdate v cuerpo of
      Just upd -> upd
      Nothing  -> v

-- Asumiendo que tu parser te garantiza "FuncDef" termina con Return e
processBody :: Comm -> String
processBody (Return e) = evalExp e
processBody (Seq c1 c2) =
    case c2 of
      Return e -> -- c1 se ignora o algo básico
                  -- si c1 no es nada, devuelves evalExp e
                  evalExp e
      _ -> error "FuncDef must end with return"
processBody _ = error "Invalid function body"


-- Construye la firma de tipo en Haskell, ejemplo:
--  suma :: Int -> Int -> Int
makeTypeSignature :: Variable -> [(Type, Variable)] -> Type -> String
makeTypeSignature funcName params retType =
    let paramTypes = map (translateType . fst) params
        paramSig   = unwords $ map (++ " ->") paramTypes
        retSig     = translateType retType
    in funcName ++ " :: " ++ paramSig ++ " " ++ retSig

-- Traducción de expresiones aritméticas a código Haskell
evalExp :: Exp -> String
evalExp (BoolAsIntExp b) = 
    "(if " ++ evalBoolExp b ++ " then 1 else 0)"  -- usa evalBoolExp para b
evalExp (IntExp n)      = show n
evalExp (IntExp n)      = show n
evalExp (FloatExp n)    = show n
evalExp (StringExp s)   = show s
evalExp (CharExp c)     = show c
evalExp (VarExp var)    = var
evalExp (UMinus e)      = "(-" ++ evalExp e ++ ")"
evalExp (AddExp e1 e2)  = "(" ++ evalExp e1 ++ " + " ++ evalExp e2 ++ ")"
evalExp (SubExp e1 e2)  = "(" ++ evalExp e1 ++ " - " ++ evalExp e2 ++ ")"
evalExp (MulExp e1 e2)  = "(" ++ evalExp e1 ++ " * " ++ evalExp e2 ++ ")"
evalExp (DivExp e1 e2)  = "(" ++ evalExp e1 ++ " `div` " ++ evalExp e2 ++ ")"
evalExp (CallExp fn args) = fn ++ " " ++ unwords (map evalExp args)

-- Traducción de expresiones booleanas a código Haskell
evalBoolExp :: BoolExp -> String
evalBoolExp BTrue         = "True"
evalBoolExp BFalse        = "False"
evalBoolExp (Not b)       = "not (" ++ evalBoolExp b ++ ")"
evalBoolExp (And b1 b2)   = "(" ++ evalBoolExp b1 ++ " && " ++ evalBoolExp b2 ++ ")"
evalBoolExp (Or b1 b2)    = "(" ++ evalBoolExp b1 ++ " || " ++ evalBoolExp b2 ++ ")"
evalBoolExp (Eq e1 e2)    = "(" ++ evalExp e1 ++ " == " ++ evalExp e2 ++ ")"
evalBoolExp (Neq e1 e2)   = "(" ++ evalExp e1 ++ " /= " ++ evalExp e2 ++ ")"
evalBoolExp (Lt e1 e2)    = "(" ++ evalExp e1 ++ " < " ++ evalExp e2 ++ ")"
evalBoolExp (Le e1 e2)    = "(" ++ evalExp e1 ++ " <= " ++ evalExp e2 ++ ")"
evalBoolExp (Gt e1 e2)    = "(" ++ evalExp e1 ++ " > " ++ evalExp e2 ++ ")"
evalBoolExp (Ge e1 e2)    = "(" ++ evalExp e1 ++ " >= " ++ evalExp e2 ++ ")"

-- Traducción de printf a putStrLn
translatePrintf :: String -> [Exp] -> String
translatePrintf s [] = show s  -- Si no hay expresiones, solo mostrar el string
translatePrintf s exps = buildPrintfString s exps



-- Construye una cadena con concatenaciones y show de expresiones
-- Asume que los placeholders son "%d" y que están en orden.
buildPrintfString :: String -> [Exp] -> String
buildPrintfString s exps =
    let parts = splitOn "%d" s
    in "\"" ++ concat (zipWith (\p e -> p ++ "\" ++ show (" ++ evalExp e ++ ") ++ \"") parts exps) ++ 
                   (if length parts > length exps 
                       then last parts 
                       else "") ++ "\""

-- Función simple para dividir el string en base al patrón "%d"
splitOn :: String -> String -> [String]
splitOn _ "" = [""]
splitOn delim str =
    if delim `isPrefixOf` str
        then "" : splitOn delim (drop (length delim) str)
        else 
            case str of
                [] -> []
                (c:cs) -> let (before, after) = breakOn delim cs
                          in (c : before) : splitOn delim after

-- Helper para romper el string en base al delimitador
breakOn :: String -> String -> (String, String)
breakOn _ "" = ("", "")
breakOn delim str
    | delim `isPrefixOf` str = ("", str)
    | otherwise =
        case str of
            [] -> ("", "")
            (c:cs) ->
                let (before, after) = breakOn delim cs
                in (c : before, after)


-- Traducción de tipos de C a Haskell
translateType :: Type -> String
translateType TInt    = "Int"
translateType TFloat  = "Float"
translateType TDouble = "Double"
translateType TChar   = "Char"
translateType TLong   = "Integer"
translateType TShort  = "Int"
translateType TString = "String"

-- Helper para la indentación
indentStr :: Int -> String
indentStr n = replicate n ' '
