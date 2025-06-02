module AST where

import Control.Exception (Exception)

type Variable = String

-- Tipos de datos primitivos
data Type 
    = TInt 
    | TFloat 
    | TDouble
    | TChar
    | TLong
    | TShort
    | TString
    | TVoid
    | TArray Type Int
    deriving (Show, Eq)


-- Expresiones (sólo aritméticas)
data Exp 
    = IntExp Integer
    | FloatExp Double
    | StringExp String
    | CharExp Char
    | UMinus Exp
    | VarExp Variable
    | AddExp Exp Exp
    | SubExp Exp Exp
    | MulExp Exp Exp
    | DivExp Exp Exp
    | ModExp Exp Exp  
    | CallExp Variable [Exp]  -- llamada a función con argumentos aritméticos
    | BoolAsIntExp BoolExp 
    | PostIncr Variable     --  x++
    | PostDecr Variable     --  x++
    | PreIncr Variable      -- ++x
    | PreDecr Variable      -- --x
    | ArrayAccess Variable Exp -- arr[i]
    deriving (Show, Eq)

-- Expresiones Booleanas, separadas
data BoolExp
    = BTrue
    | BFalse
    | Not BoolExp
    | And BoolExp BoolExp
    | Or  BoolExp BoolExp
    | Eq  Exp Exp     -- eq e1 e2 (e1, e2 son Exp aritméticas)
    | Neq Exp Exp
    | Lt  Exp Exp
    | Le  Exp Exp
    | Gt  Exp Exp
    | Ge  Exp Exp
    deriving (Show, Eq)

-- Comandos (sentencias)
data Comm
    = Skip
    | LetType Type Variable Exp
    | LetConst Type Variable Exp
    | LetUninit Type Variable
    | Assign  Variable Exp
    | Seq Comm Comm
    | Cond BoolExp Comm Comm   -- if cond then c1 else c2
    | CondNoElse BoolExp Comm
    | Repeat Comm BoolExp      -- repeat c until cond
    | Break  
    | FuncDef Type Variable [(Type, Variable)] Comm
    | Return Exp
    | Printf String [Exp]
    | Scanf String [Variable]
    | While BoolExp Comm
    | DoWhile Comm BoolExp --  do { body } while (cond);
    | ExprStmt Exp   -- permite ejecutar expresiones como comandos, ej. var++
    | Block Comm
    | For (Maybe Comm) BoolExp (Maybe Comm) Comm   -- init; cond; step { body }
    | AssignArr Variable Exp Exp  -- arr[i] = e;
    | Switch Exp [Case]         -- switch (e) { … }
    deriving (Show, Eq)

data Case
    = Case      Exp   Comm      -- case v:  …
    | DefaultCase Comm          -- default: …
    deriving (Show, Eq)