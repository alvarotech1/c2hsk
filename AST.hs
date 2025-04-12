module AST where

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
    | CallExp Variable [Exp]  -- llamada a función con argumentos aritméticos
    | BoolAsIntExp BoolExp 
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
    | Assign  Variable Exp
    | Seq Comm Comm
    | Cond BoolExp Comm Comm   -- if cond then c1 else c2
    | Repeat Comm BoolExp      -- repeat c until cond
    | FuncDef Type Variable [(Type, Variable)] Comm
    | Return Exp
    | Printf String [Exp]
    | While BoolExp Comm
    deriving (Show, Eq)
