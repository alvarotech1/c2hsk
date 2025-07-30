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
    | TPtr Type       -- puntero a un tipo dado
    | TStruct String 
    deriving (Show, Eq)

-- Expresiones (sólo aritméticas)
data Exp 
    = IntExp Integer
    | FloatExp Double
    | StringExp String
    | CharExp Char
    | InitList [Exp]     
    | UMinus Exp
    | VarExp Variable
    | AddExp Exp Exp
    | SubExp Exp Exp
    | MulExp Exp Exp
    | DivExp Exp Exp
    | ModExp Exp Exp
    | Sqrt Exp             
    | Pow Exp Exp              
    | Log2 Exp
    | CallExp Variable [Exp]  -- llamada a función con argumentos aritméticos
    | BoolAsIntExp BoolExp 
    | PostIncr Variable     --  x++
    | PostDecr Variable     --  x++
    | PreIncr Variable      -- ++x
    | PreDecr Variable      -- --x
    | AddrOf Exp        -- &e    (tomar dirección)
    | Deref  Exp        -- *e    (desreferenciar)
    | ArrayAccess Variable Exp -- arr[i]
    | FieldAccess Exp Variable
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
    | Repeat Comm BoolExp      -- repeat c until cond
    | Break  
    | FuncDef Type Variable [(Type, Variable)] Comm
    | Return Exp  -- return e;
    | ReturnVoid  -- return; (solo en funciones void)
    | Printf String [Exp]
    | Scanf String [Exp]
    | While BoolExp Comm
    | DoWhile Comm BoolExp --  do { body } while (cond);
    | ExprStmt Exp   -- permite ejecutar expresiones como comandos, ej. var++
    | Block Comm
    | For (Maybe Comm) BoolExp (Maybe Comm) Comm   -- init; cond; step { body }
    | AssignArr Variable Exp Exp  -- arr[i] = e;
    | Switch Exp [Case]         -- switch (e) { … }
    | AssignDeref Exp Exp   -- *p = e;
    | StructDef String [(Type, Variable)]
    | AssignField Variable Variable Exp
    deriving (Show, Eq)

data Case
    = Case      Exp   Comm      -- case v:  …
    | DefaultCase Comm          -- default: …
    deriving (Show, Eq)
