module TypeChecker (typeOf, checkType) where

import AST

-- Función para obtener el tipo de una expresión
typeOf :: Exp -> Type
typeOf (IntExp _)    = TInt
typeOf (FloatExp _)  = TFloat
typeOf (StringExp _) = TString
typeOf (CharExp _)   = TChar
typeOf (VarExp _)    = error "Cannot determine type of variable at this stage"
typeOf (AddExp e1 e2) = checkType e1 e2
typeOf (SubExp e1 e2) = checkType e1 e2
typeOf (MulExp e1 e2) = checkType e1 e2
typeOf (DivExp e1 e2) = checkType e1 e2
typeOf (UMinus e)     = typeOf e

-- Chequeo de tipos para operaciones binarias
checkType :: Exp -> Exp -> Type
checkType e1 e2
  | t1 == TInt    && t2 == TInt    = TInt
  | t1 == TFloat  && t2 == TFloat  = TFloat
  | t1 == TDouble && t2 == TDouble = TDouble
  | t1 == TInt    && t2 == TFloat  = TFloat
  | t1 == TFloat  && t2 == TInt    = TFloat
  | t1 == TInt    && t2 == TDouble = TDouble
  | t1 == TDouble && t2 == TInt    = TDouble
  -- etc.
  | otherwise = error "Type mismatch in binary operation"
  where
    t1 = typeOf e1
    t2 = typeOf e2
