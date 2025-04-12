module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
    whiteSpace lis
    t <- p
    eof
    return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , reservedNames   = ["true","false","skip","if","then","else","end",
                         "while","do","return","printf"]
    , reservedOpNames = [ "+", "-", "*", "/", "==", "!=", "<=", ">=", "<", ">", "&&", "||", "!", "=", ";", "{", "}", ",", "(", ")" ]
    }
  )


program :: Parser Comm
program = do
  cs <- many comm   -- many => parsea repetidas veces "comm" 
  return (foldr Seq Skip cs)

-- totparser comm estaba antes. program es un paso antes de comm, lo unico q hace es llamar varias veces a comm
-- esto sirve para que detecte más de una función

-----------------------------------
--- Parser de Exp (sólo aritméticas)
-----------------------------------
intexp :: Parser Exp
intexp = parseAddSub

parseAddSub :: Parser Exp
parseAddSub = chainl1 parseMulDiv (addOp <|> subOp)
  where
    addOp = do { reservedOp lis "+"; return AddExp }
    subOp = do { reservedOp lis "-"; return SubExp }

parseMulDiv :: Parser Exp
parseMulDiv = chainl1 parseFactor (mulOp <|> divOp)
  where
    mulOp = do { reservedOp lis "*"; return MulExp }
    divOp = do { reservedOp lis "/"; return DivExp }

parseFactor :: Parser Exp
parseFactor =
      try parseFuncCallFactor
  <|> try parseNeg
  <|> try (parens lis intexp)
  <|> try parseFloat
  <|> try parseInt
  <|> try parseChar
  <|> try parseString
  <|> (VarExp <$> identifier lis)

parseNeg :: Parser Exp
parseNeg = do
    reservedOp lis "-"
    f <- parseFactor
    return (UMinus f)

parseFuncCallFactor :: Parser Exp
parseFuncCallFactor = do
    funName <- identifier lis
    args    <- parens lis (commaSep lis intexp)
    return (CallExp funName args)

parseFloat :: Parser Exp
parseFloat = do
    f <- float lis
    return (FloatExp f)

parseInt :: Parser Exp
parseInt = do
    i <- integer lis
    return (IntExp i)

parseChar :: Parser Exp
parseChar = do
    c <- charLiteral lis
    return (CharExp c)

parseString :: Parser Exp
parseString = do
    s <- stringLiteral lis
    return (StringExp s)



-----------------------------------
--- Parser de BoolExp
-----------------------------------
boolexp :: Parser BoolExp
boolexp = parseOr

parseOr :: Parser BoolExp
parseOr = chainl1 parseAnd (do { reservedOp lis "||"; return Or })

parseAnd :: Parser BoolExp
parseAnd = chainl1 parseNot (do { reservedOp lis "&&"; return And })

parseNot :: Parser BoolExp
parseNot =
      try (do { reservedOp lis "!"; b <- parseNot; return (Not b) })
  <|> parseRelParen

parseRelParen :: Parser BoolExp
parseRelParen =
      try (parens lis boolexp)
  <|> try (reserved lis "true"  >> return BTrue)
  <|> try (reserved lis "false" >> return BFalse)
  <|> parseComparison

parseComparison :: Parser BoolExp
parseComparison = do
    e1 <- intexp
    op <- (   try (reservedOp lis "==" >> return Eq)
          <|> try (reservedOp lis "!=" >> return Neq)
          <|> try (reservedOp lis "<=" >> return Le)
          <|> try (reservedOp lis ">=" >> return Ge)
          <|> try (reservedOp lis "<"  >> return Lt)
          <|> try (reservedOp lis ">"  >> return Gt)
          )
    e2 <- intexp
    return (op e1 e2)



-----------------------------------
--- Parser de comandos (Comm)
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp lis ";"
                              return Seq))

comm2 :: Parser Comm
comm2 =
      try (do reserved lis "skip"
              return Skip)
  <|> try parseIf
  <|> try parseRepeat
  <|> try parseWhile  
  <|> try parseVarDecl        -- int x = 5; float y = 3.2; etc.
  <|> try parseFuncDef        -- int suma(int a, int b) { ... }
  <|> try parseReturn
  <|> try parseAssign
  <|> try parsePrintf         -- <--- nuevo

-- While
parseWhile :: Parser Comm
parseWhile = do
    reserved lis "while"
    cond <- parens lis boolexp
    body <- braces lis comm
    return (While cond body)

commBlock :: Parser Comm
commBlock = braces lis (comm <|> return Skip)


-- If
parseIf :: Parser Comm
parseIf = do
    reserved lis "if"
    cond <- parens lis boolexp
    thenBlock <- commBlock
    reserved lis "else"
    elseBlock <- commBlock
    return (Cond cond thenBlock elseBlock)
  where
    commBlock :: Parser Comm
    commBlock = braces lis comm <|> comm



-- Repeat
parseRepeat :: Parser Comm
parseRepeat = do
    reserved lis "repeat"
    c <- comm
    reserved lis "until"
    cond <- boolexp
    reserved lis "end"
    return (Repeat c cond)

-- Return
parseReturn :: Parser Comm
parseReturn = do
    reserved lis "return"
    e <- intexp
    return (Return e)

-- Printf
parsePrintf :: Parser Comm
parsePrintf = do
    reserved lis "printf"
    args <- parens lis parsePrintfArgs
    return (Printf (fst args) (snd args))
  where
    parsePrintfArgs :: Parser (String, [Exp])
    parsePrintfArgs = do
        s <- stringLiteral lis
        exps <- many (do
            reservedOp lis ","
            try (BoolAsIntExp <$> boolexp) <|> intexp
           )
        return (s, exps)

parseAssign :: Parser Comm
parseAssign = do
    var <- identifier lis
    reservedOp lis "="
    -- Primero intenta boolexp, si falla, usa intexp
    e <- try (BoolAsIntExp <$> boolexp) <|> intexp
    return (Assign var e)


parseFuncCall :: Parser Exp
parseFuncCall = do
    funName <- identifier lis
    args    <- parens lis (commaSep lis intexp) 
    -- Importante: si tus expresiones son más complejas que "intexp", 
    -- puedes usar el parser general de expresiones, pero por simplicidad: 
    return (CallExp funName args)


-- Parser de declaración de variables 
--   Ejemplo: int x = 10, y = 20, z = 30;
parseVarDecl :: Parser Comm
parseVarDecl = do
    t <- typeParser
    -- NOTA: le pasamos 'lis' como primer argumento a 'commaSep1'.
    decls <- commaSep1 lis (do
                var <- identifier lis
                reservedOp lis "="
                ex  <- intexp
                return (var, ex))
    let toLet (v, e) = LetType t v e
    return (foldr1 Seq (map toLet decls))

-------------------------------------------------

parseFuncDef :: Parser Comm
parseFuncDef = do
    retType  <- typeParser
    funcName <- identifier lis
    -- NOTA: Al usar 'parens lis', adentro también le pasamos 'lis' a commaSep.
    params   <- parens lis (commaSep lis parseParam)
    reservedOp lis "{"
    body     <- comm
    reservedOp lis "}"
    return (FuncDef retType funcName params body)


parseParam :: Parser (Type, Variable)
parseParam = do
    t  <- typeParser
    nm <- identifier lis
    return (t, nm)

-- Parser para los tipos soportados
typeParser :: Parser Type
typeParser =   try (reserved lis "int"    >> return TInt)
           <|> try (reserved lis "float"  >> return TFloat)
           <|> try (reserved lis "double" >> return TDouble)
           <|> try (reserved lis "char"   >> return TChar)
           <|> try (reserved lis "long"   >> return TLong)
           <|> try (reserved lis "short"  >> return TShort)
           <|> try (reserved lis "string" >> return TString)

------------------------------------
-- Funcion de parseo principal
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser program)
