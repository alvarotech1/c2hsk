module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

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
                         "while","do","return","printf", "void","for", "const","break","scanf"]
    , reservedOpNames = [ "+", "-", "*", "/","%", "==", "!=", "<=", ">=", "<", ">", "&&", "||", "!", "=", ";", "{", "}", ",", "(", ")" ]
    }
  )


program :: Parser Comm
program = do
  cs <- many comm 
  return (foldr Seq Skip cs)

intexp :: Parser Exp
intexp = parseAddSub

-- aplica chainl1 a los operadores de izq a der
parseAddSub :: Parser Exp
parseAddSub = chainl1 parseMulDiv (addOp <|> subOp <|> modOp)
  where
    addOp = do { reservedOp lis "+"; return AddExp }
    subOp = do { reservedOp lis "-"; return SubExp }
    modOp = do { reservedOp lis "%"; return ModExp }

parseMulDiv :: Parser Exp
parseMulDiv = chainl1 parseFactor (mulOp <|> divOp)
  where
    mulOp = do { reservedOp lis "*"; return MulExp }
    divOp = do { reservedOp lis "/"; return DivExp }

parseFactor :: Parser Exp
parseFactor =
      try parseArrayAccess    
  <|> try parsePreIncr
  <|> try parsePreDecr
  <|> try parsePostIncr
  <|> try parsePostDecr
  <|> try parseFuncCallFactor
  <|> try parseNeg
  <|> try (parens lis intexp)
  <|> try parseFloat
  <|> try parseInt
  <|> try parseChar
  <|> try parseString
  <|> (VarExp <$> identifier lis)

-- pre-incremento (++x)
parsePreIncr :: Parser Exp
parsePreIncr = do
    reservedOp lis "++"
    var <- identifier lis
    return (PreIncr var)

-- pre-decremento (--x)
parsePreDecr :: Parser Exp
parsePreDecr = do
    reservedOp lis "--"
    var <- identifier lis
    return (PreDecr var)

-- post-incremento (x++)
parsePostIncr :: Parser Exp
parsePostIncr = do
    var <- identifier lis
    reservedOp lis "++"
    return (PostIncr var)

-- post-decremento (x--)
parsePostDecr :: Parser Exp
parsePostDecr = do
    var <- identifier lis
    reservedOp lis "--"
    return (PostDecr var)

parseNeg :: Parser Exp
parseNeg = do
    reservedOp lis "-"
    f <- parseFactor
    return (UMinus f)

-- parser para llamada a funciones
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




{-
--COM QUE IGNORA PUNTO Y COMA
--Secuencia de comandos:  c1; c2; c3 …  (el ‘;’ final de cada cᵢ es opcional)
comm :: Parser Comm
comm = do
    cs <- many1 $ do
        c <- comm2                 -- una sentencia
        optional (reservedOp lis ";")  -- come ‘;’ si está
        return c
    return (foldr1 Seq cs)         -- enlaza c₁ ▷ c₂ ▷ …
-}

{-

COM VIEJO:

comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp lis ";"
                              return Seq))
-}

{-
COMM2 Viejo
comm2 :: Parser Comm
comm2 =
      try (do reserved lis "skip"
              return Skip)
  <|> try parseFuncDef
  <|> try parseVarDecl        
  <|> try parseIfElse
  <|> try parseIfOnly
  <|> try parseFor
  <|> try parseBreak 
  <|> try parseWhile
  <|> try parseDoWhile  
  <|> try parseReturn
  <|> try parseArrayAssign
  <|> try parseAssign
  <|> try parsePrintf
  <|> try parseScanf
  <|> try commBlock
  <|> try parseExprStmt  -- OJO, debería estar aca? averiguar       
-}

comm :: Parser Comm
comm = do
  cs <- many1 (try commSemicolon <|> commNoSemicolon)
  return (foldr Seq Skip cs)

-- Comandos que deben terminar con ;
commSemicolon :: Parser Comm
commSemicolon = do
  c <- commNeedsSemicolon
  reservedOp lis ";"
  return c

-- Lista de comandos que requieren ;
commNeedsSemicolon :: Parser Comm
commNeedsSemicolon =
      try parseAssign
  <|> try parseExprStmt
  <|> try parsePrintf
  <|> try parseScanf
  <|> try parseReturn
  <|> try parseVarDecl
  <|> try parseBreak
  <|> (reserved lis "skip" >> return Skip)
  <|> try parseArrayAssign

-- Comandos que no requieren ;
commNoSemicolon :: Parser Comm
commNoSemicolon =
      try parseFuncDef
  <|> try parseIfElse
  <|> try parseIfOnly
  <|> try parseWhile
  <|> try parseDoWhile
  <|> try parseFor
  <|> try commBlock


parseExprStmt :: Parser Comm
parseExprStmt = do
    e <- try parsePreIncr
     <|> try parsePreDecr
     <|> try parsePostIncr
     <|> try parsePostDecr
     <|> try parseFuncCall 
    return (ExprStmt e)


parseBreak :: Parser Comm
parseBreak = reserved lis "break" >> return Break


-- do { … } while ( … );
parseDoWhile :: Parser Comm
parseDoWhile = do
    reserved lis "do"
    body <- commBlock
    reserved lis "while"
    cond <- parens lis boolexp
    reservedOp lis ";"          -- ← punto y coma OBLIGATORIO
    return (DoWhile body cond)



-- While
parseWhile :: Parser Comm
parseWhile = do
    reserved lis "while"
    cond <- parens lis boolexp
    body <- commBlock              --  ← antes usaba braces lis …
    return (While cond body)

-- for(init; cond; step) { body }
parseFor :: Parser Comm
parseFor = do
    reserved lis "for"
    reservedOp lis "("
    mInit <- optionMaybe (try commNeedsSemicolon)   -- init     (puede faltar)
    reservedOp lis ";"
    mCond <- optionMaybe boolexp       -- cond     (puede faltar → True)
    reservedOp lis ";"
    mStep <- optionMaybe (try commNeedsSemicolon)   -- step     (puede faltar)
    reservedOp lis ")"
    body  <- commBlock
    let cond = maybe BTrue id mCond
    return (For mInit cond mStep body)


-- Un bloque "{ … }" se traduce a (Block cuerpo) para abrir un nuevo scope
commBlock :: Parser Comm
commBlock = do
    body <- braces lis (comm <|> return Skip)
    return (Block body)

-- Parser para if con else (C style)
parseIfElse :: Parser Comm
parseIfElse = do
    reserved lis "if"
    cond <- parens lis boolexp
    ifBlock <- commBlock
    reserved lis "else"
    elseBlock <- commBlock
    return (Cond cond ifBlock elseBlock)

-- Parser para if sin else (C style)
parseIfOnly :: Parser Comm
parseIfOnly = do
    reserved lis "if"
    cond <- parens lis boolexp
    ifBlock <- commBlock
    return (Cond cond ifBlock Skip)


parseReturn :: Parser Comm
parseReturn = do
    reserved lis "return"
    e <- intexp
    return (Return e)

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

parseScanf :: Parser Comm
parseScanf = do
    reserved lis "scanf"
    args <- parens lis parseScanfArgs
    return (Scanf (fst args) (snd args))
  where
    parseScanfArgs :: Parser (String, [Variable])
    parseScanfArgs = do
        s <- stringLiteral lis
        vars <- many (do
            reservedOp lis ","
            identifier lis)
        return (s, vars)

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
    return (CallExp funName args)

{-
VARDECL FRANCO SACADO EL 24-05
parseVarDecl :: Parser Comm
parseVarDecl = try $ do
     isConst <- optionMaybe (reserved lis "const")
     t       <- typeParser
     decls <- commaSep1 lis $ do
        name  <- identifier lis
        mSize <- optionMaybe (brackets lis (fromInteger <$> integer lis))
        notFollowedBy (parens lis (commaSep lis parseParam))
        mInit <- optionMaybe (reservedOp lis "=" >> intexp)
        let realType = maybe t (\n -> TArray t (fromInteger n)) mSize
        return (realType, name, mInit)
     let
      toLet (ty,v,me) = case isConst of
          Just _  -> LetConst ty v (maybe (defaultInit ty) id me)
          Nothing -> LetType  ty v (maybe (defaultInit ty) id me)
     return (foldr1 Seq (map toLet decls))
-}

--VARDECL SIGMA CON IMPLEMENTACION DE CONST
parseVarDecl :: Parser Comm
parseVarDecl = try $ do
    isConst <- optionMaybe (reserved lis "const")  -- detecta si viene "const"
    t     <- typeParser
    decls <- commaSep1 lis $ do
        name <- identifier lis
        notFollowedBy (parens lis (commaSep lis parseParam))
        mInit <- optionMaybe (reservedOp lis "=" >> intexp)
        return (name, mInit)
    let
      toLet (v, me) =
        case isConst of
          Just _  -> LetConst t v (maybe (defaultInit t) id me)  -- si venía const
          Nothing -> LetType  t v (maybe (defaultInit t) id me)  -- si no venía const
    return (foldr1 Seq (map toLet decls))


-- identificador '[' exp ']'
parseArrayAccess :: Parser Exp
parseArrayAccess = do
    arr <- identifier lis
    idx <- brackets lis intexp
    return (ArrayAccess arr idx)

-- asignación a elemento: arr[exp] = exp
parseArrayAssign :: Parser Comm
parseArrayAssign = do
    arr <- identifier lis
    idx <- brackets lis intexp
    reservedOp lis "="
    -- si quisieras soportar boolexp aquí, podrías usar:
    -- rhs <- try (BoolAsIntExp <$> boolexp) <|> intexp
    rhs <- intexp
    return (AssignArr arr idx rhs)


-- Valores AST por defecto para declaraciones sin inicializador
defaultInit :: Type -> Exp
defaultInit TInt    = IntExp    0
defaultInit TFloat  = FloatExp  0.0
defaultInit TDouble = FloatExp  0.0
defaultInit TChar   = CharExp   '\0'
defaultInit TString = StringExp ""
defaultInit TLong   = IntExp    0     -- no hay LongExp, usamos Int
defaultInit TShort  = IntExp    0     -- idem Short → Int
defaultInit (TArray t n) = CallExp "replicate"
    [ IntExp (fromIntegral n)
    , defaultInit t
    ]



parseFuncDef :: Parser Comm
parseFuncDef = do
    retType  <- typeParser
    funcName <- identifier lis
    -- NOTA: Al usar 'parens lis', adentro también le pasamos 'lis' a commaSep.
    params <- parens lis (
            (reserved lis "void" >> return [])            -- f(void)  → lista vacía
            <|> commaSep lis parseParam                   -- resto de casos
          )
    reservedOp lis "{"
    body     <- comm
    reservedOp lis "}"
    return (FuncDef retType funcName params body)


parseParam :: Parser (Type, Variable)
parseParam = do
    t  <- typeParser
    nm <- identifier lis
    return (t, nm)


typeParser :: Parser Type
typeParser =   try (reserved lis "int"    >> return TInt)
           <|> try (reserved lis "float"  >> return TFloat)
           <|> try (reserved lis "double" >> return TDouble)
           <|> try (reserved lis "char"   >> return TChar)
           <|> try (reserved lis "long"   >> return TLong)
           <|> try (reserved lis "short"  >> return TShort)
           <|> try (reserved lis "string" >> return TString)
           <|> try (reserved lis "void"   >> return TVoid)

------------------------------------
-- Funcion de parseo principal
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser program)
