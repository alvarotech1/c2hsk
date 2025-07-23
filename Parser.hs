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
                         "while","do","return","printf", "void","for", "const","break","scanf","switch","case","default", "sqrt", "pow", "log2"]
    , reservedOpNames = ["+=", "-=", "*=", "+", "-", "*", "/","%", "==", "!=", "<=", ">=", "<", ">", "&&", "||", "&" ,"!", "=", ";", "{", "}", ",", "(", ")",":" ]
    }
  )


program :: Parser Comm
program = do
  many (try includeLine) -- ignora los #include del principio
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
  <|> try parseAddrOf
  <|> try parseDeref
  <|> try parseFuncCallFactor
  <|> try parseNeg
  <|> try (parens lis intexp)
  <|> try parseFloat
  <|> try parseInt
  <|> try parseChar
  <|> try parseString
  <|> try sqrtExp
  <|> try powExp
  <|> try log2Exp

  <|> (VarExp <$> identifier lis)

-- &e   (tomar dirección)
parseAddrOf :: Parser Exp
parseAddrOf = do
  reservedOp lis "&"
  e <- parseFactor
  return (AddrOf e)

-- *e   (desreferenciar)
parseDeref :: Parser Exp
parseDeref = do
  ptrStar
  e <- parseFactor
  return (Deref e)


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

-- raiz cuadrada
sqrtExp :: Parser Exp
sqrtExp = do 
     reserved lis "sqrt"
     exp <- intexp
     return (Sqrt exp)

-- potencia
powExp :: Parser Exp
powExp = do
    reserved lis "pow"
    parens lis $ do  -- el $ te sirve para asegurar que las instrucciones desp del 'do' solo seran ejecutadas si la entrada empieza con 'parens' de 'lis' osea parentesis
        base <- intexp
        comma lis
        exp <- intexp 
        return (Pow base exp)

-- logaritmo en base 2
log2Exp :: Parser Exp
log2Exp = do
    reserved lis "log2"
    exp <- intexp        
    return (Log2 exp)

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

-- Parser para ignorar solo líneas que comienzan con '#include'
includeLine :: Parser ()
includeLine = do
    _ <- try (char '#' >> string "include") -- reconoce #include
    _ <- many (noneOf "\n\r") -- consume todo lo que viene hasta el \n
    _ <- optional (oneOf "\n\r") -- consume salto de linea si hay
    whiteSpace lis    -- <-- consume espacios o lineas vacias
    return ()

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


comm :: Parser Comm
comm = do
  many (try includeLine)
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
  <|> try parseAssignDeref  
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
  <|> try parseSwitch  
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


------------------------------------------------------------------
-- switch (expr) { case … ; … ; default: … }
------------------------------------------------------------------

parseSwitch :: Parser Comm
parseSwitch = do
    reserved lis "switch"
    scrut <- parens lis (try (BoolAsIntExp <$> boolexp) <|> intexp)
    reservedOp lis "{"
    sections <- many1 parseSection
    reservedOp lis "}"

    -- si hay default lo enviamos al final
    let (defs , cases) = span isDefault sections
        finalCases     = cases ++ defs
    return (Switch scrut finalCases)
  where
    isDefault DefaultCase{} = True
    isDefault _             = False

    --------------------------------------------------------------
    -- Secciones -------------------------------------------------
    --------------------------------------------------------------
    parseSection :: Parser Case
    parseSection =  try parseCase
                <|> parseDefault

    -- case LABEL:
    parseCase = do
        reserved lis "case"
        lbl <- parseLabel
        reservedOp lis ":"
        body <- parseCaseBody
        return (Case lbl body)

    -- default:
    parseDefault = do
        reserved lis "default"
        reservedOp lis ":"
        DefaultCase <$> parseCaseBody

    -- LABEL  ::=  entero | char | bool
    parseLabel =
          try (BoolAsIntExp <$> boolexp)  -- true / false
      <|> try parseChar                    -- 'x'
      <|> parseInt                         -- 42

    --------------------------------------------------------------
    -- Cuerpo: lee comandos hasta el próximo case|default|}
    --------------------------------------------------------------
    parseCaseBody =
        fmap (foldr Seq Skip)
             (manyTill caseStmt
                       (lookAhead (   try (reserved lis "case")
                                  <|> try (reserved lis "default")
                                  <|> reservedOp lis "}")))

    -- Dentro de un case admitimos *cualquier* comando:
    caseStmt = try commSemicolon <|> commNoSemicolon


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

-- un “statement” es o bien uno de los que llevan ';', o uno sin ';' (incluyendo blocks, if, while…)
singleStmt :: Parser Comm
singleStmt = try commSemicolon <|> commNoSemicolon

parseIfElse :: Parser Comm
parseIfElse = do
    reserved lis "if"
    cond      <- parens lis boolexp
    ifBlock   <- commBlock
    reserved lis "else"
    elseBlock <- singleStmt     -- sólo un statement, no varios
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
    me <- optionMaybe intexp
    return $ maybe ReturnVoid Return me

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
    parseScanfArgs :: Parser (String, [Exp])
    parseScanfArgs = do
        fmt <- stringLiteral lis             -- "%d %d"
        reservedOp lis ","                   -- ← coma obligatoria
        vars <- commaSep1 lis parseLValue    -- al menos un l-value
        return (fmt, vars)

parseLValue :: Parser Exp
parseLValue = do
  _    <- optionMaybe (reservedOp lis "&")   -- &  (ignoramos el resultado)
  name <- identifier lis                     -- siempre hay un identificador
  mIdx <- optionMaybe (brackets lis intexp)  -- ¿viene  “[expr]”  ?
  return $ case mIdx of
             Just idx -> ArrayAccess name idx
             Nothing  -> VarExp name


parseAssign :: Parser Comm
parseAssign = do
    var <- identifier lis
    opFn <- choice
      [ try $ reservedOp lis "+=" >> return (\rhs -> AddExp (VarExp var) rhs)
      , try $ reservedOp lis "-=" >> return (\rhs -> SubExp (VarExp var) rhs)
      , try $ reservedOp lis "*=" >> return (\rhs -> MulExp (VarExp var) rhs)
      ,        reservedOp lis "="  >> return id              -- caso normal
      ]
    rhs <- try (BoolAsIntExp <$> boolexp) <|> intexp
    return (Assign var (opFn rhs))


parseFuncCall :: Parser Exp
parseFuncCall = do
    funName <- identifier lis
    args    <- parens lis (commaSep lis intexp) 
    return (CallExp funName args)

--VARDECL SIGMA CON IMPLEMENTACION DE CONST
parseVarDecl :: Parser Comm
parseVarDecl = try $ do
    isConst <- optionMaybe (reserved lis "const")    -- ¿lleva const?
    t0      <- typeParser
    ptrs <- many ptrStar              -- lee cero o más “*”
    let t = foldl (\acc _ -> TPtr acc) t0 ptrs       -- construye TPtr anidado

    decls   <- commaSep1 lis $ do
        name  <- identifier lis
        -- []  acepta “char x[]”  (devuelve 0)
        -- [N] acepta “char x[10]”
        mSize <- optionMaybe $ brackets lis $
                    option 0 (fromInteger <$> integer lis)
        notFollowedBy (parens lis (commaSep lis parseParam))
        mInit <- optionMaybe parseInitializer
        let realType = maybe t (\n -> TArray t n) mSize
        return (realType, name, mInit)

    let mkLet (ty, v, me) = case isConst of
            Just _  -> LetConst ty v (maybe (defaultInit ty) id me)
            Nothing -> case me of
                Just val -> LetType ty v val
                Nothing  -> LetUninit ty v

    return (foldr1 Seq (map mkLet decls))

-- { 1 , 2 , 3 }
parseInitializer :: Parser Exp
parseInitializer =
      try (reservedOp lis "=" >> intexp)                 -- viejo caso escalar
  <|> try (reservedOp lis "=" >> braces lis initList)    -- NUEVO

initList :: Parser Exp
initList = do
  xs <- commaSep lis intexp
  return (InitList xs)



-- *e = e
parseAssignDeref :: Parser Comm
parseAssignDeref = do
  ptrStar
  lhsExp <- parseFactor
  reservedOp lis "="
  rhsExp <- try (BoolAsIntExp <$> boolexp) <|> intexp
  return (AssignDeref (Deref lhsExp) rhsExp)


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
defaultInit (TPtr _) = error "defaultInit: no existe valor por defecto para punteros"



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
    t0   <- typeParser                       -- palabra clave: int / char / …
    ptrs <- many ptrStar      -- cero o más ‘*’
    let t = foldl (\acc _ -> TPtr acc) t0 ptrs   -- int **  →  TPtr (TPtr TInt)

    nm   <- identifier lis                   -- nombre del parámetro

    -- ¿viene “[]” indicando array?
    mArr <- optionMaybe (brackets lis (return ()))
    case mArr of
      Just _  -> return (TArray t 0, nm)     -- f(int *p[])  ⇒  puntero a inicio
      Nothing -> return (t, nm)

typeParser :: Parser Type
typeParser =   try (reserved lis "int"    >> return TInt)
           <|> try (reserved lis "float"  >> return TFloat)
           <|> try (reserved lis "double" >> return TDouble)
           <|> try (reserved lis "char"   >> return TChar)
           <|> try (reserved lis "long"   >> return TLong)
           <|> try (reserved lis "short"  >> return TShort)
           <|> try (reserved lis "string" >> return TString)
           <|> try (reserved lis "void"   >> return TVoid)

ptrStar :: Parser ()
ptrStar = lexeme lis (char '*') >> return ()

------------------------------------
-- Funcion de parseo principal
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser program)
