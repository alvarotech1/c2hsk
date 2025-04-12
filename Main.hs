module Main where

import System.Environment (getArgs)
import Parser (parseComm)
import Evaluador

main :: IO ()
main = do
    -- puedes usar getArgs si deseas pasarle el archivo por parámetro
    -- arg:_ <- getArgs
    -- run arg

    -- O simplemente forzar a leer "test.lis" para pruebas:
    let testFile = "test.lis"
    run testFile

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile = do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      --Right t    -> eval t -- Traducir a Haskell e imprimir
      Right t    -> print t  -- (debug) imprimir el AST crudo
