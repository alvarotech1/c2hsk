module Main where

import System.Environment (getArgs)
import System.FilePath      (replaceExtension)  -- sirve para poder cambiar la extension de un archivo. Ejemplo: .c -> .hs
import Parser (parseComm)
import Evaluador(generateCode)

main :: IO ()
main = do
    -- puedes usar getArgs si deseas pasarle el archivo por parámetro
    -- arg:_ <- getArgs
    -- run arg

    -- O simplemente forzar a leer "test.lis" para pruebas:
    let testFile = "test.lis"
    run testFile

-- Ejecuta un programa C, genera y guarda el .hs correspondiente
run :: FilePath -> IO ()
run ifile = do
    src <- readFile ifile
    case parseComm ifile src of
      Left err  -> print err
      Right ast -> do
      --Right ast    -> print ast  -- (debug) imprimir el AST crudo - # Comentar lo de abajo.

          -- 1) obtener el código Haskell ya formateado
          let codeStr = unlines (generateCode ast)

          -- 2) mostrarlo por consola (opcional)
          putStrLn codeStr

          -- 3) guardarlo en un .hs junto al .c
          let outFile = replaceExtension ifile "hs"
          writeFile outFile codeStr
