import Data.IORef
import Control.Monad (when)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)


readCString :: A.IOArray Int32 Char -> Int -> IO String
readCString arr size = go 0
  where
    go i | i >= size = return []
         | otherwise = do
             c <- A.readArray arr (fromIntegral i)
             if c == '\0' then return []
             else do
                 rest <- go (i+1)
                 return (c : rest)



main :: IO ()
main = do
  tmp0 <- newIORef (Nothing :: Maybe Int32)
  tmp1 <- (A.newListArray (0,5) ['H','E','L','L','O','\NUL'] :: IO (A.IOArray Int32 Char))
  mensage_ref0 <- newIORef tmp1
  done <- fmap isJust (readIORef tmp0)
  when (not done) $ do
    tmp2 <- (A.newListArray (0,5) ['W','O','R','L','D','\NUL'] :: IO (A.IOArray Int32 Char))
    mensage2_ref1 <- newIORef tmp2
    done <- fmap isJust (readIORef tmp0)
    when (not done) $ do
      mensage3_ref2 <- newIORef ("PERRO")
      done <- fmap isJust (readIORef tmp0)
      when (not done) $ do
        tmp3 <- readIORef mensage_ref0
        tmp4 <- readCString tmp3 6
        tmp5 <- readIORef mensage2_ref1
        tmp6 <- readCString tmp5 6
        tmp7 <- readIORef mensage3_ref2
        putStrLn ("el mensaje escrito es: " ++ tmp4 ++ " " ++ tmp6 ++ " " ++ tmp7 ++ "")
      -- fin guardia
    -- fin guardia
  -- fin guardia
  return ()
