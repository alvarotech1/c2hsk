import Data.IORef
import Control.Monad (when, void)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)



main :: IO ()
main = do
  tmp0 <- newIORef (Nothing :: Maybe Int32)
  p_edad_ref0 <- newIORef (0 :: Int32)
  p_altura_ref1 <- newIORef (0.0 :: Float)
  done <- fmap isJust (readIORef tmp0)
  when (not done) $ do
    writeIORef p_edad_ref0 ((30 :: Int32))
    done <- fmap isJust (readIORef tmp0)
    when (not done) $ do
      writeIORef p_altura_ref1 (1.75)
      done <- fmap isJust (readIORef tmp0)
      when (not done) $ do
        tmp1 <- readIORef p_edad_ref0
        putStrLn ("" ++ show (tmp1) ++ "")
        done <- fmap isJust (readIORef tmp0)
        when (not done) $ do
          tmp2 <- readIORef p_altura_ref1
          putStrLn ("" ++ show (tmp2) ++ "")
        
      
    
  
  return ()
