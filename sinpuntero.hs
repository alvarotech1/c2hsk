import Data.IORef
import Control.Monad (when)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)



main :: IO ()
main = do
  tmp0 <- newIORef (Nothing :: Maybe Int32)
  x_ref0 <- newIORef ((1 :: Int32))
  done <- fmap isJust (readIORef tmp0)
  when (not done) $ do
    tmp1 <- readIORef x_ref0
    putStrLn ("" ++ show (tmp1) ++ "")
  -- fin guardia
  return ()
