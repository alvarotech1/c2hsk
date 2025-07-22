import Data.IORef
import Control.Monad (when)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)


cambiarPrimero :: IORef (Int32) -> IO ()
cambiarPrimero arr = do
  arr_ref0 <- newIORef arr
  tmp0 <- newIORef (Nothing :: Maybe ())
  tmp1 <- readIORef arr_ref0
  A.writeArray tmp1 ((0 :: Int32)) ((99 :: Int32))
  return ()


main :: IO ()
main = do
  tmp2 <- newIORef (Nothing :: Maybe Int32)
  tmp3 <- (A.newListArray ((0 :: Int32),3 :: Int32) [(1 :: Int32),(2 :: Int32),(3 :: Int32),(4 :: Int32)] :: IO (A.IOArray Int32 Int32))
  datos_ref1 <- newIORef tmp3
  done <- fmap isJust (readIORef tmp2)
  when (not done) $ do
    tmp4 <- readIORef datos_ref1
    _ <- cambiarPrimero tmp4
    done <- fmap isJust (readIORef tmp2)
    when (not done) $ do
      tmp5 <- readIORef datos_ref1
      tmp6 <- A.readArray tmp5 (0 :: Int32)
      putStrLn ("" ++ show (tmp6) ++ "")
      done <- fmap isJust (readIORef tmp2)
      when (not done) $ do
        mAlready <- readIORef tmp2
        case mAlready of
          Just _  -> pure ()
          Nothing -> writeIORef tmp2 (Just ((0 :: Int32)))
      -- fin guardia
    -- fin guardia
  -- fin guardia
  return ()
