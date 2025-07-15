import Data.IORef
import Control.Monad (when)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)


modificar_doble :: IORef (Int32) -> IO ()
modificar_doble p = do
  p_ref0 <- newIORef p
  tmp0 <- newIORef (Nothing :: Maybe ())
  tmp1 <- readIORef p_ref0
  tmp2 <- readIORef p_ref0
  tmp3 <- readIORef tmp2
  writeIORef tmp1 ((tmp3 * (2 :: Int32)))
  return ()


main :: IO ()
main = do
  tmp4 <- newIORef (Nothing :: Maybe Int32)
  a_ref1 <- newIORef ((2 :: Int32))
  done <- fmap isJust (readIORef tmp4)
  when (not done) $ do
    p_ref2 <- newIORef (a_ref1)
    done <- fmap isJust (readIORef tmp4)
    when (not done) $ do
      tmp5 <- readIORef p_ref2
      tmp6 <- readIORef tmp5
      putStrLn ("" ++ show (tmp6) ++ "")
      done <- fmap isJust (readIORef tmp4)
      when (not done) $ do
        tmp7 <- readIORef p_ref2
        _ <- modificar_doble tmp7
        done <- fmap isJust (readIORef tmp4)
        when (not done) $ do
          tmp8 <- readIORef p_ref2
          tmp9 <- readIORef tmp8
          putStrLn ("" ++ show (tmp9) ++ "")
        -- fin guardia
      -- fin guardia
    -- fin guardia
  -- fin guardia
  return ()
