import Data.IORef
import Control.Monad (when)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)


swap :: IORef (Int32) -> IORef (Int32) -> IO ()
swap a b = do
  a_ref0 <- newIORef a
  b_ref1 <- newIORef b
  tmp0 <- newIORef (Nothing :: Maybe ())
  tmp1 <- readIORef a_ref0
  tmp2 <- readIORef tmp1
  tmp_ref2 <- newIORef (tmp2)
  done <- fmap isJust (readIORef tmp0)
  when (not done) $ do
    tmp3 <- readIORef a_ref0
    tmp4 <- readIORef b_ref1
    tmp5 <- readIORef tmp4
    writeIORef tmp3 (tmp5)
    done <- fmap isJust (readIORef tmp0)
    when (not done) $ do
      tmp6 <- readIORef b_ref1
      tmp7 <- readIORef tmp_ref2
      writeIORef tmp6 (tmp7)
    -- fin guardia
  -- fin guardia
  return ()


main :: IO ()
main = do
  tmp8 <- newIORef (Nothing :: Maybe Int32)
  x_ref3 <- newIORef ((3 :: Int32))
  y_ref4 <- newIORef ((7 :: Int32))
  done <- fmap isJust (readIORef tmp8)
  when (not done) $ do
    p1_ref5 <- newIORef (x_ref3)
    done <- fmap isJust (readIORef tmp8)
    when (not done) $ do
      p2_ref6 <- newIORef (y_ref4)
      done <- fmap isJust (readIORef tmp8)
      when (not done) $ do
        tmp9 <- readIORef p1_ref5
        tmp10 <- readIORef p2_ref6
        _ <- swap tmp9 tmp10
        done <- fmap isJust (readIORef tmp8)
        when (not done) $ do
          tmp11 <- readIORef x_ref3
          tmp12 <- readIORef y_ref4
          putStrLn ("" ++ show (tmp11) ++ " " ++ show (tmp12) ++ "")
          done <- fmap isJust (readIORef tmp8)
          when (not done) $ do
            mAlready <- readIORef tmp8
            case mAlready of
              Just _  -> pure ()
              Nothing -> writeIORef tmp8 (Just ((0 :: Int32)))
          -- fin guardia
        -- fin guardia
      -- fin guardia
    -- fin guardia
  -- fin guardia
  return ()
