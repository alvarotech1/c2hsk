import Data.IORef
import Control.Monad (when, void)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)



main :: IO ()
main = do
  tmp0 <- newIORef (Nothing :: Maybe Int32)
  j_salud_ref0 <- newIORef (0 :: Int32)
  j_energia_ref1 <- newIORef (0 :: Int32)
  done <- fmap isJust (readIORef tmp0)
  when (not done) $ do
    writeIORef j_salud_ref0 ((100 :: Int32))
    done <- fmap isJust (readIORef tmp0)
    when (not done) $ do
      writeIORef j_energia_ref1 ((50 :: Int32))
      done <- fmap isJust (readIORef tmp0)
      when (not done) $ do
        tmp1 <- readIORef j_salud_ref0
        putStrLn ("Salud inicial: " ++ show (tmp1) ++ "")
        done <- fmap isJust (readIORef tmp0)
        when (not done) $ do
          tmp2 <- readIORef j_energia_ref1
          putStrLn ("Energia inicial: " ++ show (tmp2) ++ "")
          done <- fmap isJust (readIORef tmp0)
          when (not done) $ do
            tmp3 <- readIORef j_salud_ref0
            writeIORef j_salud_ref0 ((tmp3 - (30 :: Int32)))
            done <- fmap isJust (readIORef tmp0)
            when (not done) $ do
              tmp4 <- readIORef j_energia_ref1
              writeIORef j_energia_ref1 ((tmp4 - (10 :: Int32)))
              done <- fmap isJust (readIORef tmp0)
              when (not done) $ do
                tmp5 <- readIORef j_salud_ref0
                putStrLn ("Salud restante: " ++ show (tmp5) ++ "")
                done <- fmap isJust (readIORef tmp0)
                when (not done) $ do
                  tmp6 <- readIORef j_energia_ref1
                  putStrLn ("Energia restante: " ++ show (tmp6) ++ "")
                
              
            
          
        
      
    
  
  return ()
