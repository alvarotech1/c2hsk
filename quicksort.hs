import Data.IORef
import Control.Monad (when)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)


myQsort :: Int32 -> Int32 -> A.IOArray Int32 Int32 -> IO ()
myQsort min max arr = do
  min_ref0 <- newIORef min
  max_ref1 <- newIORef max
  arr_ref2 <- newIORef arr
  tmp0 <- newIORef (Nothing :: Maybe ())
  tmp1 <- readIORef min_ref0
  tmp2 <- readIORef max_ref1
  when ((tmp1 < tmp2)) $ do
    tmp3 <- readIORef min_ref0
    contMenores_ref3 <- newIORef (tmp3)
    done <- fmap isJust (readIORef tmp0)
    when (not done) $ do
      aux_ref4 <- newIORef ((0 :: Int32))
      done <- fmap isJust (readIORef tmp0)
      when (not done) $ do
        tmp4 <- readIORef max_ref1
        tmp5 <- readIORef arr_ref2
        tmp6 <- A.readArray tmp5 tmp4
        pivote_ref5 <- newIORef (tmp6)
        done <- fmap isJust (readIORef tmp0)
        when (not done) $ do
          tmp7 <- readIORef min_ref0
          i_ref6 <- newIORef (tmp7)
          tmp9 <- newIORef False
          let tmp8 = do
                        quit <- readIORef tmp9
                        done <- fmap isJust (readIORef tmp0)
                        tmp10 <- readIORef i_ref6
                        tmp11 <- readIORef max_ref1
                        when (not quit && not done && ((tmp10 < tmp11))) $ do
                          tmp12 <- readIORef pivote_ref5
                          tmp13 <- readIORef i_ref6
                          tmp14 <- readIORef arr_ref2
                          tmp15 <- A.readArray tmp14 tmp13
                          when ((tmp12 > tmp15)) $ do
                            tmp16 <- readIORef contMenores_ref3
                            tmp17 <- readIORef arr_ref2
                            tmp18 <- A.readArray tmp17 tmp16
                            writeIORef aux_ref4 (tmp18)
                            quit <- readIORef tmp9
                            done <- fmap isJust (readIORef tmp0)
                            when (not quit && not done) $ do
                              tmp19 <- readIORef contMenores_ref3
                              tmp20 <- readIORef i_ref6
                              tmp21 <- readIORef arr_ref2
                              tmp22 <- A.readArray tmp21 tmp20
                              tmp23 <- readIORef arr_ref2
                              A.writeArray tmp23 (tmp19) (tmp22)
                              quit <- readIORef tmp9
                              done <- fmap isJust (readIORef tmp0)
                              when (not quit && not done) $ do
                                tmp24 <- readIORef i_ref6
                                tmp25 <- readIORef aux_ref4
                                tmp26 <- readIORef arr_ref2
                                A.writeArray tmp26 (tmp24) (tmp25)
                                quit <- readIORef tmp9
                                done <- fmap isJust (readIORef tmp0)
                                when (not quit && not done) $ do
                                  tmp27 <- readIORef contMenores_ref3
                                  writeIORef contMenores_ref3 (tmp27 + 1)
                                -- fin guardia
                              -- fin guardia
                            -- fin guardia
                          tmp28 <- readIORef i_ref6
                          writeIORef i_ref6 (tmp28 + 1)
                          tmp8
          tmp8
          done <- fmap isJust (readIORef tmp0)
          when (not done) $ do
            tmp29 <- readIORef contMenores_ref3
            tmp30 <- readIORef arr_ref2
            tmp31 <- A.readArray tmp30 tmp29
            writeIORef aux_ref4 (tmp31)
            done <- fmap isJust (readIORef tmp0)
            when (not done) $ do
              tmp32 <- readIORef contMenores_ref3
              tmp33 <- readIORef max_ref1
              tmp34 <- readIORef arr_ref2
              tmp35 <- A.readArray tmp34 tmp33
              tmp36 <- readIORef arr_ref2
              A.writeArray tmp36 (tmp32) (tmp35)
              done <- fmap isJust (readIORef tmp0)
              when (not done) $ do
                tmp37 <- readIORef max_ref1
                tmp38 <- readIORef aux_ref4
                tmp39 <- readIORef arr_ref2
                A.writeArray tmp39 (tmp37) (tmp38)
                done <- fmap isJust (readIORef tmp0)
                when (not done) $ do
                  tmp40 <- readIORef contMenores_ref3
                  tmp41 <- readIORef max_ref1
                  tmp42 <- readIORef arr_ref2
                  _ <- myQsort (tmp40 + (1 :: Int32)) tmp41 tmp42
                  done <- fmap isJust (readIORef tmp0)
                  when (not done) $ do
                    tmp43 <- readIORef min_ref0
                    tmp44 <- readIORef contMenores_ref3
                    tmp45 <- readIORef arr_ref2
                    _ <- myQsort tmp43 (tmp44 - (1 :: Int32)) tmp45
                  -- fin guardia
                -- fin guardia
              -- fin guardia
            -- fin guardia
          -- fin guardia
        -- fin guardia
      -- fin guardia
    -- fin guardia
  return ()


main :: IO ()
main = do
  tmp46 <- newIORef (Nothing :: Maybe Int32)
  tmp47 <- (A.newListArray ((0 :: Int32),9 :: Int32) [(12 :: Int32),(4 :: Int32),(7 :: Int32),(3 :: Int32),(1 :: Int32),(8 :: Int32),(9 :: Int32),(15 :: Int32),(6 :: Int32),(2 :: Int32)] :: IO (A.IOArray Int32 Int32))
  arr_ref7 <- newIORef tmp47
  done <- fmap isJust (readIORef tmp46)
  when (not done) $ do
    i_ref8 <- newIORef ((0 :: Int32))
    tmp49 <- newIORef False
    let tmp48 = do
                   quit <- readIORef tmp49
                   done <- fmap isJust (readIORef tmp46)
                   tmp50 <- readIORef i_ref8
                   when (not quit && not done && ((tmp50 < (10 :: Int32)))) $ do
                     tmp51 <- readIORef i_ref8
                     tmp52 <- readIORef arr_ref7
                     tmp53 <- A.readArray tmp52 tmp51
                     putStrLn ("" ++ show (tmp53) ++ " ")
                     tmp54 <- readIORef i_ref8
                     writeIORef i_ref8 (tmp54 + 1)
                     tmp48
    tmp48
    done <- fmap isJust (readIORef tmp46)
    when (not done) $ do
      putStrLn ("------------")
      done <- fmap isJust (readIORef tmp46)
      when (not done) $ do
        tmp55 <- readIORef arr_ref7
        _ <- myQsort (0 :: Int32) (9 :: Int32) tmp55
        done <- fmap isJust (readIORef tmp46)
        when (not done) $ do
          i_ref9 <- newIORef ((0 :: Int32))
          tmp57 <- newIORef False
          let tmp56 = do
                         quit <- readIORef tmp57
                         done <- fmap isJust (readIORef tmp46)
                         tmp58 <- readIORef i_ref9
                         when (not quit && not done && ((tmp58 < (10 :: Int32)))) $ do
                           tmp59 <- readIORef i_ref9
                           tmp60 <- readIORef arr_ref7
                           tmp61 <- A.readArray tmp60 tmp59
                           putStrLn ("" ++ show (tmp61) ++ " ")
                           tmp62 <- readIORef i_ref9
                           writeIORef i_ref9 (tmp62 + 1)
                           tmp56
          tmp56
          done <- fmap isJust (readIORef tmp46)
          when (not done) $ do
            mAlready <- readIORef tmp46
            case mAlready of
              Just _  -> pure ()
              Nothing -> writeIORef tmp46 (Just ((0 :: Int32)))
          -- fin guardia
        -- fin guardia
      -- fin guardia
    -- fin guardia
  -- fin guardia
  return ()
