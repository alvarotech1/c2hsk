import Data.IORef
import Control.Monad (when)
import qualified Data.Array.IO as A
import Data.Maybe (isJust)
import Data.Int (Int32)


mostrarNumero :: Int32 -> IO ()
mostrarNumero op = do
  op_ref0 <- newIORef op
  tmp0 <- newIORef (Nothing :: Maybe ())
  tmp1 <- readIORef op_ref0
  tmp2 <- newIORef False
  tmp3 <- newIORef False
  quit <- readIORef tmp2
  done <- fmap isJust (readIORef tmp0)
  matched <- readIORef tmp3
  when (not quit && not done && (matched || (tmp1 == (1 :: Int32)))) $ do
    writeIORef tmp3 True
    putStrLn ("uno")
    quit <- readIORef tmp2
    done <- fmap isJust (readIORef tmp0)
    when (not quit && not done) $ do
      writeIORef tmp2 True
    -- fin guardia
  quit <- readIORef tmp2
  done <- fmap isJust (readIORef tmp0)
  matched <- readIORef tmp3
  when (not quit && not done && (matched || (tmp1 == (2 :: Int32)))) $ do
    writeIORef tmp3 True
    putStrLn ("dos")
    quit <- readIORef tmp2
    done <- fmap isJust (readIORef tmp0)
    when (not quit && not done) $ do
      writeIORef tmp2 True
    -- fin guardia
  quit <- readIORef tmp2
  done <- fmap isJust (readIORef tmp0)
  matched <- readIORef tmp3
  when (not quit && not done && (matched || (tmp1 == (3 :: Int32)))) $ do
    writeIORef tmp3 True
    putStrLn ("tres")
    quit <- readIORef tmp2
    done <- fmap isJust (readIORef tmp0)
    when (not quit && not done) $ do
      writeIORef tmp2 True
    -- fin guardia
  quit <- readIORef tmp2
  done <- fmap isJust (readIORef tmp0)
  matched <- readIORef tmp3
  when (not quit && not done && not matched) $ do
    putStrLn ("otro")
    quit <- readIORef tmp2
    done <- fmap isJust (readIORef tmp0)
    when (not quit && not done) $ do
      writeIORef tmp2 True
    -- fin guardia
  done <- fmap isJust (readIORef tmp0)
  when (not done) $ do
    putStrLn ("-- fin de mostrarNumero()")
  -- fin guardia
  return ()

procesar :: Int32 -> IO ()
procesar op = do
  op_ref1 <- newIORef op
  tmp4 <- newIORef (Nothing :: Maybe ())
  tmp5 <- readIORef op_ref1
  tmp6 <- newIORef False
  tmp7 <- newIORef False
  quit <- readIORef tmp6
  done <- fmap isJust (readIORef tmp4)
  matched <- readIORef tmp7
  when (not quit && not done && (matched || (tmp5 == (1 :: Int32)))) $ do
    writeIORef tmp7 True
    putStrLn ("proc uno")
    quit <- readIORef tmp6
    done <- fmap isJust (readIORef tmp4)
    when (not quit && not done) $ do
      mAlready <- readIORef tmp4
      case mAlready of
        Just _  -> pure ()
        Nothing -> writeIORef tmp4 (Just ())
    -- fin guardia
  quit <- readIORef tmp6
  done <- fmap isJust (readIORef tmp4)
  matched <- readIORef tmp7
  when (not quit && not done && (matched || (tmp5 == (2 :: Int32)))) $ do
    writeIORef tmp7 True
    putStrLn ("proc dos")
    quit <- readIORef tmp6
    done <- fmap isJust (readIORef tmp4)
    when (not quit && not done) $ do
      writeIORef tmp6 True
    -- fin guardia
  quit <- readIORef tmp6
  done <- fmap isJust (readIORef tmp4)
  matched <- readIORef tmp7
  when (not quit && not done && not matched) $ do
    putStrLn ("proc otro")
    quit <- readIORef tmp6
    done <- fmap isJust (readIORef tmp4)
    when (not quit && not done) $ do
      writeIORef tmp6 True
    -- fin guardia
  done <- fmap isJust (readIORef tmp4)
  when (not done) $ do
    putStrLn ("-- fin de procesar()")
  -- fin guardia
  return ()

combinacion :: Int32 -> IO ()
combinacion op = do
  op_ref2 <- newIORef op
  tmp8 <- newIORef (Nothing :: Maybe ())
  i_ref3 <- newIORef (0 :: Int32)
  num_ref4 <- newIORef (0 :: Int32)
  done <- fmap isJust (readIORef tmp8)
  when (not done) $ do
    tmp9 <- readIORef op_ref2
    tmp10 <- newIORef False
    tmp11 <- newIORef False
    quit <- readIORef tmp10
    done <- fmap isJust (readIORef tmp8)
    matched <- readIORef tmp11
    when (not quit && not done && (matched || (tmp9 == (1 :: Int32)))) $ do
      writeIORef tmp11 True
      writeIORef i_ref3 ((0 :: Int32))
      tmp13 <- newIORef False
      let tmp12 = do
                     quit <- readIORef tmp13
                     done <- fmap isJust (readIORef tmp8)
                     tmp14 <- readIORef i_ref3
                     when (not quit && not done && ((tmp14 < (3 :: Int32)))) $ do
                       tmp15 <- readIORef i_ref3
                       putStrLn ("for " ++ show (tmp15) ++ "")
                       tmp16 <- readIORef i_ref3
                       writeIORef i_ref3 (tmp16 + 1)
                       tmp12
      tmp12
      quit <- readIORef tmp10
      done <- fmap isJust (readIORef tmp8)
      when (not quit && not done) $ do
        writeIORef tmp10 True
      -- fin guardia
    quit <- readIORef tmp10
    done <- fmap isJust (readIORef tmp8)
    matched <- readIORef tmp11
    when (not quit && not done && (matched || (tmp9 == (2 :: Int32)))) $ do
      writeIORef tmp11 True
      writeIORef i_ref3 ((0 :: Int32))
      quit <- readIORef tmp10
      done <- fmap isJust (readIORef tmp8)
      when (not quit && not done) $ do
        tmp18 <- newIORef False
        let tmp17 = do
                       quit <- readIORef tmp18
                       done <- fmap isJust (readIORef tmp8)
                       tmp19 <- readIORef i_ref3
                       when (not quit && not done && ((tmp19 < (3 :: Int32)))) $ do
                         tmp20 <- readIORef i_ref3
                         putStrLn ("while " ++ show (tmp20) ++ "")
                         quit <- readIORef tmp18
                         done <- fmap isJust (readIORef tmp8)
                         when (not quit && not done) $ do
                           tmp21 <- readIORef i_ref3
                           writeIORef i_ref3 (tmp21 + 1)
                         -- fin guardia
                         tmp17
        tmp17
        quit <- readIORef tmp10
        done <- fmap isJust (readIORef tmp8)
        when (not quit && not done) $ do
          writeIORef tmp10 True
        -- fin guardia
      -- fin guardia
    quit <- readIORef tmp10
    done <- fmap isJust (readIORef tmp8)
    matched <- readIORef tmp11
    when (not quit && not done && (matched || (tmp9 == (3 :: Int32)))) $ do
      writeIORef tmp11 True
      tmp22 <- readIORef op_ref2
      if (tmp22 == (3 :: Int32)) then do
        putStrLn ("if-true")
      else do
        putStrLn ("if-false")
      quit <- readIORef tmp10
      done <- fmap isJust (readIORef tmp8)
      when (not quit && not done) $ do
        writeIORef tmp10 True
      -- fin guardia
    quit <- readIORef tmp10
    done <- fmap isJust (readIORef tmp8)
    matched <- readIORef tmp11
    when (not quit && not done && (matched || (tmp9 == (4 :: Int32)))) $ do
      writeIORef tmp11 True
      writeIORef i_ref3 ((0 :: Int32))
      quit <- readIORef tmp10
      done <- fmap isJust (readIORef tmp8)
      when (not quit && not done) $ do
        tmp24 <- newIORef False
        let tmp23 = do
                       quit <- readIORef tmp24
                       done <- fmap isJust (readIORef tmp8)
                       when (not quit && not done) $ do
                         tmp25 <- readIORef i_ref3
                         putStrLn ("do-while " ++ show (tmp25) ++ "")
                         quit <- readIORef tmp24
                         done <- fmap isJust (readIORef tmp8)
                         when (not quit && not done) $ do
                           tmp26 <- readIORef i_ref3
                           writeIORef i_ref3 (tmp26 + 1)
                         -- fin guardia
                         quit <- readIORef tmp24
                         done <- fmap isJust (readIORef tmp8)
                         tmp27 <- readIORef i_ref3
                         when (not quit && not done && ((tmp27 < (3 :: Int32)))) $ do
                           tmp23
        tmp23
        quit <- readIORef tmp10
        done <- fmap isJust (readIORef tmp8)
        when (not quit && not done) $ do
          writeIORef tmp10 True
        -- fin guardia
      -- fin guardia
    quit <- readIORef tmp10
    done <- fmap isJust (readIORef tmp8)
    matched <- readIORef tmp11
    when (not quit && not done && (matched || (tmp9 == (5 :: Int32)))) $ do
      writeIORef tmp11 True
      putStrLn ("Ingrese un entero (combinacion): ")
      quit <- readIORef tmp10
      done <- fmap isJust (readIORef tmp8)
      when (not quit && not done) $ do
        tmp28 <- getLine
        let ws = words tmp28
        writeIORef num_ref4 (read (ws !! 0) :: Int32)
        quit <- readIORef tmp10
        done <- fmap isJust (readIORef tmp8)
        when (not quit && not done) $ do
          tmp29 <- readIORef num_ref4
          putStrLn ("Recibido " ++ show (tmp29) ++ "")
          quit <- readIORef tmp10
          done <- fmap isJust (readIORef tmp8)
          when (not quit && not done) $ do
            writeIORef tmp10 True
          -- fin guardia
        -- fin guardia
      -- fin guardia
    quit <- readIORef tmp10
    done <- fmap isJust (readIORef tmp8)
    matched <- readIORef tmp11
    when (not quit && not done && not matched) $ do
      putStrLn ("comb default")
    done <- fmap isJust (readIORef tmp8)
    when (not done) $ do
      putStrLn ("-- fin de combinacion()")
    -- fin guardia
  -- fin guardia
  return ()

loopFor :: Int32 -> IO ()
loopFor n = do
  n_ref5 <- newIORef n
  tmp30 <- newIORef (Nothing :: Maybe ())
  i_ref6 <- newIORef (0 :: Int32)
  done <- fmap isJust (readIORef tmp30)
  when (not done) $ do
    tmp31 <- readIORef n_ref5
    tmp32 <- newIORef False
    tmp33 <- newIORef False
    quit <- readIORef tmp32
    done <- fmap isJust (readIORef tmp30)
    matched <- readIORef tmp33
    when (not quit && not done && (matched || (tmp31 == (0 :: Int32)))) $ do
      writeIORef tmp33 True
      writeIORef i_ref6 ((0 :: Int32))
      tmp35 <- newIORef False
      let tmp34 = do
                     quit <- readIORef tmp35
                     done <- fmap isJust (readIORef tmp30)
                     tmp36 <- readIORef i_ref6
                     when (not quit && not done && ((tmp36 < (5 :: Int32)))) $ do
                       tmp37 <- readIORef i_ref6
                       putStrLn ("for-i=" ++ show (tmp37) ++ "")
                       quit <- readIORef tmp35
                       done <- fmap isJust (readIORef tmp30)
                       when (not quit && not done) $ do
                         tmp38 <- readIORef i_ref6
                         when ((tmp38 == (2 :: Int32))) $ do
                           putStrLn ("return en i==2")
                           quit <- readIORef tmp35
                           done <- fmap isJust (readIORef tmp30)
                           when (not quit && not done) $ do
                             mAlready <- readIORef tmp30
                             case mAlready of
                               Just _  -> pure ()
                               Nothing -> writeIORef tmp30 (Just ())
                           -- fin guardia
                       -- fin guardia
                       tmp39 <- readIORef i_ref6
                       writeIORef i_ref6 (tmp39 + 1)
                       tmp34
      tmp34
      quit <- readIORef tmp32
      done <- fmap isJust (readIORef tmp30)
      when (not quit && not done) $ do
        putStrLn ("nunca se imprime")
        quit <- readIORef tmp32
        done <- fmap isJust (readIORef tmp30)
        when (not quit && not done) $ do
          writeIORef tmp32 True
        -- fin guardia
      -- fin guardia
    quit <- readIORef tmp32
    done <- fmap isJust (readIORef tmp30)
    matched <- readIORef tmp33
    when (not quit && not done && not matched) $ do
      putStrLn ("loopFor default")
      quit <- readIORef tmp32
      done <- fmap isJust (readIORef tmp30)
      when (not quit && not done) $ do
        writeIORef tmp32 True
      -- fin guardia
    done <- fmap isJust (readIORef tmp30)
    when (not done) $ do
      putStrLn ("-- fin de loopFor()")
    -- fin guardia
  -- fin guardia
  return ()

deepNested :: Int32 -> Int32 -> IO ()
deepNested cat k = do
  cat_ref7 <- newIORef cat
  k_ref8 <- newIORef k
  tmp40 <- newIORef (Nothing :: Maybe ())
  j_ref9 <- newIORef (0 :: Int32)
  done <- fmap isJust (readIORef tmp40)
  when (not done) $ do
    tmp41 <- readIORef cat_ref7
    tmp42 <- newIORef False
    tmp43 <- newIORef False
    quit <- readIORef tmp42
    done <- fmap isJust (readIORef tmp40)
    matched <- readIORef tmp43
    when (not quit && not done && (matched || (tmp41 == (1 :: Int32)))) $ do
      writeIORef tmp43 True
      putStrLn ("cat 1")
      quit <- readIORef tmp42
      done <- fmap isJust (readIORef tmp40)
      when (not quit && not done) $ do
        tmp44 <- readIORef k_ref8
        tmp45 <- newIORef False
        tmp46 <- newIORef False
        quit <- readIORef tmp45
        done <- fmap isJust (readIORef tmp40)
        matched <- readIORef tmp46
        when (not quit && not done && (matched || (tmp44 == (5 :: Int32)))) $ do
          writeIORef tmp46 True
          putStrLn ("  k=5, entro al while")
          quit <- readIORef tmp45
          done <- fmap isJust (readIORef tmp40)
          when (not quit && not done) $ do
            writeIORef j_ref9 ((0 :: Int32))
            quit <- readIORef tmp45
            done <- fmap isJust (readIORef tmp40)
            when (not quit && not done) $ do
              tmp48 <- newIORef False
              let tmp47 = do
                             quit <- readIORef tmp48
                             done <- fmap isJust (readIORef tmp40)
                             when (not quit && not done && (((1 :: Int32) == (1 :: Int32)))) $ do
                               tmp49 <- readIORef j_ref9
                               writeIORef j_ref9 (tmp49 + 1)
                               putStrLn ("    j=" ++ show (tmp49) ++ "")
                               quit <- readIORef tmp48
                               done <- fmap isJust (readIORef tmp40)
                               when (not quit && not done) $ do
                                 mAlready <- readIORef tmp40
                                 case mAlready of
                                   Just _  -> pure ()
                                   Nothing -> writeIORef tmp40 (Just ())
                               -- fin guardia
                               tmp47
              tmp47
            -- fin guardia
          -- fin guardia
        quit <- readIORef tmp45
        done <- fmap isJust (readIORef tmp40)
        matched <- readIORef tmp46
        when (not quit && not done && not matched) $ do
          putStrLn ("  sub default")
        quit <- readIORef tmp42
        done <- fmap isJust (readIORef tmp40)
        when (not quit && not done) $ do
          putStrLn ("fin cat 1")
          quit <- readIORef tmp42
          done <- fmap isJust (readIORef tmp40)
          when (not quit && not done) $ do
            writeIORef tmp42 True
          -- fin guardia
        -- fin guardia
      -- fin guardia
    quit <- readIORef tmp42
    done <- fmap isJust (readIORef tmp40)
    matched <- readIORef tmp43
    when (not quit && not done && not matched) $ do
      putStrLn ("cat default")
    done <- fmap isJust (readIORef tmp40)
    when (not done) $ do
      putStrLn ("-- fin de deepNested()")
    -- fin guardia
  -- fin guardia
  return ()

mix :: Int32 -> IO ()
mix x = do
  x_ref10 <- newIORef x
  tmp50 <- newIORef (Nothing :: Maybe ())
  p_ref11 <- newIORef (0 :: Int32)
  done <- fmap isJust (readIORef tmp50)
  when (not done) $ do
    tmp51 <- readIORef x_ref10
    tmp52 <- newIORef False
    tmp53 <- newIORef False
    quit <- readIORef tmp52
    done <- fmap isJust (readIORef tmp50)
    matched <- readIORef tmp53
    when (not quit && not done && (matched || (tmp51 == (2 :: Int32)))) $ do
      writeIORef tmp53 True
      putStrLn ("inner switch con return")
      quit <- readIORef tmp52
      done <- fmap isJust (readIORef tmp50)
      when (not quit && not done) $ do
        mAlready <- readIORef tmp50
        case mAlready of
          Just _  -> pure ()
          Nothing -> writeIORef tmp50 (Just ())
      -- fin guardia
    quit <- readIORef tmp52
    done <- fmap isJust (readIORef tmp50)
    matched <- readIORef tmp53
    when (not quit && not done && not matched) $ do
      putStrLn ("inner default")
    done <- fmap isJust (readIORef tmp50)
    when (not done) $ do
      writeIORef p_ref11 ((0 :: Int32))
      tmp55 <- newIORef False
      let tmp54 = do
                     quit <- readIORef tmp55
                     done <- fmap isJust (readIORef tmp50)
                     tmp56 <- readIORef p_ref11
                     when (not quit && not done && ((tmp56 < (3 :: Int32)))) $ do
                       tmp57 <- readIORef p_ref11
                       putStrLn ("for-p=" ++ show (tmp57) ++ "")
                       tmp58 <- readIORef p_ref11
                       writeIORef p_ref11 (tmp58 + 1)
                       tmp54
      tmp54
      done <- fmap isJust (readIORef tmp50)
      when (not done) $ do
        putStrLn ("-- fin de mix()")
      -- fin guardia
    -- fin guardia
  -- fin guardia
  return ()

analizar :: Int32 -> Int32 -> IO ()
analizar categoria subCategoria = do
  categoria_ref12 <- newIORef categoria
  subCategoria_ref13 <- newIORef subCategoria
  tmp59 <- newIORef (Nothing :: Maybe ())
  tmp60 <- readIORef categoria_ref12
  tmp61 <- newIORef False
  tmp62 <- newIORef False
  quit <- readIORef tmp61
  done <- fmap isJust (readIORef tmp59)
  matched <- readIORef tmp62
  when (not quit && not done && (matched || (tmp60 == (0 :: Int32)))) $ do
    writeIORef tmp62 True
    putStrLn ("Categoria 0")
    quit <- readIORef tmp61
    done <- fmap isJust (readIORef tmp59)
    when (not quit && not done) $ do
      writeIORef tmp61 True
    -- fin guardia
  quit <- readIORef tmp61
  done <- fmap isJust (readIORef tmp59)
  matched <- readIORef tmp62
  when (not quit && not done && (matched || (tmp60 == (1 :: Int32)))) $ do
    writeIORef tmp62 True
    putStrLn ("Categoria 1")
    quit <- readIORef tmp61
    done <- fmap isJust (readIORef tmp59)
    when (not quit && not done) $ do
      tmp63 <- readIORef subCategoria_ref13
      tmp64 <- newIORef False
      tmp65 <- newIORef False
      quit <- readIORef tmp64
      done <- fmap isJust (readIORef tmp59)
      matched <- readIORef tmp65
      when (not quit && not done && (matched || (tmp63 == (1 :: Int32)))) $ do
        writeIORef tmp65 True
        putStrLn ("  Sub 1-1")
        quit <- readIORef tmp64
        done <- fmap isJust (readIORef tmp59)
        when (not quit && not done) $ do
          writeIORef tmp64 True
        -- fin guardia
      quit <- readIORef tmp64
      done <- fmap isJust (readIORef tmp59)
      matched <- readIORef tmp65
      when (not quit && not done && (matched || (tmp63 == (2 :: Int32)))) $ do
        writeIORef tmp65 True
        putStrLn ("  Sub 1-2")
        quit <- readIORef tmp64
        done <- fmap isJust (readIORef tmp59)
        when (not quit && not done) $ do
          writeIORef tmp64 True
        -- fin guardia
      quit <- readIORef tmp64
      done <- fmap isJust (readIORef tmp59)
      matched <- readIORef tmp65
      when (not quit && not done && not matched) $ do
        putStrLn ("  Sub 1-default")
      quit <- readIORef tmp61
      done <- fmap isJust (readIORef tmp59)
      when (not quit && not done) $ do
        putStrLn ("-- fin subcat 1 --")
        quit <- readIORef tmp61
        done <- fmap isJust (readIORef tmp59)
        when (not quit && not done) $ do
          writeIORef tmp61 True
        -- fin guardia
      -- fin guardia
    -- fin guardia
  quit <- readIORef tmp61
  done <- fmap isJust (readIORef tmp59)
  matched <- readIORef tmp62
  when (not quit && not done && (matched || (tmp60 == (2 :: Int32)))) $ do
    writeIORef tmp62 True
    putStrLn ("Categoria 2")
    quit <- readIORef tmp61
    done <- fmap isJust (readIORef tmp59)
    when (not quit && not done) $ do
      writeIORef tmp61 True
    -- fin guardia
  quit <- readIORef tmp61
  done <- fmap isJust (readIORef tmp59)
  matched <- readIORef tmp62
  when (not quit && not done && not matched) $ do
    putStrLn ("Categoria default")
  done <- fmap isJust (readIORef tmp59)
  when (not done) $ do
    putStrLn ("-- fin de analizar()")
  -- fin guardia
  return ()


main :: IO ()
main = do
  tmp66 <- newIORef (Nothing :: Maybe Int32)
  i_ref14 <- newIORef (0 :: Int32)
  done <- fmap isJust (readIORef tmp66)
  when (not done) $ do
    tmp67 <- (A.newListArray ((0 :: Int32),4 :: Int32) [(1 :: Int32),(2 :: Int32),(3 :: Int32),(4 :: Int32),(5 :: Int32)] :: IO (A.IOArray Int32 Int32))
    tests_ref15 <- newIORef tmp67
    done <- fmap isJust (readIORef tmp66)
    when (not done) $ do
      putStrLn ("=== mostrarNumero ===")
      done <- fmap isJust (readIORef tmp66)
      when (not done) $ do
        _ <- mostrarNumero (1 :: Int32)
        done <- fmap isJust (readIORef tmp66)
        when (not done) $ do
          _ <- mostrarNumero (2 :: Int32)
          done <- fmap isJust (readIORef tmp66)
          when (not done) $ do
            _ <- mostrarNumero (5 :: Int32)
            done <- fmap isJust (readIORef tmp66)
            when (not done) $ do
              putStrLn ("=== procesar ===")
              done <- fmap isJust (readIORef tmp66)
              when (not done) $ do
                _ <- procesar (1 :: Int32)
                done <- fmap isJust (readIORef tmp66)
                when (not done) $ do
                  _ <- procesar (2 :: Int32)
                  done <- fmap isJust (readIORef tmp66)
                  when (not done) $ do
                    _ <- procesar (7 :: Int32)
                    done <- fmap isJust (readIORef tmp66)
                    when (not done) $ do
                      putStrLn ("=== combinacion ===")
                      done <- fmap isJust (readIORef tmp66)
                      when (not done) $ do
                        writeIORef i_ref14 ((0 :: Int32))
                        tmp69 <- newIORef False
                        let tmp68 = do
                                       quit <- readIORef tmp69
                                       done <- fmap isJust (readIORef tmp66)
                                       tmp70 <- readIORef i_ref14
                                       when (not quit && not done && ((tmp70 < (5 :: Int32)))) $ do
                                         tmp71 <- readIORef i_ref14
                                         tmp72 <- readIORef tests_ref15
                                         tmp73 <- A.readArray tmp72 tmp71
                                         _ <- combinacion tmp73
                                         tmp74 <- readIORef i_ref14
                                         writeIORef i_ref14 (tmp74 + 1)
                                         tmp68
                        tmp68
                        done <- fmap isJust (readIORef tmp66)
                        when (not done) $ do
                          putStrLn ("=== loopFor ===")
                          done <- fmap isJust (readIORef tmp66)
                          when (not done) $ do
                            _ <- loopFor (0 :: Int32)
                            done <- fmap isJust (readIORef tmp66)
                            when (not done) $ do
                              _ <- loopFor (9 :: Int32)
                              done <- fmap isJust (readIORef tmp66)
                              when (not done) $ do
                                putStrLn ("=== deepNested ===")
                                done <- fmap isJust (readIORef tmp66)
                                when (not done) $ do
                                  _ <- deepNested (1 :: Int32) (5 :: Int32)
                                  done <- fmap isJust (readIORef tmp66)
                                  when (not done) $ do
                                    _ <- deepNested (1 :: Int32) (9 :: Int32)
                                    done <- fmap isJust (readIORef tmp66)
                                    when (not done) $ do
                                      putStrLn ("=== mix ===")
                                      done <- fmap isJust (readIORef tmp66)
                                      when (not done) $ do
                                        _ <- mix (2 :: Int32)
                                        done <- fmap isJust (readIORef tmp66)
                                        when (not done) $ do
                                          _ <- mix (3 :: Int32)
                                          done <- fmap isJust (readIORef tmp66)
                                          when (not done) $ do
                                            putStrLn ("=== analizar ===")
                                            done <- fmap isJust (readIORef tmp66)
                                            when (not done) $ do
                                              _ <- analizar (1 :: Int32) (2 :: Int32)
                                              done <- fmap isJust (readIORef tmp66)
                                              when (not done) $ do
                                                _ <- analizar (0 :: Int32) (0 :: Int32)
                                                done <- fmap isJust (readIORef tmp66)
                                                when (not done) $ do
                                                  _ <- analizar (1 :: Int32) (9 :: Int32)
                                                  done <- fmap isJust (readIORef tmp66)
                                                  when (not done) $ do
                                                    _ <- analizar (3 :: Int32) (0 :: Int32)
                                                    done <- fmap isJust (readIORef tmp66)
                                                    when (not done) $ do
                                                      putStrLn ("=== scanf final de control ===")
                                                      done <- fmap isJust (readIORef tmp66)
                                                      when (not done) $ do
                                                        putStrLn ("Ingrese otro entero (final): ")
                                                        done <- fmap isJust (readIORef tmp66)
                                                        when (not done) $ do
                                                          z_ref16 <- newIORef (0 :: Int32)
                                                          done <- fmap isJust (readIORef tmp66)
                                                          when (not done) $ do
                                                            tmp75 <- getLine
                                                            let ws = words tmp75
                                                            writeIORef z_ref16 (read (ws !! 0) :: Int32)
                                                            done <- fmap isJust (readIORef tmp66)
                                                            when (not done) $ do
                                                              tmp76 <- readIORef z_ref16
                                                              putStrLn ("Fin. Leí " ++ show (tmp76) ++ "")
                                                              done <- fmap isJust (readIORef tmp66)
                                                              when (not done) $ do
                                                                mAlready <- readIORef tmp66
                                                                case mAlready of
                                                                  Just _  -> pure ()
                                                                  Nothing -> writeIORef tmp66 (Just ((0 :: Int32)))
                                                              -- fin guardia
                                                            -- fin guardia
                                                          -- fin guardia
                                                        -- fin guardia
                                                      -- fin guardia
                                                    -- fin guardia
                                                  -- fin guardia
                                                -- fin guardia
                                              -- fin guardia
                                            -- fin guardia
                                          -- fin guardia
                                        -- fin guardia
                                      -- fin guardia
                                    -- fin guardia
                                  -- fin guardia
                                -- fin guardia
                              -- fin guardia
                            -- fin guardia
                          -- fin guardia
                        -- fin guardia
                      -- fin guardia
                    -- fin guardia
                  -- fin guardia
                -- fin guardia
              -- fin guardia
            -- fin guardia
          -- fin guardia
        -- fin guardia
      -- fin guardia
    -- fin guardia
  -- fin guardia
  return ()
