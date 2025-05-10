module Main where

import Tandoori.Kludge.Show
import Tandoori.GHC    
import Tandoori.GHC.Parse
import Tandoori.GHC.Scope

import GHC    
import Outputable
import IOEnv

import System.Environment    
import System.IO
    
import Tandoori.Typing.Infer
import Tandoori.Typing.Show

import System.IO.Unsafe
--import Debug.Trace
import Text.Pretty.Simple

p :: (Monad m, Show a) => a -> m ()
p = return . unsafePerformIO . pPrintOpt defaultOutputOptions { _indentAmount = 2 }
    
typecheckMod mod = runDyn $ do
                     env <- getSession
                     (limports, ltydecls, group) <- liftIO $ runScope env mod
                     () <- return $ unsafePerformIO $ putStrLn "ty decls"
                     () <- p ltydecls
                     () <- return $ unsafePerformIO $ putStrLn "group"
                     () <- p group
                     return $ infer (map unLoc ltydecls) group
                                                                    
main' [src_filename] = do mod <- parseMod src_filename
                          (c, errors) <- typecheckMod mod
                          if not(null errors)
                            then mapM_ (\ error -> printErrs $ ppr error $ mkErrStyle neverQualify) errors
                            else return ()
                          case c of
                            Just (ctxt, m) -> printCtxt ctxt
                            Nothing -> return ()
                          return c

main' _ = error "Usage: tandoori filename.hs" 

main = do
  hSetEncoding stdout utf8
  args <- getArgs
  main' args
