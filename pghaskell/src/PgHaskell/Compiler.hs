{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Compiler where

import PgHaskell.Internal

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

compileFunction :: Text -> IO (Either InterpreterError ([Int] -> IO [Int]))
compileFunction txt = runInterpreter $ do
    unsafeSetGhcOption "-v"
    sp <- get searchPath
    liftIO (print sp)
    setImports ["Prelude"
               ,"Control.Monad.IO.Class"
               ,"Data.Monoid"
            -- XXX: GHC linked to postgresql cannot find our internal libraries for some reason :(
            --   ,"PgHaskell.Internal"
            --   ,"PgHaskell.Elog"
               ,"Language.Haskell.Interpreter"
               ]
    interpret body pure
  where
    prefix = "\\values -> do"
    body = Text.unpack $ Text.unlines . (prefix:) $ map (\l -> " " <> l) $ Text.lines txt

test :: IO ()
test = do
    r <- compileFunction "\\is -> let p = (pure :: [PgValue] -> IO [PgValue]) in p $ map (id) is"
    case r of
      Left e -> print e
      Right f -> pure (f []) >> print 0 -- (typeRep $ f [MkPgValue i])
  where
    i = 123 :: Int

