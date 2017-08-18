{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Compiler where

import PgHaskell.Internal

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

compileFunction :: Text -> IO (Either InterpreterError ([PgValue] -> IO [PgValue]))
compileFunction txt = runInterpreter $ do
    unsafeSetGhcOption "-v"
    setImports ["Prelude"
               ,"Control.Monad.IO.Class"
               ,"Data.Monoid"
               ,"PgHaskell.Internal"
               ,"PgHaskell.Elog"
               ]
    interpret body pure
  where
    prefix = "\\values -> do"
    body = Text.unpack $ Text.unlines . (prefix:) $ map (\l -> " " <> l) $ Text.lines txt

test :: IO ()
test = do
    r <- compileFunction "putStrLn \"DERP\""
    case r of
      Left e -> print e
      Right f -> pure (f []) >> print (typeRep $ f [MkPgValue i])
  where
    i = 123 :: Int

