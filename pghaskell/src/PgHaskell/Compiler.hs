{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Compiler where

import PgHaskell.Internal

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Language.Haskell.Interpreter

compileFunction :: Text -> IO (Either InterpreterError ([PgValue] -> IO [PgValue]))
compileFunction txt = runInterpreter $ do
    setImports ["Prelude"
               ,"Data.Monoid"
               ,"PgHaskell.Internal"
               ]
    interpret fbody pure
  where
    fbody = Text.unpack txt

test :: IO ()
test = do
    r <- compileFunction "\\is -> let p = (pure :: [PgValue] -> IO [PgValue]) in p $ map (id) is"
    case r of
      Left e -> print e
      Right f -> pure (f []) >> print (typeRep $ f [MkPgValue i])
  where
    i = 123 :: Int

