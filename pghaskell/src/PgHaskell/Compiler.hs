{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Compiler where

import PgHaskell.Internal
import PgHaskell.Internal.Elog

import Data.List (partition)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

data PgProc = PgProc { procCode :: String
                     , procImports :: [ModuleName]
                     } deriving (Show)

type Callable = [Int] -> PG Int

compileFunction :: Text -> IO (Either InterpreterError Callable)
compileFunction txt = runInterpreter $ do
    unsafeSetGhcOption "-v"
    unsafeSetGhcOption "-fobject-code"
    setImports $ ["Prelude"
                 ,"PgHaskell.Internal"
                 ,"PgHaskell.Internal.Elog"
                 ] ++ procImports pgproc
    interpret (procCode pgproc) fallBackCode
  where
    pgproc = processSource txt
    fallBackCode _ = do
        elog ElogWarning "Fallback pghaskell procedure"
        pure (0 :: Int)

validateFunction :: Text -> IO (Either InterpreterError Bool)
validateFunction txt = runInterpreter $ do
    unsafeSetGhcOption "-v"
    unsafeSetGhcOption "-fobject-code"
    setImports $ ["Prelude"
                 ,"PgHaskell.Internal"
                 ,"PgHaskell.Internal.Elog"
                 ] ++ procImports pgproc
    typeChecks (procCode pgproc)
  where
    pgproc = processSource txt

processSource :: Text -> PgProc
processSource txt = PgProc { procCode = body
                           , procImports = map (Text.unpack . Text.drop 7) imports
                           }
  where
    isImport = Text.isPrefixOf "import "
    (imports,source) = partition isImport (Text.lines txt)
    prefix = "\\values -> do"
    body = Text.unpack $ Text.unlines . (prefix:) $ map (\l -> " " <> l) source

test :: IO ()
test = do
    r <- compileFunction "putStrLn \"DERP\""
    case r of
      Left e -> print e
      Right f -> pure (f []) >> print (typeRep $ f [i])
  where
    i = 123 :: Int

