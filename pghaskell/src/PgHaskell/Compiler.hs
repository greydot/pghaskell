{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Compiler where

import PgHaskell.CTypes
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

type Callable = [ArgValue] -> PG Datum

compileFunction :: ProcInfo -> IO (Either InterpreterError Callable)
compileFunction pinfo = runInterpreter $ do
    setupGhc (procImports pgproc)
    interpret (procCode pgproc) fallBackCode
  where
    pgproc = processSource (procText pinfo)
    fallBackCode _ = do
        elog ElogWarning "Fallback pghaskell procedure"
        pure (Datum 0)

validateFunction :: Text -> IO (Either InterpreterError Bool)
validateFunction txt = runInterpreter $ do
    setupGhc (procImports pgproc)
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
    body = Text.unpack $ Text.unlines . (prefix:) $ map (\l -> "  " <> l) source

setupGhc :: MonadInterpreter m => [ModuleName] -> m ()
setupGhc imps = do
    unsafeSetGhcOption "-v"
    unsafeSetGhcOption "-fobject-code"
    set [languageExtensions := [OverloadedStrings]]
    setImports $ ["Prelude"
                 ,"PgHaskell.Internal"
                 ,"PgHaskell.Internal.Elog"
                 ] ++ imps

test :: IO ()
test = do
    r <- compileFunction pinfo
    case r of
      Left e -> print e
      Right f -> pure (f []) >> print (typeRep $ f [v])
  where
    v = ArgValue False (Datum 0)
    pinfo = ProcInfo "putStrLn \"DERP\"" []

