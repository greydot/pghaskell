{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Compiler where

import PgHaskell.Compiler.Context
import PgHaskell.Types
import PgHaskell.Internal

import Data.List (filter)
import Data.Monoid
import qualified Data.Text as Text
import Data.Typeable

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

compileFunction :: ProcInfo -> IO (Either InterpreterError Callable)
compileFunction pinfo = runInterpreter $ do
    setupGhc (procContext pgproc)
    interpret (procCode pgproc) fallBackCode
  where
    pgproc = processSource pinfo
    fallBackCode _ = do
        elog ElogWarning "Fallback pghaskell procedure"
        pure (Datum 0)

validateFunction :: ProcInfo -> IO (Either InterpreterError Bool)
validateFunction pinfo = fmap (const True) <$> compileFunction pinfo

processSource :: ProcInfo -> PgProc
processSource pinfo = PgProc { procCode = Text.unpack body
                             , procContext = ctx
                             }
  where
    ctx = deduceContext (procText pinfo)
    isImport = Text.isPrefixOf "import "
    source = filter (not . isImport) (Text.lines $ procText pinfo)
    prefix = "\\values -> do"
    body = Text.unlines . (prefix:) $ map (\l -> "  " <> l) source

setupGhc :: MonadInterpreter m => ProcContext -> m ()
setupGhc ctx = do
    unsafeSetGhcOption "-v"
    unsafeSetGhcOption "-fobject-code"
    set [languageExtensions := [OverloadedStrings]]
    setImportsQ $ [("Prelude", Nothing)
                  ,("PgHaskell.Internal",Nothing)
                  ] ++ procImports ctx

test :: IO ()
test = do
    r <- compileFunction pinfo
    case r of
      Left e -> print e
      Right f -> pure (f []) >> print (typeRep $ f [v])
  where
    v = ArgValue False (Datum 0)
    pinfo = ProcInfo "putStrLn \"DERP\"" []

