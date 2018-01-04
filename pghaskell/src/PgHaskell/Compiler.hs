{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Compiler where

import PgHaskell.Compiler.Context
import PgHaskell.Types
import PgHaskell.Internal

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

compileFunction :: ProcInfo -> IO (Either InterpreterError Callable)
compileFunction pinfo = runInterpreter $ do
    setupGhc (procContext pgproc)
    interpret (procCode pgproc) (as :: Callable)
  where
    pgproc = processSource pinfo

validateFunction :: ProcInfo -> IO (Either InterpreterError Bool)
validateFunction pinfo = fmap (const True) <$> compileFunction pinfo

processSource :: ProcInfo -> PgProc
processSource pinfo = PgProc { procCode = Text.unpack body
                             , procContext = ctx
                             }
  where
    (ctx, source) = splitContext (procText pinfo)
    args = zip (procArgs pinfo) [0..]
    prefix = Text.unlines $ ["\\values -> do"] ++ ["  " <> argToCode arg n | (arg,n) <- args]
    body = Text.unlines . (prefix:) $ map (\l -> "  " <> l) (Text.lines source)

argToCode :: ProcArg -> Word -> Text
argToCode arg n = mconcat [ argName arg, " <- fromDatum $ argDatum (values !! "
                          , Text.pack $ show n, ")"]

setupGhc :: MonadInterpreter m => ProcContext -> m ()
setupGhc ctx = do
    unsafeSetGhcOption "-v"
--    unsafeSetGhcOption "-fobject-code"
    set [languageExtensions := procExtensions ctx]
    setImportsQ $ [("Prelude", Nothing)
                  ,("PgHaskell.Internal",Nothing)
                  ] ++ procImports ctx

test :: IO ()
test = do
    r <- compileFunction pinfo
    case r of
      Left e -> print e
      Right f -> pure (f [v]) >> print (typeRep $ f [v])
  where
    v = ArgValue False (Datum 0)
    pinfo = ProcInfo code [ProcArg "i" "int"]
    code = Text.unlines ["{-# LANGUAGE OverloadedStrings #-}"
                        ,"import Control.Monad.IO.Class"
                        ,"import Data.Int"
                        ,"import qualified Data.Text as Text"
                        ,"liftIO $ print (i :: Int32)"
                        ,"pure (Datum 0)"
                        ]

