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
    elog ElogDebug1 ("Compiling Haskell code:\n"<> Text.pack (procCode pgproc))
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
    prefix = createProcPrefix (procArgs pinfo) (procIsStrict pinfo)
    body = prefix <> Text.unlines (map shiftLine (Text.lines source))

createProcPrefix :: [ProcArg] -> Bool -> ProcCode
createProcPrefix args strict = Text.unlines (prefix:argEx)
  where
    prefix = Text.unlines ["\\values -> if length values /= " <> nargs
                          ,"  then wrongArgumentsNum (length values) " <> nargs
                          ,"  else do"
                                            ]
    nargs = Text.pack $ show $ length args
    argEx = map (shiftLine . uncurry (argToCode strict)) $ zip args [0..]

argToCode :: Bool -> ProcArg -> Word -> Text
argToCode s arg n = mconcat [ argName arg, " <- ", getF, " (values !! "
                            , Text.pack $ show n, ")"]
  where
    getF | not s || argNullable arg = "getNullArgument"
         | otherwise = "getArgument"

shiftLine :: Text -> Text
shiftLine = ("    " <>)

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
    pinfo = ProcInfo code [ProcArg "i" "int" True] False
    code = Text.unlines ["{-# LANGUAGE OverloadedStrings #-}"
                        ,"import Control.Monad.IO.Class"
                        ,"import Data.Int"
                        ,"import qualified Data.Text as Text"
                        ,"liftIO $ print (i :: Maybe Int32)"
                        ,"pure (Datum 0)"
                        ]

