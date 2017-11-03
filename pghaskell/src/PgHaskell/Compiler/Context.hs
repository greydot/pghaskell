module PgHaskell.Compiler.Context ( deduceContext
                                  ) where

import PgHaskell.Types

import Control.Monad ((<=<))
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Language.Haskell.Exts hiding (Extension)
import Language.Haskell.Interpreter.Extension (Extension(..))
import Text.Read (readMaybe)

deduceContext :: ProcCode -> ProcContext
deduceContext code = foldl' go mempty ls
    where
        ls = Text.lines code
        go ctx l = fromParseRes ctx (checkImport ctx l <||> checkPragma ctx l)

        checkImport ctx l = do
            d <- parseImportDecl (Text.unpack l)
            let n = moduleName  $  importModule d
                a = moduleName <$> importAs d
            pure $ ctx <> mempty { procImports = [(n,a)] }
        checkPragma ctx l = do
            pragmas <- getTopPragmas (Text.unpack l)
            let exts = languagePragmas =<< pragmas
            pure $ ctx <> mempty { procExtensions = exts }

moduleName :: ModuleName l -> String
moduleName (ModuleName _ n) = n

languagePragmas :: ModulePragma l -> [Extension]
languagePragmas (LanguagePragma _ names) = mapMaybe (readMaybe <=< fromName) names
  where
    fromName (Ident _ s) = Just s
    fromName _ = Nothing
languagePragmas _ = []

(<||>) :: ParseResult a -> ParseResult a -> ParseResult a
ParseOk a <||> _ = ParseOk a
ParseFailed _ _ <||> r = r

fromParseRes :: a -> ParseResult a -> a
fromParseRes _ (ParseOk x) = x
fromParseRes d _ = d
