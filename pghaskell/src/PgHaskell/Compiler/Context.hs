module PgHaskell.Compiler.Context ( deduceContext
                                  ) where

import PgHaskell.Types

import Data.Foldable (foldl')
import qualified Data.Text as Text
import Language.Haskell.Exts

deduceContext :: ProcCode -> ProcContext
deduceContext code = foldl' go mempty ls
    where
        ls = Text.lines code
        go ctx l = fromParseRes ctx (checkImport ctx l)

        checkImport ctx l = do
            d <- parseImportDecl (Text.unpack l)
            let n = moduleName  $  importModule d
                a = moduleName <$> importAs d
            pure $ ctx { procImports = (n,a):procImports ctx }

moduleName :: ModuleName l -> String
moduleName (ModuleName _ n) = n

(<||>) :: ParseResult a -> ParseResult a -> ParseResult a
ParseOk a <||> _ = ParseOk a
ParseFailed _ _ <||> r = r

fromParseRes :: a -> ParseResult a -> a
fromParseRes _ (ParseOk x) = x
fromParseRes d _ = d
