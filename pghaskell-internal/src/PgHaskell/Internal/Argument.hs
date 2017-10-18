module PgHaskell.Internal.Argument ( getArgument
                                   ) where

import PgHaskell.Internal.Postgre
import PgHaskell.Internal.Types

-- |Get argument value.
--  Note, this function is unsafe, because there is
--  no typechecking involved.
getArgument :: Postgre t => ArgValue -> PG (Maybe t)
getArgument av | argIsNull av = pure Nothing
               | otherwise = pure <$> fromDatum (argDatum av)
