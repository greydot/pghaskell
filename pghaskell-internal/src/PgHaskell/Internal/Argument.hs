{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Internal.Argument ( getArgument
                                   , getNullArgument
                                   ) where

import PgHaskell.Internal.Elog
import PgHaskell.Internal.Postgre
import PgHaskell.Internal.Types

getArgument :: Postgre t => ArgValue -> PG t
getArgument av | argIsNull av = do
                   -- elog call produces an exception in postgresql
                   elog ElogError "trying to extract nulled argument where a non-null value was expected"
                   -- the following code is unreachable
                   pure undefined
               | otherwise = fromDatum (argDatum av)
-- |Get argument value.
--  Note, this function is unsafe, because there is
--  no typechecking involved.
getNullArgument :: Postgre t => ArgValue -> PG (Maybe t)
getNullArgument av | argIsNull av = pure Nothing
                   | otherwise = pure <$> fromDatum (argDatum av)

