{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module PgHaskell.Internal.Postgre (Postgre(..)) where

import PgHaskell.Internal.InlineC
import PgHaskell.Internal.Types

import Control.Monad.IO.Class
import Data.Int
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import qualified Language.C.Inline as C

-- | Class for converting PostgreSQL values
--   to and from Datum.
class Postgre t where
  postgreTypes :: Proxy t -> [TypeName]
  toDatum      :: t -> PG Datum
  fromDatum    :: Datum -> PG t

C.context (C.baseCtx <> postgreCtx)
C.include "<server/c.h>"
C.include "<server/postgres.h>"
C.include "<server/fmgr.h>"

instance Postgre Int64 where
  postgreTypes _ = ["int8"]
  toDatum i = liftIO [C.exp| Datum { Int64GetDatum($(int64 i)) } |]
  fromDatum d = liftIO [C.exp| int64 { DatumGetInt64($(Datum d)) } |]

instance Postgre Int32 where
  postgreTypes _ = ["int4", "int"]
  toDatum i = liftIO [C.exp| Datum { Int32GetDatum($(int32 i)) } |]
  fromDatum d = liftIO [C.exp| int32 { DatumGetInt32($(Datum d)) } |]

instance Postgre Int16 where
  postgreTypes _ = ["int2"]
  toDatum i = liftIO [C.exp| Datum { Int16GetDatum($(int16 i)) } |]
  fromDatum d = liftIO [C.exp| int16 { DatumGetInt16($(Datum d)) } |]
