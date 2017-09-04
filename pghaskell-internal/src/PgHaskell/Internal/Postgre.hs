{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module PgHaskell.Internal.Postgre where

import PgHaskell.Internal.InlineC
import PgHaskell.Internal.Types

import Control.Monad.IO.Class
import Data.Int
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import qualified Language.C.Inline as C

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
