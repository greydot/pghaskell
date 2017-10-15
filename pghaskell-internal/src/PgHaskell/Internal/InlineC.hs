{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module PgHaskell.Internal.InlineC (postgreCtx) where

import PgHaskell.Internal.Types

import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Language.C.Inline
import Language.C.Inline.Context
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

include "<server/postgres.h>"

postgreCtx :: Context
postgreCtx = mempty { ctxTypesTable = postgreTypeTable }

postgreTypeTable :: Map C.TypeSpecifier TH.TypeQ
postgreTypeTable = Map.fromList [ (C.TypeName "Datum", [t| Datum |])
                                , (C.TypeName "Oid",   [t| Oid   |])
                                , (C.TypeName "int64", [t| Int64 |])
                                , (C.TypeName "int32", [t| Int32 |])
                                , (C.TypeName "int16", [t| Int16 |])
                                ]
