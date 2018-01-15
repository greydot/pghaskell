{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Internal.Types ( Datum(..)
                                , Oid(..)
                                , PG(..)
                                , TypeName
                                , ArgValue(..)
                                ) where

import Control.Monad.IO.Class
import Data.Text (Text)

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

-- |PostgreSQL datum type.
-- See src/include/postgres.h for details.
newtype Datum = Datum CUIntPtr
  deriving (Show,Eq,Storable)

-- Wrapper for PostgreSQL Oid type
newtype Oid = Oid CUInt
  deriving (Show,Eq,Storable)

newtype PG a = MkPG { runPG :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

type TypeName = Text

data ArgValue = ArgValue { argIsNull :: Bool
                         , argDatum :: Datum
                         } deriving (Show)

#include "pghaskell/types.h"

instance Storable ArgValue where
  sizeOf _ = #{size pghsArgValue}
  alignment _ = alignment (undefined :: Bool)

  peek p = ArgValue <$> #{peek pghsArgValue, isNull} p
                    <*> #{peek pghsArgValue, datum} p
  poke p v = do #{poke pghsArgValue, isNull} p $ argIsNull v
                #{poke pghsArgValue, datum} p $ argDatum v
