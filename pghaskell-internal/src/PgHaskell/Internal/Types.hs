{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Internal.Types ( Datum(..)
                                , PG(..)
                                , TypeName
                                , FCallInfoData
                                , FCallInfo
                                ) where

import Control.Monad.IO.Class
import Data.Text (Text)

import Foreign.C.Types
import Foreign.Ptr

-- |PostgreSQL datum type.
-- See src/include/postgres.h for details.
newtype Datum = Datum CUIntPtr

newtype PG a = MkPG { runPG :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

type TypeName = Text

-- |Represents struct FunctionCallInfoData. Note that all work with this
-- type in Haskell code must be done with pointers, therefore it intentionally
-- has no constructors.
data FCallInfoData

-- |Haskell counterpart to PostgreSQL FunctionCallInfo.
type FCallInfo = Ptr FCallInfoData

