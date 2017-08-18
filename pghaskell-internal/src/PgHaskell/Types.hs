{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module PgHaskell.Types ( PG
                       , PgValue(..)
                       , PgTypeName
                       ) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Typeable (Typeable)

import Foreign.C.Types (CChar, CLong)

newtype PG a = MkPG { runPG :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

data PgValue where
  -- Note, `t' should be constrained
  MkPgValue :: PgTypeable t => t -> PgValue

class Typeable t => PgTypeable t where
  type PgType t :: *

  pgTypeName :: Proxy t -> PgTypeName
  toPgType :: t -> PgType t
  fromPgType :: PgType t -> t

type PgTypeName = Text

-- ^ Haskell FFI doesn't have anything resembling CBool,
-- | but since sizeof(bool) == 1 we can be clever here.
instance PgTypeable Bool where
  type PgType Bool = CChar

  pgTypeName _ = "boolean"
  toPgType b = if b then 1 else 0
  fromPgType 0 = False
  fromPgType _ = True

instance PgTypeable Int where
  type PgType Int = CLong

  pgTypeName _ = "bigint"
  toPgType = fromIntegral
  fromPgType = fromIntegral
