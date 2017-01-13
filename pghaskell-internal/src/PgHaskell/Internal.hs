{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
module PgHaskell.Internal where

import Data.Map (Map)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Typeable (Typeable)

data PgContext = PgContext { pgFunctions :: Map Text PgFunction }

newtype PG a = MkPG { runPG :: PgContext -> IO a }

type PGTypeName = Text

class Typeable t => HasPgType t where
  type PgType t

  pgTypeName :: Proxy t -> PGTypeName
  toPgType :: t -> PgType t
  fromPgType :: PgType t -> t

data HsValue where
  MkHsValue :: HasPgType t => t -> HsValue

data PgCode = PgCode { pgcName :: Text
                     }
-- Compiled function
data PgFunction = PgFunction { pgfName     :: Text
                             , pgfNumArgs  :: Word
                             , pgfArgTypes :: [PGTypeName]
                             , pgfRetType  :: PGTypeName
                             , pgfFunc     :: [HsValue] -> PG HsValue
                             }

data PgCompileError = PgCompileError
