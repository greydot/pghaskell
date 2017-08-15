module PgHaskell.Internal ( module Types
                          ) where

import PgHaskell.Types as Types

import Data.Map (Map)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Typeable (Typeable)

data PgCode = PgCode { pgcName :: Text
                     }
-- Compiled function
data PgFunction = PgFunction { pgfName     :: Text
                             , pgfNumArgs  :: Word
                             , pgfArgTypes :: [PgTypeName]
                             , pgfRetType  :: PgTypeName
                             , pgfFunc     :: [PgValue] -> PG PgValue
                             }

data PgCompileError = PgCompileError
