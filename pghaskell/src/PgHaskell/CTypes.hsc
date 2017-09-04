{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PgHaskell.CTypes where

import Data.Word
import Foreign.Storable

#include "pghaskell.h"

-- Wrapper for PostgreSQL Oid type
newtype Oid = Oid #{type Oid}
  deriving (Show,Eq,Storable)

data ProcKey = ProcKey { procOid :: {-# UNPACK #-} !Oid
                       , procIsTrigger :: {-# UNPACK #-} !Bool
                       , procUserId :: {-# UNPACK #-} !Oid
                       }

instance Storable ProcKey where
  sizeOf _ = #{size pghsProcKey}
  alignment _ = alignment (undefined :: Oid)

  peek p = ProcKey <$> #{peek pghsProcKey, procOid} p
                   <*> #{peek pghsProcKey, isTrigger} p
                   <*> #{peek pghsProcKey, userId} p

  poke p k = do
    #{poke pghsProcKey, procOid} p $ procOid k
    #{poke pghsProcKey, isTrigger} p $ procIsTrigger k
    #{poke pghsProcKey, userId} p $ procUserId k
