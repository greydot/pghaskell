{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PgHaskell.CTypes ( ProcKey(..)
                        , ProcArg(..)
                        ) where

import PgHaskell.Internal
import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Foreign as Text
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr
import Foreign.Storable


#include "pghaskell.h"

data ProcKey = ProcKey { procOid :: {-# UNPACK #-} !Oid
                       , procIsTrigger :: !Bool
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

data ProcArg = ProcArg { argName :: Text
                       , argTypeName :: Text
                       } deriving (Show)

nameDataLen :: Int
nameDataLen = #{const NAMEDATALEN}

instance Storable ProcArg where
  sizeOf _ = #{size pghsArg}
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    n <- peekCString $ #{ptr pghsArg, argName} p
    t <- peekCString $ #{ptr pghsArg, typeName} p
    pure $ ProcArg n t
  poke p a = do
    fillBytes p 0 (sizeOf (undefined :: ProcArg))
    Text.withCStringLen (argName a) $ \(cs,l) ->
      let d = #{ptr pghsArg, argName} p
          n = if l >= nameDataLen then nameDataLen-1 else l
      in copyBytes d cs n
    Text.withCStringLen (argTypeName a) $ \(cs,l) ->
      let d = #{ptr pghsArg, typeName} p
          n = if l >= nameDataLen then nameDataLen-1 else l
      in copyBytes d cs n


peekCString :: CString -> IO Text
peekCString cs = do
    bs <- unsafePackCString cs
    return $! Text.decodeUtf8 bs
