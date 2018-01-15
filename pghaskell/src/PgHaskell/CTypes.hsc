{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PgHaskell.CTypes ( ProcKey(..)
                        , ProcArg(..)
                        , ProcCode
                        , ProcInfo(..)
                        ) where

import PgHaskell.Internal
import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Foreign as Text
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr
import Foreign.Storable


#include "pghaskell/types.h"

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
                       , argNullable :: Bool
                       } deriving (Show)

nameDataLen :: Int
nameDataLen = #{const NAMEDATALEN}

instance Storable ProcArg where
  sizeOf _ = #{size pghsArg}
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    n <- peekCString $ #{ptr pghsArg, argName} p
    t <- peekCString $ #{ptr pghsArg, typeName} p
    nullable <- #{peek pghsArg, nullable} p
    pure $ ProcArg n t nullable
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
    #{poke pghsArg, nullable} p (argNullable a)


type ProcCode = Text

data ProcInfo = ProcInfo { procText :: ProcCode
                         , procArgs :: [ProcArg]
                         }

instance Storable ProcInfo where
  sizeOf _ = #{size pghsProcInfo}
  alignment _ = alignment (undefined :: CSize)
  peek p = do csz :: CSize <- #{peek pghsProcInfo, codeSize} p
              cptr <- #{peek pghsProcInfo, code} p
              c <- Text.peekCStringLen (cptr,fromIntegral csz)
              asz :: CSize <- #{peek pghsProcInfo, argsNum} p
              aptr <- #{peek pghsProcInfo, args} p
              a <- peekArray (fromIntegral asz) aptr
              pure $ ProcInfo c a
  poke _ _= error "ProcInfo isn't supposed to be poked"

peekCString :: CString -> IO Text
peekCString cs = do
    bs <- unsafePackCString cs
    return $! Text.decodeUtf8 bs
