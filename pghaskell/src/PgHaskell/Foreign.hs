{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Foreign where

import PgHaskell.Compiler (compileFunction, validateFunction)
import PgHaskell.CTypes
import PgHaskell.Internal
import PgHaskell.Internal.Elog

import Control.Monad (void)
import Data.Monoid ((<>))
import Foreign.C.Types
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text

type Proc = Ptr ArgValue -> CSize -> IO ()

foreign import ccall "wrapper"
  wrap :: (Proc) -> IO (FunPtr Proc)

foreign export ccall hsCompileFunction :: Ptr ProcInfo -> IO (FunPtr Proc)

hsCompileFunction :: Ptr ProcInfo -> IO (FunPtr Proc)
hsCompileFunction pinfo = do
    info <- peek pinfo
    let txt = procText info
    elog ElogDebug2 ("Compiling code:\n" <> txt)
    res <- compileFunction info
    case res of
      Left err -> nullFunPtr <$ elog ElogWarning ("Failed to compile function: " <> Text.pack (show err))
      Right f -> wrap $ \p s -> void (runPG . f =<< peekArray (fromIntegral s) p)

foreign export ccall hsValidateFunction :: Ptr CChar -> CSize -> IO CInt

hsValidateFunction :: Ptr CChar -> CSize -> IO CInt
hsValidateFunction ptr sz = do
    txt <- Text.peekCStringLen (ptr, fromIntegral sz)
    elog ElogDebug2 ("Checking code:\n" <> txt)
    res <- validateFunction txt
    case res of
      Left err -> 0 <$ elog ElogWarning ("Failed to validate function: " <> Text.pack (show err))
      Right b -> pure $ c b
  where
    c = fromIntegral . fromEnum
