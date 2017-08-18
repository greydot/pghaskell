{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Foreign where

import PgHaskell.Elog
import PgHaskell.Compiler (compileFunction)
import PgHaskell.CTypes

import Control.Monad (void)
import Data.Monoid ((<>))
import Foreign.C.Types
import Foreign.Ptr
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text

foreign import ccall "wrapper"
  wrap :: IO () -> IO (FunPtr (IO ()))

foreign export ccall hsCompileFunction :: Ptr CChar -> CSize -> IO (FunPtr (IO ()))

hsCompileFunction :: Ptr CChar -> CSize -> IO (FunPtr (IO ()))
hsCompileFunction ptr sz = do
    txt <- Text.peekCStringLen (ptr, fromIntegral sz)
    elog ElogDebug2 ("Compiling code:\n" <> txt)
    res <- compileFunction txt
    case res of
      Left err -> nullFunPtr <$ elog ElogWarning ("Failed to compile function: " <> Text.pack (show err))
      Right f -> wrap $ void (f [])
