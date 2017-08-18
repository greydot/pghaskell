{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module PgHaskell.Internal.Elog ( ElogPriority(..)
                               , elog
                               ) where

import Control.Monad.IO.Class
import qualified Language.C.Inline as C
import Data.Text (Text)
import Data.Text.Foreign (withCStringLen)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)

data ElogPriority = ElogDebug5
                  | ElogDebug4
                  | ElogDebug3
                  | ElogDebug2
                  | ElogDebug1
                  | ElogLog
                  | ElogServerOnly
                  | ElogInfo
                  | ElogNotice
                  | ElogWarning
                  | ElogError
                  | ElogFatal
                  | ElogPanic

C.include "<setjmp.h>"
C.include "<server/postgres.h>"
C.include "<server/utils/elog.h>"

elogToInt :: Integral a => ElogPriority -> a
elogToInt ElogDebug5      = fromIntegral [C.pure| int{ DEBUG5 } |]
elogToInt ElogDebug4      = fromIntegral [C.pure| int{ DEBUG4 } |]
elogToInt ElogDebug3      = fromIntegral [C.pure| int{ DEBUG3 } |]
elogToInt ElogDebug2      = fromIntegral [C.pure| int{ DEBUG2 } |]
elogToInt ElogDebug1      = fromIntegral [C.pure| int{ DEBUG1 } |]
elogToInt ElogLog         = fromIntegral [C.pure| int{ LOG } |]
elogToInt ElogServerOnly  = fromIntegral [C.pure| int{ LOG_SERVER_ONLY } |]
elogToInt ElogInfo        = fromIntegral [C.pure| int{ INFO } |]
elogToInt ElogNotice      = fromIntegral [C.pure| int{ NOTICE } |]
elogToInt ElogWarning     = fromIntegral [C.pure| int{ WARNING } |]
elogToInt ElogError       = fromIntegral [C.pure| int{ ERROR } |]
elogToInt ElogFatal       = fromIntegral [C.pure| int{ FATAL } |]
elogToInt ElogPanic       = fromIntegral [C.pure| int{ PANIC } |]

-- ^ Wrapper around PostgreSQL's elog().
-- Note that pgsqls elog is a macro, which is why we cannot wrap it into
-- an FFI call.
elog :: MonadIO m => ElogPriority -> Text -> m ()
elog p t = liftIO $ withCStringLen t $
             \(ptr,l) -> withCString ("%" ++ show l ++ "s") $
               \fstr -> -- ptr points to a non-terminated string,
                        -- so we have to pass a format string for it.
                        [C.block| void {
                                        elog($(int cp), $(const char* fstr), $(const char* ptr));
                                        return;
                                       } |]
  where
    cp = elogToInt p
