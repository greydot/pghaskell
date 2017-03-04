{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module PgHaskell.Elog ( ElogPriority(..)
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

elogToCInt :: ElogPriority -> CInt
elogToCInt ElogDebug5      = fromIntegral [C.pure| int{ DEBUG5 } |]
elogToCInt ElogDebug4      = fromIntegral [C.pure| int{ DEBUG4 } |]
elogToCInt ElogDebug3      = fromIntegral [C.pure| int{ DEBUG3 } |]
elogToCInt ElogDebug2      = fromIntegral [C.pure| int{ DEBUG2 } |]
elogToCInt ElogDebug1      = fromIntegral [C.pure| int{ DEBUG1 } |]
elogToCInt ElogLog         = fromIntegral [C.pure| int{ LOG } |]
elogToCInt ElogServerOnly  = fromIntegral [C.pure| int{ LOG_SERVER_ONLY } |]
elogToCInt ElogInfo        = fromIntegral [C.pure| int{ INFO } |]
elogToCInt ElogNotice      = fromIntegral [C.pure| int{ NOTICE } |]
elogToCInt ElogWarning     = fromIntegral [C.pure| int{ WARNING } |]
elogToCInt ElogError       = fromIntegral [C.pure| int{ ERROR } |]
elogToCInt ElogFatal       = fromIntegral [C.pure| int{ FATAL } |]
elogToCInt ElogPanic       = fromIntegral [C.pure| int{ PANIC } |]

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
    cp = elogToCInt p
