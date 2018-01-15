{-# LANGUAGE OverloadedStrings #-}
module PgHaskell.Internal.Errors ( unreachable
                                 , wrongArgumentsNum
                                 ) where

import PgHaskell.Internal.Elog

import Control.Monad.IO.Class
import qualified Data.Text as Text

unreachable :: a
unreachable = error "Unreachable code"

wrongArgumentsNum :: MonadIO m => Int -> Int -> m a
wrongArgumentsNum n e = elog ElogError msg >> pure unreachable
  where
    msg = mconcat ["Wrong number of argument: expected ", Text.pack (show e)
                  ,", found ", Text.pack (show n)
                  ]
