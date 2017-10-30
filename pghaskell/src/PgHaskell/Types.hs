module PgHaskell.Types ( module CTypes
                       , ProcContext(..)
                       , PgProc(..)
                       , Callable
                       ) where

import PgHaskell.CTypes as CTypes
import PgHaskell.Internal

import Data.List (nub)
import Data.Monoid
import Language.Haskell.Interpreter

data ProcContext = ProcContext { procExtensions :: [Extension]
                               , procImports :: [(ModuleName, Maybe String)]
                               } deriving (Show)

instance Monoid ProcContext where
  a `mappend` b = ProcContext { procExtensions = nub (procExtensions a <> procExtensions b)
                              , procImports = nub (procImports a <> procImports b)
                              }
  mempty = ProcContext [] []

data PgProc = PgProc { procCode :: String
                     , procContext :: ProcContext
                     } deriving (Show)

type Callable = [ArgValue] -> PG Datum
