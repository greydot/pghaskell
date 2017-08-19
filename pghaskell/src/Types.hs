module PgHaskell.Types where

import Language.Haskell.Interpreter

data PgProc = PgProc { procCode :: String
                     , procImports :: [ModuleName]
                     } deriving (Show)
