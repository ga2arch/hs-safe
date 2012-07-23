{-# LANGUAGE DeriveDataTypeable #-}

module Types 
	( Action(..)
	, Safe	
	) where

import Data.Typeable
import System.Console.CmdArgs
import qualified Data.Map as M

data Action = Get { key :: String}
            | Set { key :: String
                  , value :: String
                  }
    deriving (Show, Data, Typeable)

type Safe = M.Map String String