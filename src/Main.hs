{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Directory
import System.Environment
import System.Console.CmdArgs

import Types
import Safe

import qualified Data.Map as M

safeLoc = "/Users/ga2arch/Progetti/haskell/safe.data"

get = Get { key = def &= typ "KEY" &= argPos 0 }
set = Set { key = def &= typ "KEY" &= argPos 0
          , value = def &= typ "VALUE" &= argPos 1
          }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun ms
    r <- authenticate safeLoc
    case r of
        Just (p, m) -> handleOpts opts p m
        Nothing     -> putStrLn "Wrong password"
  where
    ms = cmdArgsMode $ modes [get,set]

handleOpts :: Action -> String -> Safe -> IO ()
handleOpts (Get {..}) pass m = do
    print $ M.lookup key m
handleOpts (Set {..}) pass m = do
    let nm = M.insert key value m
    saveSafe pass safeLoc nm

authenticate :: FilePath -> IO (Maybe (String, Safe))
authenticate path = do
    b <- doesFileExist path
    if b
        then openSafe path
        else createSafe path