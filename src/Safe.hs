module Safe 
    ( authenticate
    , openSafe
    , createSafe
    , saveSafe
    ) where

import Data.Aeson
import System.Directory

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Types
import Encription

authenticate :: FilePath -> IO (Maybe (String, Safe))
authenticate path = do
    b <- doesFileExist path
    if b
        then openSafe path
        else createSafe path

openSafe :: FilePath -> IO (Maybe (String, Safe))
openSafe path = do
    putStr "Password: "
    passwd <- getLine
    d <- decrypt passwd path
    let sf = decode (L.fromChunks [d])
    case sf of
        Just x  -> return $ Just (passwd, x)
        Nothing -> return Nothing

createSafe :: FilePath -> IO (Maybe (String, Safe))
createSafe path = do
    putStr "Safe creation, please insert password: "
    passwd <- getLine
    let m = M.empty :: Safe
    saveSafe passwd path m
    return $ Just (passwd, m)

saveSafe :: String -> FilePath -> Safe -> IO ()
saveSafe key path m = do
    (iv,cm) <- encrypt key path m
    C.writeFile path (C.append iv cm)


