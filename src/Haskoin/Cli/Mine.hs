{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Cli.Mine
  ( main
  ) where

import           Prelude (read, String)
import           Protolude
import qualified Data.Binary as Bin
import qualified System.Directory as Dir

import qualified Haskoin.Mining as Mining
import qualified Haskoin.Types as Types


-- | Cfg
defaultChainFile = "main.chain"
defaultAccount = "10"


main :: IO ()
main =
  do
    (filename, accountStr) <- getFileAndAccount <$> getArgs
    forever $ do
      {-
      chain <- unmarshal filename Mining.makeGenesis :: IO Types.Blockchain
      chain' <- Mining.mineOn txnPool account chain
      safeMarshal filename chain'
      -}
      unmarshal filename Mining.makeGenesis
        >>= Mining.mineOn txnPool (Types.Account $ read accountStr)
        >>= safeMarshal filename
      putStrLn $ "Block mined and saved to file '" ++ filename ++ "'!"
  where
    txnPool = return []


-- | Cli args.
getFileAndAccount :: [String] -> (FilePath, String)
getFileAndAccount args =
  case args of
    [] -> (defaultChainFile, defaultAccount)
    [fn] -> (fn, defaultAccount)
    [fn, acct] -> (fn, acct)
    _ -> panic "Usage: mine [filename] [account]"



-- | Unmarshal data (:: a) from file.
--   If no such file, create minimal data, marshall it to file.
--   Return data.
unmarshal :: Bin.Binary a => FilePath -> IO a -> IO a
unmarshal filename initialize = do
  exists <- Dir.doesFileExist filename
  if exists
    then
      Bin.decodeFile filename
    else do
      x <- initialize
      safeMarshal filename x
      -- Bin.encodeFile filename x
      return x


-- | Atomic marshalling.
--   First, marshal chain to swapfile.
--   Then cp swapfile over regular.
safeMarshal :: Bin.Binary a => FilePath -> a -> IO ()
safeMarshal filename chain =
  do
    Bin.encodeFile swapFile chain
    Dir.copyFile swapFile filename
  where
    swapFile = filename ++ ".tmp"
