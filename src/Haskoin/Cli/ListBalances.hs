{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.ListBalances where

import           Protolude
import qualified Data.Binary as Bin
import qualified Data.Map as M

import           Haskoin.Mining (accountBalances)
import           Haskoin.Types (Blockchain)


-- Cfg
defaultChainFile = "main.chain"


main :: IO ()
main = do
  chain <- Bin.decodeFile =<< getFilename
  mapM_ print (M.toAscList $ accountBalances chain)


getFilename :: IO FilePath
getFilename = do
  args <- getArgs
  return $ case args of
    []   -> defaultChainFile
    [fn] -> fn
    _    -> panic "Usage: list-balances [filename]"
