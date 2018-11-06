{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Cli.New
  ( main
  ) where

import Protolude
import Prelude (String)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BSL

import qualified Haskoin.Mining as Mining


-- | Cfg
defaultChainFile = "main.chain"


main :: IO ()
main = do
  filename <- getFilename
  chain <- Mining.makeGenesis
  BSL.writeFile filename $ Bin.encode chain
  putStrLn $ "Wrote file: " ++ filename


getFilename :: IO FilePath
getFilename = do
  args <- getArgs
  return $ case args of
    []  -> defaultChainFile
    [x] -> x
    _   -> panic "Usage: new [filename]"
