{-# LANGUAGE NoImplicitPrelude #-}

module Haskoin.Cli.Server where

import Protolude
import Haskoin.Network


main :: IO ()
main = runSeedServer
