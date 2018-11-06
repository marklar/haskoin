{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Network
  (
  ) where

import Protolude
import Control.Comonad.Cofree ( Cofree((:<)) )
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.Binary as Bin
import qualified Data.ByteString as BSL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Network.Wai as Warp
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Simple as Simple
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.HTTP.Types.Method
import System.Directory

import Haskoin.Types
import Haskoin.Serialization
import Haskoin.Mining



-- | Cfg
chainFile = "main.chain"
supernodesFile = "supernodes.bin"
seedServer = "haskoin.michaelburge.us"
haskoinPort = 31337

seedNode = Supernode "Seed" seedServer

runSeedServer :: IO ()
runSeedServer = Warp.run haskoinPort =<< appBase <$> initSeedServer
  
-- runMiningServer :: Account -> IO ()
-- runMiningServer account =
--   do
--     state <- initMiner
--     loop <- do
--       minerPid <- forkIO $ do
--         chain <- readTVarIO $ _longestChain state
--         txns <- readTVarIO $ _txnPool state
--         (block :< Node header _) <- mineOn txns account chain
--         sendReq seedNode $ ReqNewBlock block header
--         return ()
--     appBase state
--   where
--     seconds = 1000000

appBase :: ServerState -> Warp.Application
appBase state req respond = do
  reqBs <- Warp.strictRequestBody req
  haskRes <- appropriateResponse state (Bin.decode reqBs)
  respond $ Warp.responseLBS status200 [] (Bin.encode haskRes)

initSeedServer :: IO ServerState
initSeedServer = do
  chain      <- loadOrCreate chainFile makeGenesis
  supernodes <- loadOrCreate supernodesFile (return [seedNode])
  ServerState <$> newTVarIO chain <*> newTVarIO S.empty <*> newTVarIO S.empty

initMiner :: IO ServerState
initMiner = do
  (ResEntireBlockchain chain)    <- sendReq seedNode ReqEntireBlockchain
  (ResListSupernodes supernodes) <- sendReq seedNode ReqListSupernodes
  (ResListTransactions txns)     <- sendReq seedNode ReqListTransactions
  ServerState <$> newTVarIO chain <*> newTVarIO (S.fromList supernodes) <*> newTVarIO (S.fromList txns)


-- FIXME: redundant w/ unmarshal function in Cli.Mine.
loadOrCreate :: Bin.Binary a => FilePath -> IO a -> IO a
loadOrCreate filename init = do
  exists <- doesFileExist filename
  if exists
    then Bin.decodeFile filename
    else do
      x <- init
      Bin.encodeFile filename x
      return x


sendReq :: Supernode -> HaskoinRequest -> IO HaskoinResponse
sendReq Supernode{..} haskReq =
  Bin.decode . Simple.getResponseBody <$> Simple.httpLBS request
  where request = defaultRequest
          { method = "POST"
          , host = encodeUtf8 $ T.pack _nodeHost
          , port = haskoinPort
          , requestBody = RequestBodyLBS (Bin.encode haskReq)
          }

  
appropriateResponse :: ServerState -> HaskoinRequest -> IO HaskoinResponse
appropriateResponse ServerState{..} = \case
  ReqEntireBlockchain ->
    ResEntireBlockchain <$> readTVarIO _longestChain

  ReqNewBlock block header ->
    atomically $ do
      always $ isValidChain <$> readTVar _longestChain
      modifyTVar' _longestChain (addBlock block header) `orElse` return ()
      return ResNewBlock

  ReqNewTransaction txn ->
    atomically $ do
      always $ isValidTxn <$> readTVar _longestChain <*> return txn
      modifyTVar' _txnPool (S.insert txn) `orElse` return ()
      return ResNewTransaction

  ReqRegisterSupernode supernode ->
    atomically $ do
      modifyTVar' _supernodes (S.insert supernode)
      return ResRegisterSupernode

  ReqListSupernodes ->
    ResListSupernodes . S.toList <$> readTVarIO _supernodes

  ReqListTransactions ->
    ResListTransactions . S.toList <$> readTVarIO _txnPool
