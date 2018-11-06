{-# LANGUAGE NoImplicitPrelude #-}

module Haskoin.Mining
  ( mineOn
  , makeGenesis
  , isValidChain
  , isValidTxn
  , addBlock
  , accountBalances
  ) where

import Protolude
import Control.Comonad.Cofree ( Cofree((:<)) )

import qualified Crypto.Hash as Crypto
import           Crypto.Number.Serialize (os2ip)

import qualified Data.Binary as Bin

import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX

import qualified Data.Vector as V
import qualified Data.Map as M


import Haskoin.Types
-- needed for implicit importing of `instance Binary HaskoinHash`
import Haskoin.Serialization
import Haskoin.Util (average, safeDiv, timed)



type TransactionPool = IO [Transaction]

-- | Cfg
globalTransactionLimit = 1000
numBlocksToCalculateDifficulty = 5
genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
targetTime = 10
blockReward = 1000
difficultyFactor = 4



printDifficulty :: Integer -> IO ()
printDifficulty difficulty =
  print ( "Candidate difficulty: ", logDifficulty difficulty )


logDifficulty :: (Floating a) => Integer -> a
logDifficulty difficulty = logBase 10 $ fromIntegral difficulty


mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount chain =
  do
    txns <- take globalTransactionLimit . filter (isValidTxn' $ accountBalances chain) <$> pendingTransactions
    now  <- POSIX.getPOSIXTime
    -- The `nonce` starts at 0, increments with each attempt.
    timed $ do
      print ("desiredDiff", logDifficulty desiredDiff)
      let (ch, diffs) = loop now (Block $ V.fromList txns) 0 [nextDiff]
      print ("candidate difficulties", logDifficulty <$> reverse diffs)
      return ch
      
  where
    desiredDiff = desiredDifficulty chain
    nextDiff = getDifficulty chain * difficultyFactor
    validChain bc = getDifficulty bc < desiredDiff

    loop :: POSIX.POSIXTime -> Block -> Integer -> [Integer] -> (Blockchain, [Integer])
    loop time block nonce diffs@(bestDiff:_) =
      if validChain chain'
        then
          (chain', newDiffs)
        else
          loop time block (nonce+1) newDiffs
      where
        chain' = addBlock block header chain
        header = BlockHeader
          { _miner = minerAccount
          , _parentHash = Crypto.hashlazy $ Bin.encode chain
          , _nonce = nonce  -- << Incremented with each attempt.
          , _minedAt = time
          }
        newDiffs = if newDiff < bestDiff then newDiff:diffs else diffs
        newDiff = getDifficulty chain'


-- `accounts` is a map from an account (user id) to ???
-- We want to see whether a particular txn (id) 
isValidTxn' :: M.Map Account Integer -> Transaction -> Bool
isValidTxn' accounts txn =
  case M.lookup (_from txn) accounts of
    Nothing ->
      False
    Just balance ->
      balance >= _amount txn



-- Perform hash on the entire blockchain (up to this point),
-- then convert hash to an Integer.
-- SHA1 produces a 20-byte message digest (often as 40-digit hex number).
--
-- os2ip :: ByteArrayAccess ba => ba -> Integer
-- Converts a byte string (os) into a positive integer (ip).
-- The lower the number, the greater the difficulty it adheres to.
getDifficulty :: Blockchain -> Integer
getDifficulty bc = os2ip (Crypto.hashlazy $ Bin.encode bc :: HaskoinHash)


-- BEWARE: O(n * k), where k = numBlocksToCalculateDifficulty
desiredDifficulty :: Blockchain -> Integer
desiredDifficulty x = round $ loop x
  where
    loop   (_ :< Genesis)   = genesisBlockDifficulty
    loop x@(_ :< Node _ xs) = oldDifficulty / adjustmentFactor
      where
        oldDifficulty = loop xs
        adjustmentFactor = min 4.0 $ targetTime `safeDiv` blockTimeAverage x


blockTimeAverage :: Blockchain -> Clock.NominalDiffTime
blockTimeAverage bc =
  average $ zipWith (-) times (fromMaybe [] $ tailMay times)
  where
    times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc

chains :: Blockchain -> [Blockchain]
chains x@(_ :< Genesis)     = [x]
chains x@(_ :< Node _ next) = x : chains next

headers :: Blockchain -> [BlockHeader]
headers (_ :< Genesis)     = []
headers (_ :< Node x next) = x : headers next

makeGenesis :: IO Blockchain
makeGenesis = return $ Block (V.fromList []) :< Genesis

-- Create a map: from user (account) to that user's balance.
accountBalances :: Blockchain -> M.Map Account Integer
accountBalances bc = M.fromListWith (+) $ debits ++ credits ++ minings
  where
    txns = toList $ mconcat $ toList bc
    debits =  [ (_from x, negate $ _amount x) | x <- txns ]
    credits = [ (_from x,          _amount x) | x <- txns ]
    minings = [ (_miner h,       blockReward) | h <- headers bc ]


-- | Logic?
isValidChain :: Blockchain -> Bool
isValidChain _ = True

-- | Logic?
isValidTxn :: Blockchain -> Transaction -> Bool
isValidTxn _ _ = True


addBlock :: Block -> BlockHeader -> Blockchain -> Blockchain
addBlock block header chain = block :< Node header chain

----

-- testMining :: IO Blockchain
-- testMining = do
--   let txnPool = return []
--   chain <- makeGenesis
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   chain <- mineOn txnPool 0 chain
--   mineOn txnPool 0 chain
