-- Can't make a derived instance of ‘Num Account’:
--   ‘Num’ is not a stock derivable class (Eq, Show, etc.)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Using Protolude instead.
{-# LANGUAGE NoImplicitPrelude          #-}

{-# LANGUAGE DeriveTraversable          #-}

-- deriving instance Generic Blockchain
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}


module Haskoin.Types
  ( module Haskoin.Types
  ) where

import Protolude
import qualified Crypto.Hash as Crypto
import           Control.Comonad.Cofree (Cofree(..))
import           Control.Concurrent.STM.TVar (TVar)
import qualified Data.Set as S
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Vector as V
import           Network


-- If newtype isn't a record, pattern-match to get value.
-- | Used to ID a miner / user.
newtype Account = Account Integer
  deriving (Eq, Show, Num, Ord)

-- | Transference of value.
data Transaction = Transaction
  { _from   :: Account
  , _to     :: Account
  , _amount :: Integer
  } deriving (Eq, Show, Ord)

-- | BlockF = Functor form of Block.
--   A Vector of something.
newtype BlockF a = Block (V.Vector a)
  deriving (Eq, Show, Foldable, Traversable, Functor, Monoid)

-- | Block: Vector of Transactions.
type Block = BlockF Transaction

type HaskoinHash = Crypto.Digest Crypto.SHA1

-- | The header of a Block indicates…
data BlockHeader = BlockHeader
  { -- | Who mined (gets credit for) this block.
    _miner       :: Account
    -- | The link tying this Block to its predecessor.
  , _parentHash  :: HaskoinHash
    -- | There is no difficulty, so miners aren’t proving they’ve done any work.
    -- The miner can increment the nonce field until finding a good hash.
    -- Why Integer (and not Int)? Might need to be big in order to yield a sufficiently-difficult hash.
  , _nonce       :: Integer
    -- | We want to adjust our difficulty so that all blocks take roughly the same time to mine.
  , _minedAt     :: POSIX.POSIXTime
  } deriving (Eq, Show)


-- | Adds a layer onto some other type.
--   Why do we choose this weird MerkleF type over the simpler one below?
--
--       newtype Block = Block (V.Vector Transaction)
--       data Blockchain = Genesis Block
--                       | Node Block BlockHeader Blockchain
--
--   To get instances for Functor, Traversable, and Foldable.
--   We can use them to work with our Merkle tree without having to write any code.
data MerkleF a = Genesis
               | Node BlockHeader a
  deriving (Eq, Show, Functor, Traversable, Foldable)

-- | Does two things:
--   1. recursively applies `MerkleF` to produce a type for all depths of Merkle trees, and
--   2. attaches an annotation of type `Block` to each node in the tree.
--   (When using Cofree, `anno :< xf` will construct one of these annotated values.)
--
--   It's be more useful to look at an “inverted” tree - each node knows its parent, not children.
--   If children, must change every node just to add a single new block at end.
--   So MerkleF produces a _chain_, not a tree.
type Blockchain = Cofree MerkleF Block

-- requires: -XFlexibleInstances, -XDeriveGeneric, -XStandaloneDeriving
deriving instance Generic Blockchain



--------------------------
-- For a p2p network.

data ServerState = ServerState
  { _longestChain :: TVar Blockchain
  , _supernodes   :: TVar (S.Set Supernode)
  , _txnPool      :: TVar (S.Set Transaction)
  }

newtype SeedServer = SeedServer Text
  deriving (Eq, Show, Generic)

-- | ??
data Supernode = Supernode
  { _nodeName :: Text
  , _nodeHost :: Network.HostName
  } deriving (Eq, Show, Generic, Ord)

data HaskoinRequest = ReqEntireBlockchain
                    | ReqNewBlock Block BlockHeader
                    | ReqNewTransaction Transaction
                    | ReqRegisterSupernode Supernode
                    | ReqListSupernodes
                    | ReqListTransactions
                    deriving (Eq, Show, Generic)

data HaskoinResponse = ResEntireBlockchain Blockchain
                     | ResNewBlock
                     | ResNewTransaction
                     | ResRegisterSupernode
                     | ResListSupernodes [Supernode]
                     | ResListTransactions [Transaction]
                     deriving (Eq, Show, Generic)
