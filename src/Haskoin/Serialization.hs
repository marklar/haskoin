{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A whole module of typeclass instance declarations, mostly automatic (Generic).
module Haskoin.Serialization
  ( -- no explicit exports for typeclass instances
  ) where

import           Control.Comonad.Cofree ( Cofree(..) )
import qualified Crypto.Hash as Crypto
import           Data.Binary (Binary)
import qualified Data.Binary as Bin -- (get, put)
import           Data.Binary.Get (Get)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import           Data.Time.Clock.POSIX (POSIXTime)

-- Needed for importing typeclass instance. (There's no (good?) way to do it explicitly.)
import qualified Data.Vector.Binary as BinV

-- | Use same code to manipulate diff data types.
--   Generics allow us to describe a transformation that works in terms not of concrete types,
--   but general combinators that describe the shape and meta info about a data type.
--   This way, we can declare how to perform a computation on almost any data type!
--
--   Enabled by typeclasses (ad-hoc poly).
--   The Generic type class gives us a way to describe a data type in terms of a set of combinators.
--   Instances of Generic can be processed by fns that work w/ a representation of the data type, not the data type itself.
--   These fns are (by definition) polymorphic. They're also usually hidden behind a typeclass interface,
--   so the whole thing usually takes the form of automatic deriving of a type class instance.


import           GHC.Generics

import Haskoin.Types ( Account(..), Block, BlockF(..), BlockHeader(..)
                     , HaskoinRequest, HaskoinResponse, HaskoinHash
                     , MerkleF(..)
                     , SeedServer, Supernode, Transaction(..) )


-------------------
-- Binary

instance (Binary (f (Cofree f a)), Binary a) => Binary (Cofree f a) where
instance (Binary a) => Binary (MerkleF a) where
instance Binary BlockHeader where
instance Binary Transaction where

instance Binary POSIXTime where
  get = fromInteger <$> (Bin.get :: Get Integer)
  put x = Bin.put (round x :: Integer)


instance Binary SeedServer where
instance Binary Supernode where
instance Binary HaskoinRequest where
instance Binary HaskoinResponse where
deriving instance Binary Account
deriving instance Binary Block

instance Binary HaskoinHash where
  get = do
    mDigest <- Crypto.digestFromByteString <$> (Bin.get :: Get BS.ByteString)
    case mDigest of
      Nothing ->
        fail "Not a valid digest"
      Just digest ->
        return digest
  put digest =
    Bin.put (BA.convert digest :: BS.ByteString)


-------------------
-- Generic

deriving instance Generic (Cofree f a)
deriving instance Generic (MerkleF a)
deriving instance Generic BlockHeader
deriving instance Generic Transaction
