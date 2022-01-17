{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      : Database.Persist.Generic
-- Copyright   : (c) 2020 David Johnson
-- License     : All Rights Reserved
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Generic facilities for dealing with Persistent classes.
module Database.Persist.GenericADT where

{-   ( -- * Classes
    GToPersistValue (..),
    GFromPersistValue (..),

    -- * Methods
    genericToPersistValue,
    genericFromPersistValue,
  ) -}

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Char (toLower, toUpper)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Database.Persist.Sql
import GHC.Generics (C1, D1, Generic (..), M1 (..), Meta (..), U1 (..), (:+:) (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Generic class for deriving 'PersistValue'
class GToPersistValue f where
  gToPersistValue :: f a -> PersistValue

instance GToPersistValue a => GToPersistValue (D1 f a) where
  gToPersistValue (M1 x) = gToPersistValue x

instance KnownSymbol name => GToPersistValue (C1 ('MetaCons name x y) U1) where
  gToPersistValue (M1 _) = PersistText (T.pack name)
    where
      name = symbolVal (Proxy @name)

instance (GToPersistValue l, GToPersistValue r) => GToPersistValue (l :+: r) where
  gToPersistValue (L1 x) = gToPersistValue x
  gToPersistValue (R1 x) = gToPersistValue x

-- | Generic class for parsing 'PersistValue'
class GFromPersistValue f where
  gFromPersistValue :: PersistValue -> Either String (f a)

instance GFromPersistValue a => GFromPersistValue (D1 f a) where
  gFromPersistValue x = M1 <$> gFromPersistValue x

instance KnownSymbol name => GFromPersistValue (C1 ('MetaCons name x y) U1) where
  gFromPersistValue (PersistText v) =
    if T.unpack v == name
      then pure (M1 U1)
      else Left $ "Parse error: " <> name
    where
      name = symbolVal (Proxy @name)
  gFromPersistValue _ = Left $ "Invalid Type: " <> name
    where
      name = symbolVal (Proxy @name)

instance (GFromPersistValue l, GFromPersistValue r) => GFromPersistValue (l :+: r) where
  gFromPersistValue x = l <|> r
    where
      l = L1 <$> gFromPersistValue x
      r = R1 <$> gFromPersistValue x

-- | Newtype for deriving via extension, preserves constructor names as is.
newtype GenericallyViaADT a = GenericallyViaADT a
  deriving (Generic)

instance (Generic a, GToPersistValue (Rep a), GFromPersistValue (Rep a)) => PersistField (GenericallyViaADT a) where
  toPersistValue (GenericallyViaADT x) = gToPersistValue . from $ x
  fromPersistValue v = first T.pack (GenericallyViaADT . to <$> gFromPersistValue v)

-- | Newtype for deriving via extension, converts constructor names to lowercase.
newtype GenericallyLowercaseViaADT a = GenericallyLowercaseViaADT a
  deriving (Generic)

instance (Generic a, GToPersistValue (Rep a), GFromPersistValue (Rep a)) => PersistField (GenericallyLowercaseViaADT a) where
  toPersistValue (GenericallyLowercaseViaADT x) = lowerFirst . gToPersistValue . from $ x
    where
      lowerFirst val@(PersistText txt) = case T.uncons txt of
        Just (ch, chs) -> PersistText $ T.cons (toLower ch) chs
        Nothing -> val
      lowerFirst val = val

  fromPersistValue v = first T.pack (GenericallyLowercaseViaADT . to <$> (gFromPersistValue . upperFirst $ v))
    where
      upperFirst val@(PersistText txt) = case T.uncons txt of
        Just (ch, chs) -> PersistText $ T.cons (toUpper ch) chs
        Nothing -> val
      upperFirst val = val
