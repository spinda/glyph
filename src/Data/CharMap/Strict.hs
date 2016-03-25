-- | Map from Char to arbitrary data; strict version.
--
-- Wrapper around "Data.IntMap.Strict".

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.CharMap.Strict (
    -- * Map type
    CharMap
  , Key

    -- * Operators
  , (!)
  , (\\)

    -- * Query
  , null
  , size
  , member
  , notMember
  , lookup
  , findWithDefault
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE

    -- * Combine
    -- ** Union
  , union
  , unionWith
  , unionWithKey
  , unions
  , unionsWith
    -- ** Difference
  , difference
  , differenceWith
  , differenceWithKey
    -- ** Intersection
  , intersection
  , intersectionWith
  , intersectionWithKey

    -- * Conversion
    -- ** Lists
  , toList
  , fromList
  ) where

import Prelude hiding (lookup, map, filter, foldr, foldl, null)
import qualified Prelude as P

import Control.DeepSeq

import Data.Bifunctor
import Data.Char
import Data.Data
import Data.Foldable hiding (null, toList)
import Data.Traversable
import Data.Typeable

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

import qualified GHC.Exts as G

--------------------------------------------------------------------------------
-- Map type --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Manually implement Show and Read instances
-- TODO: Factor out into separate "enum-map" package

newtype CharMap a = CharMap { unCharMap :: IntMap a }
                    deriving ( Functor
                             , Foldable
                             , Traversable
                             , Eq
                             , Data
                             , Typeable
                             , Ord
                             , Read
                             , Show
                             , Monoid
                             , NFData
                             )

type Key = Char

instance G.IsList (CharMap a) where
  type Item (CharMap a) = (Key, a)
  fromList = fromList
  toList = toList

--------------------------------------------------------------------------------
-- Operators -------------------------------------------------------------------
--------------------------------------------------------------------------------

(!) :: CharMap a -> Key -> a
(!) (CharMap m) = (M.!) m . ord

(\\) :: CharMap a -> CharMap b -> CharMap a
(\\) (CharMap x) (CharMap y) = CharMap $ (M.\\) x y

--------------------------------------------------------------------------------
-- Query -----------------------------------------------------------------------
--------------------------------------------------------------------------------

null :: CharMap a -> Bool
null = M.null . unCharMap

size :: CharMap a -> Int
size = M.size . unCharMap

member :: Key -> CharMap a -> Bool
member k = M.member (ord k) . unCharMap

notMember :: Key -> CharMap a -> Bool
notMember k = M.notMember (ord k) . unCharMap

lookup :: Key -> CharMap a -> Maybe a
lookup k = M.lookup (ord k) . unCharMap

findWithDefault :: a -> Key -> CharMap a -> a
findWithDefault x k = M.findWithDefault x (ord k) . unCharMap

lookupLT :: Key -> CharMap a -> Maybe (Key, a)
lookupLT k = fmap (first chr) . M.lookupLT (ord k) . unCharMap

lookupGT :: Key -> CharMap a -> Maybe (Key, a)
lookupGT k = fmap (first chr) . M.lookupGT (ord k) . unCharMap

lookupLE :: Key -> CharMap a -> Maybe (Key, a)
lookupLE k = fmap (first chr) . M.lookupLE (ord k) . unCharMap

lookupGE :: Key -> CharMap a -> Maybe (Key, a)
lookupGE k = fmap (first chr) . M.lookupGE (ord k) . unCharMap

--------------------------------------------------------------------------------
-- Construction ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- Union -----------------------------------------------------------------------

union :: CharMap a -> CharMap a -> CharMap a
union (CharMap x) (CharMap y) = CharMap $ M.union x y

unionWith :: (a -> a -> a) -> CharMap a -> CharMap a -> CharMap a
unionWith f (CharMap x) (CharMap y) = CharMap $ M.unionWith f x y

unionWithKey :: (Key -> a -> a -> a) -> CharMap a -> CharMap a -> CharMap a
unionWithKey f (CharMap x) (CharMap y) =
  CharMap $ M.unionWithKey (f . chr) x y

unions :: [CharMap a] -> CharMap a
unions xs = CharMap $ M.unions $ P.map unCharMap xs

unionsWith :: (a -> a -> a) -> [CharMap a] -> CharMap a
unionsWith f xs = CharMap $ M.unionsWith f $ P.map unCharMap xs

-- Difference ------------------------------------------------------------------

difference :: CharMap a -> CharMap a -> CharMap a
difference (CharMap x) (CharMap y) = CharMap $ M.difference x y

differenceWith :: (a -> a -> Maybe a) -> CharMap a -> CharMap a -> CharMap a
differenceWith f (CharMap x) (CharMap y) = CharMap $ M.differenceWith f x y

differenceWithKey :: (Key -> a -> a -> Maybe a)
                  -> CharMap a -> CharMap a
                  -> CharMap a
differenceWithKey f (CharMap x) (CharMap y) =
  CharMap $ M.differenceWithKey (f . chr) x y


-- Intersection ----------------------------------------------------------------

intersection :: CharMap a -> CharMap a -> CharMap a
intersection (CharMap x) (CharMap y) = CharMap $ M.intersection x y

intersectionWith :: (a -> a -> a) -> CharMap a -> CharMap a -> CharMap a
intersectionWith f (CharMap x) (CharMap y) = CharMap $ M.intersectionWith f x y

intersectionWithKey :: (Key -> a -> a -> a) -> CharMap a -> CharMap a -> CharMap a
intersectionWithKey f (CharMap x) (CharMap y) =
  CharMap $ M.intersectionWithKey (f . chr) x y

-- Universal Combination Function ----------------------------------------------

mergeWithKey :: (Key -> a -> b -> Maybe c)
             -> (CharMap a -> CharMap c)
             -> (CharMap b -> CharMap c)
             -> CharMap a -> CharMap b
             -> CharMap c 
mergeWithKey f g1 g2 (CharMap x) (CharMap y) =
  CharMap $ M.mergeWithKey (f . chr) g1' g2' x y
  where
    g1' = unCharMap . g1 . CharMap
    g2' = unCharMap . g2 . CharMap

--------------------------------------------------------------------------------
-- Traversal -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Map -------------------------------------------------------------------------

map :: (a -> b) -> CharMap a -> CharMap b 
map f = CharMap . M.map f . unCharMap

mapWithKey :: (Key -> a -> b) -> CharMap a -> CharMap b 
mapWithKey f = CharMap . M.mapWithKey (f . chr) . unCharMap

traverseWithKey :: Applicative t
                => (Key -> a -> t b)
                -> CharMap a
                -> t (CharMap b) 
traverseWithKey f = fmap CharMap . M.traverseWithKey (f . chr) . unCharMap

mapAccum :: (a -> b -> (a, c)) -> a -> CharMap b -> (a, CharMap c)
mapAccum f x = second CharMap . M.mapAccum f x . unCharMap

--------------------------------------------------------------------------------
-- Conversion ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Lists -----------------------------------------------------------------------

toList :: CharMap a -> [(Key, a)] 
toList = P.map (first chr) . M.toList . unCharMap

fromList :: [(Key, a)] -> CharMap a 
fromList = CharMap . M.fromList . P.map (first ord)

