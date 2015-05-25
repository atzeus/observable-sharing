{-# LANGUAGE GADTs #-}

module Data.Ref.Map (
    Map
  , empty
  , singleton
  , null
  , size
  , member
  , lookup
  , insert
  , delete
  , adjust
  , union
  , difference
  , intersection
  , map
  , filter
  ) where

import Data.Ref
import Data.Unique
import Unsafe.Coerce -- lets use all the unsafe operations!

import qualified Data.HashMap.Lazy as H

import Prelude hiding (null, lookup, map, filter)

--------------------------------------------------------------------------------
-- * Reference indexed maps
--------------------------------------------------------------------------------

-- | HideType, for hiding types!
data HideType f
  where
    Hide :: f a -> HideType f

-- | A reference indexed map.
--
-- useful for associating info with a reference.
data Map f = Map (H.HashMap Int (HideType f))

--------------------------------------------------------------------------------
-- ** Construction

-- | Construct an empty map.
empty :: Map f
empty = Map H.empty

-- | Construct a map with a single element.
singleton :: Ref a -> f a -> Map f
singleton (Ref u _) v = Map $ H.singleton (hashUnique u) (Hide v)

--------------------------------------------------------------------------------
-- ** Basic interface

-- | Returns 'True' if the map is empty, 'False' otherwise.
null :: Map f -> Bool
null (Map m) = H.null m

-- | Returns the number of elements stored in this map.
size :: Map f -> Int
size (Map m) = H.size m

-- | Returns 'True' if the reference is present in the map, 'False' otherwise.
member :: Ref a -> Map f -> Bool
member (Ref u _) (Map m) = H.member (hashUnique u) m

-- | Returns the value associated with the reference, or 'Nothing' if the reference
-- has no value associated to it.
lookup :: Ref a -> Map f -> Maybe (f a)
lookup (Ref u _) (Map m) = fmap unsafeCoerce $ H.lookup (hashUnique u) m

-- | Associates a reference with the specified value. If the map already contains
-- a mapping for the reference, the old value is replaced.
insert :: Ref a -> f a -> Map f -> Map f
insert (Ref u _) v (Map m) = Map $ H.insert (hashUnique u) (Hide v) m

-- | Removes the associated value of a reference, if any is present in the map.
delete :: Ref a -> Map f -> Map f
delete (Ref u _) (Map m) = Map $ H.delete (hashUnique u) m

-- | Updates the associated value of a reference, if any is present in the map.
adjust :: (f a -> f b) -> Ref a -> Map f -> Map f
adjust f (Ref u _) (Map m) = Map $ H.adjust (\(Hide v) -> Hide $ f $ unsafeCoerce v) (hashUnique u) m

--------------------------------------------------------------------------------
-- ** Combine

-- | Union of two maps (left biased).
union :: Map f -> Map f -> Map f
union (Map m) (Map n) = Map $ H.union m n

-- | Difference of two maps.
difference :: Map f -> Map f -> Map f
difference (Map m) (Map n) = Map $ H.difference m n

-- | Intersectino of two maps.
intersection :: Map f -> Map f -> Map f
intersection (Map m) (Map n) = Map $ H.intersection m n

--------------------------------------------------------------------------------
-- ** Transformations

-- | Transforms a map by applying the given function to each value.
map :: (f a -> f b) -> Map f -> Map f
map f (Map m) = Map $ H.map (\(Hide v) -> Hide $ f $ unsafeCoerce v) m

-- | Filter this map by retaining only elements which values satisfy a predicate.
filter :: (f a -> Bool) -> Map f -> Map f
filter f (Map m) = Map $ H.filter (\(Hide v) -> f $ unsafeCoerce v) m

--------------------------------------------------------------------------------
