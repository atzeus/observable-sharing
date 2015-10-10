{-# LANGUAGE GADTs, ScopedTypeVariables #-}

module Data.Ref.Map (
    Map
  , Name
  , empty
  , singleton
  , null
  , size
  , member
  , (!)
  , lookup
  , insert
  , delete
  , adjust
  , hmap
  , union
  , difference
  , intersection
    -- eehh...
  , HideType(..)
  , dump
  ) where

import Control.Applicative ((<$>))
import Data.Ref
import Data.List (find, deleteBy)
import Data.Function (on)

import Unsafe.Coerce
import System.Mem.StableName

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Prelude hiding (null, lookup, map, filter)

--------------------------------------------------------------------------------
-- * Reference indexed maps
--------------------------------------------------------------------------------

-- | Shorthand for stable names
type Name  = StableName

-- | HideType, for hiding types!
data HideType f where
  Hide :: f a -> HideType f

-- | A reference indexed map.
--   Useful for associating info with a reference.
data Map f = Map (IntMap [(HideType Name, HideType f)])

--------------------------------------------------------------------------------
-- ** Construction

-- | Construct an empty map.
empty :: Map f
empty = Map M.empty

-- | Construct a map with a single element.
singleton :: Name a -> f a -> Map f
singleton n v = Map $ M.singleton (hashStableName n) [(Hide n, Hide v)]

--------------------------------------------------------------------------------
-- ** Basic interface

-- | Returns 'True' if the map is empty, 'False' otherwise.
null :: Map f -> Bool
null (Map m) = M.null m

-- | Returns the number of elements stored in this map.
size :: Map f -> Int
size (Map m) = M.size m

-- | Returns 'True' if the name is present in the map, 'False' otherwise.
member :: Name a -> Map f -> Bool
member n (Map m) = M.member (hashStableName n) m

-- | Unsafe lookup
(!) :: Map f -> Name a -> f a
(!) m name = maybe (error "Data.Ref.Map.(!)") id (lookup name m)

-- | Finds the value associated with the name, or 'Nothing' if the name has no
-- value associated to it.
lookup :: Name a -> Map f -> Maybe (f a)
lookup n (Map m) =  case M.lookup (hashStableName n) m of
  Nothing -> Nothing
  Just xs -> case find (\(Hide x,_) -> eqStableName x n) xs of
    Nothing         -> Nothing
    Just (_,Hide f) -> Just $ unsafeCoerce f

-- | Associates a reference with the specified value. If the map already contains
-- a mapping for the reference, the old value is replaced.
insert :: Ref a -> f a -> Map f -> Map f
insert (Ref n _) v (Map m) = Map $ M.insertWith (++) (hashStableName n) [(Hide n, Hide v)] m

-- | Removes the associated value of a reference, if any is present in the map.
delete :: forall f a. Name a -> Map f -> Map f
delete n map@(Map m) = Map $ M.update del (hashStableName n) m
  where
    del :: [(HideType Name, HideType f)] -> Maybe [(HideType Name, HideType f)]
    del [] = Nothing
    del xs = Just $ deleteBy eq (Hide n, undefined) xs

    eq  :: (HideType Name, x) -> (HideType Name, y) -> Bool
    eq  (Hide x, _) (Hide y, _) = eqStableName x y

-- | Updates the associated value of a reference, if any is present in the map.
adjust :: forall f a b. (f a -> f b) -> Name a -> Map f -> Map f
adjust f n (Map m) = Map $ M.adjust (fmap open) (hashStableName n) m
  where
    open :: (HideType Name, HideType f) -> (HideType Name, HideType f)
    open pair@(Hide x, Hide v)
      | x `eqStableName` n = (Hide x, Hide $ f $ unsafeCoerce v)
      | otherwise          = pair

--------------------------------------------------------------------------------
-- ** Traversal

-- | Map over the container types
hmap :: forall f h a. (f a -> h a) -> Map f -> Map h
hmap f (Map m) = Map $ M.map (fmap $ fmap open) m
  where
    open :: HideType f -> HideType h
    open (Hide x) = Hide $ f $ unsafeCoerce x

--------------------------------------------------------------------------------
-- ** Combine

-- | Union of two maps (left biased).
union :: Map f -> Map f -> Map f
union (Map m) (Map n) = Map $ M.union m n

-- | Difference of two maps.
difference :: Map f -> Map f -> Map f
difference (Map m) (Map n) = Map $ M.difference m n

-- | Intersectino of two maps.
intersection :: Map f -> Map f -> Map f
intersection (Map m) (Map n) = Map $ M.intersection m n

--------------------------------------------------------------------------------
-- ** Debug - These are probably not safe

-- data Entry f = forall a. Entry a (f a)

dump :: Map f -> [[(HideType Name, HideType f)]]
dump (Map m) = M.elems m

--------------------------------------------------------------------------------
