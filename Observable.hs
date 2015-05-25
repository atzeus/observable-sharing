{-# LANGUAGE GADTs #-}
module Data.Ref(Ref(..), ref, deref, Rim, emptyRim, insertRim, lookupRim) where

import Data.Unique
import System.IO.Unsafe
import Control.Applicative hiding (empty)
import Data.Map
import Unsafe.Coerce
import Prelude hiding (lookup)
--------------------------------------------------------------------------------
-- * References
--------------------------------------------------------------------------------

data Ref a = Ref { refNr :: Unique , deref :: a }

instance Eq (Ref a) where
  Ref u1 _ == Ref u2 _ = u1 == u2

instance Show a => Show (Ref a) where
  show (Ref u x) = "(Ref " ++ show (hashUnique u) ++ " " ++ show x ++ ")"

ref :: a -> Ref a
ref x = unsafePerformIO $ flip Ref x <$> newUnique
{-# NOINLINE ref #-}

data HideType f where
  Hide :: f a -> HideType f

-- | A Reference Indexed Map.
-- 
-- Useful for associating info with a reference.
data Rim f = Rim (Map Unique (HideType f))

-- | An empty rim.
emptyRim :: Rim f
emptyRim = Rim empty

insertRim :: Ref a -> f a -> Rim f -> Rim f
insertRim (Ref u _) v (Rim m) = Rim $ insert u (Hide v) m

lookupRim :: Ref a -> Rim f -> Maybe (f a)
lookupRim (Ref u _) (Rim m) = fmap unsafeCoerce $ lookup u m


--------------------------------------------------------------------------------
-- the end.
