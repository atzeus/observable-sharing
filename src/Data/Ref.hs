module Data.Ref where

import System.Mem.StableName
import System.IO.Unsafe
import Control.Applicative

--------------------------------------------------------------------------------
-- * References
--------------------------------------------------------------------------------

data Ref a = Ref { label :: StableName a, deref :: a }

instance Eq (Ref a) where
  Ref s1 _ == Ref s2 _ = s1 == s2

instance Show a => Show (Ref a) where
  show (Ref s x) = "(Ref " ++ show (hashStableName s) ++ " " ++ show x ++ ")"

ref :: a -> Ref a
ref x = unsafePerformIO $ flip Ref x <$> makeStableName x
{-# NOINLINE ref #-}

--------------------------------------------------------------------------------
-- the end.
