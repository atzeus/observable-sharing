module Data.Ref where

import Data.Unique
import System.IO.Unsafe
import Control.Applicative

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

--------------------------------------------------------------------------------
-- the end.
