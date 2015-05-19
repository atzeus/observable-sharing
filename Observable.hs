
import Data.Unique
import System.IO.Unsafe
import Control.Applicative

module Data.Ref where

data Ref a = Ref { refNr :: Unique , deref :: a } deriving Eq

instance Show a => Show (Ref a) where
  show (Ref u x) = "(Ref " ++ show (hashUnique u) ++ " " ++ show x ++ ")"

ref :: a -> Ref a
ref x = unsafePerformIO $ (\u -> Ref u x) <$> newUnique
{-# NOINLINE ref #-}

(<=>) :: Ref a -> Ref a -> Bool
(Ref u1 _) <=> (Ref u2 _) = u1 == u2
