module Test1 where

import Control.Monad.Trans.Free

data Toy b next = 
	  Output b next
	| Bell next
	| Done

data FixE f e = Fix (f (FixE f e)) | Throw e

instance Functor (Toy b) where
	fmap f (Output x next) = Output x (f next)
	fmap f (Bell     next) = Bell     (f next)
	fmap f  Done           = Done

catchF :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catchF (Fix x) f = Fix (fmap (flip catchF f) x)
catchF (Throw e) f = f e

run :: Free f r -> FreeF f r (Free f r)
run m = runFree m
