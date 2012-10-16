module Test1 where

import Control.Monad.Trans.Free

run :: Free f r -> FreeF f r (Free f r)
run m = runFree m
