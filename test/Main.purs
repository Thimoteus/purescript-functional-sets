module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Set.Functional (Set, complement, difference, elem, set, universe)

nats :: Set Int
nats = set (_ >= 0)

ints :: Set Int
ints = universe

evens :: Set Int
evens = set (\ x -> (x `mod` 2) == 0)

nonpositives :: Set Int
nonpositives = set (_ <= 0)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "nats don't have negative integers"
  logShow $ (-15) `elem` nats
  log "ints have everything"
  logShow $ (-15) `elem` ints
  logShow $ 0 `elem` ints
  logShow $ 15515 `elem` ints
  log "nats \\ evens = odds"
  logShow $ 131 `elem` (ints `difference` evens)
  log "complement of nonpositives is positives"
  logShow $ 0 `elem` (complement nonpositives)
  logShow $ (-999) `elem` (complement nonpositives)
  logShow $ 1382 `elem` (complement nonpositives)