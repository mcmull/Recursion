{-# LANGUAGE DeriveFunctor #-}

module Intro2.Change where

import UnderstandAlgebras.Algebras
import Data.List

data Nat a = Zero | Next a deriving Functor

zero, one, two :: Fix Nat
zero = Fx Zero
one = Fx (Next zero)
two = Fx (Next one)

expand :: Int -> Fix Nat
-- expand 0 = Fx Zero
-- expand n = Fx (Next (expand (n - 1)))
-- alternative impl. by anamorphism
expand = ana coalgebra

coalgebra :: Int -> Nat Int
coalgebra 0 = Zero
coalgebra n = Next (n - 1)

compress :: CVAlgebra Nat Int
compress Zero = 0
compress (Next (Attr _ f)) = 1 + compress f

type Cent = Int
type Amount = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

change :: Amount -> Int
change amt = histo go (expand amt) where
  go Zero = 1
  go curr@(Next attr) = let
    given = compress curr
    validCoins = filter (<= given) coins
    remaining = map (given -) validCoins
    (zeros, toProcess) = partition (== 0) remaining in
    length zeros + sum (map (lookup' attr) toProcess)

lookup' :: Attr Nat a -> Int -> a
lookup' cache 0 = attribute cache
lookup' cache n = lookup' inner (n-1) where (Next inner) = hole cache
