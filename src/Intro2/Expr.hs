{-# LANGUAGE DeriveFunctor #-}

module Intro2.Expr where

import Control.Arrow
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P
import Data.Monoid
import Data.Function

data Expr a
    = Literal { intVal :: Int }
    | Ident   { name :: String  }
    | Index   { target :: a, idx :: a }
    | Unary   { op :: String, target :: a }
    | Binary  { lhs :: a, op :: String, rhs :: a }
    | Call    { func :: a, args :: [a] }
    | Paren   { target :: a }
    deriving (Show, Eq, Functor)

newtype Term f = In' { out' :: f (Term f) }

-- instance Functor Expr where
--   fmap _ (Literal i) = Literal i
--   fmap _ (Ident n) = Ident n
--   fmap f (Index t i) = Index (f t) (f i)
--   fmap f (Unary o t) = Unary o (f t)
--   fmap f (Binary l o r) = Binary (f l) o (f r)
--   fmap f (Call func args) = Call (f func) (map f args)
--   fmap f (Paren t) = Paren (f t)

bottomUp' :: Functor f => (f c -> c) -> Term f -> c
bottomUp' fn' =
  out'                      -- 1) unpack a `Term a` into an `a (Term a)`
  >>> fmap (bottomUp' fn')  -- 2) recurse, with fn, into the subterms
  >>> fn'                   -- 4) finally, apply fn to the packed `Term a`

bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn =
  out'                    -- 1) unpack
  >>> fmap (bottomUp fn) -- 2) recurse
  >>> In'                 -- 3) repack
  >>> fn                 -- 4) apply

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out' >>> fmap (cata fn) >>> fn

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In' <<< fmap (ana fn) <<< fn

type RAlgebra f a = f (Term f, a) -> a

para :: Functor f => RAlgebra f a -> Term f -> a
para fn = out' >>> fmap (id &&& para fn) >>> fn

type RAlgebra' f a = Term f -> f a -> a

para'' :: Functor f => RAlgebra' f a -> Term f -> a
para'' fn term = out' term & fmap (para'' fn) & fn term

cata' :: Functor f => Algebra f a -> Term f -> a
cata' fn = para'' (const fn)

type RCoalgebra f a = a -> f (Either (Term f) a)

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo fn = In' <<< fmap (id ||| apo fn) <<< fn

data Attr f a = Attr { attribute :: a, hole :: f (Attr f a)}

type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo fn = out' >>> fmap what >>> fn
  where what term = Attr (histo fn term) (what <$> out' term)

-- para'' :: (Functor f) => RAlgebra' f a -> Term f -> a
-- para'' fn = out' >>> fmap undefined >>> fn

ten  = In' Literal { intVal = 10 }
add  = In' Ident { name = "add" }
call = In' Call { func = add, args = [ten, ten] }

countNodes :: Expr Int -> Int
countNodes Literal {} = 1
countNodes Ident {} = 1
countNodes (Index target idx) = target + idx + 1
countNodes (Unary _ target) = target + 1
countNodes (Binary lhs _ rhs) = lhs + 1 + rhs
countNodes (Call func args) = func + sum args + 1
countNodes (Paren target) = target + 1

prettyPrint :: Algebra Expr Doc
prettyPrint (Literal i) = P.int i
prettyPrint (Ident s) = P.text s
prettyPrint (Call f as)     = f <> P.parens (P.cat $ P.punctuate (P.text ", ") as)
prettyPrint (Index it idx)  = it <> P.brackets idx
prettyPrint (Unary op it)   = P.text op <> it
prettyPrint (Binary l op r) = l <> P.text op <> r
prettyPrint (Paren exp)     = P.parens exp

what :: (a -> b) -> (a -> c) -> a -> (b, c)
what f g x = (f x, g x)
