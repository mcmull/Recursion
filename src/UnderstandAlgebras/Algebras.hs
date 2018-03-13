module UnderstandAlgebras.Algebras where

import Control.Arrow

data ExprF a = Const Int
             | Add a a
             | Mul a a

newtype Fix f = Fx (f (Fix f))

type Expr = Fix ExprF

-- non-recursive top-level evaluator
alg :: ExprF Int -> Int
alg (Const x) = x
alg (Add x y) = x + y
alg (Mul x y) = x * y

-- recursive children evaluator
-- to map children recursive
eval :: a -> b
eval = undefined

instance Functor ExprF where
  fmap eval (Const x) = Const x
  fmap eval (Add x y) = Add (eval x) (eval y)
  fmap eval (Mul x y) = Mul (eval x) (eval y)

type Algebra f a = f a -> a

type ExprInitAlg = Algebra ExprF (Fix ExprF)

-- doesn't reduce anything like the evaluators (e.g. alg) we've been using so far
-- therefore called "Initial Algebra"
ex_init_alg :: ExprInitAlg
ex_init_alg = Fx

g :: Functor f => Fix f -> a
g = alg' . fmap g . unFix

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

alg' :: f a -> a
alg' = undefined

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

-- data ListF a b = Nil | Cons a b
--
-- instance Functor (ListF a) where
--   fmap _ Nil = Nil
--   fmap f (Cons x y) = Cons x $ f y
--
-- algSum :: Algebra (ListF Int) Int
-- algSum Nil = 0
-- algSum (Cons x y) = x + y

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana co = Fx . fmap (ana co) . co

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }

type CVAlgebra f a = f (Attr f a) -> a

histo' :: Functor f => CVAlgebra f a -> Fix f -> a
histo' h = h . fmap worker . unFix
  where worker fix = Attr (histo' h fix) (fmap worker (unFix fix))

histo :: Functor f => CVAlgebra f a -> Fix f -> a
histo h = attribute . worker where
  worker = mkAttr . (h &&& id) . fmap worker . unFix
  mkAttr (x, fAttr) = Attr x fAttr
