-- {-# LANGUAGE DeriveFunctor #-}
--
module Main where
--
import Lib
-- import Control.Arrow
--
main :: IO ()
main = someFunc
--
-- data Lit
--   = StrLit String
--   | IntLit Int
--   | Ident String
--   deriving (Show, Eq)
--
-- data Expr a
--   = Index a a
--   | Call [a]
--   | Unary String a
--   | Binary a String a
--   | Paren a
--   | Literal Lit
--   deriving (Show, Eq, Functor)
--
-- newtype Term f = In (f (Term f))
--
-- out :: Term f -> f (Term f)
-- out (In t) = t
--
-- bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- bottomUp fn =
--   out                    -- 1) unpack
--   >>> fmap (bottomUp fn) -- 2) recurse
--   >>> In                 -- 3) repack
--   >>> fn                 -- 4) apply
--
-- topDown'' :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- topDown'' fn =
--   In
--   <<< fmap (topDown fn)
--   <<< out
--   <<< fn
--
-- topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- topDown fn =
--   fn                    -- 1) apply
--   >>> out               -- 2) unpack
--   >>> fmap (topDown fn) -- 3) recurse
--   >>> In                -- 4) repack
--
-- topDown' :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- topDown' fn =
--   In . fmap (topDown fn) . out . fn
--
-- fn :: Term a -> Term a
-- fn = undefined
--
-- flattenTerm :: Term Expr -> Term Expr
-- flattenTerm (In (Paren a)) = a
-- flattenTerm other = other
