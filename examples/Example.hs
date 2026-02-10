{-# LANGUAGE GADTs, TypeFamilies, DataKinds, RankNTypes #-}

module Main where

import Control.Monad (forM_, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.List (foldl')

-- | Type-safe expression DSL with GADTs
data Expr a where
  Lit   :: a -> Expr a
  Add   :: Num a => Expr a -> Expr a -> Expr a
  Mul   :: Num a => Expr a -> Expr a -> Expr a
  Cond  :: Expr Bool -> Expr a -> Expr a -> Expr a
  Equal :: Eq a => Expr a -> Expr a -> Expr Bool
  App   :: Expr (a -> b) -> Expr a -> Expr b
  Lam   :: (Expr a -> Expr b) -> Expr (a -> b)

-- | Evaluate expressions
eval :: Expr a -> a
eval (Lit x)      = x
eval (Add a b)    = eval a + eval b
eval (Mul a b)    = eval a * eval b
eval (Cond c t e) = if eval c then eval t else eval e
eval (Equal a b)  = eval a == eval b
eval (App f x)    = eval f (eval x)
eval (Lam f)      = \x -> eval (f (Lit x))

-- | Free Monad for DSL interpretation
data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free f) = Free (fmap (fmap g) f)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> x = fmap f x
  Free f <*> x = Free (fmap (<*> x) f)

instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Free m >>= f = Free (fmap (>>= f) m)

-- | Key-Value Store DSL
data StoreF k v next
  = Get k (Maybe v -> next)
  | Put k v next
  | Delete k next
  deriving Functor

type Store k v = Free (StoreF k v)

get :: k -> Store k v (Maybe v)
get k = Free (Get k Pure)

put :: k -> v -> Store k v ()
put k v = Free (Put k v (Pure ()))

delete :: k -> Store k v ()
delete k = Free (Delete k (Pure ()))

-- | Interpret to IO
runStoreIO :: (Ord k, Show k, Show v) => Store k v a -> IO a
runStoreIO prog = do
  ref <- newIORef Map.empty
  let go (Pure a) = return a
      go (Free (Get k next)) = do
        m <- readIORef ref
        go (next (Map.lookup k m))
      go (Free (Put k v next)) = do
        modifyIORef' ref (Map.insert k v)
        go next
      go (Free (Delete k next)) = do
        modifyIORef' ref (Map.delete k)
        go next
  go prog

-- | Example program
program :: Store String Int ()
program = do
  put "x" 42
  put "y" 100
  mx <- get "x"
  case mx of
    Just x -> put "sum" (x + 100)
    Nothing -> pure ()
  delete "y"

main :: IO ()
main = runStoreIO program
