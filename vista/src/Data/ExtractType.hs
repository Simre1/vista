{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ExtractType (Extract, extract, Nest, nest, unNest) where

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits

data Direction = L | R | D | F | N

type family FindPath (f :: Type -> Type) b :: [Direction] where
  FindPath (Rec0 (Nest a)) b = N : FindPath (Rep a) b
  FindPath (Rec0 a) b = ValidElement a b
  FindPath (M1 d meta i) b = 'D : FindPath i b
  FindPath (l :*: r) b =
    If
      (Equal (Last (FindPath l b)) 'F)
      (L : FindPath l b)
      ('R : FindPath r b)
  FindPath _ _ = '[]

type family If c a b where
  If 'True a _ = a
  If 'False _ b = b

type family Equal a b where
  Equal a a = True
  Equal _ _ = False

type family Last (xs :: [k]) :: k where
  Last (y : x : xs) = Last (x : xs)
  Last '[x] = x

type family ValidElement a b where
  ValidElement a a = '[F]
  ValidElement _ _ = '[]

class ExtractType (path :: [Direction]) (f :: Type -> Type) b where
  extractType' :: f () -> b

instance (ExtractType path i b) => ExtractType ('D : path) (M1 d meta i) b where
  extractType' (M1 x) = extractType' @path x
  {-# INLINE extractType' #-}

instance (ExtractType path l b) => ExtractType ('L : path) (l :*: r) b where
  extractType' (l :*: _) = extractType' @path l
  {-# INLINE extractType' #-}

instance (ExtractType path r b) => ExtractType ('R : path) (l :*: r) b where
  extractType' (_ :*: r) = extractType' @path r
  {-# INLINE extractType' #-}

instance ExtractType '[F] (Rec0 b) b where
  extractType' (K1 b) = b
  {-# INLINE extractType' #-}

instance TypeError (Text "Could not extract value: " :$$: ShowType x ) => ExtractType '[] y x where
  extractType' _ = undefined
  {-# INLINE extractType' #-}

instance (ExtractType path i b) => ExtractType ('D : path) (Rec0 (i ())) b where
  extractType' (K1 x) = extractType' @path x
  {-# INLINE extractType' #-}

instance (ExtractType path (Rep a) b) => ExtractType ('N : path) (Rec0 (Nest a)) b where
  extractType' (K1 (Nest a)) = extractType' @path a
  {-# INLINE extractType' #-}

type Extract a b = (Generic a, ExtractType (FindPath (Rep a) b) (Rep a) b)

-- | Generically extract some value of type `b` from `a`. If there is no `b`, this will not typecheck.
-- If there are multiple `b`s, it will return the first one in depth-first top-down order.
extract :: forall a b. (Generic a, Extract a b) => a -> b
extract a =
  extractType' @(FindPath (Rep a) b)
    (from a :: Rep a ())
{-# INLINE extract #-}

-- | `extract` will not recurse to inner types even if those types implement `Generic`. If you want to recursively extract,
-- use the `Nest` type on those you want `extract` to search through.
newtype Nest a = Nest (Rep a ())

nest :: (Generic a) => a -> Nest a
nest a = Nest $ from a
{-# INLINE nest #-}

unNest :: (Generic a) => Nest a -> a
unNest (Nest a) = to a
{-# INLINE unNest #-}

