{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Vista.Markup
  ( Element,
    makeElement,
    HasElement,
    HasElements,
    coerceMarkup,
    ET.Nest,
    NestF,
    ET.nest,
    ET.unNest,
    InferType,
    InferType2,
    Markup,
    runMarkup,
    toMarkup,
    InferContext,

    -- * Monadic Markup
    Map (..),
    Pure (..),
    Apply (..),
    Bind (..),
    MFix (..),
    MonadContext (..),
    monadContext,

    -- * Fixpoint context
    Fix (..),
    fixContext,
    Fix2 (..),
    fix2Context,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad.Fix
import Data.Coerce (Coercible, coerce)
import Data.ExtractType qualified as ET
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

newtype Markup (c :: (Type -> Type) -> Type) (e :: Type) = Markup (forall f. c f -> f e)

newtype Element (f :: Type -> Type) (element :: Type -> Type) = Element (forall x. element x -> f x)

makeElement :: (forall e. element e -> f e) -> Element f element
makeElement = Element

data SearchF a

type family HasElement c element where
  HasElement c element = ET.Extract (c SearchF) (Element SearchF element)

type family HasElements t (elements :: [Type -> Type]) :: Constraint where
  HasElements t '[] = ()
  HasElements t '[x] = HasElement t x
  HasElements t (x : xs) = (HasElement t x, HasElements t xs)

toMarkup :: forall element c e. (HasElement c element) => element e -> Markup c e
toMarkup element = Markup $ \context ->
  let (Element f) = ET.extract @(c SearchF) @(Element SearchF element) $ unsafeCoerce context
   in unsafeCoerce $ f element

runMarkup :: c f -> forall e. Markup c e -> f e
runMarkup c (Markup f) = f c

coerceMarkup :: (Coercible e1 e2) => Markup c e1 -> Markup c e2
coerceMarkup (Markup f) = Markup $ unsafeCoerce . f

type NestF f a = ET.Nest (a f)

type family InferContext (c :: (Type -> Type) -> Type) (e :: ((Type -> Type) -> Type) -> Type -> Type) :: (Type -> Type) -> Type

data Map c b where
  Map :: (a -> b) -> Markup c a -> Map c b

instance (HasElement c (Map c)) => Functor (Markup c) where
  fmap f = toMarkup . Map f

data Pure a where
  Pure :: a -> Pure a

data Apply c b where
  Apply :: Markup c (a -> b) -> Markup c a -> Apply c b

instance (HasElements c '[Pure, Apply c, Map c]) => Applicative (Markup c) where
  pure a = toMarkup $ Pure a
  m1 <*> m2 = toMarkup $ Apply m1 m2

data Bind c b where
  Bind :: Markup c a -> (a -> Markup c b) -> Bind c b

data MFix c b where
  MFix :: (a -> Markup c a) -> MFix c a

instance (HasElements c '[Bind c, Pure, Apply c, Map c]) => Monad (Markup c) where
  m >>= f = toMarkup $ Bind m f

instance (HasElements c [Pure, Apply c, Map c], Semigroup a) => Semigroup (Markup c a) where
  (<>) = liftA2 (<>)

instance (HasElements c [Pure, Apply c, Map c], Monoid a) => Monoid (Markup c a) where
  mempty = pure mempty

instance (HasElements c [MFix c, Bind c, Pure, Apply c, Map c]) => MonadFix (Markup c) where
  mfix = toMarkup . MFix

data MonadContext c f = MonadContext
  { map :: Element f (Map c),
    pure :: Element f Pure,
    apply :: Element f (Apply c),
    bind :: Element f (Bind c),
    mfix :: Element f (MFix c)
  }
  deriving (Generic)

monadContext :: (MonadFix m) => c m -> MonadContext c m
monadContext c =
  MonadContext
    { map = makeElement $ \(Map f a) -> f <$> runMarkup c a,
      pure = makeElement $ \(Pure a) -> pure a,
      apply = makeElement $ \(Apply f a) -> runMarkup c f <*> runMarkup c a,
      bind = makeElement $ \(Bind a f) -> runMarkup c a >>= runMarkup c . f,
      mfix = makeElement $ \(MFix f) -> mfix (runMarkup c . f)
    }

newtype Fix c f = Fix
  { fix :: ET.Nest (c (Fix c) f)
  }
  deriving (Generic)

fixContext :: (Generic (c (Fix c) f)) => (Fix c f -> c (Fix c) f) -> Fix c f
fixContext k = let c = Fix $ ET.nest (k c) in c

newtype Fix2 c1 c2 f = Fix2
  { fix :: ET.Nest (c1 (Fix2 c1 c2) c2 f)
  }
  deriving (Generic)

fix2Context :: (Generic (c1 (Fix2 c1 c2) c2 f)) => (Fix2 c1 c2 f -> c1 (Fix2 c1 c2) c2 f) -> Fix2 c1 c2 f
fix2Context k = let c = Fix2 $ ET.nest (k c) in c

type family Append x t where
  Append (x : xs) ts = Append xs (x : ts)
  Append '[] ts = ts

type family InferType' (f :: Type -> Type) (part :: k -> Type -> Type) :: [k] where
  InferType' (Rec0 (ET.Nest a)) b = InferType' (Rep a) b
  InferType' (Rec0 (Element SearchF (b c))) b = '[c]
  InferType' (M1 d meta i) b = InferType' i b
  InferType' (l :*: r) b = Append (InferType' l b) (InferType' r b)
  InferType' _ _ = '[]

type family InferType2' (f :: Type -> Type) (part :: k -> l -> Type -> Type) :: [(k, l)] where
  InferType2' (Rec0 (ET.Nest a)) b = InferType2' (Rep a) b
  InferType2' (Rec0 (Element SearchF (b x y))) b = '[ '(x, y)]
  InferType2' (M1 d meta i) b = InferType2' i b
  InferType2' (l :*: r) b = Append (InferType2' l b) (InferType2' r b)
  InferType2' _ _ = '[]

type family Single as err where
  Single '[a] _ = a
  Single '[] err = TypeError (Text "Element is not within context: " :<>: err)
  Single as err = TypeError (Text "More than one element handler is within the context: " :$$: err)

type InferType f part = Single (InferType' (Rep (f SearchF)) part) (ShowType part)

type InferType2 f part = Single (InferType2' (Rep (f SearchF)) part) (ShowType part)
