{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vista.Elements.Events where

import Data.Events
import Data.Text (Text)
import Vista.Markup
import GHC.Generics (Generic)

newtype Reactive a where
  Reactive :: React a -> Reactive a

instance (HasElement c Reactive) => MonadReact (Markup c) where
  liftReact = toMarkup . Reactive

data DynamicMarkup c a where
  DynamicMarkup :: Dynamic x -> (x -> Markup c a) -> DynamicMarkup c (Event a)

data ReactiveContext c f = ReactiveContext
  { reactive :: Element f Reactive,
    dynamicMarkup :: Element f (DynamicMarkup c)
  } deriving Generic

dynamicMarkup :: (HasElement c (DynamicMarkup c)) => Dynamic x -> (x -> Markup c a) -> Markup c (Event a)
dynamicMarkup dyn f = toMarkup $ DynamicMarkup dyn f

data OnClick a where
  OnClick :: a -> OnClick (Event a)

onClick :: (HasElement c OnClick) => a -> Markup c (Event a)
onClick = toMarkup . OnClick

data OnInput a where
  OnInput :: OnInput (Event Text)

onInput :: (HasElement c OnInput) => Markup c (Event Text)
onInput = toMarkup OnInput

data OnHover a where
  OnHover :: a -> OnHover (Event a)

onHover :: (HasElement c OnHover) => a -> Markup c (Event a)
onHover = toMarkup . OnHover
