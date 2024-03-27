{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vista.Elements.Events where

import Data.Events
import Data.Text (Text)
import Vista.Markup

newtype Reactive a where
  Reactive :: React a -> Reactive a

instance (HasElement c Reactive) => MonadReact (Markup c) where
  liftReact = toMarkup . Reactive

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
