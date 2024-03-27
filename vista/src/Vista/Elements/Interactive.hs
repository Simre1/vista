module Vista.Elements.Interactive where

import Data.Events
import Data.Text (Text)
import Vista.Elements.Basic
import Vista.Elements.Events
import Vista.Markup
import GHC.Generics

data Button c a where
  Button :: Markup (ButtonContext c) a -> Button c a

data ButtonContext c f = ButtonContext
  { monad :: NestF f (MonadContext (ButtonContext c)),
    onClick :: Element f OnClick,
    child :: Element f (Child c)
  } deriving Generic

button :: forall c2 c1 a. (c2 ~ InferType c1 Button, HasElement c1 (Button c2)) => Markup (ButtonContext c2) a -> Markup c1 a
button = toMarkup . Button

data TextField a where
  TextField :: Dynamic Text -> Markup TextFieldContext a -> TextField a

data TextFieldContext f = TextFieldContext
  { monad :: NestF f (MonadContext TextFieldContext),
    onInput :: Element f OnInput
  } deriving Generic
