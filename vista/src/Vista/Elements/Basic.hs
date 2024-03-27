{-# LANGUAGE UndecidableInstances #-}

module Vista.Elements.Basic where

import Control.Monad.Trans.Writer
import Data.Text
import GHC.Generics (Generic)
import Vista.Markup

data Words a where
  Words :: Text -> Words ()

string :: (HasElement c Words) => String -> Markup c ()
string = toMarkup . Words . pack

text :: (HasElement c Words) => Text -> Markup c ()
text = toMarkup . Words

data Child c a where
  Child :: Markup c a -> Child c a

child :: forall c2 c1 a. (HasElement c1 (Child c2), c2 ~ InferType c1 Child) => Markup c2 a -> Markup c1 a
child = toMarkup . Child

data List c a where
  List :: Markup c a -> List c a

list :: forall c2 c1 a. (HasElement c1 (List c2), c2 ~ InferType c1 List) => Markup c2 a -> Markup c1 a
list = toMarkup . List

newtype Single e f = Single
  { single :: Element f e
  }
  deriving (Generic)

data Both e1 e2 f = Both
  { first :: Element f e1,
    second :: Element f e2
  }
  deriving (Generic)

-- data BasicContext c f = BasicContext
--   { monadContext :: Nest (MonadContext c f),
--     words :: Element f Words
--   }
--   deriving (Generic)

-- test :: Markup (Fix BasicContext) ()
-- test = do
--   let x = 4
--   let y = 2
--   string $ show (x + y)
--   text "Works"

-- basicContextText :: Fix BasicContext (Writer Text)
-- basicContextText =
--   Fix $
--     nest $
--       BasicContext
--         { monadContext =
--             nest $
--               MonadContext
--                 { map = makeElement $ \(Map f m) -> fmap f (runMarkup basicContextText m),
--                   pure = makeElement $ \(Pure a) -> pure a,
--                   apply = makeElement $ \(Apply f a) -> runMarkup basicContextText f <*> runMarkup basicContextText a,
--                   bind = makeElement $ \(Bind a f) -> runMarkup basicContextText a >>= runMarkup basicContextText . f
--                 },
--           words = makeElement $ \(Words t) -> tell t
--         }
