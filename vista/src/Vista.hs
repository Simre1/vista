-- module Vista where

-- import Control.Arrow (Arrow (..))
-- import Control.Monad (forM_)
-- import Control.Monad.Trans.Class (MonadTrans (lift))
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.State.Strict
-- import Data.IORef
-- import Data.Map qualified as M
-- import Data.Text (Text, pack)
-- import Data.Text.Lazy (unpack)
-- import Foreign.C
-- import Foreign.Ptr
-- import Text.Blaze qualified as Blaze

-- data Vista c a where
--   VMap :: (a -> b) -> Vista c a -> Vista c b
--   VPure :: a -> Vista c a
--   VApply :: Vista c (a -> b) -> Vista c a -> Vista c b
--   VBind :: Vista c a -> (a -> Vista c b) -> Vista c b
--   VIO :: IO a -> Vista c a
--   VistaComponent :: c a -> Vista c a

-- -- data 

-- -- data VAction a where
-- --   AddHtmlWithChild :: (Blaze.Markup -> Blaze.Markup) -> Vista a -> VAction a
-- --   AddHtml :: Blaze.Markup -> VAction ()
-- --   BindProperty :: Text -> Text -> Dynamic SomeValue -> VAction ()
-- --   OnEvent :: Text -> VAction (Event a)

-- -- newtype Backend = Backend (forall x. Vista x -> IO x)

-- -- data SomeValue
-- --   = SomeInt Int
-- --   | SomeDouble Double
-- --   | SomeText Text
-- --   | SomeBool Bool
-- --   | NoValue

-- instance Functor (Vista c) where
--   fmap = VMap

-- instance Applicative (Vista c) where
--   pure = VPure
--   (<*>) = VApply

-- instance Monad (Vista c) where
--   (>>=) = VBind

-- -- runVista :: Backend -> (Vista c) x -> IO x
-- -- runVista (Backend f) = f
