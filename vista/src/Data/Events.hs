module Data.Events where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (forM_)
import Data.IORef
import Data.Map qualified as M

class MonadReact m where
  liftReact :: React a -> m a

instance MonadReact IO where
  liftReact (React x) = x

newtype React a = React (IO a) deriving (Functor, Applicative, Monad)

newtype Event a = Event
  { hook :: IO (a -> IO ()) -> IO Finalize
  }

data Dynamic a
  = Dynamic
      { event :: Event a,
        behavior :: Behavior a
      }
  | ConstDynamic a

newtype Finalize = Finalize (IO ()) deriving (Semigroup, Monoid)

runFinalize :: Finalize -> IO ()
runFinalize (Finalize action) = action

newtype Behavior a = Behavior (IO a) deriving (Functor, Applicative, Monad, Semigroup, Monoid)

getCurrent :: (MonadReact m) => Behavior a -> m a
getCurrent (Behavior get) = liftReact $ React get

subscribe :: (MonadReact m) => Event a -> (a -> IO ()) -> m Finalize
subscribe (Event hook) f = liftReact $ React $ hook (pure f)

newEvent :: (MonadReact m) => m (Event a, a -> IO ())
newEvent = liftReact $ React $ do
  subsRef <- newIORef (M.empty, 0 :: Int)
  pure
    ( Event $ \makeF -> do
        f <- makeF
        subId <- atomicModifyIORef' subsRef $ \(subs, subId) ->
          ((M.insert subId f subs, succ subId), subId)
        pure $ Finalize $ do
          modifyIORef' subsRef $ first $ M.delete subId,
      \a -> do
        (subs, _) <- readIORef subsRef
        forM_ subs ($ a)
    )

callback :: ((a -> IO ()) -> IO Finalize) -> Event a
callback f = Event $ \makeTrigger -> do
  trigger <- makeTrigger
  f trigger

joinEvents :: Event a -> Event a -> Event a
joinEvents (Event hook1) (Event hook2) = Event $ \makeTrigger -> do
  trigger <- makeTrigger
  fin1 <- hook1 $ pure trigger
  fin2 <- hook2 $ pure trigger
  pure $ fin1 <> fin2

instance Functor Event where
  fmap f (Event hook) = Event $ \trigger -> hook $ fmap (. f) trigger

instance Functor Dynamic where
  fmap f (ConstDynamic a) = ConstDynamic (f a)
  fmap f (Dynamic event behavior) = Dynamic (f <$> event) (f <$> behavior)

instance Applicative Dynamic where
  pure = ConstDynamic
  (ConstDynamic f) <*> (ConstDynamic b) = ConstDynamic (f b)
  (ConstDynamic f) <*> (Dynamic event behavior) = Dynamic (f <$> event) (f <$> behavior)
  (Dynamic event behavior) <*> (ConstDynamic a) = Dynamic (($ a) <$> event) (($ a) <$> behavior)
  (Dynamic (Event hookF) behaviorF) <*> (Dynamic (Event hookA) behaviorA) = Dynamic event (behaviorF <*> behaviorA)
    where
      event = Event $ \makeTrigger -> do
        trigger <- makeTrigger
        finF <- hookF $ pure $ \f -> do
          a <- getCurrent behaviorA
          trigger $ f a
        finA <- hookA $ pure $ \a -> do
          f <- getCurrent behaviorF
          trigger $ f a
        pure (finF <> finA)
