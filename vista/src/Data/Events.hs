{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Data.Events where

import Control.Monad.Fix (MonadFix)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (forM_)
import Data.IORef
import Data.Map qualified as M
import GHC.Weak (deRefWeak)

class MonadReact m where
  liftReact :: React a -> m a

instance MonadReact React where
  liftReact = id

instance MonadReact IO where
  liftReact (React io) = io

newtype React a = React (IO a) deriving (Functor, Applicative, Monad, MonadFix)

newtype Event a = Event
  { hook :: (a -> IO ()) -> IO Finalize
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

fixEvent :: (Monad m, MonadReact m) => (Event x -> m (Event x, a)) -> m a
fixEvent f = do
  (event, trigger) <- newEvent
  (pastEvent, a) <- f event
  -- TODO: Investigate if this leaks memory
  finalize <- subscribe pastEvent $ \x -> trigger x
  pure a

getCurrent :: (MonadReact m) => Behavior a -> m a
getCurrent (Behavior get) = liftReact $ React get

subscribe :: (MonadReact m) => Event a -> (a -> IO ()) -> m Finalize
subscribe (Event hook) f = liftReact $ React $ hook f

newEvent :: (MonadReact m) => m (Event a, a -> IO ())
newEvent = liftReact $ React $ do
  subsRef <- newIORef (M.empty, 0 :: Int)
  pure
    ( Event $ \f -> do
        subId <- atomicModifyIORef' subsRef $ \(subs, subId) ->
          ((M.insert subId f subs, succ subId), subId)
        pure $ Finalize $ do
          modifyIORef' subsRef $ first $ M.delete subId,
      \a -> do
        (subs, _) <- readIORef subsRef
        forM_ subs ($ a)
    )

callback :: ((a -> IO ()) -> IO Finalize) -> Event a
callback f = Event f

joinEvents :: Event a -> Event a -> Event a
joinEvents (Event hook1) (Event hook2) = Event $ \trigger -> do
  fin1 <- hook1 trigger
  fin2 <- hook2 trigger
  pure $ fin1 <> fin2

foldEvent :: b -> (a -> b -> b) -> Event a -> Event b
foldEvent initial f (Event hook) = Event $ \trigger -> do
  ref <- newIORef initial

  finalize <- hook $ \a -> do
    b <- readIORef ref
    let !b' = f a b
    writeIORef ref b'
    trigger b'

  pure finalize

holdEvent :: (MonadReact m) => a -> Event a -> m (Dynamic a)
holdEvent a event = liftReact $ React $ mdo
  ref <- newIORef a
  let behavior = Behavior (readIORef ref)

  finalize <- subscribe event $ \v -> do
    ref' <- deRefWeak weakRef
    maybe (pure ()) (flip writeIORef v) ref'

  weakRef <- mkWeakIORef ref (runFinalize finalize)

  pure $ Dynamic event behavior

never :: Event a
never = Event $ \_ -> pure (Finalize mempty)

switchEvents :: Event (Event a) -> Event a
switchEvents eventEvent = Event $ \trigger -> do
  cleanUp <- newIORef (Finalize mempty)

  subscribe eventEvent $ \newEvent -> do
    newFinalize <- subscribe newEvent trigger
    oldFinalize <- readIORef cleanUp
    runFinalize oldFinalize
    writeIORef cleanUp newFinalize

instance Functor Event where
  fmap f (Event hook) = Event $ \trigger -> hook $ trigger . f

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
      event = Event $ \trigger -> do
        finF <- hookF $ \f -> do
          a <- getCurrent behaviorA
          trigger $ f a
        finA <- hookA $ \a -> do
          f <- getCurrent behaviorF
          trigger $ f a
        pure (finF <> finA)
