{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}

module Vista.Runner.Gtk where

import Control.Monad (join, liftM2, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Events
import Data.Function (fix)
import Data.GI.Base qualified as Gtk
import Data.GI.Base.Signals qualified as Gtk
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)
import GHC.Generics (Generic)
import GI.Gtk qualified as Gtk
import Vista.Elements.Basic
import Vista.Elements.Events
import Vista.Elements.Interactive
import Vista.Markup

data GtkSingle c f = GtkSingle
  { words :: Element f Words,
    button :: Element f (Button (Single Words)),
    textField :: Element f TextField,
    list :: Element f (List c)
  }
  deriving (Generic)

data GtkContainer c f = GtkContainer
  { monad :: NestF f (MonadContext c),
    elements :: NestF f (GtkSingle c),
    events :: NestF f (ReactiveContext c)
  }
  deriving (Generic)

data GtkRoot cR cB f = GtkRoot
  { monad :: NestF f (MonadContext cR),
    body :: Element f (Child cB),
    events :: NestF f (ReactiveContext cR)
  }
  deriving (Generic)

addWidget :: (Gtk.IsWidget w) => w -> GtkM p ()
addWidget widget = do
  GtkEnv {replaceWidget} <- GtkM ask
  liftIO $ Gtk.toWidget widget >>= \w -> replaceWidget Nothing (Just w)

getParent :: GtkM p p
getParent = do
  GtkEnv {parent} <- GtkM ask
  pure parent

getGtkEnv :: GtkM p (GtkEnv p)
getGtkEnv = GtkM ask

addStartupTrigger :: IO () -> GtkM p ()
addStartupTrigger trigger = do
  env <- getGtkEnv
  liftIO $ modifyIORef' env.startupTriggers (>> trigger)

data GtkEnv p = GtkEnv
  { parent :: p,
    replaceWidget :: Maybe Gtk.Widget -> Maybe Gtk.Widget -> IO (),
    startupTriggers :: IORef (IO ())
  }

newtype GtkM p a = GtkM (ReaderT (GtkEnv p) IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

runGtkM :: GtkM p a -> GtkEnv p -> IO a
runGtkM (GtkM state) = runReaderT state

gtkRoot :: cR (GtkM Gtk.ApplicationWindow) -> cB (GtkM Gtk.Widget) -> GtkRoot cR cB (GtkM Gtk.ApplicationWindow)
gtkRoot cR cB =
  GtkRoot
    { monad = nest $ monadContext cR,
      body = makeElement $ \(Child a) -> do
        env@(GtkEnv {parent}) <- getGtkEnv
        windowWidget <- Gtk.toWidget parent
        liftIO $ runGtkM (runMarkup cB a) (env {parent = windowWidget}),
      events = nest $ reactiveContext cR
    }

reactiveContext :: c (GtkM p) -> ReactiveContext c (GtkM p)
reactiveContext c =
  ReactiveContext
    { reactive = makeElement $ \(Reactive react) -> GtkM (liftIO $ liftReact react),
      dynamicMarkup = makeElement $ \(DynamicMarkup dynX makeMarkup) -> do
        firstX <- liftIO $ getCurrent (dynX.behavior)
        env <- getGtkEnv
        currentWidgets <- liftIO $ newIORef []
        nextWidgets <- liftIO $ newIORef []
        (eventA, fireA) <- liftIO $ newEvent

        firstA <-
          liftIO $
            runGtkM (runMarkup c (makeMarkup firstX)) $
              env
                { replaceWidget =
                    replaceWidget
                      (\w -> modifyIORef nextWidgets (w :))
                      (\w -> modifyIORef nextWidgets (filter (/= w)))
                      (\old new -> modifyIORef nextWidgets (replaceElem old new))
                }

        finalize <- liftIO $ subscribe dynX.event $ \newX -> do
          newA <-
            runGtkM (runMarkup c (makeMarkup newX)) $
              env
                { replaceWidget =
                    replaceWidget
                      (\w -> modifyIORef nextWidgets (w :))
                      (\w -> modifyIORef nextWidgets (filter (/= w)))
                      (\old new -> modifyIORef nextWidgets (replaceElem old new))
                }

          liftIO $ join $ liftM2 (updateWidgets env.replaceWidget) (reverse <$> readIORef currentWidgets) (reverse <$> readIORef nextWidgets)

          liftIO $ readIORef nextWidgets >>= writeIORef currentWidgets
          liftIO $ writeIORef nextWidgets []
          fireA newA

        liftIO $ join $ liftM2 (updateWidgets env.replaceWidget) (reverse <$> readIORef currentWidgets) (reverse <$> readIORef nextWidgets)

        liftIO $ readIORef nextWidgets >>= writeIORef currentWidgets
        liftIO $ writeIORef nextWidgets []

        addStartupTrigger $ fireA firstA
        pure eventA
    }
  where
    replaceElem _ _ [] = []
    replaceElem a b (x : xs)
      | a == x = (b : xs)
      | otherwise = x : replaceElem a b xs
    updateWidgets :: (Maybe Gtk.Widget -> Maybe Gtk.Widget -> IO ()) -> [Gtk.Widget] -> [Gtk.Widget] -> IO ()
    updateWidgets _ [] [] = pure ()
    updateWidgets replace (old : olds) [] = do
      replace (Just old) Nothing
      updateWidgets replace olds []
    updateWidgets replace [] (new : news) = do
      replace Nothing (Just new)
      updateWidgets replace [] news
    updateWidgets replace (old : olds) (new : news) = do
      replace (Just old) (Just new)
      updateWidgets replace olds news

replaceWidget :: (Gtk.Widget -> IO ()) -> (Gtk.Widget -> IO ()) -> (Gtk.Widget -> Gtk.Widget -> IO ()) -> Maybe Gtk.Widget -> Maybe Gtk.Widget -> IO ()
replaceWidget append remove replace old new = case (old, new) of
  (Just o, Just n) -> replace o n
  (Just o, Nothing) -> remove o
  (Nothing, Just n) -> append n
  (Nothing, Nothing) -> pure ()

gtkSingle :: c (GtkM Gtk.Widget) -> GtkSingle c (GtkM Gtk.Widget)
gtkSingle c =
  GtkSingle
    { words = makeElement $ \(Words txt) -> do
        label <- liftIO $ Gtk.new Gtk.Label [#label Gtk.:= txt]
        addWidget label
        pure (),
      button = makeElement $ \(Button m) -> do
        button <- liftIO $ Gtk.new Gtk.Button []
        env <- getGtkEnv
        a <-
          liftIO $
            runGtkM (runMarkup (buttonContext wordsContext) m) $
              GtkEnv
                button
                ( replaceWidget
                    (#setChild button . Just)
                    (\_ -> #setChild button (Nothing :: Maybe Gtk.Widget))
                    (\_ new -> #setChild button (Just new))
                )
                env.startupTriggers
        addWidget button
        pure a,
      textField = makeElement $ \(TextField dynText context) -> do
        textField <- liftIO $ Gtk.new Gtk.Text [#overwriteMode Gtk.:= False]
        env <- getGtkEnv
        liftIO $
          runGtkM (runMarkup textFieldContext context) (GtkEnv textField (\_ _ -> pure ()) env.startupTriggers),
      list = makeElement $ \(List m) -> do
        box <- liftIO $ Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
        boxWidget <- Gtk.toWidget box
        addWidget box
        env <- getGtkEnv
        liftIO $
          runGtkM (runMarkup c m) $
            GtkEnv
              boxWidget
              ( replaceWidget
                  (#append box)
                  (#remove box)
                  ( \old new -> do
                      prevSibling <- #getPrevSibling old
                      #remove box old
                      #insertChildAfter box new prevSibling
                  )
              )
              env.startupTriggers
    }
  where
    buttonContext :: c (GtkM Gtk.Button) -> ButtonContext c (GtkM Gtk.Button)
    buttonContext c =
      ButtonContext
        { monad = nest $ monadContext (buttonContext c),
          onClick = makeElement $ \(OnClick a) -> do
            button <- getParent
            pure $ callback $ \fire -> do
              id <- Gtk.on button #clicked $ fire a
              pure $ Finalize $ Gtk.disconnectSignalHandler button id,
          child = makeElement $ \(Child m) -> runMarkup c m
        }
    textFieldContext :: TextFieldContext (GtkM Gtk.Text)
    textFieldContext =
      TextFieldContext
        { monad = nest $ monadContext textFieldContext,
          onInput = makeElement $ \OnInput -> do
            textField <- getParent
            pure $ callback $ \fire -> do
              id <- Gtk.on textField #preeditChanged $ \newText -> fire newText
              pure $ Finalize $ Gtk.disconnectSignalHandler textField id
        }
    wordsContext :: Single Words (GtkM p)
    wordsContext =
      Single
        { single = makeElement $ \(Words txt) -> do
            label <- liftIO $ Gtk.new Gtk.Label [#label Gtk.:= txt]
            addWidget label
            pure ()
        }

gtkContainer :: c (GtkM Gtk.Widget) -> GtkContainer c (GtkM Gtk.Widget)
gtkContainer c =
  GtkContainer
    { monad = nest $ monadContext c,
      elements = nest $ gtkSingle c,
      events = nest $ reactiveContext c
    }

startGtk :: GtkM Gtk.ApplicationWindow () -> IO ()
startGtk markup = do
  app <-
    Gtk.new
      Gtk.Application
      [ #applicationId Gtk.:= "app",
        Gtk.On #activate (activate ?self markup)
      ]

  void $ #run app Nothing

activate :: Gtk.Application -> GtkM Gtk.ApplicationWindow () -> IO ()
activate app markup = do
  window <-
    Gtk.new
      Gtk.ApplicationWindow
      [ #application Gtk.:= app,
        #title Gtk.:= "app"
      ]

  startupTriggers <- newIORef mempty

  runGtkM markup $ GtkEnv window (\_ new -> #setChild window new) startupTriggers

  join $ readIORef startupTriggers

  #show window

testMarkup :: Markup (Fix2 GtkRoot (GtkSingle (Fix GtkContainer))) ()
testMarkup = do
  child $ list $ do
    list $ do
      string "Hello"

      (event, trigger) <- newEvent

      clickEvent <- fixEvent $ \fixedClickEvent -> do
        let clicksEvent = foldEvent 0 (+) fixedClickEvent
        dynClicks <- holdEvent 0 clicksEvent

        clickEvent <- fmap switchEvents $ dynamicMarkup dynClicks $ \clicks -> button $ do
          child $ string ("Click me: " ++ show clicks)
          onClick (1 :: Int)

        subscribe clickEvent trigger

        dynamicMarkup dynClicks $ \clicks -> do
          string ("Clicks: " ++ show clicks)

        subscribe clicksEvent print

        pure (clickEvent, ())

      pure ()
    string "Hello"
    pure ()

testApp :: IO ()
testApp = startGtk $ runMarkup (fix2Context (flip gtkRoot (gtkSingle (fixContext gtkContainer)))) testMarkup

test2 :: Markup (Fix2 GtkRoot (GtkSingle (Fix GtkContainer))) ()
test2 = pure ()
