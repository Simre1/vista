{-# LANGUAGE ImplicitParams #-}

module Vista.Runner.Gtk where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Events
import Data.Function (fix)
import Data.GI.Base qualified as Gtk
import Data.GI.Base.Signals qualified as Gtk
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
    events :: Element f Reactive
  }
  deriving (Generic)

data GtkRoot cR cB f = GtkRoot
  { monad :: NestF f (MonadContext cR),
    body :: Element f (Child cB),
    events :: Element f Reactive
  }
  deriving (Generic)

addWidget :: (Gtk.IsWidget w) => w -> GtkM p ()
addWidget widget = do
  GtkEnv {addWidget} <- GtkM ask
  liftIO $ Gtk.toWidget widget >>= addWidget

getParent :: GtkM p p
getParent = do
  GtkEnv {parent} <- GtkM ask
  pure parent

getGtkEnv :: GtkM p (GtkEnv p)
getGtkEnv = GtkM ask

data GtkEnv p = GtkEnv
  { parent :: p,
    addWidget :: Gtk.Widget -> IO (),
    replaceWidget :: Gtk.Widget -> Gtk.Widget -> IO ()
  }

newtype GtkM p a = GtkM (ReaderT (GtkEnv p) IO a) deriving (Functor, Applicative, Monad, MonadIO)

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
      events = makeElement $ \(Reactive react) -> GtkM (liftIO $ liftReact react)
    }

gtkSingle :: c (GtkM Gtk.Widget) -> GtkSingle c (GtkM Gtk.Widget)
gtkSingle c =
  GtkSingle
    { words = makeElement $ \(Words txt) -> do
        label <- liftIO $ Gtk.new Gtk.Label [#label Gtk.:= txt]
        addWidget label
        pure (),
      button = makeElement $ \(Button m) -> do
        button <- liftIO $ Gtk.new Gtk.Button []
        a <-
          liftIO $
            runGtkM (runMarkup (buttonContext wordsContext) m) $
              GtkEnv button (#setChild button . Just) (\_ -> #setChild button . Just)
        addWidget button
        pure a,
      textField = makeElement $ \(TextField dynText context) -> do
        textField <- liftIO $ Gtk.new Gtk.Text [#overwriteMode Gtk.:= False]
        liftIO $
          runGtkM (runMarkup textFieldContext context) (GtkEnv textField (\_ -> pure ()) (\_ _ -> pure ())),
      list = makeElement $ \(List m) -> do
        box <- liftIO $ Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
        boxWidget <- Gtk.toWidget box
        addWidget box
        liftIO $
          runGtkM (runMarkup c m) $
            GtkEnv
              boxWidget
              (#append box)
              ( \old new -> do
                  prevSibling <- #getPrevSibling old
                  #remove box old
                  #insertChildAfter box new prevSibling
              )
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
      events = makeElement $ \(Reactive react) -> liftIO $ liftReact react
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

  runGtkM markup $ GtkEnv window (#setChild window . Just) (\_ -> #setChild window . Just)

  #show window

testMarkup :: Markup (Fix2 GtkRoot (GtkSingle (Fix GtkContainer))) ()
testMarkup = do
  list $ pure ()
  child $ list $ do
    list $ do
      string "Hello"
      clickEvent <- button $ do
        child $ string "Click"
        onClick "Chadlord"
      subscribe clickEvent putStrLn
      pure ()
    string "Hello"
    pure ()

testApp :: IO ()
testApp = startGtk $ runMarkup (fix2Context (flip gtkRoot (gtkSingle (fixContext gtkContainer)))) testMarkup

test2 :: Markup (Fix2 GtkRoot (GtkSingle (Fix GtkContainer))) ()
test2 = pure ()
