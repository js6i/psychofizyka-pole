{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           System.Random
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Int
import           Data.Char
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.AcidState
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X
import           Debug.Trace
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = do
      result <- registerUser "login" "password"
      case result of
        Left _   -> return () >> redirect "/"
        Right au -> update (AddUser (userLogin au)) >> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",      with auth handleLoginSubmit)
         , ("/logout",     with auth handleLogout)
         , ("/new_user",   with auth handleNewUser)
         , ("/experiment", with auth handleExperiment)
         , ("/results",    handleResults)
         , ("",            serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | Handle experiment.
handleExperiment = requireUser auth (redirect "/login") $
                        method GET  handleNewExperiment
                    <|> method POST handleEndExperiment

------------------------------------------------------------------------------
-- | Generate experiment setup.
handleNewExperiment :: Handler App (AuthManager App) ()
handleNewExperiment = do
  shape <- liftIO (randomIO :: IO Shape)
  flcol <- liftIO (randomIO :: IO FillColor)
  ratio <- liftIO (randomRIO (2,   10) :: IO Pixels)
  iniSz <- liftIO (randomRIO (20, 100) :: IO Pixels)

  let col2RGB = T.pack . map toLower . show

      tst = I.bindSplices $
              [ ("ratio", return [X.TextNode . T.pack . show $ ratio])
              , ("iniSz", return [X.TextNode . T.pack . show $ iniSz])
              , ("color", return [X.TextNode . col2RGB $ flcol])
              , ("shape", return [X.TextNode . T.pack . show $ shape])
              ]

  heistLocal tst $ render "experiment"

------------------------------------------------------------------------------
-- | Finish experiment.
handleEndExperiment :: Handler App (AuthManager App) ()
handleEndExperiment = do
  vals <- getPostParams
  
  let record = do
              color  <- M.lookup "color" vals
              ratio  <- M.lookup "ratio" vals
              iniSz  <- M.lookup "initialSize" vals
              userSz <- M.lookup "userSize" vals
              shape  <- M.lookup "shape" vals

              let capitalize (x:xs) = toUpper x : xs
                  capitalize x = map toUpper x

              return $ Record { _shape   = read . B.unpack . head $ shape
                              , _color   = read . capitalize . B.unpack . head $ color
                              , _ratio   = read . B.unpack . head $  ratio
                              , _initial = read . B.unpack . head $  iniSz
                              , _result  = read . B.unpack . head $ userSz
                              }
  uid <- fmap (userLogin . fromJust) currentUser

  case record of
    Nothing -> redirect "/"
    Just r  -> update (AddRecord uid r) >> redirect "/experiment"


------------------------------------------------------------------------------
-- | Show all results.

handleResults :: Handler App App ()
handleResults = do
  results <- query GetRecords

  let tst = I.bindSplices [("records", return [X.TextNode . T.pack . show $ results])]

  heistLocal tst $ render "results"

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    c <- nestSnaplet "acid" acid $ acidInit (PersistentState M.empty)
    addRoutes routes
    addAuthSplices auth
    return $ App h s a c

