{-# LANGUAGE ScopedTypeVariables #-}
module Foundation where

import Prelude
import Control.Applicative ((<$>))
import Yesod
import Yesod.Static
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Network.Mail.Mime
import Network.Mail.Mime.SES
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import Data.Monoid
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import System.Log.FastLogger (Logger)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S8
import System.Random
import qualified Data.Text.Encoding as DTE
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Crypto.PasswordStore as PS
import Web.Cookie
import Data.Maybe
import Text.Shakespeare.Text (textFile, renderTextUrl)
import Text.Hamlet (shamletFile, hamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Monad (join)
import Control.Applicative ((<*>))
import Data.Time.Clock
import Data.Digest.Pure.MD5
import Data.String


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    , moduleRoot :: FilePath
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  approot = ApprootMaster $ appRoot . settings

  -- Store session data on the client in encrypted cookies, default
  -- session idle timeout is 120 minutes
  makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
                         (120 * 60) -- 120 minutes
                         "config/client_session_key.aes"


  defaultLayout widget = do
    current <- getCurrentRoute
    mmsg <- getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
      $(combineStylesheets 'StaticR
        [ css_normalize_css
        , css_bootstrap_min_css
        , css_angular_ui_min_css
        , css_radian_css
        , css_app_css ])
      $(combineScripts 'StaticR
        [ js_jquery_min_js
        , js_bootstrap_js
        , js_d3_v2_js
        , js_estraverse_js
        , js_escodegen_browser_js
        , js_mersenne_twister_js
        , js_numeric_1_2_4_min_js
        , js_sprintf_min_js
        , js_angular_min_js
        , js_angular_ui_min_js
        , js_angular_ui_router_min_js
        , js_ui_bootstrap_tpls_0_6_0_min_js ])
      setTitle "Prototype Survey Server"
      case current of
        Just (AuthR authpg) -> case authpg of
          LoginR -> special mmsg [whamlet|
  <div .row>
    <div .span4 .offset4>
      <div .well>
        ^{widget}
|]
          _ -> special mmsg widget
        _ -> widget
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
      where special mmsg wwrap = [whamlet|
  <div .navbar>
    <div .navbar-inner>
      <a .brand href=@{HomeR}>openPsych

  <div.container>
    <div.row>
      <div.span12>
        $maybe msg <- mmsg
          ^{msg}

        ^{wwrap}
|]

  -- This is done to provide an optimization for serving static files
  -- from a separate domain. Please see the staticRoot setting in
  -- Settings.hs
  urlRenderOverride y (StaticR s) =
    Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $
    renderRoute s
  urlRenderOverride _ _ = Nothing

  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent = addStaticContentExternal mini
    genFileName Settings.staticDir (StaticR . flip StaticRoute [])
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs
        | development = "autogen-" ++ base64md5 lbs
        | otherwise   = base64md5 lbs
      mini = if development then (\bs -> Right bs) else minifym

  -- Place Javascript at bottom of the body tag so the rest of the
  -- page loads first
  jsLoader _ = BottomOfBody

  isAuthorized route rd = checkAuthorization route rd

  -- What messages should be logged. The following includes all
  -- messages when in development, and warnings and errors in
  -- production.
  shouldLog _ _source level =
    development || level == LevelWarn || level == LevelError

  makeLogger = return . appLogger


checkAuthorization :: Route App -> Bool -> Handler AuthResult
checkAuthorization (StaticR _)           False = loggedIn True
checkAuthorization (AuthR _)             _     = loggedIn True
checkAuthorization FaviconR              False = loggedIn True
checkAuthorization RobotsR               False = loggedIn True

checkAuthorization HomeR                 False = loggedIn False

checkAuthorization ModulesR              False = loggedIn False
checkAuthorization ModuleNewR            _     = loggedIn False
checkAuthorization (ModuleEditR _)       _     = loggedIn False
checkAuthorization (ModuleDeleteR _)     True  = loggedIn False
checkAuthorization (ModuleViewR _)       False = loggedIn False
checkAuthorization (ModuleScheduleR _)   True  = loggedIn False

checkAuthorization (SurveyRunR _)        _     = loggedIn False
checkAuthorization (SurveyResultsR _)    False = loggedIn False
checkAuthorization (SurveyDeleteR _)     True  = loggedIn False

checkAuthorization UserAdminR            False = adminUser
checkAuthorization (DeleteUserR _)       True  = adminUser
checkAuthorization (ModifyUserR _ _)     True  = adminUser

checkAuthorization _ _ = return $ Unauthorized "Unknown route!"

loggedIn :: Bool -> Handler AuthResult
loggedIn goAhead = do
  mu <- maybeAuthId
  return $ case mu of
    Nothing -> if goAhead then Authorized else AuthenticationRequired
    Just _ -> Authorized

adminUser :: Handler AuthResult
adminUser = do
  uid <- requireAuthId
  admin <- runDB $ maybe False userIsAdmin <$> get uid
  return $ if admin
           then Authorized
           else Unauthorized "Must be administrator"


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool


instance YesodAuth App where
  type AuthId App = UserId

  loginDest _ = HomeR
  logoutDest _ = HomeR

  -- Find UserId for given email address.
  getAuthId creds = runDB $ do
    x <- insertBy $
         User (credsIdent creds) Nothing False Nothing
    return $ Just $
      case x of
        Left (Entity uid _) -> uid -- newly added user
        Right uid -> uid           -- existing user

  authPlugins _ = [authEmail]

  authHttpManager = httpManager

  loginHandler = lift $ defaultLayout $ $(widgetFile "auth/login-form")

  onLogin = setAlertI OK Msg.NowLoggedIn
  onLogout = deleteRMCookie



-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email


data AlertType = Error | Warn | Info | OK

setAlertI :: RenderMessage App msg => AlertType -> msg -> Handler ()
setAlertI typ msg = do
  mr <- getMessageRender
  setAlert typ $ mr msg

setAlert :: AlertType -> Text -> Handler ()
setAlert typ msg = do
  let cls = ("alert" :: Text) <> case typ of
        Error -> " alert-error"
        Warn  -> ""
        Info  -> " alert-info"
        OK    -> " alert-success"
  htmlmsg <- giveUrlRenderer [hamlet|
  <div class="#{cls}">
    <button type="button" .close data-dismiss="alert">
      &times;
    #{msg}
  |]
  setMessage htmlmsg




--------------------------------------------------------------------------------
--
--   ALL OF THE EMAIL-SPECIFIC CODE
--
--------------------------------------------------------------------------------

type Email = Text
type ResetKey = Text
type ResetUrl = Text
type SaltedPass = Text
type VerStatus = Bool

-- | Data stored in a database for each e-mail address.
data EmailCreds m = EmailCreds
  { emailCredsId :: AuthEmailId m
  , emailCredsAuthId :: Maybe (AuthId m)
  , emailCredsResetkey :: Maybe ResetKey }

class (YesodAuth m, PathPiece (AuthEmailId m)) => YesodAuthEmail m where
  type AuthEmailId m

  addEmail :: Email -> HandlerT m IO (AuthEmailId m)
  sendResetEmail :: Email -> ResetUrl -> HandlerT m IO ()
  getResetKey :: AuthEmailId m -> HandlerT m IO (Maybe ResetKey)
  setResetKey :: AuthEmailId m -> ResetKey -> HandlerT m IO ()
  clearResetKey :: AuthEmailId m -> HandlerT m IO ()
  getPassword :: AuthId m -> HandlerT m IO (Maybe SaltedPass)
  setPassword :: AuthId m -> SaltedPass -> HandlerT m IO ()
  getEmailCreds :: Email -> HandlerT m IO (Either Text (Maybe (EmailCreds m)))
  getEmail :: AuthEmailId m -> HandlerT m IO (Maybe Email)

-- | Generate a random alphanumeric string.
randomKey :: m -> IO Text
randomKey _ = newStdGen >>= return . TS.pack . fst . randomString 10

sendEmail :: Text -> (Text, Text) -> Text -> Maybe T.Text
          -> Maybe BL.ByteString -> Handler ()
sendEmail emailTo (emailFromName, emailFromAddr) subj mtxt mhtml = do
  manager <- httpManager <$> getYesod
  access <- extraAwsAccess <$> getExtra
  secret <- extraAwsSecret <$> getExtra
  let ses = SES
               { sesFrom = S8.pack $ TS.unpack emailFromAddr
               , sesTo = [DTE.encodeUtf8 emailTo]
               , sesAccessKey = access
               , sesSecretKey = secret
               }
  renderSendMailSES (manager) ses
    (emptyMail $ Address (Just emailFromName) emailFromAddr)
      { mailTo = [Address Nothing emailTo]
      , mailHeaders = [ ("Subject", subj) ]
      , mailParts = [catMaybes [mt, mh]] }
  where mt = flip fmap mtxt $
             \txt -> Part { partType = "text/plain; charset=utf-8"
                          , partEncoding = None, partFilename = Nothing
                          , partHeaders = [], partContent = encodeUtf8 txt }
        mh = flip fmap mhtml $
             \html -> Part { partType = "text/html; charset=utf-8"
                           , partEncoding = None, partFilename = Nothing
                           , partHeaders = [], partContent = html }

instance YesodAuthEmail App where
  type AuthEmailId App = UserId

  addEmail email = runDB $ insert $ User email Nothing False Nothing

  sendResetEmail email url = do
    let txt = renderTextUrl undefined
              $(textFile "templates/auth/reset-email.txt")
    let html = renderHtml $(shamletFile "templates/auth/reset-email.hamlet")
    sendEmail email ("oP password reset", "verify@bayeshive.com")
      "openPsych: password reset" (Just txt) (Just html)

  getResetKey = runDB . fmap (join . fmap userResetkey) . get
  setResetKey uid key = runDB $ update uid [UserResetkey =. Just key]
  clearResetKey uid = do
    liftIO $ putStrLn $ "Clear password reset key for " ++ show uid
    runDB $ update uid [UserResetkey =. Nothing]
  getPassword = runDB . fmap (join . fmap userPassword) . get
  setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
  getEmailCreds email = runDB $ do
    mu <- getBy $ UniqueUser email
    return $ case mu of
      Nothing -> Right Nothing
      Just (Entity uid u) ->
        Right $ Just EmailCreds { emailCredsId = uid
                                , emailCredsAuthId = Just uid
                                , emailCredsResetkey = userResetkey u }
  getEmail = runDB . fmap (fmap userEmail) . get


loginR, registerR, setPasswordR :: AuthRoute
resetPasswordR, newPasswordR, clearAllRemR :: AuthRoute
loginR = PluginR "email" ["login"]
registerR = PluginR "email" ["register"]
setPasswordR = PluginR "email" ["set-password"]
resetPasswordR = PluginR "email" ["reset-password"]
newPasswordR = PluginR "email" ["new-password"]
clearAllRemR = PluginR "email" ["clear-remember"]

resetPassword :: Text -> Text -> AuthRoute
resetPassword eid resetkey = PluginR "email" ["password-reset", eid, resetkey]

authEmail :: AuthPlugin App
authEmail = AuthPlugin "email" dispatch $
            \tm -> do
              let loginForm = $(widgetFile "auth/login-form")
              [whamlet|
$newline never
<div .well>
  ^{loginForm}
              |]
  where dispatch "GET"  ["register"]       = getRegisterR >>= sendResponse
        dispatch "POST" ["register"]       = postRegisterR >>= sendResponse
        dispatch "POST" ["login"]          = postLoginR >>= sendResponse
        dispatch "GET"  ["set-password"]   = getPasswordR >>= sendResponse
        dispatch "POST" ["set-password"]   = postPasswordR >>= sendResponse
        dispatch "GET"  ["reset-password"] = getResetPasswordR >>= sendResponse
        dispatch "POST" ["reset-password"] = postResetPasswordR >>= sendResponse
        dispatch "GET"  ["password-reset", eid, resetkey] =
          case fromPathPiece eid of
            Nothing -> notFound
            Just eid' -> getPasswordResetR eid' resetkey >>= sendResponse
        dispatch "POST" ["new-password"] = postNewPasswordR >>= sendResponse
        dispatch "POST" ["clear-remember"] = postClearRememberR >>= sendResponse
        dispatch _ _ = notFound


getResetPasswordR ::  AuthHandler App Html
getResetPasswordR = lift $ defaultLayout $(widgetFile "auth/reset-password")

postResetPasswordR :: AuthHandler App Html
postResetPasswordR = do
  y <- lift getYesod
  tm <- getRouteToParent
  email <- lift $ runInputPost $ ireq emailField "email"
  mecreds <- lift $ getEmailCreds email
  -- A little tricky.  If the given email isn't registered, we just
  -- redirect to Home.  If the email is registered but unverified, we
  -- just send a "verify your email address" message.  Only if the the
  -- email is registered and verified do we do the password reset.
  let mstuff = case mecreds of -- (lid, mresetkey)
        Left _                                   -> Nothing
        Right (Just (EmailCreds lid _ resetkey)) -> Just (lid, resetkey)
        Right Nothing                            -> Nothing
  case mstuff of
    -- Email address not registered with a password.
    Nothing -> lift $ redirect HomeR
    Just (lid, mresetkey) -> do
      resetkey <- case mresetkey of
        Nothing -> do
          -- Generate new reset key.
          key <- liftIO $ randomKey y
          lift $ setResetKey lid key
          return key
        Just key -> return key
      render <- lift getUrlRender
      let reseturl = render $ tm $ resetPassword (toPathPiece lid) resetkey
      lift $ sendResetEmail email reseturl
      lift $ redirect HomeR


getRegisterR ::  AuthHandler App Html
getRegisterR = lift $ defaultLayout $(widgetFile "auth/register")

postRegisterR :: AuthHandler App Html
postRegisterR = do
  (email, pass) <- lift $ runInputPost $ (,)
    <$> ireq emailField "email"
    <*> ireq textField "password"
  mecreds <- lift $ getEmailCreds email
  case mecreds of
    Left _ -> alreadyRegistered
    Right _ -> do
      lid <- lift $ addEmail email
      salted <- liftIO $ saltPass pass
      lift $ setPassword lid salted
  lift $ setCreds True $ Creds "email" email []
  lift $ redirect HomeR
    where alreadyRegistered = do
            tm <- getRouteToParent
            lift $ setAlert Error "Email address already registered"
            lift $ redirect $ tm registerR

getPasswordResetR :: AuthEmailId App -> Text
                  -> HandlerT Auth (HandlerT App IO) Html
getPasswordResetR lid key = lift $ do
  memail <- getEmail lid
  realKey <- getResetKey lid
  case (realKey == Just key, memail) of
    (True, Just email) -> do
      -- The user is not logged in at this point, so we need a secure
      -- way to chain the user ID and reset key along through the form
      -- for entering a new password.
      setSession "reset-email" email
      setSession "reset-key" key
      defaultLayout $ do
        setTitle "Enter new password"
        $(widgetFile "auth/new-password")
    _ -> defaultLayout $ do
      setTitle "Invalid password reset key"
      let keytype = "password reset" :: Text
      $(widgetFile "auth/invalid-key")

postNewPasswordR :: AuthHandler App Html
postNewPasswordR = lift $ do
  newpass <- runInputPost $ ireq textField "new"
  memail <- lookupSession "reset-email"
  deleteSession "reset-email"
  mkey <- lookupSession "reset-key"
  deleteSession "reset-key"
  case memail of
    Just email -> do
      muid <- runDB $ selectFirst [UserEmail ==. email, UserResetkey ==. mkey] []
      case muid of
        Just (Entity uid _) -> do
          clearResetKey uid
          salted <- liftIO $ saltPass newpass
          setPassword uid salted
          setCreds True $ Creds "email" email []
        _ -> return ()
    _ -> return ()
  defaultLayout $ do
    setTitle "Invalid password reset key"
    let keytype = "password reset" :: Text
    $(widgetFile "auth/invalid-key")

postLoginR :: AuthHandler App ()
postLoginR = do
  (email, pass, rm) <- lift $ runInputPost $ (,,)
    <$> ireq emailField "email"
    <*> ireq textField "password"
    <*> iopt boolField "remember_me"
  creds <- lift $ getEmailCreds email
  tm <- getRouteToParent
  case creds of
    Left _ -> lift $ invalid tm
    Right ecs -> lift $ valid tm (email, pass, rm) ecs
  where valid tm _                 Nothing = invalid tm
        valid tm (email, pass, rm) (Just (EmailCreds uid _ _)) = do
          passGood <- do
            mrealpass <- getPassword uid
            case mrealpass of
              Nothing -> return False
              Just realpass ->
                if isValidPass pass realpass
                then return True
                else return False
          if not passGood
            then invalid tm
            else do
            case rm of
              Just True -> newRMCookie email Nothing
              _ -> return ()
            clearResetKey uid
            setCreds True $ Creds "email" email []
        invalid tm = do
          setAlertI Error Msg.InvalidEmailPass
          redirect $ tm LoginR

getPasswordR :: AuthHandler App Html
getPasswordR = do
  tm <- getRouteToParent
  muid <- lift $ maybeAuthId
  case muid of
    Just uid -> lift $ do
      Just email <- getEmail uid
      defaultLayout $(widgetFile "auth/set-password")
    Nothing -> lift $ do
      setMessageI Msg.BadSetPass
      redirect $ tm LoginR

postPasswordR :: AuthHandler App ()
postPasswordR = do
  (current, new) <- lift $ runInputPost $ (,)
    <$> ireq textField "current"
    <*> ireq textField "new"
  tm <- getRouteToParent
  y <- lift getYesod
  muid <- lift $ maybeAuthId
  ok <- case muid of
    Nothing -> return False
    Just uid -> do
      mrealpass <- lift $ getPassword uid
      case mrealpass of
        Nothing -> return False
        Just realpass -> if isValidPass current realpass
                         then return True
                         else return False
  case ok of
    True -> lift $ do
      salted <- liftIO $ saltPass new
      setPassword (fromJust muid) salted
      setAlertI OK Msg.PassUpdated
      redirect $ loginDest y
    False -> lift $ do
      setAlertI Error Msg.BadSetPass
      redirect $ tm LoginR

postClearRememberR :: AuthHandler App RepJson
postClearRememberR = do
  muid <- lift $ maybeAuthId
  resp <- case muid of
    Just uid -> do
      Just email <- lift $ getEmail uid
      lift $ deleteAllRMCookies email
      return $ Right ("\"Remember me\" cookies deleted" :: Text)
    Nothing -> return $ Left ("Operation not permitted!" :: Text)
  return $ repJson $ toJSON resp

saltLength :: Int
saltLength = 5

-- | Salt a password with a randomly generated salt.
saltPass :: Text -> IO Text
saltPass = fmap DTE.decodeUtf8 . flip PS.makePassword 12 . DTE.encodeUtf8

saltPass' :: String -> String -> String
saltPass' salt pass = salt ++ show (md5 $ fromStr $ salt ++ pass)
  where fromStr = encodeUtf8 . T.pack

isValidPass :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass ct salted = PS.verifyPassword (DTE.encodeUtf8 ct) (DTE.encodeUtf8 salted)
                        || isValidPass' ct salted

isValidPass' :: Text -- ^ cleartext password
            -> SaltedPass -- ^ salted password
            -> Bool
isValidPass' clear' salted' = let salt = take saltLength salted
                              in salted == saltPass' salt clear
                                where clear = TS.unpack clear'
                                      salted = TS.unpack salted'


-- Delete current "remember me" cookie.

deleteRMCookie :: Handler ()
deleteRMCookie = deleteCookie "remember" "/"


-- Delete all "remember me" cookies for a given user email.

deleteAllRMCookies :: Text -> Handler ()
deleteAllRMCookies uid = do
  runDB $ deleteWhere [RMCookieEmail ==. uid]
  deleteCookie "remember" "/"


-- Convert "remember me" cookies to and from text.

renderRMCookie :: RMCookie -> Text
renderRMCookie (RMCookie u s t) = TS.concat [s, ":", t, ":", u]

parseRMCookie :: Text -> RMCookie
parseRMCookie txt = RMCookie u s t
  where txts = TS.splitOn ":" txt
        s = txts !! 0
        t = txts !! 1
        u = TS.intercalate ":" $ drop 2 txts


-- Create a new "remember me" cookie for a given user email, either
-- reusing an existing series or creating a new one.

newRMCookie :: Text -> Maybe Text -> Handler ()
newRMCookie uid mseries = do
  y <- getYesod
  series <- case mseries of
    Just s -> return s
    Nothing -> liftIO $ randomKey y
  token <- liftIO $ randomKey y
  let cookie = RMCookie uid series token
  runDB $ insert_ $ cookie
  now <- liftIO getCurrentTime
  -- NEED TO ENCRYPT THIS HERE?
  setCookie def { setCookieName = "remember"
                , setCookieValue =
                  fromString . TS.unpack . renderRMCookie $ cookie
                , setCookiePath = Just "/"
                , setCookieDomain = Nothing
                , setCookieExpires =
                  Just (fromIntegral (7 * 86400 :: Int) `addUTCTime` now)
                , setCookieHttpOnly = True }


-- Check a "remember me" cookie offered by the client.  If the cookie
-- is valid, remove from the database, create a new cookie and log the
-- user in.  If the cookie is not valid, delete all "remember me"
-- cookies for the user and go to the login page displaying an error
-- message.

data RMCookieResult = NoRMCookieAction | RMCookieValidated | RMCookieTheft

checkRMCookie :: Text -> Handler RMCookieResult
checkRMCookie txt = do
  let (RMCookie uid series token) = parseRMCookie txt
  cs <- runDB $ selectList [RMCookieEmail ==. uid,
                            RMCookieSeries ==. series,
                            RMCookieToken ==. token] []
  case cs of
    [] -> do
      -- Cookie triplet not found.  Need to check for cookie theft.
      thefts <- runDB $ selectList [RMCookieEmail ==. uid,
                                    RMCookieSeries ==. series] []
      case thefts of
        [] -> return NoRMCookieAction
        _ -> do
          -- There are cookie entries for this series that don't match
          -- the token passed.  This probably indicates cookie
          -- theft...
          deleteAllRMCookies uid
          return RMCookieTheft
    _ -> do
      -- Cookie triplet found: remove matching triples, generate new
      -- cookie from same series and return as validated.
      runDB $ mapM_ (delete . entityKey) cs
      newRMCookie uid (Just series)
      setCreds False $ Creds "email" uid []
      return RMCookieValidated
