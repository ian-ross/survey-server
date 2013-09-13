{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Angular.BasicRouter
    ( runAngularWith
    , runAngular
    , addCommand
    , addFixCommand
    , addCtrl
    , addDiscreteCtrl
    , injectModule
    , addSharedModule
    , setModuleName
    , setDefaultRoute
    , Angular
    , injectLibraryModule
    ) where

import Prelude
import Import
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (First (..))
import Text.Hamlet
import Text.Julius
import Text.Cassius (cassiusFile)
import Text.Lucius (luciusFile)
import Yesod.Static
import Language.Haskell.TH.Syntax (Q, Exp (AppE, LitE), Lit (StringL), qRunIO)
import qualified Data.Text as T
import Data.Char (isAlpha)
import System.Directory (doesFileExist)
import Data.Maybe

ngFile :: String -> Q Exp
ngFile = if development then juliusFileReload else juliusFile

urlAngularJs :: Route App
--urlAngularJs _ =
--  Right "//ajax.googleapis.com/ajax/libs/angularjs/1.0.2/angular.min.js"
urlAngularJs = StaticR $ StaticRoute ["js", "angular.min.js"] []

wrapAngular :: Text -> Widget -> Handler Html
wrapAngular modname widget =
  defaultLayout [whamlet|<div ng-app=#{modname}>^{widget}|]

data AngularWriter =
  AngularWriter
  { awCommands      :: Map Text (Handler ())
  , awPartials      :: Map Text (HtmlUrl (Route App))
  , awRoutes        :: JavascriptUrl (Route App)
  , awControllers   :: JavascriptUrl (Route App)
  , awDefaultRoute  :: First Text
  , awModuleName    :: First (Text, Maybe (CssUrl (Route App)))
  , awModules       :: Map Text (Bool, Maybe (JavascriptUrl (Route App)))
  }

instance Monoid AngularWriter where
  mempty = AngularWriter mempty mempty mempty mempty mempty mempty mempty
  AngularWriter a1 a2 a3 a4 a5 a6 a7
    `mappend` AngularWriter b1 b2 b3 b4 b5 b6 b7
        = AngularWriter (mappend a1 b1) (mappend a2 b2) (mappend a3 b3)
                        (mappend a4 b4) (mappend a5 b5) (mappend a6 b6)
                        (mappend a7 b7)

type Angular = WriterT AngularWriter Handler
type Wrapper = Text -> Widget -> Handler Html

runAngular :: Angular () -> Handler Html
runAngular = runAngularWith wrapAngular

runAngularWith :: Wrapper -> Angular () -> Handler Html
runAngularWith wrap ga = do
    ((), AngularWriter{..}) <- runWriterT ga
    modnametmp <- newIdent
    let (modname, mcss) =
          case awModuleName of
            First (Just (x, y)) -> (x, y)
            First Nothing -> (modnametmp, Nothing)

    mc <- lookupGetParam "command"
    fromMaybe (return ()) $ mc >>= flip Map.lookup awCommands
    mp <- lookupGetParam "partial"
    case mp >>= flip Map.lookup awPartials of
        Nothing -> return ()
        Just h -> getUrlRenderParams >>= sendResponse . toTypedContent . h

    let defaultRoute =
            case awDefaultRoute of
                First (Just x) -> [julius|.otherwise({redirectTo:#{toJSON x}})|]
                First Nothing -> mempty

    let injectModules = Map.keys (Map.filter fst awModules)

    wrap modname $ do
        addScript urlAngularJs
        addScript $ StaticR js_angular_ui_min_js
        addStylesheet $ StaticR css_angular_ui_min_css
        mapM_ toWidget $ catMaybes $ map snd $ Map.elems awModules
        [whamlet|<div ng-view>|]
        case mcss of
          Nothing -> return ()
          Just x -> toWidget x
        toWidget [julius|
angular
    .module(#{toJSON modname}, #{toJSON injectModules})
    .config(["$routeProvider", function($routeProvider) {
        $routeProvider ^{awRoutes} ^{defaultRoute} ;
    }]);
^{awControllers}
|]

addCommand :: (FromJSON input, ToJSON output)
           => (input -> Handler output) -> Angular Text
addCommand f = do
    name <- lift newIdent
    tell mempty { awCommands = Map.singleton name handler }
    return $ "?command=" <> name
  where handler = parseJsonBody_ >>= f >>= returnJson >>= sendResponse


addFixCommand :: ToJSON output => Handler output -> Angular Text
addFixCommand f = do
    name <- lift newIdent
    tell mempty { awCommands = Map.singleton name handler }
    return $ "?command=" <> name
  where handler = f >>= returnJson >>= sendResponse


addCtrl :: Text -- ^ module name
        -> Text -- ^ route pattern
        -> Text -- ^ template name
        -> Q Exp
addCtrl modnm route name = do
    let name' = T.filter isAlpha name
    [|addCtrlRaw $(liftT name') $(liftT route)
      $(hamletFile $ fn "hamlet") $(ngFile $ fn "julius")|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/components/", modnm,
                                     "/partials/", name, ".", suffix]

addCtrlRaw :: Text -- ^ user-friendly name
           -> Text -- ^ route pattern
           -> HtmlUrl (Route App) -- ^ template
           -> JavascriptUrl (Route App) -- ^ controller
           -> Angular ()
addCtrlRaw name' route template controller = do
    name <- (mappend $ mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials = Map.singleton name template
        , awRoutes =
          [julius|.when(#{toJSON route},
                        { controller:#{toJSON name},
                          templateUrl:"?partial=#{rawJS name}"})|]
        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        }

addDiscreteCtrl :: Text -- ^ module name
        -> Text -- ^ template name
        -> Q Exp
addDiscreteCtrl modnm name = do
    [|addDiscreteCtrlRaw $(liftT name)  $(ngFile $ fn "julius")|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/components/", modnm,
                                     "/", name, ".", suffix]

addDiscreteCtrlRaw :: Text -- ^ user-friendly name
           -> JavascriptUrl (Route App) -- ^ controller
           -> Angular ()
addDiscreteCtrlRaw name controller = do
    tell mempty
        { awControllers = [julius|var #{rawJS name} = ^{controller};|]
        }

setDefaultRoute :: Text -> Angular ()
setDefaultRoute x = tell mempty { awDefaultRoute = First $ Just x }

setModuleName :: Text -> Q Exp
setModuleName modnm = do
  casex <- qRunIO . doesFileExist . fn $ "cassius"
  lucex <- qRunIO . doesFileExist . fn $ "lucius"
  case (casex, lucex) of
    (True, _) -> [|setModuleNameRaw $(liftT modnm)
                                    (Just $(cassiusFile $ fn "cassius"))|]
    (_, True) -> [|setModuleNameRaw $(liftT modnm)
                                    (Just $(luciusFile $ fn "lucius"))|]
    (_, _)    -> [|setModuleNameRaw $(liftT modnm) Nothing|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/components/", modnm,
                                     "/", modnm, ".", suffix]

setModuleNameRaw :: Text -> Maybe (CssUrl (Route App)) -> Angular ()
setModuleNameRaw x mcss = tell mempty { awModuleName = First $ Just (x, mcss) }

injectModule :: Text -- ^ module name
             -> Text -- ^ import module name
             -> Q Exp
injectModule modnm impnm = [|addModuleRaw $(liftT impnm') True $(ngFile f)|]
  where
    impnm' = modnm `T.append` "." `T.append` impnm
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    f = T.unpack $ T.concat ["angular/components/", modnm, "/",
                             impnm, ".julius"]


addSharedModule :: Text -- ^ module name
                -> Bool -- ^ inject?
                -> Q Exp
addSharedModule impnm inject =
  [|addModuleRaw $(liftT impnm) inject $(ngFile f)|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    f = T.unpack $ T.concat ["angular/shared/", impnm, ".julius"]

injectLibraryModule :: Text -> Angular ()
injectLibraryModule impnm =
  tell mempty { awModules = Map.singleton impnm (True, Nothing) }


addModuleRaw :: Text -- ^ import module name
             -> Bool -- ^ inject?
             -> JavascriptUrl (Route App) -- ^ module contents
             -> Angular ()
addModuleRaw impnm inject source =
  tell mempty { awModules = Map.singleton impnm
                            (inject, Just [julius|^{source};|]) }
