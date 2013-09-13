{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
-- | Yesod/Angular state-based routing using the @ui-router@ module.
--
-- How this works:
--
--  * A UI description is built up within the 'AngularUI' monad, using
--    the functions 'addCommand' and 'addFixedCommand' to define
--    commands that can be called from client-side code to the server,
--    using 'addSharedModule' and 'injectModule' to provide access to
--    external JavaScript modules, and finally using 'buildStateUI' to
--    process the files defining the state-based UI to generate
--    routing code.
--
--  * Once the UI is constructed, it can be run from within a Yesod
--    handler using either the 'runAngularUI' or 'runAngularUIWith'
--    functions.
--
--  * The heart of this setup is the 'buildStateUI' function.  This is
--    a Template Haskell function that constructs Angular @ui-router@
--    state definitions based on the contents of a directory under the
--    @angular/ui-router@ directory.  The top-level directory is named
--    after the module name supplied to 'buildStateUI'.  For a module
--    named @mod@, this directory should contain the following items:
--
--    [@mod.hamlet@] Outer Hamlet template for module.  This should
--                   contain an HTML element with an @ng-controller@
--                   directive accessing the controller defined in the
--                   @mod.julius@ file, and should contain a single
--                   @ui-view@ element to contain the state-based
--                   content.
--
--    [@mod.julius@] Controller file for top-level page.  This should
--                   define a controller with the name used in the
--                   @mod.hamlet@ file's @ng-controller@ directive.
--
--    [@mod.lucius@] Optional top-level stylesheet definitions.
--
--    [@states@] Directory containing the state definitions, as
--               defined below.
--
--  * The @states@ directory in the per-module directory under
--    @angular/ui-router@ contains a hierarchical representation of
--    the states implemented by the UI.  Each state has a name made up
--    of a list of period-separated components (e.g. "folder",
--    "doc.view", "build.dynsys.step1", etc.).  The template files for
--    each state are found in a directory path made from the elements
--    of the state name.  For instance, for a state "folder" in a
--    module "test", you should put @folder.hamlet@ and
--    @folder.julius@ in
--    @angular/ui-router/test/states/folder.hamlet@, etc.  For a state
--    "doc.view" in the same module, you would have files
--    @angular/ui-router/test/states/doc/view.hamlet@, etc.
--
--  * For each state, you must provide a Hamlet file (the Hamlet files
--    are how the 'buildStateUI' function discovers the states that
--    are required).  You can also optionally provide a Julius file.
--
--  * States can have parameters that are passed to the state
--    controller on transition into the given state.  Parameters are
--    given names taken from a JavaScript comment line at the
--    beginning of the state's Julius file, of the form "// params: a
--    b c", i.e. a space separated list of state parameters introduced
--    by the string "params:".
--
--  * Some states are abstract, i.e. they exist only to provide a
--    framework for sub-states.  When a sub-state of an abstract state
--    is active, the parent abstract state is implicitly also active.
--    Here, abstract states are identified by a comment at the top of
--    their Julius file of the form "// abstract: true".  All states
--    not specifically marked as abstract are assumed to be
--    non-abstract.  Both abstract and non-abstract states may have
--    sub-states, indicated by the presence of a directory with the
--    same name of the state which contains the definitions for the
--    sub-states.  For example, if there are states "doc", "doc.view",
--    "doc.edit" and "doc.options" in module "test", the following
--    files may exist: @.../doc.hamlet@, @.../doc.julius@,
--    @.../doc/view.hamlet@, @.../doc/view.julius@,
--    @.../doc/edit.hamlet@, @.../doc/edit.julius@,
--    @.../doc/options.hamlet@, @.../doc/options.julius@.  In this
--    case, the "doc" state may or may not be abstract: in either
--    case, the template in @doc.hamlet@ provides a wrapper around the
--    templates of the sub-states, and controller code defined in
--    @doc.julius@ is shared between the sub-states.  Non-abstract
--    states with sub-states are useful for implementing the case
--    where some action is required to determine which of the
--    sub-states should be entered at transition time: the code to
--    determine the appropriate sub-state can live in the parent state
--    controller and can trigger a transition to the appropriate
--    sub-state once the initial transition processing is done.
--
--  * States with names like "modal.tag", etc. are treated specially,
--    and are assumed to define modal dialogues that can be activated
--    from any other state.  [THIS ISN'T IMPLEMENTED YET.]
--
module Angular.UIRouter
    ( runAngularUIWith, runAngularUI
    , addCommand, addFixCommand
    , buildStateUI
    , addRedirection, addDefaultRedirection
    , addSharedModule, injectLibraryModule
    , AngularUI
    ) where

import Prelude
import Import
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Map as M
import Data.Monoid
import Text.Hamlet
import Text.Julius
import Text.Cassius (cassiusFile)
import Text.Lucius (luciusFile)
import Yesod.Json ()
import Language.Haskell.TH.Syntax
  (Q, Exp (AppE, LitE, DoE), Stmt (NoBindS), Lit (StringL), qRunIO)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO
import Data.List
import Data.Maybe
import Data.Char

-- | A single state, as determined by scanning the
-- angular/ui-router/<module>/states directory tree.
--
data State = State
             { stName :: String
               -- ^ State name.
             , stHamlet :: FilePath
               -- ^ Path to Hamlet file (mandatory).
             , stJulius :: Maybe FilePath
               -- ^ Path to optional Julius file.
             , stUrl :: Maybe String
               -- ^ State URL (derived from url lines in Julius file).
             , stParams :: [String]
               -- ^ State parameters (derived from params line in Julius file).
             , stAbstract :: Bool
               -- ^ Is this state abstract?
             } deriving (Eq, Show)


-- | A single modal dialogue, as determined by scanning the
-- angular/ui-router/<module>/modal directory tree.
--
data Modal = Modal
             { moName :: String
               -- ^ Modal name.
             , moHamlet :: FilePath
               -- ^ Path to Hamlet file.
             , moJulius :: Maybe FilePath
               -- ^ Path to optional Julius file.
             } deriving (Eq, Show)


-- | Partial page type alias (Hamlet).
--
type Partial = HtmlUrl (Route App)


-- | CSS stylesheet type alias (Cassius or Lucius).
--
type Stylesheet = CssUrl (Route App)


-- | Javascript type alias (Julius).
--
type Script = JavascriptUrl (Route App)


-- | Writer type for AngularUI monad.
--
data AngularUIWriter = AngularUIWriter
       { awCommands    :: M.Map Text (Handler ())
         -- ^ Command definitions.
       , awStates      :: M.Map Text Partial
         -- ^ Router states: map from state names to partials.
       , awModals      :: M.Map Text Partial
         -- ^ Modal dialogues: map from modal names to partials.
       , awStateDefs   :: Script
         -- ^ Router state definitions.
       , awRedirDefs   :: Script
         -- ^ State redirection definitions.
       , awModalDefs   :: [Script]
         -- ^ Modal dialogues: map from modal names to partials.
       , awControllers :: Script
         -- ^ State controller definitions.
       , awModule      :: First (Text, Partial, Maybe Stylesheet, Script)
         -- ^ Top-level module information: module name, main page
         -- wrapper, stylesheet and main controller script.
       , awMods        :: M.Map Text (Bool, Maybe Script)
         -- ^ Imported modules: map from module names to injection
         -- flag and script contents.
       }


-- | Monoid instance for writer type to enable composition of router
-- setup.
--
instance Monoid AngularUIWriter where
  mempty = AngularUIWriter mempty mempty mempty mempty
                           mempty mempty mempty mempty mempty
  mappend (AngularUIWriter a1 a2 a3 a4 a5 a6 a7 a8 a9)
    (AngularUIWriter b1 b2 b3 b4 b5 b6 b7 b8 b9)
        = AngularUIWriter (a1 <> b1) (a2 <> b2) (a3 <> b3)
          (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9)


-- | Main router monad type: collects information in AngularUIWriter
-- for processing by runAngularUIWith.
--
type AngularUI = WriterT AngularUIWriter Handler


-- | Outer widget wrapper type alias.
--
type Wrapper = Text -> Widget -> Handler Html


-- | Run Angular ui-router UI with default wrapper.
--
runAngularUI :: AngularUI () -> Handler Html
runAngularUI = runAngularUIWith defaultWrapper
  where defaultWrapper modname widget =
          defaultLayout [whamlet|<div ng-app=#{modname}>^{widget}|]


-- | Run Angular ui-router UI with user-supplied wrapper.
--
runAngularUIWith :: Wrapper -> AngularUI () -> Handler Html
runAngularUIWith wrap ga = do
  -- Process UI definition.
  ((), AngularUIWriter{..}) <- runWriterT ga

  -- Extract top-level module information: module name, outer HTML,
  -- stylesheet, top-level controller script.
  mnmtmp <- newIdent
  let (mnm, mham, mmcss, mjs) = case awModule of
        First (Just (n, h, c, j)) -> (n, h, c, j)
        First Nothing -> (mnmtmp, [hamlet| |], Nothing, [julius| |])

  -- Command processing.
  mc <- lookupGetParam "command"
  fromMaybe (return ()) $ mc >>= flip M.lookup awCommands

  -- State-based partial page processing.
  ms <- lookupGetParam "state"
  case ms >>= flip M.lookup awStates of
    Nothing -> return ()
    Just h -> getUrlRenderParams >>= sendResponse . toTypedContent . h

  -- Modal dialogue partial page processing.
  mm <- lookupGetParam "modal"
  case mm >>= flip M.lookup awModals of
    Nothing -> return ()
    Just h -> getUrlRenderParams >>= sendResponse . toTypedContent . h

  let extraModules = "ui.router" :
                     if null awModalDefs then [] else ["ui.bootstrap"]
      injectModules = extraModules ++ M.keys (M.filter fst awMods)
      modalDefs = mconcat $ intersperse [julius|,|] awModalDefs

  wrap mnm $ do
    mapM_ toWidget $ catMaybes $ map snd $ M.elems awMods
    toWidget mjs
    toWidget mham
    case mmcss of
      Nothing -> return ()
      Just x -> toWidget x
    toWidget [julius|
angular.module(#{toJSON mnm}, #{toJSON injectModules})
  .config(['$stateProvider', '$urlRouterProvider',
           function($stateProvider, $urlRouterProvider)
  {
    $urlRouterProvider^{awRedirDefs};
    $stateProvider^{awStateDefs};
  }])
  .constant('uiModals', { ^{modalDefs} });
^{awControllers}
|]


-- | Add Angular command: JSON -> JSON.
--
addCommand :: (FromJSON i, ToJSON o) => (i -> Handler o) -> AngularUI Text
addCommand f = do
  name <- lift newIdent
  tell mempty { awCommands = M.singleton name handler }
  return $ "?command=" <> name
  where handler = parseJsonBody_ >>= f >>= returnJson >>= sendResponse

-- | Add Angular command: () -> JSON.
--
addFixCommand :: ToJSON o => Handler o -> AngularUI Text
addFixCommand f = do
  name <- lift newIdent
  tell mempty { awCommands = M.singleton name handler }
  return $ "?command=" <> name
  where handler = f >>= returnJson >>= sendResponse


buildStateUI :: Text -> Q Exp
buildStateUI tmodnm = do
  let modnm = T.unpack tmodnm
      basedir = "angular/ui-router/" </> modnm

  -- Extract states information from angular/ui-router/<module>
  -- directory hierarchy.
  states <- qRunIO $ extractStates $ basedir </> "states"

  -- Extract modals information from angular/ui-router/<module>
  -- directory hierarchy.
  modals <- qRunIO $ do
    let modaldir = basedir </> "modal"
    ex <- doesDirectoryExist modaldir
    case ex of
      False -> return []
      True -> extractModals modaldir

  -- Set up stuff for main page, script and stylesheet.
  setmodnm <- setModule tmodnm

  -- Make state definition Julius for each state.
  stdefs <- addStates tmodnm states

  -- Make modal definition Julius for each state.
  modefs <- addModals tmodnm modals

  -- Return "do" expression to set everything up.
  return $ DoE $ map NoBindS $ [setmodnm, stdefs, modefs]


-- | Extract state information from module ui-router directory.
--
extractStates :: FilePath -> IO [State]
extractStates stdir = do
  let nodots p = p /= "." && p /= ".."
      doone d = do
        cs <- (map (d </>)) <$> filter nodots <$> getDirectoryContents d
        let fget e = map dropExtension $ filter ((== e) . takeExtension) cs
            hamlets = fget ".hamlet"
            juliuss = fget ".julius"
            subdirs = filter (not . hasExtension) cs
        let makeState s = do
              (ab, ctrl, url, params) <-
                maybe (return (False, False, Nothing, [])) juliusConfigLines jul
              return $ State snm (addExtension s ".hamlet")
                             (if ctrl then jul else Nothing) url params ab
              where snm = intercalate "." $
                          splitDirectories $ makeRelative stdir s
                    jul = if s `elem` juliuss
                          then Just $ addExtension s ".julius"
                          else Nothing
        subs <- mapM doone subdirs
        these <- mapM makeState hamlets
        return $ these ++ concat subs
  ss <- doone stdir
  putStrLn $ stateTable ss
  return ss


-- | Extract modal information from module ui-router directory.
--
extractModals :: FilePath -> IO [Modal]
extractModals modir = do
  let nodots p = p /= "." && p /= ".."
  cs <- (map (modir </>)) <$> filter nodots <$> getDirectoryContents modir
  let fget e = map dropExtension $ filter ((== e) . takeExtension) cs
      hamlets = fget ".hamlet"
      juliuss = fget ".julius"
  let makeModal m = do
        return $ Modal mnm (addExtension m ".hamlet") jul
          where mnm = intercalate "." $
                      splitDirectories $ makeRelative modir m
                jul = if m `elem` juliuss
                      then Just $ addExtension m ".julius"
                      else Nothing
  ms <- mapM makeModal hamlets
  putStrLn $ modalTable ms
  return ms


-- | Extract comment lines from the head of a Julius file giving
-- information about state parameters, state URL, and
-- abstract/non-abstract status.
--
juliusConfigLines :: FilePath -> IO (Bool, Bool, Maybe String, [String])
juliusConfigLines j = do
  ex <- doesFileExist j
  if not ex
    then return (False, False, Nothing, [])
    else withFile j ReadMode $ go (False, True, Nothing, [])
  where go (ab, ctrl, url, ps) h = do
          l <- dropWhile (== ' ') <$> hGetLine h
          case l of
            '/' : '/' : c -> do
              let ws = words c
              case ws of
                "no-ctrl" : _ -> go (ab, False, url, ps) h
                "url:" : newurl : _ -> go (ab, ctrl, Just newurl, ps) h
                "params:" : newps  -> go (ab, ctrl, url, newps) h
                "abstract:" : abss -> case abss of
                  "true" : _ -> go (True, ctrl, url, ps) h
                  _          -> go (False, ctrl, url, ps) h
                _ -> go (ab, ctrl, url, ps) h
            _ -> return (ab, ctrl, url, ps)


-- | Produce table of processed states for debugging.
--
stateTable :: [State] -> String
stateTable ss = unlines $ header : slines
  where header = pad "State" ++ "Ctrl Pars Abs?"
        slines = map oneline ss
        padlen = 2 + (maximum $ map (length . stName) ss)
        pad s = s ++ replicate (padlen - length s) ' '
        oneline (State n _ mj _ ps ab) =
          pad n ++ "  " ++ j ++ "    " ++ p ++ "    " ++ a ++ "     "
          where j = case mj of
                  Nothing -> " "
                  Just _ -> "X"
                p = if null ps then " " else "X"
                a = if ab then "X" else " "


-- | Produce table of modals for debugging.
--
modalTable :: [Modal] -> String
modalTable ms = unlines $ header : slines
  where header = pad "Modal" ++ "Ctrl"
        slines = map oneline ms
        padlen = 2 + (maximum $ map (length . moName) ms)
        pad s = s ++ replicate (padlen - length s) ' '
        oneline (Modal n _ mj) =
          pad n ++ "  " ++ j
          where j = case mj of
                  Nothing -> " "
                  Just _ -> "X"


-- | Set up state definitions.
--
addStates :: Text     -- ^ module name
          -> [State]  -- ^ state definitions
          -> Q Exp
addStates modnm ss = mapM (addState modnm) ss >>= (return . DoE . map NoBindS)


-- | Set up single state definition.
--
addState :: Text   -- ^ module name
         -> State  -- ^ state definition
         -> Q Exp
addState modnm (State s ham mjs murl ps abst) = do
  case mjs of
    Nothing -> [|addStateRaw $(liftT s) $(hamletFile $ fn "hamlet")
                 Nothing murl ps abst|]
    Just _ -> [|addStateRaw $(liftT s) $(hamletFile $ fn "hamlet")
                (Just $(ngFile $ fn "julius")) murl ps abst|]
  where liftT t = [|T.pack|] >>= \p -> return $ AppE p $ LitE $ StringL $ t
        fn suf = T.unpack $ T.concat ["angular/ui-router/", modnm, "/states/",
                                      T.replace "." "/" (T.pack s), ".", suf]


-- | Raw function to set up single state definition for use in TH.
--
addStateRaw :: Text          -- ^ state name
            -> Partial       -- ^ template
            -> Maybe Script  -- ^ controller
            -> Maybe String  -- ^ state URL
            -> [String]      -- ^ state parameters
            -> Bool          -- ^ is state abstract?
            -> AngularUI ()
addStateRaw s ham mjs murl ps abst = do
  let c = controllerName s
      jp = rawJS $ case ps of
        [] -> ""
        _ -> T.pack $ ",params:['" ++ (intercalate "','" ps) ++ "']"
      ja = rawJS $ case abst of
        False -> "" :: Text
        True -> ",abstract:true"
      jurl = rawJS $ case murl of
        Nothing -> "" :: Text
        Just url -> T.pack $ ",url:'" ++ url ++ "'"
      (jc, ctrl) = case mjs of
        Nothing -> (mempty, mempty)
        Just j -> ([julius|,controller: #{toJSON c}|],
                   [julius|var #{rawJS c} = ^{j};|])
      opts = [julius|#{jurl}#{jp}^{jc}#{ja}|]
      sdef = [julius|.state(#{toJSON s},
                            {templateUrl: "?state=#{rawJS s}"^{opts} })|]
  tell mempty { awStates = M.singleton s ham,
                awStateDefs = sdef, awControllers = ctrl }


-- | Add a state redirection.
--
addRedirection :: Text -> Text -> Q Exp
addRedirection from to = [|addRedirectionRaw $(liftT from) $(liftT to)|]
  where liftT t = [|T.pack|] >>=
                  \p -> return $ AppE p $ LitE $ StringL $ T.unpack t


-- | Raw function to add a state redirection for use in TH.
--
addRedirectionRaw :: Text -> Text -> AngularUI ()
addRedirectionRaw from to = do
  let rdef = [julius|.when(#{toJSON from}, #{toJSON to})|]
  tell mempty { awRedirDefs = rdef }


-- | Add a default redirection.
--
addDefaultRedirection :: Text -> Q Exp
addDefaultRedirection from =
  [|addDefaultRedirectionRaw $(liftT from)|]
  where liftT t = [|T.pack|] >>=
                  \p -> return $ AppE p $ LitE $ StringL $ T.unpack t


-- | Raw function to add a default redirection for use in TH.
--
addDefaultRedirectionRaw :: Text -> AngularUI ()
addDefaultRedirectionRaw from = do
  let rdef = [julius|.otherwise(#{toJSON from})|]
  tell mempty { awRedirDefs = rdef }


-- | Generate controller name from state name.
--
controllerName :: Text -> Text
controllerName s = T.map norm1 s <> "$$Ctrl"
  where norm1 c = if isAlpha c then c else '_'


-- | Set up modal definitions.
--
addModals :: Text     -- ^ module name
          -> [Modal]  -- ^ modal definitions
          -> Q Exp
addModals modnm ms =
  if null ms
  then [|do { return () }|]
  else mapM (addModal modnm) ms >>= (return . DoE . map NoBindS)


-- | Set up single modal definition.
--
addModal :: Text   -- ^ module name
         -> Modal  -- ^ modal definition
         -> Q Exp
addModal modnm (Modal m ham mjs) = do
  case mjs of
    Nothing -> [|addModalRaw $(liftT m) $(hamletFile $ fn "hamlet") Nothing|]
    Just _ -> [|addModalRaw $(liftT m) $(hamletFile $ fn "hamlet")
                (Just $(ngFile $ fn "julius"))|]
  where liftT t = [|T.pack|] >>= \p -> return $ AppE p $ LitE $ StringL $ t
        fn suf = T.unpack $ T.concat ["angular/ui-router/", modnm, "/modal/",
                                      T.replace "." "/" (T.pack m), ".", suf]


-- | Raw function to set up single modal definition for use in TH.
--
addModalRaw :: Text          -- ^ modal name
            -> Partial       -- ^ template
            -> Maybe Script  -- ^ controller
            -> AngularUI ()
addModalRaw m ham mjs = do
  let c = controllerName m <> "Modal"
      (jc, ctrl) = case mjs of
        Nothing -> (mempty, mempty)
        Just j -> ([julius|,controller: #{toJSON c}|],
                   [julius|var #{rawJS c} = ^{j};|])
      mdef = [julius|#{toJSON m}: { templateUrl: "?modal=#{rawJS m}"^{jc} }|]
  tell mempty { awModals = M.singleton m ham,
                awModalDefs = [mdef], awControllers = ctrl }


-- | Set up module name and top level Hamlet, Julius and Cassius or
-- Lucius file details.
--
setModule :: Text -- ^ module name
          -> Q Exp
setModule modnm = do
  casex <- qRunIO . doesFileExist . fn $ "cassius"
  lucex <- qRunIO . doesFileExist . fn $ "lucius"
  case (casex, lucex) of
    (True, _) -> [|setModuleRaw $(liftT modnm) $(hamletFile $ fn "hamlet")
                   (Just $(cassiusFile $ fn "cassius"))
                   $(juliusFile $ fn "julius")|]
    (_, True) -> [|setModuleRaw $(liftT modnm) $(hamletFile $ fn "hamlet")
                   (Just $(luciusFile $ fn "lucius"))
                   $(juliusFile $ fn "julius")|]
    (_, _)    -> [|setModuleRaw $(liftT modnm) $(hamletFile $ fn "hamlet")
                   Nothing
                   $(juliusFile $ fn "julius")|]
  where
    liftT t = [|T.pack|] >>= \p -> return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/ui-router/", modnm,
                                     "/", modnm, ".", suffix]


-- | Raw function to set up module information for use in TH.
--
setModuleRaw :: Text -> Partial -> Maybe Stylesheet -> Script -> AngularUI ()
setModuleRaw nm h ms j = tell mempty { awModule = First $ Just (nm, h, ms, j) }


-- | Add shared Julius module, with optional injection into main module.
--
addSharedModule :: Text -- ^ module name
                -> Bool -- ^ inject?
                -> Q Exp
addSharedModule i inj = [|addModuleRaw $(liftT i) inj $(ngFile f)|]
  where liftT t = [|T.pack|] >>=
                  \p -> return $ AppE p $ LitE $ StringL $ T.unpack t
        f = T.unpack $ T.concat ["angular/shared/", i, ".julius"]


-- | Raw function to add shared module for use in TH.
--
addModuleRaw :: Text -> Bool -> Script -> AngularUI ()
addModuleRaw i inj s =
  tell mempty { awMods = M.singleton i (inj, Just [julius|^{s};|]) }


-- | Inject pre-existing library module into main module.
--
injectLibraryModule :: Text -> AngularUI ()
injectLibraryModule i = tell mempty { awMods = M.singleton i (True, Nothing) }


-- | Helper function to deal with Julius file reloading.
--
ngFile :: String -> Q Exp
ngFile = if development then juliusFileReload else juliusFile
