{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Rendering of module definitions into HTML and Javascript for
-- running surveys.
--
-- There are two main parts to this:
--
--  * A bunch of instances for turning lower-level elements of the
--    module DSL syntax into Javascript or HTML: these are mostly
--    instances of 'ToMarkup' or 'ToJavascript'.  Most of these are
--    fairly straightforward, the most complex being the
--    'ToJavascript' instance for 'Expr'.
--
--  * A rendering state monad.  The state here keeps track of indexes
--    into a pair of Angular scope variable arrays in the resulting
--    Javascript.  These are used to store text expressions and
--    "utility" expressions of various kinds.  The reason for doing
--    this is to allow for easy access to Angular's data binding: text
--    expressions in questions automatically have data binding enabled
--    -- references to question values will update as the user changes
--    their choices.
module Language.ModuleDSL.Render
       ( render
       ) where

import Import hiding (Module, Null, String, Bool)
import Import (String)
import qualified Import as I
import Data.List (intersperse)
import qualified Data.Aeson as A
import Data.Attoparsec.Number (Number(..))
import Data.Text.Lazy.Builder
import Text.Blaze
import Text.Julius
import Control.Monad.Trans.State

import Language.ModuleDSL.Syntax

-- | Identifiers in HTML.
instance ToMarkup Name where
  toMarkup (Name n) = toMarkup n

-- | Identifiers in Javascript.
instance ToJavascript Name where
  toJavascript (Name n) = toJavascript $ rawJS n

-- | Identifier lists in Javascript.
instance ToJavascript [Name] where
  toJavascript ns = mconcat $ intersperse "," $ map (toJavascript . rawJS) ns

-- | Literals in HTML.
instance ToMarkup Literal where
  toMarkup (String s) = toMarkup s
  toMarkup (Integer i) = toMarkup i
  toMarkup (Double d) = toMarkup d
  toMarkup (Bool b) = toMarkup b
  toMarkup Null = toMarkup ("null" :: String)

-- | Literals in Javascript.
instance ToJavascript Literal where
  toJavascript (String s) = toJavascript $ A.String s
  toJavascript (Integer i) = toJavascript $ A.Number (I i)
  toJavascript (Double d) = toJavascript $ A.Number (D d)
  toJavascript (Bool b) = toJavascript $ A.Bool b
  toJavascript Null = toJavascript $ A.Null

-- | Javascript equivalents of module DSL unary operators.
instance ToJavascript UnaryOp where
  toJavascript NegOp = "-"
  toJavascript NotOp = "!"

-- | Javascript equivalents of module DSL binary operators.
instance ToJavascript BinaryOp where
  toJavascript AddOp = "+"
  toJavascript SubOp = "-"
  toJavascript MulOp = "*"
  toJavascript DivOp = "/"
  toJavascript PowOp = "^"       -- WRONG!  (but dealt with below).
  toJavascript CatOp = "+"
  toJavascript AndOp = "&&"
  toJavascript OrOp = "||"
  toJavascript EqOp = "=="
  toJavascript NEqOp = "!="
  toJavascript GtOp = ">"
  toJavascript GEqOp = ">="
  toJavascript LtOp = "<"
  toJavascript LeqOp = "<="
  toJavascript EqCIOp = "@=="    -- WRONG!  (and not yet implemented!)
  toJavascript NEqCIOp = "@!="   -- WRONG!  (and not yet implemented!)
  toJavascript GtCIOp = "@>"     -- WRONG!  (and not yet implemented!)
  toJavascript GEqCIOp = "@>="   -- WRONG!  (and not yet implemented!)
  toJavascript LtCIOp = "@<"     -- WRONG!  (and not yet implemented!)
  toJavascript LeqCIOp = "@<="   -- WRONG!  (and not yet implemented!)

-- | Fixity information for operators and expressions.
class Fixity a where
  fixity :: a -> Maybe Int

-- | Defining fixity for an expression is a bit weird, but it
-- simplifies things a bit.
instance Fixity Expr where
  fixity (BinaryExpr op _ _) = fixity op
  fixity _ = Nothing

-- | Fixities for built-in binary operators.
instance Fixity BinaryOp where
  fixity PowOp = Just 9
  fixity MulOp = Just 8
  fixity DivOp = Just 8
  fixity AddOp = Just 7
  fixity CatOp = Just 7
  fixity SubOp = Just 7
  fixity AndOp = Just 5
  fixity OrOp  = Just 5
  fixity _     = Just 6         -- Comparison operators.

-- | Associativity information for binary operators.
data Assoc = L | R | None deriving Eq

-- | Define binary operator associativities.
assoc :: BinaryOp -> Assoc
assoc PowOp = R
assoc MulOp = L
assoc DivOp = L
assoc AddOp = L
assoc SubOp = L
assoc AndOp = L
assoc OrOp  = L
assoc CatOp = L
assoc _     = None              -- Comparison operators.

-- | Converting a module DSL expression to Javascript is mostly just a
-- case of recursively traversing the AST, converting operators,
-- literals and identifiers as we reach them.  The only wrinkle to
-- this is dealing with operator precedence and associativity.
instance ToJavascript Expr where
  toJavascript (LitExpr lit) = toJavascript lit
  toJavascript (RefExpr n) = toJavascript n
  toJavascript (UnaryExpr op e@(LitExpr _)) = toJavascript op <> toJavascript e
  toJavascript (UnaryExpr op e@(RefExpr _)) = toJavascript op <> toJavascript e
  toJavascript (UnaryExpr op e) =
    toJavascript op <> "(" <> toJavascript e <> ")"
  toJavascript (BinaryExpr PowOp e1 e2) =
    "Math.pow(" <> toJavascript e1 <> "," <> toJavascript e2 <> ")"
  toJavascript (BinaryExpr op e1 e2) =
    let Just opfix = fixity op
        js1 = case fixity e1 of
          Nothing -> toJavascript e1
          Just f1 -> if f1 < opfix || (f1 == opfix && assoc op == L)
                     then "(" <> toJavascript e1 <> ")"
                     else toJavascript e1
        js2 = case fixity e2 of
          Nothing -> toJavascript e2
          Just f2 -> if f2 < opfix || (f2 == opfix && assoc op == R)
                     then "(" <> toJavascript e2 <> ")"
                     else toJavascript e2
    in js1 <> toJavascript op <> js2
  toJavascript (FunExpr n es) =
    toJavascript n <> "(" <>
    mconcat (intersperse "," $ map toJavascript es) <> ")"
  toJavascript (IfThenElseExpr i t e) =
    "((" <> toJavascript i <> ") ? (" <>
         toJavascript t <> ") : (" <> toJavascript e <> "))"
  toJavascript (RecordExpr fs) =
    "{" <> (mconcat $ intersperse "," $
            map (\(k, v) -> toJavascript k <> "=" <> toJavascript v) fs) <> "}"
  toJavascript (ArrayExpr es) =
    "[" <> (mconcat $ intersperse "," $ map toJavascript es) <> "]"

-- | Choices are represented in Javascript as records with a "label"
-- and a "value" field.
instance ToJavascript Choice where
  toJavascript (Choice t v) =
    "{ label: " <> toJavascript (A.String t) <>
    ", value: " <> toJavascript v <> "}"

-- | Lists of choices in Javascript.
instance ToJavascript [Choice] where
  toJavascript cs =
    "[" <> (mconcat $ intersperse "," $ map toJavascript cs) <> "]"


-- | State type for rendering monad.
data RenderState =
  RenderState { textIdx :: Integer
                -- ^ Index of next text array entry to use.
              , utilIdx :: Integer
                -- ^ Index of next utility array entry to use.
              } deriving (Eq, Show)

-- | Rendering monad: has to be parameterised by the URL type in
-- 'JavascriptUrl' to play nicely with the Shakespeare templates used
-- in the rendering definitions.
type Render url = State RenderState (Markup, JavascriptUrl url)

-- | Initial render state.
initRenderState :: RenderState
initRenderState = RenderState 0 0

-- | Get the next text array entry to use and update.
nextTextIdx :: State RenderState Integer
nextTextIdx = do
  tidx <- gets textIdx
  modify (\s -> s { textIdx = tidx + 1 })
  return tidx

-- | Get the next utility array entry to use and update.
nextUtilIdx :: State RenderState Integer
nextUtilIdx = do
  uidx <- gets utilIdx
  modify (\s -> s { utilIdx = uidx + 1 })
  return uidx

-- | Values that can be rendered are must be instances of 'RunRender'.
class RunRender a where
  runRender :: a -> Render url

-- | Render an item: start with the initial rendering state and turn
-- the handle.  The result is a pair of the HTML markup for the item
-- and whatever Javascript it needs to go with that.  The Javascript
-- is written to run in a context that sets up certain Angular scope
-- variables -- see module-view.julius in module-view or
-- run-survey.julius in run-survey under angular/ui-router to get an
-- idea of how it works.
render :: RunRender a => a -> (Markup, JavascriptUrl url)
render v = evalState (runRender v) initRenderState


-- | To render a module, just render all of the components that it
-- contains.  (Note that the module name and options aren't used yet,
-- although they're captured in the AST.)
instance RunRender Module where
  runRender (Module _name _opts body) = mconcat <$> mapM runRender body


instance RunRender TopLevel where runRender = renderTopLevel

-- | Rendering top-level components, we split on the component type.
renderTopLevel :: TopLevel -> Render url

-- | To render a 'SurveyPage' value, we render each of the questions
-- in the body of the survey page, then insert their HTML into a
-- template with a simple header, and concatenate their Javascript
-- onto a script that prints a simple debugging message.
renderTopLevel (SurveyPage name _opts body) = do
  (qs, ss) <- mconcat <$> mapM runRender body
  return ([shamlet|
  <h4>
    Survey page: #{name}
  ^{qs}
          |],
          [julius|console.log("Survey page: #{name}");|] <> ss)

-- | Rendering of 'Specialisation' items is not yet implemented: just
-- return some placeholder HTML and an empty script.
renderTopLevel (Specialisation _name _params _body) =
  return (toMarkup ("Specialisation..." :: String), mempty)

-- | To render a 'Function' value, we return empty HTML and a script
-- that assigns a function to an Angular scope variable, where the
-- body of the function is modified to take account of the scoping of
-- names, which are either function parameters (so require no
-- modification) or global names (which must refer to Angular scope
-- variables giving question result values).
renderTopLevel (Function name params body) =
  return (mempty,
          [julius|
  sc.#{name} = function(#{params}) {
    return #{fscope False params body};
  };
          |])

-- | Class to handle scope control for names in function definitions.
-- The 'fscope' method takes an interpolation flag (this indicates
-- whether an expression is being manipulated for use within an
-- Angular interpolation expression within some HTML [true] or within
-- Javascript code [false]), a list of local parameter names, and an
-- AST value to transform.  It returns a modified AST where all names
-- not in the local parameter name list are treated as entries in an
-- Angular scope variable called 'results' that contains the user's
-- responses to each named survey question.
class Scopeable a where
  fscope :: I.Bool -> [Name] -> a -> a

-- | Basic scope handling for names: if a name is in the list of local
-- parameters currently in play, return it without change; otherwise,
-- construct an expression that accesses the appropriate entry in the
-- Angular scope variable array called 'results'.  If the
-- interpolation flag is true, then the Angular scope is accessed
-- implicitly, otherwise it is accessed explicitly by prepending "sc."
-- to the front of the Javascript expression.
instance Scopeable Name where
  fscope interp ls n@(Name nt)
    | n `notElem` ls =
        Name $ (if interp then "" else "sc.") <> "results['" <> nt <> "']"
    | otherwise      = n

-- | Specialised scope handling for function names, because functions
-- are defined directly as Angular scope variables, not as members of
-- the 'results' array.
fscope' :: I.Bool -> [Name] -> Name -> Name
fscope' interp ls n@(Name nt)
  | not interp && n `notElem` ls = Name $ "sc." <> nt
  | otherwise      = n

-- | Scope handling for expressions.  Mostly just a matter of
-- propagating calls to fscope down through the AST, except for the
-- treatment of function names, which requires a slight modification
-- to the 'fscope' method.
instance Scopeable Expr where
  fscope i ls (RefExpr n) = RefExpr (fscope i ls n)
  fscope i ls (UnaryExpr op e) = UnaryExpr op (fscope i ls e)
  fscope i ls (BinaryExpr op e1 e2) =
    BinaryExpr op (fscope i ls e1) (fscope i ls e2)
  fscope i ls (FunExpr n es) = FunExpr (fscope' i ls n) $ map (fscope i ls) es
  fscope i ls (IfThenElseExpr c t e) =
    IfThenElseExpr (fscope i ls c) (fscope i ls t) (fscope i ls e)
  fscope i ls (RecordExpr fs) =
    RecordExpr $ map (\(k, v) -> (k, fscope i ls v)) fs
  fscope i ls (ArrayExpr es) = ArrayExpr $ map (fscope i ls) es
  fscope _ _  (LitExpr l) = LitExpr l


instance RunRender Question where runRender = renderQuestion

-- | Rendering top-level components, we split on the component type.
renderQuestion :: Question -> Render url

-- | Rendering of 'NumericQuestion' questions as HTML range inputs.
--
-- The HTML we generate here has a wrapper with the question text
-- (accessed from the 'text' Angular scope variable to allow for data
-- binding) around a single HTML range slider input.  The limits for
-- the range slider are taken from the question options.
--
-- The Javascript we generate sets up the question text entry, sets up
-- the initial value in the 'results' array (here taken to be half way
-- between the minimum and maximum slider values) and sets the results
-- post-processor to use on the client side appropriately for a
-- numeric value.
--
-- Things to note:
--
--  * The Angular expression interpolating the question text takes its
--    value from an entry in the 'text' array, the index of which is
--    picked up from 'nextTextIdx'.  The result of this is that the
--    question text can be an arbitrary module DSL expression
--    referring to other question results and the question text gets
--    updated via Angular data binding as the user changes their
--    answers.
--
--  * The 'ng-model' data binding on the range slider ensures that the
--    appropriate entry in the 'results' array gets updated as the
--    slider is moved.  Because of the expression modifications done
--    using the 'Scopeable' class above, this means that expressions
--    that depend on this result also get updated, and can then be
--    used as textual elements in a question.
--
--  * The 'postprocessor' stuff is just some simple little functions
--    to make sure that the JSON results sent back to the server have
--    reasonable types -- numerical values instead of strings in this
--    case.
--
-- The central point here is that the way things are set up is
-- intended to make use of Angular data binding in survey definitions
-- easy, automatic even.  There are some things that could be cleaned
-- up, but I think the basic idea is sound.
renderQuestion (NumericQuestion name qt os) = do
  let LitExpr (Integer mn) = lookupOpt "min" os (LitExpr $ Integer 0)
      LitExpr (Integer mx) = lookupOpt "max" os (LitExpr $ Integer 100)
      initVal = (mn + mx) `div` 2
  tidx <- nextTextIdx
  return ([shamlet|
  <div .question>
    <div .question-text>
      {{text[#{tidx}]}}
    <div>
      <input type="range" min=#{mn} max=#{mx}
                  ng-model="results['#{name}']">
      <span>
        {{results['#{name}']}}
          |],
          [julius|
  console.log("Numeric question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.results['#{name}'] = #{toJSON initVal};
  sc.postprocess['#{name}'] = postprocessor.numeric;
          |])

-- | Rendering of 'ChoiceQuestion' questions as HTML radio button
-- inputs.
--
-- Very much like the previous range input example.  This is quite a
-- good example of combining Hamlet interpolation (the '$forall' loop
-- over the question choices) and Angular interpolation.
renderQuestion (ChoiceQuestion name qt _os cs) = do
  tidx <- nextTextIdx
  return ([shamlet|
  <div .question>
    <div .question-text>
      <span>
        {{text[#{tidx}]}}
      $forall Choice ct (LitExpr v) <- cs
        <label .radio>
          <span>
            #{ct}
          <input type="radio" ng-model="results['#{name}']"
                 value=#{toMarkup v} name=#{"radio_" <> name}>
          |],
          [julius|
  console.log("Choice question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.results['#{name}'] = null;
          |])

-- | Rendering of 'DropdownQuestion' questions as HTML select inputs.
--
-- The main difference between this case and the preceding
-- 'ChoiceQuestion' one is that we want to use Angular's 'ng-options'
-- directive to construct the select element.  In order to do this, we
-- store the dropdown question choices in the "utility" Angular array.
-- We use the "choices" postprocessor to extract the value to return
-- to the server from the label/value pair in the selected choice.
renderQuestion (DropdownQuestion name qt _os cs) = do
  tidx <- nextTextIdx
  cidx <- nextUtilIdx
  return ([shamlet|
  <div .question>
    <div .question-text>
      <span>
        {{text[#{tidx}]}}
      <select ng-model="results['#{name}']"
              ng-options="c.label for c in utils[#{cidx}]">
          |],
          [julius|
  console.log("Dropdown question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.utils[#{Number (I cidx)}] = #{cs};
  sc.results['#{name}'] = sc.utils[#{Number (I cidx)}][0];
  sc.postprocess['#{name}'] = postprocessor.choices;
          |])

-- | Rendering of 'TextEntryQuestion' questions as HTML text or
-- textarea inputs.
--
-- Here, we use an option to the question ("size") to decide what sort
-- of HTML UI element to render.  We pass the option to the client
-- side using the "utility" array then use an Angular "ng-switch"
-- directive to decide whether to use a small single-line text input
-- or a multi-line textarea.  (The switching could be done on the
-- server side as well, of course.)
renderQuestion (TextEntryQuestion name qt os) = do
  tidx <- nextTextIdx
  uidx <- nextUtilIdx
  let size = lookupOpt "size" os (LitExpr $ String "small")
  return ([shamlet|
  <div .question>
    <div .question-text>
      <div>
        {{text[#{tidx}]}}
      <div ng-switch on="utils[#{uidx}]">
        <textarea ng-switch-when="large" ng-model="results['#{name}']">
        <input ng-switch-default type="text" ng-model="results['#{name}']">
          |],
          [julius|
  console.log("Text entry question: #{name}");
  sc.text[#{Number (I tidx)}] = #{String qt};
  sc.results['#{name}'] = '';
  sc.utils[#{Number (I uidx)}] = #{size};
          |])

-- | Rendering of 'TextDisplay' items as HTML text.
--
-- Note that the question text is included as an Angular expression
-- here, so that variable interpolation within the text works.
--
-- The slightly ugly 'preEscapedToMarkup . toLazyText . toJavascript'
-- thing is needed here to convert from a 'Javascript url' value to
-- markup that can be interpolated into a Hamlet template.
renderQuestion (TextDisplay qt _os) = do
  let qts = fscope True [] qt
  return ([shamlet|
  <div .question>
    <div .question-text>
      <span>
        {{#{preEscapedToMarkup $ toLazyText $ toJavascript qts}}}
          |],
          [julius|
  console.log("Text display...");
          |])
