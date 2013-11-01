{-# LANGUAGE DeriveDataTypeable #-}
-- | AST definitions for module definition DSL.
--
-- A 'Module' is composed of a number of 'TopLevel' components, such
-- as function definitions and survey pages:
--
--  * Function definitions are simple pure functions, i.e. a list of
--    named parameters plus a single expression giving the result of
--    the function.
--
--  * Survey pages are composed of a list of 'Question's, each of
--    which has a name (used to key the data collected from the
--    survey) and some question type specific rendering information
--    (e.g. a list of textual choices and corresponding data values
--    for a radio button choice question).
--
-- The low-level syntax of the module DSL includes the usual kind of
-- expression nodes ('Expr').
--
module Language.ModuleDSL.Syntax where

import Prelude
import Data.Data
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Text.Julius


-- | Literal values: a distinction is made between integer and
-- floating point values here, but both are represented the same way
-- in generated Javascript code.
data Literal = String !Text
             | Integer !Integer
             | Double !Double
             | Bool !Bool
             | Null
             deriving (Eq, Ord, Show, Data, Typeable)

-- | Identifiers: IsString instance to work with OverloadedStrings.
data Name = Name Text
          deriving (Eq, Ord, Show, Data, Typeable)

instance IsString Name where
  fromString = Name . T.pack

instance Monoid Name where
  mempty = Name ""
  mappend (Name n1) (Name n2) = Name (n1 <> n2)

instance RawJS Name where
  rawJS (Name n) = rawJS n

-- | Unary operators: just numerical and logical negation.
data UnaryOp = NegOp | NotOp
             deriving (Eq, Ord, Show, Data, Typeable)

-- | Binary operators.
data BinaryOp = AddOp | SubOp | MulOp | DivOp | PowOp
                -- ^ Arithmetic operations.
              | AndOp | OrOp
                -- ^ Logical operations.
              | CatOp
                -- ^ String concatenation.
              | EqOp | NEqOp | GtOp | GEqOp | LtOp | LeqOp
                -- ^ Comparison operations.
              | EqCIOp | NEqCIOp | GtCIOp | GEqCIOp | LtCIOp | LeqCIOp
                -- ^ Case-insensitive comparison operations.
              deriving (Eq, Ord, Show, Data, Typeable)

-- | Expression type: operator precedence is handled in the parser, so
-- there's just a single constructor for binary operations.
data Expr = LitExpr Literal
            -- ^ Literal expressions.
          | RefExpr Name
            -- ^ Identifiers ("reference expressions").
          | UnaryExpr UnaryOp Expr
            -- ^ Unary operations.
          | BinaryExpr BinaryOp Expr Expr
            -- ^ Binary operations.
          | FunExpr Name [Expr]
            -- ^ Function calls.
          | IfThenElseExpr Expr Expr Expr
            -- ^ "if condition then result1 else result2" expressions.
          | RecordExpr [(Name, Expr)]
            -- ^ Record expressions: associate each of a list of names
            -- with an expression.
          | ArrayExpr [Expr]
            -- ^ Array expressions: ordered list of expressions.
          deriving (Eq, Ord, Show, Data, Typeable)

-- | Options for survey components are represented as record
-- expressions.
newtype Options = Options Expr
                deriving (Eq, Ord, Show, Data, Typeable)

-- | Extract an option value with a default.
lookupOpt :: Text -> Options -> Expr -> Expr
lookupOpt k (Options e) def = case e of
  RecordExpr os -> maybe def snd $ find ((== (Name k)) . fst) os
  _             -> def

-- | Choices are used for drop-down and radio button questions and
-- give a textual label and an associated value.
data Choice = Choice { chText :: Text
                     , chValue :: Expr }
            deriving (Eq, Ord, Show, Data, Typeable)

-- | Question types: in each case, the question has a name, an
-- introductory piece of text and a set of options.
data Question = NumericQuestion { nqName :: Name
                                , nqText :: Text
                                , nqOpts :: Options }
                -- ^ Question with numeric result, represented by a
                -- range slider.
              | ChoiceQuestion { cqName :: Name
                               , cqText :: Text
                               , cqOpts :: Options
                               , cqChoices :: [Choice] }
                -- ^ Radio button choice question.
              | DropdownQuestion { ddName :: Name
                                 , ddText :: Text
                                 , ddOpts :: Options
                                 , ddChoices :: [Choice] }
                -- ^ Drop-down choice question.
              | TextEntryQuestion { teName :: Name
                                  , teText :: Text
                                  , teOpts :: Options }
                -- ^ Free text entry question.
              | TextDisplay { tdText :: Expr
                            , tdOpts :: Options }
                -- ^ Text display "question": included here as a
                -- "question" to allow for "live data" interpolation.
              deriving (Eq, Ord, Show, Data, Typeable)

-- | Top-level components in a module.
data TopLevel = Specialisation { specName :: Name
                               , specParams :: [Name]
                               , specBody :: Question }
                -- ^ (Not yet implemented) This is intended to be used
                -- for naming specialisations of existing question
                -- types: for instance, you could define a
                -- "YesNoQuestion" as a specialisation of a
                -- ChoiceQuestion.
              | SurveyPage { spName :: Name
                           , spOpts :: Options
                           , spQuestions :: [Question] }
                -- ^ A survey page has a name, some options and a list
                -- of questions.
              | Function { fnName :: Name
                         , fnParams :: [Name]
                         , fnBody :: Expr }
                -- ^ Function definitions: pure functions only,
                -- recursive definitions allowed.
              deriving (Eq, Ord, Show, Data, Typeable)

-- | Module definitions: as with most other things, they have a name
-- and some options, then they have a list of top-level components.
data Module = Module { modName :: Name
                     , modOpts :: Options
                     , modBody :: [TopLevel] }
              deriving (Eq, Ord, Show, Data, Typeable)
