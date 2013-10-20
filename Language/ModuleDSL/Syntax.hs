{-# LANGUAGE DeriveDataTypeable #-}
module Language.ModuleDSL.Syntax where

import Prelude
import Data.Data
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Text.Julius


data Literal = String !Text
             | Integer !Integer
             | Double !Double
             | Bool !Bool
             | Null
             deriving (Eq, Ord, Show, Data, Typeable)

data Name = Name Text
          deriving (Eq, Ord, Show, Data, Typeable)

instance IsString Name where
  fromString = Name . T.pack

instance Monoid Name where
  mempty = Name ""
  mappend (Name n1) (Name n2) = Name (n1 <> n2)

instance RawJS Name where
  rawJS (Name n) = rawJS n

data UnaryOp = NegOp | NotOp
             deriving (Eq, Ord, Show, Data, Typeable)

data BinaryOp = AddOp | SubOp | MulOp | DivOp | PowOp
              | AndOp | OrOp
              | CatOp
              | EqOp | NEqOp | GtOp | GEqOp | LtOp | LeqOp
              | EqCIOp | NEqCIOp | GtCIOp | GEqCIOp | LtCIOp | LeqCIOp
              deriving (Eq, Ord, Show, Data, Typeable)

data Expr = LitExpr Literal
          | RefExpr Name
          | UnaryExpr UnaryOp Expr
          | BinaryExpr BinaryOp Expr Expr
          | FunExpr Name [Expr]
          | IfThenElseExpr Expr Expr Expr
          | RecordExpr [(Name, Expr)]
          | ArrayExpr [Expr]
          deriving (Eq, Ord, Show, Data, Typeable)

newtype Options = Options Expr
                deriving (Eq, Ord, Show, Data, Typeable)

lookupOpt :: Text -> Options -> Expr -> Expr
lookupOpt k (Options e) def = case e of
  RecordExpr os -> maybe def snd $ find ((== (Name k)) . fst) os
  _             -> def

data Choice = Choice { chText :: Text
                     , chValue :: Expr }
            deriving (Eq, Ord, Show, Data, Typeable)

data Question = NumericQuestion { nqName :: Name
                                , nqText :: Text
                                , nqOpts :: Options }
              | ChoiceQuestion { cqName :: Name
                               , cqText :: Text
                               , cqOpts :: Options
                               , cqChoices :: [Choice] }
              | DropdownQuestion { ddName :: Name
                                 , ddText :: Text
                                 , ddOpts :: Options
                                 , ddChoices :: [Choice] }
              | TextEntryQuestion { teName :: Name
                                  , teText :: Text
                                  , teOpts :: Options }
              | TextDisplay { tdText :: Expr
                            , tdOpts :: Options }
              deriving (Eq, Ord, Show, Data, Typeable)

data TopLevel = Specialisation { specName :: Name
                               , specParams :: [Name]
                               , specBody :: Question }
              | SurveyPage { spName :: Name
                           , spOpts :: Options
                           , spQuestions :: [Question] }
              | Function { fnName :: Name
                         , fnParams :: [Name]
                         , fnBody :: Expr }
              deriving (Eq, Ord, Show, Data, Typeable)

data Module = Module { modName :: Name
                     , modOpts :: Options
                     , modBody :: [TopLevel] }
              deriving (Eq, Ord, Show, Data, Typeable)



