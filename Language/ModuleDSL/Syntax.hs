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
              | EqOp | NEqOp | GtOp | GEqOp | LtOp | LeqOp
              | EqCIOp | NEqCIOp | GtCIOp | GEqCIOp | LtCIOp | LeqCIOp
              deriving (Eq, Ord, Show, Data, Typeable)

data Expr = LitExpr Literal
          | RefExpr Name
          | UnaryExpr UnaryOp Expr
          | BinaryExpr BinaryOp Expr Expr
          | FunExpr Name [Expr]
          deriving (Eq, Ord, Show, Data, Typeable)

data Option = Option { optKey :: Name
                     , optValue :: Literal }
            deriving (Eq, Ord, Show, Data, Typeable)

lookupOpt :: Text -> [Option] -> Literal -> Literal
lookupOpt k os def = maybe def optValue $ find ((== (Name k)) . optKey) os


data Choice = Choice { chText :: Text
                     , chValue :: Literal }
            deriving (Eq, Ord, Show, Data, Typeable)

data Question = NumericQuestion { nqText :: Text
                                , nqOpts :: [Option] }
              | ChoiceQuestion { cqText :: Text
                               , cqOpts :: [Option]
                               , cqChoices :: [Choice] }
              deriving (Eq, Ord, Show, Data, Typeable)

data TopLevel = Specialisation { specName :: Name
                               , specParams :: [Name]
                               , specBody :: Question }
              | SurveyPage { spName :: Name
                           , spOpts :: [Option]
                           , spQuestions :: [(Name, Question)] }
              deriving (Eq, Ord, Show, Data, Typeable)

data Module = Module { modName :: Name
                     , modOpts :: [Option]
                     , modBody :: [TopLevel] }
              deriving (Eq, Ord, Show, Data, Typeable)



