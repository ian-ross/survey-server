{-# LANGUAGE DeriveDataTypeable #-}
module Language.ModuleDSL.Syntax where

import Prelude
import Data.Data


data Value = String !String
           | Integer !Integer
           | Double !Double
           | Bool !Bool
           | Null
           deriving (Eq, Ord, Show, Data, Typeable)

data Name = Name String
          deriving (Eq, Ord, Show, Data, Typeable)

data Option = Option { optKey :: Name
                     , optValue :: Value }
            deriving (Eq, Ord, Show, Data, Typeable)

data Choice = Choice { chText :: String
                     , chValue :: Value }
            deriving (Eq, Ord, Show, Data, Typeable)

data Question = NumericQuestion { nqText :: String
                                , nqOpts :: [Option] }
              | ChoiceQuestion { cqText :: String
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



