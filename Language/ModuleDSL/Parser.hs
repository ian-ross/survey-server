module Language.ModuleDSL.Parser
       ( parseModule ) where

import Prelude
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Language.ModuleDSL.Syntax


-- | Basic string-based parser type.
type Parser a = P (Str Char String LineColPos) a


-- | Parse a single module from a string, returning either the module
-- definition or a list of errors.
parseModule :: String -> Either [Error LineColPos] Module
parseModule inp = if null errs then Right res else Left errs
  where (res, errs) = parse ((,) <$> pModule <*> pEnd)
                            (createStr (LineColPos 0 0 0) inp)

-- | Parse a name: starts with a lower case letter, then zero or more
-- letters, digits, underscores or dashes.
pName :: Parser Name
pName = Name <$> ((:) <$> pLower <*> pList pIdChar)
  where pIdChar = pLower <|> pUpper <|> pDigit <|> pAnySym "_-"

-- | Parse a value.
pValue :: Parser Value
pValue = String <$> pQuotedString
     <|> Integer <$> pNatural
     <|> Double <$> pDouble
     <|> Bool <$> pBool
     <|> pReturn Null <* pToken "null"
  where pBool = pReturn True <* pToken "true"
            <|> pReturn False <* pToken "false"

-- | Parse a single option setting.
pOption :: Parser Option
pOption = Option <$> pName <* pToken "=" <*> pValue

-- | Parse a list of options.
pOptions :: Parser [Option]
pOptions = pBrackets (pList1Sep pComma pOption) `opt` []

-- | Parse a single choice.
pChoice :: Parser Choice
pChoice = Choice <$> pQuotedString <* pToken "=>" <*> pValue

-- | Parse a list of choices.
pChoices :: Parser [Choice]
pChoices = pBraces (pList1Sep pComma pChoice)

-- | Parse a single question.
pQuestion :: Parser Question
pQuestion = NumericQuestion <$ pToken "NumericQuestion" <*>
            pQuotedString <*> pOptions
        <|> ChoiceQuestion <$ pToken "ChoiceQuestion" <*>
            pQuotedString <*> pOptions <*> pChoices

-- | Parse a list of question definitions of the form "n = q".
pQuestions :: Parser [(Name, Question)]
pQuestions = pMany ((,) <$> pName <* pToken "=" <*> pQuestion)

-- | Parse a top-level definition: either a survey page or a
-- parameterised specialisation of a question type.
pTopLevel :: Parser TopLevel
pTopLevel = Specialisation <$ pToken "Specialisation" <*>
            pName <*> (pParens (pListSep pComma pName) `opt` []) <*> pQuestion
        <|> SurveyPage <$ pToken "SurveyPage" <*>
            pName <*> pOptions <*> pQuestions

-- | Parse a module.
pModule :: Parser Module
pModule = Module <$ pToken "Module" <*>
          pName <*> pOptions <*> pMany pTopLevel