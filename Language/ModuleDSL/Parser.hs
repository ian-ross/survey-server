{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Language.ModuleDSL.Parser
       ( parseModule ) where

import Prelude
import Data.Char
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Language.ModuleDSL.Syntax


-- | Basic string-based parser type.
type Parser a = P (Str Char String LineColPos) a



-- | Parse a single module from a string, returning either the module
-- definition or a list of errors.
parseModule :: String -> Either [Error LineColPos] Module
parseModule inp = if null errs then Right res else Left errs
  where (res, errs) = parse ((,) <$ pSpaces <*> pModule <*> pEnd)
                            (createStr (LineColPos 0 0 0) inp)

-- | Parse a name: starts with a lower case letter, then zero or more
-- letters, digits, underscores or dashes.
pNameRaw :: Parser Name
pNameRaw = Name <$> ((:) <$> pLower <*> pList pIdChar)
  where pIdChar = pLower <|> pUpper <|> pDigit <|> pAnySym "_-"

-- | Lexeme parser for names.
pName :: Parser Name
pName = lexeme pNameRaw

-- | Parse a value.
pValue :: Parser Value
pValue = String <$> pQuotedString
     <|> head <$> amb (Double <$> pDouble <|> Integer <$> pInteger)
     <|> Bool <$> pBool
     <|> pReturn Null <* pSymbol "null"
  where pBool = pReturn True <* pSymbol "true"
            <|> pReturn False <* pSymbol "false"

-- | Parse a single option setting.
pOption :: Parser Option
pOption = Option <$> pName <* pSymbol "=" <*> pValue

-- | Parse a list of options.
pOptions :: Parser [Option]
pOptions = pBrackets (pListSep pComma pOption) `opt` []

-- | Parse a single choice.
pChoice :: Parser Choice
pChoice = Choice <$> pQuotedString <* pSymbol "=>" <*> pValue

-- | Parse a list of choices.
pChoices :: Parser [Choice]
pChoices = pBraces (pListSep pComma pChoice)

-- | Parse a single question.
pQuestion :: Parser Question
pQuestion = NumericQuestion <$ pSymbol "NumericQuestion" <*>
            pQuotedString <*> pOptions
        <|> ChoiceQuestion <$ pSymbol "ChoiceQuestion" <*>
            pQuotedString <*> pOptions <*> pChoices

-- | Parse a list of question definitions of the form "n = q".
pQuestions :: Parser [(Name, Question)]
pQuestions = pMany ((,) <$> pName <* pSymbol "=" <*> pQuestion)

-- | Parse a top-level definition: either a survey page or a
-- parameterised specialisation of a question type.
pTopLevel :: Parser TopLevel
pTopLevel = Specialisation <$ pSymbol "Specialisation" <*>
            pName <*> (pParens (pListSep pComma pName) `opt` [])
            <* pSymbol "=" <*> pQuestion
        <|> SurveyPage <$ pSymbol "SurveyPage" <*>
            pName <*> pOptions <*> pQuestions

-- | Parse a module.
pModule :: Parser Module
pModule = Module <$ pSymbol "Module" <*>
          pName <*> pOptions <*> pMany pTopLevel



-- UTILITIES
--
-- Unfortunately, although most of this is defined in
-- Text.ParserCombinators.UU.Utils, we need to redefine things here to
-- incorporate a comment syntax into the definition of whitespace.

-- | All input is normalised to have LF end-of-lines.
pLF :: Parser Char
pLF = pSym '\n'

-- | Lower case letters: use Data.Char.isLower to support Unicode
-- correctly.
pLower :: Parser Char
pLower = pSatisfy isLower (Insertion  "Lower case letter" 'x' 5)

-- | Upper case letters: use Data.Char.isUpper to support Unicode
-- correctly.
pUpper :: Parser Char
pUpper = pSatisfy isUpper (Insertion  "Upper case letter" 'X' 5)

-- | Digits: use Data.Char.isDigit to support Unicode correctly.
pDigit :: Parser Char
pDigit = pSatisfy isDigit (Insertion  "Digit" '1' 5)

-- | Comments run from "-- " to the end of the line.
pComment :: Parser String
pComment = (++) <$> pTokenCost "-- " 1000 <*> pMunch (/='\n') <* pLF

-- | Whitespace is any number of space characters or comments.  This
-- is set up to concatenate all recognised whitespace and comments
-- into a single string.  (The slightly odd form of the second option
-- in "one" is needed to ensure that this paraer cannot recognise the
-- empty string, since it is passed to pList1.)
pSpaces :: Parser String
pSpaces = (concat <$> pList1 one) <|> pReturn "" <?> "Whitespace"
  where one = pComment <|>
              ((:) <$> pAnySym spcs <*> pMunch (`elem` spcs))
        spcs = " \t\r\n"

-- | Lexeme parsers skip trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* pSpaces

-- | Lexeme parser for tokens.
pSymbol :: String -> Parser String
pSymbol = lexeme . pToken

-- | Recognise any of a list of items.
pAnySym :: String -> Parser Char
pAnySym = pAny pSym

-- | Double quoted string.
pQuotedString :: Parser String
pQuotedString = lexeme $ pSym '"' *> pList pNonQuoteVChar <* pSym '"'
  where pNonQuoteVChar =
          pSatisfy (\c -> c /= '"')
          (Insertion  ("Character in a double-quoted string") 'y' 5)

-- | Convert digits to numbers.
pDigitAsNum ::  Num a => Parser a
pDigitAsNum = digit2Int <$> pDigit
  where digit2Int a = fromInteger $ toInteger $ ord a - ord '0'

-- | Raw (non lexeme) parser for natural numbers.
pNaturalRaw :: (Num a) => Parser a
pNaturalRaw = foldl (\a b -> a * 10 + b) 0 <$> pList1 pDigitAsNum <?> "Natural"

-- | Raw (non lexeme) parser for integers.
pIntegerRaw :: (Num a) => Parser a
pIntegerRaw = pSign <*> pNaturalRaw <?> "Integer"

-- | Raw (non lexeme) parser for doubles.
pDoubleRaw :: (Read a) => Parser a
pDoubleRaw = read <$> pDoubleStr

-- | Parse textual representation of a floating point number.
pDoubleStr :: Parser  [Char]
pDoubleStr = pOptSign <*> (pToken "Infinity" <|> pPlainDouble)
             <?> "Double (eg -3.4e-5)"
  where pPlainDouble = (++) <$> ((++) <$> pList1 pDigit <*>
                                 (pFraction `opt` [])) <*> pExponent
        pFraction = (:) <$> pSym '.' <*> pList1 pDigit
        pExponent = ((:) <$> pAnySym "eE" <*>
                     (pOptSign <*> pList1 pDigit)) `opt` []
        pOptSign = ((('+':) <$ (pSym '+')) <|> (('-':) <$ (pSym '-'))) `opt` id

-- | Sign parser for numbers.
pSign :: (Num a) => Parser (a -> a)
pSign = (id <$ (pSym '+')) <|> (negate <$ (pSym '-')) `opt` id

-- | Lexeme parser for integers.
pInteger :: Num a => Parser a
pInteger = lexeme pIntegerRaw

-- | Lexeme parser for doubles.
pDouble :: Parser Double
pDouble = lexeme pDoubleRaw

-- | Lexeme parser for commas.
pComma :: Parser Char
pComma = lexeme $ pSym ','

-- | Bracketing parser creator.
brackets :: Char -> Char -> Parser a -> Parser a
brackets o c = pPacked (lexeme $ pSym o) (lexeme $ pSym c)

-- | Parenthesised values.
pParens :: Parser a -> Parser a
pParens = brackets '(' ')'

-- | Values in braces.
pBraces ::  Parser a -> Parser a
pBraces = brackets '{' '}'

-- | Bracketed values.
pBrackets ::  Parser a -> Parser a
pBrackets = brackets '[' ']'
