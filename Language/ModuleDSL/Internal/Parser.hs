{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}
module Language.ModuleDSL.Internal.Parser where

import Prelude
import Data.Char
import Data.Generics hiding (empty)
import Data.Text (Text)
import qualified Data.Text as T
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Language.ModuleDSL.Syntax


-- | Basic string-based parser type.
type Parser a = P (Str Char String LineColPos) a


-- | Parse a name component: starts with a letter, then zero or more
-- letters, digits, underscores or dashes.
pNameComponent :: Parser Text
pNameComponent = T.pack <$> ((:) <$> pIdIniChar <*> pList pIdChar)
  where pIdIniChar = pLower <|> pUpper
        pIdChar = pLower <|> pUpper <|> pDigit <|> pAnySym "_-"

-- | Raw parser for names: a period separated sequence of name
-- components.
pNameRaw :: Parser Name
pNameRaw = (Name . T.intercalate ".") <$> pList1Sep (pSym '.') pNameComponent

-- | Lexeme parser for names.
pName :: Parser Name
pName = lexeme pNameRaw `micro` 2

-- | Parse a value.
pLiteral :: Parser Literal
pLiteral =  (String . T.pack) <$> pStringLiteral
        <|> head <$> amb ((\v -> Double . maybe v (const $ v / 100.0)) <$>
                          pDouble <*> pMaybe (pSym '%') <|>
                          Integer <$> pInteger)
        <|> Bool <$> pBool
        <|> pReturn Null <* pSymbol "null"
  where pBool =  pReturn True  <* (pSymbol "true"  <|> pSymbol "yes")
             <|> pReturn False <* (pSymbol "false" <|> pSymbol "no") `micro` 1

-- | Normalise unary negation applications on literal numbers.
normalise :: Data a => a -> a
normalise = everywhere (mkT norm)
  where norm (UnaryExpr NegOp (LitExpr (Integer i))) = LitExpr (Integer (-i))
        norm (UnaryExpr NegOp (LitExpr (Double d))) = LitExpr (Double (-d))
        norm x = x

-- | Parse an expression (top-level: if/then/else).
pExpr :: Parser Expr
pExpr = normalise <$>
        ((IfThenElseExpr <$ pSymbol "if"   <*> pExpr
                         <* pSymbol "then" <*> pExpr
                         <* pSymbol "else" <*> pExpr)
         <|> pExprOr)

-- | Parse an expression (logical OR).
pExprOr :: Parser Expr
pExprOr = pChainl ((BinaryExpr OrOp) <$ pSymbol "or") pExprAnd

-- | Parse an expression (logical AND).
pExprAnd :: Parser Expr
pExprAnd =  pChainl ((BinaryExpr AndOp) <$ pSymbol "and") pExprComp

-- | Parse an expression (comparison).
pExprComp :: Parser Expr
pExprComp =  (\l op r -> BinaryExpr op l r) <$>
               pExprAdd <*> pCompOp <*> pExprAdd
         <|> pExprAdd
  where pCompOp =  EqOp    <$ pSymbol "=="
               <|> NEqOp   <$ pSymbol "!="
               <|> GtOp    <$ pSymbol ">"
               <|> GEqOp   <$ pSymbol ">="
               <|> LtOp    <$ pSymbol "<"
               <|> LeqOp   <$ pSymbol "<="
               <|> EqCIOp  <$ pSymbol "@=="
               <|> NEqCIOp <$ pSymbol "@!="
               <|> GtCIOp  <$ pSymbol "@>"
               <|> GEqCIOp <$ pSymbol "@>="
               <|> LtCIOp  <$ pSymbol "@<"
               <|> LeqCIOp <$ pSymbol "@<="

-- | Parse an expression (additive).
pExprAdd :: Parser Expr
pExprAdd =
  pChainl (prioOps [(AddOp, "+"), (SubOp, "-"), (CatOp, "<>")]) pExprMul

-- | Parse an expression (multiplicative).
pExprMul :: Parser Expr
pExprMul =  pChainl (prioOps [(MulOp, "*"), (DivOp, "/")]) pExprPow

-- | Parse an expression (power operator).
pExprPow :: Parser Expr
pExprPow =  pChainr ((BinaryExpr PowOp) <$ pSymbol "^") pExprUnary

-- | Parse an expression (unary).
pExprUnary :: Parser Expr
pExprUnary =  UnaryExpr <$> pUnaryOp <*> pExprUnary
          <|> pExprPrim

-- | Parse an expression (primitive).
pExprPrim :: Parser Expr
pExprPrim =  LitExpr <$> pLiteral
         <|> RefExpr <$> pName
         <|> FunExpr <$> pName <*> pExprList
         <|> pParens pExpr

-- | Parse a unary operator.
pUnaryOp :: Parser UnaryOp
pUnaryOp =  NegOp   <$ pSymbol "-"
        <|> NotOp   <$ pSymbol "!"


-- | Parse a comma-separated expression list.
pExprList :: Parser [Expr]
pExprList = pParens (pList1Sep pComma pExpr `opt` [])

-- | Helper for parsing binary expressions.
prioOps :: [(BinaryOp, String)] -> Parser (Expr -> Expr -> Expr)
prioOps oss = foldr (<|>) empty [(BinaryExpr o) <$ pSymbol s | (o, s) <- oss]

-- | String literals: delimited by double quotes, these can contain
-- any Unicode printing character.  The only character escaping is the
-- use of '""' to represent embedded double quotes.
pStringLiteral :: Parser String
pStringLiteral = lexeme $ pSym '"' *> pList ch <* pSym '"'
  where ch = head <$> pToken "\"\"" <|> pSatisfy (\c -> isPrint c && c /= '"')
             (Insertion  "Character in a string" 'y' 5)

-- | Parse a single option setting.
pOption :: Parser Option
pOption = Option <$> pName <* pSymbol "=" <*> pExpr

-- | Parse a list of options.
pOptions :: Parser [Option]
pOptions = pBrackets (pListSep pComma pOption) `opt` []

-- | Parse a single choice.
pChoice :: Parser Choice
pChoice = (Choice . T.pack) <$> pQuotedString <* pSymbol "=>" <*> pExpr

-- | Parse a list of choices.
pChoices :: Parser [Choice]
pChoices = pBraces (pListSep pComma pChoice)

-- | Parse a single question.
pQuestion :: Parser Question
pQuestion =  (\n t os -> NumericQuestion n (T.pack t) os) <$>
               pName <* pSymbol "=" <* pSymbol "NumericQuestion" <*>
               pQuotedString <*> pOptions
         <|> (\n t os cs -> ChoiceQuestion n (T.pack t) os cs) <$>
               pName <* pSymbol "=" <* pSymbol "ChoiceQuestion" <*>
               pQuotedString <*> pOptions <*> pChoices
         <|> (\n t os cs -> DropdownQuestion n (T.pack t) os cs) <$>
               pName <* pSymbol "=" <* pSymbol "DropdownQuestion" <*>
               pQuotedString <*> pOptions <*> pChoices
         <|> (\n t os -> TextEntryQuestion n (T.pack t) os) <$>
               pName <* pSymbol "=" <* pSymbol "TextEntryQuestion" <*>
               pQuotedString <*> pOptions
         <|> (TextDisplay) <$ pSymbol "TextDisplay" <*>
               pExpr <*> pOptions

-- | Parse a list of question definitions of the form "n = q".
pQuestions :: Parser [Question]
pQuestions = pMany pQuestion

-- | Parse a top-level definition: either a survey page or a
-- parameterised specialisation of a question type.
pTopLevel :: Parser TopLevel
pTopLevel =  Specialisation <$ pSymbol "Specialisation" <*>
               pName <*> (pParens (pListSep pComma pName) `opt` [])
               <* pSymbol "=" <*> pQuestion
         <|> SurveyPage <$ pSymbol "SurveyPage" <*>
               pName <*> pOptions <*> pQuestions
         <|> Function <$ pSymbol "Function" <*>
               pName <*> (pParens (pListSep pComma pName))
               <* pSymbol "=" <*> pExpr

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
pIntegerRaw = pNaturalRaw <?> "Integer"

-- | Raw (non lexeme) parser for doubles.
pDoubleRaw :: (Read a) => Parser a
pDoubleRaw = read <$> pDoubleStr

-- | Parse textual representation of a floating point number.
pDoubleStr :: Parser  [Char]
pDoubleStr = (pToken "Infinity" <|> pPlainDouble) <?> "Double (eg 3.4e-5)"
  where pPlainDouble = (++) <$> ((++) <$> pList1 pDigit <*>
                                 (pFraction `opt` [])) <*> pExponent
        pFraction = (:) <$> pSym '.' <*> pList1 pDigit
        pExponent = ((:) <$> pAnySym "eE" <*>
                     (pOptSign <*> pList1 pDigit)) `opt` []
        pOptSign = ((('+':) <$ (pSym '+')) <|> (('-':) <$ (pSym '-'))) `opt` id

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


-- | Test function...
ptest :: Parser a -> String -> (a, [Error LineColPos])
ptest p s = parse ((,) <$ pSpaces <*> p <* pSpaces <*> pEnd)
            (createStr (LineColPos 0 0 0) s)
