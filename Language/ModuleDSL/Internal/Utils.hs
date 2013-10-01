module Language.ModuleDSL.Internal.Utils where

import Prelude

dupquotes :: String -> String
dupquotes [] = []
dupquotes ('"':cs) = '"':'"':dupquotes cs
dupquotes (c:cs) = c:dupquotes cs
