{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import Import
import Data.Char
import Control.Monad (when)
import Control.Monad.Random
import qualified Data.Text as T


-- | Generate a 32-character base-36 hash, i.e. a random string
-- composed of digits and upper case letters.
generateHash :: IO Text
generateHash = do
  cs <- evalRandIO (getRandomRs (0, 35))
  return . T.pack . map xform . take 32 $ cs
    where xform i
            | i < 10 = chr $ i + ord '0'
            | otherwise = chr $ (i - 10) + ord 'A'

-- | Persistent key conversion.
entityToIntId :: KeyBackend b e -> Int
entityToIntId ent = do
  case fromPersistValue . unKey $ ent of
    Right (uid::Int) -> uid
    Left _           -> (-1)

-- | Determine whether a user has admin privileges.
isAdmin :: Handler Bool
isAdmin = do
  uid <- requireAuthId
  runDB $ maybe False userIsAdmin <$> get uid

-- | Admin gatekeeper handler.
requireAdmin :: Handler ()
requireAdmin = do
  admin <- isAdmin
  when (not $ admin) $ invalidArgs ["Must be admin"]

-- | Helper for rendering user admin flag.
userAdminFlag :: User -> Text
userAdminFlag u = if userIsAdmin u
                  then "true"
                  else "false"
