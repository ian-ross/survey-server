{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import Import
import Data.Char
import Control.Monad (when)
import Control.Monad.Random
import qualified Data.Text as T


generateHash :: IO Text
generateHash = do
  cs <- evalRandIO (getRandomRs (0, 35))
  let ret = T.pack . map xform . take 32 $ cs
  putStrLn $ "\n\n\ngenerateHash => " ++ T.unpack ret ++ "\n\n\n"
  return ret
    where xform i
            | i < 10 = chr $ i + ord '0'
            | otherwise = chr $ (i - 10) + ord 'A'

entityToIntId :: KeyBackend b e -> Int
entityToIntId ent = do
  case fromPersistValue . unKey $ ent of
    Right (uid::Int) -> uid
    Left _           -> (-1)

isAdmin :: Handler Bool
isAdmin = do
  uid <- requireAuthId
  runDB $ maybe False userIsAdmin <$> get uid

requireAdmin :: Handler ()
requireAdmin = do
  admin <- isAdmin
  when (not $ admin) $ invalidArgs ["Must be admin"]

userAdminFlag :: User -> Text
userAdminFlag u = if userIsAdmin u
                  then "true"
                  else "false"
