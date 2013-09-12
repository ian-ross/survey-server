module Utils where

import Import
import System.Random
import Data.Char
import qualified Data.Text as T


data AlertType = Error | Warn | Info | OK

setAlert :: AlertType -> Text -> Handler ()
setAlert typ msg = do
  let cls = ("alert" :: Text) <> case typ of
        Error -> " alert-error"
        Warn  -> ""
        Info  -> " alert-info"
        OK    -> " alert-success"
  htmlmsg <- giveUrlRenderer [hamlet|
  <div class="#{cls}">
    <button type="button" .close data-dismiss="alert">
      &times;
    #{msg}
  |]
  setMessage htmlmsg

generateHash :: IO Text
generateHash = do
  cs <- randomRs (0, 35) <$> getStdGen
  return . T.pack . map xform . take 32 $ cs
    where xform i
            | i < 10 = chr $ i + ord '0'
            | otherwise = chr $ (i - 10) + ord 'A'
