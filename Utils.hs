module Utils where

import Import

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
