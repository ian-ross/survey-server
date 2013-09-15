module Handler.Admin
       ( getUserAdminR
       , postDeleteUserR
       , postModifyUserR
       ) where

import Import
import Angular.UIRouter
import Layouts
import Utils

getUserAdminR :: Handler Html
getUserAdminR = do
  users <- runDB $ selectList [] [Asc UserId]
  runAngularUIWithLayout appLayout $ do
    $(addSharedModule "alerts" True)
    $(buildStateUI "user-admin")

postDeleteUserR :: UserId -> Handler Html
postDeleteUserR uid = do
  requireAdmin
  runDB $ delete uid
  redirect UserAdminR

postModifyUserR :: UserId -> Int -> Handler Html
postModifyUserR uid admin = do
  requireAdmin
  runDB $ update uid [UserIsAdmin =. (admin == 1)]
  redirect UserAdminR
