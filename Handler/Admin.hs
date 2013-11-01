-- | Handler for user admin actions.
--
-- Just provides the possibility to delete a user and to add or remove
-- administrator rights to a user (only administrators can delete or
-- modify users).  New user accounts are created just by registering
-- on the site.
module Handler.Admin
       ( getUserAdminR
       , postDeleteUserR
       , postModifyUserR
       ) where

import Import
import Angular.UIRouter
import Layouts
import Utils

-- | Get user admin page.
--
-- Hamlet and Julius in: angular/ui-router/user-admin
getUserAdminR :: Handler Html
getUserAdminR = do
  users <- runDB $ selectList [] [Asc UserId]
  runAngularUIWithLayout appLayout $ do
    $(addSharedModule "alerts" True)
    $(buildStateUI "user-admin")

-- | Delete a user.
postDeleteUserR :: UserId -> Handler Html
postDeleteUserR uid = do
  requireAdmin
  runDB $ delete uid
  redirect UserAdminR

-- | Modify a user's admin status.
postModifyUserR :: UserId -> Int -> Handler Html
postModifyUserR uid admin = do
  requireAdmin
  runDB $ update uid [UserIsAdmin =. (admin == 1)]
  redirect UserAdminR
