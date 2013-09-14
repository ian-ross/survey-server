{-# LANGUAGE OverloadedStrings #-}
module Layouts
       ( landingLayout
       , appLayout
       ) where

import Import
import qualified Data.Text as T
import Utils

appLayout :: Widget -> Handler Html
appLayout = baseLayout True

landingLayout :: Widget -> Handler Html
landingLayout = baseLayout False

baseLayout :: Bool -> Widget -> Handler Html
baseLayout addLinks widget = do
  mmsg <- getMessage
  defaultLayout $ do
    makeNavbar addLinks
    [whamlet|
  <div.container>
    <div.row>
      <div.span12>
        $maybe msg <- mmsg
          ^{msg}

        ^{widget}
|]


makeNavbar :: Bool -> Widget
makeNavbar addLinks = do
  r <- getCurrentRoute
  admin <- handlerToWidget isAdmin
  let adminLink = if admin
                  then makeLink r "User admin" UserAdminR
                  else [whamlet| |]
  let homeLink = makeLink r "Home" HomeR
      modulesLink = makeLink r "Modules" ModulesR
      logoutLink = makeLink r "Logout" (AuthR LogoutR)
  [whamlet|
          <div .navbar>
            <div .navbar-inner>
              <a .brand href=@{HomeR}>openPsych
              $if addLinks
                <ul .nav>
                  ^{homeLink}
                  ^{modulesLink}
                <ul .nav .pull-right>
                  ^{adminLink}
                  ^{logoutLink}
|]


makeLink :: Maybe (Route App) -> Text -> Route App -> Widget
makeLink current label dest =
  let tst b s = if b then s else ""
      licls = T.intercalate " " $ filter (not . T.null)
              [tst (current == Just dest) "active"]
  in [whamlet|
    <li class="#{licls}">
      <a href=@{dest}>#{label}
|]
