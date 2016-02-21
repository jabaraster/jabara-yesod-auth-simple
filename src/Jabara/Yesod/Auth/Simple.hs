{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Jabara.Yesod.Auth.Simple (
  YesodAuthSimple(..)
  , authSimplePlugin
  , loginR
  )
  where

import           Prelude
import           Yesod.Auth          (Auth, AuthPlugin (..), AuthRoute,
                                      Creds (..), Route (..), YesodAuth,
                                      loginErrorMessageI, setCredsRedirect)
import qualified Yesod.Auth.Message  as Msg
import           Yesod.Core
import           Yesod.Form

import           Data.Text           (Text, pack)

authName :: Text
authName = "authSimplePlugin"

loginR :: AuthRoute
loginR = PluginR authName ["login"]

class (YesodAuth site) => YesodAuthSimple site where

  -- | Check whether given user name exists among hardcoded names.
  doesUserNameExist :: Text -> HandlerT site IO Bool

  -- | Validate given user name with given password.
  validatePassword :: Text -> Text -> HandlerT site IO Bool

authSimplePlugin :: YesodAuthSimple m => AuthPlugin m
authSimplePlugin =
  AuthPlugin authName dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch m ps = notFound
    loginWidget toMaster = do
      token   <- getRequest >>= return . maybe "" id . reqToken
      [whamlet|
      <form method="post" action="@{toMaster loginR}">
        <input type=hidden name=#{defaultCsrfParamName} value=#{token}>
        <div .form-group>
          <label>ユーザー
          <input type=text name=username required .form-control>
        <div .form-group>
          <label>パスワード
          <input type=password name=password required .form-control>
        <button type=submit .btn .btn-success .btn-imp>ログイン
        <hr>
      |]

postLoginR :: (YesodAuthSimple master)
           => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR =
  do (username, password) <- lift (runInputPost
       ((,) <$> ireq textField "username"
            <*> ireq textField "password"))
     isValid <- lift (validatePassword username password)
     if isValid
        then lift (setCredsRedirect (Creds authName username []))
        else do isExists <- lift (doesUserNameExist username)
                loginErrorMessageI LoginR
                                   (if isExists
                                       then Msg.InvalidUsernamePass
                                       else Msg.IdentifierNotFound username)

