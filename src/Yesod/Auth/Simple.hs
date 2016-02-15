{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Yesod.Auth.Simple (
  YesodAuthHardcoded(..)
  , authHardcoded
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

loginR :: AuthRoute
loginR = PluginR "appauth" ["login"]

class (YesodAuth site) => YesodAuthHardcoded site where

  -- | Check whether given user name exists among hardcoded names.
  doesUserNameExist :: Text -> HandlerT site IO Bool

  -- | Validate given user name with given password.
  validatePassword :: Text -> Text -> HandlerT site IO Bool

authHardcoded :: YesodAuthHardcoded m => AuthPlugin m
authHardcoded =
  AuthPlugin "appauth" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch m ps = $logDebug m >> $logDebug (pack $ show ps) >> notFound
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

postLoginR :: (YesodAuthHardcoded master)
           => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR =
  do (username, password) <- lift (runInputPost
       ((,) <$> ireq textField "username"
            <*> ireq textField "password"))
     isValid <- lift (validatePassword username password)
     if isValid
        then lift (setCredsRedirect (Creds "appauth" username []))
        else do isExists <- lift (doesUserNameExist username)
                loginErrorMessageI LoginR
                                   (if isExists
                                       then Msg.InvalidUsernamePass
                                       else Msg.IdentifierNotFound username)

