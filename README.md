# yesod-auth-simple

Yesodの認証プラグラインです。

ユーザー名とパスワードを入力する画面、及びその画面からログイン成功までの動作シーケンスのみを提供します。

元ネタはこちら。  
[yesod-auth-hardcoded](https://hackage.haskell.org/package/yesod-auth-1.4.12/docs/Yesod-Auth-Hardcoded.html)

```models
User
    name Text
    password ByteString
    emailAddress Text
    verified Bool
    verifyKey Text
    resetPasswordKey Text
    UniqueUser name
    deriving Typeable
```

```Foundation.hs
import Jabara.Yesod.Auth.Simple (YesodAuthSimple(..), authSimplePlugin)
import Jabara.Yesod.Auth.FreeAccount (verifyPassword)

instance YesodAuthSimple App where
    doesUserNameExist userName = do
        mUser <- runDB $ getBy $ UniqueUser userName
        return $ isJust mUser
    validatePassword userName password = do
        mUser <- runDB $ getBy $ UniqueUser userName
        case mUser of
            Nothing   -> return False
            Just user -> return $ verifyPassword password (userPassword $ rec' user)
      where
        rec' :: Entity a -> a
        rec' (Entity _ a) = a
```

```Foundation.hs
    authPlugins _ = [authSimplePlugin]
```
