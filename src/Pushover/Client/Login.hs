{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pushover.Client.Login
    ( Email
    , Password
    , Secret
    , Login(..)
    , login
    ) where

import Data.Text (Text)
import Data.Aeson
import Data.String (IsString)

import Pushover.Client.HTTP
import Pushover.Client.Response

newtype Email = Email Text
    deriving (IsString, ToParam)

newtype Password = Password Text
    deriving (IsString, ToParam)

newtype Secret = Secret Text
    deriving (IsString, FromJSON, ToJSON, ToParam)

data Login = Login !Secret

instance FromJSON Login where
    parseJSON = withObject "Login" $ fmap Login . (.: "secret")

login :: Email -> Password -> IO (Response Login)
login e p = requestJSON "/users/login.json" $ urlEncodedBody
    [ ("email", toParam e)
    , ("password", toParam p)
    ]
