{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pushover.Client.Device
    ( DeviceId
    , DeviceName
    , Device(..)
    , registerDevice
    ) where

import Data.Text (Text)
import Data.Aeson
import Data.String (IsString)

import Pushover.Client.HTTP
import Pushover.Client.Login (Secret)
import Pushover.Client.Response

newtype DeviceId = DeviceId Text
    deriving (FromJSON, Show, ToJSON, ToParam, ToPathPiece)

newtype DeviceName = DeviceName Text
    deriving (IsString, ToParam)

data Device = Device !DeviceId

instance FromJSON Device where
    parseJSON = withObject "Device" $ fmap Device . (.: "id")

registerDevice :: Secret -> DeviceName -> IO (Response Device)
registerDevice s dn = requestJSON "/devices.json" $ urlEncodedBody
    [ ("secret", toParam s)
    , ("name", toParam dn)
    , ("os", "O")
    ]
