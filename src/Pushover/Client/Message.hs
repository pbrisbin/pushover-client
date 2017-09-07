{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Pushover.Client.Message
    ( MessageId
    , Message(..)
    , Messages(..)
    , getMessages
    , UpdateHighest(..)
    , updateHighestMessage
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Pushover.Client.Device (DeviceId)
import Pushover.Client.HTTP
import Pushover.Client.Login (Secret)
import Pushover.Client.Response

newtype MessageId = MessageId Int
    deriving (Eq, FromJSON, Ord, Show, ToJSON, ToParam)

data Message = Message
    { mId :: !MessageId
    , mMessage :: !Text
    , mApp :: !Text
    , mAid :: !Int
    , mIcon :: !Text
    , mDate :: !Int
    , mPriority :: !Int
    , mAcked :: !Int
    , mUmid :: !Int
    , mTitle :: !Text
    }

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o -> Message
        <$> o .: "id"
        <*> o .: "message"
        <*> o .: "app"
        <*> o .: "aid"
        <*> o .: "icon"
        <*> o .: "date"
        <*> o .: "priority"
        <*> o .: "acked"
        <*> o .: "umid"
        <*> o .: "title"

instance ToJSON Message where
    toJSON Message{..} = object
        [ "id" .= mId
        , "message" .= mMessage
        , "app" .= mApp
        , "aid" .= mAid
        , "icon" .= mIcon
        , "date" .= mDate
        , "priority" .= mPriority
        , "acked" .= mAcked
        , "umid" .= mUmid
        , "title" .= mTitle
        ]

newtype Messages = Messages [Message]
    deriving Generic

instance FromJSON Messages where
    parseJSON = withObject "Messages" $ \o -> Messages
        <$> o .: "messages"

instance ToJSON Messages

data UpdateHighest = UpdateHighest

instance FromJSON UpdateHighest where
    -- Just asserts (and allows) that we got an Object in the response
    parseJSON = withObject "UpdateHighest" $ \_ -> pure UpdateHighest

getMessages :: Secret -> DeviceId -> IO (Response Messages)
getMessages s d = requestJSON "/messages.json" $ setQueryString
    [ ("secret", Just $ toParam s)
    , ("device_id", Just $ toParam d)
    ]

updateHighestMessage :: Secret -> DeviceId -> MessageId -> IO (Response UpdateHighest)
updateHighestMessage s d m = do
    let path = "/devices/" ++ toPathPiece d ++ "/update_highest_message.json"

    requestJSON path $ urlEncodedBody
        [ ("secret", toParam s)
        , ("message", toParam m)
        ]
