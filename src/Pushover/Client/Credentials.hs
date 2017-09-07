{-# LANGUAGE DeriveGeneric #-}
module Pushover.Client.Credentials
    ( Credentials(..)
    , readCredentials
    , writeCredentials
    ) where

import Data.Aeson
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import qualified Data.ByteString.Lazy.Char8 as C8

import Pushover.Client.Device (DeviceId)
import Pushover.Client.Login (Secret)

data Credentials = Credentials !Secret !DeviceId
    deriving Generic

instance FromJSON Credentials
instance ToJSON Credentials

readCredentials :: FilePath -> IO Credentials
readCredentials fp = either error id . eitherDecode <$> C8.readFile fp

writeCredentials :: FilePath -> Credentials -> IO ()
writeCredentials fp cs = do
    createDirectoryIfMissing True $ takeDirectory fp
    C8.writeFile fp $ encode cs
