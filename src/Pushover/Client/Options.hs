module Pushover.Client.Options
    ( Action(..)
    , Options(..)
    , parseOptions
    ) where

import Data.Semigroup ((<>))
import Data.String (fromString)
import Options.Applicative
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath ((</>))

import Pushover.Client.Login (Email, Password)
import Pushover.Client.Device (DeviceName)

data Action
    = Receive
    | Register
    { oEmail :: !Email
    , oPassword :: !Password
    , oDeviceName :: !DeviceName
    }

data Options = Options
    { oCredentials :: FilePath
    , oAction :: Action
    }

parser :: FilePath -> Parser Options
parser fp = Options
    <$> pure fp
    <*> subparser
        (  command "receive" (recieve `withInfo` "Display all unread notifications")
        <> command "register" (register `withInfo` "Register this device")
        )

recieve :: Parser Action
recieve = pure Receive

register :: Parser Action
register = Register
    <$> (fromString <$> argument str (metavar "EMAIL"))
    <*> (fromString <$> argument str (metavar "PASSWORD"))
    <*> (fromString <$> argument str (metavar "DEVICE_NAME"))

parseOptions :: IO Options
parseOptions = do
    fp <- getDefaultCredentials
    execParser $ parser fp `withInfo` "See <command> --help for details"

withInfo :: Parser a -> String -> ParserInfo a
withInfo p d = info (p <**> helper) (fullDesc <> progDesc d)

getDefaultCredentials :: IO FilePath
getDefaultCredentials = (</>)
    <$> getUserCacheDir "pushover-client"
    <*> pure "device.json"
