{-# LANGUAGE OverloadedStrings #-}
module Pushover.Client.Response
    ( Response(..)
    , fromResponse
    , tryResponse
    , decodeResponse
    ) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)

import qualified Control.Exception as E

data Response a
    = OK a                      -- ^ All good
    | Exception !String         -- ^ Exception invoking API
    | JSONInvalid !String       -- ^ JSON response wasn't valid
    | JSONErrors !Value         -- ^ API had errors
    deriving Show

instance FromJSON a => FromJSON (Response a) where
    parseJSON = withObject "Response" $ \o -> do
        status <- o .: "status"

        if status == (1 :: Int)
            then OK <$> parseJSON (Object o)
            else JSONErrors <$> (o .: "errors")

-- | Perform a @'Response'@-yielding action, rescuing as @'Exception'@
tryResponse :: IO (Response a) -> IO (Response a)
tryResponse = (either err id <$>) . E.try
  where
    err :: E.SomeException -> Response a
    err = Exception . show

-- | Decode a (likely HTTP) response into @'OK'@ or @'JSONInvalid'@
decodeResponse :: FromJSON a => ByteString -> Response a
decodeResponse = either JSONInvalid id . eitherDecode

-- | Just call @'error'@ if not @'OK'@
fromResponse :: Response a -> a
fromResponse (OK a) = a
fromResponse (Exception e) = errorWithoutStackTrace $ "Exception: " ++ e
fromResponse (JSONInvalid e) = errorWithoutStackTrace $ "JSON parse error: " ++ e
fromResponse (JSONErrors es) = errorWithoutStackTrace $ "API errors: " ++ show es
