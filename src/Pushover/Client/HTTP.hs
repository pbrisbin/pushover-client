module Pushover.Client.HTTP
    ( requestJSON
    , httpJSON
    , newRequest
    , ToParam(..)
    , setQueryString
    , urlEncodedBody
    , ToPathPiece(..)
    ) where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import qualified Pushover.Client.Response as R

class ToParam a where
    toParam :: a -> ByteString

instance ToParam Int where
    toParam = C8.pack . show

instance ToParam Text where
    toParam = encodeUtf8

class ToPathPiece a where
    toPathPiece :: a -> String

instance ToPathPiece Text where
    toPathPiece = T.unpack

requestJSON :: FromJSON a => String -> (Request -> Request) -> IO (R.Response a)
requestJSON p = httpJSON <=< newRequest p

newRequest :: MonadThrow m => String -> (Request -> Request) -> m Request
newRequest p f =
    (silent . f) <$> parseUrlThrow (concat [apiBase, "/", apiVersion, "/", p'])
  where
    p' = dropWhile (== '/') p
    silent r = r { checkResponse = \_ _ -> return () }

httpJSON :: FromJSON a => Request -> IO (R.Response a)
httpJSON req = R.tryResponse $ do
    mgr <- newManager tlsManagerSettings
    R.decodeResponse . responseBody <$> httpLbs req mgr

apiBase :: String
apiBase = "https://api.pushover.net"

apiVersion :: String
apiVersion = "1"
