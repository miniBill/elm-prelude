module Http
  ( get
  ) where

import           Compat
import           Data.ByteString.Lazy (toStrict)
import           Data.Text.Encoding   (decodeUtf8)
import           Network.HTTP.Client  (defaultManagerSettings, httpLbs,
                                       newManager, parseRequest, responseBody)
import qualified String

get :: String -> IO String
get url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ String.toList url
  response <- httpLbs request manager
  return $ decodeUtf8 $ toStrict $ responseBody response
