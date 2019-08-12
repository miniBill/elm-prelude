module Http
  ( Error(..)
  , Expect
  , expectString
  , get
  ) where

import           CLI.Types.Internal      (Cmd (..))
import           Compat                  (Either (..), Monad (..))
import           Data.ByteString.Lazy    (toStrict)
import           Data.Text.Encoding      (decodeUtf8')
import           Network.HTTP.Client     (httpLbs, newManager, parseRequest,
                                          responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Result                  (Result (..))

--import qualified Result
import qualified String

get :: String -> Expect msg -> Cmd msg
get url (Expect expect) =
  Cmd
    [ do manager <- newManager tlsManagerSettings
         request <- parseRequest $ String.toList url
         response <- httpLbs request manager
         return $
           expect $
           case decodeUtf8' $ toStrict $ responseBody response of
             Left _  -> Err $ BadBody "Invalid UTF-8 inside body"
             Right t -> Ok t
    ]

newtype Expect msg =
  Expect (Result Error String -> msg)

expectString :: (Result Error String -> msg) -> Expect msg
expectString = Expect

data Error
  = BadUrl String
  | Timeout
  | NetworkError
  | BadStatus Int
  | BadBody String
