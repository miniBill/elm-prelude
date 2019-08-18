module Http
  ( Error(..)
  , Expect
  , expectString
  , get
  ) where

import           CLI.Types.Internal        (Cmd (..))
import           Compat                    (Either (..), Monad (..), catch,
                                            fromIntegral)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Text.Encoding        (decodeUtf8')
import           Network.HTTP.Client       (HttpException (..),
                                            HttpExceptionContent (..), httpLbs,
                                            newManager, parseUrlThrow,
                                            responseBody, responseStatus)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (Status (..))
import           Result                    (Result (..))

--import qualified Result
import qualified String

get :: String -> Expect msg -> Cmd msg
get url (Expect expect) =
  Cmd
    [ case parseUrlThrow $ String.toList url of
        Nothing -> return $ expect $ Err $ BadUrl url
        Just request ->
          let op = do
                manager <- newManager tlsManagerSettings
                response <- httpLbs request manager
                return $
                  expect $
                  case decodeUtf8' $ toStrict $ responseBody response of
                    Left _  -> Err $ BadBody "Invalid UTF-8 inside body"
                    Right t -> Ok t
              handler (InvalidUrlException _ err) = BadUrl $ String.fromList err
              handler (HttpExceptionRequest _ err) =
                let go (StatusCodeException response _) =
                      BadStatus $
                      fromIntegral $ statusCode $ responseStatus response
                    go (TooManyRedirects _) = BadBody "Too many redirects"
                    go OverlongHeaders = BadBody "Overlong Headers"
                    go ResponseTimeout = Timeout
                    go ConnectionTimeout = Timeout
                    go (ConnectionFailure _) = NetworkError
                    go (InvalidStatusLine _) = BadBody "Invalid status line"
                    go (InvalidHeader _) = BadBody "Invalid header"
                    go (InvalidRequestHeader _) =
                      BadUrl "Invalid request header"
                    go (InternalException _) = NetworkError
                    go (ProxyConnectException _ _ _) = NetworkError
                    go NoResponseDataReceived =
                      BadBody "No response data received"
                    go TlsNotSupported = BadUrl "TLS not supported"
                    go (WrongRequestBodyStreamSize _ _) =
                      BadUrl "Request body had the wrong size"
                    go (ResponseBodyTooShort _ _) =
                      BadBody "Response body too short"
                    go InvalidChunkHeaders = BadBody "Invalid chunk headers"
                    go IncompleteHeaders = BadBody "Incomplete headers"
                    go (InvalidDestinationHost _) =
                      BadUrl "Invalid destination host"
                    go (HttpZlibException _) =
                      BadBody "The response was invalid compressed data"
                    go (InvalidProxyEnvironmentVariable _ _) = NetworkError
                    go ConnectionClosed = NetworkError
                    go (InvalidProxySettings _) = NetworkError
                 in go err
           in catch op (return . expect . Err . handler)
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
