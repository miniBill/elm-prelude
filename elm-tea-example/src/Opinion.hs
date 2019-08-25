module Main
  ( main
  ) where

import           CLI    (CLI, Cmd, Sub)
import qualified CLI
import qualified Cmd
import qualified Http
import           Result (Result (..))
import qualified String
import qualified Sub

main :: IO ()
main = CLI.run_ $ CLI.element init view update subscriptions

-- MODEL
data Model
  = Failure
  | Loading
  | Success String

init :: () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      "https://elm-lang.org/assets/public-opinion.txt"
      (Http.expectString GotText))

-- UPDATE
data Msg =
  GotText (Result Http.Error String)

update :: Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          ( Success $
            String.replace "!!!NEWLINE!!!" "\n\n" .
            String.replace "\n" "" . String.replace "\n\n" "!!!NEWLINE!!!" $
            String.replace "\r" "" fullText
          , Cmd.none)
        Err _ -> (Failure, Cmd.none)

-- SUBSCRIPTIONS
subscriptions :: Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW
view :: Model -> CLI Msg
view model =
  case model of
    Failure          -> CLI.text "I was unable to load your book."
    Loading          -> CLI.text "Loading..."
    Success fullText -> CLI.text fullText
