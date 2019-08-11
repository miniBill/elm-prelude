module Main
  ( main
  ) where

import           CLI            (AlignmentType (..), CLI, attributes, column,
                                 input, row, text)
import qualified CLI
import           CLI.Attributes (foregroundColor)
import qualified Color

data Model =
  Model
    { name          :: String
    , password      :: String
    , passwordAgain :: String
    }

main :: IO ()
main = CLI.run_ $ CLI.sandbox init view update

init :: Model
init = Model "" "" ""

data Msg
  = Name String
  | Password String
  | PasswordAgain String

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Name name'                   -> model {name = name'}
    Password password'           -> model {password = password'}
    PasswordAgain passwordAgain' -> model {passwordAgain = passwordAgain'}

view :: Model -> CLI Msg
view (model@(Model name' password' passwordAgain')) =
  column
    AlignStart
    [ viewInput CLI.TypeText "Name" name' Name
    , viewInput CLI.TypePassword "Password" password' Password
    , viewInput CLI.TypePassword "Password (again)" passwordAgain' PasswordAgain
    , viewValidation model
    ]

viewInput :: CLI.InputType -> String -> String -> (String -> msg) -> CLI msg
viewInput t p v toMsg = row AlignCenter [text $ p ++ ":", input t v toMsg]

viewValidation :: Model -> CLI msg
viewValidation model =
  if password model == passwordAgain model
    then attributes [foregroundColor Color.green] $ text "OK"
    else attributes [foregroundColor Color.red] $ text "Passwords do not match!"
