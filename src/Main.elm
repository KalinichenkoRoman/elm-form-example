module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (String)
import String exposing (String)
import List



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if isValid model then
    div [ style "color" "green" ] [ text "Great Password" ]
  else
    div [ style "color" "red" ] [ text "Bad Password" ]

isValid : Model -> Bool
isValid model =
  if String.length model.password >= 8 
  && model.password == model.passwordAgain
  && hasUpperCase model.password
  && hasLowerCase model.password 
  && hasNumeric model.password 
  then
    True
  else
    False

hasUpperCase : String -> Bool
hasUpperCase password = String.any isUpper password 

hasLowerCase : String -> Bool
hasLowerCase password = String.any isLower password 


hasNumeric : String -> Bool
hasNumeric password = String.any isNumeric password 


isUpper : Char -> Bool
isUpper letter = 
  letter == Char.toUpper letter


isLower : Char -> Bool
isLower letter = 
  letter == Char.toLower letter

isNumeric : Char -> Bool
isNumeric letter = Char.isDigit letter