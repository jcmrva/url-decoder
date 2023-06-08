module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Model =
    { userUrl : String
    , fixedUrl : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ({ userUrl = "", fixedUrl = ""}, Cmd.none)

type Msg
    = UrlUpdate

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdate ->
            (model, Cmd.none)

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Document Msg
view _ =
    { title = "tbd"
    , body =
        [ div [ ]
            [
            ]
        ]
    }









