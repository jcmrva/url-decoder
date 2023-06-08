module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)


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
    = UrlUpdate String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdate url ->
            ({model | userUrl = url }, Cmd.none)

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Document Msg
view model =
    { title = "Safelinks Remover"
    , body =
        [ div [ ]
            [ input [ value model.userUrl, onInput UrlUpdate ] []
            ]
        ]
    }









