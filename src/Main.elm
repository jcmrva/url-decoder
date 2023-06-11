module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value, type_, checked)
import Html.Events exposing (onClick)
import Url

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
    , noSafelinks : Bool
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ({ userUrl = "", fixedUrl = "", noSafelinks = False }, Cmd.none)

type Msg
    = UrlUpdate String
    | ToggleSafelinks

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdate url ->
            ({ model | userUrl = url }, Cmd.none)
        ToggleSafelinks ->
            ({ model | noSafelinks = not model.noSafelinks }, Cmd.none)

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Document Msg
view model =
    { title = "URL Decoder"
    , body =
        [ div [ ]
            [ input [ value model.userUrl, onInput UrlUpdate ] []
            , input [ type_ "checkbox", checked model.noSafelinks, onClick ToggleSafelinks ] []
            , p [ ] [ model.userUrl |> Url.percentDecode |> Maybe.withDefault "invalid url" |> text ]
            ]
        ]
    }









