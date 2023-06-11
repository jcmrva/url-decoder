module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value, type_, checked, id, for, size)
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
    , showComponents : Bool
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ({ userUrl = "", fixedUrl = "", noSafelinks = False, showComponents = False }, Cmd.none)

type Msg
    = UrlUpdate String
    | ToggleSafelinks
    | ToggleComponents

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdate url ->
            ({ model | userUrl = url }, Cmd.none)
        ToggleSafelinks ->
            ({ model | noSafelinks = not model.noSafelinks }, Cmd.none)
        ToggleComponents ->
            ({ model | showComponents = not model.showComponents }, Cmd.none)

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Document Msg
view model =
    { title = "URL Decoder"
    , body =
        [ div [ ]
            [
              input [ type_ "checkbox", checked model.noSafelinks, onClick ToggleSafelinks, id "safelinks" ] []
            , label [ for "safelinks" ] [ text "Remove Safe Links" ]
            , input [ type_ "checkbox", checked model.showComponents, onClick ToggleComponents, id "components" ] [ ]
            , label [ for "components" ] [ text "Show Components" ]
            , br [] []
            , input [ value model.userUrl, onInput UrlUpdate, size 100 ] []
            , p [ ] [ model.userUrl |> Url.percentDecode |> Maybe.withDefault "invalid url" |> text ]
            , p [ ] [ if model.showComponents then (model.userUrl |> Url.fromString |> urlDisplay) else text ""  ]
            ]
        ]
    }


urlDisplay : Maybe Url.Url -> Html Msg
urlDisplay url =
    case url of
        Just u ->
            ul []
                [ li [] [ "protocol: " ++ (u.protocol |> protocolString) |> text]
                , li [] [ "host: " ++ u.host |> text]
                , li [] [ "path: " ++ u.path |> text]
                , li [] [ "query: " ++ (u.query |> Maybe.withDefault "") |> text]
                , li [] [ "fragment: " ++ (u.fragment |> Maybe.withDefault "") |> text]
            ]
        Nothing ->
            "invalid url" |> text

protocolString : Url.Protocol -> String
protocolString p =
    case p of
    Url.Http -> "HTTP"
    Url.Https -> "HTTPS"





