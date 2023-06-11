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
    { inputUrl : String
    , decodedUrl : Maybe String
    , noSafelinks : Bool
    , showComponents : Bool
    , x : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ({ inputUrl = "", decodedUrl = Nothing, x = "", noSafelinks = False, showComponents = False }, Cmd.none)

type Msg
    = UrlUpdate String
    | ToggleSafelinks
    | ToggleComponents

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        decoded noSF url = (if noSF then removeSafelink url else url) |> Url.percentDecode
    in
    case msg of
        UrlUpdate url ->
            ({ model | inputUrl = url, decodedUrl = decoded model.noSafelinks url }, Cmd.none)
        ToggleSafelinks ->
            let
                sf = not model.noSafelinks
            in
            ({ model | noSafelinks = sf, decodedUrl = decoded sf model.inputUrl }, Cmd.none)
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
            , input [ value model.inputUrl, onInput UrlUpdate, size 100 ] []
            , p [ ] [ model.decodedUrl |> Maybe.withDefault "" |> text ]
            , p [ ] [ if model.showComponents then (model.decodedUrl |> (Maybe.andThen Url.fromString) |> urlDisplay) else text "" ]
            ]
        ]
    }

removeSafelink : String -> String
removeSafelink url =
    if String.contains "safelinks" url then
        let
            idx = String.indexes "url=" url
        in
            case idx of
            i :: _ ->
                String.dropLeft (i + 4) url
            [] ->
                url
    else
        url

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
