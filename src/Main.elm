module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (value, type_, checked, id, for, size, href, class)
import Html.Events exposing (onInput, onClick)
import Url
import Tuple exposing (first, second)

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
    , clickable : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
    (
        { inputUrl = ""
        , decodedUrl = Nothing
        , noSafelinks = False
        , clickable = False
        , showComponents = True }
    , Cmd.none)

type Msg
    = UrlUpdate String
    | ToggleSafelinks
    | ToggleComponents
    | ToggleClickableUrl

update : Msg -> Model -> (Model, Cmd Msg)
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
        ToggleClickableUrl ->
            ({ model | clickable = not model.clickable }, Cmd.none)

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Document Msg
view model =
    let
        chkbox : String -> Bool -> String -> msg -> Html msg
        chkbox id_ val txt msg =
            div [ class "chkbox" ]
                [ input [ type_ "checkbox", checked val, onClick msg, id id_ ] []
                , label [ for id_ ] [ text txt ]
                ]
    in
    { title = "URL Decoder"
    , body =
        [ div []
            [ chkbox "safelinks" model.noSafelinks "Remove Safe Links" ToggleSafelinks
            , chkbox "components" model.showComponents "Show Components" ToggleComponents
            , chkbox "clickable" model.clickable "Clickable Decoded URL" ToggleClickableUrl
            , br [] []
            , input [ value model.inputUrl, onInput UrlUpdate, size 100 ] []
            , p [] [ formatDecodedUrl model.clickable model.decodedUrl ]
            , p [] [ if model.showComponents then (model.decodedUrl |> (Maybe.andThen Url.fromString) |> urlDisplay) else text "" ]
            ]
        ]
    }

formatDecodedUrl : Bool -> Maybe String -> Html msg
formatDecodedUrl clickable decodedUrl =
    case (clickable, decodedUrl) of
    (True, Just url) ->
        a [ href url ] [ url |> text ]
    _ ->
        decodedUrl |> Maybe.withDefault "" |> text

removeSafelink : String -> String
removeSafelink url =
    let
        idxs = String.indexes "url=" url
    in
        case idxs of
        idx :: _ ->
            String.dropLeft (idx + 4) url
        [] ->
            url

urlDisplay : Maybe Url.Url -> Html Msg
urlDisplay url =
    case url of
        Just u ->
            ul []
                [ li [] [ "protocol: " ++ (u.protocol |> protocolString) |> text ]
                , li [] [ "host: " ++ u.host |> text ]
                , li [] [ "path: " ++ u.path |> text ]
                , li [] [ "query: " |> text, (u.query |> Maybe.withDefault "") |> viewQueryString ]
                , li [] [ "fragment: " ++ (u.fragment |> Maybe.withDefault "") |> text ]
            ]
        Nothing ->
            "invalid url" |> text

protocolString : Url.Protocol -> String
protocolString p =
    case p of
    Url.Http -> "HTTP"
    Url.Https -> "HTTPS"

parseQueryString : String -> List (String, Maybe String)
parseQueryString qs =
    let
        qsKV kv =
            String.split "=" kv
        parseKVP kvs =
            case kvs of
            k :: "" :: _ ->
                (Just k, Nothing)
            k :: v :: _ ->
                (Just k, Just v)
            "" :: _ ->
                (Nothing, Nothing)
            k :: _ ->
                (Just k, Nothing)
            [] ->
                (Nothing, Nothing)
    in
    String.split "&" qs
    |> List.map (qsKV >> parseKVP)
    |> List.filter ((/=) (Nothing, Nothing))
    |> List.map (\kv -> (first kv |> Maybe.withDefault "___", second kv))

viewQueryString : String -> Html Msg
viewQueryString qs =
    let
        formatKV : (String, Maybe String) -> Html Msg
        formatKV kv =
            li [] [ first kv ++ ": " ++ (second kv |> Maybe.withDefault "") |> text ]
    in
    div [] [ ul [] (qs |> parseQueryString |> List.map formatKV) ]
