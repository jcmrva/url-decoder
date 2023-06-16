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
    , decodedUrl : Maybe Url.Url
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
        decoded noSF url =
            (if noSF then removeSafelink url else Just url) |> Maybe.andThen Url.fromString
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
            , p [] [ if model.showComponents then (model.decodedUrl |> Maybe.map urlDisplay |> Maybe.withDefault (text "")) else text "" ] -- ugh
            ]
        ]
    }

formatDecodedUrl : Bool -> Maybe Url.Url -> Html msg
formatDecodedUrl clickable decodedUrl =
    case (clickable, decodedUrl) of
    (True, Just url) ->
        let
            urlStr = url |> Url.toString
        in
        a [ href urlStr ] [ urlStr |> text ]
    _ ->
        decodedUrl |> Maybe.map Url.toString |> Maybe.withDefault "" |> text

removeSafelink : String -> Maybe String
removeSafelink urlStr =
    let
        innerUrl qs =
            qs
            |> parseQueryString
            |> List.filter (\kv -> first kv == "url")
            |> List.head
            |> Maybe.andThen second
    in
    urlStr
    |> Url.fromString
    |> Maybe.andThen .query
    |> Maybe.andThen innerUrl
    |> Maybe.andThen Url.percentDecode

urlDisplay : Url.Url -> Html Msg
urlDisplay url =
    ul []
        [ li [] [ "protocol: " ++ (url.protocol |> protocolString) |> text ]
        , li [] [ "host: " ++ url.host |> text ]
        , li [] [ "path: " ++ url.path |> text ]
        , li [] [ "query: " |> text, (url.query |> Maybe.withDefault "") |> viewQueryString ]
        , li [] [ "fragment: " ++ (url.fragment |> Maybe.withDefault "") |> text ]
    ]

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
