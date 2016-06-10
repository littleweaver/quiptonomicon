module Quote exposing (..)

import Html exposing (..)
import Line
import Http exposing (RawError, Response, send, defaultSettings, string)
import Task
import Json.Decode as Json
import Json.Encode exposing (..)


-- MODEL


type alias Model =
    List Line.Model


init : List Line.Model -> Model
init lines =
    lines



-- UPDATE


type Msg
    = SaveQuote
    | SaveSucceeded (List Int)
    | SaveFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveQuote ->
            ( model, getQuoteId model )

        SaveSucceeded quoteId ->
            ( model, Cmd.none )

        SaveFail _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Hoagies" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


toValue : Int -> Line.Model -> Value
toValue quoteId line =
    Json.Encode.object
        [ ( "speaker", Json.Encode.string line.speaker )
        , ( "words", Json.Encode.string line.words )
        , ( "quotation_id", Json.Encode.int quoteId )
        ]


linesJson : Int -> Model -> String
linesJson quoteId lines =
    let
        lineValues =
            list (List.map (toValue quoteId) lines)
    in
        encode 0 lineValues


postPreferRepresentation : String -> Http.Body -> Platform.Task Http.Error Int
postPreferRepresentation url body =
    Http.fromJson decodeQuoteId
        <| send defaultSettings
            { verb = "POST"
            , headers =
                [ ( "Prefer", "return=representation" )
                , ( "Content-Type", "application/json" )
                ]
            , url = url
            , body = body
            }


postLines : String -> Http.Body -> Platform.Task Http.Error (List Int)
postLines url body =
    Http.fromJson decodeLineIds
        <| send defaultSettings
            { verb = "POST"
            , headers =
                [ ( "Prefer", "return=representation" )
                , ( "Content-Type", "application/json" )
                ]
            , url = url
            , body = body
            }


getQuoteId : Model -> Cmd Msg
getQuoteId model =
    let
        url =
            "//limitless-tundra-10904.herokuapp.com/quotations"

        url2 =
            "//limitless-tundra-10904.herokuapp.com/lines"

        body =
            Http.string """{"created_at": "now()"}"""
    in
        Task.perform SaveFail
            SaveSucceeded
            ((postPreferRepresentation url body) `Task.andThen` (\theQuoteId -> postLines url2 (Http.string (linesJson theQuoteId model))))


decodeQuoteId : Json.Decoder Int
decodeQuoteId =
    Json.at [ "id" ] Json.int


decodeLineIds : Json.Decoder (List Int)
decodeLineIds =
    Json.list (Json.at [ "id" ] Json.int)
