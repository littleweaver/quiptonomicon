module Quote exposing (..)

import Html exposing (..)
import Line
import Http exposing (RawError, Response, send, defaultSettings, string)
import Task
import Json.Decode as Json


-- MODEL


type alias Model =
    List Line.Model


init : List Line.Model -> Model
init lines =
    lines



-- UPDATE


type Msg
    = SaveQuote
    | SaveSucceeded Int
    | SaveFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveQuote ->
            ( model, getQuoteId )

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


getQuoteId : Cmd Msg
getQuoteId =
    let
        url =
            "//localhost:3000/quotations"

        body =
            string """{"created_at": "now()"}"""
    in
        Task.perform SaveFail SaveSucceeded (postPreferRepresentation url body)


decodeQuoteId : Json.Decoder Int
decodeQuoteId =
    Json.at [ "id" ] Json.int
