module Quiptonomicon exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, (:=))
import Line
import Quote
import String
import Task


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { quotes : List Quote
    , newLines : List Line
    , uid : Int
    }


type alias Line =
    { id : Int
    , model : Line.Model
    }


type alias Quote =
    { id : Int
    , model : Quote.Model
    }


type Msg
    = NoOp
    | Add
    | Modify Int Line.Msg
    | NewQuote Quote.Msg
    | FetchSucceed (List Quote)
    | FetchFail Http.Error


createQuote : List Line -> Int -> Quote
createQuote lines id =
    { id = id
    , model =
        List.map .model lines
        --maybe we will need the line ids later??
    }


init : ( Model, Cmd Msg )
init =
    ( { newLines = [ Line 0 (Line.init "" "") ]
      , uid = 1
      , quotes = []
      }
    , getQuotations
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchFail _ ->
            -- TODO: I guess indicate an error?
            ( { model | quotes = [ { id = 0, model = [ { speaker = "poop", words = "scoop" } ] } ] }, Cmd.none )

        FetchSucceed quotes ->
            ( { model | quotes = quotes }, Cmd.none )

        Modify id msg ->
            let
                updatedNewLines =
                    List.map (updateHelp id msg) model.newLines

                newLinesComplete =
                    List.all lineIsComplete updatedNewLines

                additionalLines =
                    if newLinesComplete then
                        [ Line model.uid (Line.init "" "") ]
                    else
                        []
            in
                ( { model
                    | newLines = updatedNewLines ++ additionalLines
                    , uid =
                        if newLinesComplete then
                            model.uid + 1
                        else
                            model.uid
                  }
                , Cmd.none
                )

        NewQuote _ ->
            ( model, Cmd.none )

        Add ->
            let
                quoteToAdd =
                    createQuote (List.filter lineIsComplete model.newLines) 0

                isInvalid =
                    List.isEmpty quoteToAdd.model

                ( newQuote, newQuoteCmds ) =
                    Quote.update Quote.SaveQuote quoteToAdd.model
            in
                if isInvalid then
                    ( model, Cmd.none )
                else
                    ( { model
                        | newLines = [ Line model.uid (Line.init "" "") ]
                        , quotes = quoteToAdd :: model.quotes
                        , uid = model.uid + 1
                      }
                    , Cmd.map NewQuote newQuoteCmds
                    )


lineIsComplete : Line -> Bool
lineIsComplete line =
    let
        wordsEmpty =
            String.isEmpty line.model.words

        speakerEmpty =
            String.isEmpty line.model.speaker
    in
        (not wordsEmpty) && (not speakerEmpty)


updateHelp : Int -> Line.Msg -> Line -> Line
updateHelp targetId msg { id, model } =
    Line id
        (if targetId == id then
            Line.update msg model
         else
            model
        )



-- VIEW


renderLine : Line.Model -> Html Msg
renderLine line =
    Html.map (\_ -> NoOp) (Line.view line)


renderQuote : Quote -> Html Msg
renderQuote quote =
    div []
        ((List.map renderLine quote.model) ++ [ hr [] [] ])


renderLineForm : Line -> Html Msg
renderLineForm { id, model } =
    Html.map (Modify id) (Line.form model)


view : Model -> Html Msg
view model =
    div []
        <| List.map renderQuote model.quotes
        ++ List.map renderLineForm model.newLines
        ++ [ button [ onClick Add ] [ text "Add" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getQuotations : Cmd Msg
getQuotations =
    let
        url =
            "//localhost:3000/quotations?select=id,lines{speaker,words}"
    in
        Task.perform FetchFail FetchSucceed (Http.get decoderQuotes url)


decoderQuote : Decoder Quote
decoderQuote =
    Decode.object2 Quote
        ("id" := Decode.int)
        ("lines"
            := Decode.list
                (Decode.object2 Line.Model
                    ("speaker" := Decode.string)
                    ("words" := Decode.string)
                )
        )


decoderQuotes : Decoder (List Quote)
decoderQuotes =
    Decode.list decoderQuote
