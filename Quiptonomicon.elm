module Quiptonomicon exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.App as Html


-- MODEL


type alias Model =
    { quotes : List Quote
    , speakerInput : String
    , wordsInput : String
    }


type alias Quote =
    { speaker : String
    , words : String
    }


type Msg
    = NoOp
    | UpdateSpeakerInput String
    | UpdateWordsInput String
    | Add


newQuote : String -> String -> Quote
newQuote speaker words =
    { words = words
    , speaker = speaker
    }


initialModel =
    { speakerInput = ""
    , wordsInput = ""
    , quotes =
        [ newQuote "David Randolph" "Parsifal is the kind of opera that starts at six o'clock and after it has been going three hours, you look at your watch and it says 6:20."
        , newQuote "Igor Stravinsky" "Why is it that whenever I hear a piece of music I don't like, it's always by Villa Lobos?"
        ]
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Add ->
            let
                quoteToAdd =
                    newQuote model.speakerInput model.wordsInput
            in
                { model
                    | speakerInput = ""
                    , wordsInput = ""
                    , quotes = quoteToAdd :: model.quotes
                }

        UpdateWordsInput content ->
            { model | wordsInput = content }

        UpdateSpeakerInput content ->
            { model | speakerInput = content }



-- VIEW


renderQuote : Quote -> Html Msg
renderQuote quote =
    div []
        [ text (quote.speaker ++ ": " ++ quote.words) ]


quoteForm : Model -> Html Msg
quoteForm model =
    div []
        [ input
            [ type' "text"
            , placeholder "Dehaka"
            , value model.speakerInput
            , name "speaker"
            , autofocus True
            , onInput UpdateSpeakerInput
            ]
            []
        , input
            [ type' "text"
            , placeholder "I am Dehaka, one who collects."
            , value model.wordsInput
            , name "words"
            , onInput UpdateWordsInput
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div []
        (List.map renderQuote model.quotes ++ [ quoteForm model, button [ onClick Add ] [ text "Add" ] ])


main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }
