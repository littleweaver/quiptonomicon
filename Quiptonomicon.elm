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
    , uid : Int
    }


type alias Quote =
    { id : Int
    , lines : List Line
    }


type alias Line =
    { speaker : String
    , words : String
    }


type Msg
    = NoOp
    | UpdateSpeakerInput String
    | UpdateWordsInput String
    | Add


newQuote : String -> String -> Int -> Quote
newQuote speaker words id =
    { id = id
    , lines = [ { speaker = speaker, words = words } ]
    }


initialModel =
    { speakerInput = ""
    , wordsInput = ""
    , uid = 3
    , quotes =
        [ newQuote "David Randolph" "Parsifal is the kind of opera that starts at six o'clock and after it has been going three hours, you look at your watch and it says 6:20." 1
        , newQuote "Igor Stravinsky" "Why is it that whenever I hear a piece of music I don't like, it's always by Villa Lobos?" 2
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
                    newQuote model.speakerInput model.wordsInput model.uid
            in
                { model
                    | speakerInput = ""
                    , wordsInput = ""
                    , quotes = quoteToAdd :: model.quotes
                    , uid = model.uid + 1
                }

        UpdateWordsInput content ->
            { model | wordsInput = content }

        UpdateSpeakerInput content ->
            { model | speakerInput = content }



-- VIEW


renderLine : Line -> Html Msg
renderLine line =
    div []
        [ text (line.speaker ++ ": " ++ line.words) ]


renderQuote : Quote -> Html Msg
renderQuote quote =
    div []
        (List.map renderLine quote.lines)


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
