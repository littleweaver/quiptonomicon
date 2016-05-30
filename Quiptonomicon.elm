module Quiptonomicon exposing (..)

import Line
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html
import String


main =
    Html.beginnerProgram { model = init, view = view, update = update }



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
    , lines : List Line
    }


type Msg
    = NoOp
    | Add
    | Modify Int Line.Msg


newQuote : List Line -> Int -> Quote
newQuote lines id =
    { id = 0
    , lines = lines
    }


init : Model
init =
    { newLines = [ Line 3 (Line.init "" "") ]
    , uid = 4
    , quotes = []
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Modify id msg ->
            let
                newLinesComplete =
                    List.all lineIsComplete model.newLines

                updatedNewLines =
                    List.map (updateHelp id msg) model.newLines

                additionalLines =
                    if newLinesComplete then
                        [ Line model.uid (Line.init "" "") ]
                    else
                        []
            in
                { model
                    | newLines = updatedNewLines ++ additionalLines
                    , uid =
                        if newLinesComplete then
                            model.uid + 1
                        else
                            model.uid
                }

        Add ->
            let
                quoteToAdd =
                    newQuote (List.filter lineIsComplete model.newLines) 0

                isInvalid =
                    List.isEmpty quoteToAdd.lines
            in
                if isInvalid then
                    model
                else
                    { model
                        | newLines = [ Line model.uid (Line.init "" "") ]
                        , quotes = quoteToAdd :: model.quotes
                        , uid = model.uid + 1
                    }


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


renderLine : Line -> Html Msg
renderLine { id, model } =
    Html.map (\_ -> NoOp) (Line.view model)


renderQuote : Quote -> Html Msg
renderQuote quote =
    div []
        ((List.map renderLine quote.lines) ++ [ hr [] [] ])


renderLineForm : Line -> Html Msg
renderLineForm { id, model } =
    Html.map (Modify id) (Line.form model)


view : Model -> Html Msg
view model =
    div []
        <| List.map renderQuote model.quotes
        ++ List.map renderLineForm model.newLines
        ++ [ button [ onClick Add ] [ text "Add" ] ]
