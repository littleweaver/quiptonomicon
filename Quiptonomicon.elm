module Quiptonomicon exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Line
import Quote
import String


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
    , lines : List Line
    }


type Msg
    = NoOp
    | Add
    | Modify Int Line.Msg
    | NewQuote Quote.Msg


createQuote : List Line -> Int -> Quote
createQuote lines id =
    { id = 0
    , lines = lines
    }


init : (Model, Cmd Msg)
init =
    ({ newLines = [ Line 0 (Line.init "" "") ]
    , uid = 1
    , quotes = []
    }, Cmd.none)



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
            ( model, Cmd.none)

        Add ->
            let
                quoteToAdd =
                    createQuote (List.filter lineIsComplete model.newLines) 0

                isInvalid =
                    List.isEmpty quoteToAdd.lines

                (newQuote, newQuoteCmds) =
                    Quote.update Quote.SaveQuote newQuote
            in
                if isInvalid then
                    ( model, Cmd.none)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
