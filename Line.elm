module Line exposing (Model, Msg, init, update, view, form)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MODEL


type alias Model =
    { speaker : String
    , words : String
    }


init : String -> String -> Model
init speaker words =
    Model speaker words



-- UPDATE


type Msg
    = UpdateSpeaker String
    | UpdateWords String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSpeaker speaker ->
            { model | speaker = speaker }

        UpdateWords words ->
            { model | words = words }


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ text (model.speaker ++ ": " ++ model.words) ]


form : Model -> Html Msg
form model =
    div []
        [ input
            [ type' "text"
            , placeholder "Speaker"
            , value model.speaker
            , name "speaker"
            , autofocus True
            , onInput UpdateSpeaker
            ]
            []
        , input
            [ type' "text"
            , placeholder "What was said"
            , value model.words
            , name "words"
            , onInput UpdateWords
            ]
            []
        ]
