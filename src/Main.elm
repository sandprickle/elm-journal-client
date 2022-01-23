module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h3, input, p, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http


baseUrl : String
baseUrl =
    "https://phmnhghlz9.execute-api.us-east-1.amazonaws.com"



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Status
    = Failure
    | Loading
    | Success


type alias JournalEntry =
    { content : String
    , timestamp : String
    , journalId : String
    }


type alias Model =
    { journalId : String
    , content : String
    , message : String
    , status : Status
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { journalId = "elm-test"
      , content = ""
      , message = ""
      , status = Success
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Content String
    | JournalId String
    | SubmitEntry
    | SentEntry (Result Http.Error String)
    | GetEntries


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Content newContent ->
            ( { model | content = newContent }, Cmd.none )

        JournalId newJournalId ->
            ( { model
                | journalId = newJournalId
              }
            , Cmd.none
            )

        SentEntry result ->
            ( model, Cmd.none )

        SubmitEntry ->
            ( model, Cmd.none )

        GetEntries ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "inputs" ]
            [ div []
                [ input
                    [ placeholder "Journal ID"
                    , value model.journalId
                    , onInput JournalId
                    ]
                    []
                ]
            , div []
                [ textarea
                    [ placeholder "Journal entry goes here."
                    , value model.content
                    , onInput Content
                    ]
                    []
                ]
            , button
                [ class "submit"
                , onClick SubmitEntry
                ]
                [ text "Submit Entry" ]
            ]
        , p [ class "message" ] [ text model.message ]
        ]
