module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h3, input, p, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, at, field, map3, string)
import Time


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
    , journalEntries : List JournalEntry
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { journalId = "elm-test"
      , content = ""
      , message = ""
      , status = Success
      , journalEntries = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Content String
    | JournalId String
    | SubmitEntry
    | SentRequest (Result Http.Error String)
    | GetJournalEntries
    | GotJournalEntries (Result Http.Error String)
    | UpdateMessage String


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

        UpdateMessage newMessage ->
            ( { model | message = newMessage }, Cmd.none )

        _ ->
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



-- HELPERS


timestamp : Time.Posix -> String
timestamp posix =
    String.fromInt (Time.posixToMillis posix)


getJournalEntries : String -> Cmd Msg
getJournalEntries journalId =
    Http.get
        { url = baseUrl ++ "/entries?journal=" ++ journalId
        , expect = Http.expectJson GotJournalEntries journalEntryListDecoder
        }


journalEntryDecoder : Decoder JournalEntry
journalEntryDecoder =
    map3 JournalEntry
        (at [ "journalId" ] string)
        (at [ "timestamp" ] string)
        (at [ "content" ] string)


journalEntryListDecoder : Decoder
