module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h3, input, p, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode exposing (encode, int, string)
import Task
import Time exposing (millisToPosix, posixToMillis)


baseUrl : String
baseUrl =
    "https://fv5c7jlkul.execute-api.us-east-1.amazonaws.com"



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
    | None


type alias JournalEntry =
    { journalId : String
    , timestamp : Int
    , content : String
    }


type alias Model =
    { currentJournalId : String
    , currentContent : String
    , status : Status
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentJournalId = "elm-test"
      , currentContent = ""
      , status = None
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewContent String
    | NewJournalId String
    | SubmittedEntry
    | PostedEntry (Result Http.Error ())
    | GotTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent newContent ->
            ( { model | currentContent = newContent }, Cmd.none )

        NewJournalId newJournalId ->
            ( { model
                | currentJournalId = newJournalId
              }
            , Cmd.none
            )

        SubmittedEntry ->
            ( { model | status = Loading }, Task.perform GotTime Time.now )

        GotTime time ->
            let
                timestamp : Int
                timestamp =
                    posixToMillis time

                body : Encode.Value
                body =
                    Encode.object
                        [ ( "journalId", Encode.string model.currentJournalId )
                        , ( "timestamp", Encode.int timestamp )
                        , ( "content", Encode.string model.currentContent )
                        ]
            in
            ( model
            , Http.post
                { url = baseUrl ++ "/entries"
                , body = Http.jsonBody body
                , expect = Http.expectWhatever PostedEntry
                }
            )

        PostedEntry (Ok response) ->
            ( { model | status = Success, currentContent = "" }, Cmd.none )

        PostedEntry (Err error) ->
            ( { model | status = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "inputs" ]
            [ div
                []
                [ input
                    [ placeholder "Journal ID"
                    , value model.currentJournalId
                    , onInput NewJournalId
                    ]
                    []
                ]
            , div
                []
                [ textarea
                    [ placeholder "Journal entry goes here."
                    , value model.currentContent
                    , onInput NewContent
                    ]
                    []
                ]
            , button
                [ class "submit"
                , onClick SubmittedEntry
                ]
                [ text "Submit Entry" ]
            ]
        , div
            [ class "status" ]
            [ viewStatus model.status ]
        ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Failure ->
            p [] [ text "Something went wrong!" ]

        Loading ->
            p [] [ text "Loading..." ]

        Success ->
            p [] [ text "Success!" ]

        None ->
            p [] [ text "Nothing to report." ]
