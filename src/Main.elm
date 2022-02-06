module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Task
import Time exposing (posixToMillis)


baseUrl : String
baseUrl =
    "https://btjsz1qlnf.execute-api.us-east-1.amazonaws.com"



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


type CurrentView
    = Edit
    | Read


type alias Model =
    { currentJournalId : String
    , currentContent : String
    , status : Status
    , currentView : CurrentView
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentJournalId = "elm-test"
      , currentContent = ""
      , status = None
      , currentView = Edit
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
    | ClickedEdit
    | ClickedRead


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

                url : String
                url =
                    baseUrl ++ "/entries"

                body : Http.Body
                body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "journalId", Encode.string model.currentJournalId )
                            , ( "timestamp", Encode.int timestamp )
                            , ( "content", Encode.string model.currentContent )
                            ]
                        )

                expect : Http.Expect Msg
                expect =
                    Http.expectWhatever PostedEntry
            in
            ( model
            , Http.post
                { url = url
                , body = body
                , expect = expect
                }
            )

        PostedEntry (Ok _) ->
            ( { model | status = Success, currentContent = "" }, Cmd.none )

        PostedEntry (Err _) ->
            ( { model | status = Failure }, Cmd.none )

        ClickedEdit ->
            ( { model | currentView = Edit }, Cmd.none )

        ClickedRead ->
            ( { model | currentView = Read }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.currentView of
        Edit ->
            div []
                [ viewHeader model
                , viewEditor model
                ]

        Read ->
            div []
                [ viewHeader model
                , viewReader model
                ]


viewHeader : Model -> Html Msg
viewHeader model =
    header
        [ class "my-8 flex justify-between items-center" ]
        [ viewSwitcher model.currentView
        , div []
            [ span
                [ class "mr-4" ]
                [ text "Current Journal:" ]
            , input
                [ placeholder "Journal ID"
                , type_ "text"
                , value model.currentJournalId
                , onInput NewJournalId
                , class "input text-gray-400 focus:text-gray-300 bg-gray-800"
                ]
                []
            ]
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    div
        [ class "editor mt-4 sm:mt-8" ]
        [ div
            [ class "mb-8" ]
            [ textarea
                [ placeholder "Journal entry goes here."
                , rows 10
                , cols 60
                , value model.currentContent
                , onInput NewContent
                , class "bg-gray-700 w-full input"
                ]
                []
            ]
        , div
            [ class "flex items-center justify-between" ]
            [ button
                [ class "px-4 py-2 bg-cyan-900 rounded-full hover:bg-cyan-700"
                , onClick SubmittedEntry
                ]
                [ text "Submit Entry" ]
            , viewStatus model.status
            ]
        ]


viewReader : Model -> Html Msg
viewReader model =
    p [] [ text "reader" ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Failure ->
            p [ class "status" ] [ text "Something went wrong!" ]

        Loading ->
            p [ class "status" ] [ text "Loading..." ]

        Success ->
            p [ class "status" ] [ text "Success!" ]

        None ->
            p [ class "status" ] [ text "Nothing to report." ]


viewSwitcher : CurrentView -> Html Msg
viewSwitcher currentView =
    let
        selectedClass =
            "bg-cyan-700 px-4 py-2"

        deselectedClass =
            "bg-cyan-900 hover:bg-cyan-600 px-4 py-2"
    in
    case currentView of
        Edit ->
            div
                [ class "rounded overflow-hidden" ]
                [ button
                    [ class selectedClass ]
                    [ text "Edit" ]
                , button
                    [ class deselectedClass
                    , onClick ClickedRead
                    ]
                    [ text "Read" ]
                ]

        Read ->
            div
                [ class "rounded" ]
                [ button
                    [ class deselectedClass
                    , onClick ClickedEdit
                    ]
                    [ text "Edit" ]
                , button
                    [ class selectedClass ]
                    [ text "Read" ]
                ]
