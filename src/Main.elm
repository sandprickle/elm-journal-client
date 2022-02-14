module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes
    exposing
        ( class
        , cols
        , href
        , placeholder
        , rows
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Task
import Time exposing (posixToMillis)
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = \model -> { title = "Journal", body = [ view model ] }
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


type Route
    = Write
    | Browse
    | Login
    | Auth Token


type Token
    = Token String


type alias Model =
    { currentJournalId : String
    , currentContent : String
    , username : String
    , password : String
    , status : Status
    , currentView : CurrentView
    , route : Route
    , idToken : Maybe Token
    , accessToken : Maybe Token
    , url : Url
    , key : Nav.Key
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { currentJournalId = "elm-test"
      , currentContent = ""
      , username = ""
      , password = ""
      , status = None
      , currentView = Edit
      , route = Login
      , idToken = Just (Token "faketoken")
      , accessToken = Nothing
      , url = url
      , key = key
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewContent String
    | NewJournalId String
    | NewUsername String
    | NewPassword String
    | SubmittedEntry
    | PostedEntry (Result Http.Error ())
    | GotTime Time.Posix
    | ClickedEdit
    | ClickedRead
    | ClickedLogout
    | ClickedLogin
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest


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

        NewUsername newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        NewPassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        SubmittedEntry ->
            ( { model | status = Loading }, Task.perform GotTime Time.now )

        GotTime time ->
            let
                timestamp : Int
                timestamp =
                    posixToMillis time

                baseUrl : String
                baseUrl =
                    "https://btjsz1qlnf.execute-api.us-east-1.amazonaws.com"

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

        ClickedLogout ->
            ( { model
                | idToken = Nothing
                , accessToken = Nothing
              }
            , Cmd.none
            )

        ClickedLogin ->
            ( { model
                | username = ""
                , password = ""
                , idToken = Just (Token "SomeFakeToken")
              }
            , Cmd.none
            )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )



-- VIEW


view : Model -> Html Msg
view model =
    case model.idToken of
        Nothing ->
            div
                [ class "w-full h-screen flex items-center justify-center" ]
                [ viewLogin model ]

        Just _ ->
            viewApp model


viewLogin : Model -> Html Msg
viewLogin model =
    div
        [ class "p-8 rounded bg-gray-800" ]
        [ div
            [ class "" ]
            [ input
                [ type_ "text"
                , placeholder "username"
                , class "input mb-8 block bg-gray-700 text-center"
                , onInput NewUsername
                , value model.username
                ]
                []
            , input
                [ type_ "password"
                , placeholder "password"
                , class "input mb-8 block bg-gray-700 text-center"
                , onInput NewPassword
                , value model.password
                ]
                []
            , button
                [ class "input bg-cyan-900 w-full block hover:bg-cyan-800"
                , onClick ClickedLogin
                ]
                [ text "Login" ]
            ]
        ]


viewApp : Model -> Html Msg
viewApp model =
    case model.currentView of
        Edit ->
            div
                []
                [ viewHeader model
                , viewEditor model
                ]

        Read ->
            div
                []
                [ viewHeader model
                , viewReader model
                ]


viewHeader : Model -> Html Msg
viewHeader model =
    header
        [ class "my-8 flex justify-between items-center" ]
        [ div
            [ class "flex items-center" ]
            [ viewSwitcher model.currentView
            , button
                [ class "ml-8 underline text-sm"
                , onClick ClickedLogout
                ]
                [ text "Log Out" ]
            ]
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
                [ class "rounded overflow-hidden inline-block" ]
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
                [ class "rounded overflow-hidden inline-block" ]
                [ button
                    [ class deselectedClass
                    , onClick ClickedEdit
                    ]
                    [ text "Edit" ]
                , button
                    [ class selectedClass ]
                    [ text "Read" ]
                ]
