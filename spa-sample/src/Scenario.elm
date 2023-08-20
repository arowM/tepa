module Scenario exposing
    ( main, test, sections, MarkupConfig
    , Section
    )

{-| Scenario

@docs main, test, sections, MarkupConfig

-}

import App exposing (Memory)
import App.Session as Session
import DebugToJson
import Dict
import Expect
import Html
import Html.Attributes as Attributes
import Json.Encode as JE
import Markdown
import String.Multiline exposing (here)
import Tepa exposing (Document, Layer)
import Tepa.Scenario as Scenario exposing (markup, setParam, userComment)
import Tepa.Time as Time
import Test exposing (Test)
import TimeZone
import Widget.Toast as Toast



-- # Expose


{-| Scenario document server.
-}
main : Tepa.Program ScenarioMemory
main =
    Tepa.application
        { init = init
        , procedure =
            \_ _ _ ->
                Tepa.bind (Tepa.newLayer ()) <|
                    \layer ->
                        [ Tepa.modify <|
                            \m ->
                                { m | page = Just layer }
                        , Tepa.void <|
                            Tepa.onLayer
                                { get = .page
                                , set = \l m -> { m | page = Just l }
                                }
                            <|
                                Tepa.sequence
                                    [ Tepa.neverResolved
                                    ]
                        ]
        , view =
            \{ page } ->
                case page of
                    Nothing ->
                        { title = "Loading"
                        , body = [ Html.text "Loading..." ]
                        }

                    Just layer ->
                        documentView layer
        , onUrlRequest = \_ _ _ -> Tepa.none
        , onUrlChange = \_ _ _ -> Tepa.none
        }


type alias ScenarioMemory =
    { page : Maybe (Layer ())
    }


init : ScenarioMemory
init =
    { page = Nothing
    }


documentView : Layer () -> Document
documentView =
    Tepa.layerView <|
        \{ setKey, checks } ->
            let
                devModeCheckboxId =
                    "devModeCheckboxId"

                config : MarkupConfig
                config =
                    { dev =
                        Dict.get devModeCheckboxId (Debug.log "checks" checks)
                            |> Maybe.withDefault False
                    }

                markdown =
                    Scenario.toMarkdown
                        { title = "Sample scenario"
                        , sections = sections config
                        , config = Scenario.en_US
                        }
            in
            { title = "User Scenario"
            , body =
                case markdown of
                    Err err ->
                        [ Html.text <|
                            Debug.toString err
                        ]

                    Ok content ->
                        [ Html.label
                            []
                            [ Html.input
                                (Attributes.type_ "checkbox"
                                    :: setKey devModeCheckboxId
                                )
                                []
                            , Html.text "Enable dev mode"
                            ]
                        , Markdown.toHtml [] content
                        ]
            }


{-| Test for `scenarios`.
-}
test : Test
test =
    Scenario.toTest
        { props = App.props
        , sections =
            sections
                { dev = True
                }
        }


type alias MarkupConfig =
    { dev : Bool
    }



-- # Users


sakuraChan : Scenario.User
sakuraChan =
    Scenario.defineUser
        { name = sakuraChanName
        }


sakuraChanName : String
sakuraChanName =
    "Sakura-chan"


warunasubiKun : Scenario.User
warunasubiKun =
    Scenario.defineUser
        { name = warunasubiKunName
        }


warunasubiKunName : String
warunasubiKunName =
    "Warunasubi-kun"



-- # Sessions


sakuraChanMainSession : Scenario.Session
sakuraChanMainSession =
    Scenario.defineSession
        { uniqueName = "Sakura-chan's main session"
        , user = sakuraChan
        }


sakuraChanSecondSession : Scenario.Session
sakuraChanSecondSession =
    Scenario.defineSession
        { uniqueName = "Sakura-chan's second session"
        , user = sakuraChan
        }


warunasubiKunMainSession : Scenario.Session
warunasubiKunMainSession =
    Scenario.defineSession
        { uniqueName = "Warunasubi-kun's main session"
        , user = warunasubiKun
        }



-- # Scenarios


onSakuraChanMainSession : App.ScenarioSet
onSakuraChanMainSession =
    App.scenario sakuraChanMainSession


onSakuraChanSecondSession : App.ScenarioSet
onSakuraChanSecondSession =
    App.scenario sakuraChanSecondSession


onWarunasubiKunMainSession : App.ScenarioSet
onWarunasubiKunMainSession =
    App.scenario warunasubiKunMainSession


type alias Section =
    Scenario.Section Memory


{-| -}
sections : MarkupConfig -> List Section
sections config =
    [ introduction1 config
    , introduction1_sub1 config
    , introduction1_1 config
    , pageHomeCase1 config
    , pageHomeCase2 config
    ]


pathPrefix : String
pathPrefix =
    "tepa"


introduction1 : MarkupConfig -> Section
introduction1 config =
    { title = "Introduction Scenario #1"
    , dependency =
        Scenario.EntryPoint
            (TimeZone.asia__tokyo ())
            (Time.millisToPosix 1672531200000)

    -- 2023-01-01 09:00:00 in Asia_Tokyo
    , content =
        [ userComment sakuraChan "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan "Today I'm going to try a chat application."
        , userComment sakuraChan "I'm trying to access the URL."
        , Scenario.loadApp sakuraChanMainSession
            (markup
                """
              **{{name}}**: Type the following URL in the address bar.

              ```
              https://example.com/
              ```
              """
                |> setParam "name" sakuraChanName
            )
            { path =
                { path = [ pathPrefix ]
                , queryParameters = Dict.empty
                , fragment = Nothing
                }
            , flags = JE.object []
            }
        , if config.dev then
            Scenario.systemComment sakuraChanMainSession <|
                "The client requests the user profile from the server."

          else
            Scenario.none
        , let
            responseMeta =
                { url = "https://example.com/api/profile"
                , statusCode = 401
                , statusText = "Unauthorized"
                , headers = Dict.empty
                }

            responseBody =
                here """
                {
                    "code": "LoginRequired"
                }
                """
          in
          onSakuraChanMainSession.app.receiveProfile
            (\_ ->
                Just ( responseMeta, responseBody )
            )
            (markup """
              The backend responds to the profile request.

              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.app.fetchProfileEndpoint)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , onSakuraChanMainSession.login.expectAvailable <|
            markup "Display login page."
        , userComment sakuraChan
            "I see I have to log in! I remember my dad gave me the account information beforehand."
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors <|
            markup "The login form shows no errors at first."
        , userComment warunasubiKun
            "I'm Warunasubi-kun. I'm going to play a prank on Sakura-chan. Muahahahahaha! 🍆😈"
        , userComment warunasubiKun
            "Sakura-chan, here is the account information note your father gave you. 😈"
        , userComment sakuraChan
            "Thank you, Warunasubi-kun. 🌸"
        , let
            value =
                "guest"
          in
          onSakuraChanMainSession.login.changeLoginId
            { value = value
            }
            (markup <| "Enter \"" ++ value ++ "\" in the login ID.")
        , userComment sakuraChan
            "The note says that the password can be left blank."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (markup "Click the login button.")
        , let
            error =
                "Password is required."
          in
          onSakuraChanMainSession.login.expectLoginFormShowError
            { error = error
            }
            (markup <| "Form shows error: " ++ error)
        , userComment sakuraChan
            "Oh my goat, I got an error..."
        , userComment warunasubiKun
            "Sorry, sorry, I just got a little naughty. Here's the real note."
        , userComment sakuraChan
            "Okay, I'll try again."
        , let
            value =
                "fuestPass"
          in
          onSakuraChanMainSession.login.changeLoginPass
            { value = value
            }
            (markup <| "Change the password to \"" ++ value ++ "\"")
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors
            (markup "Login form shows no errors at the time.")
        , userComment sakuraChan
            "It looks good."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (markup "Click the login button.")
        , Scenario.todo sakuraChanMainSession
            (markup """
            The login button becomes disabled
            """
            )
        , let
            requestBody =
                JE.object
                    [ ( "id", JE.string "guest" )
                    , ( "pass", JE.string "fuestPass" )
                    ]

            responseMeta =
                { url = "https://example.com/api/login"
                , statusCode = 401
                , statusText = "Unauthorized"
                , headers = Dict.empty
                }

            responseBody =
                here """
                {
                    "code": "IncorrectIdOrPassword"
                }
                """
          in
          onSakuraChanMainSession.login.receiveLoginResp
            (\body ->
                if body == requestBody then
                    Just ( responseMeta, responseBody )

                else
                    Nothing
            )
            (markup """
              The backend responds to the login request.

              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

                  ```json
                  {{requestBody|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.login.loginEndpoint)
                |> setParam "requestBody"
                    (JE.encode 4 requestBody)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , Scenario.todo sakuraChanMainSession
            (markup """
            The login button becomes reenabled.
            """
            )
        , let
            error =
                "Incorrect ID or password."
          in
          onSakuraChanMainSession.login.expectLoginFormShowError
            { error = error
            }
            (markup <| "The form shows error: " ++ error)
        , userComment sakuraChan "Oops!"
        , userComment warunasubiKun "Maybe you mistyped the password."
        , userComment sakuraChan "That might be true. It's hard to type with my two-fingered hooves..."
        , let
            value =
                "guestPass"
          in
          onSakuraChanMainSession.login.changeLoginPass
            { value = value
            }
            (markup <| "Enter \"" ++ value ++ "\" in the password field.")
        , onSakuraChanMainSession.login.clickSubmitLogin
            (markup "Click the login button.")
        , let
            requestBody =
                JE.object
                    [ ( "id", JE.string "guest" )
                    , ( "pass", JE.string "guestPass" )
                    ]
          in
          onSakuraChanMainSession.login.expectRequestLogin
            requestBody
            (markup """
              The client sends a login request to the backend with a timeout of 5000 milliseconds.

              ```json
              {{requestMeta|block}}
              ```

              ```json
              {{requestBody|block}}
              ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.login.loginEndpoint)
                |> setParam "requestBody"
                    (JE.encode 4 requestBody)
                |> setLogLevelDev config
            )
        , if config.dev then
            Scenario.systemComment sakuraChanMainSession <|
                "The client requests the user profile from the server with a timeout of 5000 milliseconds."

          else
            Scenario.none
        , Scenario.sleep
            (markup <| "5000 milliseconds have passed.")
            5000
        , let
            currentTime =
                1672531205000
          in
          Scenario.expectCurrentTime
            (markup <| "Current time in POSIX: " ++ String.fromInt currentTime ++ ".")
            { expectation =
                Expect.equal
                    (Time.millisToPosix currentTime)
            }
        , Scenario.expectHttpRequest
            sakuraChanMainSession
            (markup <| "The login request timed out.")
            { layer = onSakuraChanMainSession.login.layer
            , expectation =
                List.length
                    >> Expect.equal 0
            }
        , onSakuraChanMainSession.login.toast.expectErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (markup "A toast popup appears: \"Network error, please try again.\"")
        , userComment sakuraChan "Oops!"
        ]
    }


introduction1_sub1 : MarkupConfig -> Section
introduction1_sub1 config =
    { title = "Introduction Scenario #1-0"
    , dependency = Scenario.RunAfter (introduction1 config).title
    , content =
        [ onSakuraChanMainSession.login.toast.closeErrorsByMessage
            { message = "Network error, please check your network and try again."
            }
            (markup "Click the close button on the popup.")
        , onSakuraChanMainSession.login.toast.expectDisappearingErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (markup "The popup begins to disappear.")
        , Scenario.sleep
            (markup <| String.fromInt Toast.toastFadeOutDuration ++ " milliseconds passes.")
            Toast.toastFadeOutDuration
        , onSakuraChanMainSession.login.toast.expectNoMessages
            (markup "No toast popups now.")
        ]
    }


introduction1_1 : MarkupConfig -> Section
introduction1_1 config =
    { title = "Introduction Scenario #1-1"
    , dependency = Scenario.RunAfter (introduction1 config).title
    , content =
        [ Scenario.sleep
            (markup <| String.fromInt Toast.toastTimeout ++ " milliseconds passes.")
            Toast.toastTimeout
        , onSakuraChanMainSession.login.toast.expectDisappearingErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (markup "The popup begins to disappear.")
        , Scenario.sleep
            (markup <| String.fromInt Toast.toastFadeOutDuration ++ " milliseconds passes.")
            Toast.toastFadeOutDuration
        , onSakuraChanMainSession.login.toast.expectNoMessages
            (markup "No toast popups now.")
        , userComment sakuraChan "Try again."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (markup "Click the login button.")
        , Scenario.todo sakuraChanMainSession
            (markup """
            The login button becomes disabled
            """
            )
        , let
            requestBody =
                JE.object
                    [ ( "id", JE.string "guest" )
                    , ( "pass", JE.string "guestPass" )
                    ]

            responseMeta =
                { url = "https://example.com/api/login"
                , statusCode = 200
                , statusText = "OK"
                , headers =
                    Dict.fromList
                        [ ( "Set-Cookie"
                          , "auth_token=authenticated; Secure; HttpOnly; Domain=.example.com; Max-Age=2592000"
                          )
                        ]
                }

            responseBody =
                here """
                {
                    "profile": {
                        "id": "guest",
                        "name": "Guest"
                    }
                }
                """
          in
          onSakuraChanMainSession.login.receiveLoginResp
            (\body ->
                if body == requestBody then
                    Just ( responseMeta, responseBody )

                else
                    Nothing
            )
            (markup """
              The backend responds to the login request.

              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

                  ```json
                  {{requestBody|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.login.loginEndpoint)
                |> setParam "requestBody"
                    (JE.encode 4 requestBody)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , Scenario.todo sakuraChanMainSession
            (markup """
            The login button becomes reenabled.
            """
            )
        , onSakuraChanMainSession.login.receiveRandomLuckyHay
            { value = Session.LuckyHayAlfalfa
            }
            (markup "Client receives random value for lucky hay: Alfalfa"
                |> setLogLevelDev config
            )
        , onSakuraChanMainSession.home.expectAvailable
            (markup "Redirect to home page.")
        , let
            value =
                "Alfalfa"
          in
          onSakuraChanMainSession.home.expectLuckyHayMessage
            { value = value
            }
            (markup <| "The lucky grass hay is \"" ++ value ++ "\"")
        , let
            value =
                "2023-01-01 09:00:15"
          in
          onSakuraChanMainSession.home.expectClockMessage
            { value = value
            }
            (markup <| "The clock says \"" ++ value ++ "\"")
        , userComment sakuraChan "Yes!"
        ]
    }


pageHomeCase1 : MarkupConfig -> Section
pageHomeCase1 config =
    { title = "Home page #1"
    , dependency = Scenario.RunAfter (introduction1_1 config).title
    , content =
        [ let
            value =
                "Guest"
          in
          onSakuraChanMainSession.home.expectGreetingMessage
            { value = value
            }
            (markup <| "The greeting message says \"Hi, " ++ value ++ "!\"")
        , userComment sakuraChan "I'm Sakura-chan! Not \"Guest\"! 💢🐐"
        , let
            value =
                "Sakura-chan"
          in
          onSakuraChanMainSession.home.changeEditAccountFormAccountId
            { value = value
            }
            (markup <| "Change the name input field value to \"" ++ value ++ "\".")
        , onSakuraChanMainSession.home.clickSubmitEditAccount
            (markup "Click the save button.")
        , let
            requestBody =
                JE.object
                    [ ( "name", JE.string "Sakura-chan" )
                    ]

            responseMeta =
                { url = "https://example.com/api/edit-account"
                , statusCode = 200
                , statusText = "OK"
                , headers =
                    Dict.fromList
                        [ ( "Set-Cookie"
                          , "auth_token=authenticated; Secure; HttpOnly; Domain=.example.com; Max-Age=2592000"
                          )
                        ]
                }

            responseBody =
                here """
                {
                    "profile": {
                        "name": "Sakura-chan"
                    }
                }
                """
          in
          onSakuraChanMainSession.home.receiveEditAccountResp
            (\body ->
                if body == requestBody then
                    Just ( responseMeta, responseBody )

                else
                    Nothing
            )
            (markup """
              The backend responds to the edit account request.
              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.home.editAccountEndpoint)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , let
            value =
                "Sakura-chan"
          in
          onSakuraChanMainSession.home.expectGreetingMessage
            { value = value
            }
            (markup <| "The greeting message says \"Hi, " ++ value ++ "!\"")
        , Scenario.loadApp sakuraChanSecondSession
            (markup """
              **{{name}}**: Open new tab, and type the following URL in the address bar.

              ```
              https://example.com/
              ```
              """
                |> setParam "name" sakuraChanName
            )
            { path =
                { path = [ pathPrefix ]
                , queryParameters = Dict.empty
                , fragment = Nothing
                }
            , flags = JE.object []
            }
        , let
            responseMeta =
                { url = "https://example.com/api/profile"
                , statusCode = 200
                , statusText = "OK"
                , headers =
                    Dict.fromList
                        [ ( "Set-Cookie"
                          , "auth_token=authenticated; Secure; HttpOnly; Domain=.example.com; Max-Age=2592000"
                          )
                        ]
                }

            responseBody =
                here """
                {
                  "profile": {
                    "id": "guest",
                    "name": "Sakura-chan"
                  }
                }
                """
          in
          onSakuraChanSecondSession.app.receiveProfile
            (\_ ->
                Just ( responseMeta, responseBody )
            )
            (markup """
              The backend responds to the profile request.

              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.app.fetchProfileEndpoint)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , onSakuraChanSecondSession.app.receiveRandomLuckyHay
            { value = Session.LuckyHayTimothy
            }
            (markup """
              Client receives a random response for lucky hay: Timothy
              """
                |> setLogLevelDev config
            )
        , let
            value =
                "Timothy"
          in
          onSakuraChanSecondSession.home.expectLuckyHayMessage
            { value = value
            }
            (markup <| "The lucky grass hay is \"" ++ value ++ "\"")
        , let
            value =
                "Sakura-chan"
          in
          onSakuraChanSecondSession.home.expectGreetingMessage
            { value = value
            }
            (markup <| "The greeting message says \"Hi " ++ value ++ "!\"")
        ]
    }


pageHomeCase2 : MarkupConfig -> Section
pageHomeCase2 config =
    { title = "Home page #2"
    , dependency = Scenario.RunAfter (introduction1_1 config).title
    , content =
        [ Scenario.todo sakuraChanMainSession
            (markup "Click the \"Start Chat\" button.")
        , Scenario.todo sakuraChanMainSession
            (markup "Redirect to chat page.")

        , Scenario.todo sakuraChanMainSession
            (markup "The client checks localStorage to restore the previous user input, but nothing is found."
                |> setLogLevelDev config
            )

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client sends a handshake request to the WS server.
            """
            |> setLogLevelDev config
            )
        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives an initialization message from the WS server.

            ```json
            {
                "message": "initialized",
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    }
                ]
            }
            ```
            """
            |> setLogLevelDev config
            )
        , let
            userNames =
                [ "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The "Active users" field says:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )
        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "new-user",
                "user": {
                    "displayName": "Sakura-chan"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    }
                ]

            }
            ```
            """
            |> setLogLevelDev config
            )
        , let
            message =
                "Sakura-chan has entered."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The Message Timeline area displays only one system message:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , userComment sakuraChan "Warunasubi-kun, shall we chat?"
        , userComment warunasubiKun "It sounds good!"
        , Scenario.loadApp sakuraChanMainSession
            (markup """
              **{{name}}**: Enter the chat page URL in the address bar.

              ```
              https://example.com/chat/
              ```
              """
                |> setParam "name" warunasubiKunName
            )
            { path =
                { path = [ pathPrefix, "chat" ]
                , queryParameters = Dict.empty
                , fragment = Nothing
                }
            , flags = JE.object []
            }
        , onWarunasubiKunMainSession.login.expectAvailable <|
            markup "Display login page."
        , userComment sakuraChan
            "I see the note also contains other account information."
        , let
            value =
                "guest2"
          in
          onWarunasubiKunMainSession.login.changeLoginId
            { value = value
            }
            (markup <| "Enter \"" ++ value ++ "\" in the login ID.")
        , let
            value =
                "guestPass2"
          in
          onWarunasubiKunMainSession.login.changeLoginPass
            { value = value
            }
            (markup <| "Enter \"" ++ value ++ "\" in the password field.")
        , onWarunasubiKunMainSession.login.clickSubmitLogin
            (markup "Click the login button.")
        , let
            requestBody =
                JE.object
                    [ ( "id", JE.string "guest2" )
                    , ( "pass", JE.string "guestPass2" )
                    ]

            responseMeta =
                { url = "https://example.com/api/login"
                , statusCode = 200
                , statusText = "OK"
                , headers =
                    Dict.fromList
                        [ ( "Set-Cookie"
                          , "auth_token=authenticated; Secure; HttpOnly; Domain=.example.com; Max-Age=2592000"
                          )
                        ]
                }

            responseBody =
                here """
                {
                    "profile": {
                        "id": "guest2",
                        "name": "Guest2"
                    }
                }
                """
          in
          onWarunasubiKunMainSession.login.receiveLoginResp
            (\body ->
                if body == requestBody then
                    Just ( responseMeta, responseBody )

                else
                    Nothing
            )
            (markup """
              The backend responds to the login request.

              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

                  ```json
                  {{requestMeta|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.login.loginEndpoint)
                |> setParam "requestBody"
                    (JE.encode 4 requestBody)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , onWarunasubiKunMainSession.login.receiveRandomLuckyHay
            { value = Session.LuckyHayTimothy
            }
            (markup """
              Client receives random value for lucky hay: Timothy
              """
                |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup "Redirect to the chat page.")

        , Scenario.todo warunasubiKunMainSession
            (markup "The client checks localStorage to restore the previous user input, but nothing is found."
                |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client sends a handshake request to the WS server.
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives an initialization message from the WS server.

            ```json
            {
                "message": "initialized",
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Guest2"
                    }
                ]
            }
            ```
            """
            |> setLogLevelDev config
            )
        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              The "Active users" field says:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "new-user",
                "user": {
                    "displayName": "Guest2"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Guest2"
                    }
                ]

            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              The Message Timeline area displays only one system message:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "new-user",
                "user": {
                    "displayName": "Guest2"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Guest2"
                    }
                ]

            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The "Active users" field is changed:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )
        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new system message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , let
            message =
                "Welcome, Warunasubi-kun!"
          in
          Scenario.userTodo sakuraChanMainSession
            (markup <| "Enter \"" ++ message ++ "\" in the message input field.")
        , Scenario.userTodo sakuraChanMainSession
            (markup <| "Enter \"Submit\" button.")

        , Scenario.todo sakuraChanMainSession
            (markup <| "The \"Submit\" button becomes disabled.")

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client sends a message to the WS server.

            ```json
            {
                "action": "push-message",
                "message": "Welcome, Warunasubi-kun!"
            }
            ```
            """
            |> setLogLevelDev config
            )

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a new message from the WS server.

            ```json
            {
                "message": "new-message",
                "value": "Welcome, Warunasubi-kun!"
            }
            ```
            """
            |> setLogLevelDev config
            )

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client resets the message field value in localStorage.
            """
            |> setLogLevelDev config
            )

        , Scenario.todo sakuraChanMainSession
            (markup <| "The \"Submit\" button becomes reenabled.")
        , Scenario.todo sakuraChanMainSession
            (markup <| "The message input field becomes empty.")

        , let
            message =
                "Sakura-chan: Welcome, Warunasubi-kun!"
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives a new message from the WS server.

            ```json
            {
                "message": "new-message",
                "value": "Welcome, Warunasubi-kun!"
            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            message =
                "Sakura-chan: Welcome, Warunasubi-kun!"
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              A new message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , let
            message =
                "Hi, Sakura-"
          in
          Scenario.userTodo warunasubiKunMainSession
            (markup <| "Refresh the page accidentally in the middle of writing \"" ++ message ++ "\" in the message input field.")

        , Scenario.todo warunasubiKunMainSession
            (markup "The client saves the input to localStorage during input."
                |> setLogLevelDev config
            )

        , Scenario.closeApp warunasubiKunMainSession
            (markup "The page is reloading.")

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "user-left",
                "user": {
                    "displayName": "Guest2"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    }
                ]
            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            message =
                "Guest2 has left."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new system message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , let
            userNames =
                [ "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The "Active users" field is changed:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )
        , Scenario.loadApp warunasubiKunMainSession
            (markup "The page is reloaded.")
            { path =
                { path = [ pathPrefix, "chat" ]
                , queryParameters = Dict.empty
                , fragment = Nothing
                }
            , flags = JE.object []
            }
        , let
            responseMeta =
                { url = "https://example.com/api/profile"
                , statusCode = 200
                , statusText = "OK"
                , headers =
                    Dict.fromList
                        [ ( "Set-Cookie"
                          , "auth_token=authenticated; Secure; HttpOnly; Domain=.example.com; Max-Age=2592000"
                          )
                        ]
                }

            responseBody =
                here """
                {
                  "profile": {
                    "id": "guest2",
                    "name": "Guest2"
                  }
                }
                """
          in
          onWarunasubiKunMainSession.app.receiveProfile
            (\_ ->
                Just ( responseMeta, responseBody )
            )
            (markup """
              The backend responds to the profile request.

              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.app.fetchProfileEndpoint)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , onWarunasubiKunMainSession.app.receiveRandomLuckyHay
            { value = Session.LuckyHayOrchard
            }
            (markup """
              Client receives random value for lucky hay: Orchard
              """
                |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup "Display the chat page.")

        , let
            message = here """
            Hi, Sakura-
            """
          in
          Scenario.todo sakuraChanMainSession
            (markup """
            The client checks localStorage to restore the previous user input.

            Found:

            ```
            {{message|block}}
            ```
            """
            |> setParam "message" message
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client sends a handshake request to the WS server.
            """
            |> setLogLevelDev config
            )
        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives an initialization message from the WS server.

            ```json
            {
                "message": "initialized",
                "users": [
                    {
                        "displayName": "Guest2"
                    }
                ]
            }
            ```
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "new-user",
                "user": {
                    "displayName": "Guest2"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Guest2"
                    }
                ]

            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              The "Active users" field says:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )
        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              The Message Timeline area displays only one system message:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , let
            message =
                "Hi, Sakura-"
          in
          Scenario.todo warunasubiKunMainSession
            (markup <| "The previous message \"" ++ message ++ "\" remains in the message input field.")

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "new-user",
                "user": {
                    "displayName": "Guest2"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Guest2"
                    }
                ]

            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The "Active users" field is changed:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )
        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new system message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , let
            message =
                "Hi, Sakura-chan."
          in
          Scenario.userTodo warunasubiKunMainSession
            (markup <| "Change the message input field value to \"" ++ message ++ "\".")
        , Scenario.userTodo warunasubiKunMainSession
            (markup <| "Enter \"Submit\" button.")

        , Scenario.todo warunasubiKunMainSession
            (markup <| "The \"Submit\" button becomes disabled.")

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client sends a message to the WS server.

            ```json
            {
                "action": "push-message",
                "message": "Hi, Sakura-chan."
            }
            ```
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives a new message from the WS server.

            ```json
            {
                "message": "new-message",
                "value": "Hi, Sakura-chan."
            }
            ```
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client resets the message field value in localStorage.
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup <| "The \"Submit\" button becomes reenabled.")
        , Scenario.todo warunasubiKunMainSession
            (markup <| "The message input field becomes empty.")

        , let
            message =
                "Guest2: Hi, Sakura-chan."
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              A new message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a new message from the WS server.

            ```json
            {
                "message": "new-message",
                "value": "Hi, Sakura-chan."
            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            message =
                "Guest2: Hi, Sakura-chan."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , userComment sakuraChan "Looks good!"

        , userComment warunasubiKun "I just thought of a fun prank. 😈"

        , Scenario.userTodo warunasubiKunMainSession
            (markup <| "Click header.")

        , Scenario.todo warunasubiKunMainSession
            (markup "Redirect to the home page.")

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "user-left",
                "user": {
                    "displayName": "Guest2"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    }
                ]
            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            message =
                "Guest2 has left."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new system message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , let
            userNames =
                [ "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The "Active users" field is changed:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )

        , let
            value =
                "Sakura-chan"
          in
          onWarunasubiKunMainSession.home.changeEditAccountFormAccountId
            { value = value
            }
            (markup <| "Change the name input field value to \"" ++ value ++ "\".")

        , onWarunasubiKunMainSession.home.clickSubmitEditAccount
            (markup "Click the save button.")

        , let
            requestBody =
                JE.object
                    [ ( "name", JE.string "Sakura-chan" )
                    ]

            responseMeta =
                { url = "https://example.com/api/edit-account"
                , statusCode = 200
                , statusText = "OK"
                , headers =
                    Dict.fromList
                        [ ( "Set-Cookie"
                          , "auth_token=authenticated; Secure; HttpOnly; Domain=.example.com; Max-Age=2592000"
                          )
                        ]
                }

            responseBody =
                here """
                {
                    "profile": {
                        "name": "Sakura-chan"
                    }
                }
                """
          in
          onWarunasubiKunMainSession.home.receiveEditAccountResp
            (\body ->
                if body == requestBody then
                    Just ( responseMeta, responseBody )

                else
                    Nothing
            )
            (markup """
              The backend responds to the edit account request.
              - Request:

                  ```json
                  {{requestMeta|block}}
                  ```

              - Response:

                  ```json
                  {{responseMeta|block}}
                  ```

                  ```json
                  {{responseBody|block}}
                  ```
              """
                |> setParam "requestMeta"
                    (ppr onSakuraChanMainSession.home.editAccountEndpoint)
                |> setParam "responseMeta"
                    (ppr responseMeta)
                |> setParam "responseBody" responseBody
                |> setLogLevelDev config
            )
        , Scenario.todo warunasubiKunMainSession
            (markup "Click the \"Start Chat\" button.")
        , Scenario.todo warunasubiKunMainSession
            (markup "Redirect to chat page.")

        , Scenario.todo warunasubiKunMainSession
            (markup "The client checks localStorage to restore the previous user input, but nothing is found."
                |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client sends a handshake request to the WS server.
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives an initialization message from the WS server.

            ```json
            {
                "message": "initialized",
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Sakura-chan"
                    }
                ]
            }
            ```
            """
            |> setLogLevelDev config
            )

        , let
            userNames =
                [ "Sakura-chan"
                , "Sakura-chan"
                ]
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              The "Active users" field says:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "new-user",
                "user": {
                    "displayName": "Sakura-chan"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Sakura-chan"
                    }
                ]

            }
            ```
            """
            |> setLogLevelDev config
            )
        , let
            message =
                "Sakura-chan has entered."
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              The Message Timeline area displays only one system message:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a system message from the WS server.

            ```json
            {
                "message": "new-user",
                "user": {
                    "displayName": "Sakura-chan"
                },
                "users": [
                    {
                        "displayName": "Sakura-chan"
                    },
                    {
                        "displayName": "Sakura-chan"
                    }
                ]

            }
            ```
            """
            |> setLogLevelDev config
            )


        , let
            userNames =
                [ "Sakura-chan"
                , "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The "Active users" field is changed:

              {{users|block}}
              """
                |> setParam "users"
                    (List.map
                        (\name -> "- " ++ name)
                        userNames
                        |> String.join "\n"
                    )
            )
        , let
            message =
                "Sakura-chan has entered."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new system message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , let
            message =
                "Guest2: Hi, Sakura-chan."
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              The display name of existing messages remain unchanged:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , let
            message =
                "Warunasubi-kun is smart and cool.🥰"
          in
          Scenario.userTodo warunasubiKunMainSession
            (markup <| "Enter \"" ++ message ++ "\" in the message input field.")
        , Scenario.userTodo warunasubiKunMainSession
            (markup <| "Enter \"Submit\" button.")

        , Scenario.todo warunasubiKunMainSession
            (markup <| "The \"Submit\" button becomes disabled.")

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client sends a message to the WS server.

            ```json
            {
                "action": "push-message",
                "message": "Warunasubi-kun is smart and cool.🥰"
            }
            ```
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client receives a new message from the WS server.

            ```json
            {
                "message": "new-message",
                "value": "Warunasubi-kun is smart and cool.🥰"
            }
            ```
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup """
            The client resets the message field value in localStorage.
            """
            |> setLogLevelDev config
            )

        , Scenario.todo warunasubiKunMainSession
            (markup <| "The \"Submit\" button becomes reenabled.")
        , Scenario.todo warunasubiKunMainSession
            (markup <| "The message input field becomes empty.")

        , let
            message =
                "Sakura-chan: Warunasubi-kun is smart and cool.🥰"
          in
          Scenario.todo warunasubiKunMainSession
            (markup """
              A new message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )

        , Scenario.todo sakuraChanMainSession
            (markup """
            The client receives a new message from the WS server.

            ```json
            {
                "message": "new-message",
                "value": "Warunasubi-kun is smart and cool.🥰"
            }
            ```
            """
            |> setLogLevelDev config
            )


        , let
            message =
                "Sakura-chan: Warunasubi-kun is smart and cool.🥰"
          in
          Scenario.todo sakuraChanMainSession
            (markup """
              A new message is added to the Message TimeLine area:

              ```
              {{message|block}}
              ```
              """
                |> setParam "message" message
            )
        , userComment sakuraChan "Hey Warunasubi-kun, I know you posted it. The display name is also Sakura-chan, but with a different color."
        , userComment warunasubiKun "😋"
        ]
    }


setLogLevelDev : MarkupConfig -> Scenario.Markup -> Scenario.Markup
setLogLevelDev config =
    Scenario.hide <| not config.dev


{-| Helper function to pretty print records.
-}
ppr : a -> String
ppr a =
    Debug.toString a
        |> DebugToJson.pp
