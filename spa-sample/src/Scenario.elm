module Scenario exposing
    ( main, test, sections, MarkupConfig
    , Model, Msg, Section
    )

{-| Scenario

@docs main, test, sections, MarkupConfig

-}

import App exposing (Memory)
import App.Session as Session
import Browser
import DebugToJson
import Dict
import Expect
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Encode as JE
import MarkdownAst as Markdown
import Tepa.Scenario as Scenario exposing (userComment)
import Tepa.Time as Time
import Test exposing (Test)
import TimeZone
import Widget.Toast as Toast



-- # Expose


{-| Generate document for `scenarios`.
-}
main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { dev = True
            }
        , view =
            \config ->
                Html.div
                    []
                    [ Html.label
                        []
                        [ Html.input
                            [ Attributes.type_ "checkbox"
                            , Attributes.checked config.dev
                            , Events.onInput (\_ -> ToggleDevMode)
                            ]
                            []
                        , Html.text "Enable dev mode"
                        ]
                    , Scenario.toHtml
                        { title = "Sample scenario"
                        , sections = sections config
                        , config = Scenario.en_US
                        }
                    ]
        , update = update
        }


type alias Model =
    MarkupConfig


type Msg
    = ToggleDevMode


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDevMode ->
            { model | dev = not model.dev }


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
            { content =
                [ Markdown.StrongEmphasis sakuraChanName
                , Markdown.PlainText ": Type the following URL in the address bar."
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.InlineCode "https://example.com/"
                    ]
                ]
            , appear = True
            }
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
                """
                {
                    "code": "LoginRequired"
                }
                """
          in
          onSakuraChanMainSession.app.receiveProfile
            (\_ ->
                Just ( responseMeta, responseBody )
            )
            { content =
                [ Markdown.PlainText "The backend responds to the profile request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onSakuraChanMainSession.app.fetchProfileEndpoint
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , onSakuraChanMainSession.login.expectAvailable <|
            Scenario.textContent "Display login page."
        , userComment sakuraChan
            "I see I have to log in! I remember my dad gave me the account information beforehand."
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors <|
            Scenario.textContent "The login form shows no errors at first."
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
            (Scenario.textContent <| "Enter \"" ++ value ++ "\" in the login ID.")
        , userComment sakuraChan
            "The note says that the password can be left blank."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Click the login button.")
        , let
            error =
                "Password is required."
          in
          onSakuraChanMainSession.login.expectLoginFormShowError
            { error = error
            }
            (Scenario.textContent <| "Form shows error: " ++ error)
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
            (Scenario.textContent <| "Change the password to \"" ++ value ++ "\"")
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors
            (Scenario.textContent "Login form shows no errors at the time.")
        , userComment sakuraChan
            "It looks good."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Click the login button.")
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
                """
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
            { content =
                [ Markdown.PlainText "The backend responds to the login request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onSakuraChanMainSession.login.loginEndpoint
                                , Markdown.CodeBlock <| "json\n" ++ JE.encode 4 requestBody
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , let
            error =
                "Incorrect ID or password."
          in
          onSakuraChanMainSession.login.expectLoginFormShowError
            { error = error
            }
            (Scenario.textContent <| "The form shows error: " ++ error)
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
            (Scenario.textContent <| "Enter \"" ++ value ++ "\" in the password field.")
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Click login button.")
        , let
            requestBody =
                JE.object
                    [ ( "id", JE.string "guest" )
                    , ( "pass", JE.string "guestPass" )
                    ]
          in
          onSakuraChanMainSession.login.expectRequestLogin
            requestBody
            { content =
                [ Markdown.PlainText "The client sends a login request to the backend with a timeout of 5000 milliseconds."
                ]
            , detail =
                [ Markdown.CodeBlock <|
                    ppr
                        onSakuraChanMainSession.login.loginEndpoint
                , Markdown.CodeBlock <| "json\n" ++ JE.encode 4 requestBody
                ]
            , appear = config.dev
            }
        , if config.dev then
            Scenario.systemComment sakuraChanMainSession <|
                "The client requests the user profile from the server with a timeout of 5000 milliseconds."

          else
            Scenario.none
        , Scenario.sleep
            (Scenario.textContent <| "5000 milliseconds have passed.")
            5000
        , let
            currentTime =
                1672531205000
          in
          Scenario.expectCurrentTime
            (Scenario.textContent <| "Current time in POSIX: " ++ String.fromInt currentTime ++ ".")
            { expectation =
                Expect.equal
                    (Time.millisToPosix currentTime)
            }
        , Scenario.expectHttpRequest
            sakuraChanMainSession
            (Scenario.textContent <| "The login request timed out.")
            { layer = onSakuraChanMainSession.login.layer
            , expectation =
                List.length
                    >> Expect.equal 0
            }
        , onSakuraChanMainSession.login.toast.expectErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (Scenario.textContent "A toast popup appears: \"Network error, please try again.\"")
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
            (Scenario.textContent "Click the close button on the popup.")
        , onSakuraChanMainSession.login.toast.expectDisappearingErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (Scenario.textContent "The popup begins to disappear.")
        , Scenario.sleep
            (Scenario.textContent <| String.fromInt Toast.toastFadeOutDuration ++ " milliseconds passes.")
            Toast.toastFadeOutDuration
        , onSakuraChanMainSession.login.toast.expectNoMessages
            (Scenario.textContent "No toast popups now.")
        ]
    }


introduction1_1 : MarkupConfig -> Section
introduction1_1 config =
    { title = "Introduction Scenario #1-1"
    , dependency = Scenario.RunAfter (introduction1 config).title
    , content =
        [ Scenario.sleep
            (Scenario.textContent <| String.fromInt Toast.toastTimeout ++ " milliseconds passes.")
            Toast.toastTimeout
        , onSakuraChanMainSession.login.toast.expectDisappearingErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (Scenario.textContent "The popup begins to disappear.")
        , Scenario.sleep
            (Scenario.textContent <| String.fromInt Toast.toastFadeOutDuration ++ " milliseconds passes.")
            Toast.toastFadeOutDuration
        , onSakuraChanMainSession.login.toast.expectNoMessages
            (Scenario.textContent "No toast popups now.")
        , userComment sakuraChan "Try again."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Click the login button.")
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
                """
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
            { content =
                [ Markdown.PlainText "The backend responds to the login request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onSakuraChanMainSession.login.loginEndpoint
                                , Markdown.CodeBlock <| "json\n" ++ JE.encode 4 requestBody
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , onSakuraChanMainSession.login.receiveRandomLuckyHay
            { value = Session.LuckyHayAlfalfa
            }
            { content =
                [ Markdown.PlainText "Client receives random value for lucky hay: Alfalfa"
                ]
            , detail = []
            , appear = config.dev
            }
        , onSakuraChanMainSession.home.expectAvailable
            (Scenario.textContent "Redirect to home page.")
        , let
            value =
                "Alfalfa"
          in
          onSakuraChanMainSession.home.expectLuckyHayMessage
            { value = value
            }
            (Scenario.textContent <| "The lucky grass hay is \"" ++ value ++ "\"")
        , let
            value =
                "2023-01-01 09:00:15"
          in
          onSakuraChanMainSession.home.expectClockMessage
            { value = value
            }
            (Scenario.textContent <| "The clock says \"" ++ value ++ "\"")
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
            (Scenario.textContent <| "The greeting message says \"Hi, " ++ value ++ "!\"")
        , userComment sakuraChan "I'm Sakura-chan! Not \"Guest\"! 💢🐐"
        , let
            value =
                "Sakura-chan"
          in
          onSakuraChanMainSession.home.changeEditAccountFormAccountId
            { value = value
            }
            (Scenario.textContent <| "Change the name input field value to \"" ++ value ++ "\".")
        , onSakuraChanMainSession.home.clickSubmitEditAccount
            (Scenario.textContent "Click the save button.")
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
                """
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
            { content =
                [ Markdown.PlainText "The backend responds to the edit account request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onSakuraChanMainSession.home.editAccountEndpoint
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , let
            value =
                "Sakura-chan"
          in
          onSakuraChanMainSession.home.expectGreetingMessage
            { value = value
            }
            (Scenario.textContent <| "The greeting message says \"Hi, " ++ value ++ "!\"")
        , Scenario.loadApp sakuraChanSecondSession
            { content =
                [ Markdown.StrongEmphasis sakuraChanName
                , Markdown.PlainText ": Open new tab, and type the following URL in the address bar."
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.InlineCode "https://example.com/"
                    ]
                ]
            , appear = True
            }
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
                """
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
            { content =
                [ Markdown.PlainText "The backend responds to the profile request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onSakuraChanSecondSession.app.fetchProfileEndpoint
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , onSakuraChanSecondSession.app.receiveRandomLuckyHay
            { value = Session.LuckyHayTimothy
            }
            { content =
                [ Markdown.PlainText "Client receives a random response for lucky hay: Timothy"
                ]
            , detail = []
            , appear = config.dev
            }
        , let
            value =
                "Timothy"
          in
          onSakuraChanSecondSession.home.expectLuckyHayMessage
            { value = value
            }
            (Scenario.textContent <| "The lucky grass hay is \"" ++ value ++ "\"")
        , let
            value =
                "Sakura-chan"
          in
          onSakuraChanSecondSession.home.expectGreetingMessage
            { value = value
            }
            (Scenario.textContent <| "The greeting message says \"Hi " ++ value ++ "!\"")
        ]
    }


pageHomeCase2 : MarkupConfig -> Section
pageHomeCase2 config =
    { title = "Home page #2"
    , dependency = Scenario.RunAfter (introduction1_1 config).title
    , content =
        [ Scenario.todo sakuraChanMainSession
            (Scenario.textContent "Click the \"Start Chat\" button.")
        , Scenario.todo sakuraChanMainSession
            (Scenario.textContent "Redirect to chat page.")
        , let
            userNames =
                [ "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field says:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            message =
                "Sakura-chan has entered."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The Message Timeline area displays only one system message:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , userComment sakuraChan "Warunasubi-kun, shall we chat?"
        , userComment warunasubiKun "It sounds good!"
        , Scenario.loadApp sakuraChanMainSession
            { content =
                [ Markdown.StrongEmphasis warunasubiKunName
                , Markdown.PlainText ": Enter the chat page URL in the address bar."
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.InlineCode "https://example.com/chat/"
                    ]
                ]
            , appear = True
            }
            { path =
                { path = [ pathPrefix, "chat" ]
                , queryParameters = Dict.empty
                , fragment = Nothing
                }
            , flags = JE.object []
            }
        , onWarunasubiKunMainSession.login.expectAvailable <|
            Scenario.textContent "Display login page."
        , userComment sakuraChan
            "I see the note also contains other account information."
        , let
            value =
                "guest2"
          in
          onWarunasubiKunMainSession.login.changeLoginId
            { value = value
            }
            (Scenario.textContent <| "Enter \"" ++ value ++ "\" in the login ID.")
        , let
            value =
                "guestPass2"
          in
          onWarunasubiKunMainSession.login.changeLoginPass
            { value = value
            }
            (Scenario.textContent <| "Enter \"" ++ value ++ "\" in the password field.")
        , onWarunasubiKunMainSession.login.clickSubmitLogin
            (Scenario.textContent "Click the login button.")
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
                """
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
            { content =
                [ Markdown.PlainText "The backend responds to the login request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onSakuraChanMainSession.login.loginEndpoint
                                , Markdown.CodeBlock <| "json\n" ++ JE.encode 4 requestBody
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , onWarunasubiKunMainSession.login.receiveRandomLuckyHay
            { value = Session.LuckyHayTimothy
            }
            { content =
                [ Markdown.PlainText "Client receives random value for lucky hay: Timothy"
                ]
            , detail = []
            , appear = config.dev
            }
        , Scenario.todo warunasubiKunMainSession
            (Scenario.textContent "Redirect to the chat page.")
        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field says:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "The Message Timeline area displays only one system message:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field is changed:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new system message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Welcome, Warunasubi-kun!"
          in
          Scenario.userTodo sakuraChanMainSession
            (Scenario.textContent <| "Enter \"" ++ message ++ "\" in the message input field.")
        , Scenario.userTodo sakuraChanMainSession
            (Scenario.textContent <| "Enter \"Submit\" button.")
        , let
            message =
                "Sakura-chan: Welcome, Warunasubi-kun!"
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Sakura-chan: Welcome, Warunasubi-kun!"
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "A new message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Hi, Sakura-"
          in
          Scenario.userTodo warunasubiKunMainSession
            (Scenario.textContent <| "Refresh the page accidentally in the middle of writing \"" ++ message ++ "\" in the message input field.")
        , Scenario.closeApp warunasubiKunMainSession
            (Scenario.textContent "The page is reloading.")
        , let
            message =
                "Guest2 has left."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new system message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            userNames =
                [ "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field is changed:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , Scenario.loadApp warunasubiKunMainSession
            (Scenario.textContent "The page is reloaded.")
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
                """
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
            { content =
                [ Markdown.PlainText "The backend responds to the profile request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onWarunasubiKunMainSession.app.fetchProfileEndpoint
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , onWarunasubiKunMainSession.app.receiveRandomLuckyHay
            { value = Session.LuckyHayOrchard
            }
            { content =
                [ Markdown.PlainText "Client receives a random response for lucky hay: Orchard"
                ]
            , detail = []
            , appear = config.dev
            }
        , Scenario.todo warunasubiKunMainSession
            (Scenario.textContent "Display the chat page.")
        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field says:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "The Message Timeline area displayes only one system message:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Hi, Sakura-"
          in
          Scenario.todo warunasubiKunMainSession
            (Scenario.textContent <| "The previous message \"" ++ message ++ "\" remains in the message input field.")
        , let
            userNames =
                [ "Sakura-chan"
                , "Guest2"
                ]
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field is changed:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            message =
                "Guest2 has entered."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new system message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Hi, Sakura-chan."
          in
          Scenario.userTodo warunasubiKunMainSession
            (Scenario.textContent <| "Change the message input field value to \"" ++ message ++ "\".")
        , Scenario.userTodo warunasubiKunMainSession
            (Scenario.textContent <| "Enter \"Submit\" button.")
        , let
            message =
                "Guest2: Hi, Sakura-chan."
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "A new message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Guest2: Hi, Sakura-chan."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , userComment sakuraChan "Looks good!"
        , userComment warunasubiKun "I just thought of a fun prank. 😈"
        , Scenario.userTodo warunasubiKunMainSession
            (Scenario.textContent <| "Click header.")
        , Scenario.todo warunasubiKunMainSession
            (Scenario.textContent "Redirect to the home page.")
        , let
            message =
                "Guest2 has left."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new system message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            userNames =
                [ "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field is changed:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            value =
                "Sakura-chan"
          in
          onWarunasubiKunMainSession.home.changeEditAccountFormAccountId
            { value = value
            }
            (Scenario.textContent <| "Change the name input field value to \"" ++ value ++ "\".")
        , onWarunasubiKunMainSession.home.clickSubmitEditAccount
            (Scenario.textContent "Click the save button.")
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
                """
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
            { content =
                [ Markdown.PlainText "The backend responds to the edit account request."
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = False
                    , items =
                        [ { content =
                                [ Markdown.PlainText "Request:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <|
                                    ppr
                                        onWarunasubiKunMainSession.home.editAccountEndpoint
                                ]
                          }
                        , { content =
                                [ Markdown.PlainText "Response:"
                                ]
                          , children =
                                [ Markdown.CodeBlock <| ppr responseMeta
                                , Markdown.CodeBlock <| "json" ++ responseBody
                                ]
                          }
                        ]
                    }
                ]
            , appear = config.dev
            }
        , Scenario.todo warunasubiKunMainSession
            (Scenario.textContent "Click the \"Start Chat\" button.")
        , Scenario.todo warunasubiKunMainSession
            (Scenario.textContent "Redirect to chat page.")
        , let
            userNames =
                [ "Sakura-chan"
                , "Sakura-chan"
                ]
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field says:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            message =
                "Sakura-chan has entered."
          in
          Scenario.todo warunasubiKunMainSession
            { content =
                [ Markdown.PlainText "The Message Timeline area displays only one system message:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            userNames =
                [ "Sakura-chan"
                , "Sakura-chan"
                ]
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The \"Active users\" field is changed:"
                ]
            , detail =
                [ Markdown.ListBlock
                    { ordered = True
                    , items =
                        List.map
                            (\userName ->
                                { content = [ Markdown.PlainText userName ]
                                , children = []
                                }
                            )
                            userNames
                    }
                ]
            , appear = True
            }
        , let
            message =
                "Sakura-chan has entered."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new system message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Guest2: Hi, Sakura-chan."
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The display name of existing messages remain unchanged:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , let
            message =
                "Warunasubi-kun is smart and cool.🥰"
          in
          Scenario.userTodo sakuraChanMainSession
            (Scenario.textContent <| "Enter \"" ++ message ++ "\" in the message input field.")
        , Scenario.userTodo sakuraChanMainSession
            (Scenario.textContent <| "Enter \"Submit\" button.")
        , let
            message =
                "Sakura-chan: Warunasubi-kun is smart and cool.🥰"
          in
          Scenario.todo sakuraChanMainSession
            { content =
                [ Markdown.PlainText "A new message is added to the Message TimeLine area:"
                ]
            , detail =
                [ Markdown.ParagraphBlock
                    [ Markdown.PlainText message
                    ]
                ]
            , appear = True
            }
        , userComment sakuraChan "Hey Warnusbi-kun, I know you posted it. The display name is also Sakura-chan, but with a different color."
        , userComment warunasubiKun "😋"
        ]
    }


{-| Helper function to pretty print records.
-}
ppr : a -> String
ppr a =
    Debug.toString a
        |> DebugToJson.pp
        |> String.append "json\n"
