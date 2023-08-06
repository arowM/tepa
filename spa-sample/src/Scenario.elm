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


yabugarashiKun : Scenario.User
yabugarashiKun =
    Scenario.defineUser
        { name = "Yabugarashi-kun"
        }



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



-- # Scenarios


onSakuraChanMainSession : App.ScenarioSet
onSakuraChanMainSession =
    App.scenario sakuraChanMainSession


onSakuraChanSecondSession : App.ScenarioSet
onSakuraChanSecondSession =
    App.scenario sakuraChanSecondSession


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
        , userComment sakuraChan "Today I'm going to try a goat management service."
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
        , userComment yabugarashiKun
            "I'm Yabugarashi-kun. I'm going to play a prank on Sakura-chan. Muahahahahaha! 😈"
        , userComment yabugarashiKun
            "Sakura-chan, here is the account information note your father gave you. 😈"
        , userComment sakuraChan
            "Thank you, Yabugarashi-kun. 🌸"
        , onSakuraChanMainSession.login.changeLoginId
            { value = "guest"
            }
            (Scenario.textContent "Login ID entered.")
        , userComment sakuraChan
            "The note says that the password can be left blank."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Clicked the login button.")
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
        , userComment yabugarashiKun
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
            (Scenario.textContent <| "Password changed to \"" ++ value ++ "\"")
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors
            (Scenario.textContent "Login form shows no errors at the time.")
        , userComment sakuraChan
            "It looks good."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Clicked the login button.")
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
        , userComment yabugarashiKun "Maybe you mistyped the password."
        , userComment sakuraChan "That might be true. It's hard to type with my two-fingered hooves..."
        , let
            value =
                "guestPass"
          in
          onSakuraChanMainSession.login.changeLoginPass
            { value = value
            }
            (Scenario.textContent <| "Enter \"" ++ value ++ "\" in the password field")
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
    { title = "Introduction Scenario #1 (Sub Episode #1)"
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
            (Scenario.textContent "Clicked the login button.")
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
            (Scenario.textContent <| "Enter \"" ++ value ++ "\" in the name input field.")
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
        [ onSakuraChanMainSession.home.expectAvailable
            (Scenario.textContent "TODO2")
        ]
    }


{-| Helper function to pretty print records.
-}
ppr : a -> String
ppr a =
    Debug.toString a
        |> DebugToJson.pp
        |> String.append "json\n"
