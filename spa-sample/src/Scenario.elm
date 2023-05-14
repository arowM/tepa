module Scenario exposing
    ( main, test, sections, MarkupConfig
    , Model, Msg, Section
    )

{-| Scenario

@docs main, test, sections, MarkupConfig

-}

import App exposing (Event, Memory)
import Browser
import DebugToJson
import Dict
import Expect
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Encode as JE exposing (Value)
import MarkdownAst as Markdown
import Tepa.AbsolutePath exposing (absolutePath)
import Tepa.Scenario as Scenario exposing (userComment)
import Test exposing (Test)
import Time
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



-- sakuraChanSecondSession : Scenario.Session
-- sakuraChanSecondSession =
--     Scenario.defineSession
--         { uniqueName = "Sakura-chan's second session"
--         , user = sakuraChan
--         }
-- # Scenarios


onSakuraChanMainSession : App.ScenarioSet flags
onSakuraChanMainSession =
    App.scenario sakuraChanMainSession


type alias Section =
    Scenario.Section Value Memory Event


{-| -}
sections : MarkupConfig -> List Section
sections config =
    [ introduction1 config
    , pageHomeCase1 config
    , pageHomeCase2 config
    ]


pathPrefix : String
pathPrefix =
    "tepa"


introduction1 : MarkupConfig -> Section
introduction1 config =
    { title = "Introduction Scenario #1"
    , dependency = Scenario.EntryPoint (Time.millisToPosix 1672531200000)
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
            { path = absolutePath [ pathPrefix ] [] Nothing
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
            Scenario.textContent "Displays login page."
        , userComment sakuraChan
            "I see I need to log in! I remember my dad gave me the account information in advance."
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors <|
            Scenario.textContent "The login form shows no errors at first."
        , userComment yabugarashiKun
            "I'm Yabugarashi-kun. I'm going play a prank on Sakura-chan. Muahahahahaha! 😈"
        , userComment yabugarashiKun
            "Sakura-chan, here is the account information note your father gave you. 😈"
        , userComment sakuraChan
            "Thanks, Yabugarashi-kun. 🌸"
        , onSakuraChanMainSession.login.changeLoginId
            { value = "guest"
            }
            (Scenario.textContent "Entered login ID.")
        , userComment sakuraChan
            "The note says that the password can be left blank."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Clicked login button.")
        , let
            error =
                "Password is required."
          in
          onSakuraChanMainSession.login.expectLoginFormShowError
            { error = error
            }
            (Scenario.textContent <| "The form shows error: " ++ error)
        , userComment sakuraChan
            "Oh my goat, I got an error..."
        , userComment yabugarashiKun
            "Sorry, sorry, I just got a little naughty. Here's the real note."
        , userComment sakuraChan
            "OK, I'll try again."
        , let
            value =
                "fuestPass"
          in
          onSakuraChanMainSession.login.changeLoginPass
            { value = value
            }
            (Scenario.textContent <| "Changed password to \"" ++ value ++ "\"")
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors
            (Scenario.textContent "The login form shows no errors at this point.")
        , userComment sakuraChan
            "It looks good."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Clicked login button.")
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
        , userComment yabugarashiKun "Maybe you typed the password wrong."
        , userComment sakuraChan "That may be true. It's hard to type with my two-fingered hooves..."
        , let
            value =
                "guestPass"
          in
          onSakuraChanMainSession.login.changeLoginPass
            { value = value
            }
            (Scenario.textContent <| "Entered login password: \"" ++ value ++ "\"")
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Clicked login button.")
        , if config.dev then
            Scenario.systemComment sakuraChanMainSession <|
                "The client requests the user profile from the server with a timeout of 5000 milliseconds."

          else
            Scenario.none
        , Scenario.sleep
            (Scenario.textContent <| "Passing 5000 milliseconds.")
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
        , onSakuraChanMainSession.login.toast.expectErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (Scenario.textContent "A toast pops up: \"Network error, please try again.\"")
        , userComment sakuraChan "Oops!"
        , Scenario.sleep
            (Scenario.textContent <| "Passing " ++ String.fromInt Toast.toastTimeout ++ " milliseconds.")
            Toast.toastTimeout
        , onSakuraChanMainSession.login.toast.expectDisappearingErrorMessage
            { message = "Network error, please check your network and try again."
            }
            (Scenario.textContent "The popup begin to disappear.")
        , Scenario.sleep
            (Scenario.textContent <| "Passing " ++ String.fromInt Toast.toastFadeOutDuration ++ " milliseconds.")
            Toast.toastFadeOutDuration
        , onSakuraChanMainSession.login.toast.expectNoMessages
            (Scenario.textContent "No toast popups now.")
        , userComment sakuraChan "Try again."
        , onSakuraChanMainSession.login.clickSubmitLogin
            (Scenario.textContent "Clicked login button.")
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
                        "id": "Sakura-chan-ID",
                        "name": "Sakura-chan"
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
        , onSakuraChanMainSession.home.expectAvailable
            (Scenario.textContent "Redirect to home page.")
        , userComment sakuraChan "Yes!"
        ]
    }


pageHomeCase1 : MarkupConfig -> Section
pageHomeCase1 config =
    { title = "Home page #1"
    , dependency = Scenario.RunAfter (introduction1 config).title
    , content =
        [ onSakuraChanMainSession.home.expectAvailable
            (Scenario.textContent "TODO")
        ]
    }


pageHomeCase2 : MarkupConfig -> Section
pageHomeCase2 config =
    { title = "Home page #2"
    , dependency = Scenario.RunAfter (introduction1 config).title
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
