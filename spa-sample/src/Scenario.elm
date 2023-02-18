module Scenario exposing (main, test)

{-| Scenario

@docs main, test

-}

import App exposing (Command, Event, Memory)
import Dict
import Html exposing (Html)
import Json.Encode as JE exposing (Value)
import Tepa
import Tepa.AbsolutePath exposing (absolutePath)
import Tepa.Scenario as Scenario exposing (userComment)
import Test exposing (Test)
import Time
import Widget.Toast as Toast



-- # Expose


{-| Generate document for `scenarios`.
-}
main : Html ()
main =
    Scenario.toHtml
        { title = "Sample scenario"
        , sections = sections
        }


{-| Test for `scenarios`.
-}
test : Test
test =
    Scenario.toTest
        { props = App.props
        , sections = sections
        }



-- # Users


sakuraChan : Scenario.User
sakuraChan =
    Scenario.defineUser
        { name = "Sakura-chan"
        }


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
    Scenario.Section Value Command Memory Event


{-| -}
sections : List Section
sections =
    [ introduction1
    , pageHomeCase1
    , pageHomeCase2
    ]


pathPrefix : String
pathPrefix =
    "tepa"


introduction1 : Section
introduction1 =
    { title = "Introduction Scenario #1"
    , dependency = Scenario.EntryPoint (Time.millisToPosix 1672531200000)
    , content =
        [ userComment sakuraChan "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan "Today I'll try a goat management service."
        , userComment sakuraChan "I'll try to access the URL."
        , Scenario.loadApp sakuraChanMainSession
            "Load the home page."
            { path = absolutePath [ pathPrefix ] [] Nothing
            , flags = JE.object []
            }
        , onSakuraChanMainSession.app.receiveProfile <|
            Ok
                ( { url = "https://example.com/api/profile"
                  , statusCode = 401
                  , statusText = "Unauthorized"
                  , headers = Dict.empty
                  }
                , """
                  {
                    "code": "LoginRequired"
                  }
                  """
                )
        , onSakuraChanMainSession.login.expectAvailable
            "Displays login page."
        , userComment sakuraChan
            "I see I need to log in! I remember my dad gave me the account information in advance."
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors
            "The login form shows no errors at first."
        , userComment yabugarashiKun
            "I'm Yabugarashi-kun. I'm going play a prank on Sakura-chan. Muahahahahaha! 😈"
        , userComment yabugarashiKun
            "Sakura-chan, here is the account information note your father gave you. 😈"
        , userComment sakuraChan
            "Thanks, Yabugarashi-kun. 🌸"
        , onSakuraChanMainSession.login.changeLoginId "guest"
        , userComment sakuraChan
            "The note says that the password can be left blank."
        , onSakuraChanMainSession.login.clickSubmitLogin
        , onSakuraChanMainSession.login.expectLoginFormShowError
            "Password is required."
        , userComment sakuraChan
            "Oh my goat, I got an error..."
        , userComment yabugarashiKun
            "Sorry, sorry, I just got a little naughty. Here's the real note."
        , userComment sakuraChan
            "OK, I'll try again."
        , onSakuraChanMainSession.login.changeLoginPass "fuestPass"
        , onSakuraChanMainSession.login.expectLoginFormShowNoErrors
            "The login form shows no errors at this point."
        , userComment sakuraChan
            "It looks good."
        , onSakuraChanMainSession.login.clickSubmitLogin
        , onSakuraChanMainSession.login.receiveLoginResp <|
            Ok
                ( { url = "https://example.com/api/login"
                  , statusCode = 401
                  , statusText = "Unauthorized"
                  , headers = Dict.empty
                  }
                , """
              {
                "code": "IncorrectIdOrPassword"
              }
              """
                )
        , onSakuraChanMainSession.login.expectLoginFormShowError
            "Incorrect ID or password."
        , userComment sakuraChan "Oops!"
        , userComment yabugarashiKun "Maybe you typed the password wrong."
        , userComment sakuraChan "That may be true. It's hard to type with my two-fingered hooves..."
        , onSakuraChanMainSession.login.changeLoginPass "guestPass"
        , onSakuraChanMainSession.login.clickSubmitLogin
        , onSakuraChanMainSession.login.receiveLoginResp <|
            Err Tepa.NetworkError
        , onSakuraChanMainSession.login.toast.expectErrorMessage
            { message = "Network error, please try again."
            }
            "A toast pops up: \"Network error, please try again.\""
        , userComment sakuraChan "Oops!"
        , Scenario.sleep sakuraChanMainSession
            ("Wait for " ++ String.fromInt Toast.toastTimeout ++ " milliseconds.")
            Toast.toastTimeout
        , onSakuraChanMainSession.login.toast.expectDisappearingErrorMessage
            { message = "Network error, please try again."
            }
            "The popup begin to disappear."
        , Scenario.sleep sakuraChanMainSession
            ("Wait for " ++ String.fromInt Toast.toastFadeOutDuration ++ " milliseconds.")
            Toast.toastFadeOutDuration
        , onSakuraChanMainSession.login.toast.expectNoMessages
            "No toast popups now."
        , userComment sakuraChan "Try again."
        , onSakuraChanMainSession.login.clickSubmitLogin
        , onSakuraChanMainSession.login.receiveLoginResp <|
            Ok
                ( { url = "https://example.com/api/login"
                  , statusCode = 200
                  , statusText = "OK"
                  , headers =
                        Dict.fromList
                            [ ( "Set-Cookie"
                              , "auth_token=authenticated; Secure; HttpOnly; Domain=.example.com; Max-Age=2592000"
                              )
                            ]
                  }
                , """
                {
                  "profile": {
                    "id": "Sakura-chan-ID",
                    "name": "Sakura-chan"
                  }
                }
                """
                )
        , onSakuraChanMainSession.home.expectAvailable
            "Redirect to home page."
        , userComment sakuraChan "Yes!"
        ]
    }


pageHomeCase1 : Section
pageHomeCase1 =
    { title = "Home page #1"
    , dependency = Scenario.RunAfter introduction1.title
    , content =
        [ onSakuraChanMainSession.home.expectAvailable
            "TODO"
        ]
    }


pageHomeCase2 : Section
pageHomeCase2 =
    { title = "Home page #2"
    , dependency = Scenario.RunAfter introduction1.title
    , content =
        [ onSakuraChanMainSession.home.expectAvailable
            "TODO2"
        ]
    }
