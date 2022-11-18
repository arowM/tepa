module Scenario exposing (main, test)

{-| Scenario

@docs main, test

-}

import App exposing (Command, Event, Memory)
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Tepa.Scenario as Scenario exposing (Scenario, userComment)
import Test exposing (Test)



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


type alias Scenario =
    Scenario.Scenario Value Command Memory Event


{-| -}
sections : List Section
sections =
    [ Scenario.section "Introduction Scenario #1" introduction1
    ]


introduction1 : List Scenario
introduction1 =
    [ userComment sakuraChan "Hi. I'm Sakura-chan, the cutest goat girl in the world."
    , userComment sakuraChan "Today I'll try a goat management service."
    , userComment sakuraChan "I'll try to access the URL."
    , Scenario.loadApp sakuraChanMainSession
        "Load the home page."
        { route =
            { path = "tepa/"
            , query = Nothing
            , fragment = Nothing
            }
        , flags = JE.object []
        }
    , onSakuraChanMainSession.app.receiveSession <|
        Err (Http.BadStatus 403)
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
        Err (Http.BadStatus 401)
    , onSakuraChanMainSession.login.toast.expectErrorMessage
        { message = "Incorrect ID or Password." }
        "Toast popup shows error: \"Incorrect ID or Password.\""
    , userComment sakuraChan "Oops!"
    , onSakuraChanMainSession.login.toast.awaitAllToDisappear
    , onSakuraChanMainSession.login.toast.expectNoMessages
        "No error popup messages now."
    , userComment yabugarashiKun "Maybe you typed the password wrong."
    , userComment sakuraChan "That may be true. It's hard to type with my two-fingered hooves..."
    , onSakuraChanMainSession.login.changeLoginPass "guestPass"
    , onSakuraChanMainSession.login.clickSubmitLogin
    , Scenario.fromOk "Sample response"
        (JD.decodeString JD.value """
            {
              "profile": {
                "id": "Sakura-chan-ID"
              }
            }
          """)
      <|
        \resp ->
            [ onSakuraChanMainSession.login.receiveLoginResp <| Ok resp
            ]
    , onSakuraChanMainSession.home.expectAvailable
        "Redirect to home page."
    , userComment sakuraChan "Yes!"
    , Scenario.cases
        [ Scenario.section "Home page #1" pageHomeCase1
        , Scenario.section "Home page #2" pageHomeCase2
        ]
    ]


pageHomeCase1 : List Scenario
pageHomeCase1 =
    [ onSakuraChanMainSession.home.expectAvailable
        "TODO"
    ]


pageHomeCase2 : List Scenario
pageHomeCase2 =
    [ onSakuraChanMainSession.home.expectAvailable
        "TODO2"
    ]
