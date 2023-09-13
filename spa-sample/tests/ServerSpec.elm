module ServerSpec exposing (suite)

import App.Dom
import App.FetchProfile
import Expect
import Page.Chat.NewMessage
import Page.Home.EditAccount
import Page.Login.Login
import Test exposing (Test)


suite : Test
suite =
    Test.describe "responses"
        [ Test.test "Dom" <|
            \() ->
                Ok App.Dom.response
                    |> Expect.ok
        , Test.test "FetchProfile" <|
            \() ->
                Ok App.FetchProfile.response
                    |> Expect.ok
        , Test.test "Login.Login" <|
            \() ->
                Ok Page.Login.Login.response
                    |> Expect.ok
        , Test.test "Home.EditAccount" <|
            \() ->
                Ok Page.Home.EditAccount.response
                    |> Expect.ok
        , Test.test "Chat.NewMessage" <|
            \() ->
                Ok Page.Chat.NewMessage.response
                    |> Expect.ok
        ]
