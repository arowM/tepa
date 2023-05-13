module VerifyExample exposing (deps)

import App.FetchProfile
import Page.Home.EditAccount
import Page.Login.Login


deps : List ()
deps =
    [ always () App.FetchProfile.response
    , always () Page.Home.EditAccount.response
    , always () Page.Login.Login.response
    ]
