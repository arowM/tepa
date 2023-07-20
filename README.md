# TEPA

[![test](https://github.com/arowM/tepa/actions/workflows/test.yaml/badge.svg)](https://github.com/arowM/tepa/actions/workflows/test.yaml)  
[Document](https://package.elm-lang.org/packages/arowM/tepa/latest/)  

![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

TEPA is a framework for building user-centric web applications through scenario-driven development.

## What TEPA can do?

TEPA has three main features:

* Generate scenario document
* Generate scenario test
* Develop robust web application with chronological description

### Generate Scenario Document

The `Tepa.Scenario` module allows you to programmatically describe scenario documents.
The resulting document can be viewed in real time in a web browser as a document server, or saved as a markdown file.

Sample Scenario:

```elm
...
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
...
```

Generated markdown:

```elm
...
* **\[Sakura\-chan's main session\]** **System**: Display login page.
* **Sakura\-chan**: I see I have to log in\! I remember my dad gave me the account information beforehand.
* **\[Sakura\-chan's main session\]** **System**: The login form shows no errors at first.
* **Yabugarashi\-kun**: I'm Yabugarashi\-kun. I'm going to play a prank on Sakura\-chan. Muahahahahaha\! 😈
* **Yabugarashi\-kun**: Sakura\-chan, here is the account information note your father gave you. 😈
* **Sakura\-chan**: Thank you, Yabugarashi\-kun. 🌸
* **\[Sakura\-chan's main session\]** **Sakura\-chan**: Login ID entered.
* **Sakura\-chan**: The note says that the password can be left blank.
* **\[Sakura\-chan's main session\]** **Sakura\-chan**: Clicked the login button.
* **\[Sakura\-chan's main session\]** **System**: Form shows error: Password is required.
* **Sakura\-chan**: Oh my goat, I got an error...
* **Yabugarashi\-kun**: Sorry, sorry, I just got a little naughty. Here's the real note.
* **Sakura\-chan**: Okay, I'll try again.
* **\[Sakura\-chan's main session\]** **Sakura\-chan**: Password changed to "fuestPass"
* **\[Sakura\-chan's main session\]** **System**: Login form shows no errors at the time.
* **Sakura\-chan**: It looks good.
* **\[Sakura\-chan's main session\]** **Sakura\-chan**: Clicked the login button.
...
```

In scenario-driven development, which TEPA advocates, the first step is to imagine a concrete use case and create a scenario, and in team development using GitHub, etc., the markdown output of the scenario can be included in pull requests so that reviewers can compare it to the current scenario and see what has changed. Reviewers can easily see what has changed from the current scenario.

### Generating Scenario Tests

By describing scenarios with the `Tepa.Scenario` module, you can automatically test that your TEPA application behaves as described in the scenario. All real world effects like HTTP responses, random numbers, and passage of time, e.t.c., will be simulated as described in the scenario.

### Develop robust web application with chronological description

Static analysis is an essential technique for building reliable and robust applications. To make static analysis more powerful, strong static types are used. However, some languages with strong static types have a feature known as exceptions. This feature negates the benefits of static typing.

One way to enable strong, statically typed, exception-free programming is [The Elm Architecture](https://guide.elm-lang.org/architecture/)(TEA).
The basic design of TEA is great, but using TEA as it is, it is difficult to create user-centric design applications. This is because TEA describes the application by data-centric design, which determines the next process and state based only on the current state of the application when some events (such as user operation) have occurred. On the other hand, real user behavior is an extension of their previous operations, so creating a user-centric application requires a framework that wraps TEA to make it specialized for that use.

TEPA provides such a dedicated framework, which is closer to a scenario because it allows applications to be described in chronological order. As a result, it inherits the features of imperative programming, which facilitates user-centered design, and the features of functional programming, which facilitates static analysis.

```elm
submitLoginProcedure : Bucket -> Promise Memory ()
submitLoginProcedure bucket =
    let
        modifyLoginForm f =
            Tepa.modify <|
                \m ->
                    { m | loginForm = f m.loginForm }
    in
    Tepa.sequence
        [ modifyLoginForm <|
            \m -> { m | isBusy = True }
        , Tepa.bind Tepa.getValues <|
            \form ->
                case Login.fromForm form of
                    Err _ ->
                        [ modifyLoginForm <|
                            \m ->
                                { m
                                    | isBusy = False
                                    , showError = True
                                }
                        ]

                    Ok login ->
                        [ Tepa.bind (Login.request login) <|
                            \response ->
                                case response of
                                    Login.TemporaryErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Network error, please check your network and try again."
                                                |> runToastPromise
                                                |> Tepa.void
                                            , Tepa.sequence
                                                [ modifyLoginForm <|
                                                    \m ->
                                                        { m | isBusy = False }
                                                ]
                                            ]
                                        ]

                                    Login.FatalErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Internal error, please contact our support team."
                                                |> runToastPromise
                                                |> Tepa.void
                                            , Tepa.sequence
                                                [ modifyLoginForm <|
                                                    \m ->
                                                        { m | isBusy = False }
                                                ]
                                            ]
                                        ]

                                    Login.IncorrectIdOrPasswordResponse ->
                                        [ modifyLoginForm <|
                                            \m ->
                                                { m
                                                    | isBusy = False
                                                    , incorrectIdOrPass = True
                                                    , showError = True
                                                }
                                        ]

                                    Login.GoodResponse resp ->
                                        [ Tepa.bind (Random.request Session.randomLuckyHay) <|
                                            \luckyHay ->
                                                [ Tepa.modify <|
                                                    \m ->
                                                        { m
                                                            | msession =
                                                                Just
                                                                    { profile = resp.profile
                                                                    , luckyHay = luckyHay
                                                                    }
                                                            , loginForm =
                                                                let
                                                                    loginForm =
                                                                        m.loginForm
                                                                in
                                                                { loginForm
                                                                    | isBusy = False
                                                                }
                                                        }
                                                ]
                                        , Nav.pushPath bucket.key
                                            (bucket.requestPath.queryParameters
                                                |> Dict.get "back"
                                                |> Maybe.andThen List.head
                                                |> Maybe.andThen Path.toAppUrl
                                                |> Maybe.withDefault
                                                    { path =
                                                        [ Path.prefix
                                                        ]
                                                    , queryParameters = Dict.empty
                                                    , fragment = Nothing
                                                    }
                                            )
                                        ]
                        ]
        ]
```

## Getting started

We are preparing a tutorial. You can check a sample application in the `spa-sample` directory.
