module Tepa exposing
    ( application
    , ApplicationProps
    , Program
    , Document
    , Promise
    , sequence
    , modify
    , none
    , currentState
    , bind, bind2, bind3, bindAll
    , succeed, sync, hardcoded
    , lazy
    , void
    , when
    , unless
    , withMaybe
    , syncAll
    , forEach
    , portRequest
    , PortRequest, PortResponse
    , portStream
    , map
    , liftMemory, maybeLiftMemory
    , andThen, bindAndThen, bindAndThen2, bindAndThen3, bindAndThenAll
    , NavKey
    , UrlRequest(..)
    , Layer
    , newLayer
    , LayerMemory
    , onLink, onBody
    , linkSequence, bodySequence
    , modifyLink, modifyBody
    , onLayer, ResultOnLayer(..)
    , onChildLayer
    , Html
    , Mixin
    , layerView
    , ViewContext
    , getValue, getValues
    , setValue
    , getCheck, getChecks, setCheck
    , awaitViewEvent, awaitCustomViewEvent
    , viewEventStream, customViewEventStream
    , assertionError
    , headless
    , Msg
    )

{-| This module provides core functionality for TEPA.

⚠⚠⚠⚠⚠⚠
If you haven't read the [README](https://github.com/arowM/tepa/blob/main/README.md), read the README first.
If you want to know how to use it first, then learning TEPA is of no value to you. Go home.
⚠⚠⚠⚠⚠⚠


# Overview of the application structure

TEPA application is written in the [Elm language](https://elm-lang.org/), and is converted to JavaScript, so you import it to use in your JavaScript (or TypeScript) file.
If you are a [Parcel](https://parceljs.org/) user, you can load the Elm file that contains the `main` function built with `application` as follows:

`index.html`:

    <html>
      <body>
        <script type="module" src="./index.js"></script>
      </body>
    </html>

`index.js`:

    // Specify your file that contains `main` function here.
    import { Elm } from "./Main.elm"

    Elm.Main.init({
      // Pass some JSON value to TEPA upon initialization.
      flags: {
        "loaded-at": Date.now()
      }
    })

`Main.elm` (Details will be explained later):

    import Json.Decode (Value)
    import Tepa exposing (AppUrl, NavKey, Program, Promise)
    import Tepa.Time as Time

    main : Program Memory
    main =
        Tepa.application
            { init = init
            , view = view
            , initView = initView
            , onLoad = onLoad
            , onUrlRequest = onUrlRequest
            , onUrlChange = onUrlChange
            }

    {- Data type for representing your application state.
    -}
    type alias Memory =
        { ...
        , ...
        }

    {- Data type for representing _Flags_.
    -}
    type alias Flags =
        { ...
        , ...
        }

    {-| Your implementation for `init`.
    -}
    init : Value -> Promise () (Flags, Memory)
    init =
        Debug.todo ""

    {-| Your implementation for `onLoad`.
    -}
    onLoad : Flags -> AppUrl -> NavKey -> Promise Memory ()
    onLoad flags url key =
        Debug.todo ""

    {-| Your implementation for `onUrlRequest`.
    -}
    onUrlRequest : Flags -> UrlRequest -> NavKey -> Promise memory ()
    onUrlRequest flags url key =
        Debug.todo ""

    {-| Your implementation for `onUrlChange`.
    -}
    onUrlChange : Flags -> AppUrl -> NavKey -> Promise memory ()
    onUrlChange flags url key =
        Debug.todo ""

    {-| Your implementation for `initView`.
    -}
    initView : Document
    initView layer =
        Debug.todo ""

    {-| Your implementation for `view`.
    -}
    view : Layer Memory -> Document
    view layer =
        Debug.todo ""

If you are not familiar with Elm language, we recommend you to check [Core Language](https://guide.elm-lang.org/core_language), [Types](https://guide.elm-lang.org/types/), and [Error Handling](https://guide.elm-lang.org/error_handling/) section of the Elm guide.
API documentation for the core libraries of the language can be found on the [package site](https://package.elm-lang.org/packages/elm/core/latest/).
Among the core libraries, `Platform`, `Platform.Cmd`, `Platform.Sub`, `Process`, and `Task` modules are not used by TEPA, so you can just ignore them.

@docs application
@docs ApplicationProps
@docs Program
@docs Document


# Key Concepts of TEPA


## Memory

TEPA has a single _Memory_ that holds all of the application state.

    {-| Your own type that represents your application state.

    It is common to declare [type alias](https://guide.elm-lang.org/types/type_aliases) for [record](https://guide.elm-lang.org/core_language#records).

    -}
    type alias Memory =
        { pageState : PageState
        }


## Flags

You can pass a JSON value, called _Flags_, to the TEPA application upon initialization.
Common uses are passing in API keys, environment variables, and user data.

The sample `index.js` in the `application` document passes `{ "loaded-at": Data.now() }` as flags.

    Elm.Main.init({
      // Pass some JSON value to TEPA upon initialization.
      flags: {
        "loaded-at": Date.now()
      }

On the Elm side, the JSON object is converted to `Value` type which you can _decode_ with the [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode) module.


## Procedure

In TEPA, you describe application behaviour as _Procedure_, the time sequence of proccessing.


### Promise

To build your procedure, you combine some _Promises_ that represent the eventual completion of an operation and its resulting value. Similar to [Promise in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise).

@docs Promise

The main difference from Promise in JavaScript is that Promise in TEPA does not be rejected. When you represent rejected state, just return [`Result`](https://package.elm-lang.org/packages/elm/core/latest/Result#Result) value as its result.
The rejection of JavaScript Promise is a sort of Exception, which drops context.


#### Primitive Promises

@docs sequence
@docs modify
@docs none
@docs currentState


#### Bind results

`currentState` is resolved with the current state, but how to retrieve the value? You can use `bind` to pass the result into another sequence of procedures.

@docs bind, bind2, bind3, bindAll
@docs succeed, sync, hardcoded


#### Recursive procedures

It is common technique to call procedures recursively.

    formProcedure : Promise Memory ()
    formProcedure =
        Tepa.bind submit <|
            \result ->
                case result of
                    Err error ->
                        [ Tepa.modify <|
                            \m ->
                                { m | error = error }
                        , formProcedure
                        ]

                    Ok body ->
                        [ Debug.todo ""
                        ]

However, this code may cause run out of stack memory. To optimize memory usage, you can use `lazy`.

    formProcedure : Promise Memory ()
    formProcedure =
        Tepa.bind submit <|
            \result ->
                case result of
                    Err error ->
                        [ Tepa.modify <|
                            \m ->
                                { m | error = error }
                        , Tepa.lazy <|
                            \_ ->
                                formProcedure
                        ]

                    Ok body ->
                        [ Debug.todo ""
                        ]

@docs lazy


#### Common helpers

@docs void
@docs when
@docs unless
@docs withMaybe
@docs syncAll
@docs forEach


#### DOM events

To request DOM related operations, you can use [`Tepa.Dom`](./Tepa-Dom) module.


#### Page navigation

To navigate to another page, you can use [`Tepa.Navigation`](./Tepa-Navigation) module.


#### Random values

To request random values, you can use [`Tepa.Random`](./Tepa-Random) module.


#### Handle time

To request time related operations, you can use [`Tepa.Time`](./Tepa-Time) module.


#### HTTP requests

To send HTTP request to the backend server, you can use [`Tepa.Http`](./Tepa-Http) module.


#### Port requests

@docs portRequest
@docs PortRequest, PortResponse
@docs portStream


#### Streams

Some Promises are resolved with `Stream`, which is an advanced technique for controlling Promises precisely.
See [Tepa.Stream](./Tepa-Stream) for details.


#### Lower level functions

@docs map
@docs liftMemory, maybeLiftMemory
@docs andThen, bindAndThen, bindAndThen2, bindAndThen3, bindAndThenAll


### Application Procedures

There are four types of procedures that you specify as the `application` property.


### `init`

The `init` Procedure is executed on page load to decode your Flags and set the initial Memory state.
It takes a raw `Value` which you can _decode_ to your Flags using the [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode) module.

    init : Value -> Promise () ( flags, memory )

Note that its type has `()` as a Memory type, so you cannot access memory state during initialization process.


### `onLoad`

The `onLoad` is the main Procedure that is executed on every page load right after `init`.
It takes three arguments:

    onLoad : flags -> AppUrl -> NavKey -> Promise memory ()

  - Flags: Flags value decoded by the `init` Procedure.

  - Application URL: The initial URL requested by a user.

    TEPA uses the [`AppUrl`](https://package.elm-lang.org/packages/lydell/elm-app-url/latest/AppUrl) type to represent internal URLs.

  - Navigation Key: Required by functions exposed by `Tepa.Navigation`.

@docs NavKey


### `onUrlRequest`

You specify the `onUrlRequest` Procedure for handling page transition requests.
It takes three arguments:

    onUrlRequest : flags -> UrlRequest -> NavKey -> Promise memory ()

  - Flags: Flags value decoded by the `init` Procedure.
  - Requested URL: [`UrlRequest`](#UrlRequest) value that indecates requested URL.
  - Navigation Key: Required by functions exposed by `Tepa.Navigation`.

@docs UrlRequest

In most cases, you will declare the function as follows:

    onUrlRequest : Flags -> Tepa.UrlRequest -> NavKey -> Promise Memory ()
    onUrlRequest _ urlRequest key =
        case urlRequest of
            Tepa.InternalPath url ->
                Nav.pushPath key url

            Tepa.ExternalPage href ->
                Nav.load href


### `onUrlChange`

Immediately after the URL is changed, `onUrlChange` Procedure is evaluated.
A common use case is to change the page state based on the new URL.

It takes three arguments:

    onUrlChange : flags -> AppUrl -> NavKey -> Promise memory ()

  - Flags: Flags value decoded by the `init` Procedure.
  - New URL: Loaded new URL.
  - Navigation Key: Required by functions exposed by `Tepa.Navigation`.

In [sample application](https://github.com/arowM/tepa-sample), the `onUrlChange` function retrieves the session information, such as the logged-in user data, and then calls the main procedure.


## Layer


### Motivation

The _Layer_ is the concept of an isolated space. You can create a new layer with the `newLayer` function, execute a procedure on a layer with the `onLayer` function, delete or overwrite the existing layer with `modify`.

A main use of the layer is to manage page transition. See that you have the following `Page` type to represent your page state.

    import MyApp.Profile exposing (Profile)
    import Page.Home
    import Page.Users
    import Tepa exposing (Layer)

    type alias Memory =
        { page : Page
        , session :
            { mprofile : Maybe Profile
            }
        }

    type Page
        = PageNotFound
        | PageHome Page.Home.Memory
        | PageUsers Page.Users.Memory

This approach may seem to work well at first, but it may exhibit unexpected behavior, such as

1.  A user opens the home page.
      - The `init` Procedure initializing the `page` in Memory to `PageHome param` (where `param` is the initial value of `PageHome.Memory`).
      - The `onLoad` Procedure is called, and it execute the Procedure for the home page.
      - The Procedure for the home page is executed to replace the top image to be displayed every 20 seconds (process A).
2.  The user is redirected to the user list page.
      - The `onChangeUrl` Procedure is called and the `page` in memory is replaced with `PageUsers param` (`param` is the initial value of `PageUsers.Memory`).
      - At this point, process A is asleep.
3.  The user immediately changes to the home page.
      - The `onChangeUrl` Procedure is called, and the `page` in memory is again replaced with the `PageHome param` (`param` is the initial value of `PageHome.Memory`).
      - The `onChangeUrl` Procedure continues to execute a new Procedure for the home page.
      - A new process (Process B) runs to update the display image every 20 seconds, overlapping with Process A.
      - Now it is time for Process A to refresh the image.
          - Because the memory state is still `PageHome`, Process A cannot detect that the page has changed in the middle of the process.
      - Process B also refreshes images periodically, so the slideshow now refreshes images at odd times!

The reason for this unexpected behavior is that the process cannot determine that the page state has changed in the middle of a page based only on the memory state. To solve this problem, you can use _Layer_.

    type Page
        = PageNotFound
        | PageHome (Layer Page.Home.MemoryBody)
        | PageUsers (Layer Page.Users.MemoryBody)

The new `Page` definition above uses `Layer` to wrap each page memory state. A procedure executed on a Layer will be aborted when the Layer has expired by being overwritten by another Layer. This allows you to avoid running duplicate procedures.

@docs Layer
@docs newLayer


### Layer Memory

On each Layer you define Memory with `LayerMemory`.

@docs LayerMemory

The `LayerMemory` has two parameters:

  - `link`: External Memory state which this Layer depends on.
  - `body`: The Memory body for the Layer.

For example, you can declare `Memory` for the home page as follows:

    module Page.Home exposing
        ( Memory
        , MemoryBody
        )

    import MyApp.Profile exposing (Profile)
    import Tepa exposing (LayerMemory, Promise)

    {-| Memory structure for home page.
    -}
    type alias Memory =
        LayerMemory
            MemoryLink
            MemoryBody

    {-| Link to the external memory spaces.
    The main use is to share the application state between pages.
    -}
    type alias MemoryLink =
        { profile : Profile
        }

    {-| Memory body
    -}
    type alias MemoryBody =
        { clicked : Bool
        }

Now you can declare your Procedures.

    init : Promise m MemoryBody
    init =
        Tepa.succeed
            { clicked = False
            }

    onLoad : Promise Memory ()
    onLoad =
        Tepa.bind
            (Tepa.awaitViewEvent
                { key = "myButton"
                , type_ = "click"
                }
            )
        <|
            \() ->
                [ Tepa.bodySequence
                    [ Tepa.modify <| \m -> { m | clicked = True }
                    ]

                -- This operation affects other pages.
                , Tepa.linkSequence
                    [ Tepa.modify <|
                        \({ profile } as m) ->
                            { m
                                | profile =
                                    { profile | hasClickedHomeButton = True }
                            }
                    ]
                ]

@docs onLink, onBody
@docs linkSequence, bodySequence
@docs modifyLink, modifyBody

To call Procedures for `LayerMemory` on the Procedures for the parent Memory, you can use `onLayer`.

@docs onLayer, ResultOnLayer

To call Procedures for `LayerMemory` on the Procedures for the parent `LayerMemory`, you can use `onChildLayer`.

@docs onChildLayer


## View

The _View_ determines how your application is rendered in the web browser, based only on the current Layer state.
You use [Tepa.Html](Tepa-Html) and [Tepa.Mixin](Tepa-Mixin) to tell the web browser how to render the page.

@docs Html
@docs Mixin

The `application` function takes two types of Views: `initView` for the View during the initialization on page load, and `view` for Views after the initialization process.


### Define `view`

To define `view` function, you use `layerView`.

@docs layerView
@docs ViewContext

    import Tepa exposing (Document, Layer)

    view : Layer Memory -> Document
    view =
        Tepa.layerView <|
            \_ state ->
                { title = "Sample App"
                , body =
                    [ case state.page of
                        PageNotFound ->
                            pageNotFoundView

                        PageLogin pageLogin ->
                            PageLogin.view pageLogin

                        PageHome pageHome ->
                            PageHome.view pageHome
                    ]
                }

The `state` field of the `ViewContext` indicates current memory state of the Layer.


### Keys

Key is used to specify a specific View element.

    import Tepa exposing (Layer)
    import Tepa.Html as Html exposing (Html)
    import Tepa.Mixin as Mixin

    formView : Layer Memory -> Html
    formView =
        Tepa.layerView <|
            \{ setKey, values } _ ->
                Html.node "form"
                    [ Mixin.attribute "novalidate" "true"
                    ]
                    [ Html.node "label"
                        []
                        [ Html.text "Name: "
                        , Html.node "input"
                            [ setKey "form_name"
                            , Mixin.attribute "type" "text"
                            , Mixin.attribute "placeholder" "Sakura-chan"
                            ]
                            []
                        ]
                    , Html.button
                        [ setKey "form_submit"
                        ]
                        [ Html.text "Submit"
                        ]
                    , errors values
                    ]

    errors : Dict String String -> Html
    errors formValues =
        case Dict.get "form_name" formValues of
            Just "" ->
                Html.text "Name is required."

            _ ->
                Html.text ""

This example uses the `setKey` of the `ViewContext` to set the Key named "form\_name" to the name input, and "form\_submit" to the submit button. In this way, you can retrieve the user input value with the `values` field of the `ViewContext`.

Note that Keys on the same Layer must be unique.

_For advanced use: You can use `setKey` alternative for `Html.Attribute` that elm/html exposes. This means you can use layout libraries like [neat-layout](https://package.elm-lang.org/packages/arowM/elm-neat-layout/latest/) or elm-ui._


### Handle form value on Procedure

The key is also used to communicate View and Procedure. If you want to get user inputs on the Procedure side, you can use `getValue` or `getValues`.

@docs getValue, getValues

The `setValue` is used to overwrite or initiate the input value from the Procedure side.

@docs setValue

Note that the Procedure can only get/set values in Views on the same Layer that the Procedure is executed.


### Handle form check state on Procedure

You can get/set `checked` property values of radio/checkbox elements.

@docs getCheck, getChecks, setCheck

Note that the Procedure can only get/set check state in Views on the same Layer that the Procedure is executed.


### Handle View events on Procedure

To capture View events on Procedure, you can use `awaitViewEvent` or `awaitCustomViewEvent`.

@docs awaitViewEvent, awaitCustomViewEvent

For more precise control, you can use [`Stream`](./Tepa-Stream) versions.

@docs viewEventStream, customViewEventStream

Note that the Procedure can only capture events in Views on the same Layer that the Procedure is executed.


### Freshness of input values

The user input values obtained by the `value` field of the `ViewContext` and `getValue` / `getValues` in the Procedure are updated whenever the `change` or `blur` event of the target element occurs. So if you want to implement something like an incremental search, getting values in this ways will not give you the latest input values.
Use [search type of input element](https://developer.mozilla.org/docs/Web/HTML/Element/input/search) or capture the `input` event with `awaitCustomViewEvent` to handle this situation.


# Assertion

You may have a situation where you do not want a Promise to result in a certain result, such as when your `onLayer` result in `LayerNotExists` on a Layer that should exist at that moment. Assertion is a good practice to detect such logic bugs.

@docs assertionError

Assertion has no effect while the TEPA code is running as an application, but if the same code is used for scenario testing, the test will fail when an Assertion Error occurs.


# Scenario

To create user scenarios and generate tests for them, see the [`Tepa.Scenario`](./Tepa-Scenario) module.


# Headless

@docs headless


# Internal

@docs Msg

-}

import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Attribute)
import Internal.Core as Core
    exposing
        ( AppState(..)
        , Msg(..)
        , Stream
        )
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Mixin
import Platform
import Url exposing (Url)


{-| -}
type alias Promise memory result =
    Core.Promise memory result


{-| Build a Promise that is always completed with the given value immediately.

This is usefull for building Promise for concurrent operations with `sync`.

-}
succeed : a -> Promise memory a
succeed =
    Core.LayerExist >> Core.succeedPromise


{-| Transform a resulting value produced by a Promise.
-}
map : (a -> b) -> Promise m a -> Promise m b
map f =
    Core.mapPromise
        (\state ->
            case state of
                Core.LayerExist a ->
                    Core.LayerExist <| f a

                Core.LayerUnreachable ->
                    Core.LayerUnreachable

                Core.MemoryUnreachable ->
                    Core.MemoryUnreachable
        )


{-| Transform the Promise Memory.
-}
liftMemory :
    { get : m0 -> m1
    , set : m1 -> m0 -> m0
    }
    -> Promise m1 a
    -> Promise m0 a
liftMemory =
    Core.liftPromiseMemory


{-| Similar to `liftMemory`, but for the memory which may not exist.
When the memory part is unreachable during execution, it resolved to `Nothing`.
-}
maybeLiftMemory :
    { get : m0 -> Maybe m1
    , set : m1 -> m0 -> m0
    }
    -> Promise m1 a
    -> Promise m0 (Maybe a)
maybeLiftMemory param p =
    Core.maybeLiftPromiseMemory param p
        |> Core.mapPromise
            (\state ->
                case state of
                    Core.LayerExist a ->
                        Core.LayerExist <| Just a

                    Core.MemoryUnreachable ->
                        Core.LayerExist Nothing

                    Core.LayerUnreachable ->
                        Core.LayerUnreachable
            )


{-| _Layer_ is a concept that deals with a part of the application. It can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m =
    Core.Layer m



-- Composition


{-| Build a new Promise that evaluate two Promises sequentially.
-}
andThen : (a -> Promise m b) -> Promise m a -> Promise m b
andThen f =
    Core.andThenPromise
        (\state ->
            case state of
                Core.LayerExist a ->
                    f a

                Core.LayerUnreachable ->
                    Core.succeedPromise Core.LayerUnreachable

                Core.MemoryUnreachable ->
                    Core.succeedPromise Core.MemoryUnreachable
        )


{-| Flipped version of `andThen`.

You can use `bindAndThen` to bind some Promise result to a variable:

    bindAndThen somePromise <|
        \result ->
            anotherPromise result

-}
bindAndThen : Promise m a -> (a -> Promise m b) -> Promise m b
bindAndThen p f =
    andThen f p


{-| Run two Promises concurrently, and bind the results to variables when both are complete.

    import Tepa exposing (Promise)
    import Tepa.Time as Time

    sample : Promise m ( Time.Zone, Time.Posix )
    sample =
        Tepa.bindAndThen2 Time.now Time.here <|
            \now here ->
                Tepa.succeed ( here, now )

-}
bindAndThen2 : Promise m a -> Promise m b -> (a -> b -> Promise m c) -> Promise m c
bindAndThen2 pa pb f =
    succeed f
        |> sync pa
        |> sync pb
        |> andThen identity


{-| Run three Promises concurrently, and bind the results to variables when all are complete.

If you need to bind more Promises, use `bindAndThenAll` or `sync`.

-}
bindAndThen3 : Promise m a -> Promise m b -> Promise m c -> (a -> b -> c -> Promise m d) -> Promise m d
bindAndThen3 pa pb pc f =
    succeed f
        |> sync pa
        |> sync pb
        |> sync pc
        |> andThen identity


{-| Run Promises concurrently, and bind the results to variables when all are complete.
-}
bindAndThenAll : List (Promise m a) -> (List a -> Promise m b) -> Promise m b
bindAndThenAll ps f =
    List.foldl
        (\p acc ->
            succeed (::)
                |> sync p
                |> sync acc
        )
        (succeed [])
        ps
        |> andThen
            (\reversed ->
                f <| List.reverse reversed
            )


{-| Run many Promises concurrently to reduce all results.

    type alias Response =
        { resp1 : Resp1
        , resp2 : Resp2
        }

    request1 : Promise Memory Event Resp1
    request1 =
        Debug.todo ""

    request2 : Promise Memory Event Resp2
    request2 =
        Debug.todo ""

    -- Returns `Response` value when both Promises has been completed.
    batched : Promise Memory Event Response
    batched =
        succeed Response
            |> sync request1
            |> sync request2

-}
sync : Promise m a -> Promise m (a -> b) -> Promise m b
sync =
    Core.syncPromise


{-| Alias for `sync << succeed`, especially useful when defining `init` functions:

    type alias MemoryBody =
        { field1 : Widget1.MemoryBody
        , field2 : Bool
        , field3 : Int
        }

    init : Promise MemoryBody
    init =
        succeed MemoryBody
            |> sync Widget1.init
            |> hardcoded False
            |> hardcoded 0

-}
hardcoded : a -> Promise m (a -> b) -> Promise m b
hardcoded =
    sync << succeed



-- Procedures


{-| Wait for a promise to be resolved and just ignore the result.
-}
void : Promise m a -> Promise m ()
void =
    map (\_ -> ())


{-| Sequentially process Procedures.

    sample : Promise m ()
    sample =
        sequence
            [ executedFirst
            , executedAfterFirstIsResolved
            ]

-}
sequence : List (Promise m ()) -> Promise m ()
sequence =
    Core.sequence


andThenSequence : (a -> List (Promise m ())) -> Promise m a -> Promise m ()
andThenSequence f =
    andThen (f >> sequence)


{-| Helper function to _bind_ the result of a promise.

    import Tepa exposing (Promise)

    procedure : Promise m ()
    procedure =
        Tepa.bind Tepa.currentState <|
            \state ->
                if state.noGoats then
                    [ Tepa.modify <|
                        \m ->
                            { m | message = "Oh my Goat!" }
                    , unfortunatelyNoGoatsProcedure
                    ]

                else
                    [ youAreLuckyProcedure
                    ]

-}
bind : Promise m a -> (a -> List (Promise m ())) -> Promise m ()
bind promise f =
    andThenSequence f promise


{-| Run two Promises concurrently, and bind the results to variables when both are complete.

    import Tepa exposing (Promise)
    import Tepa.Time as Time

    sample : Promise m ()
    sample =
        Tepa.bind2 Time.now Time.here <|
            \now here ->
                [ yourProcedure now here
                ]

-}
bind2 : Promise m a -> Promise m b -> (a -> b -> List (Promise m ())) -> Promise m ()
bind2 p1 p2 f =
    succeed Tuple.pair
        |> sync p1
        |> sync p2
        |> andThenSequence
            (\( a, b ) -> f a b)


{-| Run three Promises concurrently, and bind the results to variables when all are complete.

If you need to bind more Promises, use `bindAll` or `sync`.

-}
bind3 :
    Promise m a
    -> Promise m b
    -> Promise m c
    -> (a -> b -> c -> List (Promise m ()))
    -> Promise m ()
bind3 p1 p2 p3 f =
    succeed (\a b c -> ( a, b, c ))
        |> sync p1
        |> sync p2
        |> sync p3
        |> andThenSequence
            (\( a, b, c ) -> f a b c)


{-| Run Promises concurrently, and bind the results to variables when all are complete.
-}
bindAll :
    List (Promise m a)
    -> (List a -> List (Promise m ()))
    -> Promise m ()
bindAll ps f =
    List.foldl
        (\p pacc ->
            succeed (\acc a -> a :: acc)
                |> sync pacc
                |> sync p
        )
        (succeed [])
        ps
        |> andThenSequence (f << List.reverse)


{-| Procedure that does nothing.
-}
none : Promise m ()
none =
    Core.none


{-| Run Procedures concurrently, and await all to be completed.
-}
syncAll : List (Promise m ()) -> Promise m ()
syncAll proms =
    (case proms of
        [] ->
            Core.succeedPromise <| Core.LayerExist ()

        p0 :: ps ->
            List.foldl
                (\p acc ->
                    succeed
                        (\_ _ -> ())
                        |> Core.syncPromise p
                        |> Core.syncPromise acc
                )
                p0
                ps
    )
        |> Core.mapPromise (\_ -> Core.LayerExist ())


{-| Run the Procedure concurrently on each list element, and await all to be completed.
-}
forEach : List a -> (a -> List (Promise m ())) -> Promise m ()
forEach ls f =
    syncAll (List.map (f >> sequence) ls)


{-| Construct a Promise that modifies the Memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means that the state of the Memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Promise m ()
modify =
    Core.modify


{-| -}
lazy : (() -> Promise m ()) -> Promise m ()
lazy =
    Core.lazy



-- Helper Procedures


{-| Evaluate the sequence of Procedures only if the first argument is `True`, otherwise same as `none`.
-}
when : Bool -> List (Promise m ()) -> Promise m ()
when p ps =
    if p then
        sequence ps

    else
        none


{-| Evaluate the sequence of Procedures only if the first argument is `False`, otherwise same as `none`.
-}
unless : Bool -> List (Promise m ()) -> Promise m ()
unless p =
    when (not p)


{-| Evaluate the sequence of Procedures returned by the callback function only if the first argument is `Just`, otherwise same as `none`.
-}
withMaybe : Maybe a -> (a -> List (Promise m ())) -> Promise m ()
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            sequence <| f a



-- Primitive Promises


{-| Promise that requests current Memory state.
-}
currentState : Promise m m
currentState =
    Core.currentState


{-| Build a Promise to send a _port_ request and receive a single response for it.
The _port_ is an concept to allow communication between Elm and JavaScript (or TypeScript).
Ports are probably most commonly used for WebSockets and localStorage. You can see WebSocket examples in the [sample application](https://github.com/arowM/tepa-sample).

Here, we use `portRequest` to get localStorage value safely.

In JavaScript side:

```js
const app = Elm.Main.init({
  flags: {
    "loaded-at": Date.now()
};

app.ports.requestGetLocalName.subscribe((req) => {
  // It is a good practice to surround your code with `requestAnimationFrame` and `try`.
  requestAnimationFrame(() => {
    try {
      app.ports.receiveGetLocalName.send({
        // The `requestId` value, generated by TEPA, links
        // the subscribe port to the relevant send port.
        "id": req.id,
        "body": {
          name: localStorage.getItem(`Name.${req.body.userId}`),
        },
      });
    } catch {
      app.ports.receiveGetLocalName.send({
        "id": req.id,
        "body": {
          name: null,
        },
      });
    }
  });
});
```

In Elm side:

    import Json.Decode as JD
    import Json.Encode as JE exposing (Value)
    import Tepa exposing (PortRequest, PortResponse, Promise)

    port page_foo_get_local_name_request : PortRequest a

    port page_foo_get_local_name_response : PortResponse a

    type alias LocalNameResponse =
        { name : Maybe String
        }

    requestLocalName : String -> Promise Memory Event LocalNameResponse
    requestLocalName userId =
        Tepa.portRequest
            { request = page_foo_get_local_name_request
            , response = page_foo_get_local_name_response

            -- Port identifier for testing and debugging which must be
            -- unique among its layer.
            , portName = "get_local_name"
            , requestBody =
                JE.object
                    [ ( "userId"
                      , JE.string userId
                      )
                    ]
            }

As you can see, port is a mechanism for requesting a task to the JavaScript server running on the client machine.
It is the same structure as requesting a task to the backend server via HTTP Web API.

-}
portRequest :
    { request : PortRequest Msg
    , response : PortResponse Msg
    , portName : String
    , requestBody : Value
    }
    -> Promise m Value
portRequest param =
    Core.portRequest
        { ports =
            { request = param.request
            , response = param.response
            , name = param.portName
            }
        , requestBody = param.requestBody
        }


{-| -}
type alias PortRequest msg =
    Value -> Cmd msg


{-| -}
type alias PortResponse msg =
    (Value -> msg) -> Sub msg


{-| Similar to `portRequest`, but `portStream` can receive many responses.
One of the use cases is to receive WebSocket messages.

  - `request`: Request JavaScript port server for some tasks.
  - `response`: Receive responses for the request from JavaScript port server.
  - `cancel`: Called on the current Layer expires or the Stream ends. The main use is to free resources, such as closing WebSocket connections.

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer. If you call `portStream` in recursive Promise, it spawns listeners many times!

-}
portStream :
    { request : PortRequest Msg
    , response : PortResponse Msg
    , cancel : PortRequest Msg
    , portName : String
    , requestBody : Value
    }
    -> Promise m (Stream Value)
portStream param =
    Core.portStream
        { ports =
            { request = param.request
            , response = param.response
            , cancel = Just param.cancel
            , name = param.portName
            }
        , requestBody = param.requestBody
        }


{-| Tepa.Stream
-}
viewEventStream :
    { key : String
    , type_ : String
    }
    -> Promise m (Stream ())
viewEventStream param =
    customViewEventStream
        { key = param.key
        , type_ = param.type_
        , decoder = justAwait
        }


{-| Wait for an event of the [type](https://developer.mozilla.org/docs/Web/API/Event/type) specified by `type_` to occur for the element specified by `key`.
-}
awaitViewEvent :
    { key : String
    , type_ : String
    }
    -> Promise m ()
awaitViewEvent param =
    awaitCustomViewEvent
        { key = param.key
        , type_ = param.type_
        , decoder = justAwait
        }


{-| Advanced version of `awaitViewEvent`.

The `decoder` field is the [Decoder](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#Decoder) for the target [event object](https://developer.mozilla.org/en-US/docs/Web/API/Event). If the returned `stopPropagation` value is `True`, it calls the `Event.stopPropagation()` method. If the returned `preventDefault` value is `True`, it calls the `Event.preventDefault()` method.

-}
awaitCustomViewEvent :
    { key : String
    , type_ : String
    , decoder :
        Decoder
            { stopPropagation : Bool
            , preventDefault : Bool
            , value : a
            }
    }
    -> Promise m a
awaitCustomViewEvent =
    Core.awaitCustomViewEvent


{-| -}
customViewEventStream :
    { key : String
    , type_ : String
    , decoder :
        Decoder
            { stopPropagation : Bool
            , preventDefault : Bool
            , value : a
            }
    }
    -> Promise m (Stream a)
customViewEventStream =
    Core.customViewEventStream


{-| -}
justAwait :
    Decoder
        { stopPropagation : Bool
        , preventDefault : Bool
        , value : ()
        }
justAwait =
    JD.succeed
        { stopPropagation = False
        , preventDefault = False
        , value = ()
        }


{-| Get the user's input value for the view element identified by the key string.

`Nothing` means that the key does not exist or that the value of the element specified by the key has not been changed since the element was rendered.

-}
getValue : String -> Promise m (Maybe String)
getValue =
    Core.getValue


{-| Get all the user input values in the Layer as `Dict`.

    sample : Promise Memory ()
    sample =
        Tepa.bind Tepa.getValues <|
            \formValues ->
                let
                    targetValue =
                        Dict.get "form_name" formValues
                            |> Maybe.withDefault ""
                in
                [ handleValue targetValue
                ]

-}
getValues : Promise m (Dict String String)
getValues =
    Core.getValues


{-| Set the user's input value for the view element identified by the key string.

Note that it only sets initial value, but does not **overwrite** user input value.
It is due to a slightly awkward behavior of the Elm runtime.
We plan to improve this behavior in the near future, but for most applications,
just setting the default values should be fine.

You can see [`Page.Chat` module](https://github.com/arowM/tepa-sample/blob/main/src/Page/Chat.elm) of sample application for a real example of resetting user input.

-}
setValue : String -> String -> Promise m ()
setValue =
    Core.setValue


{-| Get whether the checkbox/radio specified by the key string is checked.

`Nothing` means that the checkbox/radio element with the key does not exist or that the `checked` property value of the element specified by the key has not been changed since the element was rendered.

-}
getCheck : String -> Promise m (Maybe Bool)
getCheck =
    Core.getCheck


{-| Get all the check states in the Layer as `Dict`.

    sample : Promise Memory ()
    sample =
        Tepa.bind Tepa.getChecks <|
            \formChecks ->
                let
                    targetCheck =
                        Dict.get "form_name" formChecks
                            |> Maybe.withDefault ""
                in
                [ handleCheck targetCheck
                ]

-}
getChecks : Promise m (Dict String Bool)
getChecks =
    Core.getChecks


{-| Set the user's check state for the radio/checkbox element identified by the key string.

Note that it only sets initial state, but does not **overwrite** actual check state on DOM.
It is due to a slightly awkward behavior of the Elm runtime.
We plan to improve this behavior in the near future, but for most applications,
just setting the default values should be fine.

-}
setCheck : String -> Bool -> Promise m ()
setCheck =
    Core.setCheck



-- Layer


{-| -}
newLayer : m1 -> Promise m (Layer m1)
newLayer =
    Core.newLayer


{-| -}
layerView :
    (ViewContext -> m -> view)
    -> Layer m
    -> view
layerView f layer =
    let
        args =
            Core.viewArgs layer
    in
    f
        { setKey = args.setKey
        , values = args.values
        , checks = args.checks
        , setKey_ = args.setKey_
        }
        args.state


{-| -}
type alias Html =
    Html.Html Msg


{-| -}
type alias Mixin =
    Mixin.Mixin Msg


{-|

  - `setKey`: Set a key to the element.

  - `values`: Current values of the control elements, keyed by its key strings set with `setKey`.

  - `checks`: Current check state of the radio/check elements, keyed by its key strings set with `setKey`.

  - `setKey_`: _(For advanced use) `setKey` for `elm/html` nodes._

-}
type alias ViewContext =
    { setKey : String -> Mixin
    , values : Dict String String
    , checks : Dict String Bool
    , setKey_ : String -> List (Attribute Msg)
    }


{-| Memory structure for accessing external Memory space from within Layer.
-}
type LayerMemory link body
    = LayerMemory
        { link : Maybe link
        , body : Maybe body
        }


{-| Represents result for `onLayer`.

  - `SucceedOnLayer`: The Promise has been successfully resolved.
  - `BodyExpired`: The layer has been expired during execution.
  - `LinkExpired`: The link which the layer depends on has been expired during execution.

-}
type ResultOnLayer a
    = SucceedOnLayer a
    | BodyExpired
    | LinkExpired


{-| Run Promise on the specified Layer.
For example, consider the following situation:

    Tepa.bind
        (PageLogin.init msession
            |> Tepa.andThen Tepa.newLayer
        )
    <|
        \newLayer ->
            [ Tepa.modify <| \m -> { m | page = PageLogin newLayer }
            , PageLogin.procedure key url
                |> Tepa.onLayer
                    { getLink =
                        \m ->
                            Maybe.map
                                (\profile ->
                                    { profile = profile
                                    }
                                )
                                m.session.mprofile
                    , setLink =
                        \link ({ session } as m) ->
                            { m
                                | session =
                                    { session
                                        | mprofile = Just link.profile
                                    }
                            }
                    , getBody =
                        \m ->
                            case m.page of
                                PageLogin layer ->
                                    Just layer

                                _ ->
                                    Nothing
                    , setBody =
                        \layer m ->
                            { m | page = PageLogin layer }
                    }
            ]

If the target layer disappears during the execution of a given Promise, the rest of the process for the body part is aborted, and returns `BodyExpired`.
If the link becomes to be unreachable during the execution of a given Promise, the rest of the process for the link part is aborted, and returns `LinkExpired`.

-}
onLayer :
    { getLink : memory -> Maybe link
    , setLink : link -> memory -> memory
    , getBody : memory -> Maybe (Layer body)
    , setBody : Layer body -> memory -> memory
    }
    -> Promise (LayerMemory link body) a
    -> Promise memory (ResultOnLayer a)
onLayer param proc =
    Core.onLayer param
        (Core.liftPromiseMemory
            { get = LayerMemory
            , set = \(LayerMemory a) _ -> a
            }
            proc
        )
        |> map
            (\state ->
                case state of
                    Core.LayerExist a ->
                        SucceedOnLayer a

                    Core.MemoryUnreachable ->
                        LinkExpired

                    Core.LayerUnreachable ->
                        BodyExpired
            )


{-| Similar to `onLayer`, but run on the parent Layer.
-}
onChildLayer :
    { getLink :
        { link : Maybe link0
        , body : Maybe body0
        }
        -> Maybe link1
    , setLink :
        link1
        ->
            { link : Maybe link0
            , body : Maybe body0
            }
        ->
            { link : Maybe link0
            , body : Maybe body0
            }
    , getBody :
        { link : Maybe link0
        , body : Maybe body0
        }
        -> Maybe (Layer body1)
    , setBody :
        Layer body1
        ->
            { link : Maybe link0
            , body : Maybe body0
            }
        ->
            { link : Maybe link0
            , body : Maybe body0
            }
    }
    -> Promise (LayerMemory link1 body1) a
    -> Promise (LayerMemory link0 body0) (ResultOnLayer a)
onChildLayer param =
    onLayer
        { getLink =
            \(LayerMemory lm) ->
                param.getLink lm
        , setLink =
            \link1 (LayerMemory lm) ->
                LayerMemory <| param.setLink link1 lm
        , getBody =
            \(LayerMemory lm) ->
                param.getBody lm
        , setBody =
            \lb1 (LayerMemory lm) ->
                LayerMemory <| param.setBody lb1 lm
        }


{-| Run Procedures for link part of `LayerMemory`
-}
onLink :
    Promise link a
    -> Promise (LayerMemory link body) a
onLink =
    Core.maybeLiftPromiseMemory
        { get = \(LayerMemory m) -> m.link
        , set =
            \link (LayerMemory m) ->
                LayerMemory <|
                    { m | link = Just link }
        }


{-| Run Procedures for body part of `LayerMemory`
-}
onBody :
    Promise body a
    -> Promise (LayerMemory link body) a
onBody =
    Core.maybeLiftPromiseMemory
        { get =
            \(LayerMemory m) ->
                m.body
        , set =
            \body (LayerMemory m) ->
                LayerMemory <|
                    { m | body = Just body }
        }


{-| Helper function to run Procedures for link part sequentially.
-}
linkSequence : List (Promise link ()) -> Promise (LayerMemory link body) ()
linkSequence ps =
    sequence ps
        |> onLink


{-| Helper function to run Procedures for body part sequentially.
-}
bodySequence : List (Promise body ()) -> Promise (LayerMemory link body) ()
bodySequence ps =
    sequence ps
        |> onBody


{-| Helper function to modify link part.
-}
modifyLink : (link -> link) -> Promise (LayerMemory link body) ()
modifyLink =
    onLink << modify


{-| Helper function to modify link part.
-}
modifyBody : (body -> body) -> Promise (LayerMemory link body) ()
modifyBody =
    onBody << modify



-- Assertion


{-| Cause the scenario test to fail.

    import Tepa

    sampleProcedure =
        Tepa.bind
            (Tepa.onLayer
                { getLink = getMyLink
                , setLink = setMyLink
                , getBody = getMyBody
                , setBody = setMyBody
                }
                promiseOnLayer
            )
        <|
            \result ->
                case result of
                    Tepa.SucceedOnLayer a ->
                        [ procedureOnSuccess
                        ]

                    _ ->
                        [ Tepa.assertionError "Layer error on sampleProcedure"
                        , sendErrorLog "Layer error on sampleProcedure"
                        , handleError
                        ]

-}
assertionError : String -> Promise memory ()
assertionError =
    Core.assertionError


{-| Entry point for building your applications.
-}
application :
    ApplicationProps flags memory
    -> Program flags memory
application props =
    Browser.application
        { init =
            init
                { init = props.init
                , onLoad = props.onLoad
                , onUrlRequest = props.onUrlRequest
                , onUrlChange = props.onUrlChange
                }
        , view =
            view
                { initView = props.initView
                , view = props.view
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


{-| Property values for your application.
-}
type alias ApplicationProps flags memory =
    { init : Value -> Promise () ( flags, memory )
    , onLoad : flags -> AppUrl -> NavKey -> Promise memory ()
    , onUrlRequest : flags -> UrlRequest -> NavKey -> Promise memory ()
    , onUrlChange : flags -> AppUrl -> NavKey -> Promise memory ()
    , initView : Document
    , view : flags -> memory -> Document
    }


{-| A navigation key is needed to create navigation procedures exposed by the [Tepa.Navigation](./Tepa-Navigation) module.

You only get access to a `NavKey` when you create your program with `application`, guaranteeing that your program is equipped to detect these URL changes. If `NavKey` values were available in other kinds of programs, unsuspecting programmers would be sure to run into some [annoying bugs](https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md) and learn a bunch of techniques the hard way!

_This is the TEPA version of [Browser.Navigation.Key](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#Key)._

-}
type alias NavKey =
    Core.NavKey


{-| A `Program` describes an TEPA program.

_An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program)._

-}
type alias Program flags memory =
    Platform.Program Value (Model flags memory) Msg


{-| This data specifies the `<title>` and all of the nodes that should go in the `<body>`. This means you can update the title as your application changes. Maybe your "single-page app" navigates to a "different page", maybe a calendar app shows an accurate date in the title, etc.

_This is the TEPA version of [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document)._

-}
type alias Document =
    { title : String
    , body : List Html
    }


{-| All links in an [`application`](#application) create a `UrlRequest`. So
when you click `<a href="/home">Home</a>`, it does not just navigate! It
notifies `onUrlRequest` that the user wants to change the URL.

_This is the TEPA version of [Browser.UrlRequest](https://package.elm-lang.org/packages/elm/browser/latest/Browser#UrlRequest). Refer to the documentation for more detailed notes._

-}
type UrlRequest
    = InternalPath AppUrl
    | ExternalPage String


fromBrowserUrlRequest : Browser.UrlRequest -> UrlRequest
fromBrowserUrlRequest req =
    case req of
        Browser.Internal url ->
            InternalPath <| AppUrl.fromUrl url

        Browser.External url ->
            ExternalPage url


{-| Build headless application.
You can use `headless` to save your scenario document as a markdown file.
-}
headless :
    { init : Value -> Promise () ( flags, memory )
    , onLoad : flags -> Promise memory ()
    }
    -> Program flags memory
headless props =
    Platform.worker
        { init =
            \rawFlags ->
                let
                    procs : Promise (AppState flags memory) ()
                    procs =
                        bind
                            (props.init rawFlags
                                |> Core.liftPromiseMemory
                                    { get = \_ -> ()
                                    , set = \_ -> identity
                                    }
                            )
                        <|
                            \( flags, memory ) ->
                                [ modify <| \_ -> AppLoaded flags memory
                                , props.onLoad flags
                                    |> liftLoadedProcedure flags
                                ]

                    newState : Core.NewState (AppState flags memory)
                    newState =
                        Core.init AppLoading procs
                in
                ( newState.nextModel
                , newState.realCmds
                    |> Cmd.batch
                )
        , update = update
        , subscriptions = subscriptions
        }



-- Connect to TEA app


{-| TEA update function to execute your Procedures.
-}
update : Msg -> Model flags memory -> ( Model flags memory, Cmd Msg )
update msg model =
    let
        newState =
            Core.update msg model
    in
    ( newState.nextModel
    , newState.realCmds
        |> Cmd.batch
    )


{-| -}
view :
    { initView : Document
    , view : flags -> memory -> Document
    }
    -> Model flags memory
    -> Document
view props =
    Core.documentView <|
        \appState ->
            case appState of
                AppLoading ->
                    props.initView

                AppLoaded flags m ->
                    props.view flags m


{-| TEA subscriptions function to execute your Procedures.
-}
subscriptions : Model flags memory -> Sub Msg
subscriptions =
    Core.subscriptions


{-| Construct the initial TEA data from Procedures.
-}
init :
    { init : Value -> Promise () ( flags, memory )
    , onLoad : flags -> AppUrl -> NavKey -> Promise memory ()
    , onUrlRequest : flags -> UrlRequest -> NavKey -> Promise memory ()
    , onUrlChange : flags -> AppUrl -> NavKey -> Promise memory ()
    }
    -> Value
    -> Url
    -> Browser.Navigation.Key
    -> ( Model flags memory, Cmd Msg )
init props rawFlags initialUrl rawKey =
    let
        key =
            Core.RealKey rawKey

        initialPath =
            AppUrl.fromUrl initialUrl

        procs : Promise (AppState flags memory) ()
        procs =
            bind
                (props.init rawFlags
                    |> Core.liftPromiseMemory
                        { get = \_ -> ()
                        , set = \_ -> identity
                        }
                )
            <|
                \( flags, memory ) ->
                    [ modify <| \_ -> AppLoaded flags memory
                    , syncAll
                        [ Core.listenMsg <|
                            \msg ->
                                case msg of
                                    Core.UrlRequest req ->
                                        [ props.onUrlRequest flags (fromBrowserUrlRequest req) key
                                        ]

                                    Core.UrlChange url ->
                                        [ props.onUrlChange flags url key
                                        ]

                                    _ ->
                                        []
                        , props.onLoad flags initialPath key
                        ]
                        |> liftLoadedProcedure flags
                    ]

        newState =
            Core.init AppLoading procs
    in
    ( newState.nextModel
    , newState.realCmds
        |> Cmd.batch
    )


liftLoadedProcedure : flags -> Promise m () -> Promise (AppState flags m) ()
liftLoadedProcedure flags =
    Core.maybeLiftPromiseMemory
        { get =
            \m ->
                case m of
                    AppLoading ->
                        Nothing

                    AppLoaded _ a ->
                        Just a
        , set =
            \a _ ->
                AppLoaded flags a
        }
        >> void


{-| -}
type alias Msg =
    Core.Msg


{-| -}
type alias Model flags m =
    Core.Model (AppState flags m)


{-| TEA `onUrlChange` property value.
-}
onUrlChange : Url -> Msg
onUrlChange url =
    AppUrl.fromUrl url
        |> Core.UrlChange


{-| TEA `onUrlRequest` property value.
-}
onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    Core.UrlRequest
