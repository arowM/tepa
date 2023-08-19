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
    , neverResolved
    , bind, bind2, bind3, bindAll
    , succeed, sync
    , lazy
    , void
    , when
    , unless
    , withMaybe
    , syncAll
    , portRequest, portStream
    , map
    , liftMemory
    , andThen, bindAndThen
    , NavKey
    , UrlRequest(..)
    , Layer
    , layerState
    , newLayer
    , onLayer, LayerResult(..)
    , isOnSameLayer
    , layerView
    , ViewContext
    , mapViewContext
    , getValue, getValues
    , setValue
    , getCheck, getChecks, setCheck
    , getFormState, FormState
    , awaitViewEvent, awaitCustomViewEvent
    , viewEventStream, customViewEventStream
    , update
    , subscriptions
    , init
    , Msg
    , Model
    , onUrlChange
    , onUrlRequest
    , unsafePush
    )

{-| This module provides core functionality for TEPA.


# Application Entry point

@docs application
@docs ApplicationProps
@docs Program
@docs Document


# Key Concepts of TEPA


## Memory

TEPA has a single _Memory_ that holds all of the application state.
You specify the initial state of the Memory as the `init` property.

    {-| Your own type that represents your application state.

    It is common to declare [type alias](https://guide.elm-lang.org/types/type_aliases) for [record](https://guide.elm-lang.org/core_language#records).

    -}
    type alias Memory =
        { userName : String
        }

    {-| Initial state of `Memory`. Pass it as `init` property to the `application` function.
    -}
    init : Memory
    init =
        { userName = ""
        , pageState = InitialPageState
        }


## Flags

You can pass a JSON value, called _Flags_, to the TEPA application upon initialization.
Common uses are passing in API keys, environment variables, and user data.

The sample `index.js` in the `application` document passes `{ "loaded-at": Data.now() }` as flags.

    Elm.Main.init({
      // Create a `div` node in body and pass it as the parent node in which to render your TEPA application.
      node: document.body.appendChild(document.createElement("div")),
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


#### Primitive Promises

@docs sequence
@docs modify
@docs none
@docs currentState
@docs neverResolved


#### Bind results

`currentState` is resolved with the current state, but how to retrieve the value? You can use `bind` to pass the result into another sequence of procedures.

@docs bind, bind2, bind3, bindAll
@docs succeed, sync


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

@docs portRequest, portStream


#### Streams

Some Promises are resolved with `Stream`, which is an advanced technique for controlling Promises precisely.
See [Tepa.Stream](./Tepa-Stream) for details.


#### Lower level functions

@docs map
@docs liftMemory
@docs andThen, bindAndThen


### Application Procedures

There are three type of procedures you specify as an `application` property.


### Main Procedure

The main procedure, set as the value of the `procedure` property, is executed on every page load.
It takes three arguments:

  - Flags: JSON value passed by JavaScript on initialization.

  - Application URL: The initial URL requested by a user.

    TEPA uses the [`AppUrl`](https://package.elm-lang.org/packages/lydell/elm-app-url/latest/AppUrl) type to represent internal URLs.

  - Navigation Key: Required by functions exposed by `Tepa.Navigation`.

@docs NavKey


### Procedure to handle page transition requests.

You specify the procedure for handling page transition requests as `onUrlRequest` property of `application`.
It takes three arguments:

  - Flags: JSON value passed by JavaScript on initialization.
  - Requested URL: [`UrlRequest`](#UrlRequest) value that indecates requested URL.
  - Navigation Key: Required by functions exposed by `Tepa.Navigation`.

@docs UrlRequest

In most cases, you will declare the function as follows:

    onUrlRequest : Value -> Tepa.UrlRequest -> NavKey -> Promise Memory ()
    onUrlRequest _ urlRequest key =
        case urlRequest of
            Tepa.InternalPath url ->
                Nav.pushPath key url

            Tepa.ExternalPage href ->
                Nav.load href


### Procedure executed after URL changed

Immediately after the URL is changed, `onUrlChange` property of `application` function is evaluated.
A common use case is to change the page state based on the new URL.

It takes three arguments:

  - Flags: JSON value passed by JavaScript on initialization.
  - New URL: Loaded new URL.
  - Navigation Key: Required by functions exposed by `Tepa.Navigation`.

In [spa-sample](https://github.com/arowM/tepa/tree/main/spa-sample), the `onUrlChange` function retrieves the session information, such as the logged-in user data, and then calls the main procedure.


## Layer

The _Layer_ is the concept of an isolated space. You can create a new layer with the `newLayer` function, execute a procedure on a layer with the `onLayer` function, delete or overwrite the existing layer with `modify`.

@docs Layer
@docs layerState
@docs newLayer
@docs onLayer, LayerResult
@docs isOnSameLayer

A main use of the layer is to manage page transition. See that you have the following `Page` type to represent your page state.

    import Page.Home as PageHome
    import Page.Users as PageUsers
    import Tepa exposing (Layer)

    type alias Memory =
        { page : Page
        }

    type Page
        = PageLoading
        | PageHome PageHome.Memory
        | PageUsers PageUsers.Memory

This approach may seem to work at first, but it may exhibit unexpected behavior, such as

1.  A user opens the home page.
      - The main procedure is executed, replacing the `page` in memory with the `PageHome param` (where `param` is the initial value of `PageHome.Memory`).
      - The main procedure continues to execute the procedure for the home page.
      - The home procedure is executed to replace the image to be displayed every 20 seconds (process A).
2.  The user is redirected to the user list page.
      - The `onChangeUrl` procedure is executed and the `page` in memory is replaced with `PageUsers param` (`param` is the initial value of `PageUsers.Memory`).
      - At this point, process A is asleep.
3.  The user immediately changes to the home page.
      - The `onChangeUrl` procedure is executed, and the `page` in memory is again replaced with the `PageHome param` (`param` is the initial value of `PageHome.Memory`).
      - The `onChangeUrl` procedure continues to execute a new procedure for the home page.
      - A new process (Process B) runs to update the display image every 20 seconds, overlapping with Process A.
      - Now it is time for Process A to refresh the image.
          - Because the memory state is still `PageHome`, Process A cannot detect that the page has changed in the middle of the process.
      - Process B also refreshes images periodically, so the slideshow now refreshes images at odd times!

The reason for this unexpected behavior is that the process cannot determine that the page state has changed in the middle of a page based only on the memory state. To solve this problem, you can use _Layer_.

    type Page
        = PageLoading
        | PageHome (Layer PageHome.Memory)
        | PageUsers (Layer PageUsers.Memory)

The new `Page' definition above uses`Layer' to wrap each page memory state. A procedure executed on a layer will be aborted when the layer has expired by being overwritten by another layer. This allows you to avoid running duplicate procedures.


## View

The _View_ determines how your application is rendered in the web browser, based only on the current layer state.
You use [elm/html](https://package.elm-lang.org/packages/elm/html/latest/) to tell the web browser how to render the page.
Note that TEPA **does not** use the `Html.Events` module.


### Define Views

To define View function, you use `layerView`.

@docs layerView
@docs ViewContext
@docs mapViewContext

    import Tepa exposing (Document, Layer)

    view : Layer Memory -> Document
    view =
        Tepa.layerView <|
            \{ state } ->
                { title = "Sample App"
                , body =
                    [ case state.page of
                        PageNotFound _ ->
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

    import Html exposing (Html)
    import Html.Attributes as Attributes
    import Tepa exposing (Layer, Msg)

    formView : Layer Memory -> Html Msg
    formView =
        Tepa.layerView <|
            \{ setKey, values } ->
                Html.form
                    [ Attributes.novalidate True
                    ]
                    [ Html.label []
                        [ Html.text "Name: "
                        , Html.input
                            (setKey "form_name"
                                ++ [ Attributes.type_ "text"
                                   , Attributes.placeholder "Sakura-chan"
                                   ]
                            )
                            []
                        ]
                    , Html.button
                        (setKey "form_submit")
                        [ Html.text "Submit"
                        ]
                    , errors values
                    ]

    errors : Dict String String -> Html Msg
    errors formValues =
        case Dict.get "form_name" formValues of
            Just "" ->
                Html.text "Name is required."

            _ ->
                Html.text ""

This example uses the `setKey` of the `ViewContext` to set the key named "form\_name" to the name input, and "form\_submit" to the submit button. In this way, you can retrieve the user input value with the `values` field of the `ViewContext`.

Note that keys on the same Layer must be unique.


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


### Helpers to handle form state

@docs getFormState, FormState


### Handle View events on Procedure

To capture View events on Procedure, you can use `awaitViewEvent` or `awaitCustomViewEvent`.

@docs awaitViewEvent, awaitCustomViewEvent

For more precise control, you can use [`Stream`](./Tepa-Stream) versions.

@docs viewEventStream, customViewEventStream

Note that the Procedure can only capture events in Views on the same Layer that the Procedure is executed.


### Freshness of input values

The user input values obtained by the `value` field of the `ViewContext` and `getValue` / `getValues` in the Procedure are updated whenever the `change` event of the target element occurs. So if you want to implement something like an incremental search, getting values in this ways will not give you the latest input values.
Use [search type of input element](https://developer.mozilla.org/docs/Web/HTML/Element/input/search) or capture the `input` event with `awaitCustomViewEvent` to handle this situation.


# Scenario

To create user scenarios and generate tests for them, see the [`Tepa.Scenario`](./Tepa-Scenario) module.


# Connect to TEA app

_For TEA users: If you have an existing application built with TEA, you can partially replace it with TEPA._

@docs update
@docs subscriptions
@docs init
@docs Msg
@docs Model
@docs onUrlChange
@docs onUrlRequest


# Unsafe functions

@docs unsafePush

-}

import AppUrl exposing (AppUrl)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Internal.Core as Core
    exposing
        ( Msg(..)
        , Stream
        )
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Url exposing (Url)


{-| -}
type alias Promise memory result =
    Core.Promise memory result


{-| Build a Promise that is always completed with the given value immediately.

This is usefull for building Promise for concurrent operations with `sync`.

-}
succeed : a -> Promise memory a
succeed =
    Core.succeedPromise


{-| Transform a resulting value produced by a Promise.
-}
map : (a -> b) -> Promise m a -> Promise m b
map =
    Core.mapPromise


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


{-| Run Promise on the specified Layer.

It determines the Layer to execute a given Promise with the parameter `get` based on the current memory state.

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
                    { get =
                        \m ->
                            case m.page of
                                PageLogin layer ->
                                    Just layer

                                _ ->
                                    Nothing
                    , set =
                        \layer m ->
                            { m | page = PageLogin layer }
                    }
            ]

Here, `modify` is called just before `onLayer`, and the `bind` guarantees that the `onLayer` request is generated based on the result of `modify`, even if there is another Promise to be executed in concurrently.

If the target layer disappears during the execution of a given Promise, the rest of the process is aborted, and if the result of the `get` parameter is `Nothing` at the time `onLayer` is called, the process is also aborted there.

-}
onLayer :
    { get : m -> Maybe (Layer m1)
    , set : Layer m1 -> m -> m
    }
    -> Promise m1 a
    -> Promise m (LayerResult a)
onLayer o prom1 =
    Core.onLayer o prom1
        |> map fromCoreLayerResult


fromCoreLayerResult : Core.LayerResult a -> LayerResult a
fromCoreLayerResult res =
    case res of
        Core.LayerOk a ->
            LayerOk a

        Core.LayerNotExists ->
            LayerNotExists

        Core.LayerExpired ->
            LayerExpired


{-| -}
type LayerResult a
    = LayerOk a
    | LayerNotExists
    | LayerExpired


{-| _Layer_ is a concept that deals with a part of the application. It can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m =
    Core.Layer m



-- Composition


{-| Build a new Promise that evaluate two Promises sequentially.
-}
andThen : (a -> Promise m b) -> Promise m a -> Promise m b
andThen =
    Core.andThenPromise


{-| Flipped version of `andThen`.

You can use `bindAndThen` to bind some Promise result to a variable:

    bindAndThen somePromise <|
        \result ->
            anotherPromise result

-}
bindAndThen : Promise m a -> (a -> Promise m b) -> Promise m b
bindAndThen p f =
    andThen f p


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
            [ execultedFirst
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
syncAll =
    Core.concurrent


{-| Construct a Promise that modifies the Memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the Memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Promise m ()
modify =
    Core.modify


{-| Lower level function to push TEA Commands.

For detailed documentation and testing, do not use this. We recommend using special functions like `Tepa.Http.Request`.

If you think you need to use this function, please check the following points:

1.  Check to see if there is an alternative function for TEPA that does what you want to do
2.  If not, report it in an issue on GitHub.
3.  If you still want to use it, use it at your own risk and do not ask any questions or bother the TEPA developers.

-}
unsafePush : (m -> Cmd Msg) -> Promise m ()
unsafePush =
    Core.push


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


{-| Promise that never be resolved.
You can use `neverResolved` to prevent the current Layer from expiring.
-}
neverResolved : Promise m a
neverResolved =
    Core.neverResolved


{-| Promise that requests the current Memory state of a Layer.
-}
layerState : Layer m1 -> Promise m m1
layerState (Core.Layer layer) =
    succeed layer.state


{-| Build a Promise to send one outgoing [port](https://guide.elm-lang.org/interop/ports) Message and receive the corresponding incoming port Message only once.

For example, we can use `portRequest` to get localStorage value safely.

In JavaScript side:

```js
app.ports.requestGetLocalName.subscribe((req) => {
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
```

In Elm side:

    import Json.Decode as JD
    import Json.Encode as JE exposing (Value)

    port requestGetLocalName : Value -> Cmd msg

    port receiveGetLocalName : (Value -> msg) -> Sub msg

    type alias LocalNameResponse =
        { name : Maybe String
        }

    requestLocalName : String -> Promise Memory Event LocalNameResponse
    requestLocalName userId =
        portRequest
            { ports =
                { request = requestGetLocalName
                , response = receiveGetLocalName
                }
            , requestBody =
                JE.object
                    [ ( "requestId", requestId )
                    , ( "body"
                      , JE.object
                            [ ( "userId"
                              , JE.string userId
                              )
                            ]
                      )
                    ]
            }

-}
portRequest :
    { ports :
        { request : Value -> Cmd Msg
        , response : (Value -> Msg) -> Sub Msg
        }
    , requestBody : Value
    }
    -> Promise m Value
portRequest =
    Core.portRequest


{-| Similar to `portRequest`, but `portStream` can receive many responses.
One of the use cases is to receive WebSocket messages.

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer. If you call `portStream` in recursive Promise, it spawns listeners many times!

-}
portStream :
    { ports :
        { request : Value -> Cmd Msg
        , response : (Value -> Msg) -> Sub Msg
        }
    , requestBody : Value
    }
    -> Promise m (Stream Value)
portStream =
    Core.portStream


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


{-| -}
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


{-| -}
setCheck : String -> Bool -> Promise m ()
setCheck =
    Core.setCheck


{-| Helper type alias representing a form state.
-}
type alias FormState =
    { values : Dict String String
    , checks : Dict String Bool
    }


{-| Helper function to run `getValues` and `getChecks` concurrently.
-}
getFormState : Promise m FormState
getFormState =
    getValues
        |> andThen
            (\values ->
                map (\checks -> FormState values checks) getChecks
            )



-- Layer


{-| -}
newLayer : m1 -> Promise m (Layer m1)
newLayer =
    Core.newLayer


{-| Check if the both are on the same layer.
One of the use cases is to find the target element from the list of layers.

See [`Widget.Toast`](https://github.com/arowM/tepa/blob/main/spa-sample/src/Widget/Toast.elm) in the spa-sample for detail.

-}
isOnSameLayer : Layer m -> Layer m -> Bool
isOnSameLayer (Core.Layer layer1) (Core.Layer layer2) =
    layer1.id == layer2.id


{-| -}
layerView :
    (ViewContext m -> view)
    -> Layer m
    -> view
layerView f layer =
    f <| Core.viewArgs layer


{-| -}
type alias ViewContext m =
    { state : m
    , setKey : String -> List (Attribute Msg)
    , values : Dict String String
    , checks : Dict String Bool
    , layerId : String
    }


{-| -}
mapViewContext : (m -> m1) -> ViewContext m -> ViewContext m1
mapViewContext f context =
    { state = f context.state
    , setKey = context.setKey
    , values = context.values
    , checks = context.checks
    , layerId = context.layerId
    }



-- Browser alternatives


{-| Entry point for building your applications.

TEPA application is converted to JavaScript, so you can import it to use in your JavaScript (or TypeScript) file.
If you are a [Parcel](https://parceljs.org/) user, you can load the Elm file that contains the `main` function built with `application`.

    <!-- index.html -->

    <html>
      <body>
        <script src="./index.js"></script>
      </body>
    </html>

    // index.js

    // Specify your file that contains `main` function here.
    import { Elm } from "./Main.elm"

    Elm.Main.init({
      // Create a `div` node in body and pass it as the parent node in which to render your TEPA application.
      node: document.body.appendChild(document.createElement("div")),
      // Pass some JSON value to TEPA upon initialization.
      flags: {
        "loaded-at": Date.now()
      }
    })

    -- Main.elm

    import Json.Decode (Value)
    import Tepa exposing (AppUrl, NavKey, Program, Promise)

    main : Program Memory
    main =
        Tepa.application
            { init = init
            , procedure = procedure
            , view = view
            , onUrlRequest = onUrlRequest
            , onUrlChange = onUrlChange
            }

    {- Data type for representing your application state.
    -}
    type alias Memory =
        { ...
        , ...
        }

    {-| Your implementation for `init`.
    -}
    init : Memory
    init =
        Debug.todo ""

    {-| Your implementation for `procedure`.
    -}
    procedure : Value -> AppUrl -> NavKey -> Promise Memory ()
    procedure flags url key =
        Debug.todo ""

    {-| Your implementation for `view`.
    -}
    view : Layer Memory -> Document
    view layer =
        Debug.todo ""

    {-| Your implementation for `onUrlRequest`.
    -}
    onUrlRequest : Value -> UrlRequest -> NavKey -> Promise memory ()
    onUrlRequest flags url key =
        Debug.todo ""

    {-| Your implementation for `onUrlChange`.
    -}
    onUrlChange : Value -> AppUrl -> NavKey -> Promise memory ()
    onUrlChange flags url key =
        Debug.todo ""

If you are not familiar with Elm language, we recommend you to check [Core Language](https://guide.elm-lang.org/core_language), [Types](https://guide.elm-lang.org/types/), and [Error Handling](https://guide.elm-lang.org/error_handling/) section of the Elm guide.

_For TEA users: If you have an existing TEA application, you can use the [low level API](#connect-to-tea-app) to partially integrate TEPA into your TEA application._

-}
application :
    ApplicationProps memory
    -> Program memory
application props =
    Browser.application
        { init =
            \flags url key ->
                init props.init
                    { procedure =
                        props.procedure flags (AppUrl.fromUrl url) (Core.RealKey key)
                    , onUrlChange =
                        \newUrl ->
                            props.onUrlChange flags newUrl (Core.RealKey key)
                    , onUrlRequest =
                        \req ->
                            props.onUrlRequest flags req (Core.RealKey key)
                    }
        , view = documentView props.view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


{-| Property values for your application.
-}
type alias ApplicationProps memory =
    { init : memory
    , procedure : Value -> AppUrl -> NavKey -> Promise memory ()
    , view : memory -> Document
    , onUrlRequest : Value -> UrlRequest -> NavKey -> Promise memory ()
    , onUrlChange : Value -> AppUrl -> NavKey -> Promise memory ()
    }


{-| A navigation key is needed to create navigation procedures exposed by the [Tepa.Navigation](./Navigation) module.

You only get access to a `NavKey` when you create your program with `application`, guaranteeing that your program is equipped to detect these URL changes. If `NavKey` values were available in other kinds of programs, unsuspecting programmers would be sure to run into some [annoying bugs](https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md) and learn a bunch of techniques the hard way!

_This is the TEPA version of [Browser.Navigation.Key](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#Key)._

-}
type alias NavKey =
    Core.NavKey


{-| A `Program` describes an TEPA program.

_An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program)._

-}
type alias Program memory =
    Platform.Program Value (Model memory) Msg


{-| This data specifies the `<title>` and all of the nodes that should go in the `<body>`. This means you can update the title as your application changes. Maybe your "single-page app" navigates to a "different page", maybe a calendar app shows an accurate date in the title, etc.

_This is the TEPA version of [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document)._

-}
type alias Document =
    { title : String
    , body : List (Html Msg)
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



-- Connect to TEA app


{-| TEA update function to execute your Procedures.
-}
update : Msg -> Model memory -> ( Model memory, Cmd Msg )
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
documentView : (memory -> Document) -> Model memory -> Document
documentView =
    Core.documentView


{-| TEA subscriptions function to execute your Procedures.
-}
subscriptions : Model memory -> Sub Msg
subscriptions =
    Core.subscriptions


{-| Construct the initial TEA data from Procedures.
-}
init :
    memory
    ->
        { procedure : Promise memory ()
        , onUrlChange : AppUrl -> Promise memory ()
        , onUrlRequest : UrlRequest -> Promise memory ()
        }
    -> ( Model memory, Cmd Msg )
init memory param =
    let
        procs =
            syncAll
                [ Core.listenMsg <|
                    \msg ->
                        case msg of
                            Core.UrlRequest req ->
                                [ param.onUrlRequest (fromBrowserUrlRequest req)
                                ]

                            Core.UrlChange url ->
                                [ param.onUrlChange url
                                ]

                            _ ->
                                []
                , param.procedure
                ]

        newState =
            Core.init memory procs
    in
    ( newState.nextModel
    , newState.realCmds
        |> Cmd.batch
    )


{-| -}
type alias Msg =
    Core.Msg


{-| -}
type alias Model m =
    Core.Model m


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
