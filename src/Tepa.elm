module Tepa exposing
    ( application
    , ApplicationProps
    , NavKey
    , Program
    , Document
    , UrlRequest(..)
    , Promise
    , map
    , liftMemory
    , andThen, bindAndThen
    , sync
    , void
    , sequence, andThenSequence, none
    , syncAll
    , bind, bind2, bind3, bindAll
    , modify, lazy
    , when
    , unless
    , withMaybe
    , succeed
    , currentState, layerState
    , portRequest, portStream
    , awaitViewEvent, awaitCustomViewEvent
    , viewEventStream, customViewEventStream
    , getValue, getValues, setValue
    , Layer, onLayer, LayerResult(..)
    , newLayer, isOnSameLayer
    , ViewContext, mapViewContext, layerView, keyedLayerView, layerDocument
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
@docs NavKey
@docs Program
@docs Document
@docs UrlRequest


# Promise

@docs Promise


# Transformers

@docs map
@docs liftMemory


# Composition

@docs andThen, bindAndThen
@docs sync


# Procedures

Promises that returns `()` are called as a _Procedure_.

@docs void
@docs sequence, andThenSequence, none
@docs syncAll
@docs bind, bind2, bind3, bindAll
@docs modify, lazy


# Helper Procedures

@docs when
@docs unless
@docs withMaybe


# Primitive Promises

@docs succeed
@docs currentState, layerState
@docs portRequest, portStream
@docs awaitViewEvent, awaitCustomViewEvent
@docs viewEventStream, customViewEventStream
@docs getValue, getValues, setValue


# Layer

@docs Layer, onLayer, LayerResult
@docs newLayer, isOnSameLayer
@docs ViewContext, mapViewContext, layerView, keyedLayerView, layerDocument


# Connect to TEA app

@docs update
@docs subscriptions
@docs init
@docs Msg
@docs Model
@docs onUrlChange
@docs onUrlRequest


# Lower level functions

@docs unsafePush

-}

import AppUrl exposing (AppUrl)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Internal.Core as Core
    exposing
        ( Msg(..)
        , Promise(..)
        , Stream
        )
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Url exposing (Url)


{-| The Promise represents the eventual completion of an operation and its resulting value. Similar to [Promise in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise).

The main difference from Promise in JavaScript is that Promise in TEPA does not be rejected. When you represent rejected state, just return `Result` value as its result.

-}
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


{-| Run promise on the specified Layer.

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


{-| Run another Promise concurrently to get both results.

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


{-| -}
void : Promise m a -> Promise m ()
void =
    map (\_ -> ())


{-| Concatenate given sequence of Procedures.
-}
sequence : List (Promise m ()) -> Promise m ()
sequence =
    Core.sequence


{-| Evaluate sequence of Procedures that depend on the result of a Promise.

    andThenSequence f =
        andThen (f >> sequence)

-}
andThenSequence : (a -> List (Promise m ())) -> Promise m a -> Promise m ()
andThenSequence f =
    andThen (f >> sequence)


{-| Flipped version of `andThenSequence`.

You can use `bind` to bind some Promise result to a variable:

    bind somePromise <|
        \result ->
            [ yourProcedure result
            ]

-}
bind : Promise m a -> (a -> List (Promise m ())) -> Promise m ()
bind promise f =
    andThenSequence f promise


{-| Run two Promises concurrently, and bind the results to variables when both are complete.
-}
bind2 : Promise m a -> Promise m b -> (a -> b -> List (Promise m ())) -> Promise m ()
bind2 p1 p2 f =
    succeed Tuple.pair
        |> sync p1
        |> sync p2
        |> andThenSequence
            (\( a, b ) -> f a b)


{-| Run three Promises concurrently, and bind the results to variables when all are complete.

If you need to bind more Promises, use `sync`.

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


{-| Lower level function to push Commands.

For detailed documentation and testing, do not use this. We recommend using special functions like `Tepa.Http.Request`.

If you think you need to use this function, please check the following points:

1.  check to see if there is an alternative function for TEPA that does what you want to do
2.  if not, report it in an issue on GitHub.
3.  if you still want to use it, use it at your own risk and do not ask any questions or bother the TEPA developers.

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

Note that this returns the Memory state when it is resolved:

    type alias Response =
        { memoryOnRequest : Memory
        , response1 : Response1
        }

    -- The `currentState` in this Promise returns the memory state when it called,
    -- i.e., when the `request1` is called, but not when the `request1` receives response.
    myPromise : Promise Memory Event Response
    myPromise =
        succeed Response
            (async currentState)
            (async request1)

    request1 : Promise Common Memory Event Response1
    request1 =
        Debug.todo ""

-}
currentState : Promise m m
currentState =
    Core.currentState


{-| -}
layerState : Layer m1 -> Promise m m1
layerState (Core.Layer layer) =
    succeed layer.state


{-| Build a Promise to send one outgoing port Message and receive the corresponding incoming port Message only once.

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
            { name = "Request for localStorage value"
            , ports =
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
Say you have WebSocket API endpoint.

    TODO some sample codes

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer. If you call `portStream` in recursive Promise, it spawns listeners many times!

    TODO some sample codes

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


{-| -}
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


{-| -}
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


{-| -}
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


{-| `Nothing` means that the key does not exist or the value of the element specified by the key has not been changed.
-}
getValue : String -> Promise m (Maybe String)
getValue =
    Core.getValue


{-| -}
getValues : Promise m (Dict String String)
getValues =
    Core.getValues


{-| -}
setValue : String -> String -> Promise m ()
setValue =
    Core.setValue



-- Layer


{-| -}
newLayer : m1 -> Promise m (Layer m1)
newLayer =
    Core.newLayer


{-| -}
isOnSameLayer : Layer m -> Layer m -> Bool
isOnSameLayer (Core.Layer layer1) (Core.Layer layer2) =
    layer1.id == layer2.id


{-| -}
layerView :
    (ViewContext m -> Html Msg)
    -> Layer m
    -> Html Msg
layerView =
    Core.layerView


{-| -}
type alias ViewContext m =
    { state : m
    , setKey : String -> List (Attribute Msg)
    , values : Dict String String
    }


{-| -}
mapViewContext : (m -> m1) -> ViewContext m -> ViewContext m1
mapViewContext f context =
    { state = f context.state
    , setKey = context.setKey
    , values = context.values
    }


{-| -}
keyedLayerView :
    (ViewContext m -> Html Msg)
    -> Layer m
    -> ( String, Html Msg )
keyedLayerView =
    Core.keyedLayerView


{-| -}
layerDocument :
    (ViewContext m -> Document Msg)
    -> Layer m
    -> Document Msg
layerDocument =
    Core.layerDocument



-- Browser alternatives


{-| Entry point for building your applications.

If you have an existing TEA app, you can use the [low level API](#connect-to-tea-app) to partially integrate TEPA into the TEA app.

-}
application :
    ApplicationProps flags memory
    -> Program flags memory
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

    - `init`: Initial Memory state of your application.

        TEPA has a **Memory** that holds all the application state. This property specifies the state of the Memory when the application is loaded.

    - `procedure`: How your application processes.

        It takes three arguments:

        - flags: JavaScript can pass in data when starting your program.
        - app URL: Initial URL requested by a user.
        - navigation key: Required by functions exposed by `Tepa.Navigation`.

    - `view`: How your application looks.

        It is determined only by current Memory state.

    - `onUrlRequest`: How to handle page transition requests.

        It takes three arguments:

        - flags: JavaScript can pass in data when starting your program.
        - URL request: Requested URL.
        - navigation key: Required by functions exposed by `Tepa.Navigation`.

    - `onUrlChange`: What to do after page load or transition.

        It takes three arguments:

        - flags: JavaScript can pass in data when starting your program.
        - app URL: Loaded new URL.
        - navigation key: Required by functions exposed by `Tepa.Navigation`.

-}
type alias ApplicationProps flags memory =
    { init : memory
    , procedure : flags -> AppUrl -> NavKey -> Promise memory ()
    , view : Layer memory -> Document Msg
    , onUrlRequest : flags -> UrlRequest -> NavKey -> Promise memory ()
    , onUrlChange : flags -> AppUrl -> NavKey -> Promise memory ()
    }


{-| A navigation key is needed to create navigation procedures exposed by the [Tepa.Navigation](./Navigation) module.

You only get access to a `NavKey` when you create your program with `application`, guaranteeing that your program is equipped to detect these URL changes. If `NavKey` values were available in other kinds of programs, unsuspecting programmers would be sure to run into some [annoying bugs](https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md) and learn a bunch of techniques the hard way!

This is the TEPA version of [Browser.Navigation.Key](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#Key).

-}
type alias NavKey =
    Core.NavKey


{-| A `Program` describes an TEPA program.

An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).

-}
type alias Program flags memory =
    Platform.Program flags (Model memory) Msg


{-| This data specifies the `<title>` and all of the nodes that should go in the `<body>`. This means you can update the title as your application changes. Maybe your "single-page app" navigates to a "different page", maybe a calendar app shows an accurate date in the title, etc.

Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.

-}
type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


{-| All links in an [`application`](#application) create a `UrlRequest`. So
when you click `<a href="/home">Home</a>`, it does not just navigate! It
notifies `onUrlRequest` that the user wants to change the URL.

This is the TEPA version of [Browser.UrlRequest](https://package.elm-lang.org/packages/elm/browser/latest/Browser#UrlRequest).
Refer to the `Browser.UrlRequest` documentation for more detailed notes.

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
documentView : (Layer memory -> Document Msg) -> Model memory -> Document Msg
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
