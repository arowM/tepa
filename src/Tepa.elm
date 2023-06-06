module Tepa exposing
    ( application
    , ApplicationProps
    , NavKey
    , Program
    , Document
    , UrlRequest(..)
    , Promise
    , map
    , liftEvent, liftMemory
    , orFaster
    , andThen, bindAndThen
    , sync
    , Void, void
    , sequence, andThenSequence, none, cancel
    , syncAll
    , bind, bind2, bind3
    , modify, listen, lazy
    , when
    , unless
    , withMaybe
    , succeed
    , currentState, layerMemory, layerEvent
    , portRequest
    , withLayerEvent, listenLayerEvent
    , Layer, onLayer
    , newLayer, isOnSameLayer
    , layerView, keyedLayerView, layerDocument, eventAttr, eventMixin
    , update
    , subscriptions
    , init
    , Msg
    , mapMsg
    , Model
    , onUrlChange
    , onUrlRequest
    , unsafePush
    )

{-|


# Application Entry points

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives to build your apps.

The [low level API](#connect-to-tea-app) is also available for more advanced use cases, which enables you to introduce TEPA partially into your existing TEA app.

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
@docs liftEvent, liftMemory


# Composition

@docs orFaster
@docs andThen, bindAndThen
@docs sync


# Procedures

Promises that returns `Void` are called as a _Procedure_.

@docs Void, void
@docs sequence, andThenSequence, none, cancel
@docs syncAll
@docs bind, bind2, bind3
@docs modify, listen, lazy


# Helper Procedures

@docs when
@docs unless
@docs withMaybe


# Primitive Promises

@docs succeed
@docs currentState, layerMemory, layerEvent
@docs portRequest


# Helper Promises

@docs withLayerEvent, listenLayerEvent


# Layer

@docs Layer, onLayer
@docs newLayer, isOnSameLayer
@docs layerView, keyedLayerView, layerDocument, eventAttr, eventMixin


# Connect to TEA app

@docs update
@docs subscriptions
@docs init
@docs Msg
@docs mapMsg
@docs Model
@docs onUrlChange
@docs onUrlRequest


# Lower level functions

@docs unsafePush

-}

import AppUrl exposing (AppUrl)
import Browser
import Html exposing (Attribute, Html)
import Internal.Core as Core
    exposing
        ( Msg(..)
        , Promise(..)
        )
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Url exposing (Url)


{-| The Promise represents the eventual completion of an operation and its resulting value. Similar to [Promise in JS](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise).
-}
type alias Promise memory event result =
    Core.Promise memory event result


{-| Build a Promise that is always completed with the given value immediately.

This is usefull for building Promise for concurrent operations with `sync`.

-}
succeed : a -> Promise memory event a
succeed =
    Core.succeedPromise


{-| Transform a resulting value produced by a Promise.
-}
map : (a -> b) -> Promise m e a -> Promise m e b
map =
    Core.mapPromise


{-| Transform the Promise Memory.
-}
liftMemory :
    { get : m0 -> m1
    , set : m1 -> m0 -> m0
    }
    -> Promise m1 e a
    -> Promise m0 e a
liftMemory =
    Core.liftPromiseMemory


{-| Transform the Events produced or consumed by a Procedure.
-}
liftEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Promise m e1 a
    -> Promise m e0 a
liftEvent =
    Core.liftPromiseEvent


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
    -> Promise m1 e a
    -> Promise m e a
onLayer param prom1 =
    bindAndThen currentState <|
        \state ->
            case param.get state of
                Nothing ->
                    Core.cancel

                Just (Core.Layer lid _) ->
                    Core.onLayer
                        { get =
                            \m ->
                                param.get m
                                    |> Maybe.andThen
                                        (\(Core.Layer lid_ m1) ->
                                            if lid == lid_ then
                                                Just m1

                                            else
                                                Nothing
                                        )
                        , set =
                            \m1 ->
                                param.set (Core.Layer lid m1)
                        , layerId = Core.ThisLayerId lid
                        }
                        prom1


{-| _Layer_ is a concept that deals with a part of the application. It can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m =
    Core.Layer m



-- Composition


{-| Run another Promise concurrently to get the faster result.

May you want to set timeout on your request:

    requestWithTimeout : Promise Memory Event (Result () Response)
    requestWithTimeout =
        myRequest
            |> map Ok
            |> orFaster
                (sleep 10000
                    |> map Err
                )

    sleep : Promise Memory Event Void
    sleep =
        Debug.todo ""

-}
orFaster : Promise m e a -> Promise m e a -> Promise m e a
orFaster =
    Core.andRacePromise


{-| Build a new Promise that evaluate two Promises sequentially.
-}
andThen : (a -> Promise m e b) -> Promise m e a -> Promise m e b
andThen =
    Core.andThenPromise


{-| Flipped version of `andThen`.

You can use `bindAndThen` to bind some Promise result to a variable:

    bindAndThen somePromise <|
        \result ->
            anotherPromise result

-}
bindAndThen : Promise m e a -> (a -> Promise m e b) -> Promise m e b
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
sync : Promise m e a -> Promise m e (a -> b) -> Promise m e b
sync =
    Core.syncPromise



-- Procedures


{-| -}
type alias Void =
    Core.Void


{-| -}
void : Promise m e a -> Promise m e Void
void =
    Core.void


{-| Concatenate given sequence of Procedures.
-}
sequence : List (Promise m e Void) -> Promise m e Void
sequence =
    Core.sequence


{-| Evaluate sequence of Procedures that depend on the result of a Promise.

    andThenSequence f =
        andThen (f >> sequence)

-}
andThenSequence : (a -> List (Promise m e Void)) -> Promise m e a -> Promise m e Void
andThenSequence f =
    andThen (f >> sequence)


{-| Flipped version of `andThenSequence`.

You can use `bind` to bind some Promise result to a variable:

    bind somePromise <|
        \result ->
            [ yourProcedure result
            ]

-}
bind : Promise m e a -> (a -> List (Promise m e Void)) -> Promise m e Void
bind promise f =
    andThenSequence f promise


{-| Run two Promises concurrently, and bind the results to variables when both are complete.
-}
bind2 : Promise m e a -> Promise m e b -> (a -> b -> List (Promise m e Void)) -> Promise m e Void
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
    Promise m e a
    -> Promise m e b
    -> Promise m e c
    -> (a -> b -> c -> List (Promise m e Void))
    -> Promise m e Void
bind3 p1 p2 p3 f =
    succeed (\a b c -> ( a, b, c ))
        |> sync p1
        |> sync p2
        |> sync p3
        |> andThenSequence
            (\( a, b, c ) -> f a b c)


{-| Procedure that does nothing.
-}
none : Promise m e Void
none =
    Core.none


{-| Cancel all the subsequent Procedures.
-}
cancel : Promise m e Void
cancel =
    Core.cancel


{-| Run Procedures concurrently, and await all to be completed.
-}
syncAll : List (Promise m e Void) -> Promise m e Void
syncAll =
    Core.concurrent


{-| Construct a Promise that modifies the Memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the Memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Promise m e Void
modify =
    Core.modify


{-| Lower level function to push Commands.

For detailed documentation and testing, do not use this. We recommend using special functions like `Tepa.Http.Request`.

If you think you need to use this function, please check the following points:

1.  check to see if there is an alternative function for TEPA that does what you want to do
2.  if not, report it in an issue on GitHub.
3.  if you still want to use it, use it at your own risk and do not ask any questions or bother the TEPA developers.

-}
unsafePush : (m -> Cmd (Msg e)) -> Promise m e Void
unsafePush =
    Core.push


{-| Construct a Promise that start Subscription and listen to its Events till the Layer expires.

TODO Browser.Events

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer.

    myProcedures : List (Promise Memory Event Void)
    myProcedures =
        [ newLayer myLayerPosition initValue
            |> andThen
                (\(myLayer, myPointer) ->
                    syncAll
                        [ listen
                            { "tick-listener"
                            , subscription = \_ ->
                                Time.every 1000 Tick
                            , handler = onEveryTick
                            }
                        , Debug.todo "Main Promise"
                        ]
                )

    onEveryTick : Event -> List (Promise c m e Void)
    onEveryTick event =
        case event of
            Tick time ->
                Debug.todo "Sequence of Promises"
            _ ->
                []

-}
listen :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Promise m e Void)
    }
    -> Promise m e Void
listen =
    Core.listen


{-| -}
lazy : (() -> Promise m e Void) -> Promise m e Void
lazy =
    Core.lazy



-- Helper Procedures


{-| Evaluate the sequence of Procedures only if the first argument is `True`, otherwise same as `none`.
-}
when : Bool -> List (Promise m e Void) -> Promise m e Void
when p ps =
    if p then
        sequence ps

    else
        none


{-| Evaluate the sequence of Procedures only if the first argument is `False`, otherwise same as `none`.
-}
unless : Bool -> List (Promise m e Void) -> Promise m e Void
unless p =
    when (not p)


{-| Evaluate the sequence of Procedures returned by the callback function only if the first argument is `Just`, otherwise same as `none`.
-}
withMaybe : Maybe a -> (a -> List (Promise m e Void)) -> Promise m e Void
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            sequence <| f a


{-| Run callback function when the Layer received an event; if the callback function returns empty List, it awaits another event again.
-}
withLayerEvent : (e -> List (Promise m e Void)) -> Promise m e Void
withLayerEvent f =
    layerEvent
        |> andThen
            (\e ->
                case f e of
                    [] ->
                        withLayerEvent f

                    proms ->
                        sequence proms
            )


{-| Construct a Promise that listen to Events on a layer till the Layer expires.

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer.

    myProcedures : List (Promise Memory Event Void)
    myProcedures =
        [ newLayer myLayerPosition initValue
            |> andThen
                (\(myLayer, myPointer) ->
                    syncAll
                        [ listenLayerEvent onEveryClickAddButton
                        , Debug.todo "Main Promise"
                        ]
                )

    onEveryClickAddButton : Event -> List (Promise c m e Void)
    onEveryClickAddButton event =
        case event of
            ClickAddButton ->
                Debug.todo "Sequence of Promises"
            _ ->
                []

-}
listenLayerEvent : (e -> List (Promise m e Void)) -> Promise m e Void
listenLayerEvent =
    Core.listenLayerEvent



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
currentState : Promise m e m
currentState =
    Core.currentState


{-| -}
layerMemory : Layer m1 -> Promise m e m1
layerMemory (Core.Layer _ m) =
    succeed m


{-| Lower level Promise that awaits Layer events.

This resolves with every Event the Layer receives; for specific Layer Events, you can use `withLayerEvent` and `listenLayerEvent` helper functions.

-}
layerEvent : Promise m e e
layerEvent =
    Core.layerEvent


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
    { name : String
    , ports :
        { request : Value -> Cmd (Msg e)
        , response : (Value -> Msg e) -> Sub (Msg e)
        }
    , requestBody : Value
    }
    -> Promise m e Value
portRequest =
    Core.portRequest



-- Layer


{-| -}
newLayer : m1 -> Promise m e (Layer m1)
newLayer =
    Core.newLayer


{-| -}
isOnSameLayer : Layer m -> Layer m -> Bool
isOnSameLayer (Core.Layer lid1 _) (Core.Layer lid2 _) =
    lid1 == lid2


{-| -}
layerView : (m -> Html (Msg e)) -> Layer m -> Html (Msg e)
layerView =
    Core.layerView


{-| -}
keyedLayerView : (m -> Html (Msg e)) -> Layer m -> ( String, Html (Msg e) )
keyedLayerView =
    Core.keyedLayerView


{-| -}
layerDocument : (m -> Document (Msg e)) -> Layer m -> Document (Msg e)
layerDocument =
    Core.layerDocument


{-| -}
eventAttr : Attribute e -> Attribute (Msg e)
eventAttr =
    Core.eventAttr


{-| [elm-mixin](https://package.elm-lang.org/packages/arowM/elm-mixin/latest/) version of `eventAttr`.
-}
eventMixin : Mixin e -> Mixin (Msg e)
eventMixin =
    Core.eventMixin



-- Browser alternatives


{-| Procedure version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).
-}
application :
    ApplicationProps flags memory event
    -> Program flags memory event
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


{-| -}
type alias ApplicationProps flags memory event =
    { init : memory
    , procedure : flags -> AppUrl -> NavKey -> Promise memory event Void
    , view : Layer memory -> Document (Msg event)
    , onUrlRequest : flags -> UrlRequest -> NavKey -> Promise memory event Void
    , onUrlChange : flags -> AppUrl -> NavKey -> Promise memory event Void
    }


{-| Alternative to [Browser.Navigation.Key](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#Key).

Navigation keys are required for navigation procedures exposed by the [Tepa.Navigation](./Navigation) module.

-}
type alias NavKey =
    Core.NavKey


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags memory event =
    Platform.Program flags (Model memory event) (Msg event)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document a =
    Browser.Document a


{-| TEPA version of [Browser.UrlRequest](https://package.elm-lang.org/packages/elm/browser/latest/Browser#UrlRequest).

All links in an [`application`](#application) create a `UrlRequest`. So
when you click `<a href="/home">Home</a>`, it does not just navigate! It
notifies `onUrlRequest` that the user wants to change the URL.

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


{-| TEA update function implementation for running your Procedures.
-}
update : Msg event -> Model memory event -> ( Model memory event, Cmd (Msg event) )
update msg model =
    let
        newState =
            Core.update msg model
    in
    ( newState.nextModel
    , newState.realCmds
        |> Cmd.batch
    )


{-| Just like `Procedure.documentView`.
-}
documentView : (Layer memory -> Document (Msg event)) -> Model memory event -> Document (Msg event)
documentView =
    Core.documentView


{-| TEA subscriptions function implementation for running your Procedures.
-}
subscriptions : Model memory event -> Sub (Msg event)
subscriptions =
    Core.subscriptions


{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    ->
        { procedure : Promise memory event Void
        , onUrlChange : AppUrl -> Promise memory event Void
        , onUrlRequest : UrlRequest -> Promise memory event Void
        }
    -> ( Model memory event, Cmd (Msg event) )
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


{-| TEA Message that wraps your events.
-}
type alias Msg event =
    Core.Msg event


{-| -}
mapMsg : (event1 -> event0) -> Msg event1 -> Msg event0
mapMsg =
    Core.mapMsg


{-| TEA Model that stores your Procedure state.
-}
type alias Model m e =
    Core.Model m e


{-| TEA `onUrlChange` property value.
-}
onUrlChange : Url -> Msg event
onUrlChange url =
    AppUrl.fromUrl url
        |> Core.UrlChange


{-| TEA `onUrlRequest` property value.
-}
onUrlRequest : Browser.UrlRequest -> Msg event
onUrlRequest =
    Core.UrlRequest
