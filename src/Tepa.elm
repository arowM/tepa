module Tepa exposing
    ( Promise
    , map
    , liftEvent, onLayer
    , Pointer
    , orFaster
    , andThen
    , sync
    , Void, void
    , sequence, andThenSequence, bind, none, cancel
    , syncAll
    , modify, listen, lazy
    , when
    , unless
    , withMaybe
    , sequenceOnLayer
    , succeed
    , currentState, layerEvent
    , portRequest
    , customRequest
    , anyRequest
    , withLayerEvent, listenLayerEvent
    , Layer, isPointedBy
    , layerMemory
    , maybeLayer, putMaybeLayer
    , variantLayer, putVariantLayer
    , newListItemLayer, putNewListItemLayer
    , newLayer, putNewLayer
    , layerView, keyedLayerView, layerDocument, eventAttr, eventMixin
    , element
    , document
    , application
    , ApplicationProps
    , NavKey
    , Program
    , Document
    , update
    , elementView
    , documentView
    , subscriptions
    , init
    , Msg
    , mapMsg
    , Model
    , onUrlChange
    , onUrlRequest
    , push
    )

{-|


# Promise

@docs Promise


# Transformers

@docs map
@docs liftEvent, onLayer
@docs Pointer


# Composition

@docs orFaster
@docs andThen
@docs sync


# Procedures

Promises that returns `Void` are called as a _Procedure_.

@docs Void, void
@docs sequence, andThenSequence, bind, none, cancel
@docs syncAll
@docs modify, listen, lazy


# Helper Procedures

@docs when
@docs unless
@docs withMaybe
@docs sequenceOnLayer


# Primitive Promises

@docs succeed
@docs currentState, layerEvent
@docs portRequest
@docs customRequest
@docs anyRequest


# Helper Promises

@docs withLayerEvent, listenLayerEvent


# Layer

@docs Layer, isPointedBy
@docs layerMemory
@docs maybeLayer, putMaybeLayer
@docs variantLayer, putVariantLayer
@docs newListItemLayer, putNewListItemLayer
@docs newLayer, putNewLayer
@docs layerView, keyedLayerView, layerDocument, eventAttr, eventMixin


# Browser alternatives

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#connect-to-tea-app) is also available for more advanced use cases, which enables you to introduce TEPA partially into your existing TEA app.

@docs element
@docs document
@docs application
@docs ApplicationProps
@docs NavKey
@docs Program
@docs Document


# Connect to TEA app

@docs update
@docs elementView
@docs documentView
@docs subscriptions
@docs init
@docs Msg
@docs mapMsg
@docs Model
@docs onUrlChange
@docs onUrlRequest


# Lower level functions

@docs push

-}

import Browser
import Html exposing (Attribute, Html)
import Internal.Core as Core
    exposing
        ( Msg(..)
        , Promise(..)
        )
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Tepa.ResponseType exposing (ResponseType)
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


{-| Call a child Layer Promise.
-}
onLayer : Pointer m m1 -> Promise m1 e a -> Promise m e a
onLayer =
    Core.onLayer


{-| _Layer_ is a concept that deals with a part of the application. It can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m =
    Core.Layer m


{-| -}
isPointedBy : Pointer m m1 -> Layer m1 -> Bool
isPointedBy =
    Core.isPointedBy


{-| -}
layerMemory : Layer m -> m
layerMemory (Core.Layer _ m) =
    m


{-| -}
type alias Pointer m m1 =
    Core.Pointer m m1



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
            (sync request1)
            (sync request2)

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

    [ bind
        somePromise
      <|
        \result ->
            [ yourSequence dependsOn result
            ]
    ]

-}
bind : Promise m e a -> (a -> List (Promise m e Void)) -> Promise m e Void
bind promise f =
    andThenSequence f promise


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

For detailed documentation and testing, do use this. We recommend using special functions like `Tepa.Http.Request`.

-}
push : (m -> Cmd (Msg e)) -> Promise m e Void
push =
    Core.push


{-| Construct a Promise that start Subscription and listen to its Events till the Layer expires.

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


{-| -}
sequenceOnLayer : Pointer m m1 -> List (Promise m1 e Void) -> Promise m e Void
sequenceOnLayer p proms =
    sequence proms
        |> onLayer p


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


{-| -}
customRequest :
    { name : String
    , request : (a -> Msg e) -> Cmd (Msg e)
    , responseType : ResponseType a
    }
    -> Promise m e a
customRequest =
    Core.customRequest


{-| -}
anyRequest :
    { name : String
    , request : (a -> Msg e) -> Cmd (Msg e)
    , wrap : a -> e
    , unwrap : e -> Maybe a
    }
    -> Promise m e a
anyRequest =
    Core.anyRequest



-- Layer


{-| Procedure style of `maybeLayer`.

    putMaybeLayer param =
        bind (maybeLayer param)

-}
putMaybeLayer :
    { get : m -> Maybe (Layer m1)
    , set : Maybe (Layer m1) -> m -> m
    , init : m1
    }
    -> (Pointer m m1 -> List (Promise m e Void))
    -> Promise m e Void
putMaybeLayer param =
    bind (maybeLayer param)


{-| -}
maybeLayer :
    { get : m -> Maybe (Layer m1)
    , set : Maybe (Layer m1) -> m -> m
    , init : m1
    }
    -> Promise m e (Pointer m m1)
maybeLayer o =
    newLayer
        { get =
            \getter m ->
                o.get m
                    |> Maybe.andThen getter
        , modify =
            \modifier m ->
                o.get m
                    |> Maybe.map modifier
                    |> (\ml1 -> o.set ml1 m)
        , init = o.init
        }
        |> andThen
            (\( layer, pointer ) ->
                (modify <| o.set (Just layer))
                    |> map (\_ -> pointer)
            )


{-| Procedure style of `variantLayer`.

    putVariantLayer param =
        bind (variantLayer param)

-}
putVariantLayer :
    { get : m -> v
    , set : v -> m -> m
    , wrap : Layer m1 -> v
    , unwrap : v -> Maybe (Layer m1)
    , init : m1
    }
    -> (Pointer m m1 -> List (Promise m e Void))
    -> Promise m e Void
putVariantLayer param =
    bind (variantLayer param)


{-| -}
variantLayer :
    { get : m -> v
    , set : v -> m -> m
    , wrap : Layer m1 -> v
    , unwrap : v -> Maybe (Layer m1)
    , init : m1
    }
    -> Promise m e (Pointer m m1)
variantLayer o =
    newLayer
        { get =
            \getter m ->
                o.get m
                    |> o.unwrap
                    |> Maybe.andThen getter
        , modify =
            \modifier m ->
                case o.get m |> o.unwrap of
                    Nothing ->
                        m

                    Just l1 ->
                        o.set (o.wrap <| modifier l1) m
        , init = o.init
        }
        |> andThen
            (\( layer, pointer ) ->
                (modify <| o.set (o.wrap layer))
                    |> map (\_ -> pointer)
            )


{-| Procedure style of `newListItemLayer`.

    putNewListItemLayer param init =
        bind (newListItemLayer param)

-}
putNewListItemLayer :
    { get : m -> List (Layer m1)
    , set : List (Layer m1) -> m -> m
    , init : m1
    }
    -> (( Layer m1, Pointer m m1 ) -> List (Promise m e Void))
    -> Promise m e Void
putNewListItemLayer param =
    bind (newListItemLayer param)


{-| -}
newListItemLayer :
    { get : m -> List (Layer m1)
    , set : List (Layer m1) -> m -> m
    , init : m1
    }
    -> Promise m e ( Layer m1, Pointer m m1 )
newListItemLayer o =
    newLayer
        { get =
            \getter m ->
                o.get m
                    |> List.filterMap getter
                    |> List.head
        , modify =
            \modifier m ->
                o.get m
                    |> List.map modifier
                    |> (\l1s -> o.set l1s m)
        , init = o.init
        }


{-| Procedure style of `maybeLayer`.

    putNewLayer param =
        bind (newLayer param)

-}
putNewLayer :
    { get : (Layer m1 -> Maybe m1) -> m -> Maybe m1
    , modify : (Layer m1 -> Layer m1) -> m -> m
    , init : m1
    }
    -> (( Layer m1, Pointer m m1 ) -> List (Promise m e Void))
    -> Promise m e Void
putNewLayer param =
    bind (newLayer param)


{-| -}
newLayer :
    { get : (Layer m1 -> Maybe m1) -> m -> Maybe m1
    , modify : (Layer m1 -> Layer m1) -> m -> m
    , init : m1
    }
    -> Promise m e ( Layer m1, Pointer m m1 )
newLayer param =
    Core.newLayer
        { get = param.get
        , modify = param.modify
        }
        param.init


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


{-| Procedure version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).
-}
element :
    { init : memory
    , procedure : flags -> Promise memory event Void
    , view : Layer memory -> Html (Msg event)
    }
    -> Program flags memory event
element option =
    Browser.element
        { init =
            \flags ->
                init option.init (option.procedure flags)
        , view = elementView option.view
        , update = update
        , subscriptions = subscriptions
        }


{-| Procedure version of [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).
-}
document :
    { init : memory
    , procedure : flags -> Promise memory event Void
    , view : Layer memory -> Document (Msg event)
    }
    -> Program flags memory event
document option =
    Browser.document
        { init =
            \flags ->
                init option.init (option.procedure flags)
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions
        }


{-| Procedure version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).

The `onUrlRequest` and `onUrlChange` Events are published to the application root Layer.

-}
application :
    ApplicationProps flags memory event
    -> Program flags memory event
application props =
    let
        option =
            { init = props.init
            , procedure =
                \flags url key ->
                    props.procedure flags url key
            , view = props.view
            , onUrlRequest = props.onUrlRequest
            , onUrlChange = props.onUrlChange
            }
    in
    Browser.application
        { init =
            \flags url key ->
                init option.init (option.procedure flags url (Core.RealKey key))
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest option.onUrlRequest
        , onUrlChange = onUrlChange option.onUrlChange
        }


{-| -}
type alias ApplicationProps flags memory event =
    { init : memory
    , procedure : flags -> Url -> NavKey -> Promise memory event Void
    , view : Layer memory -> Document (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
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
    , [ List.map (\(Core.Request _ _ _ c) -> c) newState.requests
      , newState.realCmds
      ]
        |> List.concat
        |> Cmd.batch
    )


{-| Construct the TEA element view function.
-}
elementView : (Layer memory -> Html (Msg event)) -> Model memory event -> Html (Msg event)
elementView =
    Core.elementView


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
    -> Promise memory event Void
    -> ( Model memory event, Cmd (Msg event) )
init memory procs =
    let
        newState =
            Core.init memory procs
    in
    ( newState.nextModel
    , [ List.map (\(Core.Request _ _ _ c) -> c) newState.requests
      , newState.realCmds
      ]
        |> List.concat
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


{-| Construct a TEA `onUrlChange` property value.
The Event you provided as an argument can be received on the root Layer.
-}
onUrlChange : (Url -> event) -> Url -> Msg event
onUrlChange f =
    Core.rootLayerMsg << f


{-| Construct a TEA `onUrlRequest` property value.
The Event you provided as an argument can be received on the root Layer.
-}
onUrlRequest : (Browser.UrlRequest -> event) -> Browser.UrlRequest -> Msg event
onUrlRequest f =
    Core.rootLayerMsg << f
