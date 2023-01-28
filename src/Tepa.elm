module Tepa exposing
    ( Promise
    , map
    , liftEvent, mapCmd, onLayer
    , Pointer
    , andRace
    , andThen
    , sync
    , Void, void
    , sequence, andThenSequence, none, cancel
    , syncAll
    , modify, push, listen, lazy
    , when
    , unless
    , withMaybe
    , sequenceOnLayer
    , succeed
    , currentState, layerEvent
    , portRequest
    , httpRequest, HttpResult, HttpRequestError(..), isGoodStatus
    , expectStringResponse, expectBytesResponse
    , customRequest
    , anyRequest
    , withLayerEvent
    , Layer, isPointedBy
    , putMaybeLayer, putVariantLayer, newListItemLayer, newLayer
    , layerView, keyedLayerView, layerDocument, eventAttr, eventMixin
    , element
    , document
    , application
    , ApplicationProps
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
    )

{-|


# Promise

@docs Promise


# Transformers

@docs map
@docs liftEvent, mapCmd, onLayer
@docs Pointer


# Composition

@docs andRace
@docs andThen
@docs sync


# Procedures

Promises that returns `Void` are called as a _Procedure_.

@docs Void, void
@docs sequence, andThenSequence, none, cancel
@docs syncAll
@docs modify, push, listen, lazy


# Helper Procedures

@docs when
@docs unless
@docs withMaybe
@docs sequenceOnLayer


# Primitive Promises

@docs succeed
@docs currentState, layerEvent
@docs portRequest
@docs httpRequest, HttpResult, HttpRequestError, isGoodStatus
@docs expectStringResponse, expectBytesResponse
@docs customRequest
@docs anyRequest


# Helper Promises

@docs withLayerEvent


# Layer

@docs Layer, isPointedBy
@docs putMaybeLayer, putVariantLayer, newListItemLayer, newLayer
@docs layerView, keyedLayerView, layerDocument, eventAttr, eventMixin


# Browser alternatives

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#connect-to-tea-app) is also available for more advanced use cases, which enables you to introduce TEPA partially into your existing TEA app.

@docs element
@docs document
@docs application
@docs ApplicationProps
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

-}

import Browser
import Bytes exposing (Bytes)
import Html exposing (Attribute, Html)
import Http
import Internal.Core as Core
    exposing
        ( Msg(..)
        , Promise(..)
        )
import Internal.RequestId exposing (RequestId)
import Internal.ResponseType as ResponseType
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Tepa.Navigation exposing (NavKey)
import Tepa.ResponseType exposing (ResponseType)
import Url exposing (Url)


{-| The Promise represents the eventual completion of an operation and its resulting value. Similar to [Promise in JS](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise).
-}
type alias Promise cmd memory event result =
    Core.Promise cmd memory event result


{-| Build a Promise that is always completed with the given value immediately.

This is usefull for building Promise for concurrent operations with `sync`.

-}
succeed : a -> Promise cmd memory event a
succeed =
    Core.succeedPromise


{-| Transform a resulting value produced by a Promise.
-}
map : (a -> b) -> Promise c m e a -> Promise c m e b
map =
    Core.mapPromise


{-| Transform the Events produced or consumed by a Procedure.
-}
liftEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Promise c m e1 a
    -> Promise c m e0 a
liftEvent =
    Core.liftPromiseEvent


{-| Transform the Commands produced by a Procedure.

You can also use this to inject actual Commands.

-}
mapCmd : (c -> cmd) -> Promise c m e a -> Promise cmd m e a
mapCmd =
    Core.mapPromiseCmd


{-| Call a child Layer Promise.
-}
onLayer : Pointer m m1 -> Promise c m1 e a -> Promise c m e a
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
type alias Pointer m m1 =
    Core.Pointer m m1



-- Composition


{-| Run another Promise concurrently to get the firtst result.

May you want to set timeout on your request:

    requestWithTimeout : Promise Command Memory Event (Result () Response)
    requestWithTimeout =
        myRequest
            |> map Ok
            |> andRace
                (sleep 10000
                    |> map Err
                )

    sleep : Promise Command Memory Event Void
    sleep =
        Debug.todo ""

-}
andRace : Promise c m e a -> Promise c m e a -> Promise c m e a
andRace =
    Core.andRacePromise


{-| Build a new Promise that evaluate two Promises sequentially.
-}
andThen : (a -> Promise c m e b) -> Promise c m e a -> Promise c m e b
andThen =
    Core.andThenPromise


{-| Run another Promise concurrently to get both results.

    type alias Response =
        { resp1 : Resp1
        , resp2 : Resp2
        }

    request1 : Promise Command Memory Event Resp1
    request1 =
        Debug.todo ""

    request2 : Promise Command Memory Event Resp2
    request2 =
        Debug.todo ""

    -- Returns `Response` value when both Promises has been completed.
    batched : Promise Command Memory Event Response
    batched =
        succeed Response
            (sync request1)
            (sync request2)

-}
sync : Promise c m e a -> Promise c m e (a -> b) -> Promise c m e b
sync =
    Core.syncPromise



-- Procedures


{-| -}
type alias Void =
    Core.Void


{-| -}
void : Promise c m e a -> Promise c m e Void
void =
    Core.void


{-| Concatenate given sequence of Procedures.
-}
sequence : List (Promise c m e Void) -> Promise c m e Void
sequence =
    Core.sequence


{-| Evaluate sequence of Procedures that depend on the result of a Promise.

    andThenSequence f =
        andThen (f >> sequence)

-}
andThenSequence : (a -> List (Promise c m e Void)) -> Promise c m e a -> Promise c m e Void
andThenSequence f =
    andThen (f >> sequence)


{-| Procedure that does nothing.
-}
none : Promise c m e Void
none =
    Core.none


{-| Cancel all the subsequent Procedures.
-}
cancel : Promise c m e Void
cancel =
    Core.cancel


{-| Run Procedures concurrently, and await all to be completed.
-}
syncAll : List (Promise c m e Void) -> Promise c m e Void
syncAll =
    Core.concurrent


{-| Construct a Promise that modifies the Memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the Memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Promise c m e Void
modify =
    Core.modify


{-| Lower level function to push Commands.

Consider using `portRequest` or `customRequest` if possible.

-}
push : (m -> c) -> Promise c m e Void
push f =
    Core.push <| \m -> [ f m ]


{-| Construct a Promise that start Subscription and listen to its Events till the Layer expires.

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer.

    myProcedures : List (Promise Command Memory Event Void)
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
    , handler : e -> List (Promise c m e Void)
    }
    -> Promise c m e Void
listen =
    Core.listen


{-| -}
lazy : (() -> Promise c m e Void) -> Promise c m e Void
lazy =
    Core.lazy



-- Helper Procedures


{-| Evaluate the sequence of Procedures only if the first argument is `True`, otherwise same as `none`.
-}
when : Bool -> List (Promise c m e Void) -> Promise c m e Void
when p ps =
    if p then
        sequence ps

    else
        none


{-| Evaluate the sequence of Procedures only if the first argument is `False`, otherwise same as `none`.
-}
unless : Bool -> List (Promise c m e Void) -> Promise c m e Void
unless p =
    when (not p)


{-| Evaluate the sequence of Procedures returned by the callback function only if the first argument is `Just`, otherwise same as `none`.
-}
withMaybe : Maybe a -> (a -> List (Promise c m e Void)) -> Promise c m e Void
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            sequence <| f a


{-| -}
sequenceOnLayer : Pointer m m1 -> List (Promise c m1 e Void) -> Promise c m e Void
sequenceOnLayer p proms =
    sequence proms
        |> onLayer p


{-| Run callback function when the Layer received an event; if the callback function returns empty List, it awaits another event again.
-}
withLayerEvent : (e -> List (Promise c m e Void)) -> Promise c m e Void
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



-- Primitive Promises


{-| Promise that requests current Memory state.

Note that this returns the Memory state when it is resolved:

    type alias Response =
        { memoryOnRequest : Memory
        , response1 : Response1
        }

    -- The `currentState` in this Promise returns the memory state when it called,
    -- i.e., when the `request1` is called, but not when the `request1` receives response.
    myPromise : Promise Command Memory Event Response
    myPromise =
        succeed Response
            (async currentState)
            (async request1)

    request1 : Promise Common Memory Event Response1
    request1 =
        Debug.todo ""

-}
currentState : Promise c m e m
currentState =
    Core.currentState


{-| Lower level Promise that awaits Layer events.

This resolves with any Event the Layer receives; for specific Layer Events, you cane use `withLayerEvent` helper function.

-}
layerEvent : Promise c m e e
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
      requestId: req.requestId,
      body: {
        name: localStorage.getItem(`Name.${req.body.userId}`),
      },
    });
  } catch {
    app.ports.receiveGetLocalName.send({
      requestId: req.id,
      body: {
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

    requestLocalName : String -> Promise Command Memory Event LocalNameResponse
    requestLocalName userId =
        portRequest
            { name = "Request for localStorage value"
            , request =
                \m { requestId } ->
                    requestGetLocalName <|
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
            , receiver = receiveGetLocalName
            , resposne =
                \requestId ->
                    JD.map2 (\rid body -> ( rid, body ))
                        (JD.field "requestId" requestId)
                        (JD.field "body"
                            (JD.map LocalNameResponse
                                (JD.field "name"
                                    (JD.nullable JD.string)
                                )
                            )
                        )
            }

-}
portRequest :
    { name : String
    , request : m -> { requestId : Value } -> c
    , receiver : (Value -> Msg e) -> Sub (Msg e)
    , response : Decoder RequestId -> Decoder ( RequestId, resp )
    }
    -> Promise c m e resp
portRequest =
    Core.portRequest


{-| -}
httpRequest :
    { name : String
    , bodyType : ResponseType body
    , request : (HttpResult body -> Msg e) -> c
    , response : Http.Metadata -> body -> a
    }
    -> Promise c m e (Result HttpRequestError a)
httpRequest o =
    customRequest
        { name = o.name
        , responseType =
            ResponseType.result
                httpRequestError
                (ResponseType.tuple
                    ResponseType.httpMetadata
                    o.bodyType
                )
        , request = o.request
        }
        |> map (Result.map (\( meta, b ) -> o.response meta b))


{-| -}
expectStringResponse : (HttpResult String -> msg) -> Http.Expect msg
expectStringResponse toMsg =
    Http.expectStringResponse toMsg toHttpResult


{-| -}
expectBytesResponse : (HttpResult Bytes -> msg) -> Http.Expect msg
expectBytesResponse toMsg =
    Http.expectBytesResponse toMsg toHttpResult


toHttpResult : Http.Response body -> HttpResult body
toHttpResult resp =
    case resp of
        Http.BadUrl_ str ->
            Err <| BadUrl str

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ meta b ->
            Ok ( meta, b )

        Http.GoodStatus_ meta b ->
            Ok ( meta, b )


{-| -}
isGoodStatus : Http.Metadata -> Bool
isGoodStatus meta =
    200 <= meta.statusCode && meta.statusCode < 300


{-| -}
type alias HttpResult body =
    Result HttpRequestError ( Http.Metadata, body )


{-| -}
type HttpRequestError
    = BadUrl String
    | Timeout
    | NetworkError


httpRequestError : ResponseType HttpRequestError
httpRequestError =
    ResponseType.variant
        { encode =
            \a ->
                case a of
                    BadUrl str ->
                        ( "BadUrl", ResponseType.encode ResponseType.string str )

                    Timeout ->
                        ( "Timeout", ResponseType.encode ResponseType.unit () )

                    NetworkError ->
                        ( "NetworkError", ResponseType.encode ResponseType.unit () )
        , decode =
            \body ->
                case body of
                    ( "BadUrl", v ) ->
                        ResponseType.decode ResponseType.string v
                            |> Maybe.map BadUrl

                    ( "Timeout", _ ) ->
                        Just Timeout

                    ( "NetworkError", _ ) ->
                        Just NetworkError

                    _ ->
                        Nothing
        }


{-| -}
customRequest :
    { name : String
    , request : (a -> Msg e) -> c
    , responseType : ResponseType a
    }
    -> Promise c m e a
customRequest =
    Core.customRequest


{-| -}
anyRequest :
    { name : String
    , request : (a -> Msg e) -> c
    , wrap : a -> e
    , unwrap : e -> Maybe a
    }
    -> Promise c m e a
anyRequest =
    Core.anyRequest



-- Layer


{-| -}
putMaybeLayer :
    { get : m -> Maybe (Layer m1)
    , set : Maybe (Layer m1) -> m -> m
    , init : m1
    }
    -> Promise c m e (Pointer m m1)
putMaybeLayer o =
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
        }
        o.init
        |> andThen
            (\( layer, pointer ) ->
                (modify <| o.set (Just layer))
                    |> map (\_ -> pointer)
            )


{-| -}
putVariantLayer :
    { get : m -> v
    , set : v -> m -> m
    , wrap : Layer m1 -> v
    , unwrap : v -> Maybe (Layer m1)
    , init : m1
    }
    -> Promise c m e (Pointer m m1)
putVariantLayer o =
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
        }
        o.init
        |> andThen
            (\( layer, pointer ) ->
                (modify <| o.set (o.wrap layer))
                    |> map (\_ -> pointer)
            )


{-| -}
newListItemLayer :
    { get : m -> List (Layer m1)
    , set : List (Layer m1) -> m -> m
    , init : m1
    }
    -> m1
    -> Promise c m e ( Layer m1, Pointer m m1 )
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
        }


{-| -}
newLayer :
    { get : (Layer m1 -> Maybe m1) -> m -> Maybe m1
    , modify : (Layer m1 -> Layer m1) -> m -> m
    }
    -> m1
    -> Promise c m e ( Layer m1, Pointer m m1 )
newLayer =
    Core.newLayer


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

You can use `mapCmd` to inject actual Cmds into your Procedure build with custom type Commands.

-}
element :
    { init : memory
    , procedure : flags -> Promise (Cmd (Msg event)) memory event Void
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

You can use `mapCmd` to inject actual Cmds into your Procedure build with custom type Commands.

-}
document :
    { init : memory
    , procedure : flags -> Promise (Cmd (Msg event)) memory event Void
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
    { props : ApplicationProps flags cmd memory event
    , runCommand : cmd -> Cmd (Msg event)
    }
    -> Program flags memory event
application { props, runCommand } =
    let
        option =
            { init = props.init
            , procedure =
                \flags url key ->
                    props.procedure flags url key
                        |> mapCmd runCommand
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
type alias ApplicationProps flags cmd memory event =
    { init : memory
    , procedure : flags -> Url -> NavKey -> Promise cmd memory event Void
    , view : Layer memory -> Document (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
    }


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags memory event =
    Platform.Program flags (Model (Cmd (Msg event)) memory event) (Msg event)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document a =
    Browser.Document a



-- Connect to TEA app


{-| TEA update function implementation for running your Procedures.
-}
update : Msg event -> Model (Cmd (Msg event)) memory event -> ( Model (Cmd (Msg event)) memory event, Cmd (Msg event) )
update msg model =
    let
        newState =
            Core.update msg model
    in
    ( newState.nextModel
    , [ List.map Tuple.second newState.cmds
      , newState.realCmds
      ]
        |> List.concat
        |> Cmd.batch
    )


{-| Construct the TEA element view function.
-}
elementView : (Layer memory -> Html (Msg event)) -> Model cmd memory event -> Html (Msg event)
elementView =
    Core.elementView


{-| Just like `Procedure.documentView`.
-}
documentView : (Layer memory -> Document (Msg event)) -> Model cmd memory event -> Document (Msg event)
documentView =
    Core.documentView


{-| TEA subscriptions function implementation for running your Procedures.
-}
subscriptions : Model cmd memory event -> Sub (Msg event)
subscriptions =
    Core.subscriptions


{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    -> Promise (Cmd (Msg event)) memory event Void
    -> ( Model (Cmd (Msg event)) memory event, Cmd (Msg event) )
init memory procs =
    let
        newState =
            Core.init memory procs
    in
    ( newState.nextModel
    , [ List.map Tuple.second newState.cmds
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
type alias Model c m e =
    Core.Model c m e


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
