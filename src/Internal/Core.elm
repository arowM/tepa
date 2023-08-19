module Internal.Core exposing
    ( Model(..), Model_, memoryState, layerState
    , Msg(..)
    , NavKey(..)
    , Promise(..)
    , PromiseState(..)
    , succeedPromise
    , mapPromise
    , andThenPromise
    , syncPromise
    , liftPromiseMemory
    , neverResolved
    , portRequest, portStream
    , httpRequest, httpBytesRequest, HttpRequestError(..), HttpRequest
    , HttpRequestBody(..)
    , now, here
    , getValue, getValues, setValue
    , getCheck, getChecks, setCheck
    , customRequest
    , awaitCustomViewEvent, customViewEventStream
    , Layer(..), Layer_, ThisLayerId(..), mapLayer
    , ThisLayerEvents(..), ThisLayerValues(..)
    , viewArgs
    , none, sequence, concurrent
    , modify, push, currentState, lazy
    , sleep, listenTimeEvery, tick, listenMsg
    , load, reload
    , RandomValue(..), RandomRequest(..), RandomSpec(..), isRequestForSpec
    , Stream(..)
    , onGoingProcedure
    , newLayer, onLayer, LayerResult(..)
    , init, update, NewState, Log(..)
    , documentView, subscriptions
    )

{-|


# Core

@docs Model, Model_, memoryState, layerState
@docs Msg


# NavKey

@docs NavKey


# Promise

@docs Promise
@docs PromiseState
@docs succeedPromise
@docs mapPromise
@docs andThenPromise
@docs syncPromise
@docs liftPromiseMemory
@docs neverResolved
@docs portRequest, portStream
@docs httpRequest, httpBytesRequest, HttpRequestError, HttpRequest
@docs HttpRequestBody
@docs now, here
@docs getValue, getValues, setValue
@docs getCheck, getChecks, setCheck
@docs customRequest
@docs awaitCustomViewEvent, customViewEventStream
@docs Layer, Layer_, ThisLayerId, mapLayer
@docs ThisLayerEvents, ThisLayerValues
@docs viewArgs
@docs none, sequence, concurrent


# Primitive Procedures

@docs modify, push, currentState, lazy
@docs sleep, listenTimeEvery, tick, listenMsg
@docs load, reload


# Random

@docs RandomValue, RandomRequest, RandomSpec, isRequestForSpec


# Stream

@docs Stream


# Helper Procedures

@docs onGoingProcedure


# Layer

@docs newLayer, onLayer, LayerResult


# TEA

@docs init, update, NewState, Log
@docs documentView, subscriptions

-}

import AppUrl exposing (AppUrl)
import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Html.Events
import Http
import Internal.LayerId as LayerId exposing (LayerId)
import Internal.RequestId as RequestId exposing (RequestId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Process
import Task
import Time exposing (Posix)



-- Model


{-| -}
type Model memory
    = Model (Model_ memory)


{-| -}
type alias Model_ m =
    -- New context after the evaluation.
    { context : Context m

    -- New state to evaluate next time.
    , next : Msg -> Context m -> NewState m
    }


endOfModel : Context m -> Model m
endOfModel context =
    Model
        { context = context
        , next = \_ -> endOfNewState
        }


{-| Execution time context for Procedures
-}
type alias Context m =
    { layer : Layer_ m
    , nextRequestId : RequestId
    , nextLayerId : LayerId
    , subs : List (m -> Maybe ( RequestId, Sub Msg ))
    }


{-| -}
type ThisLayerEvents m
    = ThisLayerEvents
        (Dict
            String
            -- key
            (Dict
                String
                -- event type
                (Decoder
                    { stopPropagation : Bool
                    , preventDefault : Bool
                    }
                )
            )
        )


unwrapThisLayerEvents :
    ThisLayerEvents m
    ->
        Dict
            String
            (Dict
                String
                -- event type
                (Decoder
                    { stopPropagation : Bool
                    , preventDefault : Bool
                    }
                )
            )
unwrapThisLayerEvents (ThisLayerEvents dict) =
    dict


{-| -}
type ThisLayerValues m
    = ThisLayerValues
        (Dict
            String
            -- key
            String
         -- changed value for the form control
        )


unwrapThisLayerValues : ThisLayerValues m -> Dict String String
unwrapThisLayerValues (ThisLayerValues dict) =
    dict


{-| -}
type ThisLayerChecks m
    = ThisLayerChecks
        (Dict
            String
            -- key
            Bool
         -- `checked` property value for the form control
        )


unwrapThisLayerChecks : ThisLayerChecks m -> Dict String Bool
unwrapThisLayerChecks (ThisLayerChecks dict) =
    dict


{-| New application state after some operations.
-}
type alias NewState m =
    { nextModel : Model m
    , realCmds : List (Cmd Msg) -- reversed
    , logs : List Log
    }


endOfNewState : Context m -> NewState m
endOfNewState context =
    { nextModel =
        Model
            { context = context
            , next = \_ -> endOfNewState
            }
    , realCmds = []
    , logs = []
    }


{-| Current memory state.
-}
memoryState : Model memory -> memory
memoryState (Model model) =
    model.context.layer.state


{-| Current Layer state.
-}
layerState : Model memory -> Layer memory
layerState (Model model) =
    Layer model.context.layer


{-| Application operation logs.
-}
type Log
    = SetTimer RequestId LayerId Int
    | StartTimeEvery RequestId LayerId Int
    | RequestCurrentTime RequestId
    | RequestCurrentZone RequestId
    | IssueHttpRequest RequestId LayerId HttpRequest
    | HandshakePortStream RequestId LayerId Value
    | IssueRandomRequest RequestId LayerId RandomRequest
    | ResolvePortRequest RequestId
    | ResolveHttpRequest RequestId
    | ResolveRandomRequest RequestId
    | LayerHasExpired LayerId
    | PushPath AppUrl
    | ReplacePath AppUrl
    | Back Int
    | Forward Int
    | LoadUrl
    | Reload
    | FocusNode RequestId String
    | BlurNode RequestId String
    | RequestViewport RequestId
    | RequestViewportOf RequestId String
    | SetViewport RequestId
    | SetViewportOf RequestId String
    | RequestElement RequestId String


{-| -}
type alias HttpRequest =
    { method : String
    , headers : List ( String, String )
    , url : String
    , requestBody : HttpRequestBody
    , timeout : Maybe Int
    , tracker : Maybe String
    }


{-| -}
type RandomSpec a
    = RandomSpecInt
        { id : String
        , min : Int
        , max : Int
        , unwrap : RandomValue -> Maybe a
        , wrap : a -> Result String RandomValue
        }
    | RandomSpecFloat
        { id : String
        , min : Float
        , max : Float
        , unwrap : RandomValue -> Maybe a
        , wrap : a -> Result String RandomValue
        }
    | RandomSpecEnum
        { id : String
        , candidates : List ( Float, a )
        , unwrap : RandomValue -> Maybe a
        , wrap : a -> Result String RandomValue
        }



-- Msg


{-| -}
type Msg
    = PortResponseMsg
        { requestId : RequestId
        , response : Value
        }
    | HttpResponseMsg
        { requestId : RequestId
        , response : Result HttpRequestError ( Http.Metadata, String )
        }
    | HttpBytesResponseMsg
        { requestId : RequestId
        , response : Result HttpRequestError ( Http.Metadata, Bytes )
        }
    | RandomResponseMsg
        { requestId : RequestId
        , response : RandomValue
        }
    | FocusMsg
        { requestId : RequestId
        , targetId : String
        , response : Result Dom.Error ()
        }
    | BlurMsg
        { requestId : RequestId
        , targetId : String
        , response : Result Dom.Error ()
        }
    | RequestViewportMsg
        { requestId : RequestId
        , response : Dom.Viewport
        }
    | RequestViewportOfMsg
        { requestId : RequestId
        , targetId : String
        , response : Result Dom.Error Dom.Viewport
        }
    | RequestSetViewportMsg
        { requestId : RequestId
        }
    | RequestSetViewportOfMsg
        { requestId : RequestId
        , targetId : String
        , response : Result Dom.Error ()
        }
    | RequestElementMsg
        { requestId : RequestId
        , targetId : String
        , response : Result Dom.Error Dom.Element
        }
    | CurrentTimeMsg
        { requestId : RequestId
        , timestamp : Posix
        }
    | CurrentZoneMsg
        { requestId : RequestId
        , zone : Time.Zone
        }
    | ViewMsg
        { layerId : LayerId
        , key : String
        , type_ : String
        , value : Value
        , decoder :
            Decoder
                { stopPropagation : Bool
                , preventDefault : Bool
                }
        }
    | WakeUpMsg
        { requestId : RequestId
        }
    | IntervalMsg
        { requestId : RequestId
        , timestamp : Posix
        }
    | UrlChange AppUrl
    | UrlRequest Browser.UrlRequest
    | NoOp


{-| -}
type RandomValue
    = RandomInt Int
    | RandomFloat Float
    | RandomEnum Int


{-| -}
type RandomRequest
    = RequestRandomInt String
    | RequestRandomFloat String
    | RequestRandomEnum String


{-| -}
isRequestForSpec : RandomSpec a -> RandomRequest -> Bool
isRequestForSpec spec req =
    case ( spec, req ) of
        ( RandomSpecInt param, RequestRandomInt id ) ->
            param.id == id

        ( RandomSpecFloat param, RequestRandomFloat id ) ->
            param.id == id

        ( RandomSpecEnum param, RequestRandomEnum id ) ->
            param.id == id

        _ ->
            False



-- Stream


{-| -}
type Stream a
    = ActiveStream
        { unwrapMsg : Msg -> ( List a, Stream a )
        }
    | EndOfStream



-- NavKey


{-| -}
type NavKey
    = RealKey Nav.Key
    | SimKey



-- Promise


{-| The Promise represents the eventual completion of an operation and its resulting value.
-}
type Promise m a
    = Promise (Context m -> PromiseEffect m a)


{-| Effects by evaluating a Promise.
-}
type alias PromiseEffect m a =
    { newContext : Context m
    , realCmds : List (Cmd Msg) -- reversed
    , logs : List Log
    , state : PromiseState m a
    }


{-| Represents current Promise state.
-}
type PromiseState m a
    = Resolved a
    | AwaitMsg (Msg -> m -> Promise m a)


{-| -}
mapPromise : (a -> b) -> Promise m a -> Promise m b
mapPromise f (Promise prom) =
    Promise <|
        \context ->
            let
                effA =
                    prom context
            in
            { newContext = effA.newContext
            , realCmds = effA.realCmds
            , logs = effA.logs
            , state =
                case effA.state of
                    Resolved a ->
                        Resolved <| f a

                    AwaitMsg next ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise f (next msg m)
            }


primitivePromise : PromiseState m a -> Promise m a
primitivePromise state =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state = state
            }


setLogs : List Log -> Promise m a -> Promise m a
setLogs logs (Promise f) =
    Promise <|
        \context ->
            let
                eff =
                    f context
            in
            { eff | logs = logs }


{-| Promise that just resolves to `a`.
-}
succeedPromise : a -> Promise m a
succeedPromise a =
    primitivePromise <| Resolved a


{-| Promise that just await next Msg.
-}
justAwaitPromise : (Msg -> m -> Promise m a) -> Promise m a
justAwaitPromise f =
    primitivePromise <| AwaitMsg f


{-| Await forever
-}
neverResolved : Promise m a
neverResolved =
    Promise <|
        \context ->
            let
                next : Msg -> m -> Promise m a
                next _ _ =
                    justAwaitPromise next
            in
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg next
            }


{-| Await both Promises to be completed.
-}
syncPromise : Promise m a -> Promise m (a -> b) -> Promise m b
syncPromise (Promise promA) (Promise promF) =
    Promise <|
        \context ->
            let
                effF =
                    promF context

                effA =
                    promA effF.newContext
            in
            { newContext = effA.newContext
            , realCmds = effF.realCmds ++ effA.realCmds
            , logs = effF.logs ++ effA.logs
            , state =
                case ( effF.state, effA.state ) of
                    ( Resolved f, Resolved a ) ->
                        Resolved <| f a

                    ( Resolved f, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise f (nextPromA msg m)

                    ( AwaitMsg nextPromF, Resolved a ) ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise (\f -> f a) (nextPromF msg m)

                    ( AwaitMsg nextPromF, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                syncPromise (nextPromA msg m) (nextPromF msg m)
            }


{-| Await one of the Promises to be completed.
-}
andRacePromise : Promise m a -> Promise m a -> Promise m a
andRacePromise (Promise prom2) (Promise prom1) =
    Promise <|
        \context ->
            let
                eff1 =
                    prom1 context

                eff2 =
                    prom2 eff1.newContext
            in
            { newContext = eff2.newContext
            , realCmds = eff1.realCmds ++ eff2.realCmds
            , logs = eff1.logs ++ eff2.logs
            , state =
                case ( eff1.state, eff2.state ) of
                    ( Resolved a1, _ ) ->
                        Resolved a1

                    ( _, Resolved a2 ) ->
                        Resolved a2

                    ( AwaitMsg nextProm1, AwaitMsg nextProm2 ) ->
                        AwaitMsg <|
                            \msg m ->
                                andRacePromise
                                    (nextProm2 msg m)
                                    (nextProm1 msg m)
            }


{-| Run Promises sequentially.
-}
andThenPromise : (a -> Promise m b) -> Promise m a -> Promise m b
andThenPromise f (Promise promA) =
    Promise <|
        \context ->
            let
                effA =
                    promA context
            in
            case effA.state of
                Resolved a ->
                    let
                        (Promise promB) =
                            f a

                        effB =
                            promB effA.newContext
                    in
                    { newContext = effB.newContext
                    , realCmds = effA.realCmds ++ effB.realCmds
                    , logs = effA.logs ++ effB.logs
                    , state = effB.state
                    }

                AwaitMsg promNextA ->
                    { newContext = effA.newContext
                    , realCmds = effA.realCmds
                    , logs = effA.logs
                    , state =
                        AwaitMsg <|
                            \msg m ->
                                promNextA msg m
                                    |> andThenPromise f
                    }


{-| -}
type LayerResult a
    = LayerOk a
    | LayerNotExists
    | LayerExpired


{-| -}
onLayer :
    { get : m -> Maybe (Layer m1)
    , set : Layer m1 -> m -> m
    }
    -> Promise m1 a
    -> Promise m (LayerResult a)
onLayer param prom1 =
    bindAndThenPromise currentState <|
        \state ->
            case param.get state of
                Nothing ->
                    succeedPromise LayerNotExists

                Just (Layer layer) ->
                    onLayer_
                        { get = param.get
                        , set = param.set
                        , layerId = layer.id
                        }
                        prom1
                        |> andRacePromise
                            (monitorChange
                                { get = param.get
                                , set = param.set
                                , layerId = layer.id
                                }
                            )


monitorChange :
    { get : m -> Maybe (Layer m1)
    , set : Layer m1 -> m -> m
    , layerId : ThisLayerId m1
    }
    -> Promise m (LayerResult a)
monitorChange param =
    Promise <|
        \context ->
            let
                state : Msg -> m -> Promise m (LayerResult a)
                state msg _ =
                    case msg of
                        ViewMsg r ->
                            if ThisLayerId r.layerId == param.layerId && r.type_ == "change" then
                                Promise <|
                                    \context_ ->
                                        let
                                            layer_ =
                                                context_.layer
                                        in
                                        case param.get layer_.state of
                                            Nothing ->
                                                { newContext = context_
                                                , realCmds = []
                                                , logs = []
                                                , state = Resolved LayerExpired
                                                }

                                            Just (Layer l1) ->
                                                if l1.id /= param.layerId then
                                                    { newContext = context_
                                                    , realCmds = []
                                                    , logs = []
                                                    , state = Resolved LayerExpired
                                                    }

                                                else
                                                    { newContext =
                                                        { context_
                                                            | layer =
                                                                { layer_
                                                                    | state =
                                                                        param.set
                                                                            ({ l1
                                                                                | values =
                                                                                    unwrapThisLayerValues l1.values
                                                                                        |> (case JD.decodeValue Html.Events.targetValue r.value of
                                                                                                Err _ ->
                                                                                                    identity

                                                                                                Ok value ->
                                                                                                    Dict.insert r.key value
                                                                                           )
                                                                                        |> ThisLayerValues
                                                                                , checks =
                                                                                    unwrapThisLayerChecks l1.checks
                                                                                        |> (case JD.decodeValue checkPropertyDecoder r.value of
                                                                                                Err _ ->
                                                                                                    identity

                                                                                                Ok checks ->
                                                                                                    Dict.insert r.key checks
                                                                                           )
                                                                                        |> ThisLayerChecks
                                                                             }
                                                                                |> Layer
                                                                            )
                                                                            layer_.state
                                                                }
                                                        }
                                                    , realCmds = []
                                                    , logs = []
                                                    , state = AwaitMsg state
                                                    }

                            else
                                justAwaitPromise state

                        _ ->
                            justAwaitPromise state
            in
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg state
            }


bindAndThenPromise : Promise m a -> (a -> Promise m b) -> Promise m b
bindAndThenPromise prom f =
    andThenPromise f prom


onLayer_ :
    { get : m -> Maybe (Layer m1)
    , set : Layer m1 -> m -> m
    , layerId : ThisLayerId m1
    }
    -> Promise m1 a
    -> Promise m (LayerResult a)
onLayer_ o (Promise prom1) =
    let
        thisLayerId =
            unwrapThisLayerId o.layerId

        awaitMsg : (Msg -> m1 -> Promise m1 a) -> Msg -> m -> Promise m (LayerResult a)
        awaitMsg nextProm msg m =
            case o.get m of
                Nothing ->
                    Promise layerExpired

                Just (Layer nextL1) ->
                    if nextL1.id /= o.layerId then
                        Promise layerExpired

                    else
                        onLayer_ o (nextProm msg nextL1.state)

        layerExpired ctx =
            { newContext = ctx
            , realCmds = []
            , logs =
                [ LayerHasExpired thisLayerId
                ]
            , state = Resolved LayerExpired
            }
    in
    Promise <|
        \context ->
            let
                withTargetLayer action =
                    case o.get context.layer.state of
                        Nothing ->
                            layerExpired context

                        Just (Layer layer1) ->
                            if layer1.id /= o.layerId then
                                layerExpired context

                            else
                                action layer1
            in
            withTargetLayer <|
                \layer1 ->
                    let
                        eff1 =
                            prom1
                                { layer = layer1
                                , nextRequestId = context.nextRequestId
                                , nextLayerId = context.nextLayerId
                                , subs = []
                                }
                    in
                    { newContext =
                        { layer =
                            { state = o.set (Layer eff1.newContext.layer) context.layer.state
                            , id = context.layer.id
                            , events = context.layer.events
                            , values = context.layer.values
                            , checks = context.layer.checks
                            }
                        , nextRequestId = eff1.newContext.nextRequestId
                        , nextLayerId = eff1.newContext.nextLayerId
                        , subs =
                            context.subs
                                ++ List.map
                                    (\f m ->
                                        o.get m
                                            |> Maybe.andThen
                                                (\(Layer l1) ->
                                                    if l1.id /= o.layerId then
                                                        Nothing

                                                    else
                                                        Just l1.state
                                                )
                                            |> Maybe.andThen f
                                    )
                                    eff1.newContext.subs
                        }
                    , realCmds = eff1.realCmds
                    , logs = eff1.logs
                    , state =
                        case eff1.state of
                            Resolved a ->
                                Resolved (LayerOk a)

                            AwaitMsg nextProm ->
                                AwaitMsg <| awaitMsg nextProm
                    }


{-| -}
liftPromiseMemory :
    { get : m -> m1
    , set : m1 -> m -> m
    }
    -> Promise m1 a
    -> Promise m a
liftPromiseMemory o (Promise prom1) =
    Promise <|
        \context ->
            let
                state1 =
                    o.get context.layer.state

                eff1 =
                    prom1
                        { layer =
                            { state = state1
                            , id =
                                unwrapThisLayerId context.layer.id
                                    |> ThisLayerId
                            , events =
                                unwrapThisLayerEvents context.layer.events
                                    |> ThisLayerEvents
                            , values =
                                unwrapThisLayerValues context.layer.values
                                    |> ThisLayerValues
                            , checks =
                                unwrapThisLayerChecks context.layer.checks
                                    |> ThisLayerChecks
                            }
                        , nextRequestId = context.nextRequestId
                        , nextLayerId = context.nextLayerId
                        , subs = []
                        }
            in
            { newContext =
                { layer =
                    { state = o.set eff1.newContext.layer.state context.layer.state
                    , id = context.layer.id
                    , events =
                        unwrapThisLayerEvents eff1.newContext.layer.events
                            |> ThisLayerEvents
                    , values =
                        unwrapThisLayerValues eff1.newContext.layer.values
                            |> ThisLayerValues
                    , checks =
                        unwrapThisLayerChecks eff1.newContext.layer.checks
                            |> ThisLayerChecks
                    }
                , nextRequestId = eff1.newContext.nextRequestId
                , nextLayerId = eff1.newContext.nextLayerId
                , subs =
                    context.subs
                        ++ List.map
                            (\f m ->
                                o.get m |> f
                            )
                            eff1.newContext.subs
                }
            , realCmds = eff1.realCmds
            , logs = eff1.logs
            , state =
                case eff1.state of
                    Resolved a ->
                        Resolved a

                    AwaitMsg nextProm ->
                        AwaitMsg <|
                            \msg m ->
                                liftPromiseMemory o (nextProm msg <| o.get m)
            }



-- Primitive Promises


{-| -}
none : Promise m ()
none =
    succeedPromise ()


{-| -}
sequence : List (Promise m ()) -> Promise m ()
sequence =
    List.foldl
        (\a acc ->
            acc
                |> andThenPromise
                    (\_ ->
                        a
                    )
        )
        none


{-| -}
concurrent : List (Promise m ()) -> Promise m ()
concurrent =
    List.foldl
        (\a acc ->
            succeedPromise (\_ _ -> ())
                |> syncPromise acc
                |> syncPromise a
        )
        none


{-| -}
currentState : Promise m m
currentState =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state = Resolved context.layer.state
            }


{-| -}
genNewLayerId : Promise m LayerId
genNewLayerId =
    Promise <|
        \context ->
            let
                newLayerId =
                    context.nextLayerId

                newContext =
                    { context | nextLayerId = LayerId.inc newLayerId }
            in
            { newContext = newContext
            , realCmds = []
            , logs = []
            , state = Resolved newLayerId
            }


{-| -}
onGoingProcedure : (PromiseEffect m () -> PromiseEffect m ()) -> Promise m ()
onGoingProcedure f =
    Promise <|
        \context ->
            f
                { newContext = context
                , realCmds = []
                , logs = []
                , state = Resolved ()
                }


{-| -}
modify : (m -> m) -> Promise m ()
modify f =
    onGoingProcedure <|
        \eff ->
            { eff
                | newContext =
                    let
                        context =
                            eff.newContext

                        layer =
                            context.layer
                    in
                    { context
                        | layer =
                            { layer | state = f layer.state }
                    }
            }


{-| -}
push : (m -> Cmd Msg) -> Promise m ()
push f =
    onGoingProcedure <|
        \eff ->
            { eff | realCmds = f eff.newContext.layer.state :: eff.realCmds }


{-| -}
lazy : (() -> Promise m ()) -> Promise m ()
lazy f =
    Promise <|
        \context ->
            let
                (Promise prom) =
                    f ()
            in
            prom context


{-| -}
type Layer m
    = Layer (Layer_ m)


{-| -}
type alias Layer_ m =
    { id : ThisLayerId m
    , state : m
    , events : ThisLayerEvents m
    , values : ThisLayerValues m
    , checks : ThisLayerChecks m
    }


{-| -}
mapLayer :
    (m1 -> m2)
    -> (Layer m -> Maybe (Layer m1))
    -> (Layer m -> Maybe (Layer m2))
mapLayer f parent =
    \l ->
        parent l
            |> Maybe.map
                (\(Layer layer1) ->
                    Layer
                        { id =
                            unwrapThisLayerId layer1.id
                                |> ThisLayerId
                        , state = f layer1.state
                        , events =
                            unwrapThisLayerEvents layer1.events
                                |> ThisLayerEvents
                        , values =
                            unwrapThisLayerValues layer1.values
                                |> ThisLayerValues
                        , checks =
                            unwrapThisLayerChecks layer1.checks
                                |> ThisLayerChecks
                        }
                )


{-| -}
newLayer : m1 -> Promise m (Layer m1)
newLayer m1 =
    genNewLayerId
        |> mapPromise
            (\layerId ->
                Layer
                    { id = ThisLayerId layerId
                    , state = m1
                    , events = ThisLayerEvents Dict.empty
                    , values = ThisLayerValues Dict.empty
                    , checks = ThisLayerChecks Dict.empty
                    }
            )


{-| -}
viewArgs :
    Layer m
    ->
        { state : m
        , setKey : String -> List (Attribute Msg)
        , values : Dict String String
        , checks : Dict String Bool
        , layerId : String
        }
viewArgs (Layer layer) =
    { state = layer.state
    , setKey =
        \key ->
            let
                events =
                    unwrapThisLayerEvents layer.events
                        |> Dict.get key
                        |> Maybe.withDefault Dict.empty
                        |> Dict.update "change"
                            (\ma ->
                                case ma of
                                    Just a ->
                                        Just a

                                    Nothing ->
                                        Just <|
                                            JD.succeed
                                                { stopPropagation = False
                                                , preventDefault = False
                                                }
                            )
            in
            Dict.toList events
                |> List.map
                    (\( type_, decoder ) ->
                        Html.Events.custom type_ <|
                            JD.map2
                                (\v { stopPropagation, preventDefault } ->
                                    { message =
                                        ViewMsg
                                            { layerId = unwrapThisLayerId layer.id
                                            , key = key
                                            , type_ = type_
                                            , value = v
                                            , decoder = decoder
                                            }
                                    , stopPropagation = stopPropagation
                                    , preventDefault = preventDefault
                                    }
                                )
                                JD.value
                                decoder
                    )
                |> List.append
                    (unwrapThisLayerValues layer.values
                        |> Dict.get key
                        |> (\mstr ->
                                case mstr of
                                    Nothing ->
                                        []

                                    Just str ->
                                        [ Attributes.property "value" <| JE.string str
                                        ]
                           )
                    )
                |> List.append
                    (unwrapThisLayerChecks layer.checks
                        |> Dict.get key
                        |> (\mp ->
                                case mp of
                                    Nothing ->
                                        []

                                    Just p ->
                                        [ Attributes.property "checked" <| JE.bool p
                                        ]
                           )
                    )
    , values = unwrapThisLayerValues layer.values
    , checks = unwrapThisLayerChecks layer.checks
    , layerId =
        unwrapThisLayerId layer.id
            |> LayerId.toString
    }


{-| -}
type ThisLayerId m
    = ThisLayerId LayerId


unwrapThisLayerId : ThisLayerId m -> LayerId
unwrapThisLayerId (ThisLayerId lid) =
    lid


{-| -}
portStream :
    { ports :
        { request : Value -> Cmd Msg
        , response : (Value -> Msg) -> Sub Msg
        }
    , requestBody : Value
    }
    -> Promise m (Stream Value)
portStream o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId context.layer.id

                responseDecoder : Decoder ( RequestId, Value )
                responseDecoder =
                    JD.map2 Tuple.pair
                        (JD.field "id" RequestId.decoder)
                        (JD.field "body" JD.value)

                nextStream : Msg -> ( List Value, Stream Value )
                nextStream msg =
                    case msg of
                        PortResponseMsg streamMsg ->
                            if streamMsg.requestId == myRequestId then
                                ( [ streamMsg.response ]
                                , ActiveStream { unwrapMsg = nextStream }
                                )

                            else
                                ( []
                                , ActiveStream { unwrapMsg = nextStream }
                                )

                        _ ->
                            ( []
                            , ActiveStream { unwrapMsg = nextStream }
                            )
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                    , subs =
                        (\_ ->
                            Just
                                ( myRequestId
                                , o.ports.response
                                    (\rawResponse ->
                                        case JD.decodeValue responseDecoder rawResponse of
                                            Err _ ->
                                                NoOp

                                            Ok ( requestId, body ) ->
                                                if requestId == myRequestId then
                                                    PortResponseMsg
                                                        { requestId = myRequestId
                                                        , response = body
                                                        }

                                                else
                                                    NoOp
                                    )
                                )
                        )
                            :: context.subs
                }
            , realCmds =
                [ o.ports.request <|
                    JE.object
                        [ ( "id", RequestId.toValue myRequestId )
                        , ( "body", o.requestBody )
                        ]
                ]
            , logs =
                [ HandshakePortStream myRequestId thisLayerId o.requestBody
                ]
            , state =
                Resolved <|
                    ActiveStream
                        { unwrapMsg = nextStream
                        }
            }


{-| -}
listenMsg :
    (Msg -> List (Promise m ()))
    -> Promise m ()
listenMsg handler =
    Promise <|
        \context ->
            let
                awaitForever : Msg -> m -> Promise m ()
                awaitForever msg _ =
                    concurrent
                        [ justAwaitPromise awaitForever
                        , handler msg
                            |> sequence
                        ]
            in
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg awaitForever
            }


{-| -}
sleep : Int -> Promise m ()
sleep msec =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId context.layer.id

                nextPromise : Msg -> m -> Promise m ()
                nextPromise msg _ =
                    case msg of
                        WakeUpMsg wakeUpMsg ->
                            if wakeUpMsg.requestId == myRequestId then
                                succeedPromise ()

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                }
            , realCmds =
                [ Process.sleep (toFloat msec)
                    |> Task.perform
                        (\() -> WakeUpMsg { requestId = myRequestId })
                ]
            , logs =
                [ SetTimer myRequestId thisLayerId msec
                ]
            , state = AwaitMsg nextPromise
            }


{-| -}
load : String -> Promise m ()
load url =
    Promise <|
        \context ->
            { newContext = context
            , realCmds =
                [ Nav.load url
                ]
            , logs =
                [ LoadUrl
                ]
            , state = Resolved ()
            }


{-| -}
reload : Bool -> Promise m ()
reload force =
    Promise <|
        \context ->
            { newContext = context
            , realCmds =
                [ if force then
                    Nav.reloadAndSkipCache

                  else
                    Nav.reload
                ]
            , logs =
                [ Reload
                ]
            , state = Resolved ()
            }


{-| -}
listenTimeEvery :
    Int
    -> (Posix -> List (Promise m ()))
    -> Promise m ()
listenTimeEvery interval handler =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                newContext =
                    { context
                        | nextRequestId = RequestId.inc context.nextRequestId
                        , subs =
                            (\_ ->
                                Just
                                    ( myRequestId
                                    , Time.every (toFloat interval) toIntervalMsg
                                    )
                            )
                                :: context.subs
                    }

                toIntervalMsg timestamp =
                    IntervalMsg
                        { requestId = myRequestId
                        , timestamp = timestamp
                        }

                awaitForever : Msg -> m -> Promise m ()
                awaitForever msg _ =
                    case msg of
                        IntervalMsg param ->
                            if param.requestId == myRequestId then
                                concurrent
                                    [ justAwaitPromise awaitForever
                                    , handler param.timestamp
                                        |> sequence
                                    ]

                            else
                                justAwaitPromise awaitForever

                        _ ->
                            justAwaitPromise awaitForever
            in
            { newContext =
                newContext
            , realCmds = []
            , logs =
                [ StartTimeEvery myRequestId thisLayerId interval
                ]
            , state = AwaitMsg awaitForever
            }


{-| -}
tick : Int -> Promise m (Stream Posix)
tick interval =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                newContext =
                    { context
                        | nextRequestId = RequestId.inc context.nextRequestId
                        , subs =
                            (\_ ->
                                Just
                                    ( myRequestId
                                    , Time.every (toFloat interval) toIntervalMsg
                                    )
                            )
                                :: context.subs
                    }

                toIntervalMsg timestamp =
                    IntervalMsg
                        { requestId = myRequestId
                        , timestamp = timestamp
                        }

                nextStream : Msg -> ( List Posix, Stream Posix )
                nextStream msg =
                    case msg of
                        IntervalMsg param ->
                            if param.requestId == myRequestId then
                                ( [ param.timestamp ]
                                , ActiveStream { unwrapMsg = nextStream }
                                )

                            else
                                ( []
                                , ActiveStream { unwrapMsg = nextStream }
                                )

                        _ ->
                            ( []
                            , ActiveStream { unwrapMsg = nextStream }
                            )
            in
            { newContext =
                newContext
            , realCmds = []
            , logs =
                [ StartTimeEvery myRequestId thisLayerId interval
                ]
            , state =
                Resolved <|
                    ActiveStream { unwrapMsg = nextStream }
            }


{-| -}
type HttpRequestError
    = BadUrl String
    | Timeout
    | NetworkError


{-| -}
type HttpRequestBody
    = EmptyHttpRequestBody
    | StringHttpRequestBody String String
    | JsonHttpRequestBody Value
    | FileHttpRequestBody File
    | BytesHttpRequestBody String Bytes


toHttpBody : HttpRequestBody -> Http.Body
toHttpBody body =
    case body of
        EmptyHttpRequestBody ->
            Http.emptyBody

        StringHttpRequestBody mime val ->
            Http.stringBody mime val

        JsonHttpRequestBody v ->
            Http.jsonBody v

        FileHttpRequestBody file ->
            Http.fileBody file

        BytesHttpRequestBody mime val ->
            Http.bytesBody mime val


{-| -}
httpRequest : HttpRequest -> Promise m (Result HttpRequestError ( Http.Metadata, String ))
httpRequest request =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                nextPromise : Msg -> m -> Promise m (Result HttpRequestError ( Http.Metadata, String ))
                nextPromise msg _ =
                    case msg of
                        HttpResponseMsg param ->
                            if param.requestId == myRequestId then
                                succeedPromise param.response
                                    |> setLogs
                                        [ ResolveHttpRequest myRequestId
                                        ]

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                }
            , realCmds =
                [ Http.request
                    { method = request.method
                    , headers =
                        List.map
                            (\( label, value ) -> Http.header label value)
                            request.headers
                    , url = request.url
                    , body =
                        toHttpBody request.requestBody
                    , expect =
                        Http.expectStringResponse
                            (\resp ->
                                HttpResponseMsg
                                    { requestId = myRequestId
                                    , response = resp
                                    }
                            )
                            processRawHttpResponse
                    , timeout = Maybe.map toFloat request.timeout
                    , tracker = request.tracker
                    }
                ]
            , logs =
                [ IssueHttpRequest myRequestId thisLayerId request
                ]
            , state = AwaitMsg nextPromise
            }


{-| -}
httpBytesRequest : HttpRequest -> Promise m (Result HttpRequestError ( Http.Metadata, Bytes ))
httpBytesRequest request =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                nextPromise : Msg -> m -> Promise m (Result HttpRequestError ( Http.Metadata, Bytes ))
                nextPromise msg _ =
                    case msg of
                        HttpBytesResponseMsg param ->
                            if param.requestId == myRequestId then
                                succeedPromise param.response
                                    |> setLogs
                                        [ ResolveHttpRequest myRequestId
                                        ]

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                }
            , realCmds =
                [ Http.request
                    { method = request.method
                    , headers =
                        List.map
                            (\( label, value ) -> Http.header label value)
                            request.headers
                    , url = request.url
                    , body =
                        toHttpBody request.requestBody
                    , expect =
                        Http.expectBytesResponse
                            (\resp ->
                                HttpBytesResponseMsg
                                    { requestId = myRequestId
                                    , response = resp
                                    }
                            )
                            processRawHttpResponse
                    , timeout = Maybe.map toFloat request.timeout
                    , tracker = request.tracker
                    }
                ]
            , logs =
                [ IssueHttpRequest myRequestId thisLayerId request
                ]
            , state = AwaitMsg nextPromise
            }


processRawHttpResponse : Http.Response a -> Result HttpRequestError ( Http.Metadata, a )
processRawHttpResponse resp =
    case resp of
        Http.BadUrl_ str ->
            Err <| BadUrl str

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ meta body ->
            Ok ( meta, body )

        Http.GoodStatus_ meta body ->
            Ok ( meta, body )


{-| -}
portRequest :
    { ports :
        { request : Value -> Cmd Msg
        , response : (Value -> Msg) -> Sub Msg
        }
    , requestBody : Value
    }
    -> Promise m Value
portRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                responseDecoder : Decoder ( RequestId, Value )
                responseDecoder =
                    JD.map2 Tuple.pair
                        (JD.field "id" RequestId.decoder)
                        (JD.field "body" JD.value)

                nextPromise : Msg -> m -> Promise m Value
                nextPromise msg _ =
                    case msg of
                        PortResponseMsg param ->
                            if param.requestId == myRequestId then
                                succeedPromise param.response
                                    |> setLogs
                                        [ ResolvePortRequest myRequestId
                                        ]

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                    , subs =
                        (\_ ->
                            Just
                                ( myRequestId
                                , o.ports.response
                                    (\rawResponse ->
                                        case JD.decodeValue responseDecoder rawResponse of
                                            Err _ ->
                                                NoOp

                                            Ok ( requestId, body ) ->
                                                if requestId == myRequestId then
                                                    PortResponseMsg
                                                        { requestId = requestId
                                                        , response = body
                                                        }

                                                else
                                                    NoOp
                                    )
                                )
                        )
                            :: context.subs
                }
            , realCmds =
                [ o.ports.request <|
                    JE.object
                        [ ( "id", RequestId.toValue myRequestId )
                        , ( "body", o.requestBody )
                        ]
                ]
            , logs =
                [ HandshakePortStream myRequestId thisLayerId o.requestBody
                ]
            , state = AwaitMsg nextPromise
            }


{-| -}
now : Promise m Posix
now =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                nextPromise : Msg -> m -> Promise m Posix
                nextPromise msg _ =
                    case msg of
                        CurrentTimeMsg param ->
                            if param.requestId == myRequestId then
                                succeedPromise param.timestamp

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                }
            , realCmds =
                [ Time.now
                    |> Task.perform
                        (\timestamp ->
                            CurrentTimeMsg
                                { requestId = myRequestId
                                , timestamp = timestamp
                                }
                        )
                ]
            , logs =
                [ RequestCurrentTime myRequestId
                ]
            , state = AwaitMsg nextPromise
            }


{-| -}
here : Promise m Time.Zone
here =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                nextPromise : Msg -> m -> Promise m Time.Zone
                nextPromise msg _ =
                    case msg of
                        CurrentZoneMsg param ->
                            if param.requestId == myRequestId then
                                succeedPromise param.zone

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                }
            , realCmds =
                [ Time.here
                    |> Task.perform
                        (\zone ->
                            CurrentZoneMsg
                                { requestId = myRequestId
                                , zone = zone
                                }
                        )
                ]
            , logs =
                [ RequestCurrentZone myRequestId
                ]
            , state = AwaitMsg nextPromise
            }


{-| -}
getValue : String -> Promise m (Maybe String)
getValue key =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state =
                unwrapThisLayerValues context.layer.values
                    |> Dict.get key
                    |> Resolved
            }


{-| -}
getCheck : String -> Promise m (Maybe Bool)
getCheck key =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state =
                unwrapThisLayerChecks context.layer.checks
                    |> Dict.get key
                    |> Resolved
            }


{-| -}
getValues : Promise m (Dict String String)
getValues =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state =
                unwrapThisLayerValues context.layer.values
                    |> Resolved
            }


{-| -}
getChecks : Promise m (Dict String Bool)
getChecks =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state =
                unwrapThisLayerChecks context.layer.checks
                    |> Resolved
            }


{-| -}
setValue : String -> String -> Promise m ()
setValue key val =
    Promise <|
        \context ->
            { newContext =
                { context
                    | layer =
                        let
                            layer =
                                context.layer
                        in
                        { layer
                            | values =
                                unwrapThisLayerValues layer.values
                                    |> Dict.insert key val
                                    |> ThisLayerValues
                        }
                }
            , realCmds = []
            , logs = []
            , state = Resolved ()
            }


{-| -}
setCheck : String -> Bool -> Promise m ()
setCheck key val =
    Promise <|
        \context ->
            { newContext =
                { context
                    | layer =
                        let
                            layer =
                                context.layer
                        in
                        { layer
                            | checks =
                                unwrapThisLayerChecks layer.checks
                                    |> Dict.insert key val
                                    |> ThisLayerChecks
                        }
                }
            , realCmds = []
            , logs = []
            , state = Resolved ()
            }


{-| -}
customRequest :
    (RequestId
     -> Msg
     -> m
     ->
        Maybe
            ( a
              -- Logs for free resources on scenario testing
            , List Log
            )
    )
    -- Real Command
    -> (RequestId -> Cmd Msg)
    -- Log on start evaluation
    -> (RequestId -> LayerId -> Log)
    -> Promise m a
customRequest newPromise_ realCmd log =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                nextPromise : Msg -> m -> Promise m a
                nextPromise msg m =
                    case newPromise_ myRequestId msg m of
                        Just ( a, logs ) ->
                            succeedPromise a
                                |> setLogs logs

                        Nothing ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                }
            , realCmds =
                [ realCmd myRequestId
                ]
            , logs =
                [ log myRequestId thisLayerId
                ]
            , state = AwaitMsg nextPromise
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
awaitCustomViewEvent param =
    Promise <|
        \context ->
            let
                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                state : Msg -> m -> Promise m a
                state msg _ =
                    case msg of
                        ViewMsg r ->
                            if r.layerId == thisLayerId && r.type_ == param.type_ then
                                case JD.decodeValue param.decoder r.value of
                                    Err _ ->
                                        justAwaitPromise state

                                    Ok { value } ->
                                        succeedPromise value

                            else
                                justAwaitPromise state

                        _ ->
                            justAwaitPromise state
            in
            { newContext =
                { context
                    | layer =
                        let
                            layer =
                                context.layer
                        in
                        { layer
                            | events =
                                unwrapThisLayerEvents layer.events
                                    |> Dict.update param.key
                                        (\mdict ->
                                            Maybe.withDefault Dict.empty mdict
                                                |> Dict.insert param.type_
                                                    (param.decoder
                                                        |> JD.map
                                                            (\{ stopPropagation, preventDefault } ->
                                                                { stopPropagation = stopPropagation
                                                                , preventDefault = preventDefault
                                                                }
                                                            )
                                                    )
                                                |> Just
                                        )
                                    |> ThisLayerEvents
                        }
                }
            , realCmds = []
            , logs = []
            , state = AwaitMsg state
            }


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
customViewEventStream param =
    Promise <|
        \context ->
            let
                thisLayerId =
                    unwrapThisLayerId
                        context.layer.id

                nextStream : Msg -> ( List a, Stream a )
                nextStream msg =
                    case msg of
                        ViewMsg r ->
                            if r.layerId == thisLayerId && r.type_ == param.type_ then
                                case JD.decodeValue param.decoder r.value of
                                    Err _ ->
                                        ( [], ActiveStream { unwrapMsg = nextStream } )

                                    Ok { value } ->
                                        ( [ value ], ActiveStream { unwrapMsg = nextStream } )

                            else
                                ( [], ActiveStream { unwrapMsg = nextStream } )

                        _ ->
                            ( [], ActiveStream { unwrapMsg = nextStream } )
            in
            { newContext =
                { context
                    | layer =
                        let
                            layer =
                                context.layer
                        in
                        { layer
                            | events =
                                unwrapThisLayerEvents layer.events
                                    |> Dict.update param.key
                                        (\mdict ->
                                            Maybe.withDefault Dict.empty mdict
                                                |> Dict.insert param.type_
                                                    (param.decoder
                                                        |> JD.map
                                                            (\{ stopPropagation, preventDefault } ->
                                                                { stopPropagation = stopPropagation
                                                                , preventDefault = preventDefault
                                                                }
                                                            )
                                                    )
                                                |> Just
                                        )
                                    |> ThisLayerEvents
                        }
                }
            , realCmds = []
            , logs = []
            , state = Resolved <| ActiveStream { unwrapMsg = nextStream }
            }



-- TEA


{-| -}
init :
    m
    -> Promise m ()
    -> NewState m
init m prom =
    toModel (initContext m) prom


initContext : m -> Context m
initContext memory =
    { layer =
        { state = memory
        , id = ThisLayerId LayerId.init
        , events = ThisLayerEvents Dict.empty
        , values = ThisLayerValues Dict.empty
        , checks = ThisLayerChecks Dict.empty
        }
    , nextRequestId = RequestId.init
    , nextLayerId = LayerId.inc LayerId.init
    , subs = []
    }


toModel : Context m -> Promise m () -> NewState m
toModel context (Promise prom) =
    let
        eff =
            prom context
    in
    case eff.state of
        Resolved _ ->
            { nextModel = endOfModel eff.newContext
            , realCmds = eff.realCmds
            , logs = eff.logs
            }

        AwaitMsg nextProm ->
            { nextModel =
                Model
                    { context = eff.newContext
                    , next =
                        \msg nextContext ->
                            case msg of
                                ViewMsg viewMsg ->
                                    if ThisLayerId viewMsg.layerId == nextContext.layer.id && viewMsg.type_ == "change" then
                                        let
                                            layer =
                                                nextContext.layer
                                        in
                                        toModel
                                            { nextContext
                                                | layer =
                                                    { layer
                                                        | values =
                                                            unwrapThisLayerValues layer.values
                                                                |> (case JD.decodeValue Html.Events.targetValue viewMsg.value of
                                                                        Err _ ->
                                                                            identity

                                                                        Ok value ->
                                                                            Dict.insert viewMsg.key value
                                                                   )
                                                                |> ThisLayerValues
                                                        , checks =
                                                            unwrapThisLayerChecks layer.checks
                                                                |> (case JD.decodeValue checkPropertyDecoder viewMsg.value of
                                                                        Err _ ->
                                                                            identity

                                                                        Ok checks ->
                                                                            Dict.insert viewMsg.key checks
                                                                   )
                                                                |> ThisLayerChecks
                                                    }
                                            }
                                            (nextProm msg nextContext.layer.state)

                                    else
                                        toModel nextContext
                                            (nextProm msg nextContext.layer.state)

                                _ ->
                                    toModel nextContext
                                        (nextProm msg nextContext.layer.state)
                    }
            , realCmds = eff.realCmds
            , logs = eff.logs
            }


checkPropertyDecoder : Decoder Bool
checkPropertyDecoder =
    JD.at [ "target", "tagName" ] JD.string
        |> JD.andThen
            (\tagName ->
                if tagName /= "INPUT" then
                    JD.fail "Not an input element"

                else
                    JD.at [ "target", "type" ] JD.string
            )
        |> JD.andThen
            (\type_ ->
                let
                    isCheckboxOrRadio =
                        List.member (String.toLower type_)
                            [ "checkbox"
                            , "radio"
                            ]
                in
                if isCheckboxOrRadio then
                    JD.at [ "target", "checked" ] JD.bool

                else
                    JD.fail "Not a checkable element"
            )


{-| -}
update : Msg -> Model m -> NewState m
update msg (Model model) =
    model.next msg model.context


{-| -}
documentView : (memory -> Document Msg) -> Model memory -> Document Msg
documentView f model =
    let
        (Layer layer_) =
            layerState model
    in
    f layer_.state


{-| -}
subscriptions : Model memory -> Sub Msg
subscriptions (Model model) =
    List.filterMap
        (\f ->
            f model.context.layer.state
        )
        model.context.subs
        |> List.map Tuple.second
        |> Sub.batch
