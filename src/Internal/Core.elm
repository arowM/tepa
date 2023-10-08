module Internal.Core exposing
    ( Model(..), Model_, AppState(..), Context, memoryState, layerState
    , Msg(..)
    , NavKey(..)
    , Promise(..)
    , PromiseState(..)
    , succeedPromise, justAwaitPromise
    , mapPromise
    , andThenPromise
    , syncPromise
    , liftPromiseMemory, maybeLiftPromiseMemory
    , neverResolved
    , portRequest, portStream, PortRequest, releasePorts
    , httpRequest, httpBytesRequest, HttpRequestError(..), HttpRequest
    , HttpRequestBody(..), RequestId(..)
    , now, here
    , getValue, getValues, setValue
    , getCheck, getChecks, setCheck
    , customRequest
    , awaitCustomViewEvent, customViewEventStream
    , Layer(..), LayerId(..), Layer_, ThisLayerId(..), mapLayerQuery, unwrapThisLayerId, mapLayer, maybeMapLayer
    , ThisLayerEvents(..), ThisLayerValues(..)
    , viewArgs
    , none, sequence, concurrent
    , modify, push, currentState, currentLayerId, lazy
    , sleep, listenTimeEvery, tick, listenMsg
    , load, reload
    , assertionError
    , RandomValue(..), RandomRequest(..), RandomSpec(..), isRequestForSpec
    , Stream(..)
    , onGoingProcedure
    , newLayer, onLayer, LayerResult(..)
    , init, update, NewState, Log(..)
    , documentView, subscriptions
    )

{-|


# Core

@docs Model, Model_, AppState, Context, memoryState, layerState
@docs Msg


# NavKey

@docs NavKey


# Promise

@docs Promise
@docs PromiseState
@docs succeedPromise, justAwaitPromise
@docs mapPromise
@docs andThenPromise
@docs syncPromise
@docs liftPromiseMemory, maybeLiftPromiseMemory
@docs neverResolved
@docs portRequest, portStream, PortRequest, releasePorts
@docs httpRequest, httpBytesRequest, HttpRequestError, HttpRequest
@docs HttpRequestBody, RequestId
@docs now, here
@docs getValue, getValues, setValue
@docs getCheck, getChecks, setCheck
@docs customRequest
@docs awaitCustomViewEvent, customViewEventStream
@docs Layer, LayerId, Layer_, ThisLayerId, mapLayerQuery, unwrapThisLayerId, mapLayer, maybeMapLayer
@docs ThisLayerEvents, ThisLayerValues
@docs viewArgs
@docs none, sequence, concurrent


# Primitive Procedures

@docs modify, push, currentState, currentLayerId, lazy
@docs sleep, listenTimeEvery, tick, listenMsg
@docs load, reload
@docs assertionError


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
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mixin
import Process
import SequenceId exposing (SequenceId)
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


{-| -}
type AppState flags memory
    = AppLoading
    | AppLoaded flags memory


{-| Execution time context for Procedures
-}
type alias Context m =
    { layer : Layer_ m
    , nextRequestId : RequestId
    , nextLayerId : LayerId
    , subs : List (m -> Maybe ( RequestId, Sub Msg ))
    , ports : List PortRequest
    }


{-| -}
type alias PortRequest =
    { request : RequestId
    , layer : LayerId
    , release : Value -> Cmd Msg
    , sub : Sub Msg
    , name : String
    , requestBody : Value
    }


{-| -}
type LayerId
    = LayerId SequenceId


incLayerId : LayerId -> LayerId
incLayerId (LayerId id) =
    SequenceId.inc id
        |> LayerId


initLayerId : LayerId
initLayerId =
    LayerId SequenceId.init


{-| -}
type RequestId
    = RequestId SequenceId


incRequestId : RequestId -> RequestId
incRequestId (RequestId id) =
    SequenceId.inc id
        |> RequestId


initRequestId : RequestId
initRequestId =
    RequestId SequenceId.init


stringifyRequestId : RequestId -> String
stringifyRequestId (RequestId id) =
    SequenceId.toString id


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
    | IssueRandomRequest RequestId LayerId RandomRequest
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
    | AssertionError String


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
        , dependencies : List RequestId
        , released : List RequestId
        }
    | EndOfStream
        { released : List RequestId
        }



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


{-| -}
assertionError : String -> Promise m ()
assertionError str =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = [ AssertionError str ]
            , state = Resolved ()
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
                            if wrapThisLayerId r.layerId == param.layerId && r.type_ == "change" then
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

        -- Release resources here.
        layerExpired : Context m -> PromiseEffect m (LayerResult a)
        layerExpired ctx =
            { newContext =
                { ctx
                    | ports =
                        List.filter
                            (\p -> p.layer /= thisLayerId)
                            ctx.ports
                }
            , realCmds =
                List.filter
                    (\p -> p.layer == thisLayerId)
                    ctx.ports
                    |> List.map
                        (\p ->
                            p.release <|
                                JE.object
                                    [ ( "id", JE.string <| stringifyRequestId p.request )
                                    ]
                        )
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
                                , ports = []
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
                        , ports = context.ports ++ eff1.newContext.ports
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
releasePorts : List RequestId -> Promise m ()
releasePorts released =
    Promise <|
        \context ->
            { newContext =
                { context
                    | ports =
                        List.filter
                            (\p -> not <| List.member p.request released)
                            context.ports
                }
            , realCmds =
                List.filter (\p -> List.member p.request released) context.ports
                    |> List.map
                        (\p ->
                            p.release <|
                                JE.object
                                    [ ( "id", JE.string <| stringifyRequestId p.request )
                                    ]
                        )
            , logs = []
            , state = Resolved ()
            }


{-| -}
maybeLiftPromiseMemory :
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    }
    -> Promise m1 a
    -> Promise m (Maybe a)
maybeLiftPromiseMemory o (Promise prom1) =
    Promise <|
        \context ->
            case o.get context.layer.state of
                Nothing ->
                    { newContext = context
                    , realCmds = []
                    , logs = []
                    , state = Resolved Nothing
                    }

                Just state1 ->
                    let
                        eff1 =
                            prom1
                                { layer =
                                    { state = state1
                                    , id =
                                        coerceThisLayerId context.layer.id
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
                                , ports = []
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
                                        o.get m |> Maybe.andThen f
                                    )
                                    eff1.newContext.subs
                        , ports = context.ports ++ eff1.newContext.ports
                        }
                    , realCmds = eff1.realCmds
                    , logs = eff1.logs
                    , state =
                        case eff1.state of
                            Resolved a ->
                                Resolved <| Just a

                            AwaitMsg nextProm ->
                                AwaitMsg <|
                                    \msg m ->
                                        case o.get m of
                                            Just mNext ->
                                                maybeLiftPromiseMemory o (nextProm msg mNext)

                                            Nothing ->
                                                succeedPromise Nothing
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
                                coerceThisLayerId context.layer.id
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
                        , ports = []
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
                , ports = context.ports ++ eff1.newContext.ports
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
currentLayerId : Promise m String
currentLayerId =
    Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state =
                context.layer.id
                    |> stringifyThisLayerId
                    |> Resolved
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
                    { context | nextLayerId = incLayerId newLayerId }
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
mapLayerQuery :
    (m1 -> m2)
    -> (Layer m -> Maybe (Layer m1))
    -> (Layer m -> Maybe (Layer m2))
mapLayerQuery f parent =
    \l ->
        parent l
            |> Maybe.map
                (\(Layer layer1) ->
                    Layer
                        { id =
                            coerceThisLayerId layer1.id
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
mapLayer :
    (m1 -> m2)
    -> Layer m1
    -> Layer m2
mapLayer f (Layer layer) =
    Layer
        { id =
            unwrapThisLayerId layer.id
                |> wrapThisLayerId
        , state = f layer.state
        , events =
            unwrapThisLayerEvents layer.events
                |> ThisLayerEvents
        , values =
            unwrapThisLayerValues layer.values
                |> ThisLayerValues
        , checks =
            unwrapThisLayerChecks layer.checks
                |> ThisLayerChecks
        }


{-| -}
maybeMapLayer :
    (m1 -> Maybe m2)
    -> Layer m1
    -> Maybe (Layer m2)
maybeMapLayer f (Layer layer) =
    Maybe.map
        (\m2 ->
            Layer
                { id =
                    unwrapThisLayerId layer.id
                        |> wrapThisLayerId
                , state = m2
                , events =
                    unwrapThisLayerEvents layer.events
                        |> ThisLayerEvents
                , values =
                    unwrapThisLayerValues layer.values
                        |> ThisLayerValues
                , checks =
                    unwrapThisLayerChecks layer.checks
                        |> ThisLayerChecks
                }
        )
        (f layer.state)


{-| -}
newLayer : m1 -> Promise m (Layer m1)
newLayer m1 =
    genNewLayerId
        |> mapPromise
            (\layerId ->
                Layer
                    { id = wrapThisLayerId layerId
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
        , setKey : String -> Mixin.Mixin Msg
        , values : Dict String String
        , checks : Dict String Bool
        , layerId : String
        , setKey_ : String -> List (Attribute Msg)
        }
viewArgs (Layer layer) =
    let
        setKey_ =
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
                                            -- This is intentional.
                                            -- `Attributes.property` causes reset user input during editing because
                                            -- it fires all the time the application Model is updated.
                                            -- We have come up with three ways to solve this problem,
                                            -- but for now we will adopt none of them and make them TEPA specifications.
                                            -- * a. Update values on `input` event
                                            --      * It causes extra costs
                                            -- * b. Remove `Attributes.property "value"` during editing
                                            --      * We gave up this solution because Elm runtime seems to set `""` on removing `Attributes.property "value"`
                                            -- * c. Create TEPA-original version of `elm` command, it enables Kernel modules for us
                                            [ Attributes.attribute "value" str
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
    in
    { state = layer.state
    , setKey =
        \key ->
            setKey_ key
                |> Mixin.fromAttributes
    , values = unwrapThisLayerValues layer.values
    , checks = unwrapThisLayerChecks layer.checks
    , layerId = stringifyThisLayerId layer.id
    , setKey_ = setKey_
    }


{-| -}
type ThisLayerId m
    = ThisLayerId SequenceId


stringifyThisLayerId : ThisLayerId m -> String
stringifyThisLayerId (ThisLayerId id) =
    SequenceId.toString id


{-| -}
unwrapThisLayerId : ThisLayerId m -> LayerId
unwrapThisLayerId (ThisLayerId id) =
    LayerId id


wrapThisLayerId : LayerId -> ThisLayerId m
wrapThisLayerId (LayerId id) =
    ThisLayerId id


coerceThisLayerId : ThisLayerId m1 -> ThisLayerId m2
coerceThisLayerId (ThisLayerId id) =
    ThisLayerId id


{-| -}
portStream :
    { ports :
        { request : Value -> Cmd Msg
        , response : (Value -> Msg) -> Sub Msg
        , cancel : Maybe (Value -> Cmd Msg)
        , name : String
        }
    , requestBody : Value
    }
    -> Promise m (Stream Value)
portStream o =
    Promise <|
        \context ->
            let
                myRequestId : RequestId
                myRequestId =
                    context.nextRequestId

                thisLayerId =
                    unwrapThisLayerId context.layer.id

                responseDecoder : Decoder ( String, Value )
                responseDecoder =
                    JD.map2 Tuple.pair
                        (JD.field "id" JD.string)
                        (JD.field "body" JD.value)

                activeStream : () -> Stream Value
                activeStream () =
                    ActiveStream
                        { unwrapMsg =
                            \msg ->
                                case msg of
                                    PortResponseMsg streamMsg ->
                                        if streamMsg.requestId == myRequestId then
                                            ( [ streamMsg.response ], activeStream () )

                                        else
                                            ( [], activeStream () )

                                    _ ->
                                        ( [], activeStream () )
                        , dependencies = [ myRequestId ]
                        , released = []
                        }
            in
            { newContext =
                { context
                    | nextRequestId = incRequestId context.nextRequestId
                    , ports =
                        { request = myRequestId
                        , layer = thisLayerId
                        , release =
                            case o.ports.cancel of
                                Nothing ->
                                    \_ -> Cmd.none

                                Just f ->
                                    f
                        , sub =
                            o.ports.response
                                (\rawResponse ->
                                    case JD.decodeValue responseDecoder rawResponse of
                                        Err _ ->
                                            NoOp

                                        Ok ( rawRequestId, body ) ->
                                            if rawRequestId == stringifyRequestId myRequestId then
                                                PortResponseMsg
                                                    { requestId = myRequestId
                                                    , response = body
                                                    }

                                            else
                                                NoOp
                                )
                        , name = o.ports.name
                        , requestBody = o.requestBody
                        }
                            :: context.ports
                }
            , realCmds =
                [ o.ports.request <|
                    JE.object
                        [ ( "id", JE.string <| stringifyRequestId myRequestId )
                        , ( "body", o.requestBody )
                        ]
                ]
            , logs = []
            , state =
                Resolved <| activeStream ()
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
                    | nextRequestId = incRequestId context.nextRequestId
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
                        | nextRequestId = incRequestId context.nextRequestId
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
                        | nextRequestId = incRequestId context.nextRequestId
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

                nextStream : () -> Stream Posix
                nextStream () =
                    ActiveStream
                        { unwrapMsg =
                            \msg ->
                                case msg of
                                    IntervalMsg param ->
                                        if param.requestId == myRequestId then
                                            ( [ param.timestamp ]
                                            , nextStream ()
                                            )

                                        else
                                            ( []
                                            , nextStream ()
                                            )

                                    _ ->
                                        ( []
                                        , nextStream ()
                                        )
                        , dependencies = [ myRequestId ]
                        , released = []
                        }
            in
            { newContext =
                newContext
            , realCmds = []
            , logs =
                [ StartTimeEvery myRequestId thisLayerId interval
                ]
            , state =
                Resolved <| nextStream ()
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
                    | nextRequestId = incRequestId context.nextRequestId
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
                    | nextRequestId = incRequestId context.nextRequestId
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
        , name : String
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

                responseDecoder : Decoder ( String, Value )
                responseDecoder =
                    JD.map2 Tuple.pair
                        (JD.field "id" JD.string)
                        (JD.field "body" JD.value)

                nextPromise : Msg -> m -> Promise m Value
                nextPromise msg _ =
                    case msg of
                        PortResponseMsg param ->
                            if param.requestId == myRequestId then
                                succeedPromise param.response

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = incRequestId context.nextRequestId
                    , ports =
                        { request = myRequestId
                        , layer = thisLayerId
                        , release = \_ -> Cmd.none
                        , sub =
                            o.ports.response
                                (\rawResponse ->
                                    case JD.decodeValue responseDecoder rawResponse of
                                        Err _ ->
                                            NoOp

                                        Ok ( rawRequestId, body ) ->
                                            if rawRequestId == stringifyRequestId myRequestId then
                                                PortResponseMsg
                                                    { requestId = myRequestId
                                                    , response = body
                                                    }

                                            else
                                                NoOp
                                )
                        , name = o.ports.name
                        , requestBody = o.requestBody
                        }
                            :: context.ports
                }
            , realCmds =
                [ o.ports.request <|
                    JE.object
                        [ ( "id", JE.string <| stringifyRequestId myRequestId )
                        , ( "body", o.requestBody )
                        ]
                ]
            , logs = []
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
                    | nextRequestId = incRequestId context.nextRequestId
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
                    | nextRequestId = incRequestId context.nextRequestId
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
                    | nextRequestId = incRequestId context.nextRequestId
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
                            if r.layerId == thisLayerId && r.type_ == param.type_ && r.key == param.key then
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

                nextStream : () -> Stream a
                nextStream () =
                    ActiveStream
                        { unwrapMsg =
                            \msg ->
                                case msg of
                                    ViewMsg r ->
                                        if r.layerId == thisLayerId && r.type_ == param.type_ && r.key == param.key then
                                            case JD.decodeValue param.decoder r.value of
                                                Err _ ->
                                                    ( [], nextStream () )

                                                Ok { value } ->
                                                    ( [ value ], nextStream () )

                                        else
                                            ( [], nextStream () )

                                    _ ->
                                        ( [], nextStream () )
                        , dependencies = []
                        , released = []
                        }
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
            , state = Resolved <| nextStream ()
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
        , id = ThisLayerId SequenceId.init
        , events = ThisLayerEvents Dict.empty
        , values = ThisLayerValues Dict.empty
        , checks = ThisLayerChecks Dict.empty
        }
    , nextRequestId = initRequestId
    , nextLayerId = incLayerId initLayerId
    , subs = []
    , ports = []
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
    [ List.filterMap
        (\f ->
            f model.context.layer.state
        )
        model.context.subs
        |> List.map Tuple.second
    , List.map .sub model.context.ports
    ]
        |> List.concat
        |> Sub.batch
