module Internal.Core exposing
    ( Model(..), Model_, AppState(..), Context, memoryState, layerState
    , Msg(..)
    , NavKey(..)
    , Promise(..)
    , PromiseState(..), LayerState(..)
    , succeedPromise, justAwaitPromise
    , mapPromise
    , andThenPromise
    , syncPromise
    , liftPromiseMemory, maybeLiftPromiseMemory
    , neverResolved
    , portRequest, portStream, PortRequest
    , httpRequest, httpBytesRequest, HttpRequestError(..), HttpRequest
    , HttpRequestBody(..), RequestId(..)
    , now, here
    , getValue, getValues, setValue
    , getCheck, getChecks, setCheck
    , customRequest
    , awaitCustomViewEvent, customViewEventStream
    , Layer(..), LayerId(..), Layer_, ThisLayerId(..), mapLayerQuery, unwrapThisLayerId, maybeMapLayer
    , ThisLayerEvents(..), ThisLayerValues(..)
    , viewArgs
    , none, sequence
    , modify, currentState, lazy
    , sleep, listenTimeEvery, tick, listenMsg
    , load, reload
    , assertionError
    , RandomValue(..), RandomRequest(..), RandomSpec(..), isRequestForSpec
    , Stream(..)
    , releaseStreamResources
    , onGoingProcedure
    , newLayer, onLayer
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
@docs PromiseState, LayerState
@docs succeedPromise, justAwaitPromise
@docs mapPromise
@docs andThenPromise
@docs syncPromise
@docs liftPromiseMemory, maybeLiftPromiseMemory
@docs neverResolved
@docs portRequest, portStream, PortRequest
@docs httpRequest, httpBytesRequest, HttpRequestError, HttpRequest
@docs HttpRequestBody, RequestId
@docs now, here
@docs getValue, getValues, setValue
@docs getCheck, getChecks, setCheck
@docs customRequest
@docs awaitCustomViewEvent, customViewEventStream
@docs Layer, LayerId, Layer_, ThisLayerId, mapLayerQuery, unwrapThisLayerId, maybeMapLayer
@docs ThisLayerEvents, ThisLayerValues
@docs viewArgs
@docs none, sequence


# Primitive Procedures

@docs modify, currentState, lazy
@docs sleep, listenTimeEvery, tick, listenMsg
@docs load, reload
@docs assertionError


# Random

@docs RandomValue, RandomRequest, RandomSpec, isRequestForSpec


# Stream

@docs Stream
@docs releaseStreamResources


# Helper Procedures

@docs onGoingProcedure


# Layer

@docs newLayer, onLayer


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
    , nextIntervalId : Dict Int Int
    , ticks : List TickRequest
    , ports : List PortRequest
    }


{-| -}
type alias TickRequest =
    { request : RequestId
    , layer : LayerId
    , msec : Float
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
    -- IGNORE TCO
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
    = Resolved (LayerState a)
    | AwaitMsg (Msg -> m -> Promise m a)


{-| Represents the ability to access lifted memory parts.
By holding the state internally, it enables to continue processing procedures for a Layer even after the lifted memory becomes unreachable, or to continue processing proceduures for a Memory part even after the Layer has expired.

See the `Tepa.maybeLiftMemory` and `Tepa.onLayer`.

-}
type LayerState a
    = LayerExist a
    | LayerUnreachable
    | MemoryUnreachable


{-| -}
mapPromise : (LayerState a -> LayerState b) -> Promise m a -> Promise m b
mapPromise f (Promise prom) =
    -- IGNORE TCO
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
            , state = Resolved (LayerExist ())
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
succeedPromise : LayerState a -> Promise m a
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
    -- IGNORE TCO
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
                    ( Resolved (LayerExist f), Resolved (LayerExist a) ) ->
                        Resolved <| LayerExist <| f a

                    ( Resolved (LayerExist _), Resolved LayerUnreachable ) ->
                        Resolved LayerUnreachable

                    ( Resolved (LayerExist _), Resolved MemoryUnreachable ) ->
                        Resolved MemoryUnreachable

                    ( Resolved (LayerExist f), AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise
                                    (\state ->
                                        case state of
                                            LayerExist a ->
                                                LayerExist <| f a

                                            MemoryUnreachable ->
                                                MemoryUnreachable

                                            LayerUnreachable ->
                                                LayerUnreachable
                                    )
                                    (nextPromA msg m)

                    ( Resolved LayerUnreachable, Resolved _ ) ->
                        Resolved LayerUnreachable

                    ( Resolved LayerUnreachable, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                nextPromA msg m
                                    |> mapPromise (\_ -> LayerUnreachable)

                    ( Resolved MemoryUnreachable, Resolved _ ) ->
                        Resolved MemoryUnreachable

                    ( Resolved MemoryUnreachable, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                nextPromA msg m
                                    |> mapPromise (\_ -> MemoryUnreachable)

                    ( AwaitMsg nextPromF, Resolved (LayerExist a) ) ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise
                                    (\res ->
                                        case res of
                                            LayerExist f ->
                                                LayerExist <| f a

                                            MemoryUnreachable ->
                                                MemoryUnreachable

                                            LayerUnreachable ->
                                                LayerUnreachable
                                    )
                                    (nextPromF msg m)

                    ( AwaitMsg nextPromF, Resolved LayerUnreachable ) ->
                        AwaitMsg <|
                            \msg m ->
                                nextPromF msg m
                                    |> mapPromise (\_ -> LayerUnreachable)

                    ( AwaitMsg nextPromF, Resolved MemoryUnreachable ) ->
                        AwaitMsg <|
                            \msg m ->
                                nextPromF msg m
                                    |> mapPromise (\_ -> MemoryUnreachable)

                    ( AwaitMsg nextPromF, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                syncPromise (nextPromA msg m) (nextPromF msg m)
            }


{-| Run monitor promise asyncronously.
-}
andMonitor : Promise m a -> Promise m a -> Promise m a
andMonitor (Promise prom2) (Promise prom1) =
    -- IGNORE TCO
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
                    ( Resolved (LayerExist a1), _ ) ->
                        Resolved <| LayerExist a1

                    ( AwaitMsg nextProm1, AwaitMsg nextProm2 ) ->
                        AwaitMsg <|
                            \msg m ->
                                andMonitor
                                    (nextProm2 msg m)
                                    (nextProm1 msg m)

                    ( AwaitMsg nextProm1, _ ) ->
                        AwaitMsg nextProm1

                    ( Resolved _, _ ) ->
                        eff1.state
            }


{-| Run Promises sequentially.
-}
andThenPromise : (LayerState a -> Promise m b) -> Promise m a -> Promise m b
andThenPromise f (Promise promA) =
    -- IGNORE TCO
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
type alias LayerMemory link body =
    { link : Maybe link
    , body : Maybe body
    }


{-| -}
onLayer :
    { getLink : m -> Maybe link
    , setLink : link -> m -> m
    , getBody : m -> Maybe (Layer body)
    , setBody : Layer body -> m -> m
    }
    -> Promise (LayerMemory link body) a
    -> Promise m (LayerState a)
onLayer param prom1 =
    currentState
        |> andThenPromise
            (\res ->
                case res of
                    LayerUnreachable ->
                        succeedPromise <| LayerExist LayerUnreachable

                    MemoryUnreachable ->
                        succeedPromise <| LayerExist MemoryUnreachable

                    LayerExist state ->
                        case param.getBody state of
                            Nothing ->
                                succeedPromise <| LayerExist LayerUnreachable

                            Just (Layer layer) ->
                                onLayer_
                                    (unwrapThisLayerId layer.id)
                                    { getLink = param.getLink
                                    , setLink = param.setLink
                                    , getBody =
                                        \m ->
                                            Maybe.andThen
                                                (\(Layer layer_) ->
                                                    if layer_.id == layer.id then
                                                        Just (Layer layer_)

                                                    else
                                                        Nothing
                                                )
                                                (param.getBody m)
                                    , setBody = param.setBody
                                    }
                                    prom1
            )


{-| Make sure to check LayerId in `getBody`.
-}
onLayer_ :
    LayerId
    ->
        { getLink : m -> Maybe link
        , setLink : link -> m -> m
        , getBody : m -> Maybe (Layer body)
        , setBody : Layer body -> m -> m
        }
    -> Promise (LayerMemory link body) a
    -> Promise m (LayerState a)
onLayer_ lid param (Promise prom1) =
    -- IGNORE TCO
    Promise <|
        \context ->
            case param.getBody context.layer.state of
                -- Layer has expired.
                -- Run Promise for link on parent Layer.
                Nothing ->
                    let
                        context1 : Context (LayerMemory link body)
                        context1 =
                            { layer =
                                { id = coerceThisLayerId context.layer.id
                                , state =
                                    { link = param.getLink context.layer.state
                                    , body = Nothing
                                    }
                                , events = ThisLayerEvents Dict.empty
                                , values = ThisLayerValues Dict.empty
                                , checks = ThisLayerChecks Dict.empty
                                }
                            , nextRequestId =
                                context.nextRequestId
                            , nextLayerId =
                                context.nextLayerId
                            , nextIntervalId = context.nextIntervalId
                            , ticks =
                                List.filter
                                    (\p -> p.layer /= lid)
                                    context.ticks
                            , ports =
                                List.filter
                                    (\p ->
                                        p.layer /= lid
                                    )
                                    context.ports
                            }

                        eff1 : PromiseEffect (LayerMemory link body) a
                        eff1 =
                            prom1 context1
                    in
                    { newContext =
                        { layer =
                            { id = context.layer.id
                            , state =
                                case eff1.newContext.layer.state.link of
                                    Nothing ->
                                        context.layer.state

                                    Just link ->
                                        param.setLink link context.layer.state
                            , events = context.layer.events
                            , values = context.layer.values
                            , checks = context.layer.checks
                            }
                        , nextRequestId = eff1.newContext.nextRequestId
                        , nextLayerId = eff1.newContext.nextLayerId
                        , nextIntervalId = eff1.newContext.nextIntervalId
                        , ticks =
                            eff1.newContext.ticks
                        , ports =
                            eff1.newContext.ports
                        }
                    , realCmds =
                        List.filterMap
                            (\p ->
                                if p.layer == lid then
                                    Just <|
                                        p.release <|
                                            JE.object
                                                [ ( "id", JE.string <| stringifyRequestId p.request )
                                                ]

                                else
                                    Nothing
                            )
                            context.ports
                            ++ eff1.realCmds
                    , logs = eff1.logs ++ [ LayerHasExpired lid ]
                    , state =
                        case eff1.state of
                            Resolved (LayerExist _) ->
                                Resolved <| LayerExist LayerUnreachable

                            Resolved LayerUnreachable ->
                                Resolved <| LayerExist LayerUnreachable

                            Resolved MemoryUnreachable ->
                                Resolved <| LayerExist MemoryUnreachable

                            AwaitMsg nextProm ->
                                AwaitMsg <|
                                    \msg m ->
                                        onLayer_ lid
                                            param
                                            (nextProm msg
                                                { link = param.getLink m
                                                , body = Nothing
                                                }
                                            )
                    }

                -- Layer exists
                Just (Layer bodyLayer) ->
                    let
                        context1 : Context (LayerMemory link body)
                        context1 =
                            { layer =
                                { id = coerceThisLayerId bodyLayer.id
                                , state =
                                    { link = param.getLink context.layer.state
                                    , body = Just bodyLayer.state
                                    }
                                , events =
                                    unwrapThisLayerEvents bodyLayer.events
                                        |> ThisLayerEvents
                                , values =
                                    unwrapThisLayerValues bodyLayer.values
                                        |> ThisLayerValues
                                , checks =
                                    unwrapThisLayerChecks bodyLayer.checks
                                        |> ThisLayerChecks
                                }
                            , nextRequestId =
                                context.nextRequestId
                            , nextLayerId =
                                context.nextLayerId
                            , nextIntervalId = context.nextIntervalId
                            , ticks = context.ticks
                            , ports = context.ports
                            }

                        eff1 : PromiseEffect (LayerMemory link body) a
                        eff1 =
                            prom1 context1
                    in
                    { newContext =
                        { layer =
                            { id = context.layer.id
                            , state =
                                (case eff1.newContext.layer.state.link of
                                    Nothing ->
                                        context.layer.state

                                    Just link ->
                                        param.setLink link context.layer.state
                                )
                                    |> (case eff1.newContext.layer.state.body of
                                            Nothing ->
                                                identity

                                            Just newBody ->
                                                param.setBody
                                                    (Layer
                                                        { id = coerceThisLayerId eff1.newContext.layer.id
                                                        , state = newBody
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
                                                    )
                                       )
                            , events = context.layer.events
                            , values = context.layer.values
                            , checks = context.layer.checks
                            }
                        , nextRequestId = eff1.newContext.nextRequestId
                        , nextLayerId = eff1.newContext.nextLayerId
                        , nextIntervalId = eff1.newContext.nextIntervalId
                        , ticks =
                            eff1.newContext.ticks
                        , ports =
                            eff1.newContext.ports
                        }
                    , realCmds = eff1.realCmds
                    , logs = eff1.logs
                    , state =
                        case eff1.state of
                            Resolved a ->
                                Resolved <| LayerExist a

                            AwaitMsg nextProm ->
                                AwaitMsg <|
                                    \msg m ->
                                        onLayer_ lid
                                            param
                                            (nextProm msg
                                                { link = param.getLink m
                                                , body =
                                                    param.getBody m
                                                        |> Maybe.map layerStateOf
                                                }
                                                |> (case ( param.getBody m, msg ) of
                                                        ( Just (Layer layer), ViewMsg r ) ->
                                                            if unwrapThisLayerId layer.id == r.layerId && List.member r.type_ [ "change", "blur" ] then
                                                                monitorChange r

                                                            else
                                                                identity

                                                        _ ->
                                                            identity
                                                   )
                                            )
                    }


monitorChange :
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
    -> Promise (LayerMemory link body) a
    -> Promise (LayerMemory link body) a
monitorChange r (Promise prom) =
    Promise <|
        \context ->
            if unwrapThisLayerId context.layer.id == r.layerId then
                let
                    layer =
                        context.layer
                in
                prom
                    { context
                        | layer =
                            { layer
                                | values =
                                    unwrapThisLayerValues layer.values
                                        |> (case JD.decodeValue Html.Events.targetValue r.value of
                                                Err _ ->
                                                    identity

                                                Ok value ->
                                                    Dict.insert r.key value
                                           )
                                        |> ThisLayerValues
                                , checks =
                                    unwrapThisLayerChecks layer.checks
                                        |> (case JD.decodeValue checkPropertyDecoder r.value of
                                                Err _ ->
                                                    identity

                                                Ok checks ->
                                                    Dict.insert r.key checks
                                           )
                                        |> ThisLayerChecks
                            }
                    }

            else
                prom context


layerStateOf : Layer m -> m
layerStateOf (Layer layer) =
    layer.state


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
            , state = Resolved (LayerExist ())
            }


{-| -}
releaseTicks : List RequestId -> Promise m ()
releaseTicks released =
    Promise <|
        \context ->
            { newContext =
                { context
                    | ticks =
                        List.filter
                            (\p -> not <| List.member p.request released)
                            context.ticks
                }
            , realCmds = []
            , logs = []
            , state = Resolved (LayerExist ())
            }


{-| -}
releaseStreamResources : List RequestId -> Promise m ()
releaseStreamResources released =
    releasePorts released
        |> andThenPromise
            (\_ ->
                releaseTicks released
            )


{-| -}
maybeLiftPromiseMemory :
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    }
    -> Promise m1 a
    -> Promise m a
maybeLiftPromiseMemory o (Promise prom1) =
    -- IGNORE TCO
    Promise <|
        \context ->
            case o.get context.layer.state of
                Nothing ->
                    { newContext = context
                    , realCmds = []
                    , logs = []
                    , state = Resolved MemoryUnreachable
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
                                , nextIntervalId = context.nextIntervalId
                                , ticks = context.ticks
                                , ports = context.ports
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
                        , nextIntervalId = eff1.newContext.nextIntervalId
                        , ticks = eff1.newContext.ticks
                        , ports = eff1.newContext.ports
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
                                        case o.get m of
                                            Just mNext ->
                                                maybeLiftPromiseMemory o (nextProm msg mNext)

                                            Nothing ->
                                                succeedPromise MemoryUnreachable
                    }


{-| -}
liftPromiseMemory :
    { get : m -> m1
    , set : m1 -> m -> m
    }
    -> Promise m1 a
    -> Promise m a
liftPromiseMemory o (Promise prom1) =
    -- IGNORE TCO
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
                        , nextIntervalId = context.nextIntervalId
                        , ticks = context.ticks
                        , ports = context.ports
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
                , nextIntervalId = eff1.newContext.nextIntervalId
                , ticks = eff1.newContext.ticks
                , ports = eff1.newContext.ports
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
    succeedPromise (LayerExist ())


{-| -}
sequence : List (Promise m ()) -> Promise m ()
sequence =
    List.foldl
        (\a acc ->
            acc
                |> andThenPromise
                    (\res ->
                        case res of
                            MemoryUnreachable ->
                                succeedPromise <| MemoryUnreachable

                            LayerUnreachable ->
                                succeedPromise <| LayerUnreachable

                            LayerExist () ->
                                a
                    )
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
            , state = Resolved <| LayerExist context.layer.state
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
                , state = Resolved <| LayerExist ()
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
            , state =
                Resolved <|
                    LayerExist <|
                        Layer
                            { id = wrapThisLayerId newLayerId
                            , state = m1
                            , events = ThisLayerEvents Dict.empty
                            , values = ThisLayerValues Dict.empty
                            , checks = ThisLayerChecks Dict.empty
                            }
            }


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
                            |> Dict.update "blur"
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
                Resolved <| LayerExist <| activeStream ()
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
                    justAwaitPromise awaitForever
                        |> andMonitor
                            (handler msg
                                |> sequence
                            )
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
                                succeedPromise <| LayerExist ()

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
            , state = Resolved <| LayerExist ()
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
            , state = Resolved <| LayerExist ()
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
                        , ticks =
                            { request = myRequestId
                            , layer = thisLayerId
                            , msec = toFloat interval
                            }
                                :: context.ticks
                    }

                awaitForever : Msg -> m -> Promise m ()
                awaitForever msg _ =
                    case msg of
                        IntervalMsg param ->
                            if param.requestId == myRequestId then
                                justAwaitPromise awaitForever
                                    |> andMonitor
                                        (handler param.timestamp
                                            |> sequence
                                        )

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

                thisIntervalId : Int
                thisIntervalId =
                    Dict.get interval context.nextIntervalId
                        |> Maybe.withDefault 0
                        |> modBy 1000

                newContext =
                    { context
                        | nextRequestId = incRequestId context.nextRequestId
                        , nextIntervalId = Dict.insert interval (thisIntervalId + 1) context.nextIntervalId
                        , ticks =
                            { request = myRequestId
                            , layer = thisLayerId
                            , msec = toKeyedInterval interval
                            }
                                :: context.ticks
                    }

                -- Work around for bug about `Time.every`.
                -- https://github.com/elm/time/issues/25
                -- It add very small amount of value to indentify the subsctiption.
                toKeyedInterval : Int -> Float
                toKeyedInterval n =
                    toFloat n + (toFloat thisIntervalId * 1.0e-6)

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
                Resolved <| LayerExist <| nextStream ()
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
                                succeedPromise (LayerExist param.response)
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
                                succeedPromise (LayerExist param.response)
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
                                succeedPromise (LayerExist param.response)

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
                                succeedPromise (LayerExist param.timestamp)

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
                                succeedPromise (LayerExist param.zone)

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
                    |> LayerExist
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
                    |> LayerExist
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
                    |> LayerExist
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
                    |> LayerExist
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
            , state = Resolved <| LayerExist ()
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
            , state = Resolved <| LayerExist ()
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
                            succeedPromise (LayerExist a)
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
                                        succeedPromise (LayerExist value)

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
            , state = Resolved <| LayerExist <| nextStream ()
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
        , id = wrapThisLayerId initLayerId
        , events = ThisLayerEvents Dict.empty
        , values = ThisLayerValues Dict.empty
        , checks = ThisLayerChecks Dict.empty
        }
    , nextRequestId = initRequestId
    , nextLayerId = incLayerId initLayerId
    , nextIntervalId = Dict.empty
    , ticks = []
    , ports = []
    }


toModel : Context m -> Promise m () -> NewState m
toModel context (Promise prom) =
    -- IGNORE TCO
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
    [ List.map
        (\req ->
            Time.every req.msec <|
                \timestamp ->
                    IntervalMsg
                        { requestId = req.request
                        , timestamp = timestamp
                        }
        )
        model.context.ticks
    , List.map .sub model.context.ports
    ]
        |> List.concat
        |> Sub.batch
