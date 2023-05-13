module Internal.Core exposing
    ( Model(..), Model_, memoryState
    , Msg(..), rootLayerMsg
    , mapMsg
    , NavKey(..)
    , Promise
    , succeedPromise
    , mapPromise
    , andRacePromise
    , andThenPromise
    , syncPromise
    , liftPromiseEvent
    , Request(..)
    , portRequest, customRequest, anyRequest
    , httpRequest, httpBytesRequest, HttpRequestError(..), HttpRequest
    , now
    , layerEvent
    , Layer(..), Pointer(..), isPointedBy
    , layerView, keyedLayerView, layerDocument, eventAttr, eventMixin
    , none, sequence, concurrent
    , Void, void
    , modify, push, currentState, cancel, lazy, listen
    , sleep, listenTimeEvery
    , onGoingProcedure
    , newLayer, onLayer
    , init, update, NewState, Log(..)
    , elementView, documentView, subscriptions
    , LayerQuery(..)
    , runQuery
    , HttpRequestBody(..), listenLayerEvent
    )

{-|


# Core

@docs Model, Model_, memoryState, layerState
@docs Msg, rootLayerMsg
@docs mapMsg


# NavKey

@docs NavKey


# Promise

@docs Promise
@docs succeedPromise
@docs mapPromise
@docs andRacePromise
@docs andThenPromise
@docs syncPromise
@docs liftPromiseMemory, liftPromiseEvent
@docs Request
@docs portRequest, customRequest, anyRequest
@docs httpRequest, httpBytesRequest, HttpRequestError, HttpRequest
@docs now
@docs layerEvent
@docs Layer, Pointer, isPointedBy
@docs layerView, keyedLayerView, layerDocument, eventAttr, eventMixin
@docs none, sequence, concurrent


# Primitive Procedures

@docs Void, void
@docs modify, push, currentState, return, cancel, lazy, listen
@docs sleep, listenTimeEvery


# Helper Procedures

@docs onGoingProcedure


# Layer

@docs newLayer, onLayer


# TEA

@docs init, update, NewState, Log
@docs elementView, documentView, subscriptions


# LayerQuery

@docs LayerQuery
@docs runQuery

-}

import Browser exposing (Document)
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import File exposing (File)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Http
import Internal.AbsolutePath exposing (AbsolutePath(..))
import Internal.LayerId as LayerId exposing (LayerId)
import Internal.RequestId as RequestId exposing (RequestId)
import Internal.ResponseType as ResponseType exposing (ResponseBody, ResponseType)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Mixin exposing (Mixin)
import Process
import Task
import Time exposing (Posix)



-- Model


{-| -}
type Model memory event
    = Model (Model_ memory event)


{-| -}
type alias Model_ m e =
    -- New context after the evaluation.
    { context : Context m e

    -- New state to evaluate next time.
    , next : Msg e -> Context m e -> NewState m e
    }


endOfModel : Context m e -> Model m e
endOfModel context =
    Model
        { context = context
        , next = \_ -> endOfNewState
        }


{-| Execution time context for Procedures
-}
type alias Context m e =
    { state : m
    , thisLayerId : ThisLayerId m
    , nextRequestId : RequestId
    , nextLayerId : LayerId
    , subs : List (m -> Maybe ( RequestId, Sub (Msg e) ))
    }


{-| New application state after some operations.
-}
type alias NewState m e =
    { nextModel : Model m e
    , requests : List (Request e) -- reversed
    , realCmds : List (Cmd (Msg e)) -- reversed
    , logs : List Log
    }


{-| -}
type Request e
    = Request String RequestId LayerId (Cmd (Msg e))


endOfNewState : Context m e -> NewState m e
endOfNewState context =
    { nextModel =
        Model
            { context = context
            , next = \_ -> endOfNewState
            }
    , requests = []
    , realCmds = []
    , logs = []
    }


{-| Current memory state.
-}
memoryState : Model memory event -> memory
memoryState (Model model) =
    model.context.state


{-| Current Layer state.
-}
layerState : Model memory event -> Layer memory
layerState (Model model) =
    let
        (ThisLayerId lid) =
            model.context.thisLayerId
    in
    Layer lid model.context.state


{-| Application operation logs.
-}
type Log
    = SetTimer RequestId LayerId Int
    | StartTimeEvery RequestId LayerId Int
    | RequestCurrentTime RequestId
    | IssueHttpRequest RequestId LayerId HttpRequest
    | IssuePortRequest RequestId LayerId Value
    | AddListener RequestId LayerId String
    | ResolvePortRequest RequestId
    | ResolveRequest RequestId
    | ResolveHttpRequest RequestId
    | LayerExpired LayerId
    | PushPath AbsolutePath
    | ReplacePath AbsolutePath
    | Back Int
    | Forward Int


type alias HttpRequest =
    { method : String
    , headers : List ( String, String )
    , url : String
    , requestBody : HttpRequestBody
    , timeout : Maybe Int
    , tracker : Maybe String
    }



-- Msg


{-| -}
type Msg event
    = LayerMsg
        { layerId : LayerId
        , event : event
        }
    | AnyResponseMsg
        { requestId : RequestId
        , event : event
        }
    | CustomResponseMsg
        { requestId : RequestId
        , response : ResponseBody
        }
    | PortResponseMsg
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
    | CurrentTimeMsg
        { requestId : RequestId
        , timestamp : Posix
        }
    | ListenerMsg
        { requestId : RequestId
        , event : event
        }
    | ViewStubMsg
        { event : event
        }
    | WakeUpMsg
        { requestId : RequestId
        }
    | IntervalMsg
        { requestId : RequestId
        , timestamp : Posix
        }
    | NoOp


{-| Issue Event on application root layer.
-}
rootLayerMsg : e -> Msg e
rootLayerMsg e =
    LayerMsg
        { layerId = LayerId.init
        , event = e
        }


{-| -}
mapMsg : (event1 -> event0) -> Msg event1 -> Msg event0
mapMsg f msg1 =
    case msg1 of
        LayerMsg r ->
            LayerMsg
                { layerId = r.layerId
                , event = f r.event
                }

        AnyResponseMsg r ->
            AnyResponseMsg
                { requestId = r.requestId
                , event = f r.event
                }

        CustomResponseMsg r ->
            CustomResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        PortResponseMsg r ->
            PortResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        ListenerMsg r ->
            ListenerMsg
                { requestId = r.requestId
                , event = f r.event
                }

        HttpResponseMsg r ->
            HttpResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        HttpBytesResponseMsg r ->
            HttpBytesResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        CurrentTimeMsg r ->
            CurrentTimeMsg
                { requestId = r.requestId
                , timestamp = r.timestamp
                }

        ViewStubMsg r ->
            ViewStubMsg
                { event = f r.event
                }

        WakeUpMsg r ->
            WakeUpMsg
                { requestId = r.requestId
                }

        IntervalMsg r ->
            IntervalMsg
                { requestId = r.requestId
                , timestamp = r.timestamp
                }

        NoOp ->
            NoOp


{-| -}
unwrapMsg : (e0 -> Maybe e1) -> Msg e0 -> Msg e1
unwrapMsg f msg1 =
    case msg1 of
        LayerMsg r ->
            case f r.event of
                Nothing ->
                    NoOp

                Just e1 ->
                    LayerMsg
                        { layerId = r.layerId
                        , event = e1
                        }

        AnyResponseMsg r ->
            case f r.event of
                Nothing ->
                    NoOp

                Just e1 ->
                    AnyResponseMsg
                        { requestId = r.requestId
                        , event = e1
                        }

        CustomResponseMsg r ->
            CustomResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        PortResponseMsg r ->
            PortResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        ListenerMsg r ->
            case f r.event of
                Nothing ->
                    NoOp

                Just e ->
                    ListenerMsg
                        { requestId = r.requestId
                        , event = e
                        }

        HttpResponseMsg r ->
            HttpResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        HttpBytesResponseMsg r ->
            HttpBytesResponseMsg
                { requestId = r.requestId
                , response = r.response
                }

        CurrentTimeMsg r ->
            CurrentTimeMsg
                { requestId = r.requestId
                , timestamp = r.timestamp
                }

        ViewStubMsg r ->
            case f r.event of
                Nothing ->
                    NoOp

                Just e ->
                    ViewStubMsg
                        { event = e
                        }

        WakeUpMsg r ->
            WakeUpMsg
                { requestId = r.requestId
                }

        IntervalMsg r ->
            IntervalMsg
                { requestId = r.requestId
                , timestamp = r.timestamp
                }

        NoOp ->
            NoOp



-- NavKey


{-| -}
type NavKey
    = RealKey Nav.Key
    | SimKey



-- Promise


{-| The Promise represents the eventual completion of an operation and its resulting value.
-}
type Promise m e a
    = Promise (Context m e -> PromiseEffect m e a)


{-| Effects by evaluating a Promise.
-}
type alias PromiseEffect m e a =
    { newContext : Context m e
    , requests : List (Request e) -- reversed
    , realCmds : List (Cmd (Msg e)) -- reversed
    , logs : List Log
    , state : PromiseState m e a
    }


{-| Represents current Promise state.
-}
type PromiseState m e a
    = Resolved a
    | Rejected
    | AwaitMsg (Msg e -> m -> Promise m e a)


{-| -}
mapPromise : (a -> b) -> Promise m e a -> Promise m e b
mapPromise f (Promise prom) =
    Promise <|
        \context ->
            let
                effA =
                    prom context
            in
            { newContext = effA.newContext
            , requests = effA.requests
            , realCmds = effA.realCmds
            , logs = effA.logs
            , state =
                case effA.state of
                    Resolved a ->
                        Resolved <| f a

                    Rejected ->
                        Rejected

                    AwaitMsg next ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise f (next msg m)
            }


primitivePromise : PromiseState m e a -> Promise m e a
primitivePromise state =
    Promise <|
        \context ->
            { newContext = context
            , requests = []
            , realCmds = []
            , logs = []
            , state = state
            }


setLogs : List Log -> Promise m e a -> Promise m e a
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
succeedPromise : a -> Promise m e a
succeedPromise a =
    primitivePromise <| Resolved a


{-| Promise that just rejected.
-}
failPromise : Promise m e a
failPromise =
    primitivePromise Rejected


{-| Promise that just await next Msg.
-}
justAwaitPromise : (Msg e -> m -> Promise m e a) -> Promise m e a
justAwaitPromise f =
    primitivePromise <| AwaitMsg f


{-| Await both Promises to be completed.
-}
syncPromise : Promise m e a -> Promise m e (a -> b) -> Promise m e b
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
            , requests = effA.requests ++ effF.requests -- reversed
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

                    ( Rejected, _ ) ->
                        Rejected

                    ( _, Rejected ) ->
                        Rejected
            }


{-| Await one of the Promises to be completed.
-}
andRacePromise : Promise m e a -> Promise m e a -> Promise m e a
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
            , requests = eff2.requests ++ eff1.requests -- reversed
            , realCmds = eff1.realCmds ++ eff2.realCmds
            , logs = eff1.logs ++ eff2.logs
            , state =
                case ( eff1.state, eff2.state ) of
                    ( Resolved a1, _ ) ->
                        Resolved a1

                    ( _, Resolved a2 ) ->
                        Resolved a2

                    ( Rejected, res2 ) ->
                        res2

                    ( res1, Rejected ) ->
                        res1

                    ( AwaitMsg nextProm1, AwaitMsg nextProm2 ) ->
                        AwaitMsg <|
                            \msg m ->
                                andRacePromise
                                    (nextProm2 msg m)
                                    (nextProm1 msg m)
            }


{-| Run Promises sequentially.
-}
andThenPromise : (a -> Promise m e b) -> Promise m e a -> Promise m e b
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
                    , requests = effB.requests ++ effA.requests -- reversed
                    , realCmds = effA.realCmds ++ effB.realCmds
                    , logs = effA.logs ++ effB.logs
                    , state = effB.state
                    }

                Rejected ->
                    { newContext = effA.newContext
                    , requests = effA.requests
                    , realCmds = effA.realCmds
                    , logs = effA.logs
                    , state = Rejected
                    }

                AwaitMsg promNextA ->
                    { newContext = effA.newContext
                    , requests = effA.requests
                    , realCmds = effA.realCmds
                    , logs = effA.logs
                    , state =
                        AwaitMsg <|
                            \msg m ->
                                promNextA msg m
                                    |> andThenPromise f
                    }


{-| -}
liftPromiseMemory :
    Pointer_ m m1
    -> Promise m1 e a
    -> Promise m e a
liftPromiseMemory o (Promise prom1) =
    Promise <|
        \context ->
            case o.get context.state of
                Nothing ->
                    let
                        (ThisLayerId expiredLayerId) =
                            o.layerId
                    in
                    { newContext = context
                    , requests = []
                    , realCmds = []
                    , logs =
                        [ LayerExpired expiredLayerId
                        ]
                    , state = Rejected
                    }

                Just state1 ->
                    let
                        eff1 =
                            prom1
                                { state = state1
                                , thisLayerId = o.layerId
                                , nextRequestId = context.nextRequestId
                                , nextLayerId = context.nextLayerId
                                , subs = []
                                }
                    in
                    { newContext =
                        { state = o.set eff1.newContext.state context.state
                        , thisLayerId = context.thisLayerId
                        , nextRequestId = eff1.newContext.nextRequestId
                        , nextLayerId = eff1.newContext.nextLayerId
                        , subs =
                            context.subs
                                ++ List.map
                                    (\f m ->
                                        o.get m
                                            |> Maybe.andThen f
                                    )
                                    eff1.newContext.subs
                        }
                    , requests = eff1.requests
                    , realCmds = eff1.realCmds
                    , logs = eff1.logs
                    , state =
                        case eff1.state of
                            Resolved a ->
                                Resolved a

                            Rejected ->
                                Rejected

                            AwaitMsg nextProm ->
                                AwaitMsg <|
                                    \msg m ->
                                        case o.get m of
                                            Nothing ->
                                                failPromise

                                            Just m1 ->
                                                liftPromiseMemory o (nextProm msg m1)
                    }


{-| -}
liftPromiseEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Promise m e1 a
    -> Promise m e0 a
liftPromiseEvent o (Promise prom1) =
    Promise <|
        \context0 ->
            let
                context1 : Context m e1
                context1 =
                    { state = context0.state
                    , thisLayerId = context0.thisLayerId
                    , nextRequestId = context0.nextRequestId
                    , nextLayerId = context0.nextLayerId
                    , subs = []
                    }

                eff1 =
                    prom1 context1

                newContext1 =
                    eff1.newContext
            in
            { newContext =
                { state = newContext1.state
                , thisLayerId = newContext1.thisLayerId
                , nextRequestId = newContext1.nextRequestId
                , nextLayerId = newContext1.nextLayerId
                , subs =
                    context0.subs
                        ++ List.map
                            (\f m ->
                                f m
                                    |> Maybe.map
                                        (\( rid, sub1 ) -> ( rid, Sub.map (mapMsg o.wrap) sub1 ))
                            )
                            newContext1.subs
                }
            , requests = List.map (\(Request name rid lid cmd) -> Request name rid lid (Cmd.map (mapMsg o.wrap) cmd)) eff1.requests
            , realCmds = List.map (Cmd.map (mapMsg o.wrap)) eff1.realCmds
            , logs = eff1.logs
            , state =
                case eff1.state of
                    Resolved a ->
                        Resolved a

                    Rejected ->
                        Rejected

                    AwaitMsg nextProm1 ->
                        AwaitMsg <|
                            \msg0 m ->
                                liftPromiseEvent o
                                    (nextProm1
                                        (unwrapMsg o.unwrap msg0)
                                        m
                                    )
            }



-- Primitive Promises


{-| -}
none : Promise m e Void
none =
    succeedPromise OnGoingProcedure


{-| -}
sequence : List (Promise m e Void) -> Promise m e Void
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
concurrent : List (Promise m e Void) -> Promise m e Void
concurrent =
    List.foldl
        (\a acc ->
            succeedPromise (\_ _ -> OnGoingProcedure)
                |> syncPromise acc
                |> syncPromise a
        )
        none


{-| -}
currentState : Promise m e m
currentState =
    Promise <|
        \context ->
            { newContext = context
            , requests = []
            , realCmds = []
            , logs = []
            , state = Resolved context.state
            }


{-| -}
genNewLayerId : Promise m e LayerId
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
            , requests = []
            , realCmds = []
            , logs = []
            , state = Resolved newLayerId
            }


{-| -}
type Void
    = OnGoingProcedure


{-| -}
void : Promise m e a -> Promise m e Void
void =
    andThenPromise (\_ -> none)


{-| -}
onGoingProcedure : (PromiseEffect m e Void -> PromiseEffect m e Void) -> Promise m e Void
onGoingProcedure f =
    Promise <|
        \context ->
            f
                { newContext = context
                , requests = []
                , realCmds = []
                , logs = []
                , state = Resolved OnGoingProcedure
                }


{-| -}
modify : (m -> m) -> Promise m e Void
modify f =
    onGoingProcedure <|
        \eff ->
            { eff
                | newContext =
                    let
                        context =
                            eff.newContext
                    in
                    { context
                        | state = f context.state
                    }
            }


{-| -}
push : (m -> Cmd (Msg e)) -> Promise m e Void
push f =
    onGoingProcedure <|
        \eff ->
            { eff | realCmds = f eff.newContext.state :: eff.realCmds }


{-| Cancel all the subsequent Procedures.
-}
cancel : Promise m e Void
cancel =
    primitivePromise Rejected


{-| -}
lazy : (() -> Promise m e Void) -> Promise m e Void
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
    = Layer LayerId m


{-| -}
newLayer :
    { get : (Layer m1 -> Maybe m1) -> m -> Maybe m1
    , modify : (Layer m1 -> Layer m1) -> m -> m
    }
    -> m1
    -> Promise m e ( Layer m1, Pointer m m1 )
newLayer o m1 =
    genNewLayerId
        |> andThenPromise
            (\layerId ->
                let
                    unwrapper : Layer m1 -> Maybe m1
                    unwrapper (Layer layerId_ m1_) =
                        if layerId_ == layerId then
                            Just m1_

                        else
                            Nothing

                    modifier : m1 -> Layer m1 -> Layer m1
                    modifier newM1 (Layer layerId_ oldM1) =
                        if layerId_ == layerId then
                            Layer layerId newM1

                        else
                            Layer layerId_ oldM1
                in
                succeedPromise <|
                    ( Layer layerId m1
                    , Pointer
                        { get =
                            \m ->
                                o.get unwrapper m
                        , set =
                            \newM1 ->
                                o.modify (modifier newM1)
                        , layerId = ThisLayerId layerId
                        }
                    )
            )


{-| -}
onLayer : Pointer m m1 -> Promise m1 e a -> Promise m e a
onLayer (Pointer layer) procs =
    liftPromiseMemory layer procs


{-| -}
layerView : (m -> Html (Msg e)) -> Layer m -> Html (Msg e)
layerView f (Layer layerId m) =
    f m
        |> Html.map
            (\msg ->
                case msg of
                    ViewStubMsg r ->
                        LayerMsg
                            { layerId = layerId
                            , event = r.event
                            }

                    _ ->
                        msg
            )


{-| -}
keyedLayerView : (m -> Html (Msg e)) -> Layer m -> ( String, Html (Msg e) )
keyedLayerView f (Layer layerId m) =
    ( LayerId.toString layerId
    , f m
        |> Html.map
            (\msg ->
                case msg of
                    ViewStubMsg r ->
                        LayerMsg
                            { layerId = layerId
                            , event = r.event
                            }

                    _ ->
                        msg
            )
    )


{-| -}
layerDocument : (m -> Document (Msg e)) -> Layer m -> Document (Msg e)
layerDocument f (Layer layerId m) =
    f m
        |> (\doc ->
                { title = doc.title
                , body =
                    doc.body
                        |> List.map
                            (Html.map
                                (\msg ->
                                    case msg of
                                        ViewStubMsg r ->
                                            LayerMsg
                                                { layerId = layerId
                                                , event = r.event
                                                }

                                        _ ->
                                            msg
                                )
                            )
                }
           )


{-| -}
eventAttr : Attribute event -> Attribute (Msg event)
eventAttr =
    Attributes.map
        (\e -> ViewStubMsg { event = e })


{-| -}
eventMixin : Mixin e -> Mixin (Msg e)
eventMixin =
    Mixin.map
        (\e -> ViewStubMsg { event = e })


type ThisLayerId m
    = ThisLayerId LayerId


{-| -}
type Pointer m m1
    = Pointer (Pointer_ m m1)


type alias Pointer_ m m1 =
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    , layerId : ThisLayerId m1
    }


{-| -}
isPointedBy : Pointer m m1 -> Layer m1 -> Bool
isPointedBy (Pointer pointer) (Layer layerId _) =
    let
        (ThisLayerId pointerLayerId) =
            pointer.layerId
    in
    pointerLayerId == layerId


{-| -}
listen :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Promise m e Void)
    }
    -> Promise m e Void
listen { name, subscription, handler } =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                newContext =
                    { context
                        | nextRequestId = RequestId.inc context.nextRequestId
                        , subs =
                            (\m ->
                                Just
                                    ( myRequestId
                                    , subscription m
                                        |> Sub.map toListenerMsg
                                    )
                            )
                                :: context.subs
                    }

                toListenerMsg e =
                    ListenerMsg
                        { requestId = myRequestId
                        , event = e
                        }

                awaitForever : Msg e -> m -> Promise m e Void
                awaitForever msg _ =
                    case msg of
                        ListenerMsg listenerMsg ->
                            if listenerMsg.requestId == myRequestId then
                                concurrent
                                    [ justAwaitPromise awaitForever
                                    , handler listenerMsg.event
                                        |> sequence
                                    ]

                            else
                                justAwaitPromise awaitForever

                        _ ->
                            justAwaitPromise awaitForever
            in
            { newContext = newContext
            , requests = []
            , realCmds = []
            , logs =
                [ AddListener myRequestId thisLayerId name
                ]
            , state = AwaitMsg awaitForever
            }


{-| -}
listenLayerEvent :
    (e -> List (Promise m e Void))
    -> Promise m e Void
listenLayerEvent handler =
    Promise <|
        \context ->
            let
                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                awaitForever : Msg e -> m -> Promise m e Void
                awaitForever msg _ =
                    case msg of
                        LayerMsg layerMsg ->
                            if layerMsg.layerId == thisLayerId then
                                concurrent
                                    [ justAwaitPromise awaitForever
                                    , handler layerMsg.event
                                        |> sequence
                                    ]

                            else
                                justAwaitPromise awaitForever

                        _ ->
                            justAwaitPromise awaitForever
            in
            { newContext = context
            , requests = []
            , realCmds = []
            , logs = []
            , state = AwaitMsg awaitForever
            }


{-| -}
sleep : Int -> Promise m e Void
sleep msec =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise m e Void
                nextPromise msg _ =
                    case msg of
                        WakeUpMsg wakeUpMsg ->
                            if wakeUpMsg.requestId == myRequestId then
                                succeedPromise OnGoingProcedure

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                }
            , requests = []
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
listenTimeEvery :
    Int
    -> (Posix -> List (Promise m e Void))
    -> Promise m e Void
listenTimeEvery interval handler =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

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

                awaitForever : Msg e -> m -> Promise m e Void
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
            , requests = []
            , realCmds = []
            , logs =
                [ StartTimeEvery myRequestId thisLayerId interval
                ]
            , state = AwaitMsg awaitForever
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
httpRequest : HttpRequest -> Promise m e (Result HttpRequestError ( Http.Metadata, String ))
httpRequest request =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise m e (Result HttpRequestError ( Http.Metadata, String ))
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
            , requests = []
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
httpBytesRequest : HttpRequest -> Promise m e (Result HttpRequestError ( Http.Metadata, Bytes ))
httpBytesRequest request =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise m e (Result HttpRequestError ( Http.Metadata, Bytes ))
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
            , requests = []
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
    { name : String
    , ports :
        { request : Value -> Cmd (Msg e)
        , response : (Value -> Msg e) -> Sub (Msg e)
        }
    , requestBody : Value
    }
    -> Promise m e Value
portRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                responseDecoder : Decoder ( RequestId, Value )
                responseDecoder =
                    JD.map2 Tuple.pair
                        (JD.field "id" RequestId.decoder)
                        (JD.field "body" JD.value)

                nextPromise : Msg e -> m -> Promise m e Value
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
            , requests = []
            , realCmds =
                [ o.ports.request <|
                    JE.object
                        [ ( "id", RequestId.toValue myRequestId )
                        , ( "body", o.requestBody )
                        ]
                ]
            , logs =
                [ IssuePortRequest myRequestId thisLayerId o.requestBody
                ]
            , state = AwaitMsg nextPromise
            }


{-| -}
customRequest :
    { name : String
    , request : (a -> Msg e) -> Cmd (Msg e)
    , responseType : ResponseType a
    }
    -> Promise m e a
customRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise m e a
                nextPromise msg _ =
                    case msg of
                        CustomResponseMsg respMsg ->
                            if respMsg.requestId == myRequestId then
                                case ResponseType.decode o.responseType respMsg.response of
                                    Nothing ->
                                        justAwaitPromise nextPromise

                                    Just a ->
                                        succeedPromise a
                                            |> setLogs
                                                [ ResolveRequest myRequestId
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
            , requests =
                [ Request
                    o.name
                    myRequestId
                    thisLayerId
                    (o.request
                        (\a ->
                            CustomResponseMsg
                                { requestId = myRequestId
                                , response = ResponseType.encode o.responseType a
                                }
                        )
                    )
                ]
            , realCmds = []
            , logs = []
            , state = AwaitMsg nextPromise
            }


{-| -}
anyRequest :
    { name : String
    , request : (a -> Msg e) -> Cmd (Msg e)
    , wrap : a -> e
    , unwrap : e -> Maybe a
    }
    -> Promise m e a
anyRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise m e a
                nextPromise msg _ =
                    case msg of
                        AnyResponseMsg respMsg ->
                            if respMsg.requestId == myRequestId then
                                case o.unwrap respMsg.event of
                                    Nothing ->
                                        justAwaitPromise nextPromise

                                    Just a ->
                                        succeedPromise a
                                            |> setLogs
                                                [ ResolveRequest myRequestId
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
            , requests =
                [ Request
                    o.name
                    myRequestId
                    thisLayerId
                    (o.request
                        (\a ->
                            AnyResponseMsg
                                { requestId = myRequestId
                                , event = o.wrap a
                                }
                        )
                    )
                ]
            , realCmds = []
            , logs = []
            , state = AwaitMsg nextPromise
            }


{-| -}
now : Promise m e Posix
now =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                nextPromise : Msg e -> m -> Promise m e Posix
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
            , requests = []
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
layerEvent : Promise m e e
layerEvent =
    Promise <|
        \context ->
            let
                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                state : Msg e -> m -> Promise m e e
                state msg _ =
                    case msg of
                        LayerMsg r ->
                            if r.layerId /= thisLayerId then
                                justAwaitPromise state

                            else
                                succeedPromise r.event

                        _ ->
                            justAwaitPromise state
            in
            { newContext = context
            , requests = []
            , realCmds = []
            , logs = []
            , state = AwaitMsg state
            }



-- TEA


{-| -}
init :
    m
    -> Promise m e Void
    -> NewState m e
init m prom =
    toModel (initContext m) prom


initContext : m -> Context m e
initContext memory =
    { state = memory
    , thisLayerId = ThisLayerId LayerId.init
    , nextRequestId = RequestId.init
    , nextLayerId = LayerId.inc LayerId.init
    , subs = []
    }


toModel : Context m e -> Promise m e Void -> NewState m e
toModel context (Promise prom) =
    let
        eff =
            prom context
    in
    case eff.state of
        Resolved _ ->
            { nextModel = endOfModel eff.newContext
            , requests = eff.requests
            , realCmds = eff.realCmds
            , logs = eff.logs
            }

        Rejected ->
            { nextModel = endOfModel eff.newContext
            , requests = eff.requests
            , realCmds = eff.realCmds
            , logs = eff.logs
            }

        AwaitMsg nextProm ->
            { nextModel =
                Model
                    { context = eff.newContext
                    , next =
                        \msg nextContext ->
                            toModel
                                nextContext
                                (nextProm msg nextContext.state)
                    }
            , requests = eff.requests
            , realCmds = eff.realCmds
            , logs = eff.logs
            }


{-| -}
update : Msg e -> Model m e -> NewState m e
update msg (Model model) =
    model.next msg model.context


{-| -}
elementView : (Layer memory -> Html (Msg event)) -> Model memory event -> Html (Msg event)
elementView f model =
    f (Layer LayerId.init (memoryState model))


{-| -}
documentView : (Layer memory -> Document (Msg event)) -> Model memory event -> Document (Msg event)
documentView f model =
    f (Layer LayerId.init (memoryState model))


{-| -}
subscriptions : Model memory event -> Sub (Msg event)
subscriptions (Model model) =
    List.filterMap
        (\f ->
            f model.context.state
        )
        model.context.subs
        |> List.map Tuple.second
        |> Sub.batch



-- LayerQuery


{-| -}
type LayerQuery m m1
    = LayerQuery (LayerQuery_ m m1)


type alias LayerQuery_ m m1 =
    { get : Layer m -> List (Layer m1)
    }


{-| -}
runQuery : LayerQuery m m1 -> Model m e -> List (Layer m1)
runQuery (LayerQuery query) model =
    layerState model
        |> query.get
