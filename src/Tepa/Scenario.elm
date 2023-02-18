module Tepa.Scenario exposing
    ( Scenario
    , none
    , sequence
    , toTest
    , toHtml
    , toMarkdown
    , Section
    , Dependency(..)
    , User
    , defineUser
    , Session
    , defineSession
    , userComment
    , systemComment
    , expectMemory
    , expectAppView
    , loadApp
    , userOperation
    , layerEvent
    , listenerEvent
    , sleep
    , portResponse
    , customResponse
    , fromJust
    , fromOk
    )

{-| Module for Scenario-Driven Development.


# Core

@docs Scenario
@docs none
@docs sequence
@docs toTest
@docs toHtml
@docs toMarkdown
@docs Section
@docs Dependency


# User

@docs User
@docs defineUser


# Session

@docs Session
@docs defineSession


# Primitives


## Comments

@docs userComment
@docs systemComment


## Expectations

@docs expectMemory
@docs expectAppView


## Event Simulators

@docs loadApp
@docs userOperation
@docs layerEvent
@docs listenerEvent
@docs sleep


## Response Simulators

@docs portResponse
@docs customResponse


# Conditions

@docs fromJust
@docs fromOk

-}

import Browser exposing (Document)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Expect.Builder as ExpBuilder
import Internal.AbsolutePath as AbsolutePath
import Internal.Core as Core exposing (Model(..))
import Internal.History as History exposing (History)
import Internal.LayerId as LayerId exposing (LayerId)
import Internal.MarkdownBuilder as MdBuilder
import Internal.RequestId exposing (RequestId)
import Json.Encode exposing (Value)
import Mixin.Html as Html exposing (Html)
import Set
import Tepa exposing (ApplicationProps, Msg)
import Tepa.AbsolutePath exposing (AbsolutePath)
import Tepa.Scenario.LayerQuery exposing (LayerQuery)
import Tepa.Scenario.Operation exposing (Operation)
import Test exposing (Test)
import Test.Html.Query exposing (Single)
import Test.Sequence as SeqTest
import Time exposing (Posix)
import Url exposing (Url)


type alias ExpBuilder a =
    ExpBuilder.Builder a



-- Scenario


{-| Scenario describes how the application reacts to the user operations along the time line.

The Scenario you built can be converted to tests with `toTest`, and to documents with `toHtml` or `toMarkdown`.

-}
type Scenario flags cmd memory event
    = Scenario
        { test : TestConfig flags cmd memory event -> TestContext cmd memory event -> SeqTest.Sequence (TestContext cmd memory event)
        , markup :
            ListBlock -> Result InvalidMarkup ListBlock
        }


type alias ListBlock =
    MdBuilder.Builder
        (MdBuilder.Builder
            (MdBuilder.Builder
                MdBuilder.Root
                MdBuilder.Section
            )
            (MdBuilder.AppendMode MdBuilder.Section)
        )
        MdBuilder.ListBlock


type alias TestConfig flags c m e =
    { view : m -> Document (Msg e)
    , init : flags -> Url -> Result String (SessionContext c m e)
    , onUrlChange : AbsolutePath -> Msg e
    }


type alias TestContext c m e =
    { sessions : Dict SessionId (SessionContext c m e)
    , currentTime : Int -- in milliseconds
    }


type alias SessionId =
    String


type alias SessionContext c m e =
    { model : Model c m e
    , cmds : List ( LayerId, c )
    , requests : List (Core.Request c)
    , timers : List Timer
    , listeners : List Listener
    , history : History
    }


{-| Manage sleep operations.
-}
type alias Timer =
    { runAfter : Int
    , every : Maybe Int
    , requestId : RequestId
    , layerId : LayerId
    }


{-| Manage listen operations.
-}
type alias Listener =
    { uniqueName : String
    , requestId : RequestId
    , layerId : LayerId
    }


{-| -}
type InvalidMarkup
    = InvalidFromJust String
    | InvalidFromOk String
    | NoDependentSection String
    | DuplicatedSection String


{-| A Scenario that does nothing.
-}
none : Scenario flags c m e
none =
    Scenario
        { test = noneTest
        , markup = Ok
        }


{-| -}
noneTest : TestConfig flags c m e -> TestContext c m e -> SeqTest.Sequence (TestContext c m e)
noneTest _ =
    SeqTest.pass


{-| Return a new Scenario that evaluates given Scenarios sequentially.
-}
sequence : List (Scenario flags c m e) -> Scenario flags c m e
sequence =
    List.foldl
        (\a acc ->
            mappend acc a
        )
        none


mappend : Scenario flags c m e -> Scenario flags c m e -> Scenario flags c m e
mappend (Scenario s1) (Scenario s2) =
    Scenario
        { test =
            \config context ->
                s1.test config context
                    |> SeqTest.andThen
                        (s2.test config)
        , markup =
            s1.markup >> Result.andThen s2.markup
        }


testUrl : AbsolutePath -> Url
testUrl (AbsolutePath.AbsolutePath path) =
    { protocol = Url.Https
    , host = "example.com"
    , port_ = Nothing
    , path = path.path
    , query = path.query
    , fragment = path.fragment
    }



-- Section


{-| Titled Sequence of Scenarios.

  - title: Title for the Section
  - content: Sequence of Scenarios for the Section
  - dependency: Dependency of the Section

You may want to branch out in the middle of your scenario.
In such case, you can declare common section, and refer to the title in `dependency` parameter:

    myTest : Test
    myTest =
        toTest
            { props = Debug.todo "props"
            , sections =
                [ commonScenario
                , caseA
                , caseB
                ]
            }

    commonScenario : Section Flags Command Memory Event
    commonScenario =
        { title = "Common scenario"
        , content =
            [ Debug.todo "Common scenarios"
            , Debug.todo "..."
            ]
        , dependency = EntryPoint (Time.millisToPosix 1672531200000)
        }

    caseA : Section Flags Command Memory Event
    caseA =
        { title = "Case A"
        , content =
            -- After common scenario,
            [ Debug.todo "User clicks button A"
            , Debug.todo "..."
            ]
        , dependency = RunAfter commonScenario.title
        }

    caseB : Section Flags Command Memory Event
    caseB =
        { title = "Case B"
        , content =
            -- After common scenario,
            [ Debug.todo "User clicks button B"
            , Debug.todo "..."
            ]
        , dependency = RunAfter commonScenario.title
        }

-}
type alias Section flags command memory event =
    { title : String
    , content : List (Scenario flags command memory event)
    , dependency : Dependency
    }


{-| Dependency of a Section.

  - `EntryPoint time`: Indecates that the Section does not have dependencies, and start at specified `time`.
  - `RunAfter sectionTitle`: Indecates that the Section is after another Section specified with the `sectionTitle`.

-}
type Dependency
    = EntryPoint Posix
    | RunAfter String


{-| An application user.
-}
type User
    = User
        { name : String
        }


{-| Define a user for your Scenario.
-}
defineUser :
    { name : String
    }
    -> User
defineUser =
    User


{-| Session is a unit that connects one application instance and its user. Basically, it corresponds to a tab in a browser.

So, for example, if you want to create a scenario where a user opens and operates two tabs, you need to define two separate sessions for the same user:

    sakuraChan : User
    sakuraChan =
        defineUser
            { name = "Sakura-chan"
            }

    sakuraChanMainSession : Session
    sakuraChanMainSession =
        defineSession
            { user = sakuraChan
            , name = "Main tab on the Sakura-chan's machine"
            }

    sakuraChanSecondSession : Session
    sakuraChanSecondSession =
        defineSession
            { user = sakuraChan
            , name = "Second tab on the Sakura-chan's machine"
            }

-}
type Session
    = Session
        { user : User
        , uniqueName : String
        }


{-| Define a session for your Scenario.
-}
defineSession :
    { uniqueName : String
    , user : User
    }
    -> Session
defineSession =
    Session



-- Primitives
-- -- Comments


{-| User comment.

    myScenario =
        [ userComment sakuraChan
            "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan
            "Today I'll try a goat management service."
        , Debug.todo "..."
        ]

This Scenario only affects document generation, and is ignored for scenario test generation.

You can start with `userComment` and `systemComment` to build the skeleton of your scenario, and gradually replace `userComment` with Event Simulator and `systemComment` with Expectation.

-}
userComment : User -> String -> Scenario flags c m e
userComment (User user) comment =
    Scenario
        { test = noneTest
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis user.name
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText comment
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


{-| System comment.

This Scenario only affects document generation, and is ignored for scenario test generation.

-}
systemComment : Session -> String -> Scenario flags c m e
systemComment (Session session) comment =
    Scenario
        { test = noneTest
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "] System")
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText comment
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }



-- -- Expectations


{-| Describe your expectations for the application memory state at the point.

Suppose your application has a counter:

    import Expect.Builder as ExpBuilder

    myScenario =
        [ Debug.todo "After some operations..."
        , expectMemory sakuraChanMainSession
            "Requests user information to the server."
            { expectation =
                ExpBuilder.partial .counter <|
                    ExpBuilder.lessThan 4
            }
        , Debug.todo "..."
        ]

You use [elm-expectation-builder]() to describe your expectation flexibly.

-}
expectMemory :
    Session
    -> String
    ->
        { target : LayerQuery m m1
        , expectation : ExpBuilder (List m1)
        }
    -> Scenario flags c m e
expectMemory (Session session) description o =
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "expectMemory: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                    \_ ->
                                        Err (Core.memoryState sessionContext.model)
                                            |> Expect.equal
                                                (Ok "expectMemory: The query should find some Layer in the current memory.")

                            layer1s ->
                                List.map (\(Core.Layer _ m1) -> m1) layer1s
                                    |> SeqTest.pass
                                    |> SeqTest.assert description
                                        (ExpBuilder.applyTo o.expectation)
                                    |> SeqTest.map (\_ -> context)
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "] System")
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


{-| Describe your expectations for the application's view at the point.

Suppose your application has a popup:

    import Html.Attribute exposing (attribute)
    import Test.Html.Query as Query
    import Test.Html.Selector as Selector

    myScenario =
        [ Debug.todo "After some operations..."
        , expectAppView sakuraChanMainSession
            "Show popup message."
            { expectation =
                \{ body } ->
                    Query.fromHtml (Html.div [] body)
                        |> Query.find [ Selector.id "popup" ]
                        |> Query.has
                            [ Selector.attribute
                                (attribute "aria-hidden" "false")
                            ]
            }
        , Debug.todo "..."
        ]

You use [elm-expectation-builder]() to describe your expectation flexibly.

Note that the `expectation` field takes page whole view even if you use it in `onLayer` function.

    onLayer popup
        [ expectAppView sakuraChanMainSession
            "expectation about the whole application view"
            { expectation =
                \html ->
                    Debug.todo
                        "the argument is not the partial view for the Layer, but for the whole page."
            }
        , Debug.todo "..."
        ]

-}
expectAppView :
    Session
    -> String
    ->
        { expectation : Document (Msg event) -> Expectation
        }
    -> Scenario flags c m event
expectAppView (Session session) description { expectation } =
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "expectAppView: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        SeqTest.pass
                            (Core.memoryState sessionContext.model
                                |> config.view
                            )
                            |> SeqTest.assert description expectation
                            |> SeqTest.map (\_ -> context)
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "] System")
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }



-- -- Event Simulators


{-| Load the app. You can also use `loadApp` to reload the app.

    import Json.Encode as JE
    import Tepa.AbsolutePath exposing (absolutePath)

    myScenario =
        [ userComment sakuraChan
            "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan
            "I'll open the home page..."
        , loadApp sakuraChanMainSession
            "Load the home page."
            { path = absolutePath [] [] Nothing
            , flags =
                JE.object []
            }
        , systemComment sakuraChanMainSession
            "Show home page."
        , userComment sakuraChan
            "Oops, I accidentally hit the F5 button..."
        , loadApp sakuraChanMainSession
            "Reload the page."
            { path = absolutePath [] [] Nothing
            , flags =
                JE.object []
            }
        , Debug.todo "..."
        ]

-}
loadApp :
    Session
    -> String
    ->
        { path : AbsolutePath
        , flags : flags
        }
    -> Scenario flags c m e
loadApp (Session session) description o =
    Scenario
        { test =
            \config context ->
                case config.init o.flags (testUrl o.path) of
                    Err err ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ -> Expect.fail err

                    Ok sessionContext ->
                        { context
                            | sessions =
                                Dict.insert session.uniqueName
                                    sessionContext
                                    context.sessions
                        }
                            |> SeqTest.pass
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "] System")
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


{-| Publish an event to its Layer.

Suppose your application has a popup:

    myScenario =
        [ Debug.todo "After some operations..."
        , onLayer popup
            [ layerEvent sakuraChanMainSession
                "Click cancel button."
                { event = ClickPopupCancelButton
                }
            ]
        , Debug.todo "..."
        ]

The example above publishes `ClickPopupCancelButton` event to the LayerId for the `popup` Layer.

-}
layerEvent :
    Session
    -> String
    ->
        { target : LayerQuery m m1
        , event : event
        }
    -> Scenario flags c m event
layerEvent (Session session) description o =
    let
        (User user) =
            session.user
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "layerEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                    \_ ->
                                        Expect.fail
                                            "layerEvent: No Layers for the query."

                            layer1s ->
                                let
                                    res =
                                        List.map
                                            (\(Core.Layer lid _) ->
                                                Core.LayerMsg
                                                    { layerId = lid
                                                    , event = o.event
                                                    }
                                            )
                                            layer1s
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        { context
                                            | sessions =
                                                Dict.insert session.uniqueName
                                                    nextSessionContext
                                                    context.sessions
                                        }
                                            |> SeqTest.pass
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "] " ++ user.name)
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


{-| -}
userOperation :
    Session
    -> String
    ->
        { target : Single (Msg e) -> Single (Msg e)
        , operation : Operation e
        }
    -> Scenario flags c m e
userOperation (Session session) description o =
    let
        (User user) =
            session.user
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "userOperation: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            rmsg =
                                Core.memoryState sessionContext.model
                                    |> config.view
                                    |> .body
                                    |> Html.div []
                                    |> Test.Html.Query.fromHtml
                                    |> o.target
                                    |> Core.runOperation o.operation
                        in
                        case rmsg of
                            Err str ->
                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                    \_ ->
                                        Expect.fail
                                            ("userOperation: " ++ str)

                            Ok msg ->
                                let
                                    res =
                                        [ msg
                                        ]
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        { context
                                            | sessions =
                                                Dict.insert session.uniqueName
                                                    nextSessionContext
                                                    context.sessions
                                        }
                                            |> SeqTest.pass
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "] " ++ user.name)
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


{-| Publish an event to a Listner.

Suppose your application has a WebSocket message Listener named "WebSocket message Listener":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After some operations..."
        , listenerEvent sakuraChanMainSession
            "Receive WebSocket message"
            { target = "WebSocket message Listener"
            , event =
                WebSocketMessage <|
                    JE.object
                        [ ( "action", JE.string "connected" )
                        ]
            }
        , Debug.todo "..."
        ]

If no Layers found for the query, it does nothing and just passes the test.

-}
listenerEvent :
    Session
    -> String
    ->
        { target : LayerQuery m m1
        , listenerName : String
        , event : event
        }
    -> Scenario flags c m event
listenerEvent (Session session) description o =
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "listenerEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                SeqTest.pass context

                            layer1s ->
                                let
                                    res =
                                        List.concatMap
                                            (\(Core.Layer thisLid _) ->
                                                sessionContext.listeners
                                                    |> List.filterMap
                                                        (\listener ->
                                                            if listener.layerId == thisLid && listener.uniqueName == o.listenerName then
                                                                Just <|
                                                                    Core.ListenerMsg
                                                                        { requestId = listener.requestId
                                                                        , event = o.event
                                                                        }

                                                            else
                                                                Nothing
                                                        )
                                            )
                                            layer1s
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        { context
                                            | sessions =
                                                Dict.insert session.uniqueName
                                                    nextSessionContext
                                                    context.sessions
                                        }
                                            |> SeqTest.pass
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "] " ++ o.listenerName)
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


{-| Wait for given micro seconds.
-}
sleep :
    Session
    -> String
    -> Int
    -> Scenario flags c m e
sleep (Session session) description msec =
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "sleep: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            res =
                                advanceClock
                                    { onUrlChange = config.onUrlChange
                                    , currentTime = context.currentTime
                                    }
                                    msec
                                    sessionContext
                        in
                        case res of
                            Err err ->
                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                    \_ -> Expect.fail err

                            Ok nextSessionContext ->
                                { context
                                    | sessions =
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context.sessions
                                    , currentTime = context.currentTime + msec
                                }
                                    |> SeqTest.pass
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "]")
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


advanceClock :
    { onUrlChange : AbsolutePath -> Msg e
    , currentTime : Int
    }
    -> Int
    -> SessionContext c m e
    -> Result String (SessionContext c m e)
advanceClock config msec context =
    case context.timers of
        [] ->
            Ok context

        timer :: timers ->
            if timer.runAfter <= msec then
                let
                    newTimers =
                        case timer.every of
                            Nothing ->
                                List.map
                                    (\t -> { t | runAfter = t.runAfter - timer.runAfter })
                                    timers

                            Just interval ->
                                List.map
                                    (\t -> { t | runAfter = t.runAfter - timer.runAfter })
                                    timers
                                    |> putTimer
                                        { timer | runAfter = interval }

                    newConfig =
                        { config
                            | currentTime = config.currentTime + timer.runAfter
                        }
                in
                update newConfig
                    (Core.WakeUpMsg { requestId = timer.requestId })
                    { context | timers = newTimers }
                    |> Result.andThen
                        (advanceClock newConfig (msec - timer.runAfter))

            else
                Ok
                    { context
                        | timers =
                            List.map
                                (\t -> { t | runAfter = t.runAfter - msec })
                                context.timers
                    }



-- Response Simulators


{-| Simulate response to the `Tepa.portRequest`.

Suppose your application requests to access localStorage via port request named "Port to get page.account.bio":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After request to the port..."
        , portResponse sakuraChanMainSession
            "Received response."
            { target = "Port to get page.account.bio"
            , response =
                JE.string "I'm Sakura-chan."
            }
        , Debug.todo "..."
        ]

If no Layers found for the query, it does nothing and just passes the test.

-}
portResponse :
    Session
    -> String
    ->
        { target : LayerQuery m m1
        , response : command -> Maybe Value
        }
    -> Scenario flags command m e
portResponse (Session session) description o =
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "portResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                context
                                    |> SeqTest.pass

                            layer1s ->
                                let
                                    res =
                                        List.concatMap
                                            (\(Core.Layer lid_ _) ->
                                                List.filterMap
                                                    (\(Core.Request _ lid c) ->
                                                        if lid == lid_ then
                                                            o.response c
                                                                |> Maybe.map
                                                                    (\v -> Core.PortResponseMsg { response = v })

                                                        else
                                                            Nothing
                                                    )
                                                    sessionContext.requests
                                            )
                                            layer1s
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        { context
                                            | sessions =
                                                Dict.insert session.uniqueName
                                                    nextSessionContext
                                                    context.sessions
                                        }
                                            |> SeqTest.pass
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "]")
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }


{-| Simulate response to the `Tepa.customRequest`.

Suppose your application requests user infomation to the backend server via custom request named "Request for user info":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After request to the backend..."
        , portResponse sakuraChanMainSession
            "Received response."
            { target = "Request for user info"
            , response =
                UserInfoResponse <|
                    Ok
                        { name = "Sakura-chan"
                        , age = 3
                        }
            }
        , Debug.todo "..."
        ]

If no Layers found for the query, it does nothing and just passes the test.

-}
customResponse :
    Session
    -> String
    ->
        { target : LayerQuery m m1
        , response : command -> Maybe (Msg event)
        }
    -> Scenario flags command m event
customResponse (Session session) description o =
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "customResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                context
                                    |> SeqTest.pass

                            layer1s ->
                                let
                                    res =
                                        List.concatMap
                                            (\(Core.Layer lid_ _) ->
                                                List.filterMap
                                                    (\(Core.Request _ lid c) ->
                                                        if lid == lid_ then
                                                            o.response c

                                                        else
                                                            Nothing
                                                    )
                                                    sessionContext.requests
                                            )
                                            layer1s
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        { context
                                            | sessions =
                                                Dict.insert session.uniqueName
                                                    nextSessionContext
                                                    context.sessions
                                        }
                                            |> SeqTest.pass
        , markup =
            MdBuilder.appendListItem
                >> MdBuilder.editListItemContent
                >> MdBuilder.pushStrongEmphasis
                    ("[" ++ session.uniqueName ++ "]")
                >> MdBuilder.pushText ": "
                >> MdBuilder.pushText description
                >> MdBuilder.endPushMode
                >> MdBuilder.break
                >> Ok
        }



-- Conditions


{-| Extract `Just` value.

If the given value is `Nothing`, document generation and tests fails.

    import Url

    myScenario =
        [ Debug.todo "After some operations..."
        , fromJust "Make URL"
            (Url.fromString "https://example.com/foo/")
          <|
            \url ->
                [ Debug.todo "Scenarios that use `url`"
                ]
        , Debug.todo "..."
        ]

-}
fromJust : String -> Maybe a -> (a -> List (Scenario flags c m e)) -> Scenario flags c m e
fromJust description ma f =
    case ma of
        Nothing ->
            Scenario
                { test =
                    \_ _ ->
                        SeqTest.fail description <|
                            \_ ->
                                ma
                                    |> Expect.notEqual Nothing
                , markup =
                    \_ ->
                        Err (InvalidFromJust description)
                }

        Just a ->
            f a
                |> sequence


{-| Similar to `fromJust`, but extract `Ok` valur from `Result`.
-}
fromOk : String -> Result err a -> (a -> List (Scenario flags c m e)) -> Scenario flags c m e
fromOk description res f =
    case res of
        Err _ ->
            Scenario
                { test =
                    \_ _ ->
                        SeqTest.fail description <|
                            \_ ->
                                res
                                    |> Expect.ok
                , markup =
                    \_ ->
                        Err (InvalidFromOk description)
                }

        Ok a ->
            f a
                |> sequence



-- Test


{-| Generate scenario tests.
-}
toTest :
    { props : ApplicationProps flags cmd memory event
    , sections : List (Section flags cmd memory event)
    }
    -> Test
toTest o =
    let
        onUrlChange path =
            Core.LayerMsg
                { layerId = LayerId.init
                , event =
                    o.props.onUrlChange (testUrl path)
                }
    in
    List.foldl
        (\sec acc ->
            let
                (Scenario { test }) =
                    sequence sec.content
            in
            SeqTest.andThen
                (\cache ->
                    (case sec.dependency of
                        EntryPoint initialTime ->
                            SeqTest.pass
                                { sessions = Dict.empty
                                , currentTime = Time.posixToMillis initialTime
                                }

                        RunAfter title ->
                            case Dict.get title cache of
                                Nothing ->
                                    SeqTest.fail ("No dependent section: " ++ "\"" ++ title ++ "\"") <|
                                        \() ->
                                            Expect.fail
                                                "Declare dependent section beforehand."

                                Just context ->
                                    SeqTest.pass context
                    )
                        |> SeqTest.andThen
                            (\context ->
                                test
                                    { view =
                                        \m ->
                                            let
                                                document =
                                                    o.props.view (Core.Layer LayerId.init m)
                                            in
                                            { title = document.title
                                            , body = document.body
                                            }
                                    , init =
                                        \flags url ->
                                            let
                                                newState =
                                                    Core.init o.props.init (o.props.procedure flags url Core.SimKey)
                                            in
                                            applyLogs
                                                { onUrlChange = onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                newState.logs
                                                { model = newState.nextModel
                                                , cmds = newState.cmds
                                                , requests = newState.requests
                                                , timers = []
                                                , listeners = []
                                                , history =
                                                    History.init <| AbsolutePath.fromUrl url
                                                }
                                    , onUrlChange = onUrlChange
                                    }
                                    context
                                    |> SeqTest.describe sec.title
                            )
                        |> SeqTest.map
                            (\res -> Dict.insert sec.title res cache)
                )
                acc
        )
        (SeqTest.pass Dict.empty)
        o.sections
        |> SeqTest.run "Scenario tests"


applyMsgsTo :
    { onUrlChange : AbsolutePath -> Msg e
    , currentTime : Int
    }
    -> SessionContext c m e
    -> List (Msg e)
    -> Result String (SessionContext c m e)
applyMsgsTo config context =
    List.foldl
        (\msg acc ->
            Result.andThen
                (update config msg)
                acc
        )
        (Ok context)


update :
    { onUrlChange : AbsolutePath -> Msg e
    , currentTime : Int
    }
    -> Msg e
    -> SessionContext c m e
    -> Result String (SessionContext c m e)
update config msg context =
    let
        newState =
            Core.update msg context.model
    in
    { model = newState.nextModel
    , cmds = newState.cmds
    , requests = newState.requests ++ context.requests -- reversed
    , timers = context.timers
    , listeners = context.listeners
    , history = context.history
    }
        |> applyLogs config newState.logs


applyLogs :
    { onUrlChange : AbsolutePath -> Msg e
    , currentTime : Int
    }
    -> List Core.Log
    -> SessionContext c m e
    -> Result String (SessionContext c m e)
applyLogs config logs context =
    List.foldl
        (\log acc ->
            Result.andThen
                (applyLog config log)
                acc
        )
        (Ok context)
        logs


applyLog :
    { onUrlChange : AbsolutePath -> Msg e
    , currentTime : Int
    }
    -> Core.Log
    -> SessionContext c m e
    -> Result String (SessionContext c m e)
applyLog config log context =
    case log of
        Core.SetTimer rid lid msec ->
            Ok
                { context
                    | timers =
                        putTimer
                            { runAfter = msec
                            , every = Nothing
                            , requestId = rid
                            , layerId = lid
                            }
                            context.timers
                }

        Core.StartTimeEvery rid lid msec ->
            Ok
                { context
                    | timers =
                        putTimer
                            { runAfter = msec
                            , every = Just msec
                            , requestId = rid
                            , layerId = lid
                            }
                            context.timers
                }

        Core.RequestCurrentTime rid ->
            update config
                (Core.CurrentTimeMsg
                    { requestId = rid
                    , timestamp = Time.millisToPosix config.currentTime
                    }
                )
                context

        Core.AddListener rid lid name ->
            Ok
                { context
                    | listeners =
                        { uniqueName = name
                        , requestId = rid
                        , layerId = lid
                        }
                            :: context.listeners

                    -- reversed
                }

        Core.ResolvePortRequest rid ->
            Ok
                { context
                    | requests =
                        List.filter
                            (\(Core.Request rid_ _ _) ->
                                rid_ /= rid
                            )
                            context.requests
                    , listeners =
                        List.filter
                            (\listener ->
                                listener.requestId /= rid
                            )
                            context.listeners
                }

        Core.ResolveRequest rid ->
            Ok
                { context
                    | requests =
                        List.filter
                            (\(Core.Request rid_ _ _) ->
                                rid_ /= rid
                            )
                            context.requests
                }

        Core.LayerExpired lid ->
            Ok
                { context
                    | requests =
                        List.filter
                            (\(Core.Request _ lid_ _) ->
                                lid_ /= lid
                            )
                            context.requests
                    , timers =
                        List.filter
                            (\timer ->
                                timer.layerId /= lid
                            )
                            context.timers
                    , listeners =
                        List.filter
                            (\listener ->
                                listener.layerId /= lid
                            )
                            context.listeners
                }

        Core.PushPath path ->
            update config
                (config.onUrlChange path)
                { context
                    | history = History.pushPath path context.history
                }

        Core.ReplacePath path ->
            update config
                (config.onUrlChange path)
                { context
                    | history = History.replacePath path context.history
                }

        Core.Back steps ->
            case History.back steps context.history of
                Nothing ->
                    Err
                        "back: Scenario test does not support navigation to pages outside of the application."

                Just newHistory ->
                    update config
                        (config.onUrlChange <| History.current newHistory)
                        { context
                            | history = newHistory
                        }

        Core.Forward steps ->
            case History.forward steps context.history of
                Nothing ->
                    Err
                        "forward: Scenario test does not support navigation to pages outside of the application."

                Just newHistory ->
                    update config
                        (config.onUrlChange <| History.current newHistory)
                        { context
                            | history = newHistory
                        }


putTimer : Timer -> List Timer -> List Timer
putTimer new timers =
    case timers of
        [] ->
            [ new ]

        t :: ts ->
            if new.runAfter <= t.runAfter then
                new :: t :: ts

            else
                t :: putTimer new ts


{-| Generate scenario document server.
-}
toHtml :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Html msg
toHtml o =
    case buildMarkdown o of
        Err err ->
            renderInvalidMarkdown err

        Ok root ->
            MdBuilder.preview root


renderInvalidMarkdown : InvalidMarkup -> Html msg
renderInvalidMarkdown reason =
    case reason of
        InvalidFromJust str ->
            Html.div
                []
                [ Html.p
                    []
                    [ Html.text "Error: fromJust"
                    ]
                , Html.p
                    []
                    [ Html.text str
                    ]
                ]

        InvalidFromOk str ->
            Html.div
                []
                [ Html.p
                    []
                    [ Html.text "Error: fromOk"
                    ]
                , Html.p
                    []
                    [ Html.text str
                    ]
                ]

        NoDependentSection name ->
            Html.div
                []
                [ Html.p
                    []
                    [ Html.text "Error"
                    ]
                , Html.p
                    []
                    [ Html.text <| "No dependent section: " ++ "\"" ++ name ++ "\""
                    ]
                , Html.p
                    []
                    [ Html.text "Declare dependent section beforehand."
                    ]
                ]

        DuplicatedSection name ->
            Html.div
                []
                [ Html.p
                    []
                    [ Html.text "Error"
                    ]
                , Html.p
                    []
                    [ Html.text <| "Multiple sections with the same title: " ++ "\"" ++ name ++ "\""
                    ]
                ]


{-| Generate scenario document markdown text.
-}
toMarkdown :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Result InvalidMarkup String
toMarkdown o =
    buildMarkdown o
        |> Result.map MdBuilder.toString


buildMarkdown :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Result InvalidMarkup MdBuilder.Root
buildMarkdown o =
    List.foldl
        (\sec acc ->
            let
                (Scenario scenario) =
                    sequence sec.content
            in
            acc
                |> Result.andThen
                    (\( builder, titles ) ->
                        if Set.member sec.title titles then
                            Err (DuplicatedSection sec.title)

                        else
                            builder
                                |> MdBuilder.appendChildSection
                                    { title = sec.title
                                    }
                                |> MdBuilder.editBody
                                |> MdBuilder.appendUnorderedList
                                |> (case sec.dependency of
                                        EntryPoint initialTime ->
                                            MdBuilder.appendListItem
                                                >> MdBuilder.editListItemContent
                                                >> MdBuilder.pushText
                                                    ("At " ++ String.fromInt (Time.posixToMillis initialTime) ++ " in POSIX time.")
                                                >> MdBuilder.break
                                                >> scenario.markup

                                        RunAfter dep ->
                                            if Set.member dep titles then
                                                MdBuilder.appendListItem
                                                    >> MdBuilder.editListItemContent
                                                    >> MdBuilder.pushText
                                                        "After "
                                                    >> MdBuilder.pushLink
                                                        { href = "#" ++ Url.percentEncode dep
                                                        , text = dep
                                                        }
                                                    >> MdBuilder.break
                                                    >> scenario.markup

                                            else
                                                \_ -> Err (NoDependentSection dep)
                                   )
                                >> Result.map
                                    (\a ->
                                        ( a
                                            |> MdBuilder.break
                                            |> MdBuilder.break
                                        , Set.insert sec.title titles
                                        )
                                    )
                    )
        )
        (( MdBuilder.root
            { title = o.title
            }
         , Set.empty
         )
            |> Ok
        )
        o.sections
        |> Result.map (Tuple.first >> MdBuilder.run)
