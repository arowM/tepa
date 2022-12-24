module Tepa.Scenario exposing
    ( Scenario
    , none
    , concat
    , toTest
    , toHtml
    , Section
    , section
    , cases
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
    -- , toMarkdown
    )

{-| Module for Scenario-Driven Development.


# Core

@docs Scenario
@docs none
@docs concat
@docs toTest
@docs toHtml


# Section

@docs Section
@docs section
@docs cases


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
import Internal.Markup as Markup
import Internal.RequestId exposing (RequestId)
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Html as Html exposing (Html)
import Tepa exposing (ApplicationProps, Msg)
import Tepa.AbsolutePath exposing (AbsolutePath)
import Tepa.Scenario.LayerQuery exposing (LayerQuery)
import Tepa.Scenario.Operation exposing (Operation)
import Test exposing (Test)
import Test.Html.Query exposing (Single)
import Test.Sequence as SeqTest
import Url exposing (Url)


type alias ExpBuilder a =
    ExpBuilder.Builder a



-- Scenario


{-| Scenario describes how the application reacts to the user operations along the time line.

The Scenario you built can be converted to tests with `toTest`, and to documents with `toHtml` or `toMarkdown`.

-}
type Scenario flags cmd memory event
    = Scenario
        { test : TestConfig flags cmd memory event -> TestContext cmd memory event -> SeqTest.Sequence (TestModel cmd memory event)

        -- , markup : MarkupBuilderContext -> MarkupBuilder
        }


type TestModel c m e
    = OnGoingTest (TestContext c m e)
    | TestAfterCases
    | InvalidTest


type alias TestConfig flags c m e =
    { view : m -> Document (Msg e)
    , init : flags -> Url -> Result String (SessionContext c m e)
    , onUrlChange : AbsolutePath -> Msg e
    }


type alias TestContext c m e =
    Dict SessionId (SessionContext c m e)


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



{- Manage sleep operations. -}


type alias Timer =
    { runAfter : Float
    , every : Maybe Float
    , requestId : RequestId
    , layerId : LayerId
    }



{- Manage listen operations. -}


type alias Listener =
    { uniqueName : String
    , requestId : RequestId
    , layerId : LayerId
    }



-- type alias MarkupBuilderContext =
--     List ( Mixin (), Markup.BlockElement ) -> Markup.Section
--
--
-- type MarkupBuilder
--     = OnGoingMarkup MarkupBuilderContext
--     | MarkupAfterCases (List Markup.Section)
--     | InvalidMarkup InvalidMarkupReason
-- {-| -}
-- invalidMarkup : InvalidMarkupReason -> MarkupBuilderContext -> MarkupBuilder
-- invalidMarkup reason _ =
--     InvalidMarkup reason
--
--
-- {-| -}
-- type InvalidMarkupReason
--     = SiblingScenarioAfterCases
--     | OtherInvalidMarkup String


{-| A Scenario that does nothing.
-}
none : Scenario flags c m e
none =
    Scenario
        { test = noneTest

        -- , markup = noneMarkup
        }


{-| -}
noneTest : TestConfig flags c m e -> TestContext c m e -> SeqTest.Sequence (TestModel c m e)
noneTest _ =
    SeqTest.pass >> SeqTest.map OnGoingTest



-- noneMarkup : MarkupBuilderContext -> MarkupBuilder
-- noneMarkup =
--     OnGoingMarkup
-- putListItemMarkup : Markup.BlockElement -> MarkupBuilderContext -> MarkupBuilder
-- putListItemMarkup item context =
--     OnGoingMarkup <|
--         \ls ->
--             context (( Mixin.none, item ) :: ls)


{-| Return a new Scenario that evaluates given Scenarios sequentially.
-}
concat : List (Scenario flags c m e) -> Scenario flags c m e
concat =
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
                        (\m ->
                            case m of
                                OnGoingTest nextContext ->
                                    s2.test config nextContext

                                InvalidTest ->
                                    SeqTest.pass InvalidTest

                                -- just ignore.
                                TestAfterCases ->
                                    SeqTest.fail "Scenario structure" <|
                                        \_ ->
                                            Expect.fail "should not have sibling scenarios after `cases`."
                        )

        -- , markup =
        --     \context ->
        --         case s1.markup context of
        --             MarkupAfterCases _ ->
        --                 InvalidMarkup SiblingScenarioAfterCases
        --             InvalidMarkup reason ->
        --                 InvalidMarkup reason
        --             OnGoingMarkup nextContext ->
        --                 s2.markup nextContext
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
-}
type Section flags command memory event
    = Section
        { title : String
        , content : Scenario flags command memory event
        }


{-| Constructor for `Section`.

It takes Section title and its sequence of Scenarios.

-}
section : String -> List (Scenario flags c m e) -> Section flags c m e
section title scenarios =
    Section
        { title = title
        , content = concat scenarios
        }


{-| You will want to create branches within your scenario. In such cases, you can use cases to branch into multiple scenarios.

    mySection : Section Flags Command Memory Event
    mySection =
        section "Common scenario"
            [ doSomething
            , cases
                [ section "When user chose Goat as their first pet."
                    goatScenario
                , section "When user chose Dog as their first pet."
                    dogScenario
                , section "When user chose Cat as their first pet."
                    catScenario
                ]
            ]

You cannot put Scenarios after `cases`; otherwise document generation and tests fails:

    invalidScenario : Scenario Flags Command Memory Event
    invalidScenario =
        section "Invalid scenario"
            [ doSomething
            , cases someCases
            , youCannotPutAnyScenario
            ]

-}
cases : List (Section flags c m e) -> Scenario flags c m e
cases sections =
    Scenario
        { test =
            \config context ->
                SeqTest.pass ()
                    |> SeqTest.namedCases
                        (\() ->
                            List.map
                                (\(Section sec) ->
                                    let
                                        (Scenario { test }) =
                                            sec.content
                                    in
                                    ( sec.title
                                    , test config context
                                        |> SeqTest.map (\_ -> ())
                                    )
                                )
                                sections
                        )
                    |> SeqTest.map (\_ -> TestAfterCases)

        -- , markup =
        --     \context ->
        --         let
        --             sectionMarkups =
        --                 List.foldl
        --                     (\a acc ->
        --                         Result.map2 (++) acc a
        --                     )
        --                     (Ok [])
        --                     (List.map toMarkupSection sections)
        --         in
        --         case sectionMarkups of
        --             Err err ->
        --                 InvalidMarkup err
        --             Ok markups ->
        --                 MarkupAfterCases <|
        --                     context []
        --                         :: markups
        }


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

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph user.name comment
        }



-- -- listItemParagraph : String -> String -> Markup.BlockElement
-- -- listItemParagraph name content =
-- --     Markup.Paragraph
-- --         [ ( Mixin.none
-- --           , Markup.StrongText name
-- --           )
-- --         , ( Mixin.none
-- --           , Markup.PlainText <|
-- --                 ": "
-- --                     ++ content
-- --           )
-- --         ]


{-| System comment.

This Scenario only affects document generation, and is ignored for scenario test generation.

-}
systemComment : Session -> String -> Scenario flags c m e
systemComment (Session session) comment =
    Scenario
        { test = noneTest

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "] System")
        --             comment
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
                case Dict.get session.uniqueName context of
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
                                    |> SeqTest.map (\_ -> OnGoingTest context)

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "] System")
        --             description
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
                case Dict.get session.uniqueName context of
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
                            |> SeqTest.map (\_ -> OnGoingTest context)

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "] System")
        --             description
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
                        Dict.insert session.uniqueName
                            sessionContext
                            context
                            |> OnGoingTest
                            |> SeqTest.pass

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "] System")
        --             description
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
                case Dict.get session.uniqueName context of
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
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> OnGoingTest
                                            |> SeqTest.pass

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "] " ++ user.name)
        --             description
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
                case Dict.get session.uniqueName context of
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
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> OnGoingTest
                                            |> SeqTest.pass

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "] " ++ user.name)
        --             description
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
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "listenerEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                OnGoingTest context
                                    |> SeqTest.pass

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
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> OnGoingTest
                                            |> SeqTest.pass

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "] " ++ o.listenerName)
        --             description
        }


{-| Wait for given micro seconds.
-}
sleep :
    Session
    -> String
    -> Float
    -> Scenario flags c m e
sleep (Session session) description msec =
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "sleep: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            res =
                                advanceClock { onUrlChange = config.onUrlChange } msec sessionContext
                        in
                        case res of
                            Err err ->
                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                    \_ -> Expect.fail err

                            Ok nextSessionContext ->
                                Dict.insert session.uniqueName
                                    nextSessionContext
                                    context
                                    |> OnGoingTest
                                    |> SeqTest.pass

        -- , markup =
        --     Core.putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "]")
        --             description
        }


advanceClock :
    { onUrlChange : AbsolutePath -> Msg e }
    -> Float
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
                in
                update config
                    (Core.WakeUpMsg { requestId = timer.requestId })
                    { context | timers = newTimers }
                    |> Result.andThen
                        (advanceClock config (msec - timer.runAfter))

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
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "portResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                OnGoingTest context
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
                                                { onUrlChange = config.onUrlChange }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> OnGoingTest
                                            |> SeqTest.pass

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "]")
        --             description
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
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "customResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case Core.runQuery o.target sessionContext.model of
                            [] ->
                                OnGoingTest context
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
                                                }
                                                sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> OnGoingTest
                                            |> SeqTest.pass

        -- , markup =
        --     putListItemMarkup <|
        --         listItemParagraph
        --             ("[" ++ session.uniqueName ++ "]")
        --             description
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

                -- , markup =
                --     invalidMarkup <|
                --         OtherInvalidMarkup <|
                --             "Error: fromJust\n"
                --                 ++ description
                }

        Just a ->
            f a
                |> concat


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

                -- , markup =
                --     invalidMarkup <|
                --         OtherInvalidMarkup <|
                --             "Error: fromResult\n"
                --                 ++ description
                }

        Ok a ->
            f a
                |> concat



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
    List.map
        (\(Section sec) ->
            let
                (Scenario { test }) =
                    sec.content
            in
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
                Dict.empty
                |> SeqTest.run sec.title
        )
        o.sections
        |> Test.describe "Scenario tests"


applyMsgsTo :
    { onUrlChange : AbsolutePath -> Msg e }
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
    { onUrlChange : AbsolutePath -> Msg e }
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
    { onUrlChange : AbsolutePath -> Msg e }
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
    { onUrlChange : AbsolutePath -> Msg e }
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



-- Document generation
-- {-| -}
-- toMarkup :
--     { title : String
--     , sections : List (Section flags c m e)
--     }
--     -> Result InvalidMarkupReason Markup.Section
-- toMarkup o =
--     List.foldl
--         (\a acc ->
--             Result.map2 (++) acc a
--         )
--         (Ok [])
--         (List.map toMarkupSection o.sections)
--         |> Result.map
--             (\children ->
--                 Markup.Section
--                     { title = o.title
--                     , titleMixin = Mixin.none
--                     , body = []
--                     , bodyMixin = Mixin.none
--                     , children = children
--                     }
--             )
--
--
-- toMarkupSection : Section flags c m e -> Result InvalidMarkupReason (List Markup.Section)
-- toMarkupSection (Section sec) =
--     let
--         (Scenario scenario) =
--             sec.content
--
--         initMarkupBuilderContext : MarkupBuilderContext
--         initMarkupBuilderContext items =
--             Markup.Section
--                 { title = sec.title
--                 , titleMixin = Mixin.none
--                 , body =
--                     [ ( Mixin.none, Markup.ListItems Mixin.none items )
--                     ]
--                 , bodyMixin = Mixin.none
--                 , children = []
--                 }
--     in
--     case scenario.markup initMarkupBuilderContext of
--         OnGoingMarkup context ->
--             Ok [ context [] ]
--
--         MarkupAfterCases secs ->
--             Ok secs
--
--         InvalidMarkup reason ->
--             Err reason
--
--
-- -- {-| -}
-- -- toMarkdown =
-- --     Debug.todo ""
-- --
--
--


{-| Generate scenario document server.
-}
toHtml :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Html ()
toHtml =
    Debug.todo ""



-- toHtml o =
--     case toMarkup o of
--         Err err ->
--             unexpectedReasonHtml err
--
--         Ok sec ->
--             Html.div
--                 [ Mixin.style "margin" "2em"
--                 ]
--                 [ Markup.toHtml sec
--                 ]
--
--
-- unexpectedReasonHtml : InvalidMarkupReason -> Html ()
-- unexpectedReasonHtml reason =
--     Html.div []
--         [ Html.text <|
--             case reason of
--                 SiblingScenarioAfterCases ->
--                     "ERROR: `cases`\nNo sibling scenarios can be after `cases`."
--
--                 OtherInvalidMarkup str ->
--                     str
--         ]
--
-- type alias MarkupContext =
--     { appendSections : List ( Mixin (), Markup.BlockElement ) -> List ( Mixin (), String, Markup.Section )
--     , listItems : List ( Mixin (), Markup.BlockElement )
--     , error : Maybe UnexpectedScenario
--     , nextSessionId : Int
--     }
--
--
-- markupScenario_ : String -> Scenario flags c m e -> MarkupContext -> MarkupContext
-- markupScenario_ title scenario context =
--     let
--         appendListItem : String -> String -> MarkupContext
--         appendListItem name content =
--             { context
--                 | listItems =
--                     ( Mixin.none
--                     , Markup.Paragraph
--                         [ ( Mixin.none
--                           , Markup.StrongText name
--                           )
--                         , ( Mixin.none
--                           , Markup.PlainText <|
--                                 ": "
--                                     ++ content
--                           )
--                         ]
--                     )
--                         :: context.listItems
--             }
--     in
--     case scenario of
--         UserComment r ->
--             let
--                 (User user) =
--                     r.user
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem user.name r.comment)
--
--         SystemComment r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.comment
--                 )
--
--         LoadApp r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         UserEvent r ->
--             let
--                 (Session session) =
--                     r.session
--
--                 (User user) =
--                     session.user
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ user.name)
--                     r.description
--                 )
--
--         ListenerEvent r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ r.target)
--                     r.description
--                 )
--
--         ExpectCommands r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         ExpectMemory r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         ExpectAppView r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         PortResponse r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ r.target)
--                     r.description
--                 )
--
--         CustomResponse r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ r.target)
--                     r.description
--                 )
--
--         NextCases r ->
--             let
--                 rnextCases : Result UnexpectedScenario (List ( Mixin (), String, Markup.Section ))
--                 rnextCases =
--                     List.foldr
--                         (\sec acc ->
--                             Result.map2 (++)
--                                 (markupSection
--                                     [ ( Mixin.none
--                                       , Markup.Paragraph
--                                             [ ( Mixin.none
--                                               , Markup.PlainText "(After "
--                                               )
--                                             , ( Mixin.none
--                                               , Markup.EmphasizedText title
--                                               )
--                                             , ( Mixin.none
--                                               , Markup.PlainText ")"
--                                               )
--                                             ]
--                                       )
--                                     ]
--                                     sec
--                                 )
--                                 acc
--                         )
--                         (Ok [])
--                         r.cases
--             in
--             case rnextCases of
--                 Err err ->
--                     { context | error = Just err }
--
--                 Ok nextCases ->
--                     { context
--                         | appendSections =
--                             \_ ->
--                                 (context.listItems
--                                     |> List.reverse
--                                     |> context.appendSections
--                                 )
--                                     ++ nextCases
--                         , listItems = []
--                     }
--
--         Unexpected r ->
--             { context | error = Just r.reason }
--
--         Nil ->
--             context
--
--
-- {-| -}
-- toMarkdown :
--     { title : String
--     , sections : List (Section flags c m e)
--     }
--     -> String
-- toMarkdown _ =
--     "todo"
