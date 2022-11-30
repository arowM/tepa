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


## Response Simulators

@docs portResponse
@docs customResponse


# Conditions

@docs fromJust
@docs fromOk

-}

import Browser exposing (Document)
import Dict
import Expect exposing (Expectation)
import Expect.Builder as ExpBuilder
import Internal.Core as Core
import Internal.Markup as Markup
import Json.Encode exposing (Value)
import Mixin
import Mixin.Html as Html exposing (Html)
import Tepa exposing (ApplicationProps, Msg)
import Tepa.AbsolutePath exposing (AbsolutePath)
import Tepa.Scenario.LayerQuery exposing (LayerQuery)
import Tepa.Scenario.Operation exposing (Operation)
import Test exposing (Test)
import Test.Html.Query exposing (Single)
import Test.Sequence as SeqTest


type alias ExpBuilder a =
    ExpBuilder.Builder a



-- Scenario


{-| Scenario describes how the application reacts to the user operations along the time line.

The Scenario you built can be converted to tests with `toTest`, and to documents with `toHtml` or `toMarkdown`.

-}
type alias Scenario flags cmd memory event =
    Core.Scenario flags cmd memory event


{-| A Scenario that does nothing.
-}
none : Scenario flags c m e
none =
    Core.noneScenario


{-| Return a new Scenario that evaluates given Scenarios sequentially.
-}
concat : List (Scenario flags c m e) -> Scenario flags c m e
concat =
    Core.concatScenario



-- Section


{-| Titled Sequence of Scenarios.
-}
type alias Section flags command memory event =
    Core.Section flags command memory event


{-| Constructor for `Section`.

It takes Section title and its sequence of Scenarios.

-}
section : String -> List (Scenario flags c m e) -> Section flags c m e
section =
    Core.section


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
cases =
    Core.cases


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
    Core.Scenario
        { test = Core.noneTest
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph user.name comment
        }


listItemParagraph : String -> String -> Markup.BlockElement
listItemParagraph name content =
    Markup.Paragraph
        [ ( Mixin.none
          , Markup.StrongText name
          )
        , ( Mixin.none
          , Markup.PlainText <|
                ": "
                    ++ content
          )
        ]


{-| System comment.

This Scenario only affects document generation, and is ignored for scenario test generation.

-}
systemComment : Session -> String -> Scenario flags c m e
systemComment (Session session) comment =
    Core.Scenario
        { test = Core.noneTest
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    comment
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
    Core.Scenario
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
                                    |> SeqTest.map (\_ -> Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    description
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
    Core.Scenario
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
                            |> SeqTest.map (\_ -> Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    description
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
    Core.Scenario
        { test =
            \config context ->
                case config.init o.flags (Core.testUrl o.path) of
                    Err err ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ -> Expect.fail err

                    Ok initSessionContext ->
                        Dict.insert session.uniqueName
                            initSessionContext
                            context
                            |> Core.OnGoingTest
                            |> SeqTest.pass
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    description
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
    Core.Scenario
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
                                    resSessionContext =
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
                                case resSessionContext of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> Core.OnGoingTest
                                            |> SeqTest.pass
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] " ++ user.name)
                    description
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
    Core.Scenario
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
                                    resSessionContext =
                                        [ msg
                                        ]
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                }
                                                sessionContext
                                in
                                case resSessionContext of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> Core.OnGoingTest
                                            |> SeqTest.pass
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] " ++ user.name)
                    description
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
    Core.Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "listenerEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case sessionContext.model of
                            Core.OnGoing onGoing ->
                                case Core.runQuery o.target sessionContext.model of
                                    [] ->
                                        Core.OnGoingTest context
                                            |> SeqTest.pass

                                    layer1s ->
                                        let
                                            resSessionContext =
                                                List.concatMap
                                                    (\(Core.Layer thisLid _) ->
                                                        onGoing.listeners
                                                            |> List.filterMap
                                                                (\listener ->
                                                                    if listener.layerId == thisLid && listener.uniqueName == Just o.listenerName then
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
                                        case resSessionContext of
                                            Err err ->
                                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                                    \_ -> Expect.fail err

                                            Ok nextSessionContext ->
                                                Dict.insert session.uniqueName
                                                    nextSessionContext
                                                    context
                                                    |> Core.OnGoingTest
                                                    |> SeqTest.pass

                            Core.EndOfProcess _ ->
                                SeqTest.pass (Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] " ++ o.listenerName)
                    description
        }



-- -- Response Simulators


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
    Core.Scenario
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
                                Core.OnGoingTest context
                                    |> SeqTest.pass

                            layer1s ->
                                let
                                    resSessionContext =
                                        List.concatMap
                                            (\(Core.Layer thisLid _) ->
                                                List.filterMap
                                                    (\( lid, c ) ->
                                                        if lid == thisLid then
                                                            o.response c
                                                                |> Maybe.map
                                                                    (\v ->
                                                                        Core.PortResponseMsg
                                                                            { response = v
                                                                            }
                                                                    )

                                                        else
                                                            Nothing
                                                    )
                                                    sessionContext.cmds
                                            )
                                            layer1s
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                }
                                                sessionContext
                                in
                                case resSessionContext of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> Core.OnGoingTest
                                            |> SeqTest.pass
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "]")
                    description
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
    Core.Scenario
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
                                Core.OnGoingTest context
                                    |> SeqTest.pass

                            layer1s ->
                                let
                                    resSessionContext =
                                        List.concatMap
                                            (\(Core.Layer thisLid _) ->
                                                List.filterMap
                                                    (\( lid, c ) ->
                                                        if lid == thisLid then
                                                            o.response c

                                                        else
                                                            Nothing
                                                    )
                                                    sessionContext.cmds
                                            )
                                            layer1s
                                            |> applyMsgsTo
                                                { onUrlChange = config.onUrlChange
                                                }
                                                sessionContext
                                in
                                case resSessionContext of
                                    Err err ->
                                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                            \_ -> Expect.fail err

                                    Ok nextSessionContext ->
                                        Dict.insert session.uniqueName
                                            nextSessionContext
                                            context
                                            |> Core.OnGoingTest
                                            |> SeqTest.pass
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "]")
                    description
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
            Core.Scenario
                { test =
                    \_ _ ->
                        SeqTest.fail description <|
                            \_ ->
                                ma
                                    |> Expect.notEqual Nothing
                , markup =
                    Core.invalidMarkup <|
                        Core.OtherInvalidMarkup <|
                            "Error: fromJust\n"
                                ++ description
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
            Core.Scenario
                { test =
                    \_ _ ->
                        SeqTest.fail description <|
                            \_ ->
                                res
                                    |> Expect.ok
                , markup =
                    Core.invalidMarkup <|
                        Core.OtherInvalidMarkup <|
                            "Error: fromResult\n"
                                ++ description
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
toTest =
    Core.toTest


applyMsgsTo : { onUrlChange : AbsolutePath -> Msg e } -> Core.SessionContext c m e -> List (Msg e) -> Result String (Core.SessionContext c m e)
applyMsgsTo config sessionContext msgs =
    let
        ( updatedModel, updatedCmds, updatedAppCmds ) =
            List.foldl
                (\msg ( model, cmds, appCms ) ->
                    let
                        ( newModel, newCmds, newAppCmds ) =
                            Core.update msg model
                    in
                    ( newModel, cmds ++ newCmds, appCms ++ newAppCmds )
                )
                ( sessionContext.model, [], [] )
                msgs
    in
    Core.applyAppCmds config
        updatedAppCmds
        { model = updatedModel
        , cmds = updatedCmds
        , history = sessionContext.history
        }



-- Document generation
-- {-| -}
-- toMarkdown =
--     Debug.todo ""
--


{-| Generate scenario document server.
-}
toHtml :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Html ()
toHtml o =
    case Core.toMarkup o of
        Err err ->
            unexpectedReasonHtml err

        Ok sec ->
            Html.div
                [ Mixin.style "margin" "2em"
                ]
                [ Markup.toHtml sec
                ]


unexpectedReasonHtml : Core.InvalidMarkupReason -> Html ()
unexpectedReasonHtml reason =
    Html.div []
        [ Html.text <|
            case reason of
                Core.SiblingScenarioAfterCases ->
                    "ERROR: `cases`\nNo sibling scenarios can be after `cases`."

                Core.OtherInvalidMarkup str ->
                    str
        ]



-- toMarkup : List (Section flags c m e) -> Result UnexpectedScenario Markup.Section
-- toMarkup sections =
--     List.foldr
--         (\sec acc ->
--             Result.map2 (++)
--                 (markupSection [] sec)
--                 acc
--         )
--         (Ok [])
--         sections
--         |> Result.map Markup.Sections
--
--
-- markupSection : List ( Mixin (), Markup.BlockElement ) -> Section flags c m e -> Result UnexpectedScenario (List ( Mixin (), String, Markup.Section ))
-- markupSection inherit (Section r) =
--     let
--         context =
--             markupScenario_ r.title
--                 r.content
--                 { appendSections =
--                     \items ->
--                         [ ( Mixin.none, r.title, Markup.SectionBody [ ( Mixin.none, Markup.ListItems (Mixin.style "list-style-type" "disc") items ) ] ) ]
--                 , listItems = inherit
--                 , error = Nothing
--                 , nextSessionId = 1
--                 }
--     in
--     case context.error of
--         Nothing ->
--             context.listItems
--                 |> List.reverse
--                 |> context.appendSections
--                 |> Ok
--
--         Just err ->
--             Err err
--
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
