module Tepa.Scenario exposing
    ( Scenario
    , none
    , sequence
    , toTest
    , toHtml
    , toMarkdown
    , InvalidMarkup(..)
    , Section
    , Dependency(..)
    , User
    , defineUser
    , Session
    , defineSession
    , Markup
    , textContent
    , userComment
    , systemComment
    , comment
    , expectMemory
    , expectAppView
    , loadApp
    , userOperation
    , layerEvent
    , listenerEvent
    , sleep
    , httpResponse
    , httpBytesResponse
    , HttpRequest
    , HttpRequestBody(..)
    , portResponse
    , customResponse
    , fromJust
    , fromOk
    , RenderConfig
    , en_US
    , ja_JP
    )

{-| Module for Scenario-Driven Development.


# Core

@docs Scenario
@docs none
@docs sequence
@docs toTest
@docs toHtml
@docs toMarkdown
@docs InvalidMarkup
@docs Section
@docs Dependency


# User

@docs User
@docs defineUser


# Session

@docs Session
@docs defineSession


# Markup

@docs Markup
@docs textContent


# Primitives


## Comments

@docs userComment
@docs systemComment
@docs comment


## Expectations

@docs expectMemory
@docs expectAppView


## Event Simulators

@docs loadApp
@docs userOperation
@docs layerEvent
@docs listenerEvent
@docs sleep


## Http response Simulators

@docs httpResponse
@docs httpBytesResponse
@docs HttpRequest
@docs HttpRequestBody


## Response Simulators

@docs portResponse
@docs customResponse


# Conditions

@docs fromJust
@docs fromOk


# RenderConfig

@docs RenderConfig
@docs en_US
@docs ja_JP

-}

import Browser exposing (Document)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import File exposing (File)
import Http
import Internal.AbsolutePath as AbsolutePath
import Internal.Core as Core exposing (Model(..))
import Internal.History as History exposing (History)
import Internal.LayerId as LayerId exposing (LayerId)
import Internal.RequestId exposing (RequestId)
import Json.Encode exposing (Value)
import MarkdownAst as MdAst
import MarkdownBuilder as MdBuilder
import Mixin.Html as Html exposing (Html)
import Set
import Tepa exposing (ApplicationProps, Layer, Msg)
import Tepa.AbsolutePath exposing (AbsolutePath)
import Test exposing (Test)
import Test.Html.Event as TestEvent
import Test.Html.Query exposing (Single)
import Test.Sequence as SeqTest
import Time exposing (Posix)
import TimeZone
import Url exposing (Url)



-- Scenario


{-| Scenario describes how the application reacts to the user operations along the time line.

The Scenario you built can be converted to tests with `toTest`, and to documents with `toHtml` or `toMarkdown`.

-}
type Scenario flags memory event
    = Scenario
        { test : TestConfig flags memory event -> TestContext memory event -> SeqTest.Sequence (TestContext memory event)
        , markup :
            RenderConfig -> ListBlock -> Result InvalidMarkup ListBlock
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


type alias TestConfig flags m e =
    { view : m -> Document (Msg e)
    , init : flags -> Url -> Result String (SessionContext m e)
    , onUrlChange : AbsolutePath -> Msg e
    }


{-| -}
type alias TestContext m e =
    { sessions : Dict String (SessionContext m e)
    , currentTime : Int -- in milliseconds
    }


{-| -}
type alias SessionContext m e =
    { model : Model m e
    , requests : List (Core.Request e) -- reversed
    , portRequests : List ( ( RequestId, LayerId ), Value ) -- reversed
    , httpRequests : List ( ( RequestId, LayerId ), Core.HttpRequest ) -- reversed
    , timers : List (Timer e)
    , listeners : List Listener -- reversed
    , history : History
    }


{-| Manage timeout operations.
-}
type alias Timer e =
    { runAfter : Int
    , every : Maybe Int
    , msg : Posix -> Core.Msg e
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
none : Scenario flags m e
none =
    Scenario
        { test = noneTest
        , markup = \_ -> Ok
        }


{-| -}
noneTest : TestConfig flags m e -> TestContext m e -> SeqTest.Sequence (TestContext m e)
noneTest _ =
    SeqTest.pass


{-| Return a new Scenario that evaluates given Scenarios sequentially.
-}
sequence : List (Scenario flags m e) -> Scenario flags m e
sequence =
    List.foldl
        (\a acc ->
            mappend acc a
        )
        none


mappend : Scenario flags m e -> Scenario flags m e -> Scenario flags m e
mappend (Scenario s1) (Scenario s2) =
    Scenario
        { test =
            \config context ->
                s1.test config context
                    |> SeqTest.andThen
                        (s2.test config)
        , markup =
            \config ->
                s1.markup config >> Result.andThen (s2.markup config)
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


{-| Titled sequence of Scenarios.

  - title: Title for the Section, which must be unique string
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
type alias Section flags memory event =
    { title : String
    , content : List (Scenario flags memory event)
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
userComment : User -> String -> Scenario flags m e
userComment (User user) commentText =
    comment
        { content =
            [ MdAst.StrongEmphasis user.name
            , MdAst.PlainText <| ": " ++ commentText
            ]
        , detail = []
        , appear = True
        }


{-| System comment.

This Scenario only affects document generation, and is ignored for scenario test generation.

-}
systemComment : Session -> String -> Scenario flags m e
systemComment (Session session) commentText =
    comment
        { content =
            [ MdAst.StrongEmphasis <|
                "["
                    ++ session.uniqueName
                    ++ "]"
            , MdAst.PlainText " "
            , MdAst.StrongEmphasis "System"
            , MdAst.PlainText <| ": " ++ commentText
            ]
        , detail = []
        , appear = True
        }


{-| Lower level function to add detailed comments.
-}
comment : Markup -> Scenario flags m e
comment markup =
    Scenario
        { test = noneTest
        , markup =
            \_ ->
                MdBuilder.appendListItem markup.content
                    >> MdBuilder.appendBlocks markup.detail
                    >> MdBuilder.break
                    >> Ok
        }


{-| Represents markup for a scenario.

Suppose you have the following `Markup`, which uses [arowM/elm-markdown-ast](https://package.elm-lang.org/packages/arowM/elm-markdown-ast/latest/):

    import MarkdownAst as Markdown

    sample : Markup
    sample =
        { content =
            [ Markdown.InlineCode "content"
            , Markdown.PlainText " for the list item"
            ]
        , detail =
            [ Markdown.ParagraphBlock
                [ Markdown.InlineCode "detail"
                , Markdown.PlainText " for the list item"
                ]
            , Markdown.CodeBlock
                """json
                {
                  "code": "Next `detail` for the list item"
                }
                """
            ]
        , appear = True
        }

The `sample` represents the bellow markdown list item:

    - `content` for the list item

        `detail` for the list item

        ```json
        {
          "code": "Next `detail` for the list item"
        }
        ```

You can set the `appear` field `False` to skip the item from appearing up in the document, which allows you to generate documents for various targets:

    import MarkdownAst as Markdown

    type DocTarget
        = Developer
        | Manager
        | Customer

    docLevelDev : DocTarget -> Bool
    docLevelDev target =
        case target of
            Developer ->
                True

            Manager ->
                False

            Customer ->
                False

    myScenario : DocTarget -> Scenario Flags Command Memory Event
    myScenario target =
        [ Debug.todo "After some operations..."
        , expectEvents sakuraChanMainSession
            { content =
                [ Markdown.PlainText "Requests user profile to the server."
                ]
            , detail = []
            , appear = docLevelDev target
            }
            (Debug.todo "Expectation Here")
        , Debug.todo "..."
        ]

-}
type alias Markup =
    { content : List MdAst.InlineElement
    , detail : List MdAst.BlockElement
    , appear : Bool
    }


{-| Helper function to construct text only markup.

    import MarkdownAst as Markdown

    textContent "Only text here."
    --> { content = [ Markdown.PlainText "Only text here." ]
    --> , detail = []
    --> , appear = True
    --> }

-}
textContent : String -> Markup
textContent str =
    { content = [ MdAst.PlainText str ]
    , detail = []
    , appear = True
    }



-- -- Expectations


{-| Describe your expectations for the application memory state at the point.

Suppose your application has a counter:

    import Expect
    import MarkdownAst as Markdown

    myScenario =
        [ Debug.todo "After some operations..."
        , expectMemory sakuraChanMainSession
            { content =
                [ Markdown.PlainText "The counter must be less than four."
                ]
            , detail = []
            , appear = True
            }
            { layer =
                pageHomeLayer
            , expectation =
                \pageHomeMemory ->
                    pageHomeMemory.counter
                        |> Expect.lessThan 4
            }
        , Debug.todo "..."
        ]

You use [Expect](https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect) module to describe your expectation.

-}
expectMemory :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , expectation : m1 -> Expectation
        }
    -> Scenario flags m e
expectMemory (Session session) markup param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "expectMemory: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case param.layer <| Core.layerState sessionContext.model of
                            Nothing ->
                                SeqTest.fail description <|
                                    \_ ->
                                        Err (Core.memoryState sessionContext.model)
                                            |> Expect.equal
                                                (Ok "expectMemory: No layer found.")

                            Just (Core.Layer _ m1) ->
                                SeqTest.pass m1
                                    |> SeqTest.assert description
                                        param.expectation
                                    |> SeqTest.map (\_ -> context)
        , markup =
            \config ->
                let
                    markup_ =
                        config.processExpectMemoryMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


stringifyInlineItems : List MdAst.InlineElement -> String
stringifyInlineItems =
    List.map
        (\item ->
            case item of
                MdAst.PlainText str ->
                    str

                MdAst.Link o ->
                    o.text

                MdAst.Image o ->
                    o.alt

                MdAst.InlineCode str ->
                    str

                MdAst.Emphasis str ->
                    str

                MdAst.StrongEmphasis str ->
                    str

                MdAst.Strikethrough str ->
                    str

                MdAst.LineBreak ->
                    " "
        )
        >> String.concat


{-| Describe your expectations for the application's view at the point.

Suppose your application has a popup:

    import Html.Attribute exposing (attribute)
    import Test.Html.Query as Query
    import Test.Html.Selector as Selector

    myScenario =
        [ Debug.todo "After some operations..."
        , expectAppView sakuraChanMainSession
            (textContent "Show popup message.")
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

You use [Expect](https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect) module to describe your expectation.

Note that the `expectation` field takes page whole view even if you use it in `onLayer` function.

    import MarkdownAst as Markdown

    onLayer popup
        [ expectAppView sakuraChanMainSession
            (textContent "expectation about the whole application view"
            )
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
    -> Markup
    ->
        { expectation : Document (Msg event) -> Expectation
        }
    -> Scenario flags m event
expectAppView (Session session) markup { expectation } =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
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
            \config ->
                let
                    markup_ =
                        config.processExpectAppViewMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }



-- -- Event Simulators


{-| Load the app. You can also reload the app by calling `loadApp`.

    import Json.Encode as JE
    import MarkdownAst as Markdown
    import Tepa.AbsolutePath exposing (absolutePath)

    myScenario =
        [ userComment sakuraChan
            "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan
            "I'll open the home page..."
        , loadApp sakuraChanMainSession
            (textContent "Load the home page.")
            { path = absolutePath [] [] Nothing
            , flags =
                JE.object []
            }
        , systemComment sakuraChanMainSession
            "Show home page."
        , userComment sakuraChan
            "Oops, I accidentally hit the F5 button..."
        , loadApp sakuraChanMainSession
            (textContent "Reload the page.")
            { path = absolutePath [] [] Nothing
            , flags =
                JE.object []
            }
        , Debug.todo "..."
        ]

-}
loadApp :
    Session
    -> Markup
    ->
        { path : AbsolutePath
        , flags : flags
        }
    -> Scenario flags m e
loadApp (Session session) markup o =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case config.init o.flags (testUrl o.path) of
                    Err err ->
                        SeqTest.fail description <|
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
            \config ->
                let
                    markup_ =
                        config.processLoadAppMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


{-| Publish an event to the only first Layer specified by query.

Suppose your application has a popup:

    import MarkdownAst as Markdown

    myScenario =
        [ Debug.todo "After some operations..."
        , onLayer popup
            [ layerEvent sakuraChanMainSession
                (textContent "Click cancel button.")
                { event = ClickPopupCancelButton
                }
            ]
        , Debug.todo "..."
        ]

The example above publishes `ClickPopupCancelButton` event to the LayerId for the `popup` Layer.

If no Layers are found for the query, the test fails.

-}
layerEvent :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , event : event
        }
    -> Scenario flags m event
layerEvent (Session session) markup param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "layerEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case param.layer <| Core.layerState sessionContext.model of
                            Nothing ->
                                SeqTest.fail description <|
                                    \_ ->
                                        Expect.fail
                                            "layerEvent: No Layer found."

                            Just (Core.Layer lid _) ->
                                let
                                    res =
                                        update
                                            { onUrlChange = config.onUrlChange
                                            , currentTime = context.currentTime
                                            }
                                            (Core.LayerMsg
                                                { layerId = lid
                                                , event = param.event
                                                }
                                            )
                                            sessionContext
                                in
                                case res of
                                    Err err ->
                                        SeqTest.fail description <|
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
            \config ->
                let
                    markup_ =
                        config.processLayerEventMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


{-| About options:

  - layer: Query to specify the event target element from your current page HTML.

    Use querying functions that [Test.Html.Query](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Query) module exports.

  - operation: Simulated event caused by user operation.

    Use event builders that [Test.Html.Event](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Event) module exports.

Simulate a custom event. The String is the event name, and the Value is the event object the browser would send to the event listener callback.

-}
userOperation :
    Session
    -> Markup
    ->
        { query : Single (Msg e) -> Single (Msg e)
        , operation : ( String, Value )
        }
    -> Scenario flags m e
userOperation (Session session) markup param =
    let
        (User user) =
            session.user

        description =
            "[" ++ session.uniqueName ++ "] " ++ user.name ++ " " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
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
                                    |> param.query
                                    |> TestEvent.simulate param.operation
                                    |> TestEvent.toResult
                        in
                        case rmsg of
                            Err str ->
                                SeqTest.fail description <|
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
                                        SeqTest.fail description <|
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
            \config ->
                let
                    markup_ =
                        config.processUserOperationMarkup
                            { uniqueSessionName = session.uniqueName
                            , userName = user.name
                            }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


{-| Publish an event to the only first Listner specified by its name and query.

Suppose your application has a WebSocket message Listener named "WebSocket message Listener":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After some operations..."
        , listenerEvent sakuraChanMainSession
            (textContent "Receive WebSocket message")
            { layer = "WebSocket message Listener"
            , event =
                WebSocketMessage <|
                    JE.object
                        [ ( "action", JE.string "connected" )
                        ]
            }
        , Debug.todo "..."
        ]

If no Layers found for the query, it does nothing and the test passes.

-}
listenerEvent :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , name : String
        , event : event
        }
    -> Scenario flags m event
listenerEvent (Session session) markup param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "listenerEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case param.layer <| Core.layerState sessionContext.model of
                            Nothing ->
                                SeqTest.pass context

                            Just (Core.Layer thisLid _) ->
                                sessionContext.listeners
                                    |> List.filterMap
                                        (\listener ->
                                            if listener.layerId == thisLid && listener.uniqueName == param.name then
                                                Just <|
                                                    Core.ListenerMsg
                                                        { requestId = listener.requestId
                                                        , event = param.event
                                                        }

                                            else
                                                Nothing
                                        )
                                    |> List.head
                                    |> Maybe.map
                                        (\msg ->
                                            update
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                msg
                                                sessionContext
                                                |> Result.map
                                                    (\nextSessionContext ->
                                                        SeqTest.pass
                                                            { context
                                                                | sessions =
                                                                    Dict.insert session.uniqueName
                                                                        nextSessionContext
                                                                        context.sessions
                                                            }
                                                    )
                                                |> unwrapResult
                                                    (\err ->
                                                        SeqTest.fail description <|
                                                            \_ -> Expect.fail err
                                                    )
                                        )
                                    |> Maybe.withDefault
                                        (SeqTest.pass context)
        , markup =
            \config ->
                let
                    markup_ =
                        config.processLayerEventMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


{-| Wait for given micro seconds.
It only affects Promises defined in `Tepa.Time`.
-}
sleep :
    Session
    -> Markup
    -> Int
    -> Scenario flags m e
sleep (Session session) markup msec =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
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
                                SeqTest.fail description <|
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
            \config ->
                let
                    markup_ =
                        config.processSleepMarkdown
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


advanceClock :
    { onUrlChange : AbsolutePath -> Msg e
    , currentTime : Int
    }
    -> Int
    -> SessionContext m e
    -> Result String (SessionContext m e)
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
                        { config | currentTime = currentTime }

                    currentTime =
                        config.currentTime + timer.runAfter
                in
                update newConfig
                    (timer.msg <| Time.millisToPosix currentTime)
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
            (textContent "Received response.")
            { layer = "Port to get page.account.bio"
            , response =
                JE.string "I'm Sakura-chan."
            }
        , Debug.todo "..."
        ]

If no Layers found for the query, it does nothing and just passes the test.

-}
portResponse :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , name : String
        , response : Value -> Maybe Value
        }
    -> Scenario flags m e
portResponse (Session session) markup param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "portResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case param.layer <| Core.layerState sessionContext.model of
                            Nothing ->
                                -- It is natural to receive responses after the Layer has expired.
                                SeqTest.pass context

                            Just (Core.Layer lid_ _) ->
                                takeLastMatched
                                    (\( ( rid, lid ), req ) ->
                                        if lid == lid_ then
                                            param.response req
                                                |> Maybe.map
                                                    (\resp ->
                                                        Core.PortResponseMsg
                                                            { requestId = rid
                                                            , response = resp
                                                            }
                                                    )

                                        else
                                            Nothing
                                    )
                                    sessionContext.portRequests
                                    |> Result.fromMaybe "portResponse: No commands found for the response."
                                    |> Result.andThen
                                        (\( msg, nextPortRequests ) ->
                                            update
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                msg
                                                { sessionContext
                                                    | portRequests = nextPortRequests
                                                }
                                        )
                                    |> Result.map
                                        (\nextSessionContext ->
                                            SeqTest.pass
                                                { context
                                                    | sessions =
                                                        Dict.insert session.uniqueName
                                                            nextSessionContext
                                                            context.sessions
                                                }
                                        )
                                    |> unwrapResult
                                        (\err ->
                                            SeqTest.fail description <|
                                                \_ -> Expect.fail err
                                        )
        , markup =
            \config ->
                let
                    markup_ =
                        config.processPortResponseMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


{-| Simulate response to the `Tepa.customRequest`.

Suppose your application requests user infomation to the backend server via custom request named "Request for user info":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After request to the backend..."
        , portResponse sakuraChanMainSession
            (textContent "Received response.")
            { layer = "Request for user info"
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
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , name : String
        , response : Msg event
        }
    -> Scenario flags m event
customResponse (Session session) markup param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "customResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case param.layer <| Core.layerState sessionContext.model of
                            Nothing ->
                                -- It is natural to receive responses after the Layer has expired.
                                SeqTest.pass context

                            Just (Core.Layer lid_ _) ->
                                takeLastMatched
                                    (\(Core.Request name _ lid _) ->
                                        if lid == lid_ && name == param.name then
                                            Just param.response

                                        else
                                            Nothing
                                    )
                                    sessionContext.requests
                                    |> Result.fromMaybe "customResponse: No commands found for the response."
                                    |> Result.andThen
                                        (\( msg, nextRequests ) ->
                                            update
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                msg
                                                { sessionContext
                                                    | requests = nextRequests
                                                }
                                        )
                                    |> Result.map
                                        (\nextSessionContext ->
                                            SeqTest.pass
                                                { context
                                                    | sessions =
                                                        Dict.insert session.uniqueName
                                                            nextSessionContext
                                                            context.sessions
                                                }
                                        )
                                    |> unwrapResult
                                        (\err ->
                                            SeqTest.fail description <|
                                                \_ -> Expect.fail err
                                        )
        , markup =
            \config ->
                let
                    markup_ =
                        config.processCustomResponseMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
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
fromJust : String -> Maybe a -> (a -> List (Scenario flags m e)) -> Scenario flags m e
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
                    \_ _ ->
                        Err (InvalidFromJust description)
                }

        Just a ->
            f a
                |> sequence


{-| Similar to `fromJust`, but extract `Ok` valur from `Result`.
-}
fromOk : String -> Result err a -> (a -> List (Scenario flags m e)) -> Scenario flags m e
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
                    \_ _ ->
                        Err (InvalidFromOk description)
                }

        Ok a ->
            f a
                |> sequence



-- Test


{-| Generate scenario tests.
-}
toTest :
    { props : ApplicationProps flags memory event
    , sections : List (Section flags memory event)
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
                                                , requests = newState.requests
                                                , portRequests = []
                                                , httpRequests = []
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
    -> SessionContext m e
    -> List (Msg e)
    -> Result String (SessionContext m e)
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
    -> SessionContext m e
    -> Result String (SessionContext m e)
update config msg context =
    let
        newState =
            Core.update msg context.model
    in
    { model = newState.nextModel
    , requests = newState.requests ++ context.requests
    , portRequests = context.portRequests
    , httpRequests = context.httpRequests
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
    -> SessionContext m e
    -> Result String (SessionContext m e)
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
    -> SessionContext m e
    -> Result String (SessionContext m e)
applyLog config log context =
    case log of
        Core.SetTimer rid lid msec ->
            Ok
                { context
                    | timers =
                        putTimer
                            { runAfter = msec
                            , every = Nothing
                            , msg =
                                \_ ->
                                    Core.WakeUpMsg
                                        { requestId = rid }
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
                            , msg =
                                \curr ->
                                    Core.IntervalMsg
                                        { requestId = rid
                                        , timestamp = curr
                                        }
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

        Core.IssuePortRequest rid lid req ->
            Ok
                { context
                    | portRequests =
                        ( ( rid, lid ), req ) :: context.portRequests
                }

        Core.IssueHttpRequest rid lid req ->
            Ok
                { context
                    | httpRequests =
                        ( ( rid, lid ), req ) :: context.httpRequests
                    , timers =
                        case req.timeout of
                            Nothing ->
                                context.timers

                            Just timeout ->
                                putTimer
                                    { runAfter = timeout
                                    , every = Nothing
                                    , msg =
                                        \_ ->
                                            Core.HttpResponseMsg
                                                { requestId = rid
                                                , response = Err Core.Timeout
                                                }
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
                    | portRequests =
                        List.filter
                            (\( ( rid_, _ ), _ ) ->
                                rid_ /= rid
                            )
                            context.portRequests
                }

        Core.ResolveHttpRequest rid ->
            Ok
                { context
                    | httpRequests =
                        List.filter
                            (\( ( rid_, _ ), _ ) ->
                                rid_ /= rid
                            )
                            context.httpRequests
                }

        Core.ResolveRequest rid ->
            Ok
                { context
                    | requests =
                        List.filter
                            (\(Core.Request _ rid_ _ _) ->
                                rid_ /= rid
                            )
                            context.requests
                }

        Core.LayerExpired lid ->
            Ok
                { context
                    | requests =
                        List.filter
                            (\(Core.Request _ _ lid_ _) ->
                                lid_ /= lid
                            )
                            context.requests
                    , portRequests =
                        List.filter
                            (\( ( _, lid_ ), _ ) -> lid_ /= lid)
                            context.portRequests
                    , httpRequests =
                        List.filter
                            (\( ( _, lid_ ), _ ) -> lid_ /= lid)
                            context.httpRequests
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


putTimer : Timer e -> List (Timer e) -> List (Timer e)
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
    , sections : List (Section flags m e)
    , config : RenderConfig
    }
    -> Html msg
toHtml o =
    case buildMarkdown o of
        Err err ->
            renderInvalidMarkdown err

        Ok root ->
            MdAst.preview root


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
    , sections : List (Section flags m e)
    , config : RenderConfig
    }
    -> Result InvalidMarkup String
toMarkdown o =
    buildMarkdown o
        |> Result.map MdAst.render


buildMarkdown :
    { title : String
    , sections : List (Section flags m e)
    , config : RenderConfig
    }
    -> Result InvalidMarkup MdAst.Section
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
                                            let
                                                item =
                                                    o.config.entryPointFirstListItem initialTime
                                            in
                                            MdBuilder.appendListItem item.content
                                                >> MdBuilder.appendBlocks item.detail
                                                >> MdBuilder.break
                                                >> scenario.markup o.config

                                        RunAfter dep ->
                                            if Set.member dep titles then
                                                let
                                                    item =
                                                        o.config.dependentScenarioFirstListItem
                                                            { href =
                                                                "#"
                                                                    ++ (String.words dep
                                                                            |> List.map
                                                                                (String.filter Char.isAlphaNum
                                                                                    >> String.toLower
                                                                                )
                                                                            |> String.join "-"
                                                                       )
                                                            , name = dep
                                                            }
                                                in
                                                MdBuilder.appendListItem item.content
                                                    >> MdBuilder.appendBlocks item.detail
                                                    >> MdBuilder.break
                                                    >> scenario.markup o.config

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


{-| Configuration for rendering scenario.

  - entryPointFirstListItem: First item on the list for an `EntryPoint` scenario.
      - argument: The time when the scenario starts
  - dependentScenarioFirstListItem: First item on the list for a `RunAfter` scenario.
      - argument: `href` and `name` for its dependency.
  - processExpectMemoryMarkup: Processor for `expectMemory` markup
  - processExpectAppViewMarkup: Processor for `expectAppView` markup
  - processSleepMarkdown: Processor for `sleep` markup
  - processLoadAppMarkup: Processor for `loadApp` markup
  - processLayerEventMarkup: Processor for `layerEvent` markup
  - processUserOperationMarkup: Processor for `userOperation` markup
  - processHttpResponseMarkup: Processor for `httpResponse` or `httpBytesResponse` markup
  - processPortResponseMarkup: Processor for `portResponse` markup
  - processCustomResponseMarkup: Processor for `customResponse` markup

-}
type alias RenderConfig =
    { entryPointFirstListItem :
        Posix
        -> Markup
    , dependentScenarioFirstListItem :
        { href : String
        , name : String
        }
        -> Markup
    , processExpectMemoryMarkup :
        { uniqueSessionName : String }
        -> Markup
        -> Markup
    , processExpectAppViewMarkup :
        { uniqueSessionName : String }
        -> Markup
        -> Markup
    , processSleepMarkdown :
        { uniqueSessionName : String }
        -> Markup
        -> Markup
    , processLoadAppMarkup :
        { uniqueSessionName : String }
        -> Markup
        -> Markup
    , processLayerEventMarkup :
        { uniqueSessionName : String }
        -> Markup
        -> Markup
    , processUserOperationMarkup :
        { uniqueSessionName : String
        , userName : String
        }
        -> Markup
        -> Markup
    , processHttpResponseMarkup :
        { uniqueSessionName : String
        }
        -> Markup
        -> Markup
    , processPortResponseMarkup :
        { uniqueSessionName : String
        }
        -> Markup
        -> Markup
    , processCustomResponseMarkup :
        { uniqueSessionName : String
        }
        -> Markup
        -> Markup
    }


{-| Standard configuration for ja\_JP.
-}
ja_JP : RenderConfig
ja_JP =
    { entryPointFirstListItem =
        \posix ->
            let
                zone =
                    TimeZone.asia__tokyo ()

                year =
                    Time.toYear zone posix

                month =
                    Time.toMonth zone posix

                day =
                    Time.toDay zone posix

                hour =
                    Time.toHour zone posix

                minute =
                    Time.toMinute zone posix

                second =
                    Time.toSecond zone posix
            in
            { content =
                [ MdAst.PlainText <|
                    String.concat
                        [ "（"
                        , String.fromInt year
                        , "/"
                        , monthIndex month
                            |> String.fromInt
                        , "/"
                        , String.fromInt day
                        , " "
                        , String.fromInt hour
                        , ":"
                        , String.fromInt minute
                            |> String.padLeft 2 '0'
                        , ":"
                        , String.fromInt second
                            |> String.padLeft 2 '0'
                        , "）"
                        ]
                ]
            , detail =
                []
            , appear = True
            }
    , dependentScenarioFirstListItem =
        \o ->
            { content =
                [ MdAst.PlainText "（「"
                , MdAst.Link
                    { href = o.href
                    , text = o.name
                    , title = Nothing
                    }
                , MdAst.PlainText "」の直後）"
                ]
            , detail =
                []
            , appear = True
            }
    , processExpectMemoryMarkup = prependSessionSystemName
    , processExpectAppViewMarkup = prependSessionSystemName
    , processSleepMarkdown = prependSessionName
    , processLoadAppMarkup = prependSessionName
    , processLayerEventMarkup = prependSessionSystemName
    , processUserOperationMarkup = prependSessionAndUserName
    , processHttpResponseMarkup = prependSessionSystemName
    , processPortResponseMarkup = prependSessionSystemName
    , processCustomResponseMarkup = prependSessionSystemName
    }


prependSessionName :
    { uniqueSessionName : String }
    -> Markup
    -> Markup
prependSessionName { uniqueSessionName } markup =
    { markup
        | content =
            [ MdAst.StrongEmphasis <|
                "["
                    ++ uniqueSessionName
                    ++ "]"
            , MdAst.PlainText " "
            ]
                ++ markup.content
    }


prependSessionSystemName :
    { uniqueSessionName : String }
    -> Markup
    -> Markup
prependSessionSystemName { uniqueSessionName } markup =
    { markup
        | content =
            [ MdAst.StrongEmphasis <|
                "["
                    ++ uniqueSessionName
                    ++ "]"
            , MdAst.PlainText " "
            , MdAst.StrongEmphasis "System"
            , MdAst.PlainText ": "
            ]
                ++ markup.content
    }


prependSessionAndUserName :
    { uniqueSessionName : String
    , userName : String
    }
    -> Markup
    -> Markup
prependSessionAndUserName { uniqueSessionName, userName } markup =
    { markup
        | content =
            [ MdAst.StrongEmphasis <|
                "["
                    ++ uniqueSessionName
                    ++ "] "
            , MdAst.PlainText " "
            , MdAst.StrongEmphasis userName
            , MdAst.PlainText ": "
            ]
                ++ markup.content
    }


{-| Standard configuration for en\_US.
-}
en_US : RenderConfig
en_US =
    { entryPointFirstListItem =
        \posix ->
            let
                zone =
                    TimeZone.asia__tokyo ()

                year =
                    Time.toYear zone posix

                month =
                    Time.toMonth zone posix

                day =
                    Time.toDay zone posix

                hour =
                    Time.toHour zone posix

                minute =
                    Time.toMinute zone posix

                second =
                    Time.toSecond zone posix
            in
            { content =
                [ MdAst.PlainText <|
                    String.concat
                        [ monthIndex month
                            |> String.fromInt
                        , "/"
                        , String.fromInt day
                        , "/"
                        , String.fromInt year
                        , " "
                        , String.fromInt hour
                        , ":"
                        , String.fromInt minute
                            |> String.padLeft 2 '0'
                        , ":"
                        , String.fromInt second
                            |> String.padLeft 2 '0'
                        ]
                ]
            , detail =
                []
            , appear = True
            }
    , dependentScenarioFirstListItem =
        \o ->
            { content =
                [ MdAst.PlainText "Just after \""
                , MdAst.Link
                    { href = o.href
                    , text = o.name
                    , title = Nothing
                    }
                , MdAst.PlainText "\""
                ]
            , detail =
                []
            , appear = True
            }
    , processExpectMemoryMarkup = prependSessionSystemName
    , processExpectAppViewMarkup = prependSessionSystemName
    , processSleepMarkdown = prependSessionName
    , processLoadAppMarkup = prependSessionName
    , processLayerEventMarkup = prependSessionSystemName
    , processUserOperationMarkup = prependSessionAndUserName
    , processHttpResponseMarkup = prependSessionSystemName
    , processPortResponseMarkup = prependSessionSystemName
    , processCustomResponseMarkup = prependSessionSystemName
    }


monthIndex : Time.Month -> Int
monthIndex month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12



-- Http


{-| Http request that your web API server will receive:

  - `method` like `GET` and `PUT`, all in upper case
  - `headers` like `accept` and `cookie`, all names in lower case, and all values as it is
  - `url`
  - `requestBody`

**Note**: It is possible for a request to have the same header multiple times. In that case, all the values end up in a single entry in the headers dictionary. The values are separated by commas.

-}
type alias HttpRequest =
    { method : String
    , headers : Dict String String
    , url : String
    , requestBody : HttpRequestBody
    }


fromCoreHttpRequest : Core.HttpRequest -> HttpRequest
fromCoreHttpRequest param =
    { method = String.toUpper param.method
    , headers =
        List.foldl
            (\( k, v ) acc ->
                Dict.update
                    (String.toLower k)
                    (\mvs ->
                        case mvs of
                            Just vs ->
                                Just <| vs ++ ", " ++ v

                            Nothing ->
                                Just v
                    )
                    acc
            )
            Dict.empty
            param.headers
    , url = param.url
    , requestBody = fromCoreHttpRequestBody param.requestBody
    }


{-| -}
type HttpRequestBody
    = EmptyHttpRequestBody
    | StringHttpRequestBody String String
    | JsonHttpRequestBody Value
    | FileHttpRequestBody File
    | BytesHttpRequestBody String Bytes


fromCoreHttpRequestBody : Core.HttpRequestBody -> HttpRequestBody
fromCoreHttpRequestBody core =
    case core of
        Core.EmptyHttpRequestBody ->
            EmptyHttpRequestBody

        Core.StringHttpRequestBody mime str ->
            StringHttpRequestBody mime str

        Core.JsonHttpRequestBody value ->
            JsonHttpRequestBody value

        Core.FileHttpRequestBody file ->
            FileHttpRequestBody file

        Core.BytesHttpRequestBody mime bytes ->
            BytesHttpRequestBody mime bytes


{-| Simulate response to the `Tepa.Http.request` and `Tepa.Http.bytesRequest`.

Suppose your application requests to access localStorage via port request named "Port to get page.account.bio":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After request to the port..."
        , portResponse sakuraChanMainSession
            (textContent "Received response.")
            { layer = pageHomeLayer
            , name = "Port to get page.account.bio"
            , response =
                JE.string "I'm Sakura-chan."
            }
        , Debug.todo "..."
        ]

If no Layers found for the query, it does nothing and just passes the test.

-}
httpResponse :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , response : HttpRequest -> Maybe ( Http.Metadata, String )
        }
    -> Scenario flags m e
httpResponse (Session session) markup param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "httpResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case param.layer <| Core.layerState sessionContext.model of
                            Nothing ->
                                -- It is natural to receive responses after the Layer has expired.
                                SeqTest.pass context

                            Just (Core.Layer lid_ _) ->
                                takeLastMatched
                                    (\( ( rid, lid ), req ) ->
                                        if lid == lid_ then
                                            param.response (fromCoreHttpRequest req)
                                                |> Maybe.map
                                                    (\resp ->
                                                        Core.HttpResponseMsg
                                                            { requestId = rid
                                                            , response = Ok resp
                                                            }
                                                    )

                                        else
                                            Nothing
                                    )
                                    sessionContext.httpRequests
                                    |> Result.fromMaybe "httpResponse: No requests found for the response."
                                    |> Result.andThen
                                        (\( msg, nextHttpRequests ) ->
                                            update
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                msg
                                                { sessionContext
                                                    | httpRequests = nextHttpRequests
                                                }
                                        )
                                    |> Result.map
                                        (\nextSessionContext ->
                                            SeqTest.pass
                                                { context
                                                    | sessions =
                                                        Dict.insert session.uniqueName
                                                            nextSessionContext
                                                            context.sessions
                                                }
                                        )
                                    |> unwrapResult
                                        (\err ->
                                            SeqTest.fail description <|
                                                \_ -> Expect.fail err
                                        )
        , markup =
            \config ->
                let
                    markup_ =
                        config.processHttpResponseMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


{-| Similar to `httpResponse`, but responds with `Bytes`.
-}
httpBytesResponse :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , response : HttpRequest -> Maybe ( Http.Metadata, Bytes )
        }
    -> Scenario flags m e
httpBytesResponse (Session session) markup param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ stringifyInlineItems markup.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "httpResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case param.layer <| Core.layerState sessionContext.model of
                            Nothing ->
                                -- It is natural to receive responses after the Layer has expired.
                                SeqTest.pass context

                            Just (Core.Layer lid_ _) ->
                                takeLastMatched
                                    (\( ( rid, lid ), req ) ->
                                        if lid == lid_ then
                                            param.response (fromCoreHttpRequest req)
                                                |> Maybe.map
                                                    (\resp ->
                                                        Core.HttpBytesResponseMsg
                                                            { requestId = rid
                                                            , response = Ok resp
                                                            }
                                                    )

                                        else
                                            Nothing
                                    )
                                    sessionContext.httpRequests
                                    |> Result.fromMaybe "httpResponse: No requests found for the response."
                                    |> Result.andThen
                                        (\( msg, nextHttpRequests ) ->
                                            update
                                                { onUrlChange = config.onUrlChange
                                                , currentTime = context.currentTime
                                                }
                                                msg
                                                { sessionContext
                                                    | httpRequests = nextHttpRequests
                                                }
                                        )
                                    |> Result.map
                                        (\nextSessionContext ->
                                            SeqTest.pass
                                                { context
                                                    | sessions =
                                                        Dict.insert session.uniqueName
                                                            nextSessionContext
                                                            context.sessions
                                                }
                                        )
                                    |> unwrapResult
                                        (\err ->
                                            SeqTest.fail description <|
                                                \_ -> Expect.fail err
                                        )
        , markup =
            \config ->
                let
                    markup_ =
                        config.processHttpResponseMarkup
                            { uniqueSessionName = session.uniqueName }
                            markup
                in
                if markup_.appear then
                    MdBuilder.appendListItem markup_.content
                        >> MdBuilder.appendBlocks markup_.detail
                        >> MdBuilder.break
                        >> Ok

                else
                    Ok
        }


takeLastMatched : (a -> Maybe b) -> List a -> Maybe ( b, List a )
takeLastMatched f ls =
    let
        res =
            List.foldl
                (\a acc ->
                    case acc of
                        ( Just b, accLs {- ordered -} ) ->
                            ( Just b, a :: accLs )

                        ( Nothing, accLs {- ordered -} ) ->
                            case f a of
                                Just b ->
                                    -- remove the matched element from result
                                    ( Just b, accLs )

                                Nothing ->
                                    ( Nothing, a :: accLs )
                )
                ( Nothing, [] )
                (List.reverse ls {- reversed -})
    in
    case res of
        ( Just b, resLs {- ordered -} ) ->
            Just ( b, resLs )

        _ ->
            Nothing


unwrapResult : (e -> a) -> Result e a -> a
unwrapResult f res =
    case res of
        Err e ->
            f e

        Ok a ->
            a
