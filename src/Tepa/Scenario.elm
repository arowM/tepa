module Tepa.Scenario exposing
    ( Scenario
    , none
    , sequence
    , toTest
    , toMarkdown
    , InvalidMarkup(..)
    , stringifyInvalidMarkup
    , Section
    , Dependency(..)
    , User
    , defineUser
    , Session
    , defineSession
    , Markup
    , markup
    , hide
    , setParam
    , modifyContent
    , userComment
    , systemComment
    , comment
    , todo
    , userTodo
    , expectMemory
    , expectAppView
    , expectValues
    , expectCurrentTime
    , expectHttpRequest
    , expectPortRequest
    , expectRandomRequest
    , appLayer
    , childLayer
    , mapLayer
    , loadApp
    , closeApp
    , userOperation
    , sleep
    , httpResponse
    , httpBytesResponse
    , HttpRequest
    , HttpRequestBody(..)
    , portResponse
    , PortRequest
    , randomResponse
    , forward
    , back
    , pushPath
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
@docs toMarkdown
@docs InvalidMarkup
@docs stringifyInvalidMarkup
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
@docs markup
@docs hide
@docs setParam
@docs modifyContent


# Primitives


## Comments

@docs userComment
@docs systemComment
@docs comment


## Stubs

@docs todo
@docs userTodo


## Expectations

@docs expectMemory
@docs expectAppView
@docs expectValues
@docs expectCurrentTime
@docs expectHttpRequest
@docs expectPortRequest
@docs expectRandomRequest


## Helper functions to specify Layer

@docs appLayer
@docs childLayer
@docs mapLayer


## Event Simulators

@docs loadApp
@docs closeApp
@docs userOperation
@docs sleep


## Http response Simulators

@docs httpResponse
@docs httpBytesResponse
@docs HttpRequest
@docs HttpRequestBody


## Port response Simulators

@docs portResponse
@docs PortRequest


## Random response Simulators

@docs randomResponse


## Browser Simulators

@docs forward
@docs back
@docs pushPath


# Conditions

@docs fromJust
@docs fromOk


# RenderConfig

@docs RenderConfig
@docs en_US
@docs ja_JP

-}

import AppUrl exposing (AppUrl)
import Browser
import Browser.Dom as BrowserDom
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import File exposing (File)
import Http
import Internal.Core as Core exposing (AppState(..), LayerId, Model(..), RequestId)
import Internal.History as History exposing (History)
import Internal.Template as Template
import Json.Encode as JE exposing (Value)
import Mixin.Html as Html exposing (Html)
import Set
import Tepa exposing (ApplicationProps, Layer, Msg)
import Tepa.Random as Random
import Tepa.Time as Time exposing (Posix, Zone)
import Test exposing (Test)
import Test.Html.Event as TestEvent
import Test.Html.Query as TestQuery exposing (Single)
import Test.Html.Selector as TestSelector
import Test.Runner as TestRunner
import Test.Sequence as SeqTest



-- Scenario


{-| Scenario describes how the application reacts to the user operations along the time line.

The Scenario you built can be converted to tests with `toTest`, and to documents with `toMarkdown`.

-}
type Scenario flags memory
    = Scenario
        { test : TestConfig (AppState flags memory) -> TestContext (AppState flags memory) -> SeqTest.Sequence (TestContext (AppState flags memory))
        , markup :
            RenderConfig -> ListBlock -> Result InvalidMarkup ListBlock
        }


type alias ListBlock =
    ( Int, List ( Int, String ) )



-- reversed


type alias TestConfig m =
    { view : m -> Tepa.Document
    , init : Value -> AppUrl -> SessionUpdateResult m
    }


{-| -}
type alias TestContext m =
    { sessions : Dict String (SessionContext m)
    , currentTime : Int -- in milliseconds
    , zone : Zone
    , view : Core.Layer_ m -> Html Core.Msg
    }


{-| -}
type alias SessionContext m =
    { model : Model m
    , httpRequests : List ( ( RequestId, LayerId ), Core.HttpRequest ) -- reversed
    , randomRequests : List ( ( RequestId, LayerId ), Core.RandomRequest ) -- reversed
    , timers : List Timer
    , history : History
    }


{-| Manage timeout operations.
-}
type alias Timer =
    { runAfter : Int
    , every : Maybe Int
    , msg : Posix -> Core.Msg
    , layerId : LayerId
    }


{-| -}
type InvalidMarkup
    = InvalidFromJust String
    | InvalidFromOk String
    | NoDependentSection String
    | DuplicatedSection String
    | ParameterNotFound String String


{-| -}
stringifyInvalidMarkup : InvalidMarkup -> String
stringifyInvalidMarkup err =
    case err of
        InvalidFromJust str ->
            str ++ "\nfromJust: The value is Nothing."

        InvalidFromOk str ->
            str ++ "\nThe value is Err."

        NoDependentSection str ->
            str ++ "\nNo dependent section."

        DuplicatedSection str ->
            str ++ "\nDuplicated section."

        ParameterNotFound name desc ->
            desc ++ "\nParameter not found: " ++ name


{-| A Scenario that does nothing.
-}
none : Scenario flags m
none =
    Scenario
        { test = noneTest
        , markup = \_ -> Ok
        }


{-| -}
noneTest : TestConfig m -> TestContext m -> SeqTest.Sequence (TestContext m)
noneTest _ =
    SeqTest.pass


{-| Return a new Scenario that evaluates given Scenarios sequentially.
-}
sequence : List (Scenario flags m) -> Scenario flags m
sequence =
    List.foldl
        (\a acc ->
            mappend acc a
        )
        none


mappend : Scenario flags m -> Scenario flags m -> Scenario flags m
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



-- Section


{-| Titled sequence of Scenarios.

  - title: Title for the Section, which must be unique string
  - content: Sequence of Scenarios for the Section
  - dependency: Dependency of the Section

You may want to branch out in the middle of your scenario.
In such case, you can declare common section, and refer to the title in `dependency` parameter:

    import TimeZone


    -- justinmimbs/timezone-data
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

    commonScenario : Section Flags Memory
    commonScenario =
        { title = "Common scenario"
        , content =
            [ Debug.todo "Common scenarios"
            , Debug.todo "..."
            ]
        , dependency =
            EntryPoint
                (TimeZone.asia__tokyo ())
                (Time.millisToPosix 1672531200000)
        }

    caseA : Section Flags Memory
    caseA =
        { title = "Case A"
        , content =
            -- After common scenario,
            [ Debug.todo "User clicks button A"
            , Debug.todo "..."
            ]
        , dependency = RunAfter commonScenario.title
        }

    caseB : Section Flags Memory
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
type alias Section flags memory =
    { title : String
    , content : List (Scenario flags memory)
    , dependency : Dependency
    }


{-| Dependency of a Section.

  - `EntryPoint zone time`: Indicates that the Section has no dependencies and starts at the specified `time` in `zone`.
  - `RunAfter sectionTitle`: Indicates that the Section is after another Section specified by the `sectionTitle`.

-}
type Dependency
    = EntryPoint Zone Posix
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
userComment : User -> String -> Scenario flags m
userComment (User user) commentText =
    comment
        (markup """
          **{{name}}**: {{comment}}
          """
            |> setParam "name" user.name
            |> setParam "comment" commentText
        )


{-| System comment.

This Scenario only affects document generation, and is ignored for scenario test generation.

-}
systemComment : Session -> String -> Scenario flags m
systemComment (Session session) commentText =
    comment
        (markup """
          **[{{session}}]** {{comment}}
          """
            |> setParam "session" session.uniqueName
            |> setParam "comment" commentText
        )


{-| Lower level function to add detailed comments.
-}
comment : Markup -> Scenario flags m
comment markup_ =
    Scenario
        { test = noneTest
        , markup =
            \_ ->
                appendMarkup markup_
        }


{-| Generates documentation, but the test always fails.

You can create a scenario first with `todo` and later replace that `todo` with an actual test, which is the scenario driven development.

-}
todo : Session -> Markup -> Scenario flags m
todo (Session session) (Markup markup_) =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ _ ->
                SeqTest.fail description <|
                    \_ ->
                        Expect.fail "todo"
        , markup =
            \config ->
                appendMarkup <|
                    config.processSessionScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Similar `todo`, but for user actions.

It prepends the username to the markup.

-}
userTodo : Session -> Markup -> Scenario flags m
userTodo (Session session) (Markup markup_) =
    let
        (User user) =
            session.user

        description =
            "[" ++ session.uniqueName ++ "] " ++ user.name ++ " " ++ markup_.content
    in
    Scenario
        { test =
            \_ _ ->
                SeqTest.fail description <|
                    \_ ->
                        Expect.fail "todo"
        , markup =
            \config ->
                appendMarkup <|
                    config.processUserScenario
                        { uniqueSessionName = session.uniqueName
                        , userName = user.name
                        }
                        (Markup markup_)
        }


{-| Represents markup for a scenario.

Suppose you have the following `Markup`, which uses [arowM/elm-markdown-ast](https://package.elm-lang.org/packages/arowM/elm-markdown-ast/latest/):

    sample : Markup
    sample =
        markup """
        `content` for the list item

        `detail` for the list item

        ```json
        {
          "code": "Next `detail` for the list item"
        }
        ```
        """

The `sample` represents the bellow markdown list item:

    - `content` for the list item

        `detail` for the list item

        ```json
        {
          "code": "Next `detail` for the list item"
        }
        ```

You can use `hide` to skip the item from appearing up in the document, which allows you to generate documents for various targets:

    type DocTarget
        = Developer
        | Manager
        | Customer

    docLevelDev : DocTarget -> Markup -> Markup
    docLevelDev target =
        hide <|
            case target of
                Developer ->
                    False

                Manager ->
                    True

                Customer ->
                    True

    myScenario : DocTarget -> Scenario Flags Command Memory Event
    myScenario target =
        [ Debug.todo "After some operations..."
        , expectEvents sakuraChanMainSession
            (markup """
              Requests user profile to the server.
              """
                |> docLevelDev
            )
            (Debug.todo "Expectation Here")
        , Debug.todo "..."
        ]

-}
type Markup
    = Markup Markup_


type alias Markup_ =
    { content : String
    , details : List ( Int, String )
    , appear : Bool
    , params : Dict String String
    }


{-| Modify `Markup` content part.

Markup:

    markup """
    This is **content**.
    You can also provide detail informations.

    ```json
    {
      "foo": 3
    }
    ```
    """
        |> modifyContent (String.prepend "(notation) ")

Rendered to:

    - (notation) This is **content**.

        You can also provide detail informations.

        ```json
        {
          "foo": 3
        }
        ```

-}
modifyContent : (String -> String) -> Markup -> Markup
modifyContent f (Markup markup_) =
    Markup
        { markup_ | content = f markup_.content }


{-| Constructor for `Markup`.

Markup:

    markup "No detail"

Rendered to:

    - No detail

Markup:

    markup """
    This is **content**.
    You can also provide detail informations.

    ```json
    {
      "foo": 3
    }
    ```
    """

Rendered:

    - This is **content**.

        You can also provide detail informations.

        ```json
        {
          "foo": 3
        }
        ```

Markup:

    markup """
    You can interpolate value by `\\{{key}}` syntax.
    Here is the sample: {{message}}
    By appending `|raw`, you can avoid sanitizing: {{message|raw}}

    The `\\{{key|json}}` syntax can embed the JSON structure from a string generated by `Debug.toString`.
    Sample:

    {{sampleStructure|json}}
    """
    |> setParam "message" "Hello!"
    |> setParam "sampleStructure"
        ( Debug.toString <|
            { "foo": 3
            , "bar": "baz"
            }
        )

Rendered:

    - You can interpolate value by `{{key}}` syntax.

        Here is the sample: Hello\!
        By appending `|raw`, you can avoid sanitizing: Hello!

        The `{{key|json}}` syntax can embed the JSON structure from a string generated by `Debug.toString`.
        Sample:

        ```json
        {
            "foo": 3,
            "bar": "baz"
        }
        ```

-}
markup : String -> Markup
markup str =
    let
        tmp =
            String.lines str
                |> List.map reduceIndent
                |> dropBlanks
    in
    case tmp of
        [] ->
            Markup
                { content = ""
                , details = []
                , appear = True
                , params = Dict.empty
                }

        ( _, content ) :: rawDetails ->
            let
                noBlanks =
                    dropBlanks rawDetails

                minLevel =
                    List.foldl
                        (\( level, str_ ) n ->
                            if str_ == "" then
                                n

                            else if level < n then
                                level

                            else
                                n
                        )
                        2147483647
                        noBlanks

                details =
                    List.map
                        (\( level, str_ ) ->
                            if str_ == "" then
                                ( 0, "" )

                            else
                                ( level - minLevel, str_ )
                        )
                        noBlanks
            in
            Markup
                { content = content
                , details = details
                , appear = True
                , params = Dict.empty
                }


reduceIndent : String -> ( Int, String )
reduceIndent =
    reduceIndent_ 0


reduceIndent_ : Int -> String -> ( Int, String )
reduceIndent_ acc str =
    case String.uncons str of
        Nothing ->
            ( acc, "" )

        Just ( ' ', s ) ->
            reduceIndent_ (acc + 1) s

        Just _ ->
            ( acc, str )


dropBlanks : List ( Int, String ) -> List ( Int, String )
dropBlanks list =
    case list of
        [] ->
            []

        ( _, "" ) :: xs ->
            dropBlanks xs

        _ ->
            list


{-| Hide the Markup from rendered scenario document.
-}
hide : Bool -> Markup -> Markup
hide p (Markup markup_) =
    Markup
        { markup_ | appear = not p }


{-| Specify values for embedded variables.
-}
setParam : String -> String -> Markup -> Markup
setParam k v (Markup markup_) =
    Markup
        { markup_ | params = Dict.insert k v markup_.params }



-- -- Expectations


{-| Describe your expectations for the application memory state at the point.

Suppose your application has a counter:

    import Expect

    myScenario =
        [ Debug.todo "After some operations..."
        , expectMemory sakuraChanMainSession
            (markup "The counter must be less than four.")
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

During the initialization phase of the application, or if no Layers are found for the query, it fails the test.

-}
expectMemory :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , expectation : m1 -> Expectation
        }
    -> Scenario flags m
expectMemory (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "expectMemory: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                SeqTest.fail description <|
                                    \_ ->
                                        Expect.fail
                                            "expectMemory: The application is during initialization."

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        SeqTest.fail description <|
                                            \_ ->
                                                Err (Core.memoryState sessionContext.model)
                                                    |> Expect.equal
                                                        (Ok "expectMemory: No layer found.")

                                    Just (Core.Layer layer1) ->
                                        SeqTest.pass layer1.state
                                            |> SeqTest.assert description
                                                param.expectation
                                            |> SeqTest.map (\_ -> context)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


maybeLoadedLayer : SessionContext (AppState flags m) -> Maybe (Layer m)
maybeLoadedLayer sessionContext =
    Core.maybeMapLayer
        (\appState ->
            case appState of
                AppLoading ->
                    Nothing

                AppLoaded _ m ->
                    Just m
        )
        (Core.layerState sessionContext.model)


sessionLayerId : SessionContext (AppState flags m) -> Core.LayerId
sessionLayerId sessionContext =
    let
        (Core.Layer layer) =
            Core.layerState sessionContext.model
    in
    Core.unwrapThisLayerId layer.id


appendMarkup :
    Markup
    -> ( Int, List ( Int, String ) )
    -> Result InvalidMarkup ( Int, List ( Int, String ) )
appendMarkup (Markup markup_) ( level, acc ) =
    let
        resolvedContent : Result Template.Error String
        resolvedContent =
            Template.resolveInline markup_.params markup_.content

        resolvedDetails : Result ( Template.Error, String ) (List ( Int, String ))
        resolvedDetails =
            -- reversed
            List.foldl
                (\( thisLevel, str ) res ->
                    res
                        |> Result.andThen
                            (\accDetails ->
                                Template.resolveInline markup_.params str
                                    |> Result.andThen (Template.resolveBlock markup_.params)
                                    |> Result.map
                                        (\block ->
                                            (String.lines block
                                                |> List.reverse
                                                |> List.map (\x -> ( thisLevel, x ))
                                            )
                                                ++ accDetails
                                        )
                                    |> Result.mapError (\e -> ( e, str ))
                            )
                )
                (Ok [])
                markup_.details
    in
    case ( resolvedContent, resolvedDetails ) of
        ( Err (Template.ParameterNotFound name), _ ) ->
            Err <| ParameterNotFound name markup_.content

        ( _, Err ( Template.ParameterNotFound name, str ) ) ->
            Err <| ParameterNotFound name str

        ( Ok content, Ok details ) ->
            Ok <|
                if markup_.appear then
                    ( level
                    , acc
                        |> List.append
                            [ ( level
                              , "1. "
                                    ++ content
                                    ++ (if List.isEmpty details then
                                            ""

                                        else
                                            "\n"
                                       )
                              )
                            ]
                        |> List.append
                            (details
                                |> List.map
                                    (\( l, s ) ->
                                        if s == "" then
                                            ( 0, "" )

                                        else
                                            ( l + level + 4, s )
                                    )
                            )
                    )

                else
                    ( level, acc )


{-| Describe your expectations for the application's view at the point.

Suppose your application has a popup:

    import Html.Attribute exposing (attribute)
    import Test.Html.Query as Query
    import Test.Html.Selector as Selector

    myScenario =
        [ Debug.todo "After some operations..."
        , expectAppView sakuraChanMainSession
            (markup "Show popup message.")
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

Note that the `expectation` field takes whole page view even if you use it in `onLayer` function.

    onLayer popup
        [ expectAppView sakuraChanMainSession
            (markup "expectation about the whole application view")
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
        { expectation : Tepa.Document -> Expectation
        }
    -> Scenario flags m
expectAppView (Session session) (Markup markup_) { expectation } =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "expectAppView: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        SeqTest.pass
                            (Core.memoryState sessionContext.model
                                |> config.view
                            )
                            |> SeqTest.assert description expectation
                            |> SeqTest.map (\_ -> context)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Describe your expectations for the user input values in the specified Layer at the point.

You use [Expect](https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect) module to describe your expectation.

During the initialization phase of the application, or if no Layers are found for the query, it fails the test.

-}
expectValues :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , expectation : Dict String String -> Expectation
        }
    -> Scenario flags m
expectValues (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "expectValues: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                SeqTest.fail description <|
                                    \_ ->
                                        Expect.fail
                                            "expectValues: The application is during initialization."

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        SeqTest.fail description <|
                                            \_ ->
                                                Err (Core.memoryState sessionContext.model)
                                                    |> Expect.equal
                                                        (Ok "expectValues: No layer found.")

                                    Just (Core.Layer layer1) ->
                                        let
                                            (Core.ThisLayerValues dict) =
                                                layer1.values
                                        in
                                        SeqTest.pass dict
                                            |> SeqTest.assert description
                                                param.expectation
                                            |> SeqTest.map (\_ -> context)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSessionScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Describe your expectations for the emulated current time of the application.

Suppose you want to check current time after `sleep`.

    import Expect
    import Tepa.Scenario as Scenario
    import Time
    import TimeZone -- justinmimbs/timezone-data


    sample : Section Flags Memory
    sample =
        { title = "Sample Scenario"
        , dependency =
            Scenario.EntryPoint
                (TimeZone.asia__tokyo ())
                (Time.millisToPosix 1672531200000)
        , content =
            [ userComment sakuraChan "I'm trying to access the Goat SNS."
            , Scenario.sleep (Scenario.markup "Passing one minutes.")
            , userComment sakuraChan "Oops, I've slept a little."
            , let
                curr = Time.millisToPosix <| 1672531200000 + 1 * 60 * 1000
              in
              Scenario.expectCurrentTime
                (Scenario.markup <| "Current time is: " ++ formatPosix curr ++ ".")
                { expectation =
                    Expect.equal curr
                }
            ]

You use [Expect](https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect) module to describe your expectation.

-}
expectCurrentTime :
    Markup
    ->
        { expectation : Posix -> Expectation
        }
    -> Scenario flags m
expectCurrentTime (Markup markup_) { expectation } =
    let
        description =
            markup_.content
    in
    Scenario
        { test =
            \_ context ->
                SeqTest.pass (Time.millisToPosix context.currentTime)
                    |> SeqTest.assert description expectation
                    |> SeqTest.map (\_ -> context)
        , markup =
            \_ ->
                appendMarkup (Markup markup_)
        }


{-| Describe your expectations for the unresolved HTTP requests at the time.

You use [Expect](https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect) module to describe your expectation.

During the initialization phase of the application, the `layer` parameter is ignored, so it is common to set `\_ -> Nothing` in such case.
After initialization, if no Layers are found for the query, it fails the test.

-}
expectHttpRequest :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , expectation : List HttpRequest -> Expectation
        }
    -> Scenario flags m
expectHttpRequest (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "expectHttpRequest: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            withLayerId : LayerId -> SeqTest.Sequence (TestContext (AppState flags m))
                            withLayerId layerId =
                                List.filterMap
                                    (\( ( _, lid ), req ) ->
                                        if lid == layerId then
                                            Just <| fromCoreHttpRequest req

                                        else
                                            Nothing
                                    )
                                    sessionContext.httpRequests
                                    |> SeqTest.pass
                                    |> SeqTest.assert description
                                        param.expectation
                                    |> SeqTest.map (\_ -> context)
                        in
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                withLayerId (sessionLayerId sessionContext)

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        SeqTest.fail description <|
                                            \_ ->
                                                Err (Core.memoryState sessionContext.model)
                                                    |> Expect.equal
                                                        (Ok "expectHttpRequest: No layer found.")

                                    Just (Core.Layer layer1) ->
                                        withLayerId (Core.unwrapThisLayerId layer1.id)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSessionScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Describe your expectations for the unresolved Port requests at the time.

You use [Expect](https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect) module to describe your expectation.

During the initialization phase of the application, the `layer` parameter is ignored, so it is common to set `\_ -> Nothing` in such case.
After initialization, if no Layers are found for the query, it fails the test.

-}
expectPortRequest :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , expectation : List PortRequest -> Expectation
        }
    -> Scenario flags m
expectPortRequest (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "expectPortRequest: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            withLayerId : LayerId -> SeqTest.Sequence (TestContext (AppState flags m))
                            withLayerId layerId =
                                List.filterMap
                                    (\p ->
                                        if p.layer == layerId then
                                            Just p

                                        else
                                            Nothing
                                    )
                                    (sessionPorts sessionContext)
                                    |> SeqTest.pass
                                    |> SeqTest.assert description
                                        (\requests ->
                                            List.map
                                                (\p ->
                                                    { name = p.name
                                                    , requestBody = p.requestBody
                                                    }
                                                )
                                                requests
                                                |> param.expectation
                                        )
                                    |> SeqTest.map (\_ -> context)
                        in
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                withLayerId (sessionLayerId sessionContext)

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        SeqTest.fail description <|
                                            \_ ->
                                                Err (Core.memoryState sessionContext.model)
                                                    |> Expect.equal
                                                        (Ok "expectPortRequest: No layer found.")

                                    Just (Core.Layer layer1) ->
                                        withLayerId (Core.unwrapThisLayerId layer1.id)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSessionScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


sessionPorts : SessionContext m -> List Core.PortRequest
sessionPorts sessionContext =
    let
        (Model model) =
            sessionContext.model
    in
    model.context.ports


{-| Describe your expectations for the unresolved Random requests at the time.

You pass `Tepa.Random.Spec` to specify your expected request.

During the initialization phase of the application, the `layer` parameter is ignored, so it is common to set `\_ -> Nothing` in such case.
After initialization, if no Layers are found for the query, it fails the test.

-}
expectRandomRequest :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , spec : Random.Spec a
        }
    -> Scenario flags m
expectRandomRequest (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "expectRandomRequest: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            withLayerId : LayerId -> SeqTest.Sequence (TestContext (AppState flags m))
                            withLayerId layerId =
                                List.filterMap
                                    (\( ( _, lid ), val ) ->
                                        if lid == layerId then
                                            Just val

                                        else
                                            Nothing
                                    )
                                    sessionContext.randomRequests
                                    |> SeqTest.pass
                                    |> SeqTest.assert description
                                        (\ls ->
                                            let
                                                hasRequest =
                                                    List.any (Core.isRequestForSpec param.spec) ls
                                            in
                                            if hasRequest then
                                                Expect.pass

                                            else
                                                Expect.fail "randomResponse: No requests found for the Spec"
                                        )
                                    |> SeqTest.map (\_ -> context)
                        in
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                withLayerId (sessionLayerId sessionContext)

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        SeqTest.fail description <|
                                            \_ ->
                                                Err (Core.memoryState sessionContext.model)
                                                    |> Expect.equal
                                                        (Ok "expectRandomRequest: No layer found.")

                                    Just (Core.Layer layer1) ->
                                        withLayerId (Core.unwrapThisLayerId layer1.id)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSessionScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }



-- -- Helper functions to specify Layer


{-| Specifies the application root layer, which is just an alias for `Just`.
-}
appLayer : Layer m -> Maybe (Layer m)
appLayer =
    Just


{-| Helper function to specify child layer.

    type alias Parent =
        { child : Maybe (Layer Child)
        }

    myParentLayer : m -> Maybe (Layer Parent)
    myParentLayer =
        Debug.todo "parent Layer"

    myChildLayer : m -> Maybe (Layer Child)
    myChildLayer =
        myParentLayer
            |> childLayer .child

-}
childLayer :
    (m1 -> Maybe (Layer m2))
    -> (Layer m -> Maybe (Layer m1))
    -> Layer m
    -> Maybe (Layer m2)
childLayer f parent =
    parent
        >> Maybe.andThen
            (\(Core.Layer layer1) ->
                f layer1.state
            )


{-| -}
mapLayer :
    (m1 -> m2)
    -> (Layer m -> Maybe (Layer m1))
    -> (Layer m -> Maybe (Layer m2))
mapLayer =
    Core.mapLayerQuery



-- -- Event Simulators


{-| Load the app. You can also reload the app by calling `loadApp`.

    import AppUrl exposing (AppUrl)
    import Dict
    import Json.Encode as JE

    myScenario =
        [ userComment sakuraChan
            "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan
            "I'll open the home page..."
        , loadApp sakuraChanMainSession
            (markup "Load the home page.")
            { path =
                { path = []
                , query = Dict.empty
                , fragment = Nothing
                }
            , flags =
                JE.object []
            }
        , systemComment sakuraChanMainSession
            "Show home page."
        , userComment sakuraChan
            "Oops, I accidentally hit the F5 button..."
        , loadApp sakuraChanMainSession
            (markup "Reload the page.")
            { path =
                { path = []
                , query = Dict.empty
                , fragment = Nothing
                }
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
        { path : AppUrl
        , flags : Value
        }
    -> Scenario flags m
loadApp (Session session) (Markup markup_) o =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \config context ->
                case config.init o.flags o.path of
                    SessionExpired ->
                        SeqTest.pass
                            { context
                                | sessions =
                                    Dict.remove session.uniqueName
                                        context.sessions
                            }

                    SessionUpdateFailed err ->
                        SeqTest.fail description <|
                            \_ -> Expect.fail err

                    SessionUpdated sessionContext ->
                        SeqTest.pass
                            { context
                                | sessions =
                                    Dict.insert session.uniqueName
                                        sessionContext
                                        context.sessions
                            }
        , markup =
            \config ->
                appendMarkup <|
                    config.processSessionScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Close the app.
-}
closeApp :
    Session
    -> Markup
    -> Scenario flags m
closeApp (Session session) (Markup markup_) =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ -> Expect.fail "The application is already not active in the session."

                    Just _ ->
                        SeqTest.pass
                            { context
                                | sessions =
                                    Dict.remove session.uniqueName
                                        context.sessions
                            }
        , markup =
            \config ->
                appendMarkup <|
                    config.processSessionScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| About options:

  - layer: Query to specify the event target element from your current page HTML.

    Use querying functions that [Test.Html.Query](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Query) module exports.

  - operation: Simulated event caused by user operation.

    Simulate a custom event. The String is the event name, and the Value is the event object the browser would send to the event listener callback.

Note that current version does not trigger page transition on clicking anchor elements.
Alternatively, you can check the anchor has expected `href` value with `expectAppView`,
and `pushPath` to the path.

-}
userOperation :
    Session
    -> Markup
    ->
        { query : Single Msg -> Single Msg
        , operation : ( String, Value )
        }
    -> Scenario flags m
userOperation (Session session) (Markup markup_) param =
    let
        (User user) =
            session.user

        description =
            "[" ++ session.uniqueName ++ "] " ++ user.name ++ " " ++ markup_.content
    in
    Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "userOperation: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            rmsg =
                                Core.memoryState sessionContext.model
                                    |> config.view
                                    |> .body
                                    |> Html.div []
                                    |> TestQuery.fromHtml
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
                                                context
                                                sessionContext
                                in
                                case res of
                                    SessionExpired ->
                                        SeqTest.pass
                                            { context
                                                | sessions =
                                                    Dict.remove session.uniqueName
                                                        context.sessions
                                            }

                                    SessionUpdateFailed err ->
                                        SeqTest.fail description <|
                                            \_ -> Expect.fail err

                                    SessionUpdated nextSessionContext ->
                                        SeqTest.pass
                                            { context
                                                | sessions =
                                                    Dict.insert session.uniqueName
                                                        nextSessionContext
                                                        context.sessions
                                            }
        , markup =
            \config ->
                appendMarkup <|
                    config.processUserScenario
                        { uniqueSessionName = session.uniqueName
                        , userName = user.name
                        }
                        (Markup markup_)
        }


{-| Wait for given micro seconds.

It only affects Promises defined in [Tepa.Time](./Tepa-Time), so you should not use [`Time` module](https://package.elm-lang.org/packages/elm/time/latest/Time) and [`Process.sleep`](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep) with TEPA.\_

-}
sleep :
    Markup
    -> Int
    -> Scenario flags m
sleep (Markup markup_) msec =
    let
        description =
            markup_.content
    in
    Scenario
        { test =
            \_ context ->
                let
                    sessionResults =
                        Dict.foldl
                            (\k sessionContext ->
                                Result.andThen <|
                                    \acc ->
                                        let
                                            updateResult =
                                                advanceClock
                                                    context
                                                    msec
                                                    sessionContext
                                        in
                                        case updateResult of
                                            SessionExpired ->
                                                Ok <| Dict.remove k acc

                                            SessionUpdateFailed err ->
                                                Err err

                                            SessionUpdated nextSessionContext ->
                                                Ok <| Dict.insert k nextSessionContext acc
                            )
                            (Ok Dict.empty)
                            context.sessions
                in
                case sessionResults of
                    Err err ->
                        SeqTest.fail description <|
                            \_ -> Expect.fail err

                    Ok nextSessions ->
                        SeqTest.pass
                            { context
                                | sessions = nextSessions
                                , currentTime = context.currentTime + msec
                            }
        , markup =
            \_ ->
                appendMarkup (Markup markup_)
        }


advanceClock :
    TestContext m
    -> Int
    -> SessionContext m
    -> SessionUpdateResult m
advanceClock config msec context =
    -- IGNORE TCO
    case context.timers of
        [] ->
            SessionUpdated context

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
                    |> sessionUpdateAndThen
                        (advanceClock newConfig (msec - timer.runAfter))

            else
                SessionUpdated
                    { context
                        | timers =
                            List.map
                                (\t -> { t | runAfter = t.runAfter - msec })
                                context.timers
                    }



-- Response Simulators


{-| Simulate response to the `Tepa.portRequest` or `Tepa.listenPortStream`.

Suppose your application requests to access localStorage via port request named "Port to get page.account.bio":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After request to the port..."
        , portResponse sakuraChanMainSession
            (markup "Received response.")
            { layer = "Port to get page.account.bio"
            , response =
                \_ ->
                    JE.string "I'm Sakura-chan."
            }
        , Debug.todo "..."
        ]

During the initialization phase of the application, the `layer` parameter is ignored, so it is common to set `\_ -> Nothing` in such case.
After initialization, if no Layers are found for the query, it does nothing and just passes the test.

-}
portResponse :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , response : PortRequest -> Maybe Value
        }
    -> Scenario flags m
portResponse (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "portResponse: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            withLayerId : LayerId -> SeqTest.Sequence (TestContext (AppState flags m))
                            withLayerId layerId =
                                takeLastMatched
                                    (\p ->
                                        if p.layer == layerId then
                                            param.response
                                                { name = p.name
                                                , requestBody = p.requestBody
                                                }
                                                |> Maybe.map
                                                    (\resp ->
                                                        Core.PortResponseMsg
                                                            { requestId = p.request
                                                            , response = resp
                                                            }
                                                    )

                                        else
                                            Nothing
                                    )
                                    (sessionPorts sessionContext)
                                    |> Result.fromMaybe "portResponse: No listening ports found for the response."
                                    |> Result.andThen
                                        (\( msg, _ ) ->
                                            let
                                                updateResult =
                                                    update
                                                        context
                                                        msg
                                                        sessionContext
                                            in
                                            case updateResult of
                                                SessionExpired ->
                                                    Ok <|
                                                        SeqTest.pass
                                                            { context
                                                                | sessions =
                                                                    Dict.remove session.uniqueName
                                                                        context.sessions
                                                            }

                                                SessionUpdateFailed err ->
                                                    Err err

                                                SessionUpdated nextSessionContext ->
                                                    Ok <|
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
                        in
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                withLayerId (sessionLayerId sessionContext)

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        -- It is natural to receive responses after the Layer has expired.
                                        SeqTest.pass context

                                    Just (Core.Layer layer) ->
                                        withLayerId (Core.unwrapThisLayerId layer.id)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Port request:

  - `name` to identify the port, which you passed as `portName` field of `portRequest` or `portStream`.
  - `requestBody`, which you passed as `requestBody` field of `portRequest` or `portStream`.

-}
type alias PortRequest =
    { name : String
    , requestBody : Value
    }


{-| Simulate response to the [`Tepa.Random.request`](./Tepa-Random#request).

Suppose your application requests random integer:

    import Tepa.Random as Random
    import Tepa.Scenario as Scenario

    oneToTen : Random.Spec Int
    oneToTen =
        Random.int 1 10

    respondToOneToTenInt : Scenario flags m
    respondToOneToTenInt =
        Scenario.randomResponse
            mySession
            (Scenario.markup "Respond `1` to the first unresolved `oneToTen` request.")
            { layer = myLayer
            , spec = oneToTen
            , value = 1
            }

    -- If there is no unresolved `oneToTen` request at the time, the test fails.

During the initialization phase of the application, the `layer` parameter is ignored, so it is common to set `\_ -> Nothing` in such case.
After initialization, if no Layers are found for the query, it does nothing and just passes the test.

-}
randomResponse :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , spec : Random.Spec a
        , response : a
        }
    -> Scenario flags m
randomResponse (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "randomResponse: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            withLayerId : LayerId -> SeqTest.Sequence (TestContext (AppState flags m))
                            withLayerId layerId =
                                takeLastMatched
                                    (\( ( rid, lid ), req ) ->
                                        if lid == layerId && Core.isRequestForSpec param.spec req then
                                            Just rid

                                        else
                                            Nothing
                                    )
                                    sessionContext.randomRequests
                                    |> Result.fromMaybe "randomResponse: No requests found for the response."
                                    |> Result.andThen
                                        (\( rid, nextRandomRequests ) ->
                                            wrapBySpec param.spec param.response
                                                |> Result.map
                                                    (\resp ->
                                                        ( Core.RandomResponseMsg
                                                            { requestId = rid
                                                            , response = resp
                                                            }
                                                        , nextRandomRequests
                                                        )
                                                    )
                                        )
                                    |> Result.andThen
                                        (\( msg, nextRandomRequests ) ->
                                            let
                                                updateResult =
                                                    update
                                                        context
                                                        msg
                                                        { sessionContext
                                                            | randomRequests = nextRandomRequests
                                                        }
                                            in
                                            case updateResult of
                                                SessionExpired ->
                                                    Ok <|
                                                        SeqTest.pass
                                                            { context
                                                                | sessions =
                                                                    Dict.remove session.uniqueName
                                                                        context.sessions
                                                            }

                                                SessionUpdateFailed err ->
                                                    Err err

                                                SessionUpdated nextSessionContext ->
                                                    Ok <|
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
                        in
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                withLayerId (sessionLayerId sessionContext)

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        -- It is natural to receive responses after the Layer has expired.
                                        SeqTest.pass context

                                    Just (Core.Layer layer) ->
                                        withLayerId (Core.unwrapThisLayerId layer.id)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Simulate borwser forward event.

If there are no pages to forward, the test fails.

-}
forward :
    Session
    -> Markup
    -> Scenario flags m
forward (Session session) (Markup markup_) =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "forward: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case History.forward 1 sessionContext.history of
                            Nothing ->
                                SeqTest.fail description <|
                                    \_ ->
                                        Expect.fail
                                            "forward: No pages to forward."

                            Just newHistory ->
                                let
                                    updateResult =
                                        update context
                                            (Core.UrlChange <| History.current newHistory)
                                            { sessionContext
                                                | history = newHistory
                                            }
                                in
                                case updateResult of
                                    SessionExpired ->
                                        SeqTest.pass
                                            { context
                                                | sessions =
                                                    Dict.remove session.uniqueName
                                                        context.sessions
                                            }

                                    SessionUpdateFailed err ->
                                        SeqTest.fail description <|
                                            \_ -> Expect.fail err

                                    SessionUpdated nextSessionContext ->
                                        SeqTest.pass
                                            { context
                                                | sessions =
                                                    Dict.insert session.uniqueName
                                                        nextSessionContext
                                                        context.sessions
                                            }
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


{-| Simulate borwser back event.

If there are no pages to back, the test fails.

-}
back :
    Session
    -> Markup
    -> Scenario flags m
back (Session session) (Markup markup_) =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "back: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        case History.back 1 sessionContext.history of
                            Nothing ->
                                SeqTest.fail description <|
                                    \_ ->
                                        Expect.fail
                                            "back: No pages to back."

                            Just newHistory ->
                                let
                                    updateResult =
                                        update context
                                            (Core.UrlChange <| History.current newHistory)
                                            { sessionContext
                                                | history = newHistory
                                            }
                                in
                                case updateResult of
                                    SessionExpired ->
                                        SeqTest.pass
                                            { context
                                                | sessions =
                                                    Dict.remove session.uniqueName
                                                        context.sessions
                                            }

                                    SessionUpdateFailed err ->
                                        SeqTest.fail description <|
                                            \_ -> Expect.fail err

                                    SessionUpdated nextSessionContext ->
                                        SeqTest.pass
                                            { context
                                                | sessions =
                                                    Dict.insert session.uniqueName
                                                        nextSessionContext
                                                        context.sessions
                                            }
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
        }


wrapBySpec : Random.Spec a -> a -> Result String Core.RandomValue
wrapBySpec spec =
    case spec of
        Core.RandomSpecInt param ->
            param.wrap

        Core.RandomSpecFloat param ->
            param.wrap

        Core.RandomSpecEnum param ->
            param.wrap


{-| Simulate page transition.
-}
pushPath :
    Session
    -> AppUrl
    -> Markup
    -> Scenario flags m
pushPath (Session session) path (Markup markup_) =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "pushPath: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            newHistory =
                                History.push path sessionContext.history

                            updateResult =
                                update context
                                    (Core.UrlChange <| History.current newHistory)
                                    { sessionContext
                                        | history = newHistory
                                    }
                        in
                        case updateResult of
                            SessionExpired ->
                                SeqTest.pass
                                    { context
                                        | sessions =
                                            Dict.remove session.uniqueName
                                                context.sessions
                                    }

                            SessionUpdateFailed err ->
                                SeqTest.fail description <|
                                    \_ -> Expect.fail err

                            SessionUpdated nextSessionContext ->
                                SeqTest.pass
                                    { context
                                        | sessions =
                                            Dict.insert session.uniqueName
                                                nextSessionContext
                                                context.sessions
                                    }
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
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
fromJust : String -> Maybe a -> (a -> List (Scenario flags m)) -> Scenario flags m
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
fromOk : String -> Result err a -> (a -> List (Scenario flags m)) -> Scenario flags m
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
    { props : ApplicationProps flags memory
    , sections : List (Section flags memory)
    , origin :
        { secure : Bool
        , hostname : String
        , port_ : Maybe Int
        }
    }
    -> Test
toTest o =
    List.foldl
        (\sec acc ->
            let
                (Scenario { test }) =
                    sequence sec.content
            in
            SeqTest.andThen
                (\cache ->
                    (case sec.dependency of
                        EntryPoint zone initialTime ->
                            SeqTest.pass
                                { sessions = Dict.empty
                                , currentTime = Time.posixToMillis initialTime
                                , zone = zone
                                , view =
                                    \layer ->
                                        case layer.state of
                                            Core.AppLoading ->
                                                o.props.initView.body
                                                    |> Html.div []

                                            Core.AppLoaded flags state ->
                                                o.props.view flags state
                                                    |> .body
                                                    |> Html.div []
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
                                        \appState ->
                                            case appState of
                                                Core.AppLoading ->
                                                    o.props.initView

                                                Core.AppLoaded flags state ->
                                                    o.props.view flags state
                                    , init =
                                        \rawFlags initialPath ->
                                            let
                                                procs =
                                                    Tepa.bind
                                                        (o.props.init rawFlags
                                                            |> Core.liftPromiseMemory
                                                                { get = \_ -> ()
                                                                , set = \_ -> identity
                                                                }
                                                        )
                                                    <|
                                                        \( flags, memory ) ->
                                                            [ Tepa.modify <| \_ -> Core.AppLoaded flags memory
                                                            , Tepa.syncAll
                                                                [ Core.listenMsg <|
                                                                    \msg ->
                                                                        case msg of
                                                                            Core.UrlRequest req ->
                                                                                [ o.props.onUrlRequest flags (fromBrowserUrlRequest req) Core.SimKey
                                                                                ]

                                                                            Core.UrlChange newUrl ->
                                                                                [ o.props.onUrlChange flags newUrl Core.SimKey
                                                                                ]

                                                                            _ ->
                                                                                []
                                                                , o.props.onLoad flags initialPath Core.SimKey
                                                                ]
                                                                |> liftLoadedProcedure flags
                                                            ]

                                                newState =
                                                    Core.init Core.AppLoading procs
                                            in
                                            applyLogs
                                                context
                                                newState.logs
                                                { model = newState.nextModel
                                                , httpRequests = []
                                                , randomRequests = []
                                                , timers = []
                                                , history =
                                                    History.init initialPath
                                                }
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


liftLoadedProcedure : flags -> Tepa.Promise m () -> Tepa.Promise (Core.AppState flags m) ()
liftLoadedProcedure flags =
    Core.maybeLiftPromiseMemory
        { get =
            \m ->
                case m of
                    Core.AppLoading ->
                        Nothing

                    Core.AppLoaded _ a ->
                        Just a
        , set =
            \a _ ->
                Core.AppLoaded flags a
        }
        >> Tepa.void


fromBrowserUrlRequest : Browser.UrlRequest -> Tepa.UrlRequest
fromBrowserUrlRequest req =
    case req of
        Browser.Internal url ->
            Tepa.InternalPath <| AppUrl.fromUrl url

        Browser.External url ->
            Tepa.ExternalPage url


onUrlChange : AppUrl -> Msg
onUrlChange path =
    Core.UrlChange path


type SessionUpdateResult m
    = SessionExpired
    | SessionUpdateFailed String
    | SessionUpdated (SessionContext m)


sessionUpdateAndThen :
    (SessionContext m -> SessionUpdateResult m)
    -> SessionUpdateResult m
    -> SessionUpdateResult m
sessionUpdateAndThen f res =
    case res of
        SessionUpdated context ->
            f context

        _ ->
            res


applyMsgsTo :
    TestContext m
    -> SessionContext m
    -> List Msg
    -> SessionUpdateResult m
applyMsgsTo config context =
    List.foldl
        (\msg acc ->
            sessionUpdateAndThen
                (update config msg)
                acc
        )
        (SessionUpdated context)


update :
    TestContext m
    -> Msg
    -> SessionContext m
    -> SessionUpdateResult m
update config msg context =
    let
        newState =
            Core.update msg context.model
    in
    { model = newState.nextModel
    , httpRequests = context.httpRequests
    , randomRequests = context.randomRequests
    , timers = context.timers
    , history = context.history
    }
        |> applyLogs config newState.logs


applyLogs :
    TestContext m
    -> List Core.Log
    -> SessionContext m
    -> SessionUpdateResult m
applyLogs config logs context =
    List.foldl
        (\log acc ->
            sessionUpdateAndThen
                (applyLog config log)
                acc
        )
        (SessionUpdated context)
        logs


applyLog :
    TestContext m
    -> Core.Log
    -> SessionContext m
    -> SessionUpdateResult m
applyLog config log context =
    case log of
        Core.SetTimer rid lid msec ->
            SessionUpdated
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
            SessionUpdated
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

        Core.RequestCurrentZone rid ->
            update config
                (Core.CurrentZoneMsg
                    { requestId = rid
                    , zone = config.zone
                    }
                )
                context

        Core.IssueHttpRequest rid lid req ->
            SessionUpdated
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

        Core.IssueRandomRequest rid lid req ->
            SessionUpdated
                { context
                    | randomRequests =
                        ( ( rid, lid ), req ) :: context.randomRequests
                }

        Core.ResolveHttpRequest rid ->
            SessionUpdated
                { context
                    | httpRequests =
                        List.filter
                            (\( ( rid_, _ ), _ ) ->
                                rid_ /= rid
                            )
                            context.httpRequests
                }

        Core.ResolveRandomRequest rid ->
            SessionUpdated
                { context
                    | randomRequests =
                        List.filter
                            (\( ( rid_, _ ), _ ) ->
                                rid_ /= rid
                            )
                            context.randomRequests
                }

        Core.LayerHasExpired lid ->
            SessionUpdated
                { context
                    | httpRequests =
                        List.filter
                            (\( ( _, lid_ ), _ ) -> lid_ /= lid)
                            context.httpRequests
                    , randomRequests =
                        List.filter
                            (\( ( _, lid_ ), _ ) -> lid_ /= lid)
                            context.randomRequests
                    , timers =
                        List.filter
                            (\timer ->
                                timer.layerId /= lid
                            )
                            context.timers
                }

        Core.PushPath path ->
            update config
                (onUrlChange path)
                { context
                    | history = History.push path context.history
                }

        Core.ReplacePath path ->
            update config
                (onUrlChange path)
                -- replacePath does not effect browser history
                context

        Core.Back steps ->
            case History.back steps context.history of
                Nothing ->
                    SessionUpdateFailed
                        "back: No previous pages."

                Just newHistory ->
                    update config
                        (onUrlChange <| History.current newHistory)
                        { context
                            | history = newHistory
                        }

        Core.Forward steps ->
            case History.forward steps context.history of
                Nothing ->
                    SessionUpdateFailed
                        "forward: No next pages."

                Just newHistory ->
                    update config
                        (onUrlChange <| History.current newHistory)
                        { context
                            | history = newHistory
                        }

        Core.LoadUrl ->
            SessionExpired

        Core.Reload ->
            SessionExpired

        Core.FocusNode rid id ->
            let
                (Model model) =
                    context.model

                html =
                    config.view model.context.layer

                hasElement =
                    TestRunner.getFailureReason
                        (TestQuery.fromHtml html
                            |> TestQuery.findAll
                                [ TestSelector.id id
                                ]
                            |> TestQuery.count (Expect.greaterThan 0)
                        )
                        == Nothing

                resMsg =
                    TestQuery.fromHtml html
                        |> TestQuery.findAll
                            [ TestSelector.id id
                            ]
                        |> TestQuery.first
                        |> TestEvent.simulate TestEvent.focus
                        |> TestEvent.toResult
            in
            applyMsgsTo config context <|
                List.filterMap identity
                    [ Just <|
                        Core.FocusMsg
                            { requestId = rid
                            , targetId = id
                            , response =
                                if hasElement then
                                    Ok ()

                                else
                                    Err <| BrowserDom.NotFound id
                            }
                    , Result.toMaybe resMsg
                    ]

        Core.BlurNode rid id ->
            let
                (Model model) =
                    context.model

                html =
                    config.view model.context.layer

                hasElement =
                    TestRunner.getFailureReason
                        (TestQuery.fromHtml html
                            |> TestQuery.findAll
                                [ TestSelector.id id
                                ]
                            |> TestQuery.count (Expect.greaterThan 0)
                        )
                        == Nothing

                resMsg =
                    TestQuery.fromHtml html
                        |> TestQuery.findAll
                            [ TestSelector.id id
                            ]
                        |> TestQuery.first
                        |> TestEvent.simulate TestEvent.blur
                        |> TestEvent.toResult
            in
            applyMsgsTo config context <|
                List.filterMap identity
                    [ Just <|
                        Core.BlurMsg
                            { requestId = rid
                            , targetId = id
                            , response =
                                if hasElement then
                                    Ok ()

                                else
                                    Err <| BrowserDom.NotFound id
                            }
                    , Result.toMaybe resMsg
                    ]

        Core.RequestViewport rid ->
            update config
                (Core.RequestViewportMsg
                    { requestId = rid
                    , response =
                        { scene =
                            { width = 0
                            , height = 0
                            }
                        , viewport =
                            { x = 0
                            , y = 0
                            , width = 0
                            , height = 0
                            }
                        }
                    }
                )
                context

        Core.RequestViewportOf rid id ->
            let
                (Model model) =
                    context.model

                html =
                    config.view model.context.layer

                hasElement =
                    TestRunner.getFailureReason
                        (TestQuery.fromHtml html
                            |> TestQuery.findAll
                                [ TestSelector.id id
                                ]
                            |> TestQuery.count (Expect.greaterThan 0)
                        )
                        == Nothing
            in
            update config
                (Core.RequestViewportOfMsg
                    { requestId = rid
                    , targetId = id
                    , response =
                        if hasElement then
                            Ok
                                { scene =
                                    { width = 0
                                    , height = 0
                                    }
                                , viewport =
                                    { x = 0
                                    , y = 0
                                    , width = 0
                                    , height = 0
                                    }
                                }

                        else
                            Err <| BrowserDom.NotFound id
                    }
                )
                context

        Core.SetViewport rid ->
            update config
                (Core.RequestSetViewportMsg
                    { requestId = rid
                    }
                )
                context

        Core.SetViewportOf rid id ->
            let
                (Model model) =
                    context.model

                html =
                    config.view model.context.layer

                hasElement =
                    TestRunner.getFailureReason
                        (TestQuery.fromHtml html
                            |> TestQuery.findAll
                                [ TestSelector.id id
                                ]
                            |> TestQuery.count (Expect.greaterThan 0)
                        )
                        == Nothing

                resMsg =
                    TestQuery.fromHtml html
                        |> TestQuery.findAll
                            [ TestSelector.id id
                            ]
                        |> TestQuery.first
                        |> TestEvent.simulate
                            (TestEvent.custom
                                "scroll"
                                (JE.object [])
                            )
                        |> TestEvent.toResult
            in
            applyMsgsTo config context <|
                List.filterMap identity
                    [ Just <|
                        Core.RequestSetViewportOfMsg
                            { requestId = rid
                            , targetId = id
                            , response =
                                if hasElement then
                                    Ok ()

                                else
                                    Err <| BrowserDom.NotFound id
                            }
                    , Result.toMaybe resMsg
                    ]

        Core.RequestElement rid id ->
            let
                (Model model) =
                    context.model

                html =
                    config.view model.context.layer

                hasElement =
                    TestRunner.getFailureReason
                        (TestQuery.fromHtml html
                            |> TestQuery.findAll
                                [ TestSelector.id id
                                ]
                            |> TestQuery.count (Expect.greaterThan 0)
                        )
                        == Nothing
            in
            update config
                (Core.RequestElementMsg
                    { requestId = rid
                    , targetId = id
                    , response =
                        if hasElement then
                            Ok
                                { scene =
                                    { width = 0
                                    , height = 0
                                    }
                                , viewport =
                                    { x = 0
                                    , y = 0
                                    , width = 0
                                    , height = 0
                                    }
                                , element =
                                    { x = 0
                                    , y = 0
                                    , width = 0
                                    , height = 0
                                    }
                                }

                        else
                            Err <| BrowserDom.NotFound id
                    }
                )
                context

        Core.AssertionError str ->
            SessionUpdateFailed <|
                "Assertion Error: "
                    ++ str


putTimer : Timer -> List Timer -> List Timer
putTimer =
    putTimer_ []


putTimer_ : List Timer -> Timer -> List Timer -> List Timer
putTimer_ reversed new timers =
    case timers of
        [] ->
            new
                :: reversed
                |> List.reverse

        t :: ts ->
            if new.runAfter <= t.runAfter then
                List.reverse reversed ++ new :: t :: ts

            else
                putTimer_ (t :: reversed) new ts


{-| Generate scenario document markdown text.
-}
toMarkdown :
    { title : String
    , sections : List (Section flags m)
    , config : RenderConfig
    }
    -> Result InvalidMarkup String
toMarkdown o =
    List.foldl
        (\sec acc ->
            let
                (Scenario scenario) =
                    sequence sec.content
            in
            acc
                |> Result.andThen
                    (\( ( level, content ), titles ) ->
                        (if Set.member sec.title titles then
                            Err (DuplicatedSection sec.title)

                         else
                            ( level
                            , content
                                |> List.append
                                    [ ( level, "\n" ++ "## " ++ sec.title ++ "\n" )
                                    ]
                            )
                                |> (case sec.dependency of
                                        EntryPoint zone initialTime ->
                                            appendMarkup <|
                                                o.config.entryPointFirstListItem zone initialTime

                                        RunAfter dep ->
                                            if Set.member dep titles then
                                                appendMarkup <|
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

                                            else
                                                \_ -> Err (NoDependentSection dep)
                                   )
                        )
                            |> Result.andThen (scenario.markup o.config)
                            |> Result.map
                                (\x ->
                                    ( x, Set.insert sec.title titles )
                                )
                    )
        )
        (Ok
            ( ( 0
              , ( 0, "# " ++ o.title )
                    :: List.map
                        (\sec ->
                            ( 0
                            , String.concat
                                [ "- "
                                , "["
                                , sec.title
                                , "](#"
                                , String.words sec.title
                                    |> List.map
                                        (String.filter Char.isAlphaNum
                                            >> String.toLower
                                        )
                                    |> String.join "-"
                                , ")"
                                ]
                            )
                        )
                        o.sections
                    |> List.reverse
              )
            , Set.empty
            )
        )
        o.sections
        |> Result.map
            (\( ( _, ls ), _ ) ->
                List.map
                    (\( indents, line ) ->
                        String.repeat indents " " ++ line
                    )
                    ls
                    |> List.reverse
                    |> String.join "\n"
            )


{-| Configuration for rendering scenario.

  - entryPointFirstListItem: First item on the list for an `EntryPoint` scenario.
      - argument: The time when the scenario starts and the time zone
  - dependentScenarioFirstListItem: First item on the list for a `RunAfter` scenario.
      - argument: `href` and `name` for its dependency.
  - processSessionScenario: Processor for markups associated with a session.
  - processSystemScenario: Processor for markups associated with the specific system in a session.
  - processUserScenario: Processor for markups associated with the specific user in a session.

-}
type alias RenderConfig =
    { entryPointFirstListItem :
        Zone
        -> Posix
        -> Markup
    , dependentScenarioFirstListItem :
        { href : String
        , name : String
        }
        -> Markup
    , processSessionScenario :
        { uniqueSessionName : String }
        -> Markup
        -> Markup
    , processSystemScenario :
        { uniqueSessionName : String }
        -> Markup
        -> Markup
    , processUserScenario :
        { uniqueSessionName : String
        , userName : String
        }
        -> Markup
        -> Markup
    }


{-| Standard configuration for ja\_JP.
-}
ja_JP : RenderConfig
ja_JP =
    { entryPointFirstListItem =
        \zone posix ->
            let
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
            markup """
            （{{year}}/{{month}}/{{day}} {{hour}}:{{minute}}:{{second}}）
            """
                |> setParam "year" (String.fromInt year)
                |> setParam "month"
                    (monthIndex month
                        |> String.fromInt
                    )
                |> setParam "day" (String.fromInt day)
                |> setParam "hour" (String.fromInt hour)
                |> setParam "minute"
                    (String.fromInt minute
                        |> String.padLeft 2 '0'
                    )
                |> setParam "second"
                    (String.fromInt second
                        |> String.padLeft 2 '0'
                    )
    , dependentScenarioFirstListItem =
        \o ->
            markup """
            （「[{{name}}]({{href|raw}})」の直後）
            """
                |> setParam "name" o.name
                |> setParam "href" o.href
    , processSessionScenario = prependSessionName
    , processSystemScenario = prependSessionSystemName
    , processUserScenario = prependSessionAndUserName
    }


prependSessionName :
    { uniqueSessionName : String }
    -> Markup
    -> Markup
prependSessionName { uniqueSessionName } =
    modifyContent <| \content -> "**[" ++ uniqueSessionName ++ "]** " ++ content


prependSessionSystemName :
    { uniqueSessionName : String }
    -> Markup
    -> Markup
prependSessionSystemName { uniqueSessionName } =
    modifyContent <| \content -> "**[" ++ uniqueSessionName ++ "]** " ++ content


prependSessionAndUserName :
    { uniqueSessionName : String
    , userName : String
    }
    -> Markup
    -> Markup
prependSessionAndUserName { uniqueSessionName, userName } =
    modifyContent <| \content -> "**[" ++ uniqueSessionName ++ "]** **" ++ userName ++ "**: " ++ content


{-| Standard configuration for en\_US.
-}
en_US : RenderConfig
en_US =
    { entryPointFirstListItem =
        \zone posix ->
            let
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
            markup """
            {{month}}/{{day}}/{{year}} {{hour}}:{{minute}}:{{second}}
            """
                |> setParam "year" (String.fromInt year)
                |> setParam "month"
                    (monthIndex month
                        |> String.fromInt
                    )
                |> setParam "day" (String.fromInt day)
                |> setParam "hour" (String.fromInt hour)
                |> setParam "minute"
                    (String.fromInt minute
                        |> String.padLeft 2 '0'
                    )
                |> setParam "second"
                    (String.fromInt second
                        |> String.padLeft 2 '0'
                    )
    , dependentScenarioFirstListItem =
        \o ->
            markup """
            Just after "[{{name}}]({{href|raw}})"
            """
                |> setParam "name" o.name
                |> setParam "href" o.href
    , processSessionScenario = prependSessionName
    , processSystemScenario = prependSessionSystemName
    , processUserScenario = prependSessionAndUserName
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


{-| Simulate response to the [`Tepa.Http.request`](./Tepa-Http#request) and [`Tepa.Http.bytesRequest`](./Tepa-Http#bytesRequest).

During the initialization phase of the application, the `layer` parameter is ignored, so it is common to set `\_ -> Nothing` in such case.
After initialization, if no Layers are found for the query, it does nothing and just passes the test.

-}
httpResponse :
    Session
    -> Markup
    ->
        { layer : Layer m -> Maybe (Layer m1)
        , response : HttpRequest -> Maybe ( Http.Metadata, String )
        }
    -> Scenario flags m
httpResponse (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "httpResponse: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            withLayerId : LayerId -> SeqTest.Sequence (TestContext (AppState flags m))
                            withLayerId layerId =
                                takeLastMatched
                                    (\( ( rid, lid ), req ) ->
                                        if lid == layerId then
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
                                            let
                                                updateResult =
                                                    update context
                                                        msg
                                                        { sessionContext
                                                            | httpRequests = nextHttpRequests
                                                        }
                                            in
                                            case updateResult of
                                                SessionExpired ->
                                                    Ok <|
                                                        SeqTest.pass
                                                            { context
                                                                | sessions =
                                                                    Dict.remove session.uniqueName
                                                                        context.sessions
                                                            }

                                                SessionUpdateFailed err ->
                                                    Err err

                                                SessionUpdated nextSessionContext ->
                                                    Ok <|
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
                        in
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                withLayerId (sessionLayerId sessionContext)

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        -- It is natural to receive responses after the Layer has expired.
                                        SeqTest.pass context

                                    Just (Core.Layer layer) ->
                                        withLayerId (Core.unwrapThisLayerId layer.id)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
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
    -> Scenario flags m
httpBytesResponse (Session session) (Markup markup_) param =
    let
        description =
            "[" ++ session.uniqueName ++ "] " ++ markup_.content
    in
    Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context.sessions of
                    Nothing ->
                        SeqTest.fail description <|
                            \_ ->
                                Expect.fail
                                    "httpResponse: The application is not active in the session. Use `loadApp` beforehand."

                    Just sessionContext ->
                        let
                            withLayerId : LayerId -> SeqTest.Sequence (TestContext (AppState flags m))
                            withLayerId layerId =
                                takeLastMatched
                                    (\( ( rid, lid ), req ) ->
                                        if lid == layerId then
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
                                            let
                                                updateResult =
                                                    update context
                                                        msg
                                                        { sessionContext
                                                            | httpRequests = nextHttpRequests
                                                        }
                                            in
                                            case updateResult of
                                                SessionExpired ->
                                                    Ok <|
                                                        SeqTest.pass
                                                            { context
                                                                | sessions =
                                                                    Dict.remove session.uniqueName context.sessions
                                                            }

                                                SessionUpdateFailed err ->
                                                    Err err

                                                SessionUpdated nextSessionContext ->
                                                    Ok <|
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
                        in
                        case maybeLoadedLayer sessionContext of
                            Nothing ->
                                withLayerId (sessionLayerId sessionContext)

                            Just loadedLayer ->
                                case param.layer loadedLayer of
                                    Nothing ->
                                        -- It is natural to receive responses after the Layer has expired.
                                        SeqTest.pass context

                                    Just (Core.Layer layer) ->
                                        withLayerId (Core.unwrapThisLayerId layer.id)
        , markup =
            \config ->
                appendMarkup <|
                    config.processSystemScenario
                        { uniqueSessionName = session.uniqueName }
                        (Markup markup_)
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
