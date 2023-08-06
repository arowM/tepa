module Tepa.Dom exposing
    ( focus, blur, Error(..)
    , currentViewport, Viewport, currentViewportOf
    , setViewport, setViewportOf
    , element, Element
    )

{-| This module allows you to manipulate the DOM in various ways.

It covers:

  - Focus and blur input elements.
  - Get the width and height of elements.
  - Get the x and y coordinates of elements.
  - Figure out the scroll position.
  - Change the scroll position!

See the `Browser.Dom` documentation for more detailed notes.


# Focus

@docs focus, blur, Error


# Get Viewport

@docs currentViewport, Viewport, currentViewportOf


# Set Viewport

@docs setViewport, setViewportOf


# Position

@docs element, Element

-}

import Browser.Dom as Dom
import Internal.Core as Core
import Task
import Tepa exposing (Promise)



-- Focus


{-| Find a DOM node by `id` and focus on it. So if you wanted to focus a node like `<input type="text" id="search-box">` you could say:

    import Tepa exposing (Promise)
    import Tepa.Dom as Dom

    focusSearchBox : Promise m (Result Dom.Error ())
    focusSearchBox =
        Dom.focus "search-box"

In scenario testing, it also simulates the focus event with an empty event object.

_This is the TEPA version of [`Browser.Dom.focus`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#focus)._

-}
focus : String -> Promise m (Result Error ())
focus str =
    Core.customRequest
        (\myRequestId msg _ ->
            case msg of
                Core.FocusMsg param ->
                    if param.requestId == myRequestId then
                        Just
                            ( param.response
                                |> Result.mapError
                                    (\(Dom.NotFound id) -> NotFound id)
                            , []
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        (\myRequestId ->
            Task.attempt
                (\res ->
                    Core.FocusMsg
                        { requestId = myRequestId
                        , targetId = str
                        , response = res
                        }
                )
                (Dom.focus str)
        )
        (\myRequestId _ ->
            Core.FocusNode myRequestId str
        )


{-| Find a DOM node by `id` and make it lose focus. So if you wanted a node
like `<input type="text" id="search-box">` to lose focus you could say:

    import Tepa exposing (Promise)
    import Tepa.Dom as Dom

    unfocusSearchBox : Promise m (Result Dom.Error ())
    unfocusSearchBox =
        Dom.blur "search-box"

In scenario testing, it also simulates the blur event with an empty event object.

_This is the TEPA version of [`Browser.Dom.blur`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#blur)._

-}
blur : String -> Promise m (Result Error ())
blur str =
    Core.customRequest
        (\myRequestId msg _ ->
            case msg of
                Core.BlurMsg param ->
                    if param.requestId == myRequestId then
                        Just
                            ( param.response
                                |> Result.mapError
                                    (\(Dom.NotFound id) -> NotFound id)
                            , []
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        (\myRequestId ->
            Task.attempt
                (\res ->
                    Core.BlurMsg
                        { requestId = myRequestId
                        , targetId = str
                        , response = res
                        }
                )
                (Dom.blur str)
        )
        (\myRequestId _ ->
            Core.BlurNode myRequestId str
        )


{-| Many functions in this module look up DOM nodes up by their `id`. If you
ask for an `id` that is not in the DOM, you will get this error.

_This is the TEPA version of [`Browser.Dom.Error`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Error)._

-}
type Error
    = NotFound String



-- Get Viewport


{-| Get information on the current viewport of the browser.

![getViewport](https://elm.github.io/browser/v1/getViewport.svg)

(Source: [Browser.Dom module document](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom))

If you want to move the viewport around (i.e. change the scroll position) you
can use [`setViewport`](#setViewport) which change the `x` and `y` of the
viewport.

In scenario testing, it always resolves to Viewport of all values zero.

_This is the TEPA version of [`Browser.Dom.getViewport`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewport)._

-}
currentViewport : Promise m Viewport
currentViewport =
    Core.customRequest
        (\myRequestId msg _ ->
            case msg of
                Core.RequestViewportMsg param ->
                    if param.requestId == myRequestId then
                        Just
                            ( param.response
                            , []
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        (\myRequestId ->
            Task.perform
                (\resp ->
                    Core.RequestViewportMsg
                        { requestId = myRequestId
                        , response = resp
                        }
                )
                Dom.getViewport
        )
        (\myRequestId _ ->
            Core.RequestViewport myRequestId
        )


{-| Same as [Browser.Dom.Viewport](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Viewport)

All the information about the current viewport.

![getViewport](https://elm.github.io/browser/v1/getViewport.svg)

(Source: [Browser.Dom module document](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom))

-}
type alias Viewport =
    { scene :
        { width : Float
        , height : Float
        }
    , viewport :
        { x : Float
        , y : Float
        , width : Float
        , height : Float
        }
    }


{-| Just like `currentViewport`, but for any scrollable DOM node.

See [`Browser.Dom.getViewportOf`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewportOf) for detail and real use cases.

_This is the TEPA version of [`Browser.Dom.getViewportOf`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewportOf)._

-}
currentViewportOf : String -> Promise m (Result Error Viewport)
currentViewportOf str =
    Core.customRequest
        (\myRequestId msg _ ->
            case msg of
                Core.RequestViewportOfMsg param ->
                    if param.requestId == myRequestId then
                        Just
                            ( param.response
                                |> Result.mapError
                                    (\(Dom.NotFound id) -> NotFound id)
                            , []
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        (\myRequestId ->
            Task.attempt
                (\res ->
                    Core.RequestViewportOfMsg
                        { requestId = myRequestId
                        , targetId = str
                        , response = res
                        }
                )
                (Dom.getViewportOf str)
        )
        (\myRequestId _ ->
            Core.RequestViewportOf myRequestId str
        )



-- Set Viewport


{-| Change the `x` and `y` offset of the browser viewport immediately. For
example, you could make a command to jump to the top of the page:

    import Tepa exposing (Promise)
    import Tepa.Dom as Dom

    resetViewport : Promise m ()
    resetViewport =
        Dom.setViewport 0 0

This sets the viewport offset to zero.

In scenario testing, it has no effects.

_This is the TEPA version of [`Browser.Dom.setViewport`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#setViewport)._

-}
setViewport : Float -> Float -> Promise m ()
setViewport x y =
    Core.customRequest
        (\myRequestId msg _ ->
            case msg of
                Core.RequestSetViewportMsg param ->
                    if param.requestId == myRequestId then
                        Just
                            ( ()
                            , []
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        (\myRequestId ->
            Task.perform
                (\_ ->
                    Core.RequestSetViewportMsg
                        { requestId = myRequestId
                        }
                )
                (Dom.setViewport x y)
        )
        (\myRequestId _ ->
            Core.SetViewport myRequestId
        )


{-| Change the `x` and `y` offset of a DOM node&rsquo;s viewport by ID. This
is common in text messaging and chat rooms, where once the messages fill the
screen, you want to always be at the very bottom of the message chain. This
way the latest message is always on screen! You could do this:

    import Tepa exposing (Promise)
    import Tepa.Dom as Dom

    jumpToBottom : String -> Promise m (Result Dom.Error ())
    jumpToBottom id =
        Tepa.bindAndThen (Dom.currentViewportOf id) <|
            \res ->
                case res of
                    Err err ->
                        Tepa.succeed res

                    Ok info ->
                        Dom.setViewportOf id 0 info.scene.height

So you could call `jumpToBottom "chat-box"` whenever you add a new message.

See [`Browser.Dom.setViewportOf`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#setViewportOf) for more detailed notes.

In scenario testing, it also simulates the scroll event with an empty event object on the target node.

_This is the TEPA version of [`Browser.Dom.setViewportOf`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#setViewportOf)._

-}
setViewportOf : String -> Float -> Float -> Promise m (Result Error ())
setViewportOf id x y =
    Core.customRequest
        (\myRequestId msg _ ->
            case msg of
                Core.RequestSetViewportOfMsg param ->
                    if param.requestId == myRequestId then
                        Just
                            ( param.response
                                |> Result.mapError
                                    (\(Dom.NotFound id_) -> NotFound id_)
                            , []
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        (\myRequestId ->
            Task.attempt
                (\res ->
                    Core.RequestSetViewportOfMsg
                        { requestId = myRequestId
                        , targetId = id
                        , response = res
                        }
                )
                (Dom.setViewportOf id x y)
        )
        (\myRequestId _ ->
            Core.SetViewportOf myRequestId id
        )


{-| Get position information about specific elements.

See [`Browser.Dom.getElement`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getElement) for detail and real use cases.

In scenario testing, it always resolves to Element of all values zero.

_This is the TEPA version of [`Browser.Dom.getElement`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getElement)._

-}
element : String -> Promise m (Result Error Element)
element str =
    Core.customRequest
        (\myRequestId msg _ ->
            case msg of
                Core.RequestElementMsg param ->
                    if param.requestId == myRequestId then
                        Just
                            ( param.response
                                |> Result.mapError
                                    (\(Dom.NotFound id) -> NotFound id)
                            , []
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        (\myRequestId ->
            Task.attempt
                (\res ->
                    Core.RequestElementMsg
                        { requestId = myRequestId
                        , targetId = str
                        , response = res
                        }
                )
                (Dom.getElement str)
        )
        (\myRequestId _ ->
            Core.RequestElement myRequestId str
        )


{-| Same as [Browser.Dom.Element](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Element)

A bunch of information about the position and size of an element relative
to the overall scene.

![getElement](https://elm.github.io/browser/v1/getElement.svg)

(Source: [Browser.Dom module document](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom))

-}
type alias Element =
    { scene :
        { width : Float
        , height : Float
        }
    , viewport :
        { x : Float
        , y : Float
        , width : Float
        , height : Float
        }
    , element :
        { x : Float
        , y : Float
        , width : Float
        , height : Float
        }
    }
