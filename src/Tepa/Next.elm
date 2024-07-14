module Tepa.Next exposing
    ( map2, map3, mapAll
    , layerView
    , ViewContext
    , pushNamespace
    , setKey, setKeyAndId, fullKeyNameFor, values, checks_
    , valueFor, checkFor_
    , checks, checkFor
    )

{-| Provide functionality that will be introduced or changed in the next version.


### New functions for convenience

@docs map2, map3, mapAll


### Pseudo-namespace for keys

You may sometimes find yourself using an element many times in a page. In such cases, it is useful to give key a pseudo-namespace.

In the following bad example, a View called `userCard` is reused many times.

    import Tepa exposing (ViewContext)
    import Tepa.Html as Html exposing (Html)
    import Tepa.Mixin as Mixin

    userCards :
        { users : List User
        }
        -> ViewContext
        -> Html
    userCards param context =
        Html.node "ol"
            []
            ( List.map
                (\user ->
                    userCard
                        { user = user
                        }
                        context
                )
                param.users
            )

    userCard :
        { user : User
        }
        -> ViewContext
        -> Html
    userCard param ({ setKey } as context) =
        Html.div
            [ Mixin.class "userCard"
            ]
            [ if user.isEditing then
                userNameForm param context
              else
                Html.div []
                    [ Html.span
                        [ Mixin.class "userCard_name"
                        ]
                        [ Html.text param.user.name
                        ]
                    , Html.button
                        [ Mixin.class "userCard_editButton"
                        , Mixin.attribute "type" "button"
                        , setKey "editButton"
                        ]
                        [ Html.text "Edit"
                        ]
                    ]
            ]

    type alias User = {
        id : String
        name : String
        isEditing : Bool
    }

This will result in duplicate use of the key named "editButton", which cannot be manipulated properly from the Procedure. Furthermore, since the name "editButton" is too generic, it is possible that a key with the same name is accidentally used in a completely unrelated location.
In such cases, pseudo-namespace is useful. When passing a `ViewContext` from a parent element to a child element, you can push unique prefix string for it:

    import Tepa
    import Tepa.Html as Html exposing (Html)
    import Tepa.Mixin as Mixin
    import Tepa.Next as Tepa exposing (ViewContext)

    userCards param context =
        Html.node "ol"
            []
            (List.map
                (\user ->
                    userCard
                        { user = user
                        }
                        (Tepa.pushNamespace ("userCard_" ++ user.id) context)
                )
                param.users
            )

    userCard param context =
        Html.div
            [ Mixin.class "userCard"
            ]
            [ if user.isEditing then
                userNameForm param context

              else
                Html.div []
                    [ Html.span
                        [ Mixin.class "userCard_name"
                        ]
                        [ Html.text param.user.name
                        ]
                    , Html.button
                        [ Mixin.class "userCard_editButton"
                        , Mixin.attribute "type" "button"
                        , Tepa.setKey context ".editButton"
                        ]
                        [ Html.text "Edit"
                        ]
                    ]
            ]

In the above example, the actual key name given to the edit button would be "userCard\_user01.editButton"; that is, you would be able to access the element from the Procedure as follows:

    Procedure.awaitViewEvent
        { key = "userCard_user01.editButton"
        , type_ = "click"
        }

The `pushNamespace` can be nested; for example, you can use `pushNamespace` further within `userCard` as follows:

    userCard param context =
        Html.div
            [ Mixin.class "userCard"
            ]
            [ if user.isEditing then
                userNameForm
                    param
                    (Tepa.pushNamespace ".userNameForm" context)

              else
                Html.div []
                    [ Html.span
                        [ Mixin.class "userCard_name"
                        ]
                        [ Html.text param.user.name
                        ]
                    , Html.button
                        [ Mixin.class "userCard_editButton"
                        , Mixin.attribute "type" "button"
                        , Tepa.setKey context ".editButton"
                        ]
                        [ Html.text "Edit"
                        ]
                    ]
            ]

In this case, using `setKey context ".foo"` in `userNameForm` will actually give the key name `userCard_user01.userNameForm.foo`.

@docs layerView
@docs ViewContext
@docs pushNamespace
@docs setKey, setKeyAndId, fullKeyNameFor, values, checks_
@docs valueFor, checkFor_

DEPRECATED

@docs checks, checkFor

-}

import Dict exposing (Dict)
import Mixin
import Tepa exposing (Promise)


{-| Run two Promises concurrently, and append a function to the results when both are complete.
-}
map2 :
    (a -> b -> c)
    -> Promise m a
    -> Promise m b
    -> Promise m c
map2 f promA promB =
    Tepa.succeed f
        |> Tepa.sync promA
        |> Tepa.sync promB


{-| Run three Promises concurrently, and append a function to the results when all are complete.

If you need to handle more Promises, use `mapAll` or `sync`.

-}
map3 :
    (a -> b -> c -> d)
    -> Promise m a
    -> Promise m b
    -> Promise m c
    -> Promise m d
map3 f promA promB promC =
    Tepa.succeed f
        |> Tepa.sync promA
        |> Tepa.sync promB
        |> Tepa.sync promC


{-| Run Promises concurrently, and apply a function to the results when all are complete.
-}
mapAll :
    (List a -> b)
    -> List (Promise m a)
    -> Promise m b
mapAll f ps =
    List.foldl
        (\p acc ->
            Tepa.succeed (::)
                |> Tepa.sync p
                |> Tepa.sync acc
        )
        (Tepa.succeed [])
        ps
        |> Tepa.map f


{-| Context about the current View, including its namespace.
-}
type ViewContext
    = ViewContext
        { setKey : String -> Tepa.Mixin
        , values : Dict String String
        , checks : Dict String Bool
        , prefix : String
        }


{-| -}
layerView :
    (ViewContext -> m -> view)
    -> Tepa.Layer m
    -> view
layerView f =
    Tepa.layerView
        (\legacy ->
            f
                (ViewContext
                    { setKey = legacy.setKey
                    , values = legacy.values
                    , checks = legacy.checks
                    , prefix = ""
                    }
                )
        )


{-| Push namespace prefix for the child element.
-}
pushNamespace : String -> ViewContext -> ViewContext
pushNamespace prefix (ViewContext context) =
    ViewContext
        { context
            | prefix = context.prefix ++ prefix
        }


{-| Set a key to the element.
The key is automatically combined with a prefix that serves as a namespace.
-}
setKey : ViewContext -> String -> Tepa.Mixin
setKey (ViewContext context) key =
    context.setKey (context.prefix ++ key)


{-| Returns the entire key name prefixed with the namespace for the View.
-}
fullKeyNameFor : ViewContext -> String -> String
fullKeyNameFor (ViewContext context) key =
    context.prefix ++ key


{-| Current values of the control elements, keyed by its key strings set with `setKey`.
-}
values : ViewContext -> Dict String String
values (ViewContext context) =
    context.values


{-| Current value of the control element specified by the key.
-}
valueFor : ViewContext -> String -> Maybe String
valueFor (ViewContext context) key =
    context.values
        |> Dict.get (context.prefix ++ key)


{-| DEPRECATED
-}
checks : ViewContext -> Dict String String
checks (ViewContext context) =
    Dict.map
        (\_ v ->
            if v then
                "true"

            else
                "false"
        )
        context.checks


{-| Current check state of the radio/check elements, keyed by its key strings set with `setKey`.
-}
checks_ : ViewContext -> Dict String Bool
checks_ (ViewContext context) =
    context.checks


{-| DEPRECATED
-}
checkFor : ViewContext -> String -> Maybe String
checkFor (ViewContext context) key =
    context.checks
        |> Dict.get (context.prefix ++ key)
        |> Maybe.map
            (\b ->
                if b then
                    "true"

                else
                    "false"
            )


{-| Current check state of the radio/check element specified by the key.
-}
checkFor_ : ViewContext -> String -> Maybe Bool
checkFor_ (ViewContext context) key =
    context.checks
        |> Dict.get (context.prefix ++ key)


{-| Helper function to set the value of the same name to the HTML ID attribute value as well as key for convenience.

    setKeyAndId context key =
        Mixin.batch
            [ setKey context key
            , Mixin.id (fullKeyNameFor context key)
            ]

-}
setKeyAndId : ViewContext -> String -> Tepa.Mixin
setKeyAndId context key =
    Mixin.batch
        [ setKey context key
        , Mixin.id (fullKeyNameFor context key)
        ]
