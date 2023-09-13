module Widget.Header exposing (view)

{-| Shared views between pages.

@docs view

-}

import App.Path as Path
import AppUrl
import Dict
import Mixin exposing (Mixin)
import Mixin.Html as Html exposing (Html)
import Tepa exposing (Msg)


{-| Page header.
-}
view : Mixin Msg -> Html Msg
view extra =
    Html.div
        [ extra
        , localClass "header"
        ]
        [ Html.a
            [ localClass "header_logo"
            , Mixin.attribute "href" <|
                AppUrl.toString <|
                    { path = [ Path.prefix ]
                    , queryParameters = Dict.empty
                    , fragment = Nothing
                    }
            ]
            [ Html.text "Goat Chat"
            ]
        ]


localClass : String -> Mixin msg
localClass name =
    Mixin.class (pagePrefix ++ name)


pagePrefix : String
pagePrefix =
    "widget_header--"
