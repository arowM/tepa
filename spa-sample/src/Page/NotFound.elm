module Page.NotFound exposing
    ( Memory
    , init
    , leave
    , procedure
    , view
    )

{-| Not found page.

@docs Memory
@docs init
@docs leave
@docs procedure
@docs view

-}

import App.Path as Path
import App.Session exposing (Session)
import AppUrl exposing (AppUrl)
import Dict
import Mixin exposing (Mixin)
import Mixin.Html as Html exposing (Html)
import Tepa exposing (Layer, Msg, NavKey, Promise)


{-| -}
type alias Memory =
    { msession : Maybe Session
    }


{-| -}
init : Maybe Session -> Promise m Memory
init msession =
    Tepa.succeed
        { msession = msession
        }


{-| -}
leave : Promise Memory (Maybe Session)
leave =
    Tepa.currentState
        |> Tepa.map .msession



-- View


{-| -}
view : Layer Memory -> Html Msg
view =
    Tepa.layerView <|
        \_ ->
            Html.div
                [ localClass "page"
                ]
                [ Html.div
                    [ localClass "mainMessage"
                    ]
                    [ Html.text "Page Not Found."
                    ]
                , Html.a
                    [ Mixin.attribute "href" <|
                        AppUrl.toString
                            { path =
                                [ Path.prefix
                                ]
                            , queryParameters = Dict.empty
                            , fragment = Nothing
                            }
                    , localClass "homeLink"
                    ]
                    [ Html.text "Home"
                    ]
                ]



-- Procedures


{-| -}
procedure : NavKey -> AppUrl -> Promise Memory ()
procedure _ _ =
    Tepa.none



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class (pagePrefix ++ name)


pagePrefix : String
pagePrefix =
    "page_notFound--"
