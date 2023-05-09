module Internal.LayerId exposing
    ( LayerId
    , init
    , inc
    , toString
    )

{-|

@docs LayerId
@docs init
@docs inc
@docs decoder
@docs toString
@docs toValue

-}

import Internal.SafeInt as SafeInt exposing (SafeInt)


{-| -}
type LayerId
    = LayerId (List SafeInt)


{-| Initial value.
-}
init : LayerId
init =
    LayerId [ SafeInt.minBound ]


{-| -}
inc : LayerId -> LayerId
inc (LayerId ls) =
    case incList ls of
        ( True, new ) ->
            LayerId <| SafeInt.minBound :: new

        ( False, new ) ->
            LayerId new


incList : List SafeInt -> ( Bool, List SafeInt )
incList =
    List.foldr
        (\a ( carry, ls ) ->
            if carry then
                SafeInt.inc a
                    |> Tuple.mapSecond (\new -> new :: ls)

            else
                ( False, a :: ls )
        )
        ( True, [] )


{-| -}
toString : LayerId -> String
toString (LayerId ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "lid_" ++ str)
