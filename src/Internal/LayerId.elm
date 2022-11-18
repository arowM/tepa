module Internal.LayerId exposing
    ( LayerId
    , init
    , inc
    , decoder
    , toString
    , toValue
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
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


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


{-| -}
decoder : Decoder LayerId
decoder =
    JD.list SafeInt.decoder
        |> JD.map LayerId


{-| -}
toValue : LayerId -> Value
toValue (LayerId ls) =
    JE.list SafeInt.toValue ls
