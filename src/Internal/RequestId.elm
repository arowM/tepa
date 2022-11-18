module Internal.RequestId exposing
    ( RequestId
    , init
    , inc
    , decoder
    , toValue
    )

{-|

@docs RequestId
@docs init
@docs inc
@docs decoder
@docs toValue

-}

import Internal.SafeInt as SafeInt exposing (SafeInt)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| -}
type RequestId
    = RequestId (List SafeInt)


{-| Initial value.
-}
init : RequestId
init =
    RequestId [ SafeInt.minBound ]


{-| -}
inc : RequestId -> RequestId
inc (RequestId ls) =
    case incList ls of
        ( True, new ) ->
            RequestId <| SafeInt.minBound :: new

        ( False, new ) ->
            RequestId new


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
decoder : Decoder RequestId
decoder =
    JD.list SafeInt.decoder
        |> JD.map RequestId


{-| -}
toValue : RequestId -> Value
toValue (RequestId sis) =
    JE.list SafeInt.toValue sis
