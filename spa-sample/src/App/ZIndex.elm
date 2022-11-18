module App.ZIndex exposing (toast)

{-| Declare z-index values.

@docs toast

-}

-- Overlay


{-| -}
mask : Int
mask =
    above baseOverlay


{-| -}
toast : Int
toast =
    above mask



-- Helper functions


above : Int -> Int
above n =
    n + 1


baseOverlay : Int
baseOverlay =
    1000
