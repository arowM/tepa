module App.Session exposing
    ( Session
    , Profile
    )

{-|

@docs Session
@docs Profile

-}


{-| Application-wide state.
-}
type alias Session =
    { profile : Profile
    }


{-| Data for logged in user.
-}
type alias Profile =
    { id : String
    , name : Maybe String
    }
