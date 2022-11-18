module Tepa.Scenario.LayerQuery exposing
    ( LayerQuery
    , self
    , child
    , children
    , filter
    , index
    , indices
    )

{-| Querying Layer.


# Core

@docs LayerQuery
@docs self


# Queries

@docs child
@docs children
@docs filter
@docs index
@docs indices

-}

import Internal.Core as Core exposing (Layer(..), LayerQuery(..))


{-| -}
type alias LayerQuery m m1 =
    Core.LayerQuery m m1


{-| -}
self : LayerQuery m m
self =
    LayerQuery
        { get = List.singleton
        }


{-| -}
child : (m1 -> Maybe (Layer m2)) -> LayerQuery m m1 -> LayerQuery m m2
child f (LayerQuery query) =
    LayerQuery
        { get =
            \lm ->
                query.get lm
                    |> List.filterMap
                        (\(Layer _ m1) -> f m1)
        }


{-| -}
children : (m1 -> List (Layer m2)) -> LayerQuery m m1 -> LayerQuery m m2
children f (LayerQuery query) =
    LayerQuery
        { get =
            \lm ->
                query.get lm
                    |> List.concatMap
                        (\(Layer _ m1) -> f m1)
        }


{-| -}
filter : (m1 -> Bool) -> LayerQuery m m1 -> LayerQuery m m1
filter p (LayerQuery query) =
    LayerQuery
        { get =
            \lm ->
                query.get lm
                    |> List.filter
                        (\(Layer _ m1) -> p m1)
        }


{-| -}
index : Int -> LayerQuery m m1 -> LayerQuery m m1
index n (LayerQuery query) =
    LayerQuery
        { get =
            \lm ->
                query.get lm
                    |> List.indexedMap
                        (\idx a ->
                            if idx == n then
                                Just a

                            else
                                Nothing
                        )
                    |> List.filterMap identity
        }


{-| -}
indices : List Int -> LayerQuery m m1 -> LayerQuery m m1
indices ns (LayerQuery query) =
    LayerQuery
        { get =
            \lm ->
                query.get lm
                    |> List.indexedMap
                        (\idx a ->
                            if List.member idx ns then
                                Just a

                            else
                                Nothing
                        )
                    |> List.filterMap identity
        }
