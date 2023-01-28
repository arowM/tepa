module Internal.ResponseType exposing
    ( ResponseType
    , ResponseBody
    , encode, decode
    , string, int, float, bool, unit, value, bytes, maybe, result, tuple, list
    , httpError, httpMetadata, httpResponse
    , zone
    , RecordType, record, field, fromRecordType
    , variant
    )

{-|

@docs ResponseType
@docs ResponseBody
@docs encode, decode


# Primitives

@docs string, int, float, bool, unit, value, bytes, maybe, result, tuple, list


# Common Types

@docs httpError, httpMetadata, httpResponse
@docs zone


# Record Type

@docs RecordType, record, field, fromRecordType


# Variant

@docs variant

-}

import Bytes exposing (Bytes)
import Http
import Json.Encode exposing (Value)
import Time


{-| -}
type ResponseType a
    = ResponseType
        { encode : a -> ResponseBody
        , decode : ResponseBody -> Maybe a
        }


{-| -}
type ResponseBody
    = StringResponse String
    | IntResponse Int
    | FloatResponse Float
    | BoolResponse Bool
    | UnitResponse
    | ValueResponse Value
    | BytesResponse Bytes
    | MaybeResponse (Maybe ResponseBody)
    | ResultResponse (Result ResponseBody ResponseBody)
    | TupleResponse ( ResponseBody, ResponseBody )
    | ListResponse (List ResponseBody)
    | RecordResponse (List ResponseBody)
    | HttpError Http.Error
    | HttpMetadata Http.Metadata
    | HttpResponse (Http.Response ResponseBody)
    | TimeZone Time.Zone
    | VariantResponse ( String, ResponseBody )


{-| -}
encode : ResponseType a -> a -> ResponseBody
encode (ResponseType r) =
    r.encode


{-| -}
decode : ResponseType a -> ResponseBody -> Maybe a
decode (ResponseType r) =
    r.decode


{-| -}
string : ResponseType String
string =
    ResponseType
        { encode = StringResponse
        , decode =
            \body ->
                case body of
                    StringResponse a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
int : ResponseType Int
int =
    ResponseType
        { encode = IntResponse
        , decode =
            \body ->
                case body of
                    IntResponse a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
float : ResponseType Float
float =
    ResponseType
        { encode = FloatResponse
        , decode =
            \body ->
                case body of
                    FloatResponse a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
bool : ResponseType Bool
bool =
    ResponseType
        { encode = BoolResponse
        , decode =
            \body ->
                case body of
                    BoolResponse a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
unit : ResponseType ()
unit =
    ResponseType
        { encode = \_ -> UnitResponse
        , decode =
            \body ->
                case body of
                    UnitResponse ->
                        Just ()

                    _ ->
                        Nothing
        }


{-| -}
value : ResponseType Value
value =
    ResponseType
        { encode = ValueResponse
        , decode =
            \body ->
                case body of
                    ValueResponse a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
bytes : ResponseType Bytes
bytes =
    ResponseType
        { encode = BytesResponse
        , decode =
            \body ->
                case body of
                    BytesResponse a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
maybe : ResponseType a -> ResponseType (Maybe a)
maybe (ResponseType typeA) =
    ResponseType
        { encode =
            \ma ->
                Maybe.map typeA.encode ma
                    |> MaybeResponse
        , decode =
            \body ->
                case body of
                    MaybeResponse Nothing ->
                        Just Nothing

                    MaybeResponse (Just ba) ->
                        Maybe.map Just <| typeA.decode ba

                    _ ->
                        Nothing
        }


{-| -}
result : ResponseType e -> ResponseType a -> ResponseType (Result e a)
result (ResponseType typeE) (ResponseType typeA) =
    ResponseType
        { encode =
            \res ->
                case res of
                    Err e ->
                        ResultResponse (Err <| typeE.encode e)

                    Ok a ->
                        ResultResponse (Ok <| typeA.encode a)
        , decode =
            \body ->
                case body of
                    ResultResponse (Err be) ->
                        Maybe.map Err <| typeE.decode be

                    ResultResponse (Ok ba) ->
                        Maybe.map Ok <| typeA.decode ba

                    _ ->
                        Nothing
        }


{-| -}
tuple : ResponseType a1 -> ResponseType a2 -> ResponseType ( a1, a2 )
tuple (ResponseType typeA1) (ResponseType typeA2) =
    ResponseType
        { encode =
            \( a1, a2 ) ->
                TupleResponse ( typeA1.encode a1, typeA2.encode a2 )
        , decode =
            \body ->
                case body of
                    TupleResponse ( ba1, ba2 ) ->
                        Maybe.map2 (\a1 a2 -> ( a1, a2 ))
                            (typeA1.decode ba1)
                            (typeA2.decode ba2)

                    _ ->
                        Nothing
        }


{-| -}
list : ResponseType a -> ResponseType (List a)
list (ResponseType typeA) =
    ResponseType
        { encode =
            \ls ->
                ListResponse <| List.map typeA.encode ls
        , decode =
            \body ->
                case body of
                    ListResponse ls ->
                        List.foldr
                            (\a macc ->
                                Maybe.map2 (::)
                                    (typeA.decode a)
                                    macc
                            )
                            (Just [])
                            ls

                    _ ->
                        Nothing
        }


{-| -}
type RecordType r a
    = RecordType
        { encode : r -> List ResponseBody
        , decode : List ResponseBody -> Maybe a
        }


{-| -}
record : constructor -> RecordType r constructor
record f =
    RecordType
        { encode = \_ -> []
        , decode = \_ -> Just f
        }


{-| -}
field : (r -> a) -> ResponseType a -> RecordType r (a -> b) -> RecordType r b
field getter (ResponseType typeA) (RecordType rbulderF) =
    RecordType
        { encode =
            \r ->
                (typeA.encode <| getter r) :: rbulderF.encode r
        , decode =
            \ls ->
                case ls of
                    [] ->
                        Nothing

                    ba :: bs ->
                        Maybe.map2 (\f a -> f a)
                            (rbulderF.decode bs)
                            (typeA.decode ba)
        }


{-| -}
fromRecordType : RecordType a a -> ResponseType a
fromRecordType (RecordType rbuilder) =
    ResponseType
        { encode =
            \a ->
                RecordResponse <| rbuilder.encode a
        , decode =
            \body ->
                case body of
                    RecordResponse bs ->
                        rbuilder.decode bs

                    _ ->
                        Nothing
        }


{-| -}
httpError : ResponseType Http.Error
httpError =
    ResponseType
        { encode =
            \a ->
                HttpError a
        , decode =
            \body ->
                case body of
                    HttpError a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
httpMetadata : ResponseType Http.Metadata
httpMetadata =
    ResponseType
        { encode =
            \a ->
                HttpMetadata a
        , decode =
            \body ->
                case body of
                    HttpMetadata a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
httpResponse : ResponseType body -> ResponseType (Http.Response body)
httpResponse (ResponseType bodyType) =
    ResponseType
        { encode =
            \a ->
                HttpResponse <|
                    case a of
                        Http.BadUrl_ str ->
                            Http.BadUrl_ str

                        Http.Timeout_ ->
                            Http.Timeout_

                        Http.NetworkError_ ->
                            Http.NetworkError_

                        Http.BadStatus_ meta b ->
                            bodyType.encode b
                                |> Http.BadStatus_ meta

                        Http.GoodStatus_ meta b ->
                            bodyType.encode b
                                |> Http.GoodStatus_ meta
        , decode =
            \body ->
                case body of
                    HttpResponse resp ->
                        case resp of
                            Http.BadUrl_ str ->
                                Just <| Http.BadUrl_ str

                            Http.Timeout_ ->
                                Just <| Http.Timeout_

                            Http.NetworkError_ ->
                                Just <| Http.NetworkError_

                            Http.BadStatus_ meta b ->
                                bodyType.decode b
                                    |> Maybe.map
                                        (Http.BadStatus_ meta)

                            Http.GoodStatus_ meta b ->
                                bodyType.decode b
                                    |> Maybe.map
                                        (Http.GoodStatus_ meta)

                    _ ->
                        Nothing
        }


{-| -}
zone : ResponseType Time.Zone
zone =
    ResponseType
        { encode =
            \a ->
                TimeZone a
        , decode =
            \body ->
                case body of
                    TimeZone a ->
                        Just a

                    _ ->
                        Nothing
        }


{-| -}
variant :
    { encode : a -> ( String, ResponseBody )
    , decode : ( String, ResponseBody ) -> Maybe a
    }
    -> ResponseType a
variant o =
    ResponseType
        { encode = o.encode >> VariantResponse
        , decode =
            \body ->
                case body of
                    VariantResponse p ->
                        o.decode p

                    _ ->
                        Nothing
        }
