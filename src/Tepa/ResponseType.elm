module Tepa.ResponseType exposing
    ( ResponseType
    , string, int, float, bool, unit, value, bytes, maybe, result, tuple, list
    , httpError, httpMetadata, httpResponse
    , zone
    , RecordType, record, field, fromRecordType
    )

{-|


# Core

@docs ResponseType


# Primitives

@docs string, int, float, bool, unit, value, bytes, maybe, result, tuple, list


# Common Types

@docs httpError, httpMetadata, httpResponse
@docs zone


# Record Type

@docs RecordType, record, field, fromRecordType

-}

import Bytes exposing (Bytes)
import Http
import Internal.ResponseType as Internal
import Json.Encode exposing (Value)
import Time



-- # Core


{-| -}
type alias ResponseType a =
    Internal.ResponseType a



-- # Primitives


{-| -}
string : ResponseType String
string =
    Internal.string


{-| -}
int : ResponseType Int
int =
    Internal.int


{-| -}
float : ResponseType Float
float =
    Internal.float


{-| -}
bool : ResponseType Bool
bool =
    Internal.bool


{-| -}
unit : ResponseType ()
unit =
    Internal.unit


{-| -}
value : ResponseType Value
value =
    Internal.value


{-| -}
bytes : ResponseType Bytes
bytes =
    Internal.bytes


{-| -}
maybe : ResponseType a -> ResponseType (Maybe a)
maybe =
    Internal.maybe


{-| -}
result : ResponseType e -> ResponseType a -> ResponseType (Result e a)
result =
    Internal.result


{-| -}
tuple : ResponseType a1 -> ResponseType a2 -> ResponseType ( a1, a2 )
tuple =
    Internal.tuple


{-| -}
list : ResponseType a -> ResponseType (List a)
list =
    Internal.list



-- # Common Types


{-| -}
httpError : ResponseType Http.Error
httpError =
    Internal.httpError


{-| -}
httpMetadata : ResponseType Http.Metadata
httpMetadata =
    Internal.httpMetadata


{-| -}
httpResponse : ResponseType body -> ResponseType (Http.Response body)
httpResponse =
    Internal.httpResponse


{-| -}
zone : ResponseType Time.Zone
zone =
    Internal.zone



-- # Record Type


{-| -}
type alias RecordType r a =
    Internal.RecordType r a


{-| -}
record : constructor -> RecordType r constructor
record =
    Internal.record


{-| -}
field : (r -> a) -> ResponseType a -> RecordType r (a -> b) -> RecordType r b
field =
    Internal.field


{-| -}
fromRecordType : RecordType a a -> ResponseType a
fromRecordType =
    Internal.fromRecordType
