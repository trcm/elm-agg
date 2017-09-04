module Decoders exposing (Item, Ids, itemParser, decodeStories, Comment, commentParser)

import Json.Decode as Json exposing (at, decodeString, list, int, float, string, nullable)
import Json.Decode.Pipeline exposing (required, decode, hardcoded, optional)


-- TYPES


type alias Item =
    { by : String
    , descendants : Int
    , id : Int
    , score : Int
    , time : Float
    , title : String
    , kids : List Int
    , url : String
    , loading : Bool
    }


type alias Comment =
    { by : String
    , kids : List Int
    , parent : Int
    , text : String
    , time : Int
    , id : Int
    }


type alias Ids =
    List Int



-- DECODERS


decodeStories : Json.Decoder Ids
decodeStories =
    (Json.list Json.int)


itemParser : Json.Decoder Item
itemParser =
    decode Item
        |> required "by" string
        |> required "descendants" int
        |> required "id" int
        |> required "score" int
        |> required "time" float
        |> required "title" string
        |> required "kids" (list int)
        |> required "url" string
        |> hardcoded False


commentParser : Json.Decoder Comment
commentParser =
    decode Comment
        |> required "by" string
        |> optional "kids" (list int) []
        |> required "parent" int
        |> required "text" string
        |> required "time" int
        |> required "id" int
