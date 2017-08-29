module Decoders exposing (Item, Ids, itemParser, decodeStories)

import Json.Decode as Json exposing (at, decodeString, list, int, float, string, nullable)
import Json.Decode.Pipeline exposing (required, decode)


-- TYPES


type alias Item =
    { by : String
    , descendants : Int
    , id : Int
    , score : Int
    , time : Float
    , title : String
    , url : String
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
        |> required "url" string
