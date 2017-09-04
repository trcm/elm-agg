module Comment exposing (comment)

import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Decoders exposing (Item)
import Date exposing (fromTime)


comment : Item -> Html a
comment item =
    div [] []
