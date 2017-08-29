module Story exposing (story)

import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Decoders exposing (Item)
import Date exposing (fromTime)


type Msg
    = NoOp


story : Item -> Html a
story { by, descendants, id, score, time, title, url } =
    case id of
        0 ->
            div [] []

        _ ->
            let
                storyDate =
                    fromTime time

                storyString =
                    toString storyDate
            in
                div [ class "storyItem" ]
                    [ p [ class "score" ] [ text (toString score) ]
                    , a [ class "title", href url, target "_blank" ] [ text title ]
                    , p [ class "author" ] [ text by ]
                    ]
