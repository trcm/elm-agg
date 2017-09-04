module Story exposing (story)

import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Decoders exposing (Item)
import Date exposing (fromTime)


type Msg
    = NoOp


story : Item -> Html a
story { by, descendants, kids, id, score, time, title, url } =
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
                div []
                    [ div [ class "storyItem" ]
                        [ p [ class "score" ] [ text (toString score) ]
                        , span [ class "link" ]
                            [ a [ href url, target "_blank" ] [ text title ]
                            , p [ class "commentSpacer" ] [ text "  ::  " ]
                            , commentLink id (List.length kids)
                            ]
                        , p [ class "author" ] [ text by ]
                        ]
                    ]


commentLink : Int -> Int -> Html a
commentLink id num =
    p [] [ a [ href ("/#/comments/" ++ (toString id)) ] [ text ((toString num) ++ " comments") ] ]
