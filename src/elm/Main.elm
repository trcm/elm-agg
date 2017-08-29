module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, on)
import Debug exposing (log)
import Json.Decode as Json exposing (at, decodeString, list, int, string)
import Http
import Decoders exposing (Item, Ids, itemParser, decodeStories)
import Story exposing (story)


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { counter : Int
    , ids : Ids
    , story : Item
    , stories : List Item
    }


initItem : Item
initItem =
    Item "" 0 0 0 0 "" ""


init : ( Model, Cmd Msg )
init =
    ( Model 0 [] initItem [], getStories )


type alias Pos =
    { scrolledHeight : Int
    , contentHeight : Int
    , containerHeight : Int
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = NoOp
    | GetStories
    | GotStories (Result Http.Error Ids)
    | GetStory Int
    | GotStory (Result Http.Error Item)
    | GetMore
    | Scroll Pos


load : Int
load =
    40


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetStories ->
            ( { model | counter = model.counter + 1 }, getStories )

        GotStories (Ok stuff) ->
            let
                _ =
                    log "got stories" (List.length stuff)

                ids =
                    List.take load stuff

                first =
                    Maybe.withDefault 0 (List.head ids)

                cmds =
                    List.map getStory ids
            in
                { model | ids = ids, counter = model.counter + load } ! cmds

        GotStories (Err e) ->
            let
                _ =
                    log "error" e
            in
                model ! []

        GetStory id ->
            model ! []

        GotStory res ->
            case res of
                Ok res ->
                    { model | stories = res :: model.stories } ! []

                Err e ->
                    model ! []

        Scroll pos ->
            let
                _ =
                    log "some stuff" " fdafs"
            in
                { model | counter = model.counter + 1 } ! []

        GetMore ->
            model ! []



-- VIEW


onScroll : (Pos -> a) -> Attribute a
onScroll msg =
    let
        _ =
            log "scroll event" " fdsafds"
    in
        on "scroll" (Json.map msg scrollInfoDecoder)


scrollInfoDecoder : Json.Decoder Pos
scrollInfoDecoder =
    Json.map3 Pos
        (Json.at [ "target", "scrollHeight" ] Json.int)
        (Json.at [ "target", "scrollTop" ] Json.int)
        (Json.at [ "target", "offsetHeight" ] Json.int)


view : Model -> Html Msg
view model =
    div [ class "container", onScroll Scroll ]
        [ p [] [ text (toString model.counter) ]
        , button [ onClick GetStories ] [ text "Refresh" ]
        , div [ onScroll Scroll ]
            (List.map story model.stories)
        ]



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }



-- HTTP


getStories : Cmd Msg
getStories =
    let
        url =
            "https://hacker-news.firebaseio.com/v0/topstories.json"

        request =
            Http.get url decodeStories
    in
        Http.send GotStories getStoriesReq


getStoriesReq : Http.Request Ids
getStoriesReq =
    Http.get "https://hacker-news.firebaseio.com/v0/topstories.json" decodeStories


getStory : Int -> Cmd Msg
getStory id =
    let
        request =
            getStoryReq id
    in
        Http.send GotStory request


getStoryReq : Int -> Http.Request Item
getStoryReq id =
    let
        url =
            "https://hacker-news.firebaseio.com/v0/item/" ++ (toString id) ++ ".json"
    in
        Http.get url itemParser
