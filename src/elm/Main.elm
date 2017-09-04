module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id, href)
import Html.Events exposing (onClick, on)
import Debug exposing (log)
import Json.Decode as Json exposing (at, decodeString, list, int, string)
import Navigation
import Http
import UrlParser exposing ((</>), s, int, string, parseHash)
import Decoders exposing (Item, Ids, itemParser, decodeStories, Comment, commentParser)
import Story exposing (story)


-- APP


main : Program Never Model Msg
main =
    Navigation.program UrlChange
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
    , comments : List Comment
    , history : List Navigation.Location
    , currentRoute : Routes
    }


initItem : Item
initItem =
    Item "" 0 0 0 0 "" [] "" False


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model 0 [] initItem [] [] [ location ] All, getStories )


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


type Routes
    = Detail Int
    | All


type alias Route =
    { route : Routes
    , id : Maybe Int
    }


type Msg
    = NoOp
    | GetStories
    | GotStories (Result Http.Error Ids)
    | GetStory Int
    | GotStory (Result Http.Error Item)
    | GetMore
    | Scroll Pos
    | UrlChange Navigation.Location
    | GetComments Int
    | GotComment (Result Http.Error Comment)


load : Int
load =
    40


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange location ->
            let
                id =
                    parseHash (UrlParser.s "comments" </> UrlParser.int) location

                update =
                    case id of
                        Just id ->
                            Detail id

                        Nothing ->
                            All

                requests =
                    case update of
                        Detail id ->
                            startCommentGet model id

                        All ->
                            []
            in
                { model | history = location :: model.history, currentRoute = update } ! requests

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
                    { model | stories = model.stories ++ [ res ] } ! []

                Err e ->
                    model ! []

        GetComments id ->
            let
                story =
                    List.filter (\a -> a.id == id) model.stories |> List.head
            in
                case story of
                    Just story ->
                        model ! getComments story

                    Nothing ->
                        model ! []

        GotComment res ->
            case res of
                Ok res ->
                    { model | comments = model.comments ++ [ res ] } ! []

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
        , button [ id "refreshBtn", onClick GetStories ] [ text "Refresh" ]
        , ul []
            [ viewLink "test" Nothing
            , viewLink "otherTest" Nothing
            , viewLink "detail" (Just 1)
            ]
        , viewHistory model.history
        , renderContent model
        ]


renderContent : Model -> Html Msg
renderContent model =
    case model.currentRoute of
        Detail id ->
            div [ class "container", onScroll Scroll ]
                [ p [] [ text "detail view" ]
                , ul [] (List.map renderComment model.comments)
                ]

        All ->
            div [ onScroll Scroll ]
                (List.map story model.stories)


viewLink : String -> Maybe Int -> Html Msg
viewLink name id =
    case id of
        Just id ->
            li [] [ a [ href ("/#/" ++ name ++ "/" ++ (toString id)) ] [ text name ] ]

        Nothing ->
            li [] [ a [ href ("/#/" ++ name) ] [ text name ] ]


viewHistory : List Navigation.Location -> Html Msg
viewHistory history =
    ul [] (List.map historyItem history)


historyItem : Navigation.Location -> Html Msg
historyItem item =
    li [] [ text (item.pathname ++ item.hash) ]


renderComment : Comment -> Html Msg
renderComment comment =
    li [] [ text comment.text ]



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }



-- HELPER


startCommentGet : Model -> Int -> List (Cmd Msg)
startCommentGet model id =
    let
        story =
            List.filter (\a -> a.id == id) model.stories |> List.head
    in
        case story of
            Just story ->
                getComments story

            Nothing ->
                []



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


getComments : Item -> List (Cmd Msg)
getComments story =
    List.map (\x -> getCommentReq x |> Http.send GotComment) story.kids


getCommentReq : Int -> Http.Request Comment
getCommentReq id =
    let
        url =
            "https://hacker-news.firebaseio.com/v0/item/" ++ (toString id) ++ ".json"
    in
        Http.get url commentParser
