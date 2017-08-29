module Request exposing (getStories, getStory)

import Decoders exposing (Ids, Item, decodeStories, itemParser)
import Debug exposing (log)
import Http


getStories : msg -> Cmd msg
getStories msg =
    let
        _ =
            log "getting stories"

        url =
            "https://hacker-news.firebaseio.com/v0/topstories.json"

        request =
            Http.get url decodeStories
    in
        Http.send msg getStoriesReq


getStoriesReq : Http.Request Ids
getStoriesReq =
    Http.get "https://hacker-news.firebaseio.com/v0/topstories.json" decodeStories


getStory : msg -> Int -> Cmd msg
getStory a id =
    let
        request =
            getStoryReq id
    in
        Http.send a request


getStoryReq : Int -> Http.Request Item
getStoryReq id =
    let
        url =
            "https://hacker-news.firebaseio.com/v0/item/" ++ (toString id) ++ ".json"
    in
        Http.get url itemParser
