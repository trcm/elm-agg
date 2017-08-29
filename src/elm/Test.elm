module Test exposing (maybeTest, maybeBindTest)

import Maybe exposing (andThen)


maybeTest : a -> Maybe a
maybeTest a =
    Just a


maybeBindTest : Int -> Maybe Int
maybeBindTest a =
    if a > 3 then
        Just a
    else
        Nothing
