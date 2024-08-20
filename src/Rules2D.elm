module Rules2D exposing (..)

import GameOfLife exposing (GameOfLife)


type alias Coord2d =
    ( Int, Int )


deltas =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


applyDelta ( cx, cy ) ( dx, dy ) =
    ( cx + dx, cy + dy )


game2D =
    { neighboursOf = \cell -> List.map (applyDelta cell) deltas
    , shouldLive = \x -> (x == 2) || (x == 3)
    , shouldBeBorn = \x -> x == 3
    }
