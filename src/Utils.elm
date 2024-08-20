module Utils exposing (frequencies)

import List.Extra as List


frequencies : List comparable -> List ( comparable, Int )
frequencies list =
    list
        |> List.sort
        |> List.group
        |> List.map (\( x, y ) -> ( x, 1 + List.length y ))
