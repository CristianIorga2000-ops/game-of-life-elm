module GameOfLife exposing (GameOfLife, transition)

import Utils exposing (frequencies)


type alias GameOfLife c =
    { neighboursOf : c -> List c
    , shouldLive : Int -> Bool
    , shouldBeBorn : Int -> Bool
    }


transition : GameOfLife comparable -> List comparable -> List comparable
transition gameRules state =
    let
        livingCellsNeighbours : List comparable
        livingCellsNeighbours =
            List.concatMap gameRules.neighboursOf state

        freqs : List ( comparable, Int )
        freqs =
            frequencies livingCellsNeighbours

        ( freqsOfCells, freqsOfEmptySpaces ) =
            freqs
                |> List.partition (\x -> List.member (Tuple.first x) state)

        cellsThatMatch : (Int -> Bool) -> List ( c, Int ) -> List c
        cellsThatMatch neighbourCondition cells =
            List.map Tuple.first <|
                List.filter (neighbourCondition << Tuple.second) cells

        survivors : List comparable
        survivors =
            cellsThatMatch gameRules.shouldLive freqsOfCells

        newborns : List comparable
        newborns =
            cellsThatMatch gameRules.shouldBeBorn freqsOfEmptySpaces
    in
    survivors ++ newborns
