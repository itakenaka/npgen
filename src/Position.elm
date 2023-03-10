module Position exposing
    ( Position
    , allPosition
    , groupEq
    , toIntTuple
    )


type alias Position =
    { row : Row
    , col : Col
    , block : Block
    }


type Row
    = R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | R9


type Col
    = C1
    | C2
    | C3
    | C4
    | C5
    | C6
    | C7
    | C8
    | C9


type Block
    = B1
    | B2
    | B3
    | B4
    | B5
    | B6
    | B7
    | B8
    | B9


type BlockPosition
    = BP1
    | BP2
    | BP3


groupEq : Position -> Position -> Bool
groupEq p1 p2 =
    (p1 /= p2)
        && (p1.row == p2.row || p1.col == p2.col || p1.block == p2.block)


rowToInt : Row -> Int
rowToInt x =
    case x of
        R1 ->
            1

        R2 ->
            2

        R3 ->
            3

        R4 ->
            4

        R5 ->
            5

        R6 ->
            6

        R7 ->
            7

        R8 ->
            8

        R9 ->
            9


colToInt : Col -> Int
colToInt x =
    case x of
        C1 ->
            1

        C2 ->
            2

        C3 ->
            3

        C4 ->
            4

        C5 ->
            5

        C6 ->
            6

        C7 ->
            7

        C8 ->
            8

        C9 ->
            9


allRow : List Row
allRow =
    [ R1, R2, R3, R4, R5, R6, R7, R8, R9 ]


allCol : List Col
allCol =
    [ C1, C2, C3, C4, C5, C6, C7, C8, C9 ]


allPosition : List Position
allPosition =
    let
        addCol : Row -> List ( Row, Col )
        addCol row =
            allCol
                |> List.map (\col -> ( row, col ))

        toPotition : ( Row, Col ) -> Position
        toPotition ( row, col ) =
            case ( rowToBlockPosition row, colToBlockPosition col ) of
                ( BP1, BP1 ) ->
                    Position row col B1

                ( BP1, BP2 ) ->
                    Position row col B2

                ( BP1, BP3 ) ->
                    Position row col B3

                ( BP2, BP1 ) ->
                    Position row col B4

                ( BP2, BP2 ) ->
                    Position row col B5

                ( BP2, BP3 ) ->
                    Position row col B6

                ( BP3, BP1 ) ->
                    Position row col B7

                ( BP3, BP2 ) ->
                    Position row col B8

                ( BP3, BP3 ) ->
                    Position row col B9

        rowToBlockPosition : Row -> BlockPosition
        rowToBlockPosition row =
            case row of
                R1 ->
                    BP1

                R2 ->
                    BP1

                R3 ->
                    BP1

                R4 ->
                    BP2

                R5 ->
                    BP2

                R6 ->
                    BP2

                R7 ->
                    BP3

                R8 ->
                    BP3

                R9 ->
                    BP3

        colToBlockPosition : Col -> BlockPosition
        colToBlockPosition col =
            case col of
                C1 ->
                    BP1

                C2 ->
                    BP1

                C3 ->
                    BP1

                C4 ->
                    BP2

                C5 ->
                    BP2

                C6 ->
                    BP2

                C7 ->
                    BP3

                C8 ->
                    BP3

                C9 ->
                    BP3
    in
    allRow
        |> List.concatMap addCol
        |> List.map toPotition


toIntTuple : Position -> ( Int, Int )
toIntTuple position =
    ( position.row, position.col )
        |> Tuple.mapBoth rowToInt colToInt
