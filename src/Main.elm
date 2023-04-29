--
-- generate solution
-- then remove one number from solution
-- then check if it is solvable
--


module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Events
import Json.Decode as D
import List.Extra as Lx
import Position as P exposing (Position)
import Process
import Random exposing (Generator)
import Random.List
import Task exposing (Task)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { viewCellXs : List ViewCell
    , inputCell : Maybe ViewCell
    , page : Page
    , undo : List ViewCell
    , redColorOnWrongNum : Bool
    , maybeWheelSelection : Maybe WheelSelection
    }


type Msg
    = Sudoku State
    | Generate
    | Input (Maybe ViewCell)
    | NumPad CellValue
    | Undo
    | RedColorOnWrongNum Bool
    | IncreaseWheelNum ViewCell
    | DecreaseWheelNum ViewCell
    | MouseLeave ViewCell


type Page
    = GenerateSudoku
    | Game


type alias State =
    { empty : List Cell
    , filled : List FilledCell
    }


type alias Cell1 =
    { position : Position
    , candidate : List Num
    }


type alias Cell =
    { position : Position
    , candidate : List Num
    , rightNum : Num
    }


type alias FilledCell =
    { position : Position
    , value : Num
    }


type alias ViewCell =
    { position : Position
    , value : CellValue
    , rightNum : Num
    }


type CellValue
    = Fixed Num
    | Enter Num
    | Blank


type Num
    = N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7
    | N8
    | N9



--
-- INIT
--


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewCellXs = []
      , inputCell = Nothing
      , page = GenerateSudoku
      , undo = []
      , redColorOnWrongNum = False
      , maybeWheelSelection = Nothing
      }
    , Task.perform Sudoku longTask
    )



--
-- UPDATE
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( { model
                | inputCell = Nothing
                , page = GenerateSudoku
              }
            , Task.perform Sudoku longTask
            )

        Sudoku x ->
            ( { model
                | viewCellXs = stateToViewCellXs x
                , inputCell = Nothing
                , page = Game
              }
            , Cmd.none
            )

        Input maybeViewCell ->
            ( { model
                | inputCell = maybeViewCell
              }
            , Cmd.none
            )

        NumPad cellValue ->
            case model.inputCell of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just c ->
                    ( { model
                        | viewCellXs =
                            updateViewCellXs c.position cellValue model.viewCellXs
                        , undo = c :: model.undo
                        , inputCell = Nothing
                      }
                    , Cmd.none
                    )

        Undo ->
            case model.undo of
                [] ->
                    ( model
                    , Cmd.none
                    )

                x :: xs ->
                    case x.value of
                        Fixed _ ->
                            ( model
                            , Cmd.none
                            )

                        Enter y ->
                            ( { model
                                | viewCellXs =
                                    updateViewCellXs x.position (Enter y) model.viewCellXs
                                , undo = xs
                              }
                            , Cmd.none
                            )

                        Blank ->
                            ( { model
                                | viewCellXs =
                                    updateViewCellXs x.position Blank model.viewCellXs
                                , undo = xs
                              }
                            , Cmd.none
                            )

        RedColorOnWrongNum x ->
            ( { model
                | redColorOnWrongNum = x
              }
            , Cmd.none
            )

        -- TODO try match with ( WheelSelection, Maybe WheelNum )
        -- use tryCellValueToWheelNum
        -- and change to IncreaseWheelNum ViewCell
        IncreaseWheelNum viewCell ->
            case ( model.maybeWheelSelection
                 , tryCellValueToWheelNum viewCell.value
                 )
            of
                (_, Nothing) ->
                    ( model
                    , Cmd.none
                    )
                (Nothing, Just wheelNum) ->
                    ( { model
                      | maybeWheelSelection =
                            { position = viewCell.position
                            , wheelNum =
                                increaseWheelNum wheelNum
                            }
                            |> Just
                      }
                    , Cmd.none
                    )
                (Just ws, _) ->
                    ( { model
                      | maybeWheelSelection =
                            { ws
                            | wheelNum =
                                increaseWheelNum ws.wheelNum
                            }
                            |> Just
                      }
                    , Cmd.none
                    )

        DecreaseWheelNum viewCell ->
            case ( model.maybeWheelSelection
                 , tryCellValueToWheelNum viewCell.value
                 )
            of
                (_, Nothing) ->
                    ( model
                    , Cmd.none
                    )
                (Nothing, Just wheelNum) ->
                    ( { model
                      | maybeWheelSelection =
                            { position = viewCell.position
                            , wheelNum =
                                decreaseWheelNum wheelNum
                            }
                            |> Just
                      }
                    , Cmd.none
                    )
                (Just ws, _) ->
                    ( { model
                      | maybeWheelSelection =
                            { ws
                            | wheelNum =
                                decreaseWheelNum ws.wheelNum
                            }
                            |> Just
                      }
                    , Cmd.none
                    )

        MouseLeave viewCell ->
            case model.maybeWheelSelection of
                Nothing ->
                    ( model
                    , Cmd.none
                    )
                Just ws ->
                    if viewCell.position == ws.position then
                        update (NumPad (wheelNumToCellValue ws.wheelNum))
                            { model
                            | inputCell = Just viewCell
                            , maybeWheelSelection = Nothing
                            }
                    else
                        ( model
                        , Cmd.none
                        )


--
-- Subscriptions
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress keyDecoder


keyDecoder : D.Decoder Msg
keyDecoder =
    D.andThen toMsg (D.field "key" D.string)


toMsg : String -> D.Decoder Msg
toMsg keyValue =
    case String.uncons keyValue of
        Just ('1', "") ->
            Enter N1
            |> NumPad
            |> D.succeed
        Just ('2', "") ->
            Enter N2
            |> NumPad
            |> D.succeed
        Just ('3', "") ->
            Enter N3
            |> NumPad
            |> D.succeed
        Just ('4', "") ->
            Enter N4
            |> NumPad
            |> D.succeed
        Just ('5', "") ->
            Enter N5
            |> NumPad
            |> D.succeed
        Just ('6', "") ->
            Enter N6
            |> NumPad
            |> D.succeed
        Just ('7', "") ->
            Enter N7
            |> NumPad
            |> D.succeed
        Just ('8', "") ->
            Enter N8
            |> NumPad
            |> D.succeed
        Just ('9', "") ->
            Enter N9
            |> NumPad
            |> D.succeed
        _ ->
            D.fail ""


updateViewCellXs : Position -> CellValue -> List ViewCell -> List ViewCell
updateViewCellXs position v viewCellXs =
    let
        f : ViewCell -> ViewCell
        f x =
            case ( position == x.position, x.value, v ) of
                ( False, _, _ ) ->
                    x

                ( _, Fixed _, _ ) ->
                    x

                ( _, _, Fixed _ ) ->
                    x

                ( True, _, _ ) ->
                    { x | value = v }
    in
    viewCellXs
        |> List.map f


stateToViewCellXs : State -> List ViewCell
stateToViewCellXs state =
    let
        toBlankViewCell : Cell -> ViewCell
        toBlankViewCell x =
            ViewCell x.position Blank x.rightNum

        toFixedViewCell : FilledCell -> ViewCell
        toFixedViewCell x =
            ViewCell x.position (Fixed x.value) x.value
    in
    state.empty
        |> List.map toBlankViewCell
        |> List.append (List.map toFixedViewCell state.filled)



-- use process to get back to render view


longTask : Task Never State
longTask =
    Process.sleep 0
        |> Task.andThen (always generateSolution)
        |> Task.andThen solutionToSudoku


generateSolution : Task Never (List FilledCell)
generateSolution =
    initialCells
        |> Random.map (fillAllCell [])
        |> Random.andThen generateSolutionHelper
        |> randomToTask


generateSolutionHelper : Maybe (List FilledCell) -> Generator (List FilledCell)
generateSolutionHelper maybeXs =
    case maybeXs of
        Nothing ->
            initialCells
                |> Random.map (fillAllCell [])
                |> Random.andThen generateSolutionHelper

        Just filledCellXs ->
            filledCellXs
                |> Random.constant



-- each cell has shuffled candidate


initialCells : Generator (List Cell1)
initialCells =
    shuffledCandidateList
        |> Random.map initCellList


initCell : List Num -> Position -> Cell1
initCell candidates p =
    Cell1 p candidates


initCellList : List (List Num) -> List Cell1
initCellList candidateList =
    P.allPosition
        |> List.map2 initCell candidateList


shuffledCandidateList : Generator (List (List Num))
shuffledCandidateList =
    Random.List.shuffle allNum
        |> Random.list 81


fillAllCell : List FilledCell -> List Cell1 -> Maybe (List FilledCell)
fillAllCell acc cellList =
    case cellList of
        [] ->
            acc
                |> Just

        x :: xs ->
            case tryFillNumbers x xs of
                Nothing ->
                    Nothing

                Just ( v, xs2 ) ->
                    fillAllCell (v :: acc) xs2


tryFillNumbers : Cell1 -> List Cell1 -> Maybe ( FilledCell, List Cell1 )
tryFillNumbers c list =
    case c.candidate of
        [] ->
            Nothing

        n :: _ ->
            Just ( FilledCell c.position n, updateCandidate c n list )



-- remove not available candidate from each cell


updateCandidate : Cell1 -> Num -> List Cell1 -> List Cell1
updateCandidate c n cellList =
    let
        f : Cell1 -> Cell1
        f x =
            if P.groupEq x.position c.position then
                { x | candidate = List.filter ((/=) n) x.candidate }

            else
                x
    in
    cellList
        |> List.map f



--
-- Generate sudoku
--


solutionToSudoku : List FilledCell -> Task Never State
solutionToSudoku filledCellXs =
    filledCellXs
        |> Random.List.shuffle
        |> Random.map genSudoku
        |> randomToTask


genSudoku : List FilledCell -> State
genSudoku filledCells =
    State [] filledCells
        |> reduceFilledCell []


reduceFilledCell : List FilledCell -> State -> State
reduceFilledCell failedCells state =
    let
        f : FilledCell -> Cell
        f x =
            Cell x.position allNum x.value
    in
    case ( failedCells, state.filled ) of
        ( [], [] ) ->
            state

        -- could not reach here (all empty)
        ( _, [] ) ->
            State state.empty failedCells

        ( _, x :: xs ) ->
            if
                State (f x :: state.empty) (List.append failedCells xs)
                    |> isSolvable
            then
                State (f x :: state.empty) xs
                    |> reduceFilledCell failedCells

            else
                State state.empty xs
                    |> reduceFilledCell (x :: failedCells)


fillSoleCandidateCell : List Cell -> Maybe FilledCell
fillSoleCandidateCell cellXs =
    let
        f : Cell -> Maybe FilledCell
        f c =
            case c.candidate of
                [ x ] ->
                    FilledCell c.position x
                        |> Just

                _ ->
                    Nothing
    in
    cellXs
        |> Lx.find (\c -> List.length c.candidate |> (==) 1)
        |> Maybe.andThen f


basicSolver :
    List Cell
    -> List Cell
    -> List FilledCell
    -> List Cell
    -> Maybe (List Cell)
basicSolver okAcc ngAcc fcellXs cellXs =
    let
        cellUpdater : Cell -> Maybe Cell
        cellUpdater x =
            fcellXs
                |> List.filter (.position >> P.groupEq x.position)
                |> candidateUpdater [] [] x.candidate
                |> Maybe.map (\ys -> { x | candidate = ys })

        candidateUpdater : List Num -> List Num -> List Num -> List FilledCell -> Maybe (List Num)
        candidateUpdater ok ng candidate filledCellXs =
            case ( ng, candidate ) of
                ( [], [] ) ->
                    Nothing

                ( _, [] ) ->
                    Just ok

                -- we need valid candidates not ng
                ( _, x :: xs ) ->
                    if filledCellXs |> List.any (\y -> x == y.value) then
                        candidateUpdater ok (x :: ng) xs filledCellXs

                    else
                        candidateUpdater (x :: ok) ng xs filledCellXs
    in
    case ( cellXs, okAcc ) of
        ( [], [] ) ->
            Nothing

        ( [], _ ) ->
            List.append okAcc ngAcc
                -- we need both to make whole cell list
                |> Just

        ( x :: xs, _ ) ->
            case cellUpdater x of
                Nothing ->
                    basicSolver okAcc (x :: ngAcc) fcellXs xs

                Just x2 ->
                    basicSolver (x2 :: okAcc) ngAcc fcellXs xs


isSolvable : State -> Bool
isSolvable state =
    case solveSudoku state of
        Just _ ->
            True

        Nothing ->
            False


solveSudoku : State -> Maybe (List FilledCell)
solveSudoku state =
    let
        f : FilledCell -> List Cell
        f fcell =
            state.empty
                |> List.filter (\x -> x.position /= fcell.position)
                |> List.map (\x -> { x | candidate = allNum })
    in
    case state.empty of
        [] ->
            Just state.filled

        x ->
            case trySolveOneCell state of
                Nothing ->
                    Nothing

                Just filledCell ->
                    filledCell
                        :: state.filled
                        |> (\y ->
                                State (f filledCell) y
                                    |> solveSudoku
                           )


trySolveOneCell : State -> Maybe FilledCell
trySolveOneCell state =
    state
        |> removeCandidateAsManyAsPossible
        |> Maybe.andThen fillSoleCandidateCell


removeCandidateAsManyAsPossible : State -> Maybe (List Cell)
removeCandidateAsManyAsPossible state =
    let
        f : State -> Maybe (List Cell)
        f s =
            case s.empty of
                [] ->
                    Just s.empty

                _ ->
                    basicSolver [] [] s.filled s.empty
    in
    case f state of
        Just cellXs ->
            State cellXs state.filled
                |> removeCandidateAsManyAsPossible

        Nothing ->
            Just state.empty


view : Model -> Html Msg
view model =
    case model.page of
        GenerateSudoku ->
            Element.text "generating new game"
                |> Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                |> Element.layout []

        _ ->
            appView model


sudokuView : Model -> Element Msg
sudokuView model =
    Element.el
        [ Element.Border.width 2
        , Element.centerX
        ]
    <|
        Element.column [] <|
            makeBlockRows <|
                modelToElementList model


appView : Model -> Html Msg
appView model =
    Element.layout [] <|
        Element.column
            [ Element.centerX
            , Element.padding 5
            , Element.spacing 10
            ]
            [ sudokuView model
            , model.maybeWheelSelection
              |> Maybe.map .wheelNum
              |> numberPad
            , Element.Input.button
                [ Element.Border.rounded 1
                , Element.Border.width 1
                , Element.mouseOver
                    [ Element.Background.color yellow ]
                ]
                { onPress = Just Generate
                , label = Element.text "new game"
                }
            , Element.Input.button
                [ Element.Border.rounded 1
                , Element.Border.width 1
                , Element.mouseOver
                    [ Element.Background.color yellow ]
                ]
                { onPress = Just Undo
                , label = Element.text "undo"
                }
            , Element.Input.checkbox
                []
                { onChange = RedColorOnWrongNum
                , icon = Element.Input.defaultCheckbox
                , checked = model.redColorOnWrongNum
                , label =
                    Element.Input.labelRight []
                        (Element.text "red color on wrong number")
                }
            ]


numberPad : Maybe WheelNum -> Element Msg
numberPad maybeWheelNum =
    let
        blankButtonBg : Element.Color
        blankButtonBg =
            case maybeWheelNum of
                Just WheelBlank ->
                    yellow
                _ ->
                    white

        buttonBg : Num -> Element.Color
        buttonBg num =
            case (num, maybeWheelNum) of
                (N1, Just W1) -> yellow
                (N2, Just W2) -> yellow
                (N3, Just W3) -> yellow
                (N4, Just W4) -> yellow
                (N5, Just W5) -> yellow
                (N6, Just W6) -> yellow
                (N7, Just W7) -> yellow
                (N8, Just W8) -> yellow
                (N9, Just W9) -> yellow
                _ -> white

        makeButton : Num -> Element Msg
        makeButton x =
            Element.el
                [ Element.Border.width 1
                , Element.width <| Element.px 50
                , Element.height <| Element.px 50
                ]
            <|
                Element.Input.button
                    [ Element.mouseOver
                        [ Element.Background.color yellow ]
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Font.center
                    , buttonBg x |> Element.Background.color
                    ]
                    { onPress = NumPad (Enter x) |> Just
                    , label =
                        x
                            |> numToString
                            |> Element.text
                    }

        blankButton : Element Msg
        blankButton =
            Element.el
                [ Element.Border.width 1
                , Element.width <| Element.px 50
                , Element.height <| Element.px 50
                ]
            <|
                Element.Input.button
                    [ Element.mouseOver
                        [ Element.Background.color yellow ]
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Font.center
                    , Element.Background.color blankButtonBg
                    ]
                    { onPress = NumPad Blank |> Just
                    , label = Element.text ""
                    }
    in
    allNum
        |> List.map makeButton
        |> (\xs ->
                List.append xs [ blankButton ]
                    |> Element.row
                        [ Element.Border.width 1
                        , Element.centerX
                        ]
           )


modelToElementList : Model -> List (Element Msg)
modelToElementList model =
    let
        toEnterNumColor : ViewCell -> Num -> Element.Color
        toEnterNumColor viewCell n =
            if model.redColorOnWrongNum && (viewCell.rightNum /= n) then
                red

            else
                green

        wsNumIfPosEq : ViewCell -> Maybe WheelSelection -> Maybe WheelNum
        wsNumIfPosEq viewCell maybeWs =
            case maybeWs of
                Nothing ->
                    Nothing
                Just ws ->
                    if viewCell.position == ws.position then
                        Just ws.wheelNum
                    else
                        Nothing

        wheelNumToString : WheelNum -> String
        wheelNumToString wheelNum =
            case wheelNum of
                WheelBlank -> ""
                W1 -> "1"
                W2 -> "2"
                W3 -> "3"
                W4 -> "4"
                W5 -> "5"
                W6 -> "6"
                W7 -> "7"
                W8 -> "8"
                W9 -> "9"

        viewCellToElement : Maybe WheelSelection -> ViewCell -> Element Msg
        viewCellToElement maybeWs x =
            case (x.value, (wsNumIfPosEq x maybeWs)) of
                (Fixed y, _) ->
                    numToString y
                        |> Element.text
                        |> Element.el
                            [ Element.centerX
                            , Element.centerY
                            ]

                (_, Just wheelNum) ->
                    Element.Input.button
                        [ Element.mouseOver
                            [ Element.Background.color yellow ]
                        , Element.width Element.fill
                        , Element.height Element.fill
                        , Element.Font.center
                        , yellow |> Element.Background.color
                        , wheelAction x
                        , mouseLeaveAction x
                        ]
                        { onPress = Input (Just x) |> Just
                        , label = wheelNumToString wheelNum |> Element.text
                        }
                (Enter v, _) ->
                    Element.Input.button
                        [ Element.mouseOver
                            [ Element.Background.color yellow ]
                        , Element.width Element.fill
                        , Element.height Element.fill
                        , Element.Font.center
                        , toEnterNumColor x v |> Element.Background.color
                        , wheelAction x
                        , mouseLeaveAction x
                        ]
                        { onPress = Input (Just x) |> Just
                        , label = numToString v |> Element.text
                        }

                (Blank, _) ->
                    Element.Input.button
                        [ Element.mouseOver
                            [ Element.Background.color yellow ]
                        , Element.width Element.fill
                        , Element.height Element.fill
                        , Element.Font.center
                        , Element.Background.color white
                        , wheelAction x
                        , mouseLeaveAction x
                        ]
                        { onPress = Input (Just x) |> Just
                        , label = Element.text ""
                        }
    in
    model.viewCellXs
        |> List.sortBy (.position >> P.toIntTuple)
        |> List.map (viewCellToElement model.maybeWheelSelection)


makeBlockRows : List (Element Msg) -> List (Element Msg)
makeBlockRows elementList =
    Lx.groupsOf 27 elementList
        |> List.map makeBlockRow


makeBlockRow : List (Element Msg) -> Element Msg
makeBlockRow numsBlockRow =
    case numsBlockRow |> Lx.groupsOf 3 |> Lx.groupsOf 3 of
        [ top, middle, bottom ] ->
            List.map3 (\a b c -> [ a, b, c ]) top middle bottom
                |> List.map makeBlock
                |> Element.row []

        _ ->
            Element.none


makeBlock : List (List (Element Msg)) -> Element Msg
makeBlock threeByThree =
    Element.el
        [ Element.Border.width 1 ]
    <|
        Element.column [] <|
            List.map makeCellRow <|
                threeByThree


makeCellRow : List (Element Msg) -> Element Msg
makeCellRow threeCells =
    Element.row [] <|
        List.map makeCell <|
            threeCells


makeCell : Element Msg -> Element Msg
makeCell cellElement =
    Element.el
        [ Element.Border.width 1
        , Element.width <| Element.px 50
        , Element.height <| Element.px 50
        ]
        cellElement


white : Element.Color
white =
    Element.rgb255 255 255 255


red : Element.Color
red =
    Element.rgb255 255 0 0


green : Element.Color
green =
    Element.rgb255 0 255 0


yellow : Element.Color
yellow =
    Element.rgb255 255 255 0


numToString : Num -> String
numToString x =
    case x of
        N1 ->
            "1"

        N2 ->
            "2"

        N3 ->
            "3"

        N4 ->
            "4"

        N5 ->
            "5"

        N6 ->
            "6"

        N7 ->
            "7"

        N8 ->
            "8"

        N9 ->
            "9"


allNum : List Num
allNum =
    [ N1, N2, N3, N4, N5, N6, N7, N8, N9 ]


randomToTask : Generator a -> Task x a
randomToTask generator =
    Time.now
        |> Task.map Time.posixToMillis
        |> Task.map toFloat
        |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << round)


type alias WheelSelection =
    { position : Position
    , wheelNum : WheelNum
    }


type WheelNum
    = WheelBlank
    | W1
    | W2
    | W3
    | W4
    | W5
    | W6
    | W7
    | W8
    | W9


increaseWheelNum : WheelNum -> WheelNum
increaseWheelNum wheelNum =
    case wheelNum of
        WheelBlank -> W1
        W1 -> W2
        W2 -> W3
        W3 -> W4
        W4 -> W5
        W5 -> W6
        W6 -> W7
        W7 -> W8
        W8 -> W9
        W9 -> WheelBlank


decreaseWheelNum : WheelNum -> WheelNum
decreaseWheelNum wheelNum =
    case wheelNum of
        W1 -> WheelBlank
        W2 -> W1
        W3 -> W2
        W4 -> W3
        W5 -> W4
        W6 -> W5
        W7 -> W6
        W8 -> W7
        W9 -> W8
        WheelBlank -> W9


wheelNumToCellValue : WheelNum -> CellValue
wheelNumToCellValue wheelNum =
    case wheelNum of
        W1 -> Enter N1
        W2 -> Enter N2
        W3 -> Enter N3
        W4 -> Enter N4
        W5 -> Enter N5
        W6 -> Enter N6
        W7 -> Enter N7
        W8 -> Enter N8
        W9 -> Enter N9
        WheelBlank -> Blank


tryCellValueToWheelNum : CellValue -> Maybe WheelNum
tryCellValueToWheelNum cellValue =
    case cellValue of
        Enter N1 -> Just W1
        Enter N2 -> Just W2
        Enter N3 -> Just W3
        Enter N4 -> Just W4
        Enter N5 -> Just W5
        Enter N6 -> Just W6
        Enter N7 -> Just W7
        Enter N8 -> Just W8
        Enter N9 -> Just W9
        Blank -> Just WheelBlank
        _ -> Nothing


wheelAction : ViewCell -> Element.Attribute Msg
wheelAction viewCell =
    D.field "deltaY" D.float
    |> D.andThen (floatToDecoderMsg viewCell)
    |> D.map (\msg -> (msg, True))
    |> Html.Events.preventDefaultOn "wheel"
    |> Element.htmlAttribute
                

floatToDecoderMsg : ViewCell -> Float -> D.Decoder Msg
floatToDecoderMsg viewCell deltaY =
    if deltaY > 0 then
        D.succeed (IncreaseWheelNum viewCell)
    else if deltaY < 0 then
        D.succeed (DecreaseWheelNum viewCell)
    else
        D.fail "invalid deltaY"


mouseLeaveAction : ViewCell -> Element.Attribute Msg
mouseLeaveAction viewCell =
    MouseLeave viewCell
    |> Html.Events.onMouseLeave
    |> Element.htmlAttribute
