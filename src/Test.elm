module Test exposing (main)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Main
    exposing
        ( FilledCell
        , State
        , genSudoku
        , generateSolution
        , numToString
        , randomToTask
        , solveSudoku
        , yellow
        )
import Position as P exposing (Position)
import Process
import Random exposing (Generator)
import Random.List
import Task exposing (Task)


type alias Model =
    { page : Page
    , result : TestResult
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type Msg
    = Testing
    | TestDone TestResult


type Page
    = InitialPage
    | TestingPage
    | ResultPage


type TestResult
    = Match String String
    | NotMatch String String
    | FailToSolve
    | NoResult


view : Model -> Html Msg
view model =
    viewSelector model
        |> Element.layout []


viewSelector : Model -> Element Msg
viewSelector model =
    case model.page of
        InitialPage ->
            initialPage

        TestingPage ->
            testingPage

        ResultPage ->
            resultPage model


testingPage : Element Msg
testingPage =
    Element.text "testing"


initialPage : Element Msg
initialPage =
    startTestButton


startTestButton : Element Msg
startTestButton =
    Element.Input.button
        [ Element.Border.rounded 1
        , Element.Border.width 1
        , Element.mouseOver
            [ Element.Background.color yellow ]
        ]
        { onPress = Just Testing
        , label = Element.text "start testing"
        }


resultPage : Model -> Element Msg
resultPage model =
    Element.column []
        [ startTestButton
        , resultToElement model.result
        ]


resultToElement : TestResult -> Element Msg
resultToElement result =
    case result of
        NoResult ->
            Element.none

        FailToSolve ->
            Element.text "failed to solve"

        Match a b ->
            Element.column []
                [ Element.text "result match"
                , Element.text a
                , Element.text b
                ]

        NotMatch a b ->
            Element.column []
                [ Element.text "result not match"
                , Element.text a
                , Element.text b
                ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = InitialPage
      , result = NoResult
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Testing ->
            ( { model
                | page = TestingPage
              }
            , Task.perform TestDone testTask
            )

        TestDone result ->
            ( { model
                | page = ResultPage
                , result = result
              }
            , Cmd.none
            )


testTask : Task Never TestResult
testTask =
    Process.sleep 0
        |> Task.andThen (always generateSolution)
        |> Task.andThen testSudokuResult


testSudokuResult : List FilledCell -> Task Never TestResult
testSudokuResult filledCellXs =
    let
        solutionString : String
        solutionString =
            filledCellXs
                |> filledCellListToString

        filledCellListToString : List FilledCell -> String
        filledCellListToString xs =
            xs
                |> List.sortBy (.position >> P.toIntTuple)
                |> List.map (.value >> numToString)
                |> String.concat

        toTestResult : String -> String -> TestResult
        toTestResult a b =
            if a == b then
                Match a b

            else
                NotMatch a b

        helper : Maybe (List FilledCell) -> TestResult
        helper x =
            case x of
                Just filledCellYs ->
                    toTestResult
                        solutionString
                        (filledCellYs |> filledCellListToString)

                Nothing ->
                    FailToSolve
    in
    filledCellXs
        |> Random.List.shuffle
        |> Random.map genSudoku
        |> Random.map solveSudoku
        |> Random.map helper
        |> randomToTask
