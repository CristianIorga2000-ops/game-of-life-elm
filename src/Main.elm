module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Canvas
import Canvas.Settings
import Color
import GameOfLife exposing (transition)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import List
import List.Extra as List
import Process
import Random
import Rules2D as Rules exposing (game2D)
import Task
import Time


type alias Model =
    { cells : List ( Int, Int )
    , windowHeight : Int
    , windowWidth : Int
    , randomSeed : Random.Seed
    , isPlaying : Bool
    }


type Msg
    = CanvasClick Mouse.Event
    | SetWindowSize ( Int, Int )
    | Clear
    | TogglePlay
    | Randomize
    | GeneratedSeed Random.Seed
    | Tick Time.Posix


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


borderWidth =
    2


maxGenerated =
    1000


cellSize =
    10


gridSize =
    10


buttonHeight =
    30


timeBetweenTransitionsMs =
    250


toPos : Int -> Float
toPos c =
    toFloat (c * gridSize)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cells = [], windowWidth = 0, windowHeight = 0, randomSeed = Random.initialSeed 0, isPlaying = False }
    , Cmd.batch [ getWindowSize, generateRandomSeed ]
    )


generateRandomSeed : Cmd Msg
generateRandomSeed =
    Random.generate GeneratedSeed Random.independentSeed


getWindowSize : Cmd Msg
getWindowSize =
    Browser.Dom.getViewport
        |> Task.perform
            (\vp ->
                SetWindowSize
                    ( truncate (vp.viewport.width - vp.viewport.x)
                    , truncate (vp.viewport.height - vp.viewport.y)
                    )
            )


view : Model -> Document Msg
view model =
    { title = "Game of life"
    , body =
        [ body model
            |> Html.div []
        ]
    }


body : Model -> List (Html Msg)
body model =
    let
        truncateByGridSize : Int -> Int
        truncateByGridSize size =
            (size // gridSize) * gridSize

        canvasHeight =
            model.windowHeight
                - buttonHeight
                |> truncateByGridSize

        canvasWidth =
            model.windowWidth
                - borderWidth
                |> truncateByGridSize
    in
    [ Canvas.toHtml ( canvasWidth, canvasHeight )
        [ Mouse.onClick CanvasClick ]
        [ Canvas.clear ( 0, 0 ) (toFloat canvasWidth) (toFloat canvasHeight)
        , Canvas.shapes
            [ Canvas.Settings.fill Color.black ]
            (cellsView model)
        ]
    , Html.div []
        [ Html.button
            [ Html.Events.onClick Clear
            , style "height" (String.fromInt buttonHeight ++ "px")
            , style "width" "32%"
            ]
            [ Html.text "CLEAR" ]
        , Html.button
            [ Html.Events.onClick Randomize
            , style "height" (String.fromInt buttonHeight ++ "px")
            , style "width" "32%"
            ]
            [ Html.text "RANDOMIZE" ]
        , renderPlayOrPause model
        ]
    ]


renderPlayOrPause : Model -> Html Msg
renderPlayOrPause model =
    let
        buttonMsg : String
        buttonMsg =
            if model.isPlaying then
                "Stop"

            else
                "Play"
    in
    Html.button
        [ Html.Events.onClick TogglePlay
        , style "height" (String.fromInt buttonHeight ++ "px")
        , style "width" "32%"
        ]
        [ Html.text buttonMsg ]


cellsView : Model -> List Canvas.Shape
cellsView model =
    model.cells
        |> List.map (\( x, y ) -> ( toPos x, toPos y ))
        |> List.map (\cell -> Canvas.rect cell cellSize cellSize)


cellsGenerator : Int -> Int -> Random.Generator (List ( Int, Int ))
cellsGenerator maxX maxY =
    let
        generateX =
            Random.int 0 maxX

        generateY =
            Random.int 0 maxY
    in
    Random.int 0 maxGenerated
        |> Random.andThen
            (\cellCount ->
                Random.list cellCount (Random.map2 Tuple.pair generateX generateY)
            )
        |> Random.map List.unique


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedSeed seed ->
            ( { model | randomSeed = seed }, Cmd.none )

        Randomize ->
            let
                maxY =
                    model.windowHeight // gridSize

                maxX =
                    model.windowWidth // gridSize

                ( generatedCells, nextSeed ) =
                    Random.step (cellsGenerator maxX maxY) model.randomSeed
            in
            ( { model | cells = generatedCells, randomSeed = nextSeed }
            , Cmd.none
            )

        SetWindowSize ( width, height ) ->
            ( { model | windowWidth = width, windowHeight = height }
            , Cmd.none
            )

        CanvasClick event ->
            let
                ( posX, posY ) =
                    event.pagePos

                x =
                    toCoord posX

                y =
                    toCoord posY

                toCoord : Float -> Int
                toCoord f =
                    truncate (f / gridSize)

                newModel =
                    { model | cells = ( x, y ) :: model.cells }
            in
            ( newModel, Cmd.none )

        Clear ->
            ( { model | cells = [] }, Cmd.none )

        Tick _ ->
            let
                newState =
                    transition Rules.game2D model.cells

                newModel =
                    { model | cells = newState }
            in
            if model.isPlaying then
                ( newModel, Cmd.none )

            else
                ( model, Cmd.none )

        TogglePlay ->
            ( { model | isPlaying = not model.isPlaying }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tickCommand : List (Sub Msg)
        tickCommand =
            if model.isPlaying then
                [ Time.every timeBetweenTransitionsMs Tick ]

            else
                []
    in
    [ Browser.Events.onResize (\w h -> SetWindowSize ( w, h )) ]
        |> (++) tickCommand
        |> Sub.batch
