module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, fill, fillPortion, height, padding, px, rgb255, rgba255, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type AppState
    = INTRO
    | RUNNING
    | WIN
    | LOSE LosingCondition


type LosingCondition
    = VEGETARIAN
    | CARNIVORUS
    | CARNAGE


type Entity
    = CABBAGE
    | GOAT
    | WOLF


type alias Entities =
    ( Maybe Entity, Maybe Entity, Maybe Entity )


type alias GameState =
    { moveCount : Int
    , origin : Entities
    , target : Entities
    , boatPosition : Position
    , boatLoad : Maybe Entity
    }


type alias Model =
    { appState : AppState
    , gameState : GameState
    }


type Position
    = ORIGIN
    | TARGET


modelInitialValue : Model
modelInitialValue =
    { appState = INTRO
    , gameState =
        { moveCount = 0
        , origin =
            ( Just CABBAGE
            , Just GOAT
            , Just WOLF
            )
        , target = ( Nothing, Nothing, Nothing )
        , boatPosition = ORIGIN
        , boatLoad = Nothing
        }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( modelInitialValue, Cmd.none )


updateEntity : Entities -> Entity -> Maybe Entity -> Entities
updateEntity ( a, b, c ) target value =
    case target of
        CABBAGE ->
            ( value, b, c )

        GOAT ->
            ( a, value, c )

        WOLF ->
            ( a, b, value )


removeEntity : Entities -> Entity -> Entities
removeEntity entities target =
    updateEntity entities target Nothing


addEntity : Entities -> Entity -> Entities
addEntity entities target =
    updateEntity entities target (Just target)


boardEntity : GameState -> Entity -> GameState
boardEntity gameState entity =
    let
        currentPos =
            gameState.boatPosition

        currentEntities =
            case currentPos of
                ORIGIN ->
                    gameState.origin

                TARGET ->
                    gameState.target

        newEntities =
            let
                temp =
                    removeEntity currentEntities entity
            in
            case gameState.boatLoad of
                Just e ->
                    addEntity temp e

                Nothing ->
                    temp
    in
    case currentPos of
        ORIGIN ->
            { gameState | origin = newEntities, boatLoad = Just entity }

        TARGET ->
            { gameState | target = newEntities, boatLoad = Just entity }


unBoard : GameState -> Entity -> GameState
unBoard gameState entity =
    let
        currentPos =
            gameState.boatPosition

        currentEntiies =
            case currentPos of
                ORIGIN ->
                    gameState.origin

                TARGET ->
                    gameState.target

        newEntities =
            addEntity currentEntiies entity
    in
    case currentPos of
        ORIGIN ->
            { gameState | origin = newEntities, boatLoad = Nothing }

        TARGET ->
            { gameState | target = newEntities, boatLoad = Nothing }


togglePosition : Position -> Position
togglePosition pos =
    case pos of
        ORIGIN ->
            TARGET

        TARGET ->
            ORIGIN


cross : GameState -> GameState
cross gameState =
    { gameState | boatPosition = togglePosition gameState.boatPosition, moveCount = gameState.moveCount + 1 }


checkUnattended : GameState -> AppState
checkUnattended gameState =
    let
        unattendedSide =
            case gameState.boatPosition of
                ORIGIN ->
                    gameState.target

                TARGET ->
                    gameState.origin
    in
    case unattendedSide of
        ( Just _, Just _, Just _ ) ->
            LOSE CARNAGE

        ( Just _, Just _, Nothing ) ->
            LOSE VEGETARIAN

        ( Nothing, Just _, Just _ ) ->
            LOSE CARNIVORUS

        _ ->
            RUNNING


checkWin : Entities -> AppState
checkWin target =
    case target of
        ( Just _, Just _, Just _ ) ->
            WIN

        _ ->
            RUNNING


type Msg
    = START
    | BOARD Entity
    | UNBOARD Entity
    | CROSS
    | CHECK_UNATTENDED
    | CHECK_WIN


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        START ->
            ( { modelInitialValue | appState = RUNNING }, Cmd.none )

        BOARD entity ->
            ( { model | gameState = boardEntity model.gameState entity }, Cmd.none )

        UNBOARD entity ->
            update CHECK_WIN { model | gameState = unBoard model.gameState entity }

        CROSS ->
            update CHECK_UNATTENDED { model | gameState = cross model.gameState, appState = checkUnattended model.gameState }

        CHECK_UNATTENDED ->
            ( { model | appState = checkUnattended model.gameState }, Cmd.none )

        CHECK_WIN ->
            ( { model | appState = checkWin model.gameState.target }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


gameDescription =
    """This is "goat, cabbage, wolf".

In order to win the puzzle, the farmer must get all of his possesion on the other side of the river.
However, he knows that, when he is not close, his animals behave instinctively:
\t- He can not leave the goat with the cabbage, other wise the goat would eat it.
\t- He cannot leave the wolf with the goat, otherwise murder ensue.

Good luck!
"""


view : Model -> Browser.Document Msg
view model =
    { title = "That cabbage game"
    , body =
        [ Element.layout
            []
            (case model.appState of
                INTRO ->
                    Element.column
                        [ width fill
                        , height fill
                        , centerX
                        , centerY
                        , Element.inFront
                            (modal gameDescription "To the game!" START)
                        ]
                        [ gameView model ]

                RUNNING ->
                    gameView model

                WIN ->
                    Element.column
                        [ width fill
                        , height fill
                        , centerX
                        , centerY
                        , Element.inFront
                            (modal "You did it! The famrer can now go home without having lost anything!" "Restart" START)
                        ]
                        [ gameView model ]

                LOSE reason ->
                    let
                        msg =
                            case reason of
                                VEGETARIAN ->
                                    "YOU LOSE:\nFLAVOUR: VEGETARIAN\n\nArg! The goat ate the cabbage..."

                                CARNIVORUS ->
                                    "YOU LOSE:\nFLAVOUR: CARNIVOROUS\n\nArg! The wolf has killed the goat!"

                                CARNAGE ->
                                    "YOU LOSE:\nFLAVOUR: CARNAGE\n\nDude! Where you even trying!"
                    in
                    Element.column
                        [ width fill
                        , height fill
                        , centerX
                        , centerY
                        , Element.inFront
                            (modal
                                msg
                                "Restart"
                                START
                            )
                        ]
                        [ gameView model ]
            )
        ]
    }


gameView : Model -> Element Msg
gameView model =
    Element.column [ width fill, height fill ]
        [ Element.row
            [ Background.color <| rgb255 103 148 54
            , height <| fillPortion 1
            , width fill
            ]
            [ Element.el [] <| text "Origin"
            , Element.row [ centerY, centerX, spacing 10 ] <|
                entitiesView model.gameState.origin
            ]
        , Element.row
            [ Background.color <| rgb255 66 122 161
            , height <| fillPortion 3
            , width fill
            ]
            [ Element.el [] <| text "River"
            , Element.row
                [ centerX
                , centerY
                , padding 50
                , Background.color <| rgb255 55 50 42
                ]
                [ Element.row
                    [ width fill
                    , height fill
                    , spacing 25
                    ]
                    [ Element.el
                        [ width <| px 100
                        , height <| px 100
                        , centerY
                        , centerX
                        , Border.dashed
                        , Border.width <| 5
                        , Border.color <| rgb255 255 255 255
                        , Font.color <| rgb255 255 255 255
                        ]
                      <|
                        Element.el [ centerY, centerX ] <|
                            text "Farmer"
                    , Element.el
                        [ width <| px 100
                        , height <| px 100
                        , centerY
                        , alignLeft
                        , Font.color <| rgb255 255 255 255
                        ]
                      <|
                        case model.gameState.boatPosition of
                            ORIGIN ->
                                button [ centerY, centerX ]
                                    { label = text "Cross to target"
                                    , onPress = Just CROSS
                                    }

                            TARGET ->
                                button [ centerY, centerX ]
                                    { label = text "Cross to origin"
                                    , onPress = Just CROSS
                                    }
                    , Element.el
                        [ width <| px 100
                        , height <| px 100
                        , centerY
                        , alignRight
                        , Border.dashed
                        , Border.width <| 5
                        , Border.color <| rgb255 66 122 161
                        ]
                      <|
                        Element.el [ centerY, centerX ] <|
                            case model.gameState.boatLoad of
                                Nothing ->
                                    text ""

                                Just e ->
                                    button [ centerY, centerX ]
                                        { label = entityView model.gameState.boatLoad
                                        , onPress = Just (UNBOARD e)
                                        }
                    ]
                ]
            ]
        , Element.row
            [ Background.color <| rgb255 103 148 54
            , height <| fillPortion 1
            , width fill
            ]
            [ text "Target"
            , Element.row [ centerX, centerY, spacing 10 ] <| entitiesView model.gameState.target
            ]
        ]


entitiesView : Entities -> List (Element Msg)
entitiesView entities =
    let
        ( a, b, c ) =
            entities

        entityList =
            List.filter (\x -> x /= Nothing) [ a, b, c ]
    in
    List.map
        (\x ->
            case x of
                Nothing ->
                    Element.el [ width <| px 100, height <| px 100 ] <| entityView x

                Just e ->
                    boardEntityButton e <| entityView x
        )
        entityList


entityView : Maybe Entity -> Element Msg
entityView mEntity =
    case mEntity of
        Just CABBAGE ->
            text "Cabbage"

        Just GOAT ->
            text "Goat"

        Just WOLF ->
            text "Wolf"

        Nothing ->
            text ""


boardEntityButton : Entity -> Element Msg -> Element Msg
boardEntityButton entity label =
    button
        [ width <| px 100
        , height <| px 100
        , Border.dashed
        , Border.width <| 5
        , Border.color <| rgb255 255 255 255
        ]
        { onPress = Just <| BOARD entity
        , label = label
        }


modal : String -> String -> Msg -> Element Msg
modal description confirmLabel onConfirm =
    Element.column
        [ width fill
        , height fill
        , centerX
        , centerY
        , Background.color <| rgba255 255 255 255 0.5
        ]
        [ Element.column
            [ centerX
            , centerY
            , Border.rounded 20
            , padding
                20
            , height <| (fill |> Element.maximum 300 |> Element.minimum 200)
            , width <| (fill |> Element.maximum 900 |> Element.minimum 500)
            , Background.color <| rgb255 255 255 255
            ]
            [ Element.el
                [ centerX
                , centerY
                ]
              <|
                text
                    description
            , button
                [ alignRight
                , Border.rounded 10
                , padding 10
                , Background.color <|
                    rgb255 66 122 161
                ]
                { label = text confirmLabel, onPress = Just onConfirm }
            ]
        ]
