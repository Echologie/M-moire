module Main exposing (main)

import Animator
import Animator.Inline
import BoardLogic
import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, h3, input, p, small, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import MathML as Math
import MathML.Attributes as MathAttr
import Process
import Task
import Time


type alias Position =
    { x : Float
    , y : Float
    }


type alias Proposition =
    { id : Int
    , badge : String
    , title : String
    , previewFormula : FormulaId
    , steps : List String
    , pos : Maybe Position
    , comment : String
    }


type alias BoardRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias DragState =
    { propositionId : Int
    , startMouseX : Float
    , startMouseY : Float
    , startCardX : Float
    , startCardY : Float
    , moved : Bool
    }


type alias Viewport =
    { width : Int
    , height : Int
    }


type ZoomState
    = Mini
    | Maxi


type FormulaId
    = FormulaCosLinear
    | FormulaQuadratic
    | FormulaProduct
    | FormulaGeneral


type alias Model =
    { propositions : List Proposition
    , selectedPropositionId : Maybe Int
    , expandedPropositionId : Maybe Int
    , focusTimeline : Animator.Timeline ZoomState
    , dragging : Maybe DragState
    , suppressNextOpen : Bool
    , boardRect : Maybe BoardRect
    , email : String
    , viewport : Viewport
    }


type Msg
    = StartDrag Int Float Float
    | PointerMove Float Float
    | PointerUp
    | MiniMouseUp Int
    | TouchEndOnMini Int
    | CloseExpanded
    | UpdateExpandedComment String
    | UpdateEmail String
    | RefreshBoardRect
    | GotBoardRect (Result Dom.Error Dom.Element)
    | WindowResized Int Int
    | AnimatorTick Time.Posix


miniatureWidth : Float
miniatureWidth =
    320


miniatureHeight : Float
miniatureHeight =
    206


miniScale : Float
miniScale =
    0.68


overlayStartScale : Float
overlayStartScale =
    0.18


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        seeded =
            withInitialPositions initialPropositions
    in
    ( { propositions = seeded
      , selectedPropositionId = Just 1
      , expandedPropositionId = Nothing
      , focusTimeline = Animator.init Mini
      , dragging = Nothing
      , suppressNextOpen = False
      , boardRect = Nothing
      , email = ""
      , viewport = { width = 1200, height = 800 }
      }
    , Task.perform (\_ -> RefreshBoardRect) (Process.sleep 60)
    )


initialPropositions : List Proposition
initialPropositions =
    [ proposition
        1
        "A"
        "Copie A"
        FormulaCosLinear
        [ "Je remplace par cos(2x)=1-2sin(x)."
        , "Donc 1-2sin(x)=sin(x) puis 1=3sin(x)."
        , "Alors sin(x)=1/3, donc x≈0,34 ou x≈2,80."
        ]
    , proposition
        2
        "B"
        "Copie B"
        FormulaQuadratic
        [ "On part de cos(2x)=1-2sin²(x)."
        , "On obtient 1-2sin²(x)=sin(x), donc 2sin²(x)+sin(x)-1=0."
        , "En posant y=sin(x) : 2y²+y-1=0, d'ou y=1/2 ou y=-1."
        , "Donc x=π/6, 5π/6 ou 3π/2 sur l'intervalle."
        ]
    , proposition
        3
        "C"
        "Copie C"
        FormulaProduct
        [ "Comme cos(2x)=1-2sin²(x), on a 2sin²(x)+sin(x)-1=0."
        , "Factorisation : (2sin(x)-1)(sin(x)+1)=0."
        , "Alors sin(x)=1/2 ou sin(x)=-1."
        , "Dans [0;2π[ : x appartient a {π/6, 5π/6, 3π/2}."
        ]
    , proposition
        4
        "D"
        "Copie D"
        FormulaGeneral
        [ "Identite : cos(2x)=1-2sin²(x), donc 2sin²(x)+sin(x)-1=0."
        , "Produit nul : (2sin(x)-1)(sin(x)+1)=0."
        , "Cas 1 : sin(x)=1/2, donc x=π/6+2kπ ou x=5π/6+2kπ."
        , "Cas 2 : sin(x)=-1, donc x=3π/2+2kπ."
        , "Intersection avec [0;2π[ : S={π/6, 5π/6, 3π/2}."
        ]
    ]


withInitialPositions : List Proposition -> List Proposition
withInitialPositions propositions =
    let
        layout : Dict Int Position
        layout =
            Dict.fromList
                [ ( 1, { x = 0.18, y = 0.14 } )
                , ( 2, { x = 0.38, y = 0.14 } )
                , ( 3, { x = 0.58, y = 0.14 } )
                , ( 4, { x = 0.78, y = 0.14 } )
                ]
    in
    List.map
        (\item ->
            { item | pos = Dict.get item.id layout }
        )
        propositions


proposition : Int -> String -> String -> FormulaId -> List String -> Proposition
proposition id badge title previewFormula steps =
    { id = id
    , badge = badge
    , title = title
    , previewFormula = previewFormula
    , steps = steps
    , pos = Nothing
    , comment = ""
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Browser.Events.onMouseMove mouseMoveDecoder
        , Browser.Events.onMouseUp (Decode.succeed PointerUp)
        , Animator.toSubscription AnimatorTick model animator
        ]


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching .focusTimeline
            (\newFocus currentModel ->
                { currentModel | focusTimeline = newFocus }
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDrag propositionId clientX clientY ->
            let
                startPosition =
                    propositionPosition propositionId model.propositions
                        |> Maybe.withDefault { x = 0.5, y = 0.5 }
            in
            ( { model
                | dragging =
                    Just
                        { propositionId = propositionId
                        , startMouseX = clientX
                        , startMouseY = clientY
                        , startCardX = startPosition.x
                        , startCardY = startPosition.y
                        , moved = False
                        }
                , selectedPropositionId = Just propositionId
                , expandedPropositionId = Nothing
                , suppressNextOpen = False
                , focusTimeline = animateZoomTo Mini model.focusTimeline
              }
            , Task.attempt GotBoardRect (Dom.getElement "board")
            )

        PointerMove clientX clientY ->
            case ( model.dragging, model.boardRect ) of
                ( Just dragState, Just rect ) ->
                    let
                        nextPos =
                            BoardLogic.nextClampedPosition
                                miniatureWidth
                                miniatureHeight
                                miniScale
                                rect
                                dragState.startMouseX
                                dragState.startMouseY
                                dragState.startCardX
                                dragState.startCardY
                                clientX
                                clientY

                        movedNow =
                            dragState.moved || BoardLogic.movedBeyond 4 dragState.startMouseX dragState.startMouseY clientX clientY
                    in
                    ( { model
                        | propositions = updatePropositionPosition dragState.propositionId nextPos model.propositions
                        , dragging = Just { dragState | moved = movedNow }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PointerUp ->
            case model.dragging of
                Nothing ->
                    ( model, Cmd.none )

                Just dragState ->
                    ( { model
                        | dragging = Nothing
                        , suppressNextOpen = dragState.moved
                      }
                    , Cmd.none
                    )

        MiniMouseUp propositionId ->
            finishMiniRelease propositionId model

        TouchEndOnMini propositionId ->
            finishMiniRelease propositionId model

        CloseExpanded ->
            case model.expandedPropositionId of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    ( { model
                        | expandedPropositionId = Nothing
                        , focusTimeline = animateZoomTo Mini model.focusTimeline
                      }
                    , Cmd.none
                    )

        UpdateExpandedComment newComment ->
            case model.expandedPropositionId of
                Nothing ->
                    ( model, Cmd.none )

                Just propositionId ->
                    ( { model | propositions = updatePropositionComment propositionId newComment model.propositions }, Cmd.none )

        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        RefreshBoardRect ->
            ( model, Task.attempt GotBoardRect (Dom.getElement "board") )

        GotBoardRect result ->
            case result of
                Ok element ->
                    ( { model
                        | boardRect =
                            Just
                                { x = element.element.x
                                , y = element.element.y
                                , width = element.element.width
                                , height = element.element.height
                                }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        WindowResized width height ->
            ( { model | viewport = { width = width, height = height } }
            , Task.perform (\_ -> RefreshBoardRect) (Process.sleep 24)
            )

        AnimatorTick now ->
            ( Animator.update now animator model, Cmd.none )


animateZoomTo : ZoomState -> Animator.Timeline ZoomState -> Animator.Timeline ZoomState
animateZoomTo zoomState timeline =
    Animator.go (Animator.millis 220) zoomState timeline


finishMiniRelease : Int -> Model -> ( Model, Cmd Msg )
finishMiniRelease propositionId model =
    case model.dragging of
        Just dragState ->
            if dragState.propositionId /= propositionId then
                ( model, Cmd.none )

            else if dragState.moved then
                ( { model | dragging = Nothing, suppressNextOpen = False }, Cmd.none )

            else
                openOverlay propositionId { model | dragging = Nothing }

        Nothing ->
            openOverlay propositionId model


openOverlay : Int -> Model -> ( Model, Cmd Msg )
openOverlay propositionId model =
    if model.suppressNextOpen then
        ( { model | suppressNextOpen = False }, Cmd.none )

    else
        ( { model
            | selectedPropositionId = Just propositionId
            , expandedPropositionId = Just propositionId
            , focusTimeline = animateZoomTo Maxi model.focusTimeline
          }
        , Cmd.none
        )


updatePropositionPosition : Int -> Position -> List Proposition -> List Proposition
updatePropositionPosition propositionId newPos propositions =
    List.map
        (\item ->
            if item.id == propositionId then
                { item | pos = Just newPos }

            else
                item
        )
        propositions


updatePropositionComment : Int -> String -> List Proposition -> List Proposition
updatePropositionComment propositionId newComment propositions =
    List.map
        (\item ->
            if item.id == propositionId then
                { item | comment = newComment }

            else
                item
        )
        propositions


propositionPosition : Int -> List Proposition -> Maybe Position
propositionPosition propositionId propositions =
    propositions
        |> List.filter (\item -> item.id == propositionId)
        |> List.head
        |> Maybe.andThen .pos


view : Model -> Html Msg
view model =
    div
        [ style "margin" "0"
        , style "min-height" "100vh"
        , style "height" "100vh"
        , style "padding" "12px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "background" "#eaf0fb"
        , style "font-family" "system-ui, sans-serif"
        ]
        [ topHeader model
        , boardView model
        , viewExpandedLayer model
        ]


topHeader : Model -> Html msg
topHeader model =
    div
        [ style "padding" "10px 12px"
        , style "border" "1px solid #d5deef"
        , style "border-radius" "10px"
        , style "background" "white"
        , style "margin-bottom" "10px"
        ]
        [ h1 [ style "margin" "0", style "font-size" "24px" ] [ text "Evaluation de productions d'eleves" ]
        , p [ style "margin" "6px 0 0", style "color" "#33425f" ]
            [ text "Exercice : resoudre "
            , span [ style "font-weight" "700" ] [ viewExerciseEquation ]
            , text " sur "
            , span [ style "font-weight" "700" ] [ viewInterval ]
            , text "."
            ]
        , p [ style "margin" "4px 0 0", style "font-size" "13px", style "color" "#4f6185" ]
            [ text
                ("Selection : "
                    ++ selectedBadgeLabel model.selectedPropositionId
                    ++ " | Placees : "
                    ++ String.fromInt (placedCount model.propositions)
                    ++ "/"
                    ++ String.fromInt (List.length model.propositions)
                )
            ]
        ]


placedCount : List Proposition -> Int
placedCount propositions =
    propositions
        |> List.filter (\item -> item.pos /= Nothing)
        |> List.length


boardView : Model -> Html Msg
boardView model =
    let
        hiddenId =
            model.expandedPropositionId

        visiblePropositions =
            case hiddenId of
                Nothing ->
                    model.propositions

                Just propositionId ->
                    List.filter (\item -> item.id /= propositionId) model.propositions
    in
    div
        [ id "board"
        , attribute "data-testid" "board"
        , onBoardTouchMove
        , onBoardTouchEnd
        , onBoardTouchCancel
        , style "position" "relative"
        , style "flex" "1"
        , style "width" "100%"
        , style "border" "1px solid #b9c9e6"
        , style "border-radius" "12px"
        , style "background" "linear-gradient(180deg, #f9fbff 0%, #f2f6ff 100%)"
        , style "overflow" "hidden"
        , style "touch-action" "none"
        ]
        ([ axisLines ]
            ++ List.map (viewMiniature model) visiblePropositions
            ++ [ boardLegend ]
        )


boardLegend : Html msg
boardLegend =
    div
        [ style "position" "absolute"
        , style "left" "10px"
        , style "right" "10px"
        , style "bottom" "8px"
        , style "display" "flex"
        , style "justify-content" "space-between"
        , style "font-size" "12px"
        , style "color" "#4c5d7f"
        ]
        [ span [] [ text "Precision faible" ]
        , span [] [ text "Rigueur elevee" ]
        ]


axisLines : Html msg
axisLines =
    div []
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "right" "0"
            , style "top" "50%"
            , style "height" "1px"
            , style "background" "#b9c9e6"
            ]
            []
        , div
            [ style "position" "absolute"
            , style "top" "0"
            , style "bottom" "0"
            , style "left" "50%"
            , style "width" "1px"
            , style "background" "#b9c9e6"
            ]
            []
        ]


viewMiniature : Model -> Proposition -> Html Msg
viewMiniature model item =
    case item.pos of
        Nothing ->
            text ""

        Just pos ->
            let
                isDragging =
                    case model.dragging of
                        Just dragState ->
                            dragState.propositionId == item.id

                        Nothing ->
                            False

                scaledWidth =
                    miniatureWidth * miniScale

                scaledHeight =
                    miniatureHeight * miniScale

                cursorStyle =
                    if isDragging then
                        "grabbing"

                    else
                        "grab"
            in
            div
                [ style "position" "absolute"
                , style "left" (String.fromFloat (pos.x * 100) ++ "%")
                , style "top" (String.fromFloat (pos.y * 100) ++ "%")
                , style "transform" "translate(-50%, -50%)"
                , style "width" (String.fromFloat scaledWidth ++ "px")
                , style "height" (String.fromFloat scaledHeight ++ "px")
                , style "overflow" "visible"
                , style "z-index"
                    (if isDragging then
                        "80"

                     else
                        "30"
                    )
                ]
                [ div
                    [ preventDefaultOn "mousedown"
                        (Decode.map
                            (\( x, y ) -> ( StartDrag item.id x y, True ))
                            mousePointDecoder
                        )
                    , onMiniMouseUp item.id
                    , onMiniTouchStart item.id
                    , onMiniTouchEnd item.id
                    , style "transform" ("scale(" ++ String.fromFloat miniScale ++ ")")
                    , style "transform-origin" "top left"
                    , style "position" "relative"
                    , style "width" (String.fromFloat miniatureWidth ++ "px")
                    , style "height" (String.fromFloat miniatureHeight ++ "px")
                    , style "border"
                        (if isDragging then
                            "2px solid #2563eb"

                         else
                            "1px solid #c7d3ea"
                        )
                    , style "border-radius" "12px"
                    , style "background" "#fbfdff"
                    , style "box-shadow"
                        (if isDragging then
                            "0 12px 24px rgba(15,34,80,0.25)"

                         else
                            "0 4px 12px rgba(0,0,0,0.14)"
                        )
                    , style "padding" "12px"
                    , style "overflow" "hidden"
                    , style "cursor" cursorStyle
                    , style "user-select" "none"
                    , style "touch-action" "none"
                    , style "outline" "none"
                    , attribute "data-testid" ("mini-" ++ item.badge)
                    ]
                    [ notchBadge item.badge
                    , div [ style "margin-left" "48px" ]
                        [ h3 [ style "margin" "0 0 4px", style "font-size" "16px", style "color" "#1a2947" ] [ text item.title ]
                        , p [ style "margin" "0", style "font-size" "12px", style "color" "#4f6185" ] [ text "Cliquer pour agrandir, glisser pour placer." ]
                        ]
                    , div [ style "margin-top" "12px" ]
                        [ p [ style "margin" "0", style "font-size" "12px", style "color" "#33425f" ] [ text "Version miniaturisee" ]
                        , div [ style "margin" "8px 0 0", style "font-size" "13px", style "color" "#273554" ] [ viewFormulaInline item.previewFormula ]
                        ]
                    ]
                ]


viewExpandedLayer : Model -> Html Msg
viewExpandedLayer model =
    case model.expandedPropositionId of
        Nothing ->
            text ""

        Just propositionId ->
            case propositionById propositionId model.propositions of
                Nothing ->
                    text ""

                Just item ->
                    div
                        [ style "position" "fixed"
                        , style "inset" "0"
                        , style "z-index" "9999"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "justify-content" "center"
                        , style "pointer-events" "auto"
                        , attribute "data-testid" "expanded-layer"
                        , onClick CloseExpanded
                        ]
                        [ viewExpandedCard model item ]


viewExpandedCard : Model -> Proposition -> Html Msg
viewExpandedCard model item =
    div
        [ onClick CloseExpanded
        , attribute "data-testid" "expanded-card"
        , Animator.Inline.scale model.focusTimeline
            (\zoom ->
                case zoom of
                    Mini ->
                        Animator.at overlayStartScale |> Animator.arriveSmoothly 0.75

                    Maxi ->
                        Animator.at 1 |> Animator.arriveSmoothly 0.75
            )
        , style "transform-origin" "center center"
        , style "position" "relative"
        , style "width" "min(1240px, 95vw)"
        , style "max-height" "92vh"
        , style "overflow" "auto"
        , style "background" "white"
        , style "border" "1px solid #c8d6ef"
        , style "border-radius" "14px"
        , style "padding" "16px"
        , style "box-shadow" "0 24px 56px rgba(0,0,0,0.24)"
        ]
        [ button
            [ onClick CloseExpanded
            , attribute "data-testid" "close-expanded"
            , style "position" "absolute"
            , style "top" "10px"
            , style "right" "10px"
            , style "border" "1px solid #b7c7e6"
            , style "background" "white"
            , style "border-radius" "8px"
            , style "padding" "4px 8px"
            , style "cursor" "pointer"
            , style "font-weight" "700"
            ]
            [ text "Fermer" ]
        , div [ style "position" "relative", style "padding-top" "2px" ] [ notchBadge item.badge ]
        , div [ style "margin-left" "54px", style "margin-top" "2px" ]
            [ h2 [ style "margin" "0 0 4px" ] [ text item.title ]
            , p [ style "margin" "0", style "font-size" "13px", style "color" "#4f6185" ] [ text "Version eleve" ]
            ]
        , div [ style "margin-top" "10px", style "font-size" "18px", style "color" "#243353" ] [ viewFormulaInline item.previewFormula ]
        , div [ style "margin-top" "12px" ] (List.map viewStep item.steps)
        , h3 [ style "margin" "14px 0 8px" ] [ text "Commentaire" ]
        , textarea
            [ rows 5
            , style "width" "100%"
            , style "resize" "vertical"
            , style "padding" "8px"
            , style "border" "1px solid #c7d3ea"
            , style "border-radius" "8px"
            , placeholder "Observations sur cette copie..."
            , value item.comment
            , onInput UpdateExpandedComment
            ]
            []
        , h3 [ style "margin" "12px 0 8px" ] [ text "Email (optionnel)" ]
        , input
            [ type_ "email"
            , placeholder "nom@exemple.fr"
            , value model.email
            , onInput UpdateEmail
            , style "width" "100%"
            , style "padding" "10px"
            , style "border" "1px solid #c7d3ea"
            , style "border-radius" "8px"
            ]
            []
        , small [ style "display" "block", style "margin-top" "8px", style "color" "#6b7892" ]
            [ text "Cliquer hors de la fiche pour la reduire." ]
        ]


viewExerciseEquation : Html msg
viewExerciseEquation =
    mathInline
        [ Math.mrow []
            [ Math.mi [] [ text "cos" ]
            , Math.mo [] [ text "(" ]
            , Math.mn [] [ text "2" ]
            , Math.mi [] [ text "x" ]
            , Math.mo [] [ text ")" ]
            , Math.mo [] [ text "=" ]
            , Math.mi [] [ text "sin" ]
            , Math.mo [] [ text "(" ]
            , Math.mi [] [ text "x" ]
            , Math.mo [] [ text ")" ]
            ]
        ]


viewInterval : Html msg
viewInterval =
    mathInline
        [ Math.mrow []
            [ Math.mo [] [ text "[" ]
            , Math.mn [] [ text "0" ]
            , Math.mo [] [ text ";" ]
            , Math.mn [] [ text "2" ]
            , Math.mi [] [ text "π" ]
            , Math.mo [] [ text "[" ]
            ]
        ]


viewFormulaInline : FormulaId -> Html msg
viewFormulaInline formulaId =
    case formulaId of
        FormulaCosLinear ->
            mathInline
                [ Math.mrow []
                    [ Math.mi [] [ text "cos" ]
                    , Math.mo [] [ text "(" ]
                    , Math.mn [] [ text "2" ]
                    , Math.mi [] [ text "x" ]
                    , Math.mo [] [ text ")" ]
                    , Math.mo [] [ text "=" ]
                    , Math.mn [] [ text "1" ]
                    , Math.mo [] [ text "-" ]
                    , Math.mn [] [ text "2" ]
                    , Math.mi [] [ text "sin" ]
                    , Math.mo [] [ text "(" ]
                    , Math.mi [] [ text "x" ]
                    , Math.mo [] [ text ")" ]
                    ]
                ]

        FormulaQuadratic ->
            mathInline
                [ Math.mrow []
                    [ Math.mn [] [ text "2" ]
                    , Math.msup []
                        [ Math.mrow []
                            [ Math.mi [] [ text "sin" ]
                            , Math.mo [] [ text "(" ]
                            , Math.mi [] [ text "x" ]
                            , Math.mo [] [ text ")" ]
                            ]
                        , Math.mn [] [ text "2" ]
                        ]
                    , Math.mo [] [ text "+" ]
                    , Math.mi [] [ text "sin" ]
                    , Math.mo [] [ text "(" ]
                    , Math.mi [] [ text "x" ]
                    , Math.mo [] [ text ")" ]
                    , Math.mo [] [ text "-" ]
                    , Math.mn [] [ text "1" ]
                    , Math.mo [] [ text "=" ]
                    , Math.mn [] [ text "0" ]
                    ]
                ]

        FormulaProduct ->
            mathInline
                [ Math.mrow []
                    [ Math.mo [] [ text "(" ]
                    , Math.mn [] [ text "2" ]
                    , Math.mi [] [ text "sin" ]
                    , Math.mo [] [ text "(" ]
                    , Math.mi [] [ text "x" ]
                    , Math.mo [] [ text ")" ]
                    , Math.mo [] [ text "-" ]
                    , Math.mn [] [ text "1" ]
                    , Math.mo [] [ text ")" ]
                    , Math.mo [] [ text "(" ]
                    , Math.mi [] [ text "sin" ]
                    , Math.mo [] [ text "(" ]
                    , Math.mi [] [ text "x" ]
                    , Math.mo [] [ text ")" ]
                    , Math.mo [] [ text "+" ]
                    , Math.mn [] [ text "1" ]
                    , Math.mo [] [ text ")" ]
                    , Math.mo [] [ text "=" ]
                    , Math.mn [] [ text "0" ]
                    ]
                ]

        FormulaGeneral ->
            mathInline
                [ Math.mrow []
                    [ Math.mi [] [ text "x" ]
                    , Math.mo [] [ text "=" ]
                    , Math.mfrac []
                        [ Math.mi [] [ text "π" ]
                        , Math.mn [] [ text "6" ]
                        ]
                    , Math.mo [] [ text "+" ]
                    , Math.mn [] [ text "2" ]
                    , Math.mi [] [ text "k" ]
                    , Math.mi [] [ text "π" ]
                    ]
                ]


mathInline : List (Html msg) -> Html msg
mathInline nodes =
    Math.math
        [ MathAttr.display "inline"
        , MathAttr.xmlns "http://www.w3.org/1998/Math/MathML"
        ]
        nodes


viewStep : String -> Html msg
viewStep stepText =
    p [ style "margin" "6px 0", style "line-height" "1.35", style "color" "#1f2a44" ] [ text stepText ]


notchBadge : String -> Html msg
notchBadge badge =
    div
        [ style "position" "absolute"
        , style "top" "8px"
        , style "left" "8px"
        , style "min-width" "34px"
        , style "height" "26px"
        , style "padding" "0 8px"
        , style "border-radius" "999px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "14px"
        , style "font-weight" "800"
        , style "color" "white"
        , style "background" "linear-gradient(135deg, #1d4ed8 0%, #2563eb 100%)"
        , style "box-shadow" "0 2px 8px rgba(29,78,216,0.35)"
        ]
        [ text badge ]


selectedBadgeLabel : Maybe Int -> String
selectedBadgeLabel maybeId =
    case maybeId of
        Just propositionId ->
            case propositionId of
                1 ->
                    "A"

                2 ->
                    "B"

                3 ->
                    "C"

                4 ->
                    "D"

                _ ->
                    "?"

        Nothing ->
            "aucune"


propositionById : Int -> List Proposition -> Maybe Proposition
propositionById propositionId propositions =
    propositions
        |> List.filter (\item -> item.id == propositionId)
        |> List.head


onMiniTouchStart : Int -> Html.Attribute Msg
onMiniTouchStart propositionId =
    preventDefaultOn "touchstart"
        (Decode.map
            (\( clientX, clientY ) -> ( StartDrag propositionId clientX clientY, True ))
            touchPointDecoder
        )


onMiniMouseUp : Int -> Html.Attribute Msg
onMiniMouseUp propositionId =
    stopPropagationOn "mouseup" (Decode.succeed ( MiniMouseUp propositionId, True ))


onMiniTouchEnd : Int -> Html.Attribute Msg
onMiniTouchEnd propositionId =
    stopPropagationOn "touchend" (Decode.succeed ( TouchEndOnMini propositionId, True ))


onBoardTouchMove : Html.Attribute Msg
onBoardTouchMove =
    preventDefaultOn "touchmove"
        (Decode.map
            (\( clientX, clientY ) -> ( PointerMove clientX clientY, True ))
            touchPointDecoder
        )


onBoardTouchEnd : Html.Attribute Msg
onBoardTouchEnd =
    on "touchend" (Decode.succeed PointerUp)


onBoardTouchCancel : Html.Attribute Msg
onBoardTouchCancel =
    on "touchcancel" (Decode.succeed PointerUp)


mouseMoveDecoder : Decode.Decoder Msg
mouseMoveDecoder =
    Decode.map2 PointerMove
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


mousePointDecoder : Decode.Decoder ( Float, Float )
mousePointDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


touchPointDecoder : Decode.Decoder ( Float, Float )
touchPointDecoder =
    Decode.oneOf
        [ Decode.map2 Tuple.pair
            (Decode.at [ "touches", "0", "clientX" ] Decode.float)
            (Decode.at [ "touches", "0", "clientY" ] Decode.float)
        , Decode.map2 Tuple.pair
            (Decode.at [ "changedTouches", "0", "clientX" ] Decode.float)
            (Decode.at [ "changedTouches", "0", "clientY" ] Decode.float)
        ]
