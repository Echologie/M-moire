port module Main exposing (main)

import Animator
import Animator.Inline
import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, h3, input, p, small, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import Process
import Task
import Time


port renderMath : String -> Cmd msg


type alias Position =
    { x : Float
    , y : Float
    }


type alias Proposition =
    { id : Int
    , badge : String
    , title : String
    , preview : String
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
    , startX : Float
    , startY : Float
    , moved : Bool
    , pointerOffsetX : Float
    , pointerOffsetY : Float
    }


type alias Viewport =
    { width : Int
    , height : Int
    }


type alias Model =
    { propositions : List Proposition
    , expandedPropositionId : Maybe Int
    , selectedPropositionId : Maybe Int
    , isClosingExpanded : Bool
    , dragging : Maybe DragState
    , focusTimeline : Animator.Timeline (Maybe Int)
    , boardRect : Maybe BoardRect
    , lastKeyEvent : String
    , email : String
    , viewport : Viewport
    }


type Msg
    = StartDrag Int Float Float
    | MiniPointerUp Int
    | KeyPressed String String String
    | PointerMove Float Float
    | PointerUp
    | CloseExpanded
    | FinishCloseExpanded
    | UpdateExpandedComment String
    | UpdateEmail String
    | RefreshBoardRect
    | GotBoardRect (Result Dom.Error Dom.Element)
    | WindowResized Int Int
    | RenderMathNow
    | AnimatorTick Time.Posix
    | NoOp


miniatureWidth : Float
miniatureWidth =
    320


miniatureHeight : Float
miniatureHeight =
    206


miniScale : Float
miniScale =
    0.68


focusedScale : Float
focusedScale =
    0.42


overlayClosedScale : Float
overlayClosedScale =
    0.1


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
      , expandedPropositionId = Nothing
      , selectedPropositionId = Just 1
      , isClosingExpanded = False
      , dragging = Nothing
      , focusTimeline = Animator.init Nothing
      , boardRect = Nothing
      , lastKeyEvent = "aucune"
      , email = ""
      , viewport = { width = 1200, height = 800 }
      }
    , Cmd.batch
        [ Task.perform (\_ -> RefreshBoardRect) (Process.sleep 60)
        , scheduleMathRender
        ]
    )


initialPropositions : List Proposition
initialPropositions =
    [ proposition
        1
        "A"
        "Copie A"
        "$\\cos(2x)=1-2\\sin(x)$"
        [ "Je remplace par $\\cos(2x)=1-2\\sin(x)$."
        , "Donc $1-2\\sin(x)=\\sin(x)$ puis $1=3\\sin(x)$."
        , "Alors $\\sin(x)=\\dfrac{1}{3}$, donc $x\\approx0{,}34$ ou $x\\approx2{,}80$."
        ]
    , proposition
        2
        "B"
        "Copie B"
        "$2\\sin^2(x)+\\sin(x)-1=0$"
        [ "On part de $\\cos(2x)=1-2\\sin^2(x)$."
        , "On obtient $1-2\\sin^2(x)=\\sin(x)$, donc $2\\sin^2(x)+\\sin(x)-1=0$."
        , "En posant $y=\\sin(x)$ : $2y^2+y-1=0$, d'ou $y=\\dfrac{1}{2}$ ou $y=-1$."
        , "Donc $x=\\dfrac{\\pi}{6}$, $\\dfrac{5\\pi}{6}$ ou $\\dfrac{3\\pi}{2}$ sur l'intervalle."
        ]
    , proposition
        3
        "C"
        "Copie C"
        "$(2\\sin(x)-1)(\\sin(x)+1)=0$"
        [ "Comme $\\cos(2x)=1-2\\sin^2(x)$, on a $2\\sin^2(x)+\\sin(x)-1=0$."
        , "Factorisation : $(2\\sin(x)-1)(\\sin(x)+1)=0$."
        , "Alors $\\sin(x)=\\dfrac{1}{2}$ ou $\\sin(x)=-1$."
        , "Dans $[0;2\\pi[$ : $x\\in\\left\\{\\dfrac{\\pi}{6},\\dfrac{5\\pi}{6},\\dfrac{3\\pi}{2}\\right\\}$."
        ]
    , proposition
        4
        "D"
        "Copie D"
        "$x=\\dfrac{\\pi}{6}+2k\\pi$"
        [ "Identite : $\\cos(2x)=1-2\\sin^2(x)$, donc $2\\sin^2(x)+\\sin(x)-1=0$."
        , "Produit nul : $(2\\sin(x)-1)(\\sin(x)+1)=0$."
        , "Cas 1 : $\\sin(x)=\\dfrac{1}{2}\\iff x=\\dfrac{\\pi}{6}+2k\\pi$ ou $x=\\dfrac{5\\pi}{6}+2k\\pi$."
        , "Cas 2 : $\\sin(x)=-1\\iff x=\\dfrac{3\\pi}{2}+2k\\pi$."
        , "Intersection avec $[0;2\\pi[$ : $S=\\left\\{\\dfrac{\\pi}{6},\\dfrac{5\\pi}{6},\\dfrac{3\\pi}{2}\\right\\}$."
        ]
    ]


withInitialPositions : List Proposition -> List Proposition
withInitialPositions propositions =
    let
        layout : Dict Int Position
        layout =
            Dict.fromList
                [ ( 1, { x = 0.16, y = 0.14 } )
                , ( 2, { x = 0.36, y = 0.14 } )
                , ( 3, { x = 0.56, y = 0.14 } )
                , ( 4, { x = 0.76, y = 0.14 } )
                ]
    in
    List.map
        (\item ->
            { item | pos = Dict.get item.id layout }
        )
        propositions


proposition : Int -> String -> String -> String -> List String -> Proposition
proposition id badge title preview steps =
    { id = id
    , badge = badge
    , title = title
    , preview = preview
    , steps = steps
    , pos = Nothing
    , comment = ""
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Browser.Events.onKeyDown keyDownDecoder
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
                ( offsetX, offsetY ) =
                    case ( model.boardRect, propositionPosition propositionId model.propositions ) of
                        ( Just rect, Just pos ) ->
                            let
                                centerX =
                                    rect.x + (pos.x * rect.width)

                                centerY =
                                    rect.y + (pos.y * rect.height)
                            in
                            ( clientX - centerX, clientY - centerY )

                        _ ->
                            ( 0, 0 )
            in
            ( { model
                | dragging =
                    Just
                        { propositionId = propositionId
                        , startX = clientX
                        , startY = clientY
                        , moved = False
                        , pointerOffsetX = offsetX
                        , pointerOffsetY = offsetY
                        }
                , expandedPropositionId = Nothing
                , selectedPropositionId = Just propositionId
                , isClosingExpanded = False
                , focusTimeline = animateFocusTo Nothing model.focusTimeline
              }
            , Task.attempt GotBoardRect (Dom.getElement "board")
            )

        MiniPointerUp propositionId ->
            case model.dragging of
                Just dragState ->
                    if dragState.propositionId /= propositionId then
                        ( model, Cmd.none )

                    else if dragState.moved then
                        ( { model | dragging = Nothing }, Cmd.none )

                    else
                        openExpanded propositionId { model | dragging = Nothing }

                Nothing ->
                    openExpanded propositionId { model | selectedPropositionId = Just propositionId }

        KeyPressed key code targetTag ->
            let
                keyboardInfo =
                    "key=" ++ key ++ " code=" ++ code ++ " target=" ++ String.toUpper targetTag

                modelWithKey =
                    { model | lastKeyEvent = keyboardInfo }
            in
            if isShortcutA key code && not (isEditableTarget targetTag) then
                case model.selectedPropositionId of
                    Just propositionId ->
                        openExpanded propositionId modelWithKey

                    Nothing ->
                        ( modelWithKey, Cmd.none )

            else
                ( modelWithKey, Cmd.none )

        PointerMove clientX clientY ->
            case ( model.dragging, model.boardRect ) of
                ( Just dragState, Just rect ) ->
                    let
                        movedDistance =
                            distance dragState.startX dragState.startY clientX clientY

                        hasMoved =
                            dragState.moved || (movedDistance > 10)

                        nextPos =
                            positionFromClientWithOffsetBounded
                                rect
                                clientX
                                clientY
                                dragState.pointerOffsetX
                                dragState.pointerOffsetY

                        updatedPropositions =
                            if hasMoved then
                                updatePropositionPosition dragState.propositionId nextPos model.propositions

                            else
                                model.propositions
                    in
                    ( { model
                        | propositions = updatedPropositions
                        , dragging = Just { dragState | moved = hasMoved }
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
                    if dragState.moved then
                        ( { model | dragging = Nothing }, Cmd.none )

                    else
                        openExpanded dragState.propositionId { model | dragging = Nothing }

        CloseExpanded ->
            case model.expandedPropositionId of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    if model.isClosingExpanded then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | isClosingExpanded = True
                            , focusTimeline = animateFocusTo Nothing model.focusTimeline
                          }
                        , Task.perform (\_ -> FinishCloseExpanded) (Process.sleep 190)
                        )

        FinishCloseExpanded ->
            if model.isClosingExpanded then
                ( { model
                    | expandedPropositionId = Nothing
                    , isClosingExpanded = False
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

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

        RenderMathNow ->
            ( model, renderMath "refresh" )

        AnimatorTick newTime ->
            ( Animator.update newTime animator model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


scheduleMathRender : Cmd Msg
scheduleMathRender =
    renderMath "refresh"


animateFocusTo : Maybe Int -> Animator.Timeline (Maybe Int) -> Animator.Timeline (Maybe Int)
animateFocusTo propositionId timeline =
    Animator.go (Animator.millis 240) propositionId timeline


openExpanded : Int -> Model -> ( Model, Cmd Msg )
openExpanded propositionId model =
    ( { model
        | expandedPropositionId = Just propositionId
        , selectedPropositionId = Just propositionId
        , isClosingExpanded = False
        , focusTimeline = animateFocusTo (Just propositionId) model.focusTimeline
      }
    , scheduleMathRender
    )


distance : Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 =
    sqrt (((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))


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


positionFromClientWithOffsetBounded : BoardRect -> Float -> Float -> Float -> Float -> Position
positionFromClientWithOffsetBounded rect clientX clientY pointerOffsetX pointerOffsetY =
    let
        safeWidth =
            if rect.width <= 0 then
                1

            else
                rect.width

        safeHeight =
            if rect.height <= 0 then
                1

            else
                rect.height

        centerX =
            clientX - pointerOffsetX

        centerY =
            clientY - pointerOffsetY

        rawX =
            (centerX - rect.x) / safeWidth

        rawY =
            (centerY - rect.y) / safeHeight

        marginX =
            ((miniatureWidth * miniScale) / 2) / safeWidth

        marginY =
            ((miniatureHeight * miniScale) / 2) / safeHeight
    in
    { x = clamp marginX (1 - marginX) rawX
    , y = clamp marginY (1 - marginY) rawY
    }


clamp : Float -> Float -> Float -> Float
clamp minVal maxVal value =
    if value < minVal then
        minVal

    else if value > maxVal then
        maxVal

    else
        value


view : Model -> Html Msg
view model =
    let
        overlay =
            case model.expandedPropositionId of
                Nothing ->
                    text ""

                Just propositionId ->
                    case propositionById propositionId model.propositions of
                        Just item ->
                            viewExpandedOverlay model item

                        Nothing ->
                            viewMissingOverlay propositionId
    in
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
        , overlay
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
            , span [ style "font-weight" "700" ] [ text "$\\cos(2x)=\\sin(x)$" ]
            , text " sur "
            , span [ style "font-weight" "700" ] [ text "$[0;2\\pi[$" ]
            , text "."
            ]
        , p [ style "margin" "4px 0 0", style "font-size" "13px", style "color" "#4f6185" ]
            [ text ("Selection : " ++ selectedBadgeLabel model.selectedPropositionId ++ " | Expanded : " ++ selectedBadgeLabel model.expandedPropositionId ++ " | touche A = agrandir") ]
        , p [ style "margin" "2px 0 0", style "font-size" "12px", style "color" "#6b7892" ]
            [ text ("Derniere touche: " ++ model.lastKeyEvent) ]
        ]


boardView : Model -> Html Msg
boardView model =
    div
        [ id "board"
        , onBoardMouseMove
        , onBoardMouseUp
        , onBoardMouseLeave
        , onBoardTouchMove
        , onBoardTouchEnd
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
            ++ List.map (viewMiniature model) model.propositions
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

                isExpanded =
                    model.expandedPropositionId == Just item.id

                isSelected =
                    model.selectedPropositionId == Just item.id

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
                        "70"

                     else if isExpanded then
                        "45"

                     else
                        "30"
                    )
                ]
                [ div
                    [ style "transform" ("scale(" ++ String.fromFloat miniScale ++ ")")
                    , style "transform-origin" "top left"
                    , style "position" "relative"
                    , style "width" (String.fromFloat miniatureWidth ++ "px")
                    , style "height" (String.fromFloat miniatureHeight ++ "px")
                    , style "border"
                        (if isExpanded then
                            "2px solid #0f62fe"

                         else if isDragging then
                            "2px solid #3b82f6"

                         else if isSelected then
                            "2px solid #93c5fd"

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
                    , onMiniMouseDown item.id
                    , onMiniTouchStart item.id
                    , onMiniMouseUp item.id
                    , onMiniTouchEnd item.id
                    ]
                    [ notchBadge item.badge
                    , div [ style "margin-left" "48px" ]
                        [ h3 [ style "margin" "0 0 4px", style "font-size" "16px", style "color" "#1a2947" ] [ text item.title ]
                        , p [ style "margin" "0", style "font-size" "12px", style "color" "#4f6185" ] [ text "Cliquer pour agrandir, glisser pour placer." ]
                        ]
                    , div [ style "margin-top" "12px" ]
                        [ p [ style "margin" "0", style "font-size" "12px", style "color" "#33425f" ] [ text "Version miniaturisee" ]
                        , p [ style "margin" "8px 0 0", style "font-size" "12px", style "color" "#5a6986" ] [ text "La redaction complete s'affiche au clic." ]
                        ]
                    ]
                ]


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


viewExpandedOverlay : Model -> Proposition -> Html Msg
viewExpandedOverlay model item =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "left" "0"
        , style "z-index" "9999"
        , style "background" "rgba(16,24,40,0.42)"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "opacity" "1"
        , onClick CloseExpanded
        ]
        [ div
            [ stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            , Animator.Inline.scale model.focusTimeline
                (\focusedId ->
                    if model.isClosingExpanded then
                        Animator.at overlayClosedScale |> Animator.arriveSmoothly 0.68

                    else if focusedId == Just item.id then
                        Animator.at 1 |> Animator.arriveSmoothly 0.68

                    else
                        Animator.at overlayClosedScale |> Animator.arriveSmoothly 0.68
                )
            , style "position" "relative"
            , style "transform-origin" "center center"
            , style "width" "min(1380px, 98vw)"
            , style "max-height" "92vh"
            , style "overflow" "auto"
            , style "background" "white"
            , style "border" "1px solid #c8d6ef"
            , style "border-radius" "14px"
            , style "padding" "16px"
            , style "box-shadow" "0 20px 48px rgba(0,0,0,0.24)"
            ]
            [ button
                [ onClick CloseExpanded
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
            , div [ style "margin-top" "10px", style "font-size" "12px", style "color" "#4f6185" ]
                [ text "Etat overlay: scale 0.1 -> 1" ]
            ]
        ]


viewStep : String -> Html msg
viewStep stepText =
    p [ style "margin" "6px 0", style "line-height" "1.35", style "color" "#1f2a44" ] [ text stepText ]


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


isShortcutA : String -> String -> Bool
isShortcutA key code =
    String.toLower key == "a" || code == "KeyA"


isEditableTarget : String -> Bool
isEditableTarget targetTag =
    List.member (String.toUpper targetTag) [ "INPUT", "TEXTAREA", "SELECT" ]


propositionById : Int -> List Proposition -> Maybe Proposition
propositionById propositionId propositions =
    propositions
        |> List.filter (\item -> item.id == propositionId)
        |> List.head


selectedProposition : Model -> Maybe Proposition
selectedProposition model =
    case model.expandedPropositionId of
        Nothing ->
            Nothing

        Just propositionId ->
            propositionById propositionId model.propositions


viewMissingOverlay : Int -> Html Msg
viewMissingOverlay propositionId =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "left" "0"
        , style "z-index" "2147483647"
        , style "background" "rgba(255,0,0,0.38)"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "28px"
        , style "font-weight" "800"
        , style "color" "#7f1d1d"
        ]
        [ text ("Overlay missing proposition id=" ++ String.fromInt propositionId) ]


onMiniMouseDown : Int -> Html.Attribute Msg
onMiniMouseDown propositionId =
    on "mousedown"
        (Decode.map2
            (StartDrag propositionId)
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)
        )


onMiniTouchStart : Int -> Html.Attribute Msg
onMiniTouchStart propositionId =
    preventDefaultOn "touchstart"
        (Decode.map
            (\( clientX, clientY ) -> ( StartDrag propositionId clientX clientY, True ))
            touchPointDecoder
        )


onMiniMouseUp : Int -> Html.Attribute Msg
onMiniMouseUp propositionId =
    stopPropagationOn "mouseup" (Decode.succeed ( MiniPointerUp propositionId, True ))


onMiniTouchEnd : Int -> Html.Attribute Msg
onMiniTouchEnd propositionId =
    stopPropagationOn "touchend" (Decode.succeed ( MiniPointerUp propositionId, True ))


onBoardMouseMove : Html.Attribute Msg
onBoardMouseMove =
    on "mousemove"
        (Decode.map2 PointerMove
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)
        )


onBoardMouseUp : Html.Attribute Msg
onBoardMouseUp =
    on "mouseup" (Decode.succeed PointerUp)


onBoardMouseLeave : Html.Attribute Msg
onBoardMouseLeave =
    on "mouseleave" (Decode.succeed PointerUp)


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


keyDownDecoder : Decode.Decoder Msg
keyDownDecoder =
    Decode.map3 KeyPressed
        (Decode.oneOf [ Decode.field "key" Decode.string, Decode.succeed "" ])
        (Decode.oneOf [ Decode.field "code" Decode.string, Decode.succeed "" ])
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])


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
