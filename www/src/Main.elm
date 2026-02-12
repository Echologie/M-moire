port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, button, div, h1, h2, h3, input, p, small, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Process
import Task


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
    { propositionId : Int }


type alias Viewport =
    { width : Int
    , height : Int
    }


type alias Model =
    { propositions : List Proposition
    , activePropositionId : Maybe Int
    , dragging : Maybe DragState
    , boardRect : Maybe BoardRect
    , email : String
    , viewport : Viewport
    }


type Msg
    = StartDrag Int
    | DragOver
    | DropOnBoard Float Float
    | EndDrag
    | SelectProposition Int
    | UpdateSelectedComment String
    | UpdateEmail String
    | RefreshBoardRect
    | GotBoardRect (Result Dom.Error Dom.Element)
    | WindowResized Int Int
    | RenderMathNow


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
        initial =
            initialPropositions
    in
    ( { propositions = initial
      , activePropositionId = initial |> List.head |> Maybe.map .id
      , dragging = Nothing
      , boardRect = Nothing
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
        [ "On cherche les solutions de $\\cos(2x)=\\sin(x)$ sur $[0;2\\pi[$."
        , "Je remplace par $\\cos(2x)=1-2\\sin(x)$."
        , "Donc $1-2\\sin(x)=\\sin(x)$ puis $1=3\\sin(x)$."
        , "Alors $\\sin(x)=\\dfrac{1}{3}$, donc $x\\approx0{,}34$ ou $x\\approx2{,}80$."
        ]
    , proposition
        2
        "B"
        "Copie B"
        "$2\\sin^2(x)+\\sin(x)-1=0$"
        [ "On part de $\\cos(2x)=\\sin(x)$ et de $\\cos(2x)=1-2\\sin^2(x)$."
        , "On obtient $1-2\\sin^2(x)=\\sin(x)$, donc $2\\sin^2(x)+\\sin(x)-1=0$."
        , "En posant $y=\\sin(x)$ : $2y^2+y-1=0$, d'ou $y=\\dfrac{1}{2}$ ou $y=-1$."
        , "Donc $x=\\dfrac{\\pi}{6}$, $\\dfrac{5\\pi}{6}$ ou $\\dfrac{3\\pi}{2}$ sur l'intervalle."
        ]
    , proposition
        3
        "C"
        "Copie C"
        "$(2\\sin(x)-1)(\\sin(x)+1)=0$"
        [ "On resout $\\cos(2x)=\\sin(x)$ sur $[0;2\\pi[$."
        , "Comme $\\cos(2x)=1-2\\sin^2(x)$, on a $2\\sin^2(x)+\\sin(x)-1=0$."
        , "Factorisation : $(2\\sin(x)-1)(\\sin(x)+1)=0$."
        , "Alors $\\sin(x)=\\dfrac{1}{2}$ ou $\\sin(x)=-1$."
        , "Dans $[0;2\\pi[$ : $x\\in\\left\\{\\dfrac{\\pi}{6},\\dfrac{5\\pi}{6},\\dfrac{3\\pi}{2}\\right\\}$."
        ]
    , proposition
        4
        "D"
        "Copie D"
        "$x=\\dfrac{\\pi}{6}+2k\\pi$"
        [ "Equation : $\\cos(2x)=\\sin(x)$."
        , "Identite : $\\cos(2x)=1-2\\sin^2(x)$, donc $2\\sin^2(x)+\\sin(x)-1=0$."
        , "Produit nul : $(2\\sin(x)-1)(\\sin(x)+1)=0$."
        , "Cas 1 : $\\sin(x)=\\dfrac{1}{2}\\iff x=\\dfrac{\\pi}{6}+2k\\pi$ ou $x=\\dfrac{5\\pi}{6}+2k\\pi$."
        , "Cas 2 : $\\sin(x)=-1\\iff x=\\dfrac{3\\pi}{2}+2k\\pi$."
        , "Intersection avec $[0;2\\pi[$ : $S=\\left\\{\\dfrac{\\pi}{6},\\dfrac{5\\pi}{6},\\dfrac{3\\pi}{2}\\right\\}$."
        ]
    ]


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
subscriptions _ =
    Browser.Events.onResize WindowResized


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDrag propositionId ->
            ( { model
                | dragging = Just { propositionId = propositionId }
                , activePropositionId = Just propositionId
              }
            , Task.attempt GotBoardRect (Dom.getElement "board")
            )

        DragOver ->
            ( model, Cmd.none )

        DropOnBoard clientX clientY ->
            case ( model.dragging, model.boardRect ) of
                ( Just dragState, Just rect ) ->
                    let
                        alreadyPlaced =
                            isPlaced dragState.propositionId model.propositions

                        pos =
                            positionFromClient rect clientX clientY

                        updated =
                            updatePropositionPosition dragState.propositionId pos model.propositions

                        nextActive =
                            if alreadyPlaced then
                                Just dragState.propositionId

                            else
                                firstUnplacedId updated |> Maybe.withDefault dragState.propositionId |> Just
                    in
                    ( { model
                        | propositions = updated
                        , dragging = Nothing
                        , activePropositionId = nextActive
                      }
                    , scheduleMathRender
                    )

                _ ->
                    ( { model | dragging = Nothing }, Cmd.none )

        EndDrag ->
            ( { model | dragging = Nothing }, Cmd.none )

        SelectProposition propositionId ->
            ( { model | activePropositionId = Just propositionId }, scheduleMathRender )

        UpdateSelectedComment newComment ->
            case model.activePropositionId of
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
            , Task.perform (\_ -> RefreshBoardRect) (Process.sleep 20)
            )

        RenderMathNow ->
            ( model, renderMath "refresh" )


scheduleMathRender : Cmd Msg
scheduleMathRender =
    Task.perform (\_ -> RenderMathNow) (Process.sleep 20)


isMobileViewport : Viewport -> Bool
isMobileViewport viewport =
    viewport.width < 980


isPlaced : Int -> List Proposition -> Bool
isPlaced propositionId propositions =
    propositions
        |> List.filter (\item -> item.id == propositionId)
        |> List.head
        |> Maybe.andThen .pos
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


firstUnplacedId : List Proposition -> Maybe Int
firstUnplacedId propositions =
    propositions
        |> List.filter (\item -> item.pos == Nothing)
        |> List.head
        |> Maybe.map .id


updatePropositionPosition : Int -> Position -> List Proposition -> List Proposition
updatePropositionPosition propositionId pos propositions =
    List.map
        (\item ->
            if item.id == propositionId then
                { item | pos = Just pos }

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


positionFromClient : BoardRect -> Float -> Float -> Position
positionFromClient rect clientX clientY =
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
    in
    { x = clamp 0 1 ((clientX - rect.x) / safeWidth)
    , y = clamp 0 1 ((clientY - rect.y) / safeHeight)
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
        placed =
            List.filter (\item -> item.pos /= Nothing) model.propositions

        totalCount =
            List.length model.propositions

        placedCount =
            List.length placed

        remainingCount =
            totalCount - placedCount

        mobile =
            isMobileViewport model.viewport
    in
    div
        [ style "font-family" "system-ui, sans-serif"
        , style "margin" "0"
        , style "padding" "16px"
        , style "background" "#eef3fb"
        , style "min-height" "100vh"
        ]
        [ topHeader placedCount totalCount remainingCount
        , if mobile then
            mobileWorkspace model placed

          else
            desktopWorkspace model placed
        ]


topHeader : Int -> Int -> Int -> Html msg
topHeader placedCount totalCount remainingCount =
    div
        [ style "margin-bottom" "12px"
        , style "padding" "12px"
        , style "border" "1px solid #d5deef"
        , style "border-radius" "12px"
        , style "background" "white"
        ]
        [ h1 [ style "margin" "0 0 8px", style "font-size" "24px" ] [ text "Evaluation de productions d'eleves" ]
        , p [ style "margin" "0", style "color" "#33425f" ]
            [ text "Exercice : resoudre "
            , span [ style "font-weight" "700" ] [ text "$\\cos(2x)=\\sin(x)$" ]
            , text " sur "
            , span [ style "font-weight" "700" ] [ text "$[0;2\\pi[$" ]
            , text "."
            ]
        , p [ style "margin" "8px 0 0", style "color" "#516182", style "font-size" "14px" ]
            [ text
                ("Placees : "
                    ++ String.fromInt placedCount
                    ++ " / "
                    ++ String.fromInt totalCount
                    ++ "  |  Restantes : "
                    ++ String.fromInt remainingCount
                )
            ]
        ]


desktopWorkspace : Model -> List Proposition -> Html Msg
desktopWorkspace model placed =
    div
        [ style "display" "flex"
        , style "gap" "16px"
        , style "align-items" "flex-start"
        ]
        [ panelView model False
        , boardPanel model placed False
        ]


mobileWorkspace : Model -> List Proposition -> Html Msg
mobileWorkspace model placed =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        [ panelView model True
        , boardPanel model placed True
        ]


panelView : Model -> Bool -> Html Msg
panelView model compact =
    let
        active =
            activeProposition model

        cardHeight =
            if compact then
                "180px"

            else
                "260px"
    in
    div
        [ style "background" "white"
        , style "border" "1px solid #d9e0ee"
        , style "border-radius" "12px"
        , style "padding" "12px"
        , style "flex" "1 1 420px"
        ]
        [ h2 [ style "margin" "0 0 8px", style "font-size" "18px" ] [ text "Copies" ]
        , tabsView model compact
        , case active of
            Nothing ->
                p [ style "color" "#5a6986", style "font-size" "14px" ] [ text "Toutes les copies sont placees. Clique une miniature pour la rouvrir." ]

            Just item ->
                div []
                    [ viewDraggableSheet model.dragging item cardHeight
                    , h3 [ style "margin" "12px 0 8px" ] [ text "Commentaire" ]
                    , textarea
                        [ rows
                            (if compact then
                                2

                             else
                                5
                            )
                        , style "width" "100%"
                        , style "resize" "vertical"
                        , style "padding" "8px"
                        , style "border" "1px solid #c7d3ea"
                        , style "border-radius" "8px"
                        , placeholder "Observations sur cette copie..."
                        , value item.comment
                        , onInput UpdateSelectedComment
                        ]
                        []
                    ]
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
        , p [ style "font-size" "12px", style "color" "#6b7892", style "margin" "8px 0 0" ]
            [ text "L'email reste facultatif et separe des evaluations." ]
        ]


tabsView : Model -> Bool -> Html Msg
tabsView model compact =
    div
        [ style "display" "flex"
        , style "flex-wrap" "wrap"
        , style "gap" "8px"
        , style "margin-bottom" "10px"
        ]
        (List.map (tabItem model.activePropositionId model.propositions compact) model.propositions)


tabItem : Maybe Int -> List Proposition -> Bool -> Proposition -> Html Msg
tabItem activeId allPropositions compact item =
    let
        isActive =
            activeId == Just item.id

        isAlreadyPlaced =
            isPlaced item.id allPropositions
    in
    button
        [ onClick (SelectProposition item.id)
        , style "display" "inline-flex"
        , style "align-items" "center"
        , style "gap" "6px"
        , style "padding" "6px 10px"
        , style "border-radius" "999px"
        , style "border"
            (if isActive then
                "2px solid #0f62fe"

             else
                "1px solid #b7c7e6"
            )
        , style "background" "white"
        , style "cursor" "pointer"
        ]
        [ miniBadge item.badge
        , if compact then
            text ""

          else
            span [ style "font-size" "13px", style "font-weight" "600", style "color" "#2d3f63" ] [ text item.title ]
        , span
            [ style "display" "inline-block"
            , style "width" "8px"
            , style "height" "8px"
            , style "border-radius" "999px"
            , style "background"
                (if isAlreadyPlaced then
                    "#16a34a"

                 else
                    "#94a3b8"
                )
            ]
            []
        ]


viewDraggableSheet : Maybe DragState -> Proposition -> String -> Html Msg
viewDraggableSheet dragging item heightText =
    let
        isDragging =
            case dragging of
                Just dragState ->
                    dragState.propositionId == item.id

                Nothing ->
                    False
    in
    div
        [ draggable "true"
        , onDragStartCard item.id
        , onDragEndCard
        , onClick (SelectProposition item.id)
        , attribute "data-drag-source" "proposition"
        , attribute "data-badge" item.badge
        , attribute "data-title" item.title
        , style "position" "relative"
        , style "border" "1px solid #c8d6ef"
        , style "border-radius" "12px"
        , style "background" "#fbfdff"
        , style "padding" "12px"
        , style "cursor" "grab"
        , style "user-select" "none"
        , style "opacity"
            (if isDragging then
                "0"

             else
                "1"
            )
        ]
        [ badgeView item.badge "18px"
        , div [ style "margin-left" "58px" ]
            [ h3 [ style "margin" "0 0 4px" ] [ text item.title ]
            , p [ style "margin" "0", style "font-size" "13px", style "color" "#4f6185" ] [ text "Version eleve" ]
            ]
        , div
            [ style "margin-top" "10px"
            , style "max-height" heightText
            , style "overflow" "auto"
            , style "padding-right" "4px"
            ]
            (List.map viewStep item.steps)
        ]


viewStep : String -> Html msg
viewStep stepText =
    p [ style "margin" "6px 0", style "line-height" "1.35", style "color" "#1f2a44" ] [ text stepText ]


miniBadge : String -> Html msg
miniBadge badge =
    span
        [ style "display" "inline-flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "min-width" "20px"
        , style "height" "20px"
        , style "border-radius" "999px"
        , style "background" "#2563eb"
        , style "color" "white"
        , style "font-size" "12px"
        , style "font-weight" "700"
        ]
        [ text badge ]


badgeView : String -> String -> Html msg
badgeView label sizeText =
    div
        [ style "position" "absolute"
        , style "top" "8px"
        , style "left" "8px"
        , style "min-width" "38px"
        , style "height" "30px"
        , style "padding" "0 8px"
        , style "border-radius" "999px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" sizeText
        , style "font-weight" "800"
        , style "color" "white"
        , style "background" "linear-gradient(135deg, #1d4ed8 0%, #2563eb 100%)"
        ]
        [ text label ]


boardPanel : Model -> List Proposition -> Bool -> Html Msg
boardPanel model placedPropositions compact =
    let
        boardHeight =
            if compact then
                "56vh"

            else
                "560px"
    in
    div
        [ style "background" "white"
        , style "border" "1px solid #d9e0ee"
        , style "border-radius" "12px"
        , style "padding" "14px"
        , style "flex" "2 1 520px"
        ]
        [ h2 [ style "margin" "4px 0 10px" ] [ text "Plan Precision / Rigueur" ]
        , div [ style "position" "relative" ]
            [ div [ style "display" "flex", style "justify-content" "space-between", style "font-size" "13px", style "margin-bottom" "4px", style "color" "#40506a" ]
                [ span [] [ text "Rigueur elevee" ], span [] [ text "" ] ]
            , div
                [ id "board"
                , style "position" "relative"
                , style "height" boardHeight
                , style "min-height" "320px"
                , style "border" "1px solid #b9c9e6"
                , style "border-radius" "8px"
                , style "background" "linear-gradient(180deg, #f9fbff 0%, #f2f6ff 100%)"
                ]
                ([ axisLines ] ++ List.map (viewPlacedMiniature model.activePropositionId model.dragging compact) placedPropositions ++ [ dragOverlay model.dragging ])
            , div [ style "display" "flex", style "justify-content" "space-between", style "font-size" "13px", style "margin-top" "6px", style "color" "#40506a" ]
                [ span [] [ text "Precision faible" ], span [] [ text "Precision elevee" ] ]
            ]
        , small [ style "display" "block", style "margin-top" "10px", style "color" "#6b7892" ]
            [ text "Cliquer une miniature la reouvre en grand. Drag tactile : appui long + glisser." ]
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


dragOverlay : Maybe DragState -> Html Msg
dragOverlay dragging =
    case dragging of
        Nothing ->
            text ""

        Just _ ->
            div
                [ onBoardDragOver
                , onBoardDrop
                , style "position" "absolute"
                , style "inset" "0"
                , style "z-index" "50"
                ]
                []


viewPlacedMiniature : Maybe Int -> Maybe DragState -> Bool -> Proposition -> Html Msg
viewPlacedMiniature activeId dragging compact item =
    case item.pos of
        Nothing ->
            text ""

        Just pos ->
            let
                isActive =
                    activeId == Just item.id

                isDragging =
                    case dragging of
                        Just dragState ->
                            dragState.propositionId == item.id

                        Nothing ->
                            False

                widthText =
                    if compact then
                        "128px"

                    else
                        "168px"
            in
            div
                [ draggable "true"
                , onDragStartCard item.id
                , onDragEndCard
                , onClick (SelectProposition item.id)
                , attribute "data-drag-source" "miniature"
                , attribute "data-badge" item.badge
                , attribute "data-title" item.title
                , style "position" "absolute"
                , style "left" (String.fromFloat (pos.x * 100) ++ "%")
                , style "top" (String.fromFloat (pos.y * 100) ++ "%")
                , style "transform" "translate(-50%, -50%)"
                , style "width" widthText
                , style "min-height" "58px"
                , style "border"
                    (if isActive then
                        "2px solid #0f62fe"

                     else
                        "1px solid #7a92c8"
                    )
                , style "background" "white"
                , style "border-radius" "12px"
                , style "box-shadow" "0 2px 8px rgba(0,0,0,0.10)"
                , style "padding" "8px 8px 8px 10px"
                , style "cursor" "grab"
                , style "user-select" "none"
                , style "opacity"
                    (if isDragging then
                        "0"

                     else
                        "1"
                    )
                ]
                [ badgeView item.badge "13px"
                , div [ style "padding-left" "48px", style "font-size" "11px", style "color" "#253556" ]
                    [ div [ style "font-weight" "700", style "margin-bottom" "2px" ] [ text item.title ]
                    , div [ style "font-size" "10px", style "color" "#5f6f8e" ] [ text item.preview ]
                    ]
                ]


activeProposition : Model -> Maybe Proposition
activeProposition model =
    case model.activePropositionId of
        Nothing ->
            Nothing

        Just activeId ->
            model.propositions
                |> List.filter (\item -> item.id == activeId)
                |> List.head


onDragStartCard : Int -> Html.Attribute Msg
onDragStartCard propositionId =
    on "dragstart" (Decode.succeed (StartDrag propositionId))


onDragEndCard : Html.Attribute Msg
onDragEndCard =
    on "dragend" (Decode.succeed EndDrag)


onBoardDragOver : Html.Attribute Msg
onBoardDragOver =
    preventDefaultOn "dragover" (Decode.succeed ( DragOver, True ))


onBoardDrop : Html.Attribute Msg
onBoardDrop =
    preventDefaultOn "drop"
        (Decode.map
            (\( x, y ) -> ( DropOnBoard x y, True ))
            dragPointDecoder
        )


dragPointDecoder : Decode.Decoder ( Float, Float )
dragPointDecoder =
    Decode.oneOf
        [ Decode.map2 Tuple.pair
            (Decode.field "pageX" Decode.float)
            (Decode.field "pageY" Decode.float)
        , Decode.map2 Tuple.pair
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)
        , Decode.map2 Tuple.pair
            (Decode.at [ "touches", "0", "pageX" ] Decode.float)
            (Decode.at [ "touches", "0", "pageY" ] Decode.float)
        , Decode.map2 Tuple.pair
            (Decode.at [ "touches", "0", "clientX" ] Decode.float)
            (Decode.at [ "touches", "0", "clientY" ] Decode.float)
        , Decode.map2 Tuple.pair
            (Decode.at [ "changedTouches", "0", "pageX" ] Decode.float)
            (Decode.at [ "changedTouches", "0", "pageY" ] Decode.float)
        , Decode.map2 Tuple.pair
            (Decode.at [ "changedTouches", "0", "clientX" ] Decode.float)
            (Decode.at [ "changedTouches", "0", "clientY" ] Decode.float)
        ]
