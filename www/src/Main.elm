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
    , level : String
    , summary : String
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


type alias Model =
    { propositions : List Proposition
    , activePropositionId : Maybe Int
    , dragging : Maybe DragState
    , boardRect : Maybe BoardRect
    , email : String
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
        "A1"
        "Rédaction 1"
        "Niveau de rigueur : faible"
        "Erreur de formule, conclusion numérique approximative."
        [ "On cherche les solutions de $\\cos(2x)=\\sin(x)$ sur $[0;2\\pi[$."
        , "Je remplace par $\\cos(2x)=1-2\\sin(x)$."
        , "Donc $1-2\\sin(x)=\\sin(x)$ puis $1=3\\sin(x)$."
        , "Alors $\\sin(x)=\\dfrac{1}{3}$, d'ou $x\\approx0{,}34$ ou $x\\approx2{,}80$."
        ]
    , proposition
        2
        "B2"
        "Rédaction 2"
        "Niveau de rigueur : moyen"
        "Bonne methode algebrique, justification partielle des angles."
        [ "On part de $\\cos(2x)=\\sin(x)$ et de $\\cos(2x)=1-2\\sin^2(x)$."
        , "On obtient $1-2\\sin^2(x)=\\sin(x)$, donc $2\\sin^2(x)+\\sin(x)-1=0$."
        , "En posant $y=\\sin(x)$ : $2y^2+y-1=0$, d'ou $y=\\dfrac{1}{2}$ ou $y=-1$."
        , "Donc $x=\\dfrac{\\pi}{6}$, $\\dfrac{5\\pi}{6}$ ou $\\dfrac{3\\pi}{2}$ sur l'intervalle."
        ]
    , proposition
        3
        "C3"
        "Rédaction 3"
        "Niveau de rigueur : bon"
        "Demarche correcte avec etapes explicites et valeurs exactes."
        [ "On resout $\\cos(2x)=\\sin(x)$ sur $[0;2\\pi[$."
        , "Comme $\\cos(2x)=1-2\\sin^2(x)$, on a $2\\sin^2(x)+\\sin(x)-1=0$."
        , "Factorisation : $(2\\sin(x)-1)(\\sin(x)+1)=0$."
        , "Alors $\\sin(x)=\\dfrac{1}{2}$ ou $\\sin(x)=-1$."
        , "Dans $[0;2\\pi[$ : $x\\in\\left\\{\\dfrac{\\pi}{6},\\dfrac{5\\pi}{6},\\dfrac{3\\pi}{2}\\right\\}$."
        ]
    , proposition
        4
        "D4"
        "Rédaction 4"
        "Niveau de rigueur : tres bon"
        "Resolution complete avec ensemble general puis restriction."
        [ "Equation : $\\cos(2x)=\\sin(x)$."
        , "Identite : $\\cos(2x)=1-2\\sin^2(x)$, donc $2\\sin^2(x)+\\sin(x)-1=0$."
        , "Produit nul : $(2\\sin(x)-1)(\\sin(x)+1)=0$."
        , "Cas 1 : $\\sin(x)=\\dfrac{1}{2}\\iff x=\\dfrac{\\pi}{6}+2k\\pi$ ou $x=\\dfrac{5\\pi}{6}+2k\\pi$."
        , "Cas 2 : $\\sin(x)=-1\\iff x=\\dfrac{3\\pi}{2}+2k\\pi$."
        , "Intersection avec $[0;2\\pi[$ : $S=\\left\\{\\dfrac{\\pi}{6},\\dfrac{5\\pi}{6},\\dfrac{3\\pi}{2}\\right\\}$."
        ]
    ]


proposition : Int -> String -> String -> String -> String -> List String -> Proposition
proposition id badge title level summary steps =
    { id = id
    , badge = badge
    , title = title
    , level = level
    , summary = summary
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
                                case firstUnplacedId updated of
                                    Just nextId ->
                                        Just nextId

                                    Nothing ->
                                        Just dragState.propositionId
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
            ( { model | activePropositionId = Just propositionId }
            , scheduleMathRender
            )

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

        WindowResized _ _ ->
            ( model, Task.perform (\_ -> RefreshBoardRect) (Process.sleep 20) )

        RenderMathNow ->
            ( model, renderMath "refresh" )


scheduleMathRender : Cmd Msg
scheduleMathRender =
    Task.perform (\_ -> RenderMathNow) (Process.sleep 20)


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
        placedCount =
            List.filter (\item -> item.pos /= Nothing) model.propositions |> List.length

        totalCount =
            List.length model.propositions

        placedPropositions =
            List.filter (\item -> item.pos /= Nothing) model.propositions
    in
    div
        [ style "font-family" "system-ui, sans-serif"
        , style "margin" "0"
        , style "padding" "20px"
        , style "background" "#f5f7fb"
        , style "min-height" "100vh"
        ]
        [ h1 [ style "margin-top" "0" ] [ text "Evaluation de redactions - Prototype UX" ]
        , p [ style "margin" "0 0 12px", style "color" "#33425f" ]
            [ text "Exercice teste (niveau premiere) : resoudre "
            , span [ style "font-weight" "700" ] [ text "$\\cos(2x)=\\sin(x)$" ]
            , text " sur "
            , span [ style "font-weight" "700" ] [ text "$[0;2\\pi[$" ]
            , text "."
            ]
        , p [ style "margin" "0 0 18px", style "color" "#5a6986" ]
            [ text ("Progression : " ++ String.fromInt placedCount ++ " / " ++ String.fromInt totalCount ++ " proposition(s) placee(s)") ]
        , div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "gap" "16px"
            , style "align-items" "flex-start"
            ]
            [ leftPanel model
            , boardPanel model placedPropositions
            ]
        ]


leftPanel : Model -> Html Msg
leftPanel model =
    let
        active =
            activeProposition model
    in
    div
        [ style "background" "white"
        , style "border" "1px solid #d9e0ee"
        , style "border-radius" "10px"
        , style "padding" "14px"
        , style "flex" "1 1 360px"
        , style "max-width" "520px"
        ]
        [ h2 [ style "margin" "4px 0 12px" ] [ text "Proposition a evaluer" ]
        , case active of
            Nothing ->
                p [ style "color" "#5a6986" ] [ text "Toutes les propositions sont placees. Tu peux cliquer une miniature sur le plan pour la rouvrir en grand." ]

            Just item ->
                div []
                    [ viewLargePropositionCard model.dragging item
                    , h3 [ style "margin" "14px 0 8px" ] [ text "Commentaire" ]
                    , textarea
                        [ rows 5
                        , style "width" "100%"
                        , style "resize" "vertical"
                        , style "padding" "8px"
                        , style "border" "1px solid #c7d3ea"
                        , style "border-radius" "8px"
                        , placeholder "Observations sur cette redaction..."
                        , value item.comment
                        , onInput UpdateSelectedComment
                        ]
                        []
                    ]
        , h3 [ style "margin" "16px 0 8px" ] [ text "Email (optionnel)" ]
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
        , button
            [ style "margin-top" "14px"
            , style "padding" "10px 14px"
            , style "background" "#0f62fe"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "8px"
            , style "cursor" "pointer"
            ]
            [ text "Valider (MVP sans backend)" ]
        ]


viewLargePropositionCard : Maybe DragState -> Proposition -> Html Msg
viewLargePropositionCard dragging item =
    div
        [ style "position" "relative"
        , style "border" "1px solid #c8d6ef"
        , style "border-radius" "12px"
        , style "background" "#fbfdff"
        , style "padding" "14px"
        ]
        [ badgeView item.badge "18px"
        , div [ style "margin-left" "62px" ]
            [ h3 [ style "margin" "0 0 4px" ] [ text item.title ]
            , p [ style "margin" "0", style "font-size" "13px", style "color" "#4f6185" ] [ text item.level ]
            ]
        , p [ style "margin" "10px 0 8px", style "color" "#22314f", style "font-weight" "600" ] [ text "Version complete" ]
        , div [] (List.map viewStep item.steps)
        , p [ style "margin" "12px 0 6px", style "color" "#5a6986", style "font-size" "13px" ]
            [ text "Glisse la miniature pour positionner cette redaction sur le plan." ]
        , viewDragMiniature dragging item
        ]


viewStep : String -> Html msg
viewStep stepText =
    p [ style "margin" "6px 0", style "line-height" "1.4", style "color" "#1f2a44" ] [ text stepText ]


viewDragMiniature : Maybe DragState -> Proposition -> Html Msg
viewDragMiniature dragging item =
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
        , style "position" "relative"
        , style "width" "170px"
        , style "min-height" "64px"
        , style "border-radius" "12px"
        , style "border" "1px solid #9cb4e6"
        , style "background" "white"
        , style "padding" "10px 10px 10px 12px"
        , style "box-shadow" "0 4px 12px rgba(20,55,120,0.10)"
        , style "cursor" "grab"
        , style "user-select" "none"
        , style "opacity"
            (if isDragging then
                "0"

             else
                "1"
            )
        ]
        [ badgeView item.badge "14px"
        , div [ style "padding-left" "54px", style "font-size" "12px", style "color" "#253556" ]
            [ div [ style "font-weight" "700", style "margin-bottom" "2px" ] [ text item.title ]
            , div [ style "font-size" "11px", style "color" "#5f6f8e" ] [ text item.summary ]
            ]
        ]


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
        , style "letter-spacing" "0.5px"
        ]
        [ text label ]


boardPanel : Model -> List Proposition -> Html Msg
boardPanel model placedPropositions =
    div
        [ style "background" "white"
        , style "border" "1px solid #d9e0ee"
        , style "border-radius" "10px"
        , style "padding" "14px"
        , style "flex" "2 1 520px"
        , style "min-width" "280px"
        ]
        [ h2 [ style "margin" "4px 0 10px" ] [ text "Plan Precision / Rigueur" ]
        , div [ style "position" "relative" ]
            [ div [ style "display" "flex", style "justify-content" "space-between", style "font-size" "13px", style "margin-bottom" "4px", style "color" "#40506a" ]
                [ span [] [ text "Rigueur elevee" ], span [] [ text "" ] ]
            , div
                [ id "board"
                , style "position" "relative"
                , style "height" "560px"
                , style "border" "1px solid #b9c9e6"
                , style "border-radius" "8px"
                , style "background" "linear-gradient(180deg, #f9fbff 0%, #f2f6ff 100%)"
                ]
                ([ axisLines ] ++ List.map (viewPlacedMiniature model.activePropositionId model.dragging) placedPropositions ++ [ dragOverlay model.dragging ])
            , div [ style "display" "flex", style "justify-content" "space-between", style "font-size" "13px", style "margin-top" "6px", style "color" "#40506a" ]
                [ span [] [ text "Precision faible" ], span [] [ text "Precision elevee" ] ]
            ]
        , small [ style "display" "block", style "margin-top" "10px", style "color" "#6b7892" ]
            [ text "Cliquer une miniature la rouvre en grand dans le panneau de gauche." ]
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


viewPlacedMiniature : Maybe Int -> Maybe DragState -> Proposition -> Html Msg
viewPlacedMiniature activeId dragging item =
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
            in
            div
                [ draggable "true"
                , onDragStartCard item.id
                , onDragEndCard
                , onClick (SelectProposition item.id)
                , style "position" "absolute"
                , style "left" (String.fromFloat (pos.x * 100) ++ "%")
                , style "top" (String.fromFloat (pos.y * 100) ++ "%")
                , style "transform" "translate(-50%, -50%)"
                , style "width" "170px"
                , style "min-height" "64px"
                , style "border"
                    (if isActive then
                        "2px solid #0f62fe"

                     else
                        "1px solid #7a92c8"
                    )
                , style "background" "white"
                , style "border-radius" "12px"
                , style "box-shadow" "0 2px 8px rgba(0,0,0,0.10)"
                , style "padding" "10px 10px 10px 12px"
                , style "cursor" "grab"
                , style "user-select" "none"
                , style "font-size" "14px"
                , style "opacity"
                    (if isDragging then
                        "0"

                     else
                        "1"
                    )
                ]
                [ badgeView item.badge "14px"
                , div [ style "padding-left" "54px", style "font-size" "12px", style "color" "#253556" ]
                    [ div [ style "font-weight" "700", style "margin-bottom" "2px" ] [ text item.title ]
                    , div [ style "font-size" "11px", style "color" "#5f6f8e" ] [ text item.summary ]
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
