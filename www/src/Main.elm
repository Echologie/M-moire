module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, button, div, h1, h2, h3, input, p, small, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Process
import Task


type alias Position =
    { x : Float
    , y : Float
    }


type alias Card =
    { id : Int
    , title : String
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
    { cardId : Int }


type alias Model =
    { cards : List Card
    , selectedCardId : Maybe Int
    , dragging : Maybe DragState
    , boardRect : Maybe BoardRect
    , email : String
    }


type Msg
    = StartDrag Int
    | DragOver
    | DropOnBoard Float Float
    | EndDrag
    | SelectCard Int
    | UpdateSelectedComment String
    | UpdateEmail String
    | RefreshBoardRect
    | GotBoardRect (Result Dom.Error Dom.Element)
    | WindowResized Int Int


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
    ( { cards =
            [ newCard 1 "Proposition A"
            , newCard 2 "Proposition B"
            , newCard 3 "Proposition C"
            , newCard 4 "Proposition D"
            , newCard 5 "Proposition E"
            ]
      , selectedCardId = Nothing
      , dragging = Nothing
      , boardRect = Nothing
      , email = ""
      }
    , Task.perform (\_ -> RefreshBoardRect) (Process.sleep 60)
    )


newCard : Int -> String -> Card
newCard id title =
    { id = id, title = title, pos = Nothing, comment = "" }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDrag cardId ->
            ( { model
                | dragging = Just { cardId = cardId }
                , selectedCardId = Just cardId
              }
            , Task.attempt GotBoardRect (Dom.getElement "board")
            )

        DragOver ->
            ( model, Cmd.none )

        DropOnBoard clientX clientY ->
            case ( model.dragging, model.boardRect ) of
                ( Just dragState, Just rect ) ->
                    let
                        pos =
                            positionFromClient rect clientX clientY
                    in
                    ( { model
                        | cards = updateCardPosition dragState.cardId pos model.cards
                        , dragging = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | dragging = Nothing }, Cmd.none )

        EndDrag ->
            ( { model | dragging = Nothing }, Cmd.none )

        SelectCard cardId ->
            ( { model | selectedCardId = Just cardId }, Cmd.none )

        UpdateSelectedComment newComment ->
            case model.selectedCardId of
                Nothing ->
                    ( model, Cmd.none )

                Just cardId ->
                    ( { model | cards = updateCardComment cardId newComment model.cards }, Cmd.none )

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


updateCardPosition : Int -> Position -> List Card -> List Card
updateCardPosition cardId pos cards =
    List.map
        (\card ->
            if card.id == cardId then
                { card | pos = Just pos }

            else
                card
        )
        cards


updateCardComment : Int -> String -> List Card -> List Card
updateCardComment cardId newComment cards =
    List.map
        (\card ->
            if card.id == cardId then
                { card | comment = newComment }

            else
                card
        )
        cards


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
        placedCards =
            List.filter (\card -> card.pos /= Nothing) model.cards

        unplacedCards =
            List.filter (\card -> card.pos == Nothing) model.cards
    in
    div
        [ style "font-family" "system-ui, sans-serif"
        , style "margin" "0"
        , style "padding" "20px"
        , style "background" "#f5f7fb"
        , style "min-height" "100vh"
        ]
        [ h1 [ style "margin-top" "0" ] [ text "Prototype UX – Évaluation de productions" ]
        , p []
            [ text "Glisse chaque proposition sur le plan selon les axes : "
            , strongText "Précision"
            , text " (horizontal) et "
            , strongText "Rigueur"
            , text " (vertical)."
            ]
        , div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "gap" "16px"
            , style "align-items" "flex-start"
            ]
            [ leftPanel model unplacedCards
            , boardPanel model placedCards
            ]
        ]


strongText : String -> Html msg
strongText txt =
    span [ style "font-weight" "700" ] [ text txt ]


leftPanel : Model -> List Card -> Html Msg
leftPanel model unplacedCards =
    let
        selectedCard =
            selectedCardFromModel model
    in
    div
        [ style "background" "white"
        , style "border" "1px solid #d9e0ee"
        , style "border-radius" "10px"
        , style "padding" "14px"
        , style "flex" "1 1 300px"
        , style "max-width" "360px"
        ]
        [ h2 [ style "margin" "4px 0 10px" ] [ text "Propositions" ]
        , p [ style "margin" "0 0 10px", style "font-size" "14px", style "color" "#4b5a75" ] [ text "Carte non placée : glisser vers le plan." ]
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "8px" ]
            (List.map (viewCardInList model.selectedCardId model.dragging) unplacedCards)
        , h3 [ style "margin" "18px 0 8px" ] [ text "Commentaire" ]
        , case selectedCard of
            Nothing ->
                p [ style "font-size" "14px", style "color" "#6b7892" ] [ text "Sélectionne une proposition pour commenter." ]

            Just card ->
                div []
                    [ p [ style "margin" "0 0 6px", style "font-weight" "600" ] [ text card.title ]
                    , textarea
                        [ rows 6
                        , style "width" "100%"
                        , style "resize" "vertical"
                        , style "padding" "8px"
                        , style "border" "1px solid #c7d3ea"
                        , style "border-radius" "8px"
                        , placeholder "Observations sur la proposition..."
                        , value card.comment
                        , onInput UpdateSelectedComment
                        ]
                        []
                    ]
        , h3 [ style "margin" "18px 0 8px" ] [ text "Email (optionnel)" ]
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
            [ text "L'email est séparé des évaluations et reste facultatif." ]
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


viewCardInList : Maybe Int -> Maybe DragState -> Card -> Html Msg
viewCardInList selectedId dragging card =
    let
        isSelected =
            selectedId == Just card.id

        isDragging =
            case dragging of
                Just dragState ->
                    dragState.cardId == card.id

                Nothing ->
                    False
    in
    div
        [ draggable "true"
        , onDragStartCard card.id
        , onDragEndCard
        , onClick (SelectCard card.id)
        , style "padding" "10px"
        , style "border"
            (if isSelected then
                "2px solid #0f62fe"

             else
                "1px solid #c7d3ea"
            )
        , style "border-radius" "8px"
        , style "background" "#fbfcff"
        , style "cursor" "grab"
        , style "user-select" "none"
        , style "opacity"
            (if isDragging then
                "0"

             else
                "1"
            )
        ]
        [ text card.title ]


boardPanel : Model -> List Card -> Html Msg
boardPanel model placedCards =
    div
        [ style "background" "white"
        , style "border" "1px solid #d9e0ee"
        , style "border-radius" "10px"
        , style "padding" "14px"
        , style "flex" "2 1 520px"
        , style "min-width" "280px"
        ]
        [ h2 [ style "margin" "4px 0 10px" ] [ text "Plan de positionnement" ]
        , div [ style "position" "relative" ]
            [ div [ style "display" "flex", style "justify-content" "space-between", style "font-size" "13px", style "margin-bottom" "4px", style "color" "#40506a" ]
                [ span [] [ text "Rigueur élevée" ], span [] [ text "" ] ]
            , div
                [ id "board"
                , style "position" "relative"
                , style "height" "560px"
                , style "border" "1px solid #b9c9e6"
                , style "border-radius" "8px"
                , style "background" "linear-gradient(180deg, #f9fbff 0%, #f2f6ff 100%)"
                ]
                ([ axisLines ] ++ List.map (viewCardOnBoard model.selectedCardId model.dragging) placedCards ++ [ dragOverlay model.dragging ])
            , div [ style "display" "flex", style "justify-content" "space-between", style "font-size" "13px", style "margin-top" "6px", style "color" "#40506a" ]
                [ span [] [ text "Précision faible" ], span [] [ text "Précision élevée" ] ]
            ]
        , small [ style "display" "block", style "margin-top" "10px", style "color" "#6b7892" ]
            [ text "Astuce : appuie longuement puis glisse pour déplacer une carte sur mobile." ]
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


viewCardOnBoard : Maybe Int -> Maybe DragState -> Card -> Html Msg
viewCardOnBoard selectedId dragging card =
    case card.pos of
        Nothing ->
            text ""

        Just pos ->
            let
                isSelected =
                    selectedId == Just card.id

                isDragging =
                    case dragging of
                        Just dragState ->
                            dragState.cardId == card.id

                        Nothing ->
                            False
            in
            div
                [ draggable "true"
                , onDragStartCard card.id
                , onDragEndCard
                , onClick (SelectCard card.id)
                , style "position" "absolute"
                , style "left" (String.fromFloat (pos.x * 100) ++ "%")
                , style "top" (String.fromFloat (pos.y * 100) ++ "%")
                , style "transform" "translate(-50%, -50%)"
                , style "padding" "8px 10px"
                , style "border"
                    (if isSelected then
                        "2px solid #0f62fe"

                     else
                        "1px solid #7a92c8"
                    )
                , style "background" "white"
                , style "border-radius" "8px"
                , style "box-shadow" "0 1px 5px rgba(0,0,0,0.08)"
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
                [ text card.title ]


selectedCardFromModel : Model -> Maybe Card
selectedCardFromModel model =
    case model.selectedCardId of
        Nothing ->
            Nothing

        Just cardId ->
            List.filter (\card -> card.id == cardId) model.cards
                |> List.head


onDragStartCard : Int -> Html.Attribute Msg
onDragStartCard cardId =
    on "dragstart" (Decode.succeed (StartDrag cardId))


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
