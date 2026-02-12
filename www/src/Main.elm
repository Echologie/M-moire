module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, button, div, h1, h2, h3, input, p, small, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onMouseDown, preventDefaultOn)
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
    | PointerMoved Float Float
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
            [ newCard 1 "Production A"
            , newCard 2 "Production B"
            , newCard 3 "Production C"
            , newCard 4 "Production D"
            , newCard 5 "Production E"
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
subscriptions model =
    let
        mouseSubs =
            case model.dragging of
                Nothing ->
                    Sub.none

                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove
                            (Decode.map2 PointerMoved
                                (Decode.field "clientX" Decode.float)
                                (Decode.field "clientY" Decode.float)
                            )
                        , Browser.Events.onMouseUp (Decode.succeed EndDrag)
                        ]
    in
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , mouseSubs
        ]


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

        PointerMoved clientX clientY ->
            case ( model.dragging, model.boardRect ) of
                ( Just dragState, Just rect ) ->
                    let
                        pos =
                            positionFromClient rect clientX clientY
                    in
                    ( { model | cards = updateCardPosition dragState.cardId pos model.cards }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
    { x = clamp 0 1 ((clientX - rect.x) / rect.width)
    , y = clamp 0 1 ((clientY - rect.y) / rect.height)
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
        , style "touch-action" "none"
        , onTouchMovePointer
        , onTouchEndDrag
        , onTouchCancelDrag
        , onPointerMovePointer
        , onPointerUpDrag
        , onPointerCancelDrag
        ]
        [ h1 [ style "margin-top" "0" ] [ text "Prototype UX – Évaluation de productions" ]
        , p []
            [ text "Glisse chaque production sur le plan selon les axes : "
            , strongText "Précision"
            , text " (horizontal) et "
            , strongText "Rigueur"
            , text " (vertical)."
            ]
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "320px 1fr"
            , style "gap" "16px"
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
        ]
        [ h2 [ style "margin" "4px 0 10px" ] [ text "Productions" ]
        , p [ style "margin" "0 0 10px", style "font-size" "14px", style "color" "#4b5a75" ] [ text "Carte non placée: glisser vers le plan." ]
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "8px" ]
            (List.map (viewCardInList model.selectedCardId) unplacedCards)
        , h3 [ style "margin" "18px 0 8px" ] [ text "Commentaire" ]
        , case selectedCard of
            Nothing ->
                p [ style "font-size" "14px", style "color" "#6b7892" ] [ text "Sélectionne une production pour commenter." ]

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
                        , placeholder "Observations sur la production..."
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


viewCardInList : Maybe Int -> Card -> Html Msg
viewCardInList selectedId card =
    let
        isSelected =
            selectedId == Just card.id
    in
    div
        [ onMouseDown (StartDrag card.id)
        , onPointerDownDrag card.id
        , onTouchStartDrag card.id
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
        , style "touch-action" "none"
        ]
        [ text card.title ]


boardPanel : Model -> List Card -> Html Msg
boardPanel model placedCards =
    div
        [ style "background" "white"
        , style "border" "1px solid #d9e0ee"
        , style "border-radius" "10px"
        , style "padding" "14px"
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
                , style "touch-action" "none"
                ]
                ([ axisLines ] ++ List.map (viewCardOnBoard model.selectedCardId) placedCards)
            , div [ style "display" "flex", style "justify-content" "space-between", style "font-size" "13px", style "margin-top" "6px", style "color" "#40506a" ]
                [ span [] [ text "Précision faible" ], span [] [ text "Précision élevée" ] ]
            ]
        , small [ style "display" "block", style "margin-top" "10px", style "color" "#6b7892" ]
            [ text "Astuce: clique une carte pour la commenter, puis glisse-la pour la repositionner." ]
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


viewCardOnBoard : Maybe Int -> Card -> Html Msg
viewCardOnBoard selectedId card =
    case card.pos of
        Nothing ->
            text ""

        Just pos ->
            let
                isSelected =
                    selectedId == Just card.id
            in
            div
                [ onMouseDown (StartDrag card.id)
                , onPointerDownDrag card.id
                , onTouchStartDrag card.id
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
                , style "touch-action" "none"
                , style "font-size" "14px"
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


onTouchStartDrag : Int -> Html.Attribute Msg
onTouchStartDrag cardId =
    preventDefaultOn "touchstart" (Decode.succeed ( StartDrag cardId, True ))


onTouchMovePointer : Html.Attribute Msg
onTouchMovePointer =
    preventDefaultOn "touchmove"
        (Decode.map
            (\( x, y ) -> ( PointerMoved x y, True ))
            touchPointDecoder
        )


onTouchEndDrag : Html.Attribute Msg
onTouchEndDrag =
    on "touchend" (Decode.succeed EndDrag)


onTouchCancelDrag : Html.Attribute Msg
onTouchCancelDrag =
    on "touchcancel" (Decode.succeed EndDrag)


onPointerDownDrag : Int -> Html.Attribute Msg
onPointerDownDrag cardId =
    preventDefaultOn "pointerdown" (Decode.succeed ( StartDrag cardId, True ))


onPointerMovePointer : Html.Attribute Msg
onPointerMovePointer =
    preventDefaultOn "pointermove"
        (Decode.map
            (\( x, y ) -> ( PointerMoved x y, True ))
            pointerPointDecoder
        )


onPointerUpDrag : Html.Attribute Msg
onPointerUpDrag =
    on "pointerup" (Decode.succeed EndDrag)


onPointerCancelDrag : Html.Attribute Msg
onPointerCancelDrag =
    on "pointercancel" (Decode.succeed EndDrag)


pointerPointDecoder : Decode.Decoder ( Float, Float )
pointerPointDecoder =
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
