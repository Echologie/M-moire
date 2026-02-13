module Test exposing (main)

import Browser
import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode


type alias Model =
    { expanded : Bool
    }


type Msg
    = Open
    | Close
    | NoOp


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { expanded = False }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Open ->
            { model | expanded = True }

        Close ->
            { model | expanded = False }

        NoOp ->
            model


view : Model -> Html Msg
view model =
    div
        [ style "margin" "0"
        , style "width" "100vw"
        , style "height" "100vh"
        , style "background" "white"
        , style "position" "relative"
        , style "font-family" "Georgia, serif"
        ]
        [ viewMiniCard
        , if model.expanded then
            viewExpandedOverlay

          else
            text ""
        ]


viewMiniCard : Html Msg
viewMiniCard =
    div
        [ style "position" "absolute"
        , style "left" "50%"
        , style "top" "50%"
        , style "transform" "translate(-50%, -50%) scale(0.25)"
        , style "transform-origin" "center center"
        , style "width" "880px"
        , style "padding" "20px"
        , style "border" "1px solid #8ba7d6"
        , style "border-radius" "12px"
        , style "background" "#f9fbff"
        , style "box-shadow" "0 8px 20px rgba(0,0,0,0.12)"
        , style "cursor" "pointer"
        , onClick Open
        ]
        [ h2 [ style "margin" "0 0 8px", style "font-size" "34px" ] [ text "Proposition A" ]
        , p [ style "margin" "0", style "font-size" "26px", style "line-height" "1.4" ] [ text "On part de cos(2x) = sin(x), puis on ecrit 1 - 2sin^2(x) = sin(x)." ]
        , p [ style "margin" "10px 0 0", style "font-size" "26px", style "line-height" "1.4" ] [ text "On obtient 2sin^2(x) + sin(x) - 1 = 0, puis (2sin(x)-1)(sin(x)+1)=0." ]
        , p [ style "margin" "10px 0 0", style "font-size" "22px", style "color" "#43506d" ] [ text "Cliquer pour agrandir" ]
        ]


viewExpandedOverlay : Html Msg
viewExpandedOverlay =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "left" "0"
        , style "background" "rgba(16,24,40,0.35)"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "z-index" "9999"
        , onClick Close
        ]
        [ div
            [ stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            , style "width" "min(1100px, 94vw)"
            , style "max-height" "92vh"
            , style "overflow" "auto"
            , style "padding" "24px"
            , style "border" "1px solid #8ba7d6"
            , style "border-radius" "14px"
            , style "background" "white"
            , style "box-shadow" "0 24px 50px rgba(0,0,0,0.25)"
            ]
            [ h2 [ style "margin" "0 0 10px", style "font-size" "40px" ] [ text "Proposition A" ]
            , p [ style "margin" "0", style "font-size" "30px", style "line-height" "1.5" ] [ text "On part de cos(2x) = sin(x), puis on ecrit 1 - 2sin^2(x) = sin(x)." ]
            , p [ style "margin" "14px 0 0", style "font-size" "30px", style "line-height" "1.5" ] [ text "On obtient 2sin^2(x) + sin(x) - 1 = 0, puis (2sin(x)-1)(sin(x)+1)=0." ]
            , p [ style "margin" "14px 0 0", style "font-size" "26px", style "line-height" "1.5" ] [ text "Solutions sur [0;2pi[: x = pi/6, 5pi/6, 3pi/2." ]
            , p [ style "margin" "16px 0 0", style "font-size" "20px", style "color" "#5a6785" ] [ text "Cliquer en dehors pour refermer." ]
            ]
        ]
