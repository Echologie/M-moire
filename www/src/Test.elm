module Test exposing (main)

import Animator
import Animator.Inline
import Browser
import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Time


type ZoomState
    = Mini
    | Maxi


type alias Model =
    { zoomTimeline : Animator.Timeline ZoomState
    }


type Msg
    = Open
    | Close
    | AnimatorTick Time.Posix
    | NoOp


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
    ( { zoomTimeline = Animator.init Mini }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription AnimatorTick model animator


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching .zoomTimeline
            (\newZoom currentModel ->
                { currentModel | zoomTimeline = newZoom }
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Open ->
            ( { model | zoomTimeline = animateZoom Maxi model.zoomTimeline }, Cmd.none )

        Close ->
            ( { model | zoomTimeline = animateZoom Mini model.zoomTimeline }, Cmd.none )

        AnimatorTick now ->
            ( Animator.update now animator model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


animateZoom : ZoomState -> Animator.Timeline ZoomState -> Animator.Timeline ZoomState
animateZoom target timeline =
    Animator.go (Animator.millis 220) target timeline


view : Model -> Html Msg
view model =
    div
        [ style "margin" "0"
        , style "width" "100vw"
        , style "height" "100vh"
        , style "background" "white"
        , style "position" "relative"
        , style "overflow" "hidden"
        , style "font-family" "Georgia, serif"
        , onClick Close
        ]
        [ viewCard model ]


viewCard : Model -> Html Msg
viewCard model =
    div
        [ stopPropagationOn "click" (Decode.succeed ( Open, True ))
        , style "position" "absolute"
        , style "left" "50%"
        , style "top" "50%"
        , Animator.Inline.scale model.zoomTimeline
            (\zoom ->
                case zoom of
                    Mini ->
                        Animator.at 0.25 |> Animator.arriveSmoothly 0.75

                    Maxi ->
                        Animator.at 1 |> Animator.arriveSmoothly 0.75
            )
        , style "transform-origin" "center center"
        , style "width" "min(1100px, 94vw)"
        , style "max-height" "92vh"
        , style "overflow" "auto"
        , style "padding" "24px"
        , style "border" "1px solid #8ba7d6"
        , style "border-radius" "14px"
        , style "background" "#f9fbff"
        , style "box-shadow" "0 24px 50px rgba(0,0,0,0.18)"
        ]
        [ h2 [ style "margin" "0 0 10px", style "font-size" "40px" ] [ text "Proposition A" ]
        , p [ style "margin" "0", style "font-size" "30px", style "line-height" "1.5" ] [ text "On part de cos(2x) = sin(x), puis on ecrit 1 - 2sin^2(x) = sin(x)." ]
        , p [ style "margin" "14px 0 0", style "font-size" "30px", style "line-height" "1.5" ] [ text "On obtient 2sin^2(x) + sin(x) - 1 = 0, puis (2sin(x)-1)(sin(x)+1)=0." ]
        , p [ style "margin" "14px 0 0", style "font-size" "26px", style "line-height" "1.5" ] [ text "Solutions sur [0;2pi[: x = pi/6, 5pi/6, 3pi/2." ]
        , p [ style "margin" "16px 0 0", style "font-size" "20px", style "color" "#5a6785" ] [ text "Cliquer sur la fiche pour agrandir. Cliquer ailleurs pour reduire." ]
        ]
