module Survey.Model exposing (Answer, Point, Production, Question, answer, axes, complete, decodeQuestion, encodeAnswer, encodePoint, move, point, touchPlane)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


type alias Point =
    { x : Float, y : Float, z : Float }


type alias Answer =
    { note : Maybe Float, initialNote : Maybe Float, point : Point, judged : List String }


type alias Production =
    { id : String, content : String }


type alias Question =
    { id : String, level : String, domain : String, statement : String, productions : List Production }


point : Point
point =
    Point 0 0 0


answer : Answer
answer =
    Answer Nothing Nothing point []


axes : List ( String, String, String )
axes =
    [ ( "x", "Confus", "Lisible" ), ( "y", "Vague", "Précis" ), ( "z", "Fautif", "Valide" ) ]


{-| The hidden coordinate is preserved here, independently of the renderer.
-}
move : String -> Point -> Point -> Point
move plane incoming previous =
    let
        bounded n =
            clamp -10 10 n
    in
    case plane of
        "xy" ->
            { previous | x = bounded incoming.x, y = bounded incoming.y }

        "xz" ->
            { previous | x = bounded incoming.x, z = bounded incoming.z }

        "yz" ->
            { previous | y = bounded incoming.y, z = bounded incoming.z }

        _ ->
            previous


touchPlane : String -> List String -> List String
touchPlane plane previous =
    List.foldl
        (\a found ->
            if List.member a found then
                found

            else
                a :: found
        )
        previous
        (case plane of
            "xy" ->
                [ "x", "y" ]

            "xz" ->
                [ "x", "z" ]

            "yz" ->
                [ "y", "z" ]

            _ ->
                []
        )


complete : Dict String Answer -> Question -> Bool
complete answers question =
    List.all (\p -> Dict.get p.id answers |> Maybe.map (\a -> a.note /= Nothing && List.length a.judged == 3) |> Maybe.withDefault False) question.productions


decodeQuestion : D.Decoder Question
decodeQuestion =
    D.map5 Question
        (D.field "id" D.string)
        (D.field "level" D.string)
        (D.field "domain" D.string)
        (D.field "statement" D.string)
        (D.field "productions" (D.list (D.map2 Production (D.field "id" D.string) (D.field "content" D.string))))


encodePoint : Point -> E.Value
encodePoint p =
    E.object [ ( "x", E.float p.x ), ( "y", E.float p.y ), ( "z", E.float p.z ) ]


encodeAnswer : Answer -> E.Value
encodeAnswer a =
    E.object
        [ ( "note", Maybe.map E.float a.note |> Maybe.withDefault E.null )
        , ( "initialNote", Maybe.map E.float a.initialNote |> Maybe.withDefault E.null )
        , ( "coordinates", encodePoint a.point )
        , ( "evaluatedAxes", E.list E.string a.judged )
        ]
