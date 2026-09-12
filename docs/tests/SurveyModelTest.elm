module SurveyModelTest exposing (suite)

import Dict
import Expect
import Fuzz
import Survey.Model as S
import Test exposing (Test, describe, fuzz3, test)


suite : Test
suite =
    describe "Coordonnées indépendantes et validation du sondage"
        [ fuzz3 (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) "le curseur X conserve exactement Y et Z" <|
            \x y z ->
                Expect.equal (S.Point (clamp -10 10 x) y z) (S.move "x" x (S.Point 1 y z))
        , fuzz3 (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) "le curseur Y conserve exactement X et Z" <|
            \x y z ->
                Expect.equal (S.Point x (clamp -10 10 y) z) (S.move "y" y (S.Point x 2 z))
        , fuzz3 (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) "le curseur Z conserve exactement X et Y" <|
            \x y z ->
                Expect.equal (S.Point x y (clamp -10 10 z)) (S.move "z" z (S.Point x y 3))
        , test "la vue libre ne change aucune coordonnée" <|
            \_ -> Expect.equal (S.Point 1 2 3) (S.move "3d" 10 (S.Point 1 2 3))
        , test "une valeur hors limites ne modifie que l’axe demandé" <|
            \_ -> Expect.equal (S.Point -10 2 3) (S.move "x" -200 (S.Point 1 2 3))
        , test "le zéro explicite se distingue d’une absence de jugement" <|
            \_ ->
                let
                    q =
                        S.Question "q" "Tle spé" "Analyse" "Énoncé" [ S.Production "p" "Texte" ]

                    a =
                        S.answer

                    graded =
                        { a | note = Just 0, initialNote = Just 0 }

                    placed =
                        { graded | judged = [ "x", "y", "z" ] }
                in
                Expect.equal ( False, False, True ) ( S.complete Dict.empty q, S.complete (Dict.singleton "p" graded) q, S.complete (Dict.singleton "p" placed) q )
        , test "répéter un axe ne valide jamais les autres" <|
            \_ ->
                Expect.equal [ "x" ] (S.touchAxis "unknown" (S.touchAxis "x" (S.touchAxis "x" [])))
        ]
