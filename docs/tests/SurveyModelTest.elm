module SurveyModelTest exposing (suite)

import Dict
import Expect
import Fuzz
import Survey.Model as S
import Test exposing (Test, describe, fuzz3, test)


suite : Test
suite =
    describe "Coordonnées et validation du sondage"
        [ fuzz3 (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) "un déplacement XY conserve exactement Z" <|
            \x y z ->
                let
                    previous =
                        S.Point 1 2 z
                in
                Expect.within (Expect.Absolute 0) z (S.move "xy" (S.Point x y 999) previous).z
        , fuzz3 (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) "un déplacement XZ conserve exactement Y" <|
            \x y z ->
                Expect.within (Expect.Absolute 0) y (S.move "xz" (S.Point x 999 z) (S.Point 1 y 3)).y
        , fuzz3 (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100) "un déplacement YZ conserve exactement X" <|
            \x y z ->
                Expect.within (Expect.Absolute 0) x (S.move "yz" (S.Point 999 y z) (S.Point x 2 3)).x
        , test "la vue libre ne change aucune coordonnée" <|
            \_ -> Expect.equal (S.Point 1 2 3) (S.move "3d" (S.Point 10 10 10) (S.Point 1 2 3))
        , test "seuls les axes visibles sont bornés" <|
            \_ -> Expect.equal (S.Point -10 10 3) (S.move "xy" (S.Point -200 200 10) (S.Point 1 2 3))
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
        , test "deux faces suffisent, répéter la même face ne valide pas le troisième axe" <|
            \_ ->
                Expect.equal ( 2, 3 ) ( List.length (S.touchPlane "xy" (S.touchPlane "xy" [])), List.length (S.touchPlane "xz" (S.touchPlane "xy" [])) )
        ]
