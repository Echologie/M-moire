module BoardLogicTest exposing (suite)

import BoardLogic
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "BoardLogic"
        [ test "movedBeyond detecte un drag reel" <|
            \_ ->
                Expect.equal True (BoardLogic.movedBeyond 4 100 100 112 100)
        , test "movedBeyond reste faux sur un clic quasi immobile" <|
            \_ ->
                Expect.equal False (BoardLogic.movedBeyond 4 100 100 102 102)
        , test "nextClampedPosition borne en bord gauche" <|
            \_ ->
                let
                    rect =
                        { x = 0, y = 0, width = 1000, height = 600 }

                    pos =
                        BoardLogic.nextClampedPosition 320 206 0.68 rect 400 200 0.2 0.2 -500 200
                in
                Expect.atMost 0.00001 (abs (pos.x - 0.1088))
        , test "nextClampedPosition borne en bord droit" <|
            \_ ->
                let
                    rect =
                        { x = 0, y = 0, width = 1000, height = 600 }

                    pos =
                        BoardLogic.nextClampedPosition 320 206 0.68 rect 400 200 0.8 0.3 5000 200
                in
                Expect.atMost 0.00001 (abs (pos.x - 0.8912))
        ]
