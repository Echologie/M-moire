module Example exposing (suite)

import Expect
import Test exposing (Test, test)


suite : Test
suite =
    test "smoke" <| \_ ->
        Expect.equal 1 1
