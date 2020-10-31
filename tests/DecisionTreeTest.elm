module DecisionTreeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import DecisionTree exposing (DecisionTree(..), treeDecoder)
import Test exposing (..)


leafJson =
    """
    {
        "error": 0.12345,
        "samples": 120,
        "value": [ 0.123, 0.456 ],
        "label": "Test label"
    }
    """

splitJson =
    """
    {
        "error": 0.12345,
        "samples": 120,
        "value": [ 0.123, 0.456 ],
        "label": "Test label",
        "children": [
            {
                "error": 0.12345,
                "samples": 120,
                "value": [ 0.123, 0.456 ],
                "label": "Test label"
            }
        ]
    }
    """

suite : Test
suite =
    describe "DecisionTree"
        [ test "decode leaf" <|
            \() ->
                {
                    error = 0.12345,
                    samples = 120,
                    value = [ 0.123, 0.456 ],
                    label = "Test label"
                }
                    |> DecisionTree.Leaf
                    |> Ok
                    |> Expect.equal (decodeString treeDecoder leafJson)
        , test "decode split" <|
            \() ->
                {
                    error = 0.12345,
                    samples = 120,
                    value = [ 0.123, 0.456 ],
                    label = "Test label",
                    children = [
                        DecisionTree.Leaf
                            {
                                error = 0.12345,
                                samples = 120,
                                value = [ 0.123, 0.456 ],
                                label = "Test label"
                            }
                    ]
                }
                    |> DecisionTree.Split
                    |> Ok
                    |> Expect.equal (decodeString treeDecoder splitJson)
        ]
