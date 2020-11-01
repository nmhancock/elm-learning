module DecisionTreeTest exposing (..)

import Array exposing (fromList)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import DecisionTree exposing (Tree(..), treeDecoder)
import Test exposing (..)


leafJson =
    """
    {
        "threshold": 0.12345,
        "feature": 0,
        "probabilities": [ [ 0.123, 0.456 ] ],
        "label": "Test label"
    }
    """

splitJson =
    """
    {
        "threshold": 0.12345,
        "feature": 1,
        "probabilities": [ [ 0.123, 0.456 ] ],
        "label": "Test label",
        "left": {
            "threshold": 0.12345,
            "feature": 0,
            "probabilities": [ [ 0.789, 0.101 ] ],
            "label": "Test label"
        },
        "right": {
            "threshold": 0.12345,
            "feature": 0,
            "probabilities": [ [ 0.121, 0.141 ] ],
            "label": "Test label"
        }
    }
    """

suite : Test
suite =
    describe "DecisionTree"
        [ test "decode leaf" <|
            \() ->
                {
                    threshold = 0.12345,
                    feature = 0,
                    probabilities = fromList [ fromList [ 0.123, 0.456 ] ]
                }
                    |> (\decision -> Node decision Empty Empty)
                    |> Ok
                    |> Expect.equal (decodeString treeDecoder leafJson)
        , test "decode split" <|
            \() ->
                (Node
                    {
                        threshold = 0.12345
                        , feature = 1
                        , probabilities = fromList [ fromList [ 0.123, 0.456 ] ]
                    }
                    (Node
                        {
                            threshold = 0.12345
                            , feature = 0
                            , probabilities = fromList [ fromList [ 0.789, 0.101 ] ]
                        }
                        Empty
                        Empty
                    )
                    (Node
                        {
                            threshold = 0.12345
                            , feature = 0
                            , probabilities = fromList [ fromList [ 0.121, 0.141 ] ]
                        }
                        Empty
                        Empty
                    )
                )
                    |> Ok
                    |> Expect.equal (decodeString treeDecoder splitJson)
        ]
