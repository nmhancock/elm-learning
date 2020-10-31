module DecisionTree exposing (DecisionTree(..), treeDecoder)

import Json.Decode as Decode


type alias SplitRecord =
    { error : Float
    , samples: Int
    , value: List Float
    , label: String
    , children: List DecisionTree
    }

type alias LeafRecord =
    { error : Float
    , samples: Int
    , value: List Float
    , label: String
    }

type DecisionTree = Split SplitRecord | Leaf LeafRecord

splitDecoder =
    Decode.map5 SplitRecord
        (Decode.field "error" Decode.float)
        (Decode.field "samples" Decode.int)
        (Decode.field "value" (Decode.list Decode.float))
        (Decode.field "label" Decode.string)
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> treeDecoder))))
    |> Decode.map Split
    
leafDecoder =
    Decode.map4 LeafRecord
        (Decode.field "error" Decode.float)
        (Decode.field "samples" Decode.int)
        (Decode.field "value" (Decode.list Decode.float))
        (Decode.field "label" Decode.string)
    |> Decode.map Leaf

treeDecoder = Decode.oneOf ([ splitDecoder, leafDecoder ])
