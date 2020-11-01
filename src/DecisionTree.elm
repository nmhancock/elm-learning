module DecisionTree exposing (Tree(..), treeDecoder)

import Array exposing (Array, get)
import Json.Decode as Decode

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)

type alias Decision =
    { threshold : Float
    , feature: Int
    , probabilities: Array (Array Float)
    }
type alias DecisionTree = (Tree Decision)

decisionDecoder =
    Decode.map3 Decision
        (Decode.field "threshold" Decode.float)
        (Decode.field "feature" Decode.int)
        (Decode.field "probabilities" (Decode.array (Decode.array Decode.float)))

treeDecoder : Decode.Decoder DecisionTree
treeDecoder =
    Decode.oneOf([
        Decode.map3 (\decision left right -> Node decision left right)
            decisionDecoder
            (Decode.field "left" (Decode.lazy (\_ -> treeDecoder)))
            (Decode.field "right" (Decode.lazy (\_ -> treeDecoder))),

        Decode.map2 (\decision left -> Node decision left Empty)
            decisionDecoder
            (Decode.field "left" (Decode.lazy (\_ -> treeDecoder))),

        Decode.map2 (\decision right -> Node decision Empty right)
            decisionDecoder
            (Decode.field "right" (Decode.lazy (\_ -> treeDecoder))),

        Decode.map (\decision -> Node decision Empty Empty)
            decisionDecoder
    ])

predictHelp: Array Float -> DecisionTree -> (Array (Array Float)) -> Maybe (Array (Array Float))
predictHelp x dt best_guess =
    case dt of
        Empty ->
            Just best_guess
        Node decision left right ->
            Maybe.andThen
                (\feature -> predictHelp
                    x
                    (if feature <= decision.threshold then left else right)
                    decision.probabilities
                )
                (get decision.feature x)
predict: Array Float -> DecisionTree -> Maybe (Array (Array Float))
predict x dt = case dt of
    Empty -> Nothing
    Node decision left right -> predictHelp x dt decision.probabilities
