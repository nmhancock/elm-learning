module DecisionTree exposing (Tree(..), treeDecoder, predict)

import Array exposing (Array, get)
import Json.Decode as Dece

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
    Dece.map3 Decision
        (Dece.field "threshold" Dece.float)
        (Dece.field "feature" Dece.int)
        (Dece.field "probabilities" (Dece.array (Dece.array Dece.float)))

treeDecoder : Dece.Decoder DecisionTree
treeDecoder =
    Dece.oneOf([
        Dece.map3 (\decision left right -> Node decision left right)
            decisionDecoder
            (Dece.field "left" (Dece.lazy (\_ -> treeDecoder)))
            (Dece.field "right" (Dece.lazy (\_ -> treeDecoder))),

        Dece.map2 (\decision left -> Node decision left Empty)
            decisionDecoder
            (Dece.field "left" (Dece.lazy (\_ -> treeDecoder))),

        Dece.map2 (\decision right -> Node decision Empty right)
            decisionDecoder
            (Dece.field "right" (Dece.lazy (\_ -> treeDecoder))),

        Dece.map (\decision -> Node decision Empty Empty)
            decisionDecoder
    ])

type alias Float2D = Array (Array Float)
predictHelp: Array Float -> DecisionTree -> Float2D -> Maybe Float2D
predictHelp x dt best_guess =
    case dt of
        Empty -> Just best_guess
        Node decision left right ->
            Maybe.andThen
                (\feature -> predictHelp
                    x
                    (if feature <= decision.threshold then left else right)
                    decision.probabilities
                )
                (get decision.feature x)
predict: Array Float -> DecisionTree -> Maybe Float2D
predict x dt = case dt of
    Empty -> Nothing
    Node decision left right -> predictHelp x dt decision.probabilities
