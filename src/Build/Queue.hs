module Build.Queue where

import qualified Elm.Compiler.Module as Module


newtype Queue =
    Queue ([Module.Name], [Module.Name])


empty : Queue
empty =
    Queue ([],[])


enqueue : [Module.Name] -> Queue -> Queue
enqueue names (Queue (front, back)) =
    Queue (front, names ++ back)


dequeue : Int -> Queue -> ([Module.Name], Queue)
dequeue n (Queue (front, back)) =
    case splitAt n front of
        (names, []) ->
            let (names', front') = splitAt (length names) (reverse back)
            in
                (names ++ names', Queue (front', []))

        (names, front') ->
            (names, Queue (front', back))