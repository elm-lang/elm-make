module Utils.Queue where


newtype Queue a =
    Queue ([a], [a])


empty :: Queue a
empty =
    Queue ([],[])


fromList :: [a] -> Queue a
fromList list =
    Queue (list, [])


size :: Queue a -> Int
size (Queue (front, back)) =
    length front + length back


enqueue :: [a] -> Queue a -> Queue a
enqueue names (Queue (front, back)) =
    Queue (front, names ++ back)


dequeue :: Int -> Queue a -> ([a], Queue a)
dequeue n queue =
    dequeueHelp [] n queue


dequeueHelp :: [a] -> Int -> Queue a -> ([a], Queue a)
dequeueHelp results n queue@(Queue (front, back)) =
  case n of
    0 -> (results, queue)
    _ ->
      case (front, back) of
        ([],  []) -> (results, queue)
        ([],   _) -> dequeueHelp results n (Queue (reverse back, []))
        (x:xs, _) -> dequeueHelp (x:results) (n-1) (Queue (xs, back))