module History where

type History a = { past : [a], future : [a] }

infinite = { past = [], future = [] }
undo : History a -> Maybe (History a)
undo h =
  case h.past of
    [] -> Nothing
    x::xs -> Just { h | past <- xs, future <- x :: h.future }

redo : History a -> Maybe (History a)
redo h =
  case h.future of
    [] -> Nothing
    x::xs -> Just { h | past <- x :: h.past, future <- xs }

add  : a -> History a -> History a
add x h =
  { h | past <- x :: h.past, future <- [] }

value : History a -> Maybe a
value h =
  case h.past of
    x::_ -> Just x
    [] -> Nothing
