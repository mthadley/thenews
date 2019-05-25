module Util.List exposing (takeMap)

{-|

@docs takeMap

-}

import List.Extra


{-| Like `takeWhile` combined with `filterMap`.
-}
takeMap : (a -> Maybe b) -> List a -> List b
takeMap f =
    List.map f
        >> List.Extra.takeWhile ((/=) Nothing)
        >> List.filterMap identity
