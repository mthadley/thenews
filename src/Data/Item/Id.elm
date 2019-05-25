module Data.Item.Id exposing (Id, Ident)

import Tagged exposing (Tagged)


type Ident
    = Ident


type alias Id =
    Tagged Ident Int
