module RopeFilled exposing
    ( RopeFilled(..)
    , singleton, fromListFilled
    , append, prepend, appendTo
    , length, allMap, anyMap, sum, product, minimum, maximum
    , reverse
    , toList, foldl, foldr
    )

{-| Non-empty rope

@docs RopeFilled


# Create

@docs singleton, fromListFilled
@docs append, prepend, appendTo


# Observe

@docs length, allMap, anyMap, sum, product, minimum, maximum


# Alter

@docs reverse


# Transform

@docs toList, foldl, foldr

-}

import ListFilled exposing (ListFilled)


type RopeFilled a
    = Leaf (ListFilled a)
    | Node (ListFilled (RopeFilled a))


singleton : a -> RopeFilled a
singleton value =
    Leaf (ListFilled.singleton value)


prepend : a -> RopeFilled a -> RopeFilled a
prepend head tail =
    case tail of
        Leaf list ->
            Leaf (ListFilled.prepend head list)

        Node ropes ->
            Node (ListFilled.prepend (Leaf (ListFilled.singleton head)) ropes)


append : a -> RopeFilled a -> RopeFilled a
append last init =
    Node ( init, [ Leaf (ListFilled.singleton last) ] )


appendTo : RopeFilled a -> RopeFilled a -> RopeFilled a
appendTo early late =
    case late of
        Node ropes ->
            Node (ListFilled.prepend early ropes)

        Leaf _ ->
            Node ( early, [ late ] )


fromListFilled : ListFilled a -> RopeFilled a
fromListFilled listFilled =
    Leaf listFilled


foldl : (a -> b -> b) -> b -> RopeFilled a -> b
foldl f initialAcc rope =
    case rope of
        Leaf list ->
            ListFilled.foldl f initialAcc list

        Node ropes ->
            ListFilled.foldl
                (\subRope acc -> foldl f acc subRope)
                initialAcc
                ropes


foldr : (a -> b -> b) -> b -> RopeFilled a -> b
foldr f initialAcc rope =
    case rope of
        Leaf list ->
            ListFilled.foldr f initialAcc list

        Node ropes ->
            ListFilled.foldr
                (\childRope childAcc -> foldr f childAcc childRope)
                initialAcc
                ropes


toList : RopeFilled a -> List a
toList rope =
    case rope of
        Leaf list ->
            ListFilled.toList list

        Node _ ->
            foldr (::) [] rope


length : RopeFilled a -> Int
length rope =
    case rope of
        Leaf list ->
            ListFilled.length list

        Node ropes ->
            ListFilled.sumMap length ropes


reverse : RopeFilled a -> RopeFilled a
reverse rope =
    case rope of
        Leaf list ->
            Leaf (ListFilled.reverse list)

        Node ropes ->
            Node (ListFilled.reverseMap reverse ropes)


allMap : (a -> Bool) -> RopeFilled a -> Bool
allMap isOkay rope =
    case rope of
        Leaf list ->
            ListFilled.allMap isOkay list

        Node ropes ->
            ListFilled.allMap (\subRope -> allMap isOkay subRope) ropes


anyMap : (a -> Bool) -> RopeFilled a -> Bool
anyMap isOkay rope =
    case rope of
        Leaf list ->
            ListFilled.anyMap isOkay list

        Node ropes ->
            ListFilled.anyMap (\subRope -> anyMap isOkay subRope) ropes


maximum : RopeFilled comparable -> comparable
maximum rope =
    case rope of
        Leaf list ->
            ListFilled.maximum list

        Node ropes ->
            ListFilled.foldlFromFirstMap maximum Basics.max ropes


minimum : RopeFilled comparable -> comparable
minimum rope =
    case rope of
        Leaf list ->
            ListFilled.minimum list

        Node ropes ->
            ListFilled.foldlFromFirstMap minimum Basics.min ropes


sum : RopeFilled number -> number
sum numbers =
    case numbers of
        Leaf list ->
            ListFilled.sum list

        Node ropes ->
            ListFilled.sumMap sum ropes


product : RopeFilled number -> number
product numbers =
    case numbers of
        Leaf list ->
            ListFilled.product list

        Node ropes ->
            ListFilled.productMap product ropes
