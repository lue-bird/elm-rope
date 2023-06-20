module ListFilled exposing
    ( ListFilled
    , singleton, prepend
    , length, anyMap, allMap, sum, sumMap, product, productMap, maximum, minimum
    , reverse, reverseMap
    , map, filterMap
    , toList, foldr, foldl, foldlFromFirstMap, foldrFromOne
    )

{-| Non-empty list, represented as a tuple of the first element and a tail-`List`

@docs ListFilled


# Create

@docs singleton, prepend


# Observe

@docs length, anyMap, allMap, sum, sumMap, product, productMap, maximum, minimum


# Alter

@docs reverse, reverseMap
@docs map, filterMap


# Transform

@docs toList, foldr, foldl, foldlFromFirstMap, foldrFromOne

-}


type alias ListFilled a =
    ( a, List a )


singleton : a -> ListFilled a
singleton value =
    ( value, [] )


prepend : a -> (ListFilled a -> ListFilled a)
prepend newHead =
    \( head, tail ) -> ( newHead, head :: tail )


length : ListFilled a -> Int
length =
    \( _, tail ) -> 1 + List.length tail


toList : ListFilled a -> List a
toList =
    \( head, tail ) -> head :: tail


map : (a -> b) -> (ListFilled a -> ListFilled b)
map f =
    \( head, tail ) ->
        ( f head, List.map f tail )


filterMap : (a -> Maybe b) -> (ListFilled a -> List b)
filterMap try listFilled =
    List.filterMap try (toList listFilled)


reverse : ListFilled a -> ListFilled a
reverse listFilled =
    foldlFromOne singleton prepend listFilled


reverseMap : (a -> b) -> ListFilled a -> ListFilled b
reverseMap f listFilled =
    foldlFromOne (\first -> singleton (f first))
        (\el soFar -> prepend (f el) soFar)
        listFilled


sum : ListFilled number -> number
sum listFilled =
    foldlFromFirst (+) listFilled


sumMap : (a -> number) -> (ListFilled a -> number)
sumMap toNumber listFilled =
    foldlFromFirstMap toNumber (+) listFilled


product : ListFilled number -> number
product listFilled =
    foldlFromFirst (*) listFilled


productMap : (a -> number) -> (ListFilled a -> number)
productMap toNumber listFilled =
    foldlFromFirstMap toNumber (*) listFilled


allMap : (a -> Bool) -> (ListFilled a -> Bool)
allMap toNumber listFilled =
    foldlFromFirstMap toNumber (&&) listFilled


anyMap : (a -> Bool) -> (ListFilled a -> Bool)
anyMap toNumber listFilled =
    foldlFromFirstMap toNumber (||) listFilled


maximum : ListFilled comparable -> comparable
maximum listFilled =
    foldlFromFirst Basics.max listFilled


minimum : ListFilled comparable -> comparable
minimum listFilled =
    foldlFromFirst Basics.min listFilled


foldl : (a -> (b -> b)) -> b -> (ListFilled a -> b)
foldl reduce firstToInitialAccumulator =
    \( el0, el1Up ) ->
        List.foldl reduce (firstToInitialAccumulator |> reduce el0) el1Up


foldlFromOne : (a -> b) -> (a -> (b -> b)) -> (ListFilled a -> b)
foldlFromOne firstToInitialAccumulator reduce =
    \( el0, el1Up ) ->
        List.foldl reduce (firstToInitialAccumulator el0) el1Up


foldr : (a -> (b -> b)) -> b -> (ListFilled a -> b)
foldr reduce initialAccumulator listFilled =
    foldrFromOne (\el -> initialAccumulator |> reduce el) reduce listFilled


foldrFromOne : (a -> b) -> (a -> (b -> b)) -> (ListFilled a -> b)
foldrFromOne firstToInitialAccumulator reduce listFilled =
    -- similar to https://github.com/elm/core/blob/master/src/List.elm#L174
    foldrHelper firstToInitialAccumulator reduce 0 listFilled


foldrHelper : (a -> b) -> (a -> b -> b) -> Int -> ListFilled a -> b
foldrHelper firstToInitialAccumulator reduce ctr =
    \( a, r1 ) ->
        case r1 of
            [] ->
                firstToInitialAccumulator a

            b :: r2 ->
                case r2 of
                    [] ->
                        reduce a (firstToInitialAccumulator b)

                    c :: r3 ->
                        case r3 of
                            [] ->
                                reduce a (reduce b (firstToInitialAccumulator c))

                            d :: r4 ->
                                case r4 of
                                    [] ->
                                        reduce a (reduce b (reduce c (firstToInitialAccumulator d)))

                                    e :: r5 ->
                                        let
                                            res : b
                                            res =
                                                if ctr > 500 then
                                                    foldlFromOne firstToInitialAccumulator reduce (reverse ( e, r5 ))

                                                else
                                                    foldrHelper firstToInitialAccumulator reduce (ctr + 1) ( e, r5 )
                                        in
                                        reduce a (reduce b (reduce c (reduce d res)))


foldlFromFirstMap : (a -> b) -> (b -> (b -> b)) -> (ListFilled a -> b)
foldlFromFirstMap change reduce listFilled =
    foldlFromOne
        change
        (\el soFar ->
            soFar |> reduce (change el)
        )
        listFilled


foldlFromFirst : (a -> (a -> a)) -> (ListFilled a -> a)
foldlFromFirst reduce =
    \( el0, el1Up ) ->
        List.foldl reduce el0 el1Up
