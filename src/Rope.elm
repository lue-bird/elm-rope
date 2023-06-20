module Rope exposing
    ( Rope
    , empty, singleton, append, prepend, fromList
    , map, indexedMap, foldl, foldr, filter, filterMap, toList
    , length, reverse, member, all, any, maximum, minimum, sum, product
    , appendTo, prependTo, concat, concatMap
    , isEmpty
    )

{-|


# Types

@docs Rope


# Create

@docs empty, singleton, append, prepend, fromList


# Transform

@docs map, indexedMap, foldl, foldr, filter, filterMap, toList


# Utilities

@docs length, reverse, member, all, any, maximum, minimum, sum, product


# Combine

@docs appendTo, prependTo, concat, concatMap


# Deconstruct

@docs isEmpty

-}

import ListFilled exposing (ListFilled)


{-| A `Rope` is similar to a list, but has fast (constant time) concatenation at both ends, and fast concatenation of two `Rope`s.

It's slightly slower (O(n + operations) instead of O(n)) to iterate through, so you should convert it to a `List` if you plan to use it repeatedly.

Internally the `Rope` is a tree of lists.

-}
type Rope a
    = Empty
    | Filled (RopeFilled a)


type RopeFilled a
    = Leaf (ListFilled a)
    | Node (ListFilled (RopeFilled a))



-- CREATE


{-| An empty rope.
-}
empty : Rope a
empty =
    Empty


{-| Create a rope with only one element:

    singleton 1234 |> toList
    --> [ 1234 ]

    singleton "hi" |> toList
    --> [ "hi" ]

-}
singleton : a -> Rope a
singleton value =
    Filled (Leaf (ListFilled.singleton value))


{-| Add an element to the front of a list.

    prepend 1 (fromList [2,3]) |> toList
    --> [1,2,3]
    prepend 1 empty |> toList
    --> [1]

Complexity: O(1)

-}
prepend : a -> Rope a -> Rope a
prepend head tail =
    case tail of
        Empty ->
            singleton head

        Filled tailFilled ->
            case tailFilled of
                Leaf list ->
                    Filled (Leaf (ListFilled.prepend head list))

                Node ropes ->
                    Filled (Node (ListFilled.prepend (Leaf (ListFilled.singleton head)) ropes))


{-| Add an element to the end of a list.

    append 1 (fromList [2,3]) |> toList
    --> [2,3,1]
    append 1 empty |> toList
    --> [1]

Complexity: O(1)

-}
append : a -> Rope a -> Rope a
append last init =
    case init of
        Empty ->
            singleton last

        Filled initFilled ->
            Filled (Node ( initFilled, [ Leaf (ListFilled.singleton last) ] ))


{-| Build a rope from a list.

Complexity: O(1)

-}
fromList : List a -> Rope a
fromList list =
    case list of
        [] ->
            Empty

        head :: tail ->
            Filled (Leaf ( head, tail ))



-- TRANSFORM


filledToJust : Rope a -> Maybe (RopeFilled a)
filledToJust rope =
    case rope of
        Empty ->
            Nothing

        Filled ropeFilled ->
            Just ropeFilled


justToFilled : Maybe (RopeFilled a) -> Rope a
justToFilled maybeRopeFilled =
    case maybeRopeFilled of
        Nothing ->
            Empty

        Just ropeFilled ->
            Filled ropeFilled


{-| Apply a function to every element of a rope.

    map sqrt (fromList [1,4,9]) |> toList
    --> [1,2,3]

    map not (fromList [True,False,True]) |> toList
    --> [False,True,False]

So `map func (fromList [ a, b, c ])` is the same as `fromList [ func a, func b, func c ]`

Complexity: O(n)

-}
map : (a -> b) -> Rope a -> Rope b
map f rope =
    rope
        |> foldl (\e acc -> f e :: acc) []
        |> List.reverse
        |> fromList


{-| Same as `map` but the function is also applied to the index of each
element (starting at zero).

    indexedMap Tuple.pair (fromList [ "Tom", "Sue", "Bob" ]) |> toList
    --> [ ( 0, "Tom" ), ( 1, "Sue" ), ( 2, "Bob" ) ]

Complexity: O(n)

-}
indexedMap : (Int -> a -> b) -> Rope a -> Rope b
indexedMap f rope =
    rope
        |> foldl (\e ( i, acc ) -> ( i + 1, f i e :: acc )) ( 0, [] )
        |> Tuple.second
        |> List.reverse
        |> fromList


{-| Reduce a rope from the left.

    foldl (+) 0 (fromList [ 1, 2, 3 ])
    --> 6

    foldl (::) [] (fromList [ 1, 2, 3 ])
    --> [ 3, 2, 1 ]

So `foldl step state [1,2,3]` is like saying:

    state
        |> step 1
        |> step 2
        |> step 3

Complexity: O(n)

-}
foldl : (a -> b -> b) -> b -> Rope a -> b
foldl f initialAcc rope =
    case rope of
        Empty ->
            initialAcc

        Filled ropeFilled ->
            filledFoldl f initialAcc ropeFilled


filledFoldl : (a -> b -> b) -> b -> RopeFilled a -> b
filledFoldl f initialAcc rope =
    case rope of
        Leaf list ->
            ListFilled.foldl f initialAcc list

        Node ropes ->
            ListFilled.foldl
                (\subRope acc -> filledFoldl f acc subRope)
                initialAcc
                ropes


{-| Reduce a rope from the right.

    foldr (+) 0 (fromList [ 1, 2, 3 ])
    --> 6

    foldr (::) [] (fromList [ 1, 2, 3 ])
    --> [ 1, 2, 3 ]

So `foldr step state [1,2,3]` is like saying:

    state
        |> step 3
        |> step 2
        |> step 1

-}
foldr : (a -> b -> b) -> b -> Rope a -> b
foldr f initialAcc rope =
    case rope of
        Empty ->
            initialAcc

        Filled ropeFilled ->
            filledFoldr f initialAcc ropeFilled


filledFoldr : (a -> b -> b) -> b -> RopeFilled a -> b
filledFoldr f initialAcc rope =
    case rope of
        Leaf list ->
            ListFilled.foldr f initialAcc list

        Node ropes ->
            ListFilled.foldr
                (\childRope childAcc -> filledFoldr f childAcc childRope)
                initialAcc
                ropes


{-| Keep elements that satisfy the test.

    filter (\n -> modBy 2 n == 0) (fromList [ 1, 2, 3, 4, 5, 6 ]) |> toList
    --> [ 2, 4, 6 ]

-}
filter : (a -> Bool) -> Rope a -> Rope a
filter isGood rope =
    foldr
        (\el acc ->
            if isGood el then
                el :: acc

            else
                acc
        )
        []
        rope
        |> fromList


{-| Filter out certain values. For example, maybe you have a bunch of strings
from an untrusted source and you want to turn them into numbers:

    numbers : Rope Int
    numbers =
        filterMap String.toInt (fromList [ "3", "hi", "12", "4th", "May" ])

    numbers -> fromList [ 3, 12 ]

-}
filterMap : (a -> Maybe b) -> Rope a -> Rope b
filterMap try rope =
    foldr
        (\el acc ->
            case try el of
                Just elSuccess ->
                    elSuccess :: acc

                Nothing ->
                    acc
        )
        []
        rope
        |> fromList


{-| Convert a rope into the equivalent list.

    concat (fromList [ fromList [ 1, 2 ], fromList [ 3, 4 ] ]) |> toList
    --> [ 1, 2, 3, 4 ]

Complexity: O(n), in practice it can be O(1) if the top level is the result of `fromList`

-}
toList : Rope a -> List a
toList rope =
    case rope of
        Empty ->
            []

        Filled ropeFilled ->
            case ropeFilled of
                Leaf list ->
                    ListFilled.toList list

                Node _ ->
                    foldr (::) [] rope



-- UTILITIES


{-| Determine the length of a rope.

    length (fromList [ 1, 2, 3 ])
    --> 3

-}
length : Rope a -> Int
length rope =
    case rope of
        Empty ->
            0

        Filled ropeFilled ->
            filledLength ropeFilled


filledLength : RopeFilled a -> Int
filledLength rope =
    case rope of
        Leaf list ->
            ListFilled.length list

        Node ropes ->
            ListFilled.sumMap filledLength ropes


{-| Reverse a rope.

    reverse (fromList [ 1, 2, 3, 4 ]) |> toList
    --> [ 4, 3, 2, 1 ]

-}
reverse : Rope a -> Rope a
reverse rope =
    case rope of
        Empty ->
            Empty

        Filled ropeFilled ->
            Filled (filledReverse ropeFilled)


filledReverse : RopeFilled a -> RopeFilled a
filledReverse rope =
    case rope of
        Leaf list ->
            Leaf (ListFilled.reverse list)

        Node ropes ->
            Node (ListFilled.reverseMap filledReverse ropes)


{-| Figure out whether a rope contains a value.

    member 9 (fromList [1,2,3,4])
    --> False

    member 4 (fromList [1,2,3,4])
    --> True

-}
member : a -> Rope a -> Bool
member needle rope =
    any (\a -> a == needle) rope


{-| Determine if all elements satisfy some test.

    all (\n -> modBy 2 n == 0) (fromList [ 2, 4 ])
    --> True
    all (\n -> modBy 2 n == 0) (fromList [ 2, 3 ])
    --> False
    all (\n -> modBy 2 n == 0) (fromList [])
    --> True

-}
all : (a -> Bool) -> Rope a -> Bool
all isOkay rope =
    case rope of
        Empty ->
            True

        Filled ropeFilled ->
            filledAll isOkay ropeFilled


filledAll : (a -> Bool) -> RopeFilled a -> Bool
filledAll isOkay rope =
    case rope of
        Leaf list ->
            ListFilled.allMap isOkay list

        Node ropes ->
            ListFilled.allMap (\subRope -> filledAll isOkay subRope) ropes


{-| Determine if any elements satisfy some test.

    any (\n -> modBy 2 n == 0) (fromList [ 2, 3 ])
    --> True

    any (\n -> modBy 2 n == 0) (fromList [ 1, 3 ])
    --> False

    any (\n -> modBy 2 n == 0) (fromList [])
    --> False

-}
any : (a -> Bool) -> Rope a -> Bool
any isOkay rope =
    case rope of
        Empty ->
            False

        Filled ropeFilled ->
            filledAny isOkay ropeFilled


filledAny : (a -> Bool) -> RopeFilled a -> Bool
filledAny isOkay rope =
    case rope of
        Leaf list ->
            ListFilled.anyMap isOkay list

        Node ropes ->
            ListFilled.anyMap (\subRope -> filledAny isOkay subRope) ropes


{-| Find the maximum element in a non-empty rope.

    maximum (fromList [ 1, 4, 2 ])
    --> Just 4

    maximum (fromList [])
    --> Nothing

-}
maximum : Rope comparable -> Maybe comparable
maximum rope =
    case rope of
        Empty ->
            Nothing

        Filled ropeFilled ->
            Just (filledMaximum ropeFilled)


filledMaximum : RopeFilled comparable -> comparable
filledMaximum rope =
    case rope of
        Leaf list ->
            ListFilled.maximum list

        Node ropes ->
            ListFilled.foldlFromFirstMap filledMaximum Basics.max ropes


{-| Find the minimum element in a non-empty rope.

    minimum (fromList [ 3, 2, 1 ])
    --> Just 1

    minimum (fromList [])
    --> Nothing

-}
minimum : Rope comparable -> Maybe comparable
minimum rope =
    case rope of
        Empty ->
            Nothing

        Filled ropeFilled ->
            Just (filledMinimum ropeFilled)


filledMinimum : RopeFilled comparable -> comparable
filledMinimum rope =
    case rope of
        Leaf list ->
            ListFilled.minimum list

        Node ropes ->
            ListFilled.foldlFromFirstMap filledMinimum Basics.min ropes


{-| Get the sum of the rope elements.

    sum (fromList [ 1, 2, 3 ])
    --> 6

    sum (fromList [ 1, 1, 1 ])
    --> 3

    sum (fromList [])
    --> 0

-}
sum : Rope number -> number
sum numbers =
    case numbers of
        Empty ->
            0

        Filled ropeFilled ->
            filledSum ropeFilled


filledSum : RopeFilled number -> number
filledSum numbers =
    case numbers of
        Leaf list ->
            ListFilled.sum list

        Node ropes ->
            ListFilled.sumMap filledSum ropes


{-| Get the product of the rope elements.

    product (fromList [ 2, 2, 2 ])
    --> 8

    product (fromList [ 3, 3, 3 ])
    --> 27

    product (fromList [])
    --> 1

-}
product : Rope number -> number
product numbers =
    case numbers of
        Empty ->
            1

        Filled ropeFilled ->
            filledProduct ropeFilled


filledProduct : RopeFilled number -> number
filledProduct numbers =
    case numbers of
        Leaf list ->
            ListFilled.product list

        Node ropes ->
            ListFilled.productMap filledProduct ropes



-- COMBINE


{-| Put two ropes together, the second after the first.

    appendTo (fromList [ 1, 1, 2 ]) (fromList [ 3, 5, 8 ]) |> toList
    --> [ 1, 1, 2, 3, 5, 8 ]

    appendTo (fromList [ 'a', 'b' ]) (fromList [ 'c' ]) |> toList
    --> [ 'a', 'b', 'c' ]

Complexity: O(1)

-}
appendTo : Rope a -> Rope a -> Rope a
appendTo early late =
    case early of
        Empty ->
            late

        Filled earlyFilled ->
            case late of
                Empty ->
                    early

                Filled lateFilled ->
                    case lateFilled of
                        Node ropes ->
                            Filled (Node (ListFilled.prepend earlyFilled ropes))

                        Leaf _ ->
                            Filled (Node ( earlyFilled, [ lateFilled ] ))


{-| Put two ropes together, the first after the second.

    prependTo (fromList [ 1, 1, 2 ]) (fromList [ 3, 5, 8 ]) |> toList
    --> [ 3, 5, 8, 1, 1, 2 ]

    prependTo (fromList [ 'a', 'b' ]) (fromList [ 'c' ]) |> toList
    --> [ 'c', 'a', 'b' ]

Complexity: O(1)

-}
prependTo : Rope a -> Rope a -> Rope a
prependTo late early =
    appendTo early late


{-| Concatenate a bunch of ropes into a single rope:

    concat (fromList [ fromList [ 1, 2 ], fromList [ 3 ], fromList [ 4, 5 ] ]) |> toList
    --> [ 1, 2, 3, 4, 5 ]

Complexity: O(n), in practice it can be O(1) if the top level is the result of `fromList`

-}
concat : Rope (Rope a) -> Rope a
concat ropes =
    case ropes of
        Empty ->
            empty

        Filled ropeFilled ->
            justToFilled (filledConcat ropeFilled)


filledConcat : RopeFilled (Rope a) -> Maybe (RopeFilled a)
filledConcat ropes =
    case ropes of
        Leaf list ->
            case ListFilled.filterMap filledToJust list of
                [] ->
                    Nothing

                head :: tail ->
                    Just (Node ( head, tail ))

        Node children ->
            case ListFilled.filterMap filledConcat children of
                [] ->
                    Nothing

                head :: tail ->
                    Just (Node ( head, tail ))


{-| Map a given function onto a list and flatten the resulting ropes.

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> Rope b) -> Rope a -> Rope b
concatMap f rope =
    case rope of
        Empty ->
            empty

        Filled ropeFilled ->
            justToFilled (filledConcatMap f ropeFilled)


filledConcatMap : (a -> Rope b) -> RopeFilled a -> Maybe (RopeFilled b)
filledConcatMap f rope =
    case rope of
        Leaf list ->
            case ListFilled.filterMap (\el -> filledToJust (f el)) list of
                [] ->
                    Nothing

                head :: tail ->
                    Just (Node ( head, tail ))

        Node ropes ->
            let
                ropesConcatenated : List (RopeFilled b)
                ropesConcatenated =
                    ListFilled.foldr
                        (\subRope acc ->
                            filledFoldr
                                (\el subAcc ->
                                    case f el of
                                        Empty ->
                                            subAcc

                                        Filled elFilled ->
                                            elFilled :: subAcc
                                )
                                acc
                                subRope
                        )
                        []
                        ropes
            in
            case ropesConcatenated of
                [] ->
                    Nothing

                head :: tail ->
                    Just (Node ( head, tail ))



-- DECONSTRUCT


{-| Determine if a rope is empty.

    isEmpty (fromList [])
    --> True

    isEmpty (fromList [] |> appendTo (fromList []))
    --> True

-}
isEmpty : Rope a -> Bool
isEmpty rope =
    case rope of
        Empty ->
            True

        Filled _ ->
            False
