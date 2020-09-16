{-
inits [1,2,3] = inits 1:[2,3]
= [] : map (1:) (inits [2,3])
= [] : map (1:) ([] : map (2:) (inits [3]))
= [] : map (1:) ([] : map (2:) ([] : map (3:) inits []))
= [] : map (1:) ([] : map (2:) ([] : map (3:) [[]]))
= [] : map (1:) ([] : map (2:) ([] : [(3:) []]))
= [] : map (1:) ([] : map (2:) ([] : [(3:) []]))
= [] : map (1:) ([] : map (2:) ([] : [[3]]))
= [] : map (1:) ([] : map (2:) [[],[3]])
= [] : map (1:) ([] : [[2],[2,3]])
= [] : map (1:) ([[],[2],[2,3]])
= [] : ([[1],[1,2],[1,2,3]])
= ([[],[1],[1,2],[1,2,3]])
uiuiui.

scanl f e = map (foldl f e) . inits
^ this is a bit stupid, since it throws away the intermediate value and 
starts the calculation from scratch for every sublist.
the obviously better thing apparently can be derived with slightly magical looking equational reasoning o_O

the claim that is used in that reasoning says basically:
"adding another element on the left of the array is the same as putting that into the initial element directly"
which is correct, since foldl starts calculating on the left
-}
