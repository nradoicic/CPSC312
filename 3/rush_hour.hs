{-| Nikola Radoicic  - 30478093
    I consulted with my friend Jaxsun (70779095) mostly when we both had
    a working solution and we were dealing with corner cases and wondering
    why some cases ran slowly. |-}

import Text.Regex.PCRE -- Needed perl style regex instead of default for grouped regex
import Data.List       -- Needed transpose

{-| Some test maps
a = ["--B---","--B---","XXB---","---AA-","------","------"] -- Given example
b = ["--AA--","--BB--","--CC--","-----G","-----G","------"] -- Shifty without a target
c = ["------","------","---XX-","------","------","------"] -- Most trivial example
d = ["------","------","XX----","------","------","------"] -- Trivial example
e = ["GG-YYY","---OLL","-XXO-P","--BDDP","EEB--P","--BTTT"] -- Hard Example (potentia impossible)
f = ["------","-----A","XX---A","-----A","------","------"] -- 2 Moves
g = ["--AA--","--BB--","------","------","------","------"] -- Shifty 2 pieces parallel
h = ["B-----","B--AA-","------","------","------","------"] -- Shifty 2 pieces perpendicular
i = ["GGOY--","P-OY--","PXXY--","PBBB--","------","---TTT"] -- Example we found online
j = ["Q--WWW","QEEERT","XXYURT","IIYURT","---PAA","-SSP--"] -- Jaxsun

board = i

-- Usefull print
main = do
    -- mapM_ (mapM_ print) $ newStates board []
    mapM_ print board
    print ""
    mapM_ (mapM_ print) $ (map addRow (reverse (statesearch [board] [])))
    print "Done!"

addRow x = x ++ [""]
|-}

-- This is the output we want
rush_hour board = reverse (statesearch [board] [])

-- Prase movable cars and their index from a row
-- input:  a row of the board
-- output: a list of tuples - one for each car in the row which can move right
--         (index where the car starts in the string, the pattern of the car and space)
car_info row = zip (map fst (car_indexes row))
                   (map access (car_strings row)) where access x = (!!) x 0
-- Car helpers
movable_car_pattern = "([A-Z])\\1\\1-|([A-Z])\\2-"
has_movable_cars row = (row =~ movable_car_pattern :: Bool) || ((reverse row) =~ movable_car_pattern :: Bool)
car_indexes row = getAllMatches $ (row =~ movable_car_pattern :: AllMatches [] (MatchOffset, MatchLength))
car_strings row = row =~ movable_car_pattern :: [[String]]


-- Generate the new states for a single row
--      Push everything right then flip the row and do the same.
-- input:  a row of the board
-- output: a list of all possible single-move slides for that row
newRowStates row = newRowStates_left row ++ newRowStates_right row
newRowStates_left row = map reverse (newRowStates_right (reverse row))
newRowStates_right row = newRowStates_helper row (car_info row)
newRowStates_helper row [] = []
newRowStates_helper row (x:xs) = (replaceSegment row (fst x) (reverse (snd x))) : newRowStates_helper row xs


-- Generate new boards for each row
--      Similar to above, shuffle everything left / right.
--      Then transpose, do the same, and transpose (effectively pushing
--      everything up / downe)
-- input:  a configuration of the board
-- output: all possible boards that involve moving a single car a single space from the input
newStates board = (newStatesLR board ++ newStatesUD board)
newStatesUD board = map transpose (newStatesLR (transpose board))
newStatesLR board = newStatesLR_helper [] board
newStatesLR_helper above (x:xs)
    | null xs = if has_movable_cars x then newStatesFromRows above (newRowStates x) [] else []
    | otherwise = if has_movable_cars x
        then (newStatesFromRows above (newRowStates x) xs) ++ (newStatesLR_helper (above ++ [x]) xs)
        else (newStatesLR_helper (above ++ [x]) xs)


-- Generate whole board states from a list of rows for a given level
-- and the rows above / bellow
-- input:  a list of rows of the board, possible new rows, and lower rows
--         [first n-1 row of the board] [possible future rows for row n] [remaining rows]
-- output: all the boards that could be made if a car in row n moved
newStatesFromRows above (x:xs) bellow
    | null xs = [above ++ [x] ++ bellow]
    | otherwise = (above ++ [x] ++ bellow) : (newStatesFromRows above xs bellow)


-- Checks if the goal condition is being met
goal_check board = (exit_row !! 0) == 'X' && (exit_row !! 1) == 'X'
    where exit_row = reverse (board !! 2)


-- Stuff we were given from the peg puzzle (with some modifications to statesearch)
replaceSegment oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise =
        (head oldList):
        (replaceSegment (tail oldList) (pos - 1) segment)


statesearch unexplored path
   | null unexplored              = []
   | goal_check (head unexplored) = (head unexplored):path
   | (not (null result))          = result
   | otherwise                    =
        statesearch (tail (unexplored)) path
     where result = statesearch
                       ((newStates (head unexplored)) \\ path)
                       ((head (unexplored)):path)
