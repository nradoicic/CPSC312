import Text.Regex.Posix
import Data.List

type Row = String
type Board = [Row]
type Rating = (Board, Int)

main = mapM_ print $ out

a = parse_current_board 3 "WWW-WW-------BB-BBB"
b = parse_current_board 3 "WWW-WW--B---B---BBB"
out = generate 'W' [["WWW","-WW-","-----","-BB-","BBB"]] ["WWW","-WW-","-----","-BB-","BBB"] -- First Board
-- out = generate "W" ["---","----","--W--","----","---"] -- One piece in middle
-- out = generate "B" ["WW-","-W--","-----","B-B-","--B"] -- 12 moves for B

--Entry point
crusher_z9k7:: [String] -> Char -> Int -> Int -> [String]
crusher_z9k7 board_strings player max_depth board_size
    = map dump_board result
    where 
        boards = map (parse_current_board board_size) board_strings
        result = pick_board boards player max_depth

-- Board parser
-- Input  : A string to represent the board, the size of one side of the board
-- Output : A list of strings, each string representing a different row of the board
--      "WWW-WW-------BB-BBB" -> ["WWW","-WW-","-----","-BB-","BBB"]
parse_current_board board_size board_string  = parse_current_board_helper board_string board_size 1 []

parse_current_board_helper board_string board_size current_row result
    | null board_string = reverse result
    | otherwise = parse_current_board_helper (drop row board_string) board_size (current_row + 1) ((take row board_string):result)
    where
        row = row_size board_size current_row
		
-- Board Serializer
-- Input  : A list of strings representing the rows of a board
-- Output : A String which represents the input board
dump_board:: Board -> String
dump_board board = foldl (++) "" board

-- Board Rotator
--     Takes successive slices of the board and cons them into a result list
-- Input  : A board (list of strings)
-- Output : The same board viewed from a different side (rotated once)
rotate_board board = reverse (rotate_board_helper board (size_of board) 1 [])
rotate_board_helper board board_size current_row result
    | null (non_null board) = result
    | otherwise = rotate_board_helper(snd sliced_board) board_size (current_row + 1) ((fst sliced_board):result)
    where
        row = row_size board_size current_row
        sliced_board = slice_board board row

-- Rotates the board n number of time clockwise
rotate_board_n n board
    | n == 0 = board
    | otherwise = rotate_board_n (n-1) (rotate_board board)

{-| fst    lst
    */  *  *        Returns a tuple with the first value
   */  *  *  *   <- as the "slice" taken from the left (string of values)
  */  *  *  *  *    and the second value as the remainder of the board
----------------
   *  *  *  *    <- The rest of the board which is ignored
     *  *  *
|-}
slice_board board size = slice_board_helper (reverse (take size (non_null board))) (drop size (non_null board)) [] []
slice_board_helper (x:xs) rest_of_board row remainder
    | null xs = ((reverse ((head x):row)),(((tail x):remainder) ++ rest_of_board))
    | otherwise = slice_board_helper xs rest_of_board ((head x):row) ((tail x): remainder)

-- MINIMAX
-- Main Entry point, computes the next move to make and returns a history list with the next move at the head.
-- If no moves are availble the next board will just be the current board.
--
--  Arguments: 
--      (board:history): The history of boards with the current board state at the head
--      player:          A character representation for which piece is moving. May only be 'B' or 'W'
--      max:             The maximum number of moves for the algorithm to look ahead.
pick_board:: [Board] -> Char -> Int -> [Board]
pick_board (board:history) player max 
    | null opening_moves = board:(board:history)
    | otherwise = (fst (best_of player max 1 (board:history) opening_moves)):(board:history)
        where opening_moves = (generate player (board:history) board)


-- Function which performs actual minimax search
--
--  Arguments: 
--      (p:h):   The history of boards with the current board state at the head
--      player:  A character representation for which piece is moving. May only be 'B' or 'W'
--      max:     The maximum number of moves for the algorithm to look ahead.
--      depth:   The current depth of search
--      options: The boards available to move to at the current search depth
best_of:: Char -> Int -> Int -> [Board] -> [Board] -> Rating
-- base case where no moves were available. This means that the last board state was a winning move.
-- so rate the last board state as it is a leaf of the search tree.
best_of player max depth (p:h) [] = rate_board player (p:h) p
-- when we are at a leaf of the search (depth == max) rate each board and compare the results
-- otherwise compute the possible boards attainable from the current depth and recurse
best_of player max depth (p:h) options
    | depth == max  = comp (rate_all options) -- leaves
    | otherwise     = comp (map (next_gen_best) options) 
    where
        -- alternate finding the minimum and maximum of our options
        comp = [find_min,find_max]!!(mod depth 2)
        -- helpers to rate everything
        rate_all            = map (rate_all_h)
        rate_all_h option   = rate_board player (option:(p:h)) option
        -- helper to compute the next generation of boards
        next_gen option = generate (other player) (option:(p:h)) option
        -- recursion helper
        next_gen_best option = swap_out option (best_of (other player) max (depth+1) (option:(p:h)) (next_gen option))
        -- we are looking for the best next move to make, so propagate the score, but not the board
        -- we invert the score each time, this is because each board rating will rate boards positively
        -- based on which piece is moving, so we have to alternate the signs to keep things in line.
        swap_out option rating = (option, -1*(snd rating))

-- Functions to find the min or max scored board by rating
find_min:: [Rating] -> Rating
find_min (r:rs) = find_helper r rs (<)

find_max:: [Rating] -> Rating
find_max (r:rs) = find_helper r rs (>)

find_helper:: Rating -> [Rating] -> (Int -> Int -> Bool) -> Rating
find_helper best [] _ = best
find_helper best (r:rs) op 
    | op (snd best) (snd r) = find_helper best rs op
    | otherwise             = find_helper r rs op
        
-- Heuristics --
-- Basic Heuristic 
-- it really likes to win
-- it really doesn't like to lose
-- it likes having pieces
rate_board:: Char -> [Board] -> Board -> Rating
rate_board player history board = (board, heuristic player history board)

heuristic:: Char -> [Board] -> Board -> Int
heuristic player history board
    | is_win_for  player history board  = 99
    | is_loss_for player history board  = -99
    | otherwise                 = (count player board) - (count (other player) board)
    
is_win_for::  Char -> [Board] -> Board -> Bool
is_win_for  player history board = is_loss_for (other player) history board
is_loss_for:: Char -> [Board] -> Board -> Bool
is_loss_for player history board = ((count player board) < size_of board) --doesn't check out of moves yet

count:: Char -> Board -> Int
count c board = length (filter (== c) (dump_board board))

-- helper to switch players
other:: Char -> Char
other player 
    | player == 'B' = 'W'
    | player == 'W' = 'B'

-- Remove all null values from a list
non_null [] = []
non_null (x:xs)
    | null x = non_null xs
    | otherwise = x : non_null xs

    
-- Board generation
generate player history board = (foldl (++) [] (generate_helper 0 player board)) \\ history -- different added ~x3 time hit
generate_helper n player board
    | n == 6 = []
    | otherwise = (map (rotate_board_n (6-n)) (new_boards player (rotate_board_n n board))) : generate_helper (n+1) player board

new_boards player board = new_board_for_row 0 player board

new_board_for_row num player board
    | num == longest (size_of board) = []
    | otherwise = (new_board_helper player (take num board) (new_moves player (board !! num)) (drop (num + 1) board))
        ++ new_board_for_row (num + 1) player board

new_board_helper _ _ [] _ = []
new_board_helper player top (x:xs) bottom = (top ++ (x:bottom)) : new_board_helper player top xs bottom

new_moves player row = (new_slides player row) ++ (new_jumps player row)
new_jumps player row = new_jumps_right player row
new_jumps_right player row = jump_right player row (match_indexes player row jump_match_pattern)

new_slides player row = new_slides_right player row
new_slides_right player row = slide_right player row (match_indexes player row slide_match_pattern)

slide_match_pattern   colour = colour:"-"
slide_replace_pattern colour = '-':[colour]
jump_match_pattern    colour = colour:colour:'[':'^':colour:"]"
jump_replace_pattern  colour = '-':colour:[colour]

match_indexes player row pattern = getAllMatches $ (row =~ (pattern player) :: AllMatches [] (MatchOffset, MatchLength))
slide_right _ row [] = []
slide_right player row (x:xs) = (replaceSegment row (fst x) (slide_replace_pattern player)) : slide_right player row xs
jump_right _ row [] = []
jump_right player row (x:xs) = (replaceSegment row (fst x) (jump_replace_pattern player)) : jump_right player row xs


-- Replace Segment we were given from pegpuzzle.hs
replaceSegment oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise =
        (head oldList):
        (replaceSegment (tail oldList) (pos - 1) segment)


-- Board Math
longest :: Int -> Int
longest size = (size * 2) - 1

row_size board_size current_row = ((2 * board_size) - abs(board_size - current_row) - 1)

size_of board = length (head board)

tiles_in board = (2*n - 1) + (n - 1) * (3*n - 2)
    where 
        n = size_of board
