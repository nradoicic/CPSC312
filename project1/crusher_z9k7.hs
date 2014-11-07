import Text.Regex.Posix
import Data.List

type Row = String
type Board = [Row]
type Rating = (Board, Int)

main = mapM_ print $ out

a = parse_current_board_z9k7 3 "WWW-WW-------BB-BBB"
out = generate_z9k7 'W' [["WWW","-WW-","-----","-BB-","BBB"]] ["WWW","-WW-","-----","-BB-","BBB"] -- First Board
-- out = generate_z9k7 "W" ["---","----","--W--","----","---"] -- One piece in middle
-- out = generate_z9k7 "B" ["WW-","-W--","-----","B-B-","--B"] -- 12 moves for B

--Entry point
crusher_z9k7:: [String] -> Char -> Int -> Int -> [String]
crusher_z9k7 board_strings player max_depth board_size
    = map dump_board_z9k7 result
    where 
        boards = map (parse_current_board_z9k7 board_size) board_strings
        result = pick_board_z9k7 boards player max_depth

-- Board parser
-- Input  : A string to represent the board, the size of one side of the board
-- Output : A list of strings, each string representing a different row of the board
--      3, "WWW-WW-------BB-BBB" -> ["WWW","-WW-","-----","-BB-","BBB"]
parse_current_board_z9k7 board_size board_string = parse_current_board_helper_z9k7 board_string board_size 1 []

parse_current_board_helper_z9k7 board_string board_size current_row result
    | null board_string = reverse result
    | otherwise = parse_current_board_helper_z9k7 (drop row board_string) board_size (current_row + 1) ((take row board_string):result)
    where
        row = row_size_z9k7 board_size current_row


-- Board Serializer
-- Input  : A list of strings representing the rows of a board
-- Output : A String which represents the input board
dump_board_z9k7:: Board -> String
dump_board_z9k7 board = foldl (++) "" board


-- Board Rotator
--     Takes successive slices of the board and cons them into a result list
-- Input  : A board (list of strings)
-- Output : The same board viewed from a different side (rotated once)
rotate_board_z9k7 board = reverse (rotate_board_helper_z9k7 board (size_of_z9k7 board) 1 [])
rotate_board_helper_z9k7 board board_size current_row result
    | null (non_null_z9k7 board) = result
    | otherwise = rotate_board_helper_z9k7(snd sliced_board) board_size (current_row + 1) ((fst sliced_board):result)
    where
        row = row_size_z9k7 board_size current_row
        sliced_board = slice_board_z9k7 board row

-- Rotates the board n number of time clockwise
-- Input  : A board (list of strings), a number of times to rotate the board
-- Output : The board rotated the provided number of times
rotate_board_z9k7_n n board
    | n == 0 = board
    | otherwise = rotate_board_z9k7_n (n-1) (rotate_board_z9k7 board)

{-| fst    lst
    */  *  *        Returns a tuple with the first value
   */  *  *  *   <- as the "slice" taken from the left (string of values)
  */  *  *  *  *    and the second value as the remainder of the board
----------------
   *  *  *  *    <- The rest of the board which is ignored
     *  *  *        |-}
slice_board_z9k7 board size = slice_board_helper_z9k7 (reverse (take size (non_null_z9k7 board))) (drop size (non_null_z9k7 board)) [] []
slice_board_helper_z9k7 (x:xs) rest_of_board row remainder
    | null xs = ((reverse ((head x):row)),(((tail x):remainder) ++ rest_of_board))
    | otherwise = slice_board_helper_z9k7 xs rest_of_board ((head x):row) ((tail x): remainder)

-- MINIMAX
-- Main Entry point, computes the next move to make and returns a history list with the next move at the head.
-- If no moves are availble the next board will just be the current board.
--
--  Arguments: 
--      (board:history): The history of boards with the current board state at the head
--      player:          A character representation for which piece is moving. May only be 'B' or 'W'
--      max:             The maximum number of moves for the algorithm to look ahead.
pick_board_z9k7:: [Board] -> Char -> Int -> [Board]
pick_board_z9k7 (board:history) player max 
    | null opening_moves = board:(board:history)
    | otherwise = (fst (best_of_z9k7 player max 1 (board:history) opening_moves)):(board:history)
        where opening_moves = (generate_z9k7 player (board:history) board)


-- Function which performs actual minimax search
--
--  Arguments: 
--      (p:h):   The history of boards with the current board state at the head
--      player:  A character representation for which piece is moving. May only be 'B' or 'W'
--      max:     The maximum number of moves for the algorithm to look ahead.
--      depth:   The current depth of search
--      options: The boards available to move to at the current search depth
best_of_z9k7:: Char -> Int -> Int -> [Board] -> [Board] -> Rating
-- base case where no moves were available. This means that the last board state was a winning move.
-- so rate the last board state as it is a leaf of the search tree.
best_of_z9k7 player max depth (p:h) [] = rate_board_z9k7 player (p:h) p
-- when we are at a leaf of the search (depth == max) rate each board and compare the results
-- otherwise compute the possible boards attainable from the current depth and recurse
best_of_z9k7 player max depth (p:h) options
    | depth == max  = comp (rate_all options) -- leaves
    | otherwise     = comp (map (next_gen_best) options) 
    where
        -- alternate finding the minimum and maximum of our options
        comp = [find_min_z9k7,find_max_z9k7]!!(mod depth 2)
        -- helpers to rate everything
        rate_all            = map (rate_all_h)
        rate_all_h option   = rate_board_z9k7 player (option:(p:h)) option
        -- helper to compute the next generation of boards
        next_gen option = generate_z9k7 (other_z9k7 player) (option:(p:h)) option
        -- recursion helper
        next_gen_best option = swap_out option (best_of_z9k7 (other_z9k7 player) max (depth+1) (option:(p:h)) (next_gen option))
        -- we are looking for the best next move to make, so propagate the score, but not the board
        -- we invert the score each time, this is because each board rating will rate boards positively
        -- based on which piece is moving, so we have to alternate the signs to keep things in line.
        swap_out option rating = (option, -1*(snd rating))

-- Functions to find the min or max scored board by rating
find_min_z9k7:: [Rating] -> Rating
find_min_z9k7 (r:rs) = find_helper_z9k7 r rs (<)

find_max_z9k7:: [Rating] -> Rating
find_max_z9k7 (r:rs) = find_helper_z9k7 r rs (>)

find_helper_z9k7:: Rating -> [Rating] -> (Int -> Int -> Bool) -> Rating
find_helper_z9k7 best [] _ = best
find_helper_z9k7 best (r:rs) op 
    | op (snd best) (snd r) = find_helper_z9k7 best rs op
    | otherwise             = find_helper_z9k7 r rs op

-- heuristic_z9k7s --
-- Basic heuristic_z9k7
-- it really likes to win
-- it really doesn't like to lose
-- it likes having pieces
rate_board_z9k7:: Char -> [Board] -> Board -> Rating
rate_board_z9k7 player history board = (board, heuristic_z9k7 player history board)

heuristic_z9k7:: Char -> [Board] -> Board -> Int
heuristic_z9k7 player history board
    | is_win_for_z9k7  player history board  = 99
    | is_loss_for_z9k7 player history board  = -99
    | otherwise                 = (count_z9k7 player board) - (count_z9k7 (other_z9k7 player) board)

is_win_for_z9k7::  Char -> [Board] -> Board -> Bool
is_win_for_z9k7  player history board = is_loss_for_z9k7 (other_z9k7 player) history board
is_loss_for_z9k7:: Char -> [Board] -> Board -> Bool
is_loss_for_z9k7 player history board = ((count_z9k7 player board) < size_of_z9k7 board)

count_z9k7:: Char -> Board -> Int
count_z9k7 c board = length (filter (== c) (dump_board_z9k7 board))

-- helper to switch players
other_z9k7:: Char -> Char
other_z9k7 player
    | player == 'B' = 'W'
    | player == 'W' = 'B'

-- Remove all null values from a list
-- Input  : A list of lists
-- Output : The same list with empty values removed
non_null_z9k7 [] = []
non_null_z9k7 (x:xs)
    | null x = non_null_z9k7 xs
    | otherwise = x : non_null_z9k7 xs

-- Board Generation
--      This function is the top-level function to look one move forward in the game,
--      It will try to move all the pieces horizontally in the top half of the board
--      and then rotate the board by 60 degrees.  It will run through this cycle
--      6 times to get all of the possible moves.
-- Input  : A history of the previous boards in this game
--          The current board
--          The player whose turn it is
-- Output : A list of possible board one move ahead
generate_z9k7 player history board = (foldl (++) [] (generate_helper_z9k7 0 player board)) \\ history -- different added ~x3 time hit

generate_helper_z9k7 n player board
    | n == 6 = []
    | otherwise = (map (rotate_board_z9k7_n (6-n)) (new_boards_z9k7 player (rotate_board_z9k7_n n board))) : generate_helper_z9k7 (n+1) player board


-- generate_z9k7s new boards for a given orientation of the board.
--      Try and slide or crush a piece in the top half of the board
-- Input  : The player whose turn it is and the board
-- Output : All new board if the pieces only moved horizontally
new_boards_z9k7 player board = new_board_for_row_z9k7 0 player board

new_board_for_row_z9k7 num player board
    | num == longest_z9k7 (size_of_z9k7 board) = []
    | otherwise = (new_board_helper_z9k7 player (take num board) (new_moves_z9k7 player (board !! num)) (drop (num + 1) board))
        ++ new_board_for_row_z9k7 (num + 1) player board

new_board_helper_z9k7 _ _ [] _ = []
new_board_helper_z9k7 player top (x:xs) bottom = (top ++ (x:bottom)) : new_board_helper_z9k7 player top xs bottom


-- Given a single "row" of the board (a horizontal slice of 1 row of pieces)
--      This function will return all possible slides or crushes in this row
-- Input  : The player whose turn it is and a row from the board
-- Output : All the rows resulting from legal moves by the current player in this row
new_moves_z9k7 player row = (new_slides_z9k7 player row) ++ (new_jumps_z9k7 player row)

-- Given a single "row" of the board (a horizontal slice of 1 row of pieces)
--      This function will return all possible jumps (or crushes)
-- Input  : The player whose turn it is and a row from the board
-- Output : All the rows resulting from the current player "crushing" only
new_jumps_z9k7 player row = new_jumps_z9k7_right player row
new_jumps_z9k7_right player row = jump_right_z9k7 player row (match_indexes_z9k7 player row jump_match_pattern_z9k7)

-- Given a single "row" of the board (a horizontal slice of 1 row of pieces)
--      This function will return all possible slides in this row
-- Input  : The player whose turn it is and a row from the board
-- Output : All the rows resulting from the current player sliding a piece only
new_slides_z9k7 player row = new_slides_z9k7_right player row
new_slides_z9k7_right player row = slide_right_z9k7 player row (match_indexes_z9k7 player row slide_match_pattern_z9k7)


-- Regex patters for jumps and slides
slide_match_pattern_z9k7   colour = colour:"-"
slide_replace_pattern_z9k7 colour = '-':[colour]
jump_match_pattern_z9k7    colour = colour:colour:'[':'^':colour:"]"
jump_replace_pattern_z9k7  colour = '-':colour:[colour]

-- Regex Matching
-- Input  : The player whose turn it is and row from the board
-- Output : the indexes in the row which a player may slide or crush a piece
match_indexes_z9k7 player row pattern = getAllMatches $ (row =~ (pattern player) :: AllMatches [] (MatchOffset, MatchLength))
slide_right_z9k7 _ row [] = []
slide_right_z9k7 player row (x:xs) = (replaceSegment_z9k7 row (fst x) (slide_replace_pattern_z9k7 player)) : slide_right_z9k7 player row xs
jump_right_z9k7 _ row [] = []
jump_right_z9k7 player row (x:xs) = (replaceSegment_z9k7 row (fst x) (jump_replace_pattern_z9k7 player)) : jump_right_z9k7 player row xs


-- Replace Segment we were given from pegpuzzle.hs
replaceSegment_z9k7 oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise =
        (head oldList):
        (replaceSegment_z9k7 (tail oldList) (pos - 1) segment)


-- Board Math
-- Small helper functions which are self explanatory
longest_z9k7 :: Int -> Int
longest_z9k7 size = (size * 2) - 1

row_size_z9k7 board_size current_row = ((2 * board_size) - abs(board_size - current_row) - 1)

size_of_z9k7 board = length (head board)

tiles_in board = (2*n - 1) + (n - 1) * (3*n - 2)
    where
        n = size_of_z9k7 board
