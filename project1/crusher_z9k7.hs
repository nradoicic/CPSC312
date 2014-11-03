import Text.Regex.Posix

type Row = String
type Board = [Row]
type Rating = (Board, Int)

main = mapM_ print $ out

a = parse_current_board "WWW-WW-------BB-BBB" 3
out = generate "W" [["WWW","-WW-","-----","-BB-","BBB"]] ["WWW","-WW-","-----","-BB-","BBB"] -- First Board
-- out = generate "W" ["---","----","--W--","----","---"] -- One piece in middle
-- out = generate "B" ["WW-","-W--","-----","B-B-","--B"] -- 12 moves for B

-- Board parser
-- Input  : A string to represent the board, the size of one side of the board
-- Output : A list of strings, each string representing a different row of the board
--      "WWW-WW-------BB-BBB" -> ["WWW","-WW-","-----","-BB-","BBB"]
parse_current_board board_string board_size = parse_current_board_helper board_string board_size 1 []

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

-- Board Multi Rotator
-- Rotates a board the given number of times
rotate_board_multi:: Board -> Int -> Board
rotate_board_multi board 0 = board
rotate_board_multi board x = rotate_board_multi (rotate_board board) (x-1)

-- MINIMAX
pick_board:: [Board] -> String -> Int -> Board
pick_board (board:history) player max = fst (best_of player max 1 (board:history) (generate player (board:history) board))

best_of:: String -> Int -> Int -> [Board] -> [Board] -> Rating
best_of player max depth (p:h) [] = rate_board player (p:h) p -- consider using the difference between max and depth, win earlier is better?
best_of player max depth (p:h) options
    | depth == max  = comp (rate_all options) --leaves
    | otherwise     = comp (map (next_gen_best) options) 
    where
        comp = [find_min,find_max]!!(mod depth 2)
        base = [9999,-9999]!!(mod depth 2)
        rate_all = map (rate_board player (p:h))
        next_gen option = generate (other player) (option:(p:h)) option
        next_gen_best option = swap_out option (best_of (other player) max (depth+1) (option:(p:h)) (next_gen option))
        swap_out option rating = (option, snd rating)

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
rate_board:: String -> [Board] -> Board -> Rating
rate_board player history board = (board, heuristic player history board)

heuristic:: String -> [Board] -> Board -> Int
heuristic player history board
    | is_win_for  player history board  = 99
    | is_loss_for player history board  = -99
    | otherwise                 = (count player board) - (count (other player) board)
    
is_win_for::  String -> [Board] -> Board -> Bool
is_win_for  player history board = is_loss_for (other player) history board
is_loss_for:: String -> [Board] -> Board -> Bool
is_loss_for player history board = ((count player board) < size_of board) || (null (generate player history board))

count:: String -> Board -> Int
count c board = length (filter (== (c!!0)) (dump_board board))

other:: String -> String
other player 
    | player == "B" = "W"
    | player == "W" = "B"

-- Remove all null values from a list
non_null [] = []
non_null (x:xs)
    | null x = non_null xs
    | otherwise = x : non_null xs

    
-- Board generation

generate player history board = foldl (++) [] (generate_helper 0 player board)
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

slide_match_pattern   colour = colour ++ "-"
slide_replace_pattern colour = "-" ++ colour
jump_match_pattern    colour = colour ++ colour ++ "[^" ++ colour ++ "]"
jump_replace_pattern  colour = "-" ++ colour ++ colour

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
