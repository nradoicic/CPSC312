type Row = String
type Board = [Row]

main = print $ out

s = "WWW-WW-------BB-BBB"
z = 3
a = parse_current_board s z
out = rotate_board (rotate_board a)


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

-- Heuristics --
-- Basic Heuristic 
-- it really likes to win
-- it really doesn't like to lose
-- it likes having pieces
rate_board:: Board -> Char -> Int
rate_board board player
    | is_win_for  player board  = 999
    | is_loss_for player board  = -999
    | otherwise                 = count player board
    
is_win_for:: Char -> Board -> Bool
is_win_for  player board = is_loss_for (other player) board
is_loss_for:: Char -> Board -> Bool
is_loss_for player board = (count player board) < size_of board

count:: Char -> Board -> Int
count c board = length (filter (== c) (dump_board board))

other:: Char -> Char
other player 
    | player == 'B' = 'W'
    | player == 'W' = 'B'

-- Remove all null values from a list
non_null [] = []
non_null (x:xs)
    | null x = non_null xs
    | otherwise = x : non_null xs


-- Board Math
longest :: Int -> Int
longest size = (size * 2) - 1

row_size board_size current_row = ((2 * board_size) - abs(board_size - current_row) - 1)

size_of board = length (head board)

tiles_in board = (2*n - 1) + (n - 1) * (3*n - 2)
    where 
        n = size_of board