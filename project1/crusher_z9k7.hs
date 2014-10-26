main = print $ out

a = parse_current_board "WWW-WW-------BB-BBB" 3
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
