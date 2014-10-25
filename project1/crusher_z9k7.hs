
main = print $ parse_current_board "WWW-WW-------BB-BBB" 3
parse_current_board board size = parse_current_board_helper board size 1 []

parse_current_board_helper board size current_row result
    | (length board) == size = reverse ([board] ++ result)
    | otherwise = parse_current_board_helper (drop row_size board) size (current_row + 1) ((take row_size board):result)
    where
        row_size = ((2 * size) - abs(size - current_row) - 1)

longest :: Int -> Int
longest size = (size * 2) - 1
