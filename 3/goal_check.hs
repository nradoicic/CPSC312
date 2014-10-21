a = ["--B---","--B---","XXB---","--AA--","------","------"]
b = ["--B---","--B---","--BXX-","--AA--","------","------"]
c = ["--B---","--B---","--B-XX","--AA--","------","------"]

the_board = b

main = print $ goal_check the_board

goal_check board = (exit_row !! 0) == 'X' && (exit_row !! 1) == 'X'
    where exit_row = reverse (board !! 2)
