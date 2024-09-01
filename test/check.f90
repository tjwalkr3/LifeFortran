program check
use Life, only: print_board
implicit none
  integer, dimension(3, 3) :: board
  board = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(board))
  call print_board(board)
end program check
