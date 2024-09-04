program check
use Life, only: print_board, live_or_die
implicit none
  integer, dimension(3, 3) :: board1
  integer, dimension(3, 3) :: board2

  ! test the method for printing the board
  print *, "testing print_board"
  board1 = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(board1))
  call print_board(board1)

  ! test the method for killing cells
  print *, "test 1 for live_or_die"
  board2 = reshape((/ 0, 0, 0, 1, 0, 1, 0, 0, 1 /), shape(board2))
  call print_board(board2)
  print *, "Running on middle cell"
  call live_or_die(board2, 2, 2)
  call print_board(board2)
  
end program check
