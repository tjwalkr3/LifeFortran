program check
use Life, only: print_board, count_logic, loop
implicit none
  integer, dimension(3, 3) :: board1
  integer, dimension(3, 3) :: board2
  integer, dimension(5, 5) :: board3

  integer:: generations

  ! test the method for printing the board
  print *, "testing print_board"
  board1 = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(board1))
  call print_board(board1)

  ! test the method for killing cells
  print *, "test 1 for count_logic"
  board2 = reshape((/ 0, 0, 0, 1, 0, 1, 0, 0, 1 /), shape(board2))
  call print_board(board2)
  print *, "Running on middle cell"
  call count_logic(board2)
  call print_board(board2)

  ! test the method for killing cells in 5 board
  print *, "test 2 for count_logic"
  board3 = reshape((/ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), shape(board3))
  call print_board(board3)
  print *, "Running on middle cell"
  call count_logic(board3)
  call print_board(board3)

  ! test the method for adding cells in 5 board
  print *, "test 3 for count_logic"
  board3 = reshape((/ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), shape(board3))
  call print_board(board3)
  print *, "Running on middle cell"
  call count_logic(board3)
  call print_board(board3)

  print *, "test 1 for loop"
  board3 = reshape((/ 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), shape(board3))
  generations = 2
  call print_board(board3)
  call loop(board3, generations)
  ! call print_board(board3)

  !  ! test the method for adding cells
  ! print *, "test 2 for loop"
  ! board3 = reshape((/ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), shape(board3))
  ! call print_board(board3)
  ! print *, "Running on middle cell"
  ! call loop(board3,1)
  ! call print_board(board3)


  
end program check
