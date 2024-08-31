module Life
  implicit none
  private

  public :: print_board
contains
  subroutine print_board(board)
    integer, intent(in) :: board(:, :)
    integer :: column, row
    do row = 1, size(board, 2)
      do column = 1, size(board, 1)
        write(*,fmt='(I0)',advance="no") board(column, row)
      end do
      print *
    end do
  end subroutine print_board
end module Life

module TestBoard
  use Life, only: print_board
  implicit none
  private

  public :: test_print_board
contains
  subroutine test_print_board()
    integer, dimension(3, 3) :: board
    board = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(board))
    call print_board(board)
  end subroutine test_print_board
end module TestBoard
