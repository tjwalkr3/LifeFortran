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

