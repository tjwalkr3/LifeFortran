module Life
  implicit none
  private

  public :: print_board, wrap_up, wrap_down, live_or_die
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

  integer function wrap_up(index, count)
    integer, intent(in) :: index, count
    wrap_up = mod((index + 1), count)
  end function wrap_up

  integer function wrap_down(index, count)
    integer, intent(in) :: index, count
    wrap_down = mod((index + count - 1), count)
  end function wrap_down

  subroutine live_or_die(board, column, row)
    integer, intent(inout) :: board(:, :)
    integer, intent(in) :: column, row
    integer :: count = 0
    integer :: col_count
    integer :: row_count
    col_count = size(board, dim=2)
    row_count = size(board, dim=1)
    
    ! check right
    if (board(wrap_up(column, col_count), row) == 1) then
      count = count + 1
    end if

    ! check left
    if (board(wrap_down(column, col_count), row) == 1) then
      count = count + 1
    end if
    
    ! check up
    if (board(column, wrap_up(row, row_count)) == 1) then
      count = count + 1
    end if

    ! check down
    if (board(column, wrap_down(row, row_count)) == 1) then
      count = count + 1
    end if
    
    ! check up right
    if (board(wrap_up(column, col_count), wrap_up(row, row_count)) == 1) then
      count = count + 1
    end if
    
    ! check up left
    if (board(wrap_down(column, col_count), wrap_up(row, row_count)) == 1) then
      count = count + 1
    end if

    ! check down right
    if (board(wrap_up(column, col_count), wrap_down(row, row_count)) == 1) then
      count = count + 1
    end if
    
    ! check down left
    if (board(wrap_down(column, col_count), wrap_down(row, row_count)) == 1) then
      count = count + 1
    end if

    ! A live cell dies if it has fewer than two live neighbors.
    ! A live cell with two or three live neighbors lives on to the next generation.
    ! A live cell with more than three live neighbors dies. 
    ! A dead cell will be brought back to live if it has exactly three live neighbors.
    if (count < 2) then
      board(column, row) = 0
    else if (count == 3) then
      board(column, row) = 1
    else if (count > 3) then
      board(column, row) = 0
    else if (count <= 4 .and. count >= 1 .and. board(column, row) == 1) then
      board(column, row) = 1
    end if
  end subroutine live_or_die
end module Life
