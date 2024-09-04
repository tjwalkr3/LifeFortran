module Life
  implicit none
  private

  public :: print_board, wrap_up, wrap_down, live_or_die, count_logic, loop, run_game

contains

  subroutine print_board(board)
    integer, intent(in) :: board(:, :)
    integer :: column, row
    do row = 1, size(board, 2)
      do column = 1, size(board, 1)
        write(*, fmt='(I0)', advance="no") board(column, row)
      end do
      print *
    end do
  end subroutine print_board

  integer function wrap_up(index, count)
    integer, intent(in) :: index, count
    if (index == count) then
      wrap_up = 1
    else
      wrap_up = index + 1
    end if
  end function wrap_up

  integer function wrap_down(index, count)
    integer, intent(in) :: index, count
    if (index == 1) then
      wrap_down = count
    else
      wrap_down = index - 1
    end if
  end function wrap_down

  function live_or_die(board, column, row) result(count)
    integer, intent(inout) :: board(:, :)
    integer, intent(in) :: column, row
    integer :: count
    integer :: col_count, row_count

    count = 0
    col_count = size(board, dim=2)
    row_count = size(board, dim=1)

    ! Check all eight neighbors
    ! right
    if (board(wrap_up(column, col_count), row) == 1) count = count + 1
    ! left
    if (board(wrap_down(column, col_count), row) == 1) count = count + 1
    ! up
    if (board(column, wrap_up(row, row_count)) == 1) count = count + 1
    ! down
    if (board(column, wrap_down(row, row_count)) == 1) count = count + 1
    ! up right
    if (board(wrap_up(column, col_count), wrap_up(row, row_count)) == 1) count = count + 1
    ! up left
    if (board(wrap_down(column, col_count), wrap_up(row, row_count)) == 1) count = count + 1
    ! down right
    if (board(wrap_up(column, col_count), wrap_down(row, row_count)) == 1) count = count + 1
    ! down left
    if (board(wrap_down(column, col_count), wrap_down(row, row_count)) == 1) count = count + 1

  end function live_or_die

  subroutine count_logic(board)
    integer, intent(inout) :: board(:, :)
    integer :: col_count, row_count
    integer :: column, row, count
    integer :: temp_board(size(board, 1), size(board, 2))

    col_count = size(board, dim=2)
    row_count = size(board, dim=1)

    ! Iterate over each cell in the board
    do row = 1, row_count
      do column = 1, col_count
        count = live_or_die(board, column, row)

        ! Apply the Game of Life rules
        if (board(column, row) == 1) then
          if (count < 2 .or. count > 3) then
            temp_board(column, row) = 0
          else
            temp_board(column, row) = 1
          end if
        else
          if (count == 3) then
            temp_board(column, row) = 1
          else
            temp_board(column, row) = 0
          end if
        end if
      end do
    end do

    ! Update the board with the next generation
    board = temp_board

  end subroutine count_logic
 
  subroutine loop(board, generations)
    integer, intent(inout) :: board(:, :)
    integer :: generations
    integer :: i

    do i=1, generations
     call count_logic(board)
     print *, "Running Generation: ",i 
     call sleep(1)
     call print_board(board)
    end do
  end subroutine loop

  subroutine run_game(rows, columns, generations)
    integer, intent(in) :: columns , rows, generations
    integer :: column, row
    integer :: index
    real :: rand_real
    integer, dimension(columns, rows) :: gameboard
    
    do row = 1, size(gameboard, 2)
      do column = 1, size(gameboard, 1)
        call random_number(rand_real)
        gameboard(row, column) = FLOOR(2 * rand_real)
      end do
    end do

    call loop(gameboard, generations)
  end subroutine run_game

end module Life
