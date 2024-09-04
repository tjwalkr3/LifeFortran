program main
  use Life, only: run_game
  implicit none

  print *, "Press any key to run. "
  read(*,*)

  call run_game(10, 10, 10)
end program main
