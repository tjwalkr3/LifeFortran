module LifeFortran
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, LifeFortran!"
  end subroutine say_hello
end module LifeFortran
