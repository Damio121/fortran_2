module mathfun

  use types
  use geom2d
  implicit none
  private
  public :: add, swap


contains

  real(kind=rkind) function add(a,b) result(res)
    real(kind=rkind), intent(in) :: a, b
    res = a+b
  end function add

  subroutine swap(x,y)
    real(kind=rkind) :: x, y, a, b
    a = x
    b = y
    x = b
    y = a
  end subroutine swap

end module mathfun
