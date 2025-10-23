module geom2d
  use types
  implicit none
  private
  public :: carea, sqarea, rectap


contains

  real(kind=rkind) function carea(r) result(a)
    real(kind=rkind), intent(in) :: r
    real(kind=rkind) :: pi
    pi = 4.0_rkind * atan(1.0_rkind)
    a = pi * r**2.0_rkind
  end function carea

  real(kind=rkind) function sqarea(a) result(sq)
    real(kind=rkind), intent(in) :: a
    sq = a**2.0_rkind
  end function sqarea

  subroutine rectap(a, b, ar, pe)
    real(kind=rkind), intent(in) :: a,b
    real(kind=rkind), intent(out) :: ar, pe
    ar = a*b
    pe = 2.0_rkind*(a+b)

  end subroutine rectap

end module geom2d


