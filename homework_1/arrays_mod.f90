module arrays_mod
  use types
  implicit none

contains

  subroutine static_demo()
    implicit none

    real(kind=rkind), dimension(10) :: a, b, c
    integer(kind=ikind) :: i

    a = 1.0_rkind
    b = 2.0_rkind

    c(1) = 3.0_rkind

    do i = 2, 10
      c(i) = a(i) + b(i) + c (i-1)
    end do

    print *, "static_demo:␣c(1)=", c(1), "␣c(10)=", c(10)

    print *, "array A, B and C is:"

    do i=1, 10
      print *, "i=", i, "A(i) = ", a(i), "b(i) =", b(i), "c(i)=", c(i)
    end do
  end subroutine static_demo

  subroutine dynamic_demo(n,m)
    implicit none

    integer(kind=ikind), intent(in) :: n, m
    real(kind=rkind), allocatable :: x(:), y(:)
    real(kind=rkind), allocatable :: A(:,:)

    allocate(A(n,m), x(m), y(n))

    print *, "dynamic_demo: A rows", lbound(A,1), ubound(A,1), &
          " cols:", lbound(A,2), ubound(A,2)

  end subroutine dynamic_demo


  subroutine alloc_and_bounds(n)
    implicit none

    integer(kind=ikind), intent(in) :: n
    real(kind=rkind), dimension(:), allocatable :: v
    real(kind=rkind), dimension(:,:), allocatable :: M
    integer(kind=ikind) :: rL, rU, cL, cU

    allocate( v(n), M(n,n) )



    rL = lbound(M,1); rU = ubound(M,1)
    cL = lbound(M,2); cU = ubound(M,2)

    print *, "lower␣and␣upper_bounds␣of␣v␣", lbound(v,1), ubound(v,1)

    print *, "alloc_and_bounds:␣M␣rows:", rL, rU, "␣cols:", cL, cU

  end subroutine alloc_and_bounds

  subroutine onion

  integer(kind=ikind), intent(in) :: n,m
  real (kind=rkind), dimension (:,:), allocatable :: M

  allocate(A(n,m))



  end subroutine onion

end module arrays_mod


