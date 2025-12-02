module linalg_mod
  use types
  implicit none

contains

  function vmin(v) result(min_val)

    real(kind=rkind), intent(in) :: v(:)
    real(kind=rkind) :: min_val

    min_val = minval(v)

  end function vmin


  function vmax(v) result(max_val)

    real(kind=rkind), intent(in) :: v(:)
    real(kind=rkind) :: max_val

    max_val = maxval(v)

  end function vmax


  function vsum(v) result(sum_val)

    real(kind=rkind) :: v(:)
    real (kind=rkind) :: sum_val

    sum_val = sum(v)

  end function vsum


  subroutine minmaxloc(v)

    real(kind=rkind), intent(in):: v(:)

    print *, "maximum location =", maxloc(v), "minimum location =", minloc(v)

  end subroutine minmaxloc


  function eucnorm(v) result(norm_val)

    real(kind=rkind) :: v(:)
    real(kind=rkind) :: norm_val

    norm_val = norm2(v)

  end function eucnorm

  subroutine diag_matrix(A, v)

    real(kind=rkind) :: A(:,:)
    real(kind=rkind) :: v(:)
    integer(kind=ikind) :: i

    A = 0_rkind

    do i = lbound(v,1), ubound(v,1)
      A(i,i) = v(i)
      print *, A(i,i)

    end do

  end subroutine diag_matrix

end module linalg_mod
