module sortnumbers

  use types
  use linalg_mod

  private :: checkinputs
  public :: sorthighlow, sortlowhigh, sorthighlow_inplace, sortlowhigh_inplace

contains

  function checkinputs(array_in, array_out) result(success)

    real(kind=rkind), intent(in), allocatable, dimension (:) :: array_in
    real(kind=rkind), intent(out), allocatable, dimension (:) :: array_out
    logical :: success

    if(.not.(allocated(array_in))) then
      success = .false.
      return
    end if

    if(.not.(allocated(array_out))) then
      allocate(array_out(ubound(array_in, 1)))

    else if ((ubound(array_out,1)) /= (ubound(array_in,1))) then
        deallocate(array_out)
        allocate(array_out(ubound(array_in,1)))
    end if

    success = .true.


  end function checkinputs



  subroutine sorthighlow (array_in, array_out, success)

    real(kind=rkind), intent(in), allocatable, dimension (:) :: array_in
    real(kind=rkind), intent(out), allocatable, dimension (:) :: array_out
    logical :: success
    integer (kind=ikind) :: i
    real(kind=rkind) :: tmp
    integer (kind=ikind), dimension(1) :: pos

    if (.not. checkinputs(array_in, array_out)) then
      ERROR STOP "incorrect inputs, exited from sortnumbers::sorthighlow"
    end if

    array_out = array_in

    do i = 1, ubound(array_out, 1)

      pos = maxloc(array_out(i:)) + i - 1

      tmp = array_out(pos(1))
      array_out(pos(1)) = array_out(i)
      array_out(i) = tmp

    end do

    success = .true.
    print *, success

  end subroutine sorthighlow


  subroutine sortlowhigh (array_in, array_out, success)

    real(kind=rkind), intent(in), allocatable, dimension (:) :: array_in
    real(kind=rkind), intent(out), allocatable, dimension (:) :: array_out
    logical :: success
    real(kind=rkind) :: tmp
    integer (kind=ikind) :: i
    integer (kind=ikind), dimension(1) :: pos

    if (.not. checkinputs(array_in, array_out)) then
      ERROR STOP "incorrect inputs, exited from sortnumbers::sortlowhigh"
     end if


    array_out = array_in

    do i = 1, ubound(array_out, 1)

      pos = minloc(array_out(i:)) + i - 1

      tmp = array_out(pos(1))
      array_out(pos(1)) = array_out(i)
      array_out(i) = tmp

    end do

    success = .true.
    print *, success

  end subroutine sortlowhigh



  subroutine sorthighlow_inplace(array)

    real(kind=rkind), intent(inout), dimension(:), allocatable :: array

    integer(kind=ikind) :: n, i
    integer(kind=ikind), dimension(1) :: pos
    real(kind=rkind) :: tmp

    if (.not. allocated(array)) return

    n = size(array)

    if (n <= 1) return


    do i = 1, n

      if (i < n) then
        pos = maxloc(array(i:)) + i - 1
      else
        pos(1) = i
      end if


      if (pos(1) /= i) then
        tmp = array(i)
        array(i) = array(pos(1))
        array(pos(1)) = tmp
      end if

    end do


  end subroutine sorthighlow_inplace



  subroutine sortlowhigh_inplace(array)

    real(kind=rkind), intent(inout), dimension(:), allocatable :: array

    integer(kind=ikind) :: n, i
    integer(kind=ikind), dimension(1) :: pos
    real(kind=rkind) :: tmp

     if (.not. allocated(array)) return

    n = size(array)

    if (n <= 1) return


    do i = 1, n

      if (i < n) then
        pos = minloc(array(i:)) + i - 1
      else
        pos(1) = i
      end if


      if (pos(1) /= i) then
        tmp = array(i)
        array(i) = array(pos(1))
        array(pos(1)) = tmp
      end if

    end do

  end subroutine sortlowhigh_inplace




end module sortnumbers
