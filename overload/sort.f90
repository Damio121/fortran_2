module data

  implicit none
  use types


  interface sort
    model procedure sort_real
    model procedure sort_int
  end interface sort


  contains

    subroutine sort_real(real_vect)

      real(kind=rkind), intent(inout), dimension (:), allocatable :: real_vect

      integer(kind=ikind) :: n, i
      integer(kind=ikind), dimension(1) :: pos
      real(kind=rkind) :: tmp

      if (.not. allocated(real_vect)) return
      n = size(real_vect)
      if (n <= 1) return

      do i = 1, n
        if (i < n) then
          pos = maxloc(real_vect(i:))
          pos(1) = pos(1) + i - 1
        else
          pos(1) = i
        end if

        if (pos(1) /= i) then
          tmp = a(i)
          real_vect(i) = real_vect(pos(1))
          real_vect(pos(1)) = tmp
        end if
      end do

    end subroutine sort_real



    subroutine sort_int (int_vect)

      integer(kind=ikind), intent(inout), dimension (:), allocatable :: int_vect

      integer(kind=ikind) :: n, i
      integer(kind=ikind), dimension(1) :: pos
      integer(kind=ikind) :: tmp

      if (.not. allocated(int_vect)) return
      n = size(int_vect)
      if (n <= 1) return

      do i = 1, n
        if (i < n) then
          pos = maxloc(int_vect(i:))
          pos(1) = pos(1) + i - 1
        else
          pos(1) = i
        end if

        if (pos(1) /= i) then
          tmp = a(i)
          int_vect(i) = int_vect(pos(1))
          int_vect(pos(1)) = tmp
        end if
      end do

    end subroutine sort_int


end module data
