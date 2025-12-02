program main

  use types
  use linalg_mod
  use sortnumbers

  implicit none

  real(kind=rkind), allocatable, dimension (:) :: v, vout
  logical :: success

  allocate(v(7))


  v = (/5, 2, 1, -8, 90, 12,6/)

  print *, "original vector =", v

  call sorthighlow(v, vout, success)

  print *, "new vector =", vout

  print *, "orginal check =", v

  call sorthighlow_inplace(v)

  print *, "changed vector =", v

  deallocate(v)

end program main
