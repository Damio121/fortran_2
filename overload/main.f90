program main

  use types
  use data

  integer(kind=ikind), allocatable, dimension (:) :: int_v
  real(kind=rkind), allocatable, dimension (:) :: real_v
  logical :: success

    allocate(int_v(7))
    allocate(real_v(3))

    int_v = [5, 2, 1, 90, 12, 6]
    real_v = [0.46, 5.0, 1.2]

    print *, int_v

    call sort(int_v)

    print *, int_v



    print *, real_v

    call sort(real_v)

    print *, real_v

    deallocate (int_v, real_v)


end program main
