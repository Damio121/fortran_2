program main

  use types
  use linalg
  implicit none

  integer(kind=ikind) :: n
  real(kind=rkind), allocatable :: v(:)
  real(kind=rkind), allocatable :: A(:,:)


  print *, "enter n, n>2:"

  read *, n


  if (n <= 2) then
    print *, "n must be > 2"
    return
  end if


  allocate(v(n))
  allocate(A(n,n))

  call random_seed()
  call random_number(v)


  print *, "vector minimum =", vmin(v)
  print *, "vector maximum =", vmax(v)
  print *, "vector sum =", vsum(v)
  print *, "euclidian norm =", eucnorm(v)
  call minmaxloc(v)

  print *, "diagonal of A"
  call diag_matrix (A = A, v = v)


  deallocate(v, A)

end program main
