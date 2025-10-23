program main
  use types
  use geom2d
  use mathfun
  implicit none

  real(kind=rkind):: x, y, res, a, sq

  print *, "enter value x"

  read *, x

  print *, "enter value y"

  read *, y


  res = add(x,y)
  print *, res

  call swap(x,y)
  print *, "x =", x, "y =", y

  a = carea(x)
  print *, "circle area = ", a

  sq = sqarea (y)
  print *, "square area = ", sq



end program main
