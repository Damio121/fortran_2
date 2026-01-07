program main

  use types
  use objects
  implicit none



  class(shape), allocatable :: s
  type(right_triangle) :: t
  type(rectangle) :: r
  type(triangle_xy) :: xy
  type(quadangle_xy) :: q
  integer(kind=ikind) :: user_choice


! ==============================================

  q%x1 = 0
  q%x2 = 0
  q%x3 = 1
  q%x4 = 1

  q%y1 = 0
  q%y2 = 1
  q%y3 = 1
  q%y4 = 0


  xy%x1 = 0
  xy%x2 = 0
  xy%x3 = 1

  xy%y1 = 0
  xy%y2 = 1
  xy%y3 = 0

  t%a = 3.0_rkind
  t%b = 4.0_rkind

  r%width = 2.0_rkind
  r%height = 5.0_rkind

! ========================================================

  print *, "Choose shape: 1 = right triangle, 2 = rectangle, 3 = xy defined triangle, 4 = xy defined quadangle"
  read(*,*) user_choice

  if (user_choice == 1) then
      allocate(s, source=t)

  end if

  if (user_choice == 2) then
        allocate(s, source=r)

  end if

  if (user_choice == 3) then
        allocate(s, source=xy)

  end if

  if (user_choice == 4) then
        allocate(s, source=q)

  end if


  print *, "Area = ", area(s)
  print *, "Perimeter = ", perimeter(s)



end program main
