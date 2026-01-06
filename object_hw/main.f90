program main

  use types
  use objects
  use objects_3d
  implicit none


  type(vector2d) :: v
  type(vector2d) :: v2
  type(vector2d) :: v_sum
  type(vector3d) :: u

  class(shape), allocatable :: s
  type(right_triangle) :: t
  type(rectangle) :: r
  type(triangle_xy) :: xy
  type(quadangle_xy) :: q
  integer(kind=ikind) :: user_choice

  class(solid), allocatable :: s3
  type(tetrahedron_xyz) :: t3
  integer(kind=ikind) :: user_choice_3d

! ==============================================

  v%x = 3.0_rkind
  v%y = 4.0_rkind

  v2%x = 3.0_rkind
  v2%y = 2.0_rkind

  print *, "norm (v) = ", v%norm()

  u%x = 3.0_rkind
  u%y = 4.0_rkind
  u%z = 5.0_rkind

  print *, "norm (u) = ", u%norm()


  v_sum = v%sum(v2)
  print *, v_sum%x, v_sum%y

! ==============================================

  t3%x1 = 0
  t3%x2 = 0
  t3%x3 = 1
  t3%x4 = 1

  t3%y1 = 0
  t3%y2 = 1
  t3%y3 = 0
  t3%y4 = 1

  t3%z1 = 0
  t3%z2 = 1
  t3%z3 = 1
  t3%z4 = 0

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


  print *, "Area = ", s%area()
  print *, "Perimeter = ", s%perimeter()

! ==========================================================

  print *, "Choose solid: 1 = tetrahedron_xyz"
  read(*,*) user_choice_3d

  if (user_choice_3d == 1) then
      allocate(s3, source=t3)

  end if

  print *, "Volume = ", s3%volume()

! =======================================================

end program main
