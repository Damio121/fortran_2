program precisionn
  implicit none

  real :: small, big, res

  small = 1.02
  big = 1.02


  do

  res = small + big
  print *, res, small, big

  if (res <= big) then
    print *, "lost percision at small =", small
    exit
  end if

  small = small / 2

  end do

end program precisionn
