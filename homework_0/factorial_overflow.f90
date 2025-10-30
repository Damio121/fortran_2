program factorial_over

  implicit none

  integer(kind=8) :: res
  integer(kind=4) :: i, n

  print *, "enter value"

  read *, n

  res = 1

  do i = 1, n
    if (res * i < res) then
      print *, "overflow detected at n =", i
      exit
    else
      res = res * i
    end if
  end do

  print *, res

end program factorial_over
