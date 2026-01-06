module objects

  use types
  implicit none

! ======================================================

  type, abstract :: shape
  contains
    procedure(area_ifc), deferred :: area
    procedure(perimeter_ifc), deferred :: perimeter
  end type shape


  abstract interface
    function area_ifc(self) result(a)
      import :: shape, rkind
      class(shape), intent(in) :: self
      real(kind=rkind) :: a
    end function

    function perimeter_ifc(self) result(p)
      import :: shape, rkind
      class(shape), intent(in) :: self
      real(kind=rkind) :: p
    end function
  end interface




  type, extends(shape) :: rectangle
    real(kind=rkind) :: width, height
  contains
    procedure :: area => rectangle_area
    procedure :: perimeter => rectangle_perimeter
  end type rectangle

  type, extends(shape) :: right_triangle
    real(kind=rkind) :: a, b
  contains
    procedure :: area => rtriangle_area
    procedure :: perimeter => rtriangle_perimeter
  end type right_triangle

  type, extends(shape) :: triangle_xy
    real(kind=rkind) :: x1, x2, x3, y1, y2, y3
  contains
    procedure :: area => xytriangle_area
    procedure :: perimeter => xytriangle_perimeter
  end type triangle_xy

  type, extends(shape) :: quadangle_xy
    real(kind=rkind) :: x1, x2, x3, x4, y1, y2, y3, y4
  contains
    procedure :: area => xyquadangle_area
    procedure :: perimeter => xyquadangle_perimeter
  end type quadangle_xy


! =======================================================

  type :: vector2d
    real (kind=rkind) :: x, y
  contains
    procedure :: norm => vector2d_norm
    procedure :: sum => vector2d_sum
  end type vector2d


  type, extends(vector2d) :: vector3d
    real (kind=rkind) :: z
  contains
    procedure :: norm => vector3d_norm
  end type vector3d




! ===========================================================


contains



  function vector2d_norm(self) result(val)
    class(vector2d), intent (in) :: self
    real(kind=rkind) :: val
    val = sqrt((self%x)**2 + (self%y)**2)
  end function


  function vector3d_norm(self) result(val)
    class(vector3d), intent (in) :: self
    real (kind=rkind) :: val
    val = sqrt((self%x)**2 + (self%y)**2 + (self%z)**2)
  end function

  function vector2d_sum (self, v2) result(v_sum)

    class(vector2d), intent (in) :: self
    class(vector2d), intent (in) :: v2
    type(vector2d) :: v_sum

    v_sum%x = (self%x + v2%x)
    v_sum%y = (self%y + v2%y)

  end function

! ==================================================

  function rectangle_area(self) result(ar)
    class(rectangle), intent(in) :: self
    real(kind=rkind) :: ar
    ar = self%width * self%height
  end function rectangle_area

  function rectangle_perimeter(self) result(p)
    class(rectangle), intent(in) :: self
    real(kind=rkind) :: p
    p = 2.0_rkind * (self%width + self%height)
  end function rectangle_perimeter


  function rtriangle_area(self) result(ar)
    class(right_triangle), intent(in) :: self
    real(kind=rkind) :: ar
    ar = self%a * self%b / 2
  end function rtriangle_area

  function rtriangle_perimeter(self) result(p)
    class(right_triangle), intent(in) :: self
    real(kind=rkind) :: p
    p = self%a + self%b + sqrt((self%a)**2 + (self%b)**2)
  end function rtriangle_perimeter


  function xytriangle_area(self) result(ar)
    class(triangle_xy), intent(in) :: self
    real(kind=rkind) :: ar
    ar = abs(self%x1*(self%y2 - self%y3) + self%x2*(self%y3 - self%y1) + self%x3*(self%y1 - self%y2))/2

  end function xytriangle_area

  function  xytriangle_perimeter(self) result(p)
    class(triangle_xy), intent(in) :: self
    real(kind=rkind) :: p
    p = (sqrt((self%x2-self%x1)**2 + (self%y2-self%y1)**2) + &
       & sqrt((self%x3-self%x2)**2 + (self%y3-self%y2)**2) + &
       & sqrt((self%x1-self%x3)**2 + (self%y1-self%y3)**2))

  end function xytriangle_perimeter

  function xyquadangle_area(self) result(ar)
    class(quadangle_xy), intent(in) :: self
    real(kind=rkind) :: ar
    ar = abs(self%x1*(self%y2 - self%y3) + self%x2*(self%y3 - self%y1) + self%x3*(self%y1 - self%y2))/2 + &
       + abs(self%x3*(self%y4 - self%y1) + self%x4*(self%y1 - self%y3) + self%x1*(self%y3 - self%y4))/2

  end function xyquadangle_area

  function xyquadangle_perimeter(self) result(p)
    class(quadangle_xy), intent(in) :: self
    real(kind=rkind) :: p
    p = (sqrt((self%x2-self%x1)**2 + (self%y2-self%y1)**2) + &
       & sqrt((self%x3-self%x2)**2 + (self%y3-self%y2)**2) + &
       & sqrt((self%x4-self%x3)**2 + (self%y4-self%y3)**2) + &
       & sqrt((self%x1-self%x4)**2 + (self%y1-self%y4)**2))

  end function xyquadangle_perimeter

end module objects
