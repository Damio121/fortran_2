module shapes
  implicit none
  private
  public :: shape_with_ops, rectangle, circle

  integer, parameter :: rkind = kind(1.0d0)

  !-------------------------------------------------
  ! 1) Forward declaration of the abstract type
  !-------------------------------------------------
  type, abstract :: shape_with_ops
  end type shape_with_ops

  !-------------------------------------------------
  ! 2) Abstract interfaces (now the type name exists)
  !-------------------------------------------------
  abstract interface
    function area_ifc(self) result(a)
      import :: shape_with_ops, rkind
      class(shape_with_ops), intent(in) :: self
      real(kind=rkind) :: a
    end function area_ifc

    function perimeter_ifc(self) result(p)
      import :: shape_with_ops, rkind
      class(shape_with_ops), intent(in) :: self
      real(kind=rkind) :: p
    end function perimeter_ifc
  end interface

  !-------------------------------------------------
  ! 3) Complete the abstract type with bindings
  !-------------------------------------------------
  type, abstract :: shape_with_ops
  contains
    procedure(area_ifc), deferred :: area
    procedure(perimeter_ifc), deferred :: perimeter
  end type shape_with_ops

  !-------------------------------------------------
  ! 4) Concrete types
  !-------------------------------------------------
  type, extends(shape_with_ops) :: rectangle
    real(kind=rkind) :: width, height
  contains
    procedure :: area      => rectangle_area
    procedure :: perimeter => rectangle_perimeter
  end type rectangle

  type, extends(shape_with_ops) :: circle
    real(kind=rkind) :: radius
  contains
    procedure :: area      => circle_area
    procedure :: perimeter => circle_perimeter
  end type circle

contains

  function rectangle_area(self) result(a)
    class(rectangle), intent(in) :: self
    real(kind=rkind) :: a
    a = self%width * self%height
  end function rectangle_area

  function rectangle_perimeter(self) result(p)
    class(rectangle), intent(in) :: self
    real(kind=rkind) :: p
    p = 2.0_rkind * (self%width + self%height)
  end function rectangle_perimeter

  function circle_area(self) result(a)
    class(circle), intent(in) :: self
    real(kind=rkind) :: a
    a = 3.141592653589793_rkind * self%radius**2
  end function circle_area

  function circle_perimeter(self) result(p)
    class(circle), intent(in) :: self
    real(kind=rkind) :: p
    p = 2.0_rkind * 3.141592653589793_rkind * self%radius
  end function circle_perimeter

end module shapes
