module solids
  use types
  implicit none

  ! 1. Abstract Base Type for 3D objects
  type, abstract :: solid
  contains
    procedure(volume_ifc), deferred :: volume
  end type solid

  ! 2. Abstract Interface for Volume
  abstract interface
    function volume_ifc(self) result(v)
      import :: solid, rkind
      class(solid), intent(in) :: self
      real(kind=rkind) :: v
    end function
  end interface

  ! 3. Tetrahedron Type
  type, extends(solid) :: tetrahedron_xyz
    ! 4 points (x,y,z)
    real(kind=rkind) :: x1, y1, z1
    real(kind=rkind) :: x2, y2, z2
    real(kind=rkind) :: x3, y3, z3
    real(kind=rkind) :: x4, y4, z4
  contains
    procedure :: volume => tetrahedron_volume
  end type tetrahedron_xyz

contains

  ! ---------------------------------------------------------
  ! Implementation: Volume of Tetrahedron
  ! Formula: V = |det(A, B, C)| / 6
  ! Where A, B, C are vectors from point 1 to points 2, 3, 4
  ! ---------------------------------------------------------
  function tetrahedron_volume(self) result(v)
    class(tetrahedron_xyz), intent(in) :: self
    real(kind=rkind) :: v
    
    ! Vectors relative to point 1
    real(kind=rkind) :: ax, ay, az
    real(kind=rkind) :: bx, by, bz
    real(kind=rkind) :: cx, cy, cz
    real(kind=rkind) :: det

    ! Vector 1->2
    ax = self%x2 - self%x1
    ay = self%y2 - self%y1
    az = self%z2 - self%z1

    ! Vector 1->3
    bx = self%x3 - self%x1
    by = self%y3 - self%y1
    bz = self%z3 - self%z1

    ! Vector 1->4
    cx = self%x4 - self%x1
    cy = self%y4 - self%y1
    cz = self%z4 - self%z1

    ! Determinant calculation (Scalar Triple Product)
    ! Det = ax(by*cz - bz*cy) - ay(bx*cz - bz*cx) + az(bx*cy - by*cx)
    det = ax * (by * cz - bz * cy) &
        - ay * (bx * cz - bz * cx) &
        + az * (bx * cy - by * cx)

    v = abs(det) / 6.0_rkind

  end function tetrahedron_volume

end module solids