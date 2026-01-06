module objects_3d
  use types
  implicit none


  type, abstract :: solid
  contains
    procedure(volume_ifc), deferred :: volume
  end type solid


  abstract interface
    function volume_ifc(self) result(v)
      import :: solid, rkind
      class(solid), intent(in) :: self
      real(kind=rkind) :: v
    end function
  end interface


  type, extends(solid) :: tetrahedron_xyz

    real(kind=rkind) :: x1, y1, z1
    real(kind=rkind) :: x2, y2, z2
    real(kind=rkind) :: x3, y3, z3
    real(kind=rkind) :: x4, y4, z4
  contains
    procedure :: volume => tetrahedron_volume
  end type tetrahedron_xyz

contains


  function tetrahedron_volume(self) result(v)
    class(tetrahedron_xyz), intent(in) :: self
    real(kind=rkind) :: v

    real(kind=rkind) :: ax, ay, az
    real(kind=rkind) :: bx, by, bz
    real(kind=rkind) :: cx, cy, cz
    real(kind=rkind) :: det

    ax = self%x2 - self%x1
    ay = self%y2 - self%y1
    az = self%z2 - self%z1

    bx = self%x3 - self%x1
    by = self%y3 - self%y1
    bz = self%z3 - self%z1

    cx = self%x4 - self%x1
    cy = self%y4 - self%y1
    cz = self%z4 - self%z1

    det = ax * (by * cz - bz * cy) &
        - ay * (bx * cz - bz * cx) &
        + az * (bx * cy - by * cx)

    v = abs(det) / 6.0_rkind

  end function tetrahedron_volume

end module objects_3d
