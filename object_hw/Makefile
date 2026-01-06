FC = gfortran
FFLAGS = -Wall -Wextra -g -fbounds-check -fimplicit-none

ALL_objs := types.o objects.o objects_3d.o

all : main.o $(ALL_objs)
	$(FC) -g -o object_hw main.o $(ALL_objs)


types.o: types.f90
	$(FC) $(FFLAGS) -c types.f90

objects.o: types.o objects.f90
	$(FC) $(FFLAGS) -c objects.f90


objects_3d.o: types.o objects_3d.f90
	$(FC) $(FFLAGS) -c objects_3d.f90


main.o:  main.f90 $(ALL_objs)
	$(FC) $(FFLAGS) -c main.f90

clean:
	rm -rf *.o *.mod


