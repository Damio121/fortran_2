FC = gfortran
FFLAGS = -Wall -Wextra -g -fbounds-check -fimplicit-none

ALL_objs := types.o linalg_mod.o sortnumbers.o

all : main.o $(ALL_objs)
	$(FC) -g -o arrays_hw main.o $(ALL_objs)


types.o: types.f90
	$(FC) $(FFLAGS) -c types.f90

linalg_mod.o: types.o linalg_mod.f90
	$(FC) $(FFLAGS) -c linalg_mod.f90


sortnumbers.o: types.o linalg_mod.f90 sortnumbers.f90
	$(FC) $(FFLAGS) -c sortnumbers.f90


main.o: $(ALL_objs) main.f90
	$(FC) $(FFLAGS) -c main.f90

clean:
	rm -rf *.o *.mod
