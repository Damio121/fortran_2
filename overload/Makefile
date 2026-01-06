FC = gfortran
FFLAGS = -Wall -Wextra -g -fbounds-check -fimplicit-none

ALL_objs := types.o data.o

all : main.o $(ALL_objs)
	$(FC) -g -o overload main.o $(ALL_objs)


types.o: types.f90
	$(FC) $(FFLAGS) -c types.f90


data.o: types.o data.f90
	$(FC) $(FFLAGS) -c data.f90


main.o: $(ALL_objs) main.f90
	$(FC) $(FFLAGS) -c main.f90

clean:
	rm -rf *.o *.mod
