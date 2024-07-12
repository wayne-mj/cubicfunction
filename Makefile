FC=gfortran
FFLAGS=-O3 -Wall -Wextra
MODULES=factors.f90 mergesort.f90 quadratic.f90 newton.f90 rationalroot.f90
PROG=cubic.f90
SRC=$(MODULES) $(PROG)
OBJ=${SRC:.f90=.o}
BASE=${SRC:.f90=}

all: clean $(PROG:.f90=)

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

$(PROG:.f90=): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	rm -f *.o *.mod $(BASE) *.dat
