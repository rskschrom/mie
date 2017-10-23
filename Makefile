# makefile : builds mie program

# compile options
FC = gfortran
FCFLAGS = 
FLIBS = 
LPATH = 
OBJS = math.o
MAIN = mie.f90

# main program instructions
mie : $(MAIN) $(OBJS)
	$(FC) $(FCFLAGS) $(LPATH) -o $@ $^ $(FLIBS)

# compile object files here
%.o : %.f90
	$(FC) $(FCFLAGS) -c $< $(FLIBS)

clean :
	rm *.o
