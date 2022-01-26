FC=gfortran
CFLAG=-Wall -g
LDFLAG=-fbacktrace -fdump-core
#	-fcheck-array-temporaries  -static-libgfortran
SOURCES=bitmap.f90 static.f90 util.f90 encode.f90 decode.f90 stegutil.f90
EXECUTABLE=stegutil
OBJECTS=$(SOURCES:.f90=.o)

all: stegutil

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(OBJECTS) $(LDFLAG) -o $@

bitmap.o: bitmap.f90
	$(FC) -c $(CFLAG) $<

static.o: static.f90
	$(FC) -c $(CFLAG) $<

stegutil.o: stegutil.f90
	$(FC) -c $(CFLAG) $<

encode.o: encode.f90
	$(FC) -c $(CFLAG) $<

decode.o: decode.f90
	$(FC) -c $(CFLAG) $<

util.o: util.f90
	$(FC) -c $(CFLAG) $<

clean:
	rm -fv $(EXECUTABLE) *.o *.mod