SRCS = $(wildcard day*.f)
PROGS = $(patsubst %.f,%,$(SRCS))

OBJECTS = intcode.o gcd.o lcm.o

all: $(PROGS)

$(OBJECTS): %.o : %.f

libadvent.a: $(OBJECTS)
	ar cr -o $@ $^

%: %.f libadvent.a
	$(FC) $(FFLAGS) -o $@ $^

.PHONY: clean
clean :
	rm -f day?? *.o *.a
