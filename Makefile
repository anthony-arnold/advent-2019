SRCS = $(wildcard day*.f)
PROGS = $(patsubst %.f,%,$(SRCS))

all: $(PROGS)

intcode.o: intcode.f

%: %.f intcode.o
	$(FC) $(FFLAGS) -o $@ $< intcode.o

.PHONY: clean
clean :
	rm -f day?? *.o
