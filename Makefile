all: calcmem.out

%.out: %.scm
	bigloo $^ -o $@

clean:
	rm -rf *.out *.o

mrproper: clean
	rm -rf *~
