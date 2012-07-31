all: OBJ_load.h OBJ_load.cc test.cc ../Model.c
	g++ -g -c OBJ_load.cc
	g++ -g -c test.cc
	g++ -g -c ../Model.c -o Model.o
	g++ -o test *.o

clean:
	@rm -f test
	@rm -f *.o
	
