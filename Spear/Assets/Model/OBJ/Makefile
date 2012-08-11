test: ../Model.o OBJ_load.o cvector.o test.o
	$(CC) Model.o OBJ_load.o cvector.o test.o -o $@ -lm

vector: cvector.o vector-test.o
	$(CC) cvector.o vector-test.o -o vector
	

%.o: %.c %.h
	$(CC) -g -c $<


clean:
	@rm -f test vector
	@rm -f *.o
	
