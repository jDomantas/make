tests/foo.exe: tests/foo.o tests/bar.o
	gcc -o $@ $^
	
tests/%.o: tests/%.c
	gcc -c -o $@ $<