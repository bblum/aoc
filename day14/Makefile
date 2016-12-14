all: part1 part2

part1: part1.c array_list.h common.h
	gcc -lbsd -std=gnu99 part1.c -o part1
part2: part2.c array_list.h common.h
	gcc -lbsd -std=gnu99 part2.c -o part2
clean:
	rm -f part1 part2
