#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "array_list.h"

char *input[] = {
"R3", "L5", "R2", "L2", "R1", "L3", "R1", "R3", "L4", "R3", "L1", "L1", "R1", "L3", "R2", "L3", "L2", "R1", "R1", "L1", "R4", "L1", "L4", "R3", "L2", "L2", "R1", "L1", "R5", "R4", "R2", "L5", "L2", "R5", "R5", "L2", "R3", "R1", "R1", "L3", "R1", "L4", "L4", "L190", "L5", "L2", "R4", "L5", "R4", "R5", "L4", "R1", "R2", "L5", "R50", "L2", "R1", "R73", "R1", "L2", "R191", "R2", "L4", "R1", "L5", "L5", "R5", "L3", "L5", "L4", "R4", "R5", "L4", "R4", "R4", "R5", "L2", "L5", "R3", "L4", "L4", "L5", "R2", "R2", "R2", "R4", "L3", "R4", "R5", "L3", "R5", "L2", "R3", "L1", "R2", "R2", "L3", "L1", "R5", "L3", "L5", "R2", "R4", "R1", "L1", "L5", "R3", "R2", "L3", "L4", "L5", "L1", "R3", "L5", "L2", "R2", "L3", "L4", "L1", "R1", "R4", "R2", "R2", "R4", "R2", "R2", "L3", "L3", "L4", "R4", "L4", "L4", "R1", "L4", "L4", "R1", "L2", "R5", "R2", "R3", "R3", "L2", "L5", "R3", "L3", "R5", "L2", "R3", "R2", "L4", "L3", "L1", "R2", "L2", "L3", "L5", "R3", "L1", "L3", "L4", "L3",
};

#define LEN 161

int heading = 0;
int x = 0;
int y = 0;

#define DIST() ((x > 0 ? x : -x) + (y > 0 ? y : -y))

struct loc { int x; int y; };
ARRAY_LIST(struct loc) locs;

void turn(char dir)
{
	if (dir == 'L') {
		if (heading == 0) heading = 3;
		else heading--;
	} else if (dir == 'R') {
		if (heading == 3) heading = 0;
		else heading++;
	} else assert(0);
}

void step() {
	bool found = false;
	struct loc *locp;
	int i;
	ARRAY_LIST_FOREACH(&locs, i, locp) {
		if (locp->x == x && locp->y == y) {
			found = true;
		}
	}
	if (found) {
		printf("visited (%d,%d) twice @ dist %d\n", x, y, DIST());
	} else {
		struct loc new_loc = { .x = x, .y = y };
		ARRAY_LIST_APPEND(&locs, new_loc);
	}
}

void move(int dist)
{
	if (heading == 0) {
		while (dist > 0) {
			y++;
			dist--;
			step();
		}
	} else if (heading == 1) {
		while (dist > 0) {
			x++;
			dist--;
			step();
		}
	} else if (heading == 2) {
		while (dist > 0) {
			y--;
			dist--;
			step();
		}
	} else if (heading == 3) {
		while (dist > 0) {
			x--;
			dist--;
			step();
		}
	} else {
		assert(0);
	}

}

int main()
{
	ARRAY_LIST_INIT(&locs, 10);
	for (int i = 0; i < LEN; i++) {
		turn(input[i][0]);
		move(atoi(&input[i][1]));
	}
	printf("%d\n", DIST());
	return 0;
}
