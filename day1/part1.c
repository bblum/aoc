#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

char *input[] = {
"R3", "L5", "R2", "L2", "R1", "L3", "R1", "R3", "L4", "R3", "L1", "L1", "R1", "L3", "R2", "L3", "L2", "R1", "R1", "L1", "R4", "L1", "L4", "R3", "L2", "L2", "R1", "L1", "R5", "R4", "R2", "L5", "L2", "R5", "R5", "L2", "R3", "R1", "R1", "L3", "R1", "L4", "L4", "L190", "L5", "L2", "R4", "L5", "R4", "R5", "L4", "R1", "R2", "L5", "R50", "L2", "R1", "R73", "R1", "L2", "R191", "R2", "L4", "R1", "L5", "L5", "R5", "L3", "L5", "L4", "R4", "R5", "L4", "R4", "R4", "R5", "L2", "L5", "R3", "L4", "L4", "L5", "R2", "R2", "R2", "R4", "L3", "R4", "R5", "L3", "R5", "L2", "R3", "L1", "R2", "R2", "L3", "L1", "R5", "L3", "L5", "R2", "R4", "R1", "L1", "L5", "R3", "R2", "L3", "L4", "L5", "L1", "R3", "L5", "L2", "R2", "L3", "L4", "L1", "R1", "R4", "R2", "R2", "R4", "R2", "R2", "L3", "L3", "L4", "R4", "L4", "L4", "R1", "L4", "L4", "R1", "L2", "R5", "R2", "R3", "R3", "L2", "L5", "R3", "L3", "R5", "L2", "R3", "R2", "L4", "L3", "L1", "R2", "L2", "L3", "L5", "R3", "L1", "L3", "L4", "L3",
};

#define LEN 161

int heading = 0;
int x = 0;
int y = 0;

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

void move(int dist)
{
	if (heading == 0) {
		y += dist;
	} else if (heading == 1) {
		x += dist;
	} else if (heading == 2) {
		y -= dist;
	} else if (heading == 3) {
		x -= dist;
	} else {
		assert(0);
	}
}

int main()
{

	for (int i = 0; i < LEN; i++) {
		turn(input[i][0]);
		move(atoi(&input[i][1]));
	}
	if (x < 0) x = -x;
	if (y < 0) y = -y;
	printf("%d\n", x+y);
	return 0;
}
