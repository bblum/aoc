#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"

struct { int width, pos; } discs[] = {
	{ .width = 17, .pos = 15, },
	{ .width = 3, .pos = 2, },
	{ .width = 19, .pos = 4, },
	{ .width = 13, .pos = 2, },
	{ .width = 7, .pos = 2, },
	{ .width = 5, .pos = 0, },
	{ .width = 11, .pos = 0, },
};

int main(int argc, char **argv)
{
	for (int t = 0; true; t++) {
		bool success = true;
		for (int d = 0; d < ARRAY_SIZE(discs); d++) {
			if  ((discs[d].pos + t + d + 1) % discs[d].width != 0) {
				success = false;
				break;
			}
		}
		if (success) {
			printf("this is a disappointing problem but the answer is %d\n", t);
			return 0;
		}
	}
	return 0;
}
