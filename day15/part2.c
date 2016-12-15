#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"

struct { unsigned long width, pos; } discs[] = {
	// { .width = 43  , .pos = 2,},
	// { .width = 53  , .pos = 7,},
	// { .width = 61  , .pos = 10,},
	// { .width = 37  , .pos = 2,},
	// { .width = 127 , .pos = 9,},

	// "Upping the ante" input from
	// https://www.reddit.com/r/adventofcode/comments/5ifvyc/2016_day_15_part_3_our_discs_got_larger/
	{ .width = 101 , .pos = 2 },
	{ .width = 163 , .pos = 7 },
	{ .width = 263 , .pos = 10 },
	{ .width = 293 , .pos = 2 },
	{ .width = 373 , .pos = 9 },
	{ .width = 499 , .pos = 0 },
	{ .width = 577 , .pos = 0 },
};

int main(int argc, char **argv)
{
	unsigned long base = 0;
	unsigned long incr = 1;
	for (int disc = 0; disc < ARRAY_SIZE(discs); disc++) {
		int n = discs[disc].width;
		int a = n - (discs[disc].pos + disc + 1);
		for (unsigned long i = base; true; i += incr) {
			if (i % n == a) {
				base = i;
				incr *= n;
				printf("new base %lu, new incr %lu\n", base, incr);
				break;
			}
		}
	}

	return 0;
}
