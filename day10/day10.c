#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "common.h"

struct bot { int vals[2]; int *low_to; int *high_to; } bots[10000];
int outputs[10000];

#define give(p, v) do { if ((p)[0] == 0) (p)[0] = (v); else (p)[1] = (v); } while (0)

int main(int argc, char **argv)
{
        char buf[256];
        while(fgets(buf, 256, stdin) != NULL) {
                int r;
                int botno, value, low, high;
                if ((r = sscanf(buf, "value %d goes to bot %d", &value, &botno)) != 0) {
			give(&bots[botno].vals[0], value);
		} else if ((r = sscanf(buf, "bot %d gives low to bot %d and high to bot %d",
					 &botno, &low, &high)) == 3) {
			bots[botno].low_to = &bots[low].vals[0];
			bots[botno].high_to = &bots[high].vals[0];
		} else if ((r = sscanf(buf, "bot %d gives low to bot %d and high to output %d",
					 &botno, &low, &high)) == 3) {
			bots[botno].low_to = &bots[low].vals[0];
			bots[botno].high_to = &outputs[high];
		} else if ((r = sscanf(buf, "bot %d gives low to output %d and high to bot %d",
					 &botno, &low, &high)) == 3) {
			bots[botno].low_to = &outputs[low];
			bots[botno].high_to = &bots[high].vals[0];
		} else if ((r = sscanf(buf, "bot %d gives low to output %d and high to output %d",
				       &botno, &low, &high)) == 3) {
			bots[botno].low_to = &outputs[low];
			bots[botno].high_to = &outputs[high];
		}
	}

	bool anything_to_do;
	do {
		anything_to_do = false;
		for (int i = 0; i < 10000; i++) {
			if (bots[i].vals[0] != 0 && bots[i].vals[1] != 0) {
				anything_to_do = true;
				int lower = MIN(bots[i].vals[0], bots[i].vals[1]);
				int higher = MAX(bots[i].vals[0], bots[i].vals[1]);
				if (lower == 17 && higher == 61) printf("part 1: %d\n", i);
				give(bots[i].low_to, lower);
				give(bots[i].high_to, higher);
				bots[i].vals[0] = 0;
				bots[i].vals[1] = 0;
			}
		}
	} while (anything_to_do);

	printf("part 2: %d\n", outputs[0] * outputs[1] * outputs[2]);

	return 0;
}
