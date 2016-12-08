#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
	char buf[256];
	char screen[6][50] = { { 0 } };
	while(fgets(buf, 256, stdin) != NULL) {
		int ret;
		unsigned int x, y, amt;
		if ((ret = sscanf(buf, "rect %ux%u", &x, &y)) != 0) {
			//printf("rec %u %u\n", x, y);
			for (int i = 0; i < x; i++) {
				for (int j = 0; j < y; j++) {
					screen[j][i] = 1;
				}
			}
		} else if ((ret = sscanf(buf, "rotate row y=%u by %u", &y, &amt)) != 0) {
			//printf("row %u %u\n", y, amt);
			char newrow[50] = { 0 };
			for (int i = 0; i < 50; i++) {
				newrow[(i + amt) % 50] = screen[y][i];
			}
			for (int i = 0; i < 50; i++) {
				screen[y][i] = newrow[i];
			}
		} else if ((ret = sscanf(buf, "rotate column x=%u by %u", &x, &amt)) != 0) {
			//printf("col %u %u\n", x, amt);
			char newcol[6] = { 0 };
			for (int i = 0; i < 6; i++) {
				newcol[(i + amt) % 6] = screen[i][x];
			}
			for (int i = 0; i < 6; i++) {
				screen[i][x] = newcol[i];
			}
		}
	}

	int sum = 0;
	for (int j = 0; j < 6; j++) {
		for (int i = 0; i < 50; i++) {
			printf("%c", screen[j][i] ? '#' : ' ');
			if (screen[j][i] != 0) sum++;
		}
		printf("\n");
	}
	printf("%d\n", sum);
	return 0;
}
