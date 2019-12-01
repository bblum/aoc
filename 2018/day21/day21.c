#include <stdio.h>
#include <stdbool.h>

bool seen[16777216] = { false };

void main()
{
	int reg4 = 0, prev_reg4 = -1;
	while (true) {
		int reg3 = reg4 | 65536;
		reg4 = 10552971;
retry:
		reg4 += (reg3 & 255);
		reg4 &= 16777215;
		reg4 *= 65899;
		reg4 &= 16777215;
		if (reg3 >= 256) {
			reg3 /= 256;
			goto retry;
		}

		if (seen[reg4]) {
			printf("%d\n", prev_reg4);
			return;
		} else {
			seen[prev_reg4 = reg4] = true;
		}
	}
}
