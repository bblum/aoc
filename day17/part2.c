#include <stdio.h>

int main() {
	int pos = 0, result;
	for (int i = 0; i < 50000000; i++) {
		if ((pos = 1 + ((pos + 377) % (i + 1))) == 1)
			result = i + 1;
	}
	printf("%d\n", result);
}
