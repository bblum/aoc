#include <stdio.h>
#define LIMIT 50000000
int nobes[LIMIT + 1];
int main () {
	for (int i = 0, *nobe = &nobes[0]; i < LIMIT; i++) {
		for (int j = 0; j < 377; j++)
			nobe = &nobes[*nobe];
		nobes[i+1] = *nobe;
		*nobe = i+1;
		nobe = &nobes[i+1];
	}
	printf("%d\n", nobes[0]);
}
