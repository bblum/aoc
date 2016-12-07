#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "array_list.h"

int main(int argc, char **argv)
{
	int brackets=0;
	bool match = false, notmatch = false;
	for (int i = 0; i < strlen(argv[1]); i++) {
		if (argv[1][i] == '[') {
			assert(brackets==0);
			brackets++;
			continue;
		} else if (argv[1][i] == ']' ){
			assert(brackets==1);
			brackets--;
			continue;
		}
		if (i < 3) continue;

		if (argv[1][i] == argv[1][i-3] &&
			argv[1][i-1] == argv[1][i-2] &&
			argv[1][i-1] != argv[1][i-3]) {
			if (brackets > 0) {
				notmatch = true;
			} else {
				match = true;
			}
		}
	}
	if (match && !notmatch) printf("yes\n");
	return 0;
}
