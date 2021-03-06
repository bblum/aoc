#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "array_list.h"

struct aba { char a; char b; bool bracket; };
ARRAY_LIST(struct aba) abas;

int main(int argc, char **argv)
{
	int brackets=0;
	ARRAY_LIST_INIT(&abas, 10);
	char a = 0, b = 0;
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
		if (i < 2) continue;

		if (argv[1][i] == argv[1][i-2] &&
		    argv[1][i-1] != argv[1][i-2]) {
			struct aba new;
			if (brackets > 0) {
				new.bracket = true;
				new.a = argv[1][i-1];
				new.b = argv[1][i];
			} else {
				new.bracket = false;
				new.a = argv[1][i];
				new.b = argv[1][i-1];
			}
			int i; struct aba *p;
			ARRAY_LIST_FOREACH(&abas, i, p) {
				if (p->a == new.a && p->b == new.b &&
				    p->bracket != new.bracket) {
					printf("yes\n");
					return 0;
				}
			}
			ARRAY_LIST_APPEND(&abas, new);
		}
	}
	return 0;
}
