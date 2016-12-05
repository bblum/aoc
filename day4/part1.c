#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "array_list.h"
#include "variable_queue.h"

#define NUM(x) ((x)-'a')
int count[26];

int get_max_index()
{
	int maxval = -1, max_index = -1;
	for (int i = 0; i<26; i++) {
		if (count[i] > maxval) {
			maxval = count[i];
			max_index = i;
		}
	}
	return max_index;
}

int main(int argc, char **argv)
{
	assert(argc == 4);
	for (int i = 0; i < 26; i++) count[i] = 0;
	for (int i = 0; i < strlen(argv[1]); i++) {
		count[NUM(argv[1][i])]++;
	}

	// check hash
	bool match = true;
	for (int i = 0; i < strlen(argv[3]); i++) {
		int max_index = get_max_index();
		if (NUM(argv[3][i]) == max_index) {
			count[NUM(argv[3][i])] = -1;
		} else {
			match = false;
			break;
		}

	}
	if (match)
		printf("%s+\n", argv[2]);
	return 0;
}
