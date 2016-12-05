#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "array_list.h"
#include "variable_queue.h"

//#define NUM(x) ((x)-'a')
//int count[26];
//
//int max()
//{
//	int maxval = -1;
//	for (int i = 0; i<26; i++) {
//		if (count[i] > maxval) maxval = count[i];
//	}
//	return maxval;
//}

int main(int argc, char **argv)
{
	assert(argc == 4);
	int key = atoi(argv[2]);
	for (int i = 0; i < strlen(argv[1]); i++) {
		int x = argv[1][i];
		if (x != '-') {
			x = x - 'a';
			x += key;
			x = x % 26;
			assert(x < 26);
			x = x + 'a';
		}
		printf("%c", x);
	}
	printf("\t %d\n", key);

	return 0;
//	for (int i = 0; i < 26; i++) count[i] = 0;
//	for (int i = 0; i < strlen(argv[1]); i++) {
//		if (argv[1][i] == '-') continue;
//		count[NUM(argv[1][i])]++;
//	}
//
//	// check hash
//	bool match = true;
//	for (int i = 0; i < strlen(argv[3]); i++) {
//		if (count[NUM(argv[3][i])] == max()) {
//			count[NUM(argv[3][i])] = -1;
//		} else {
//			match = false;
//			break;
//		}
//
//	}
//	if (match)
//		printf("%s\t%s\n", argv[1], argv[2]);
//	return 0;
}
