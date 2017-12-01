#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "array_list.h"
#include <sys/types.h>
#include <bsd/md5.h>

#define INPUT "ngcjuoqr"
//#define INPUT "abc"

struct hash { int idx; char output[32]; };
ARRAY_LIST(struct hash) shits;

bool success(int to_check) {
	struct hash *h = ARRAY_LIST_GET(&shits, to_check);
	for (int i = 0; i < strlen(h->output); i++) {
		if (i < 2) continue;
		if (h->output[i] == h->output[i - 2] &&
		    h->output[i] == h->output[i - 1]) {
		    	bool foundone = false;
		    	for (int to_compare = to_check + 1; to_compare <= to_check + 1000; to_compare++) {
		    		struct hash *h2 = ARRAY_LIST_GET(&shits, to_compare);
		    		for (int j = 0; j < strlen(h2->output); j++) {
		    			if (j < 4) continue;
		    			if (h2->output[j] != h->output[i]) continue;
					if (h2->output[j] == h2->output[j - 2] &&
					    h2->output[j] == h2->output[j - 1] &&
					    h2->output[j] == h2->output[j - 3] &&
					    h2->output[j] == h2->output[j - 4]) {
						printf("success on %d (%s); match with %d (%s)\n",
						       to_check, h->output, to_compare, h2->output);
						return true;
					}
				}
			}
		    	return false;
		}
	}
	return false;
}
int main(int argc, char **argv)
{
	ARRAY_LIST_INIT(&shits, 10000);
	for (int index = 0; index < 2000; index++) {
		MD5_CTX ctx;
		MD5Init(&ctx);
		char data[1024];
		snprintf(data, 1024, "%s%d", INPUT, index);
		MD5Update(&ctx, data, strlen(data));
		struct hash result = { .idx = index };
		MD5End(&ctx, result.output);
		assert(ARRAY_LIST_SIZE(&shits) == index);
		ARRAY_LIST_APPEND(&shits, result);
	}
	for (int index = 2000, keys = 0; keys < 64; index++) {
		MD5_CTX ctx;
		MD5Init(&ctx);
		char data[1024];
		snprintf(data, 1024, "%s%d", INPUT, index);
		MD5Update(&ctx, data, strlen(data));
		struct hash result = { .idx = index };
		MD5End(&ctx, result.output);
		assert(ARRAY_LIST_SIZE(&shits) == index);
		ARRAY_LIST_APPEND(&shits, result);

		int to_check = index - 2000;
		if (success(to_check)) {
			printf("index %d made %dth key\n", index - 2000, keys);
			keys++;
		}
	}
	return 0;
}
