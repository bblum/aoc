#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <bsd/md5.h>


#define HASH_LEN 32
int main(int argc, char **argv)
{
	char *data0="cxdnnyjw";
	//char *data0="abc";
	char data[1024];
	int salt = 0;
	int chars = 0;

	while (chars < 8) {
		//if (salt % 10000 == 0) printf("salt %d chars %d\n", salt, chars);
		MD5_CTX ctx;
		MD5Init(&ctx);
		snprintf(data, 1024, "%s%d", data0, salt);
		MD5Update(&ctx, data, strlen(data));
		char output[HASH_LEN];
		//MD5Final(&output[0], &ctx);
		MD5End(&ctx, output);
		bool match = true;
		for (int i = 0; i < 5; i++) {
			if (output[i] != '0') {
				match = false;
			}
		}
		if (match) {
			printf("%c", output[5]);
			chars++;
		}
		salt++;
	}
	printf("\n");
	return 0;
}
