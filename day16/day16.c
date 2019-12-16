#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *phase0 = "59769638638635227792873839600619296161830243411826562620803755357641409702942441381982799297881659288888243793321154293102743325904757198668820213885307612900972273311499185929901117664387559657706110034992786489002400852438961738219627639830515185618184324995881914532256988843436511730932141380017180796681870256240757580454505096230610520430997536145341074585637105456401238209187118397046373589766408080120984817035699228422366952628344235542849850709181363703172334788744537357607446322903743644673800140770982283290068502972397970799328249132774293609700245065522290562319955768092155530250003587007804302344866598232236645453817273744027537630";

int pattern(int repeat_factor, int index) {
	// 0th thing is just base: [0,1,0,-1]
	int repeatlen = (repeat_factor + 1) * 4;
	// the +1 is equivalent to the `tail` in my haskell
	int offset = (index + 1) % repeatlen;
	if (offset < repeatlen / 4) {
		return 0;
	} else if (offset < repeatlen / 2) {
		return 1;
	} else if (offset < (3 * repeatlen) / 4) {
		return 0;
	} else {
		return -1;
	}
}

char *phase(char *input, int buflen, int phasenum) {
	char *output = malloc((sizeof(char) * buflen) + 1);
	assert(output);
	output[buflen] = 0;

	// TODO: fork threads for this outer loop
	for (int i_out = 0; i_out < buflen; i_out++) {
		long value = 0;
		for (int i_in = 0; i_in < buflen; i_in++) {
			long summand = pattern(i_out, i_in);
			long input_value = input[i_in] - '0';
			value += summand * input_value;
		}
		if (value < 0) { value = -value; }
		char output_char = (value % 10) + '0';
		output[i_out] = output_char;
		if ((i_out % 1000) == 999) {
			printf("phase %d digit %d computed\n", phasenum, i_out);
			exit(0);
		}
	}
	return output;
}

char *replicate_input(int factor, int orig_len) {
	char *result = malloc((sizeof(char) * orig_len * factor) + 1);
	assert(result);
	result[orig_len * factor] = 0;
	for (int i = 0; i < orig_len * factor; i++) {
		result[i] = phase0[i % orig_len];
	}
	return result;
}

int main() {
	int factor = 1;
	// int factor = 10000;
	int orig_len = strlen(phase0);
	int buflen = factor * orig_len;
	char *prev = replicate_input(factor, orig_len);
	assert(prev);
	for (int phasenum = 0; phasenum < 100; phasenum++) {
		char *next = phase(prev, buflen, phasenum);
		free(prev);
		prev = next;
	}
	printf("%s\n", prev);
	return 0;
}
