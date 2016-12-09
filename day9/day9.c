#include <stdio.h>
#include <string.h>

unsigned long decompress (char *s, int input_length)
{
	unsigned long output_length = 0;
	for (int i = 0; i < input_length;) {
		int ret, num_chars, repeat_count;
		if ((ret = sscanf(&s[i], "(%dx%d)", &num_chars,&repeat_count)) != 0) {
			i = strstr(&s[i], ")") - s + 1;

			// Part 1
			// output_length += num_chars * repeat_count;
			// Part 2
			output_length += decompress(&s[i], num_chars) * repeat_count;

			i += num_chars;
		} else {
			output_length++;
			i++;
		}
	}
	return output_length;
}
int main(int argc, char **argv)
{
	printf("%lu\n", decompress(argv[1], strlen(argv[1])));
}
