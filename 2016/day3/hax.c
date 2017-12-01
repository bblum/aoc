#include <stdio.h>

void process(int max, int x, int y)
{
	if (x + y > max) printf("yes\n");
	else printf("no\n");
}
void process_triangle(int a, int b, int c)
{
	if (a >= b && a >= c) process(a,b,c);
	else if (b >= a && b >= c) process(b,a,c);
	else if (c >= a && c >= b) process(c,a,b);
}

int main(int argc, char **argv[])
{
	int a = atoi(argv[1]);
	int b = atoi(argv[2]);
	int c = atoi(argv[3]);

	int d = atoi(argv[4]);
	int e = atoi(argv[5]);
	int f = atoi(argv[6]);

	int g = atoi(argv[7]);
	int h = atoi(argv[8]);
	int i = atoi(argv[9]);

	process_triangle(a,d,g);
	process_triangle(b,e,h);
	process_triangle(c,f,i);

	return 0;
}
