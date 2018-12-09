#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct nobe { long val; struct nobe *prev, *next; };

void solve(long elves, long marbs) {
	struct nobe zero = { .val = 0, .next = &zero, .prev = &zero }, *cur = &zero;
	long score[elves], lo, hi = 0;
	memset(score, 0, sizeof score);
	for (long i = 1; i <= marbs; i++) {
		if (i % 23) {
			struct nobe *n = malloc(sizeof *n);
			n->val = i;
			n->next = cur->next->next;
			n->prev = cur->next;
			cur = cur->next->next = cur->next->next->prev = n;
		} else {
			struct nobe *n = cur;
			for (int j = 0; j < 7; j++) n = n->prev;
			n->next->prev = n->prev;
			cur = n->prev->next = n->next;
			if ((lo = score[i % elves] += i + n->val) > hi) hi = lo;
		}
	}
	printf("%lu\n", hi);
}

void main() { solve(418, 71339); solve(418, 7133900); }
