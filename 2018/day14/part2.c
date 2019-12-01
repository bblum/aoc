#include <stdbool.h>
#include <stdio.h>
#include "array_list.h"

ARRAY_LIST(int) nobes;

bool done(int n, bool bss_ol_bar) {
	if (n < 6) return false;
	if (bss_ol_bar && done(n-1, false)) return true;
	if (*ARRAY_LIST_GET(&nobes, n-1) != 1) return false;
	if (*ARRAY_LIST_GET(&nobes, n-2) != 0) return false;
	if (*ARRAY_LIST_GET(&nobes, n-3) != 2) return false;
	if (*ARRAY_LIST_GET(&nobes, n-4) != 0) return false;
	if (*ARRAY_LIST_GET(&nobes, n-5) != 1) return false;
	if (*ARRAY_LIST_GET(&nobes, n-6) != 1) return false;
	printf("%d\n", n - 6);
	return true;
}

void main()
{
	ARRAY_LIST_INIT(&nobes, 8);
	ARRAY_LIST_APPEND(&nobes, 3);
	ARRAY_LIST_APPEND(&nobes, 7);
	int elf1 = 0, elf2 = 1, n = 2;
	while (!done(n, true)) {
		int nobe1 = *ARRAY_LIST_GET(&nobes, elf1);
		int nobe2 = *ARRAY_LIST_GET(&nobes, elf2);
		if (nobe1 + nobe2 > 9) {
			ARRAY_LIST_APPEND(&nobes, (nobe1 + nobe2) / 10);
			n++;
		}
		ARRAY_LIST_APPEND(&nobes, (nobe1 + nobe2) % 10);
		n++;
		elf1 = (elf1 + 1 + nobe1) % n;
		elf2 = (elf2 + 1 + nobe2) % n;
	}
}
