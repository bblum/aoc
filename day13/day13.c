#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct { int dist; bool visited; } graph[1024][1024];
struct nobe { int x, y; struct nobe *next; };

#define OPEN(x,y) ((__builtin_popcount(x*x + 3*x + 2*x*y + y + y*y + 1362) & 1) == 0)

int distinct_locs = 0;

void fnbr(struct nobe **tail, struct nobe *pos, int newx, int newy) {
	if (newx < 0 || newy < 0 || !OPEN(newx, newy)) return;
	if (graph[newx][newy].visited) {
		if (graph[newx][newy].dist <= graph[pos->x][pos->y].dist + 1)
			return;
	} else if (graph[pos->x][pos->y].dist < 50) {
		distinct_locs++;
	}
	struct nobe *newnobe = malloc(sizeof(struct nobe));
	newnobe->x = newx;
	newnobe->y = newy;
	newnobe->next = NULL;
	*tail = (*tail)->next = newnobe;
	graph[newx][newy].dist = graph[pos->x][pos->y].dist + 1;
	graph[newx][newy].visited = true;
}

void main() {
	struct nobe begin = { .x = 1, .y = 1, .next = NULL, };
	for (struct nobe *head = &begin, *tail = &begin; head != NULL; head = head->next) {
		if (head->x == 31 && head->y == 39) {
			printf("part1: %d\n", graph[head->x][head->y].dist);
			printf("part2: %d\n", distinct_locs);
			return;
		}

		fnbr(&tail, head, head->x + 1, head->y);
		fnbr(&tail, head, head->x - 1, head->y);
		fnbr(&tail, head, head->x, head->y - 1);
		fnbr(&tail, head, head->x, head->y + 1);
	}
}
