#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "array_list.h"
#include "variable_queue.h"

#if 0
int depth = 510;
#define TARG_Y 10
#define TARG_X 10
#else
#if 1
// my input
int depth = 9465;
#define TARG_Y 704
#define TARG_X 13
#else
// asdf input
int depth = 11394;
#define TARG_Y 701
#define TARG_X 7
#endif
#endif

#define TSZ 4096

struct geos { bool set; int value; };
struct geos table[TSZ][TSZ];

int geo(int y, int x);
int ero(int y, int x);
int memo_geo(int y, int x)
{
	if (y == 0) {
		return x*16807;
	} else if (x == 0) {
		return y*48271;
	} else if (y == TARG_Y && x == TARG_X) {
		return 0;
	} else {
		return ero(y-1, x) * ero(y, x-1);
	}
}
int geo(int y, int x)
{
	if (!table[y][x].set) {
		table[y][x].value = memo_geo(y,x);
		table[y][x].set = 1;
	}
	return table[y][x].value;
}
int ero(int y, int x)
{
	return (geo(y,x) + depth) % 20183;
}

#define TYPE(y,x) (ero((y),(x)) % 3)

void part1()
{
	int sum = 0;
	for (int y = 0; y <= TARG_Y; y++) {
		for (int x = 0; x <= TARG_X; x++) {
			sum += TYPE(y,x);
		}
	}
	printf("%d\n",sum);
}

unsigned int path[TSZ][TSZ][2][2];

struct coord { int y, x; bool clim, torch; };
typedef ARRAY_LIST(struct coord) list_t;
list_t todo;

#define EQUIP 7
#define PUT_AWAY_IF_HELD(x) do { if (x)  { x = false; distance += EQUIP; } } while (0)
#define GET_IF_NOT_HELD(x)  do { if (!x) { x = true;  distance += EQUIP; } } while (0)
#define ATOMICALLY_MEANS_QUICKLY(wanted,banned) \
	do { if (!wanted || banned) { wanted = true; banned = false; distance += EQUIP; } } while (0)
#define ADD_TODO(y,x,clim,torch) \
	do { struct coord __c = { (y), (x), (clim), (torch) }; ARRAY_LIST_APPEND(&todo, __c); } while (0)

void consider(struct coord *nobe, int y, int x, unsigned int target_best)
{
	assert(y < TSZ && x < TSZ);
	if (y < 0 || x < 0) {
		return;
	}
	int cur_type = TYPE(nobe->y, nobe->x);
	int new_type = TYPE(y, x);
	bool clim = nobe->clim, torch = nobe->torch;
	unsigned int distance = path[nobe->y][nobe->x][clim][torch] + 1;
	if (cur_type == 0) { // rock
		assert(clim || torch);
		if (new_type == 1) ATOMICALLY_MEANS_QUICKLY(clim, torch); // -> wet
		if (new_type == 2) ATOMICALLY_MEANS_QUICKLY(torch, clim); // -> nar
	} else if (cur_type == 1) { // wet
		assert(!torch);
		if (new_type == 0) GET_IF_NOT_HELD(clim); // -> rock
		if (new_type == 2) PUT_AWAY_IF_HELD(clim); // -> nar
	} else if (cur_type == 2) { // nar
		assert(!clim);
		if (new_type == 0) GET_IF_NOT_HELD(torch); // -> rock
		if (new_type == 1) PUT_AWAY_IF_HELD(torch); // -> wet
	}
	if (distance < path[y][x][(int)clim][(int)torch] && distance < target_best) {
		path[y][x][(int)clim][(int)torch] = distance;
		ADD_TODO(y, x, clim, torch);
	}
}
void part2()
{
	for (int y = 0; y < TSZ; y++) {
		for (int x = 0; x < TSZ; x++) {
			path[y][x][0][0] = UINT_MAX;
			path[y][x][0][1] = UINT_MAX;
			path[y][x][1][0] = UINT_MAX;
			path[y][x][1][1] = UINT_MAX;
		}
	}
	unsigned int target_best = UINT_MAX;
	ARRAY_LIST_INIT(&todo, TSZ);
	ADD_TODO(0, 0, false, true); // start w torch only, you idiot
	path[0][0][0][1] = 0;

	while (ARRAY_LIST_SIZE(&todo) > 0) {
		// find todo coordinate closest to the origin to explore next
		unsigned int closest_coord = UINT_MAX;
		unsigned int closest_index = UINT_MAX;
		for (int i = 0; i < ARRAY_LIST_SIZE(&todo); i++) {
			unsigned int yx = ARRAY_LIST_GET(&todo, i)->y + ARRAY_LIST_GET(&todo, i)->x;
			if (yx < closest_coord) {
				closest_coord = yx;
				closest_index = i;
			}
		}
		// pop it out of the queue
		struct coord nobe = *ARRAY_LIST_GET(&todo, closest_index);
		ARRAY_LIST_REMOVE_SWAP(&todo, closest_index);
		// check if it's the goal zone
		if (nobe.y == TARG_Y && nobe.x == TARG_X) {
			unsigned int this_target = path[nobe.y][nobe.x][nobe.clim][nobe.torch];
			if (!nobe.torch) this_target += EQUIP;
			if (target_best > this_target) target_best = this_target;
		}
		// consider its nbrs in each direction for future todoing
		consider(&nobe, nobe.y+1, nobe.x, target_best);
		consider(&nobe, nobe.y, nobe.x+1, target_best);
		consider(&nobe, nobe.y-1, nobe.x, target_best);
		consider(&nobe, nobe.y, nobe.x-1, target_best);
	}
	printf("%d\n", target_best);
}

void main() { part1(); part2(); }
