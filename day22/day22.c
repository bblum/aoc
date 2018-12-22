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
#define TSZ 20
#else
int depth = 9465;

#define TARG_Y 704
#define TARG_X 13
#define TSZ 4096
#endif

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
	if (table[y][x].set) {
		return table[y][x].value;
	} else {
		table[y][x].value = memo_geo(y,x);
		table[y][x].set = 1;
		return table[y][x].value;
	}
}
int ero(int y, int x)
{
	int result = (geo(y,x) + depth) % 20183;
	//printf("ero %d,%d = %d\n", x, y, result);
	return result;
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

struct shit { unsigned int distance; bool clim; bool torch; };
struct shit path[TSZ][TSZ];

struct coord { int y, x; };
typedef ARRAY_LIST(struct coord) list_t;
list_t todo1;
list_t todo2;

#define EQUIP 7
#define PUT_AWAY_IF_HELD(x) do { if (x)  { x = false; distance += EQUIP; } } while (0)
#define GET_IF_NOT_HELD(x)  do { if (!x) { x = true;  distance += EQUIP; } } while (0)

void consider(int cur_y, int cur_x, int y, int x, list_t *todo_now, unsigned int target_best)
{
	if (y < 0 || x < 0 || y == TSZ || x == TSZ) {
		return;
	}
	int cur_type = TYPE(cur_y, cur_x);
	int new_type = TYPE(y, x);
	unsigned int distance = path[cur_y][cur_x].distance + 1;
	bool clim    = path[cur_y][cur_x].clim;
	bool torch   = path[cur_y][cur_x].torch;
	bool oldclim = clim;
	bool oldtorch = torch;
	if (cur_type == 0) { // rock
		assert(clim || torch);
		if (new_type == 1) {        // -> wet
			PUT_AWAY_IF_HELD(torch);
		} else if (new_type == 2) { // -> nar
			PUT_AWAY_IF_HELD(clim);
		}
	} else if (cur_type == 1) { // wet
		assert(!torch);
		if (new_type == 0) {        // -> rock
			GET_IF_NOT_HELD(clim);
		} else if (new_type == 2) { // -> nar
			PUT_AWAY_IF_HELD(clim);
		}
	} else if (cur_type == 2) { // nar
		assert(!clim);
		if (new_type == 0) {        // -> rock
			GET_IF_NOT_HELD(torch);
		} else if (new_type == 1) { // -> wet
			// put away torch if held
			PUT_AWAY_IF_HELD(torch);
		}
	}
	//printf("(%d,%d) type %d, clim=%d tor=%d, -> "
	//       "(%d,%d) type %d, clim=%d tor=%d, distance %d\n",
	//       cur_y, cur_x, cur_type, oldclim, oldtorch,
	//       y,     x,     new_type, clim,    torch, distance);
	if (distance < path[y][x].distance && distance < target_best) {
		// record it in shit, IF its better
		// add y x to todo-later, IF it was better
		path[y][x].distance = distance;
		path[y][x].clim = clim;
		path[y][x].torch = torch;
		struct coord c = { .y = y, .x = x };
		ARRAY_LIST_APPEND(todo_now, c);
		//printf("explored (%d,%d), best path %d\n", y, x, distance);
	}
}
void part2(bool start_clim, bool start_torch)
{
	for (int y = 0; y < TSZ; y++) {
		for (int x = 0; x < TSZ; x++) {
			path[y][x].distance = (unsigned int)(-1);
		}
	}
	unsigned int target_best = (unsigned int)(-1);
	ARRAY_LIST_INIT(&todo1, TSZ);
	ARRAY_LIST_INIT(&todo2, TSZ);
	list_t *todo_now = &todo1;
	struct coord c = { .y = 0, .x = 0 };
	ARRAY_LIST_APPEND(todo_now, c);
	path[0][0].distance = 0;
	path[0][0].clim = start_clim;
	path[0][0].torch = start_torch;

	while (ARRAY_LIST_SIZE(todo_now) > 0) {
		// find todo coordinate closest to the origin to explore next
		unsigned int closest_coord = (unsigned int)(-1);
		unsigned int closest_index = (unsigned int)(-1);
		for (int i = 0; i < ARRAY_LIST_SIZE(todo_now); i++) {
			unsigned int this_coord = ARRAY_LIST_GET(todo_now, i)->y +
			                          ARRAY_LIST_GET(todo_now, i)->x;
			if (this_coord < closest_coord) {
				closest_coord = this_coord;
				closest_index = i;
			}
		}
		assert(closest_index != (unsigned int)(-1));
		// pop it out of the queue
		int cur_y = ARRAY_LIST_GET(todo_now, closest_index)->y;
		int cur_x = ARRAY_LIST_GET(todo_now, closest_index)->x;
		ARRAY_LIST_REMOVE_SWAP(todo_now, closest_index);
		// check if it's the goal zone
		if (cur_y == TARG_Y && cur_x == TARG_X) {
			if (target_best > path[cur_y][cur_x].distance) {
				target_best = path[cur_y][cur_x].distance;
				printf("new best: %d\n", target_best);
			}
			continue; // no point making extra loops
		}
		// consider its nbrs in each direction for future todoing
		consider(cur_y, cur_x, cur_y+1, cur_x, todo_now, target_best);
		consider(cur_y, cur_x, cur_y, cur_x+1, todo_now, target_best);
		consider(cur_y, cur_x, cur_y-1, cur_x, todo_now, target_best);
		consider(cur_y, cur_x, cur_y, cur_x-1, todo_now, target_best);
	}
}

void main() { part1(); part2(true, true); part2(true, false); part2(false, true); }
