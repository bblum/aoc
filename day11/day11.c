// NB. May require "ulimit -s unlimited".

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "array_list.h"

#if 1
#define ELTS 2
#define FLOORS 4
struct state { int elevator; int generators[ELTS]; int chips[ELTS]; };
struct state initial_state = { .elevator = 0, .generators = {1,2}, .chips = {0,0} };

struct memo { bool visited; bool computed; int result; };
struct memo states[FLOORS][FLOORS][FLOORS][FLOORS][FLOORS];

#define MEMO(s) (&(states \
		  [(s)->elevator] \
		  [(s)->generators[0]] \
		  [(s)->generators[1]] \
		  [(s)->chips[0]] \
		  [(s)->chips[1]]))
#else
#define ELTS 5
#define FLOORS 4

struct state { int elevator; int generators[ELTS]; int chips[ELTS]; };
struct state initial_state = { .elevator = 0, .generators = {0,0,0,0,0}, .chips = {1,0,1,0,0} };

struct memo { bool visited; bool computed; int result; };
struct memo states[FLOORS]
		[FLOORS][FLOORS][FLOORS][FLOORS][FLOORS]
		[FLOORS][FLOORS][FLOORS][FLOORS][FLOORS];

#define MEMO(s) (&(states \
		  [(s)->elevator] \
		  [(s)->generators[0]] \
		  [(s)->generators[1]] \
		  [(s)->generators[2]] \
		  [(s)->generators[3]] \
		  [(s)->generators[4]] \
		  [(s)->chips[0]] \
		  [(s)->chips[1]] \
		  [(s)->chips[2]] \
		  [(s)->chips[3]] \
		  [(s)->chips[4]]))
#endif

void print_state(struct state *state, char *buf, int buflen)
{
	int pos = snprintf(buf, buflen, "%d", state->elevator);
	for (int i = 0; i < ELTS; i++) {
		pos += snprintf(buf+pos, buflen-pos, "%d", state->generators[i]);
		pos += snprintf(buf+pos, buflen-pos, "%d", state->chips[i]);
	}
}

bool goal_state(struct state *state) {
	if (state->elevator != FLOORS-1) return false;
	for (int i = 0; i < ELTS; i++)
		if (state->generators[i] != FLOORS-1 || state->chips[i] != FLOORS-1)
			return false;
	return true;
}

bool illegal_state(struct state *state) {
	for (int i = 0; i < ELTS; i++)
		if (state->chips[i] != state->generators[i])
			for (int j = 0; j < ELTS; j++)
				if (i != j && state->chips[i] == state->generators[j])
					return true;
	return false;
}

struct move { int to_floor; bool bring_generator[ELTS]; bool bring_chip[ELTS]; };
typedef ARRAY_LIST(struct move) movelist_t;

void get_moves(struct state *state, movelist_t *moves)
{
	int items_on_floor = 0;
	for (int i = 0; i < ELTS; i++) {
		if (state->elevator == state->generators[i]) items_on_floor++;
		if (state->elevator == state->chips[i])      items_on_floor++;
	}

	void add_moves(int to_floor, int num_items_to_bring)
	{
		for (int subset = 0; subset < (1 << items_on_floor); subset++) {
			if (__builtin_popcount(subset) != num_items_to_bring) continue;
			struct move move = { .to_floor = to_floor };
			for (int i = 0, item_id = 0; i < ELTS; i++) {
				if (state->elevator == state->generators[i])
					move.bring_generator[i] = ((subset >> item_id++) & 1) != 0;
				if (state->elevator == state->chips[i])
					move.bring_chip[i] = ((subset >> item_id++) & 1) != 0;
			}
			ARRAY_LIST_APPEND(moves, move);
		}
	}

	// prefer going up to going down
	if (state->elevator != FLOORS - 1) {
		// prefer taking more items up to fewer
		add_moves(state->elevator + 1, 2);
		add_moves(state->elevator + 1, 1);
	}
	if (state->elevator != 0) {
		// prefer taking fewer items down to more
		add_moves(state->elevator - 1, 1);
		add_moves(state->elevator - 1, 2);
	}
}

void apply_move(struct state *state, struct move *move, struct state *result)
{
	memcpy(result, state, sizeof(*state));
	for (int i = 0; i < ELTS; i++) {
		if (move->bring_generator[i]) result->generators[i] = move->to_floor;
		if (move->bring_chip[i])      result->chips[i]      = move->to_floor;
	}
	result->elevator = move->to_floor;
}

#define UNSOLVABLE 32768
bool search (struct state *state, int *result)
{
	if (illegal_state(state)) return false;

	struct memo *memo = MEMO(state);

	if (memo->visited) {
		*result = memo->result;
		return memo->computed; // don't go in cycles
	}
	assert(!memo->visited && !memo->computed);
	memo->visited = true;
	if (goal_state(state)) {
		memo->result = *result = 0;
		return memo->computed = true;
	}

	static int nobes = 0;
	if ((++nobes % 1000000) == 0) { printf("searching %dth state\n", nobes); }

	// search
	int best = UNSOLVABLE;
	movelist_t moves;
	ARRAY_LIST_INIT(&moves, 4096);
	get_moves(state, &moves);
	int i; struct move *move;
	ARRAY_LIST_FOREACH(&moves, i, move) {
		struct state state2; int result2;
		apply_move(state, move, &state2);
		if (search(&state2, &result2) && result2 < best) best = result2;
	}

	// record result
	ARRAY_LIST_FREE(&moves);
	memo->result = *result = best + 1;
	return memo->computed = true;
}

int main(int argc, char **argv)
{
	int result;
	search(&initial_state, &result);
	printf("%d\n", result);
	return 0;
}
