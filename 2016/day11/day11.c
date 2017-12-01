#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PART 2
#define FLOORS 4
#define D2 [FLOORS][FLOORS]

#if (PART == 0)
#define ELEMENTS 2
#define DIMENSIONS [FLOORS] D2 D2
#define LOOKUP(e,g,c) (&(graph [(e)][g[0]][g[1]][c[0]][c[1]]))
#define INIT_NOBE (&graph[0][1][2][0][0])
#define INIT_GENS {1,2}
#define INIT_CHIPS {0,0}
#define GOAL_NOBE (&graph[3][3][3][3][3])
#define GOAL_ITEMS {3,3}

#elif (PART == 1)
#define ELEMENTS 5
#define DIMENSIONS [FLOORS] D2 D2 D2 D2 D2
#define LOOKUP(e,g,c) (&(graph [e] [g[0]][g[1]][g[2]][g[3]][g[4]] \
                                   [c[0]][c[1]][c[2]][c[3]][c[4]]))
#define INIT_NOBE (&graph[0] [0][0][0][0][0] [1][0][1][0][0])
#define INIT_GENS {0,0,0,0,0}
#define INIT_CHIPS {1,0,1,0,0}
#define GOAL_NOBE (&graph[3] [3][3][3][3][3] [3][3][3][3][3])
#define GOAL_ITEMS {3,3,3,3,3}

#elif (PART == 2)
#define ELEMENTS 7
#define DIMENSIONS [FLOORS] D2 D2 D2 D2 D2 D2 D2
#define LOOKUP(e,g,c) (&(graph [e] [g[0]][g[1]][g[2]][g[3]][g[4]][g[5]][g[6]] \
                                   [c[0]][c[1]][c[2]][c[3]][c[4]][c[5]][c[6]]))
#define INIT_NOBE (&graph[0] [0][0][0][0][0][0][0] [1][0][1][0][0][0][0])
#define INIT_GENS {0,0,0,0,0,0,0}
#define INIT_CHIPS {1,0,1,0,0,0,0}
#define GOAL_NOBE (&graph[3] [3][3][3][3][3][3][3] [3][3][3][3][3][3][3])
#define GOAL_ITEMS {3,3,3,3,3,3,3}
#endif

struct { char distance, visited; } graph DIMENSIONS;

struct state { char elevator, gen[ELEMENTS], chip[ELEMENTS]; struct state *next; };
#define G(s) LOOKUP((s)->elevator, (s)->gen, (s)->chip)

struct move { char to_floor; bool bring_gen[ELEMENTS], bring_chip[ELEMENTS]; };

bool illegal_state(struct state *state) {
	for (int i = 0; i < ELEMENTS; i++)
		if (state->chip[i] != state->gen[i])
			for (int j = 0; j < ELEMENTS; j++)
				if (i != j && state->chip[i] == state->gen[j])
					return true;
	return false;
}

void foreach_move(struct state *state, void (*f)(struct move *)) {
	int num_items = 0;
	for (int i = 0; i < ELEMENTS; i++) {
		if (state->elevator == state->gen[i])  num_items++;
		if (state->elevator == state->chip[i]) num_items++;
	}

	for (int to_floor = 0; to_floor < FLOORS; to_floor++) {
		if (abs(to_floor - state->elevator) != 1) continue;
		for (int subset = 1; subset < (1 << num_items); subset++) {
			if (__builtin_popcount(subset) > 2) continue;
			struct move m = { .to_floor = to_floor };
			for (int i = 0, item = 0; i < ELEMENTS; i++) {
				if (state->elevator == state->gen[i])
					m.bring_gen[i]  = subset >> item++ & 1;
				if (state->elevator == state->chip[i])
					m.bring_chip[i] = subset >> item++ & 1;
			}
			f(&m);
		}
	}
}

struct state *apply_move(struct state *state, struct move *m) {
	struct state s2 = { .elevator = m->to_floor, .next = NULL };
	for (int i = 0; i < ELEMENTS; i++) {
		s2.gen[i]  = m->bring_gen[i]  ? m->to_floor : state->gen[i];
		s2.chip[i] = m->bring_chip[i] ? m->to_floor : state->chip[i];
	}

	if (illegal_state(&s2)) {
		return NULL;
	} else if (G(state)->visited + G(&s2)->visited == 3) {
		printf("%d\n", G(state)->distance + G(&s2)->distance + 1);
		exit(0);
	} else if (G(&s2)->visited != 0) {
		return NULL;
	} else {
		struct state *result = malloc(sizeof(struct state));
		memcpy(result, &s2, sizeof(s2));
		G(result)->distance = G(state)->distance + 1;
		G(result)->visited  = G(state)->visited;
		return result;
	}
}

void main() {
	struct state goal = { .elevator = 3, .gen = GOAL_ITEMS,
	                      .chip = GOAL_ITEMS, .next = NULL };
	GOAL_NOBE->distance = 0;
	GOAL_NOBE->visited = 2;
	struct state init = { .elevator = 0, .gen = INIT_GENS,
	                      .chip = INIT_CHIPS, .next = &goal };
	INIT_NOBE->distance = 0;
	INIT_NOBE->visited = 1;

	for (struct state *head = &init, *tail = &goal; head != NULL;) {
		struct state *s = head, *s2;
		foreach_move(s, ({ void fnbrs(struct move *m) {
			if ((s2 = apply_move(s, m)) != NULL)
				tail = tail->next = s2;
		} fnbrs; }));
		head = head->next;
		if (s != &init && s != &goal) free(s);
	}
}
