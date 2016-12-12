#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PART 2

/**** Test input ****/
#if (PART == 0)
#define ELTS 2
#define FLOORS 4
#define DIMENSIONS [FLOORS][FLOORS][FLOORS][FLOORS][FLOORS]

#define G_NOBE(e,g,c) (&(graph [(e)][(g)[0]][(g)[1]][(c)[0]][(c)[1]]))

#define INIT_NOBE (&graph[0][1][2][0][0])
#define INIT_GENS {1,2}
#define INIT_CHIPS {0,0}
#define GOAL_NOBE (&graph[3][3][3][3][3])
#define GOAL_GENS {3,3}

/**** Part 1 ****/
#elif (PART == 1)
#define ELTS 5
#define FLOORS 4
#define DIMENSIONS [FLOORS] \
		[FLOORS][FLOORS][FLOORS][FLOORS][FLOORS] \
		[FLOORS][FLOORS][FLOORS][FLOORS][FLOORS]

#define G_NOBE(e,g,c) (&(graph [(e)] \
                         [(g)[0]][(g)[1]][(g)[2]][(g)[3]][(g)[4]] \
                         [(c)[0]][(c)[1]][(c)[2]][(c)[3]][(c)[4]]))

#define INIT_NOBE (&graph[0] [0][0][0][0][0] [1][0][1][0][0])
#define INIT_GENS {0,0,0,0,0}
#define INIT_CHIPS {1,0,1,0,0}
#define GOAL_NOBE (&graph[3] [3][3][3][3][3] [3][3][3][3][3])
#define GOAL_GENS {3,3,3,3,3}

/**** Part 2 ****/
#elif (PART == 2)
#define ELTS 7
#define FLOORS 4

#define DIMENSIONS [FLOORS] \
		[FLOORS][FLOORS][FLOORS][FLOORS][FLOORS][FLOORS][FLOORS] \
		[FLOORS][FLOORS][FLOORS][FLOORS][FLOORS][FLOORS][FLOORS]

#define G_NOBE(e,g,c) (&(graph [(e)] \
                         [(g)[0]][(g)[1]][(g)[2]][(g)[3]][(g)[4]][(g)[5]][(g)[6]] \
                         [(c)[0]][(c)[1]][(c)[2]][(c)[3]][(c)[4]][(c)[5]][(c)[6]]))

#define INIT_NOBE (&graph[0] [0][0][0][0][0][0][0] [1][0][1][0][0][0][0])
#define INIT_GENS {0,0,0,0,0,0,0}
#define INIT_CHIPS {1,0,1,0,0,0,0}
#define GOAL_NOBE (&graph[3] [3][3][3][3][3][3][3] [3][3][3][3][3][3][3])
#define GOAL_GENS {3,3,3,3,3,3,3}
#endif

#define G(s) G_NOBE((s)->elevator, (s)->gens, (s)->chips)
#define GOAL_CHIPS GOAL_GENS

struct state {
	char elevator;
	char gens[ELTS];
	char chips[ELTS];
	struct state *next;
};

struct move { int to_floor; bool bring_gen[ELTS]; bool bring_chip[ELTS]; };

struct state_nobe {
	char distance;
	char visited; // 0: unvisited; 1: visited fwds; 2: visited reverse
};

struct state_nobe graph DIMENSIONS;

bool illegal_state(struct state *state) {
	for (int i = 0; i < ELTS; i++)
		if (state->chips[i] != state->gens[i])
			for (int j = 0; j < ELTS; j++)
				if (i != j && state->chips[i] == state->gens[j])
					return true;
	return false;
}

void foreach_move(struct state *state, void (*f)(struct move *arg))
{
	int items_on_floor = 0;
	for (int i = 0; i < ELTS; i++) {
		if (state->elevator == state->gens[i])  items_on_floor++;
		if (state->elevator == state->chips[i]) items_on_floor++;
	}

	for (int to_floor = 0; to_floor < FLOORS; to_floor++) {
		if (to_floor != state->elevator - 1 &&
		    to_floor != state->elevator + 1) continue;
		for (int subset = 1; subset < (1 << items_on_floor); subset++) {
			if (__builtin_popcount(subset) > 2) continue;
			struct move move = { .to_floor = to_floor };
			for (int i = 0, item_id = 0; i < ELTS; i++) {
				if (state->elevator == state->gens[i])
					move.bring_gen[i] =
						((subset >> item_id++) & 1) != 0;
				if (state->elevator == state->chips[i])
					move.bring_chip[i] =
						((subset >> item_id++) & 1) != 0;
			}
			f(&move);
		}
	}
}

struct state *apply_move(struct state *state, struct move *m)
{
	struct state s2;
	for (int i = 0; i < ELTS; i++) {
		s2.gens[i]  = m->bring_gen[i]  ? m->to_floor : state->gens[i];
		s2.chips[i] = m->bring_chip[i] ? m->to_floor : state->chips[i];
	}
	s2.elevator = m->to_floor;
	s2.next = NULL;

	if (illegal_state(&s2)) {
		return NULL;
	} else if (G(state)->visited + G(&s2)->visited == 3) {
		printf("%d\n", G(state)->distance + G(&s2)->distance + 1);
		exit(0);
	} else if (G(&s2)->visited != 0) {
		assert(G(&s2)->distance <= G(state)->distance + 1);
		return NULL;
	} else {
		struct state *result = malloc(sizeof(struct state));
		memcpy(result, &s2, sizeof(s2));

		G(result)->distance = G(state)->distance + 1;
		G(result)->visited  = G(state)->visited;
		return result;
	}
}

int main(int argc, char **argv)
{
	struct state goal = { .elevator = 3, .gens = GOAL_GENS,
	                      .chips = GOAL_CHIPS, .next = NULL };
	GOAL_NOBE->distance = 0;
	GOAL_NOBE->visited = 2;
	struct state init = { .elevator = 0, .gens = INIT_GENS,
	                      .chips = INIT_CHIPS, .next = &goal };
	INIT_NOBE->distance = 0;
	INIT_NOBE->visited = 1;

	for (struct state *head = &init, *tail = &goal; head != NULL;) {
		struct state *s = head;
		void fnbrs(struct move *m) {
			struct state *s2;
			if ((s2 = apply_move(s, m)) != NULL) {
				tail = tail->next = s2;
			}
		}
		foreach_move(s, fnbrs);
		head = head->next;
		if (s != &init && s != &goal) free(s);
	}
	return -1;
}
