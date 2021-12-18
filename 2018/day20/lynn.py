import sys
sys.setrecursionlimit(20000)

def parse(regex):
    stack = [[[]]]
    for c in regex.strip('^$'):
        if    c == '(':  stack.append([[]])    # new fork
        elif  c == '|':  stack[-1].append([])  # new tine
        elif  c == ')':  fork = stack.pop(); stack[-1][-1].append(fork)
        else:            stack[-1][-1].append(c)
    [[maze]] = stack
    return maze

def furthest(maze):
    if maze == []: return 0
    if isinstance(maze[0], str): return 1 + furthest(maze[1:])

    tines, *remainder = maze
    if tines[-1] == []: # dead end
        assert len(tines) == 2
        return max(furthest(tines[0]) // 2, furthest(remainder))
    else:
        assert remainder == []
        return max(furthest(t) for t in tines)

while 1: print(furthest(parse(input())))
