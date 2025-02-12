# AoC 2024 Day 23

import std/[algorithm, options, os, sequtils, sets, sugar, strutils, strformat, syncio, tables]

type Graph = Table[string, HashSet[string]]

type Group3 = array[3, string]

iterator groups(g: Graph): Group3 =
    var visited = HashSet[Group3]()

    for node, neighbours in g:
        for n1 in neighbours:
            for n2 in neighbours:
                if n1 == n2:
                    continue

                if n2 in g[n1]:
                    # n1 and n2 are connected, so we have a triangle
                    var group = [node, n1, n2]

                    sort group

                    if group notin visited:
                        visited.incl group
                        yield group

iterator candidates(g: Graph): Group3 =
    for group in groups(g):
        for node in group:
            if node.startsWith 't':
                yield group
                break

proc commonNeighbours(g: Graph, nodes: HashSet[string]): HashSet[string] =
    var common : Option[HashSet[string]] = none(HashSet[string])

    for node in nodes:
        if common.isSome:
            var intersection = common.get * g[node]

            if intersection.len == 0:
                return intersection # we're left with an empty set, no need to continue
            else:
                common = some(intersection)
        else:
            common = some(g[node])

    return common.get

# after generating the minimum groups of size 2, merge them together until no more merges are possible
proc merge(g: Graph, links: HashSet[HashSet[string]]): HashSet[HashSet[string]] =
    var current: HashSet[HashSet[system.string]] = links
    var next: HashSet[HashSet[string]]

    var dirty = true

    while dirty:
        next = initHashSet[HashSet[string]]()
        dirty = false

        for area in current:
            let neighbours = commonNeighbours(g, area)

            if neighbours.len > 0:
                dirty = true

                for neighbour in neighbours:
                    next.incl(area + [neighbour].toHashSet)
            else:
                next.incl area

        current = next

    return current

proc pairs(g: Graph): HashSet[HashSet[string]] =
    return collect(initHashSet()):
        for node, neighbours in g:
            for neighbour in neighbours:
                {[node, neighbour].toHashSet}

proc bestGroup(g: Graph): seq[string] =
    let groups = merge(g, pairs(g))
    var best = HashSet[string]()

    for group in groups:
        if group.len > best.len:
            best = group

    return best.toSeq.sorted

proc parseInput(fname: string): Graph =
    var connections = initTable[string, HashSet[string]]()

    for line in lines fname:
        let parts = collect(newSeq):
            for s in line.split("-"):
                s.strip()

        if parts.len() != 2:
            raise newException(ValueError, "invalid input")

        let a = parts[0]
        let b = parts[1]

        for (src, dest) in [(a, b), (b, a)]:
            if dest in connections:
                connections[dest].incl src
            else:
                connections[dest] = [src].toHashSet
    
    return connections

proc part1(g: Graph): int =
    var count = 0

    for group in candidates(g):
        count.inc

    return count

proc part2(g: Graph): string =
    return bestGroup(g).join(",")

when isMainModule:
    let args = commandLineParams()

    if args.len() != 1:
        # print on stderr
        stderr.writeLine &"error: wrong number of arguments\nusage: {getAppFilename()} INPUT"
        quit 2

    let connections = parseInput args[0]

    echo "part 1: ", part1(connections)
    echo "part 2: ", part2(connections)
