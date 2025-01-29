// AoC 2024 Day 18

import std.algorithm;
import std.container;
import std.array;
import std.conv;
import std.format;
import std.math;
import std.range;
import std.stdint;
import std.stdio;
import std.traits;
import std.typecons;

alias Point = Tuple!(int64_t, int64_t);

pure Point add(immutable Point a, immutable Point b) {
    return Point(a[0] + b[0], a[1] + b[1]);
}

pure int64_t manhattan(immutable Point a, immutable Point b) {
    return abs(a[0] - b[0]) + abs(a[1] - b[1]);
}

enum Direction {
    UP, DOWN, LEFT, RIGHT
}

pure Point versor(immutable Direction direction) {
    final switch (direction) {
    case Direction.UP:
        return Point(0, -1);

    case Direction.DOWN:
        return Point(0, 1);

    case Direction.LEFT:
        return Point(-1, 0);

    case Direction.RIGHT:
        return Point(1, 0);
    }
}

pure Point move(immutable Point point, immutable Direction direction) {
    return point.add(direction.versor);
}

struct Dimensions {
    int64_t rows, cols;
}

immutable auto DEFAULT_DIMS = Dimensions(71, 71);
immutable size_t DEFAULT_ITERATIONS = 1024;

class BadInputException : Exception {
    this(string msg) {
        super(msg);
    }
}

class Grid {
    this(Dimensions dims) {
        this.dims = dims;
    }

    void dump(const Point[] path = null) const {
        bool[Point] pathSet;
        
        if (path !is null) {
            foreach (point; path) {
                pathSet[point] = true;
            }
        }

        foreach (j; 0 .. this.dims.rows) {
            foreach (i; 0 .. this.dims.cols) {
                if (Point(i, j) in pathSet) {
                    write("O");
                } else {
                    write(Point(i, j) in this ? "." : "#");
                }
            }
            writeln();
        }
    }

    @property
    bool empty() const {
        return this.points.empty;
    }

    bool mark(immutable Point p) {
        if (p !in this) {
            return false;
        }

        this.points[p] = true;

        return true;
    }

    bool opBinaryRight(string op: "in")(immutable Point point) const {
        return
            point[0] >= 0
         && point[0] < this.dims.rows
         && point[1] >= 0
         && point[1] < this.dims.cols
         && point !in this.points;
    }

private:
    immutable Dimensions dims;
    bool[Point] points;
}

Point[] neighbours(immutable Point current, const Grid grid) {
    return [EnumMembers!Direction]
        .map!((direction) => current.move(direction))
        .filter!((point) => point in grid)
        .array();
}

alias Heuristic = int64_t delegate(Grid, Point, Point);

Point[] reconstructPath(const Point[Point] cameFrom, immutable Point start, immutable Point goal) {
    Point[] totalPath = [];

    if (goal !in cameFrom) {
        return null;
    }

    Point current = goal;

    while (current != start) {
        totalPath ~= current;
        current = cameFrom[current];
    }

    totalPath ~= start;

    return totalPath.reverse;
}

Point[] aStar(Grid grid, immutable Point start, immutable Point goal, Heuristic h) {
    alias Entry = Tuple!(Point, int64_t);
    
    auto openSet = heapify!"a[1] > b[1]"(Array!Entry());
    openSet.insert(Entry(start, 0));
    
    int64_t[Point] scores = [start: 0];

    Point[Point] cameFrom;

    while (!openSet.empty()) {
        immutable auto current = openSet.front[0];
        openSet.popFront();

        if (current == goal) {
            return reconstructPath(cameFrom, start, goal);
        }

        foreach (neighbour; current.neighbours(grid)) {
            immutable auto tentativeScore = scores[current] + 1;
            if (tentativeScore < scores.get(neighbour, int64_t.max)) {
                // This path to neighbor is better than any previous one. Record it!
                cameFrom[neighbour] = current;
                scores[neighbour] = tentativeScore;

                immutable auto priority = tentativeScore + h(grid, neighbour, goal);
            
                openSet.insert(Entry(neighbour, priority));
            }
        }
    }

    // no result found
    return null;
}

Point[] parseInput(string path) {
    auto file = File(path, "r");
    scope(exit) file.close();

    return file
        .byLine()
        .enumerate()
        .map!((item) {
            auto line = item[0] + 1;
            auto parts = item[1].split(",");
            if (parts.length != 2) {
                throw new BadInputException(format("at %s:%d: malformed point expression", path, line));
            }

            auto x = parts[0].to!int64_t;
            auto y = parts[1].to!int64_t;

            return Point(x, y);
        }).array();
}

struct Result {
    uint64_t part1;
    Point part2;
}

Nullable!Result solve(Point[] points, Dimensions dims, size_t iterations, Point start, Point goal) {
    auto grid = new Grid(dims);

    foreach (point; points.take(iterations)) {
        grid.mark(point);
    }
    
    immutable Heuristic heuristic = (grid, current, dest) => manhattan(current, dest);

    const auto path = aStar(grid, start, goal, heuristic);

    if (path is null) {
        return Nullable!Result.init;
    }

    grid.dump(path);

    auto part1 = path.length > 0 ? path.length - 1 : 0;

    foreach (point; points.drop(iterations)) {
        grid.mark(point);

        const auto npath = aStar(grid, start, goal, heuristic);
        if (npath is null) {
            return Result(part1, point).nullable;
        }

        writefln("\nafter %s:", point);
        grid.dump(npath);
    }

    return Nullable!Result.init;
}
    
int main(string[] args) {
    Dimensions dims = DEFAULT_DIMS;

    string fname = null;

    size_t iterations = DEFAULT_ITERATIONS;

    switch (args.length) {
    case 5:
        iterations = args[4].to!size_t;
        goto case;
    
    case 4:
        dims.cols = args[3].to!int64_t + 1;
        goto case;

    case 3:
        dims.rows = args[2].to!int64_t + 1;
        goto case;

    case 2:
        fname = args[1];
        break;

    default:
        writefln("Usage: %s <name>", args[0]);

        return 2;
    }

    immutable auto start = Point(0, 0);
    immutable auto goal = Point(dims.rows - 1, dims.cols - 1);

    Point[] points;
    
    try {
        points = parseInput(args[1]);
    } catch (Exception e) {
        writefln("error: %s", e.msg);

        return 1;
    }
    
    immutable auto solution = solve(points, dims, iterations, start, goal);

    if (solution.isNull) {
        writefln("error: no solution found");

        return 1;
    }

    writefln("part1: %d", solution.get.part1);

    immutable auto part2 = solution.get.part2;
    writefln("part2: %d,%d", part2[0], part2[1]);

    return 0;
}
