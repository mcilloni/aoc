// AoC 2024 Day 16

using GLib;
using Gee;

struct Dimensions {
    public uint rows;
    public uint cols;
}
// the plane is with x going down and y going right (row-column order)
enum Direction {
    UP = 0,
    RIGHT = 1,
    DOWN = 2,
    LEFT = 3,
}

const Direction DIRECTIONS[4] = { Direction.UP, Direction.RIGHT, Direction.DOWN, Direction.LEFT };

char direction_to_char(Direction dir) {
    switch (dir) {
    case Direction.UP:
        return '^';

    case Direction.RIGHT:
        return '>';

    case Direction.DOWN:
        return 'v';

    case Direction.LEFT:
        return '<';

    default:
        assert(false);
        return '\0';
    }
}

Direction flip(Direction dir) {
    return (dir + 2) % 4;
}

class Point : Object {
    public int x {
        get; private set;
    }

    public int y {
        get; private set;
    }

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Point add(Point other) requires(other != null) {
        return new Point(this.x + other.x, this.y + other.y);
    }

    public bool equal_to(Point other) {
        return other != null && this.x == other.x && this.y == other.y;
    }

    public uint hash() {
        return this.x * 31 + this.y;
    }

    public Point move(Direction dir) {
        Point versor = null;

        switch (dir) {
        case Direction.UP:
            versor = new Point(-1, 0);
            break;
        case Direction.DOWN:
            versor = new Point(1, 0);
            break;

        case Direction.LEFT:
            versor = new Point(0, -1);
            break;

        case Direction.RIGHT:
            versor = new Point(0, 1);
            break;
        }

        return this.add(versor);
    }

    public string to_string() {
        return @"($(this.x), $(this.y))";
    }
}

errordomain ParseError {
    DUPLICATE_END,
    DUPLICATE_START,
    INVALID_CHAR,
    MISSING_END,
    MISSING_START,
}

class Grid : Object {
    public class Iterator : Object {
        private Grid _grid;
        private uint _i = 0;
        private uint _j = 0;

        internal Iterator(Grid grid) {
            this._grid = grid;
        }

        public Point ? next_value() {
            var dims = this._grid.dimensions;

            while (true) {
                if (this._j >= dims.cols) {
                    this._j = 0;
                    ++this._i;
                }

                if (this._i >= dims.rows) {
                    return null;
                }

                var p = new Point((int) this._i, (int) this._j);

                ++this._j;

                if (p in this._grid.blocks) {
                    continue;
                }

                return p;
            }
        }
    }

    public HashSet<Point?> blocks {
        get; private set;
    }

    public Dimensions dimensions {
        get; private set;
    }

    public Point end {
        get; private set;
    }

    public Point start {
        get; private set;
    }

    private Grid() {}

    public static Grid from_file(string fpath) throws ConvertError, FileError, IOChannelError, ParseError requires(fpath != null) {
        Point start = null, end = null;
        var blocks = new HashSet<Point?> (
                                          (point) => point.hash(),
                                          (p1, p2) => p1.equal_to(p2)
        );

        int i = 0, j = 0;

        bool line_start = true;
        with(var file = new IOChannel.file(fpath, "r")) {
            unichar c = 0;
            while (file.read_unichar(out c) != IOStatus.EOF) {
                if (c.isspace()) {
                    if (c == '\n' && !line_start) {
                        line_start = true;
                        ++i;
                    }

                    continue;
                }

                if (line_start) {
                    j = 0;
                    line_start = false;
                }

                switch (c) {
                case '#' :
                    blocks.add(new Point(i, j));
                    break;

                case '.':
                    break;

                case 'E':
                    if (end != null) {
                        throw new ParseError.DUPLICATE_END("duplicate end");
                    }

                    end = new Point(i, j);
                    break;

                case 'S':
                    if (start != null) {
                        throw new ParseError.DUPLICATE_START("duplicate start");
                    }

                    start = new Point(i, j);
                    break;

                default:
                    throw new ParseError.INVALID_CHAR(@"invalid character '$(c)'");
                }

                ++j;
            }
        }

        return new Grid() {
                   blocks = blocks,
                   dimensions = { line_start? i : i + 1, j },
                   start = start,
                   end = end,
        };
    }

    public bool contains(Point p) {
        return this.is_hole(p);
    }

    public void dump(Gee.List<Step>? path = null, Gee.AbstractSet<Point>? points = null) {
        for (int i = 0; i < this.dimensions.rows; ++i) {
            for (int j = 0; j < this.dimensions.cols; ++j) {
                var p = new Point(i, j);

                if (p.equal_to(this.start)) {
                    stdout.putc('S');
                } else if (p.equal_to(this.end)) {
                    stdout.putc('E');
                } else if (p in this) {
                    if (path != null) {
                        var step = path.first_match((step) => step.point.equal_to(p));
                        if (step != null) {
                            stdout.putc(direction_to_char(step.dir));

                            continue;
                        }
                    }

                    if (points != null && p in points) {
                        stdout.putc('O');

                        continue;
                    }

                    stdout.putc('.');
                } else {
                    stdout.putc('#');
                }
            }

            stdout.putc('\n');
        }
    }

    public bool inside(Point p) {
        return p != null && p.x >= 0 && p.x < this.dimensions.rows && p.y >= 0 && p.y < this.dimensions.cols;
    }

    public bool is_hole(Point p) {
        return p != null && this.inside(p) && !this.is_wall(p);
    }

    public bool is_wall(Point p) {
        return p != null && p in this.blocks;
    }

    public Iterator iterator() {
        return new Iterator(this);
    }
}

Gee.List<Step?> reconstruct_path(HashMap<Step, Step> came_from, Point start, Step goal)
requires(came_from != null && start != null && goal != null)
{
    var total_path = new ArrayList<Step?> ();

    if (!came_from.has_key(goal)) {
        return total_path;
    }

    Step current = goal;

    while (current != null && !current.point.equal_to(start)) {
        total_path.add(current);
        current = came_from[current];
    }

    total_path.add(current);

    reverse(total_path);

    return total_path;
}

void reverse<T> (Gee.List<T> arr) {
    for (int i = 0; i < arr.size / 2; ++i) {
        var tmp = arr[i];
        arr[i] = arr[arr.size - i - 1];
        arr[arr.size - i - 1] = tmp;
    }
}

const int TURN_COST = 1000;
const int FORWARD_COST = 1;

class Step : Object {
    public Point point { get; private set; }
    public Direction dir { get; private set; }

    public Step(Point point, Direction dir) {
        this.point = point;
        this.dir = dir;
    }

    public Step ? advance(Grid grid) {
        Point next = this.point.move(this.dir);

        if (!grid.is_hole(next)) {
            return null;
        }

        return new Step(next, this.dir);
    }

    public bool equal_to(Step other) {
        return this.point.equal_to(other.point) && this.dir == other.dir;
    }

    public Step flip() {
        return new Step(this.point, global::flip(this.dir));
    }

    public uint hash() {
        return this.point.hash() * 31 + this.dir;
    }

    public string to_string() {
        return @"($(this.point), $(direction_to_char(this.dir)))";
    }
}

class Entry {
    public Step step { get; private set; }
    public int priority { get; private set; }

    public Point point {
        get {
            return this.step.point;
        }
    }

    public Entry(Step node, int priority) {
        this.step = node;
        this.priority = priority;
    }
}

V try_get<K, V> (AbstractMap<K, V> map, K key, V fallback) {
    return map.has_key(key) ? map[key] : fallback;
}

class DijkstraResult {
    public HashMap<Step, Step> came_from { get; private set; }
    public HashMap<Step, int> scores { get; private set; }

    public DijkstraResult(HashMap<Step, Step> came_from, HashMap<Step, int> scores) {
        this.came_from = came_from;
        this.scores = scores;
    }
}

// originally I planned on using A*, and I actually did solve part 1 with A*. However, I got stuck on part 2 badly,
// due to the fact that I guess you need a perfect heuristic to find all paths using A* (I dunno, I suck at pathfinding
// algorithms). So I switched to Dijkstra, which is easy to get to score all nodes in a graph. Thanks to some guy on reddit
// I then managed to correctly hit all the corner cases and finally find a solution for part 2
// TODO: study pathfinding algorithms and dynamic programming
DijkstraResult ? dijkstra(Grid grid, Step[] starting_steps = { new Step(grid.start, Direction.RIGHT) })
requires(grid != null)
{
    var open_set = new PriorityQueue<Entry> ((e1, e2) => e1.priority - e2.priority);
    var scores = new HashMap<Step, int> (
                                         (n) => n.hash(),
                                         (n1, n2) => n1.equal_to(n2)
    );

    foreach (var step in starting_steps) {
        open_set.add(new Entry(step, 0));
        scores[step] = 0;
    }

    var came_from = new HashMap<Step, Step> (
                                             (s) => s.hash(),
                                             (s1, s2) => s1.equal_to(s2)
    );

    while (true) {
        Entry ? current = open_set.poll();
        if (current == null) {
            break;
        }

        var current_score = scores[current.step];

        if (current.priority < current_score) {
            continue;
        }

        // note: this is usually useless, for the standard case where you only care about finding the shortest path you
        // can just merge turn and step for the node and just analyse the neighbours. This is what I did at first, but
        // then I noticed that the algorithm I've used in part 2 would fail finding corners if I did not generate moot
        // nodes for all possible turns. The neighbour logic then can be stripped away, because you can then handle all
        // neighbours by rotating + advancing the current node
        foreach (var dir in DIRECTIONS) {
            if (dir == current.step.dir) {
                continue;
            }

            var turn = new Step(current.step.point, dir);
            var turn_score = current_score + TURN_COST;

            if (try_get(scores, turn, int.MAX) > turn_score) {
                scores[turn] = turn_score;
                came_from[turn] = current.step;

                open_set.add(new Entry(turn, turn_score));
            }
        }

        // we've added in the queue all turn arounds. We should now also add the forward node for the current node (if
        // not a wall); the neighbours will be handled when the nodes above are popped - every node only concerns itself
        // with the step in front of itself
        var forward = current.step.advance(grid);
        if (forward == null) {
            continue;
        }

        var forward_score = current_score + FORWARD_COST;
        if (try_get(scores, forward, int.MAX) > forward_score) {
            came_from[forward] = current.step;
            scores[forward] = forward_score;

            open_set.add(new Entry(forward, forward_score));
        }
    }

    // note that we don't cut off early, we let Dijkstra run until it runs out of nodes to score. In this way we have 
    // a complete map of all the scores from the start to all the nodes (at least, I think that's what happens)
    return new DijkstraResult(came_from, scores);
}

Gee.AbstractSet<Point> optimal_points(Grid grid, DijkstraResult from_start, DijkstraResult from_end, int optimal)
requires(from_start != null && from_end != null)
{
    var points = new HashSet<Point?> (
                                      (p) => p.hash(),
                                      (p1, p2) => p1.equal_to(p2)
    );

    foreach (var hole in grid) {
        foreach (var dir in DIRECTIONS) {
            var step = new Step(hole, dir);
            var flipped = step.flip();

            if (from_start.scores.has_key(step) && from_end.scores.has_key(flipped)) {
                var score = from_start.scores[step] + from_end.scores[flipped];
                if (score == optimal) {
                    points.add(hole);
                }
            }
        }
    }

    return points;
}

class OptimalPath : Object {
    public Step final_step { get; private set; }
    public Gee.List<Step> path { get; private set; }
    public int score { get; private set; }

    private OptimalPath(Step final_step, Gee.List<Step> path, int score) {
        this.final_step = final_step;
        this.path = path;
        this.score = score;
    }

    public static OptimalPath ? detect(Grid grid, DijkstraResult result) {
        Step final_step = null;
        Gee.List<Step> path = null;
        int score = 0;

        foreach (var dir in DIRECTIONS) {
            var attempt = new Step(grid.end, dir);
            if (result.came_from.has_key(attempt)) {
                var found_score = result.scores[attempt];

                if (found_score < score || score == 0) {
                    final_step = attempt;
                    path = reconstruct_path(result.came_from, grid.start, attempt);
                    score = found_score;
                }
            }
        }

        return score == 0 ? null : new OptimalPath(final_step, path, score);
    }
}

int main(string[] args) {
    if (args.length != 2) {
        stderr.puts(@"error: wrong number of arguments\nusage: $(args[0]) <number>\n");

        return 2;
    }

    try {
        var grid = Grid.from_file(args[1]);

        var result = dijkstra(grid);

        var optimal = OptimalPath.detect(grid, result);

        if (optimal == null) {
            stdout.puts("no path found\n");
        } else {
            stdout.puts("path found\n");

            assert(optimal.final_step != null && optimal.path != null && optimal.score != 0);

            grid.dump(optimal.path);

            stdout.puts(@"part 1: $(optimal.score)\n");

            // now: I got stuck so I found inspiration online on how to solve this. The idea is that we can use Dijkstra
            // from the end in all directions to score all the paths from the end. Once we have the scores from the end,
            // it should be a matter of finding those points where the score of from_start[s] + from_end[rev(s)] is the
            // equal to the optimal score

            Step goal_dirs[4] = { null, null, null, null };

            foreach (var dir in DIRECTIONS) {
                var attempt = new Step(grid.end, dir);
                goal_dirs[dir] = attempt;
            }

            var rev_result = dijkstra(grid, goal_dirs);

            var optpoints = optimal_points(grid, result, rev_result, optimal.score);

            grid.dump(null, optpoints);

            stdout.puts(@"part 2: $(optpoints.size)\n");
        }
    } catch (Error e) {
        stderr.puts(@"error: $(e.message)\n");

        return 1;
    }

    return 0;
}