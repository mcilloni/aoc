// AoC 2025 Day 08

import std.algorithm;
import std.conv;
import std.format;
import std.math;
import std.range;
import std.stdio;
import std.typecons;

struct Point {
    long x;
    long y;
    long z;

    this(immutable long x, immutable long y, immutable long z) @safe pure nothrow {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    string toString() const {
        return format("(%d, %d, %d)", this.x, this.y, this.z);
    }
}

double distance(immutable Point p1, immutable Point p2) @safe pure nothrow {
    immutable double dx = cast(double)(p1.x - p2.x);
    immutable double dy = cast(double)(p1.y - p2.y);
    immutable double dz = cast(double)(p1.z - p2.z);

    return sqrt(dx * dx + dy * dy + dz * dz);
}

struct Link {
    alias Entry = Tuple!(ulong, "index", Point, "p");

    Entry p1;
    Entry p2;
    double dist;

    this(immutable Entry p1, immutable Entry p2) @safe pure nothrow {
        this.p1 = p1;
        this.p2 = p2;
        this.dist = distance(p1.p, p2.p);
    }

    int opCmp(const Link other) const {
        return (this.dist > other.dist) - (this.dist < other.dist);
    }

    string toString() const {
        return format("Link(%s <-> %s : %f)", this.p1, this.p2, this.dist);
    }
}

Link[] links(const Point[] points) {
    auto links = points
        .enumerate
        .array
        .pairs
        .map!((pair) {
            immutable auto p1 = pair[0];
            immutable auto p2 = pair[1];
            return Link(Link.Entry(p1.index, p1.value), Link.Entry(p2.index, p2.value));
        })
        .array;

    links.sort();

    return links;
}
    
class Circuit {
private:
    int[const(Point)] _points;

public:
    this(const Link link) @safe pure nothrow {
        this._points[link.p1.p] = 1;
        this._points[link.p2.p] = 1;
    }

    bool merge(const Circuit other) {
        Tuple!(Point, Point) lastLink;

        if (other !is null && other._points.keys.any!((point) => point in this._points)) {
            foreach (const point; other._points.keys) {
                this._points[point] = 1;
            }

            return true;
        }

        return false;
    }

    size_t length() const @safe pure nothrow {
        return this._points.length;
    }

    override string toString() const {
        return format(
            "Circuit[%d](%s)",
            this.length,
            this._points.keys
                .map!((point) => point.toString())
                .joiner(", ")
                .to!string()
        );
    }
}

Circuit[] merge(Circuit[] circuits) {
    for (;;) {
        const size_t initialLength = circuits.length;

        outer: for (size_t i = 0; i < circuits.length; ++i) {
            Circuit current = circuits[i];
            if (current !is null) {
                bool mergedAtLeastOnce = false;
                for (size_t j = i + 1; j < circuits.length; ++j) {
                    if (current.merge(circuits[j])) {
                        circuits[j] = null;
                        mergedAtLeastOnce = true;
                    }
                }

                if (mergedAtLeastOnce) {
                    circuits = circuits.filter!((c) => c !is null).array();
                    break outer;
                }
            }
        }

        if (circuits.length == initialLength) {
            break;
        }
    }

    return circuits;
}

class OutOfBoundsException : Exception {
    this(const string msg) @safe pure nothrow {
        super(msg);
    }
}

struct UnionFind {
    struct Node {
        Node* parent;
        size_t size;
    }

    private Node[] nodes;

    this(immutable size_t n) @safe pure nothrow {
        this.nodes = new Node[n];

        for (size_t i = 0; i < n; ++i) {
            this.nodes[i].parent = &this.nodes[i];
            this.nodes[i].size = 1;
        }
    }

    Node* at(immutable size_t index) @safe {
        if (index >= this.nodes.length) {
            throw new OutOfBoundsException(format("index %d out of bounds [0, %d)", index, this.nodes.length));
        }

        return &this.nodes[index];
    }

    Node* find(size_t ix) @safe {
        auto root = this.at(ix);

        while (root.parent != root) {
            root = root.parent;
        }

        auto x = this.at(ix);
        while (x.parent != root) {
            auto parent = x.parent;
            x.parent = root;
            x = parent;
        }

        return root;
    }

    size_t merge(immutable size_t ix, immutable size_t iy) {
        // Replace nodes by roots
        auto x = find(ix);
        auto y = find(iy);

        if (x == y) {
            return x.size;  // x and y are already in the same set
        }

        // If necessary, swap variables to ensure that
        // x has at least as many descendants as y
        if (x.size < y.size) {
            swap(x, y);
        }

        // Make x the new root
        y.parent = x;

        // Update the size of x
        x.size = x.size + y.size;

        return x.size;
    }
}

long part1(const Point[] points, size_t nSamples) {
    const auto allLinks = links(points);

    if (nSamples > allLinks.length) {
        nSamples = allLinks.length;
    }

    Circuit[] circuits = merge(allLinks[0 .. nSamples].map!((link) => new Circuit(link)).array());

    circuits.sort!("a.length > b.length");

    if (circuits.length < 3) {
        throw new Exception(format("expected at least 3 circuits, got %d", circuits.length));
    }

    return circuits[0 .. 3].map!((c) => c.length).fold!((a, b) => a * b);
}

long part2(const Point[] points) {
    const auto allLinks = links(points);

    // use the Disjoint-set data structure (Union-Find) to join the sets comprising the single points into a single set
    auto uf = UnionFind(points.length);

    foreach (link; allLinks) {
        immutable auto size_of_set = uf.merge(link.p1.index, link.p2.index);

        // if the merge ends up with a set containing all points, we hit the wanted link
        if (size_of_set == points.length) {
            return link.p1.p.x * link.p2.p.x;
        }
    }

    return -1;
}

template pairs(T) {
    Pairs pairs(const(T)[] data) {
        return Pairs(data);
    }    

    private struct Pairs {
        private const(T)[] data;
        private size_t i, j;

        this(const(T)[] data) {
            this.data = data;
            this.i = 0;
            this.j = this.data.length > 0 ? 1 : 0;
        }

        bool empty() @safe pure const {
            return this.i + 1 >= this.data.length;
        }

        Tuple!(T, T) front() @safe pure const {
            const auto left = this.data[this.i];
            const auto right = this.data[this.j];
            return tuple!(T,T)(left, right);
        }

        void popFront() {
            ++this.j;
            if (this.j == this.data.length) {
                ++this.i;
                this.j = this.i + 1;
            }
        }

        string toString() const {
            return "Pairs(" ~ pairs(this.data)
                .map!((pair) => format("(%s, %s)", pair[0], pair[1]))
                .joiner(", ")
                .to!string() ~ ")";
        }
    }

}

class BadInputException : Exception {
    this(const string msg) @safe pure nothrow {
        super(msg);
    }
}

Point[] parseInput(const string path) {
    auto file = File(path, "r");
    scope(exit) file.close();

    return file
        .byLine()
        .enumerate()
        .map!((item) {
            auto line = item[0] + 1;
            auto parts = item[1].split(",");
            if (parts.length != 3) {
                throw new BadInputException(format("at %s:%d: malformed point expression", path, line));
            }

            auto x = parts[0].to!long;
            auto y = parts[1].to!long;
            auto z = parts[2].to!long;

            return Point(x, y, z);
        }).array();
}

int main(immutable string[] args) {
    size_t nSamples = 1000U;

    try {
        switch (args.length) {
        case 2:
            break;
        case 3:
            nSamples = args[2].to!size_t;
            break;
        default:
            stderr.writefln("error: wrong number of arguments\nUsage: %s FILE [NSAMPLES]", args[0]);

            return 2;
        }

        const auto points = parseInput(args[1]);

        if (points.length < 2) {
            stderr.writefln("error: need at least two points to compute links");

            return 1;
        }

        writeln("Part 1: ", part1(points, nSamples));
        writeln("Part 2: ", part2(points));
    } catch (const Exception e) {
        stderr.writefln("error: %s", e.msg);
        return 1;
    }

    return 0;
}
