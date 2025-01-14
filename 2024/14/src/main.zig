// AoC 2024 Day 14

const std = @import("std");

const assert = std.debug.assert;

const Point = struct {
    x: i64,
    y: i64,

    pub fn add(self: Point, other: Point) Point {
        return .{ .x = self.x + other.x, .y = self.y + other.y };
    }

    pub fn wrappingAdd(self: Point, other: Point, bounds: Bounds) Point {
        return .{
            .x = (self.x + other.x + bounds.x) % bounds.x,
            .y = (self.y + other.y + bounds.y) % bounds.y,
        };
    }

    pub fn format(self: Point, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        return writer.print("({d}, {d})", .{ self.x, self.y });
    }
};

// bound intended over a grid [0, width) x [0, height)
const Bounds = struct {
    width: i64,
    height: i64,

    pub fn contains(self: Bounds, point: Point) bool {
        return point.x >= 0 and point.x < self.width and point.y >= 0 and point.y < self.height;
    }
};

const Robot = struct {
    position: Point,
    velocity: Point,

    pub fn format(self: Robot, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        return writer.print("p={p}, v={v}", .{ self.position, self.velocity });
    }
};

const Source = struct {
    file: std.fs.File,

    reader: @TypeOf(std.io.bufferedReader(@as(std.fs.File.Reader, undefined))),
    chr: ?u8 = null,

    const Self = @This();
    const Error = std.fs.File.ReadError || error{
        EndOfStream,
        InvalidId,
        UnexpectedToken,
    };

    pub fn close(self: *Self) void {
        self.file.close();
    }

    pub fn eof(self: *Self) bool {
        return self.chr == null;
    }

    pub fn open(path: []const u8) !Self {
        var file = try std.fs.cwd().openFile(path, .{});

        var self = Self{ .file = file, .reader = std.io.bufferedReader(file.reader()) };

        self.chr = try self.nextChar();

        return self;
    }

    pub fn next(self: *Self) !u8 {
        const chr = self.chr orelse return Self.Error.EndOfStream;

        self.chr = try self.nextChar();

        return chr;
    }

    pub fn peek(self: *Self) ?u8 {
        return self.chr;
    }

    fn nextChar(self: *Self) !?u8 {
        var bufz = [_]u8{0};

        const n = try self.reader.read(bufz[0..]);

        //return EOF
        if (n == 0) {
            return null;
        }

        return bufz[0];
    }
};

fn expect(source: *Source, expected: u8) !void {
    if (source.peek() != expected) {
        return error.UnexpectedToken;
    }

    _ = try source.next();
}

fn isAlpha(chr: u8) bool {
    return (chr >= 'A' and chr <= 'Z') or (chr >= 'a' and chr <= 'z');
}

fn isDigit(chr: u8) bool {
    return chr >= '0' and chr <= '9';
}

fn isOdd(n: i64) bool {
    return n & 1 == 1;
}

fn markBots(robots: []Robot, bits: []u8, bounds: Bounds) void {
    @memset(bits, 0);

    for (robots) |robot| {
        const index = robot.position.y * bounds.width + robot.position.x;

        bits[@bitCast(index)] = 1;
    }
}

fn parseAlpha(source: *Source) !u8 {
    const chr = source.peek() orelse return error.UnexpectedToken;

    if (!isAlpha(chr)) {
        return error.UnexpectedToken;
    }

    return try source.next();
}

fn parseDigit(source: *Source) !u8 {
    const chr = source.peek() orelse return error.UnexpectedToken;

    if (!isDigit(chr)) {
        return error.UnexpectedToken;
    }

    return try source.next() - '0';
}

fn parseInput(path: []const u8, allocator: std.mem.Allocator) ![]Robot {
    var source = try Source.open(path);
    defer source.close();

    var result = std.ArrayList(Robot).init(allocator);

    while (!source.eof()) {
        try skipWhitespace(&source);

        const robot = try parseRobot(&source);

        try result.append(robot);

        try skipWhitespace(&source);

        if (!source.eof()) {
            try parseNl(&source);
        }
    }

    return result.toOwnedSlice();
}

fn parseInt(source: *Source) !i64 {
    var result: i64 = 0;
    var signum: i64 = 1;

    if (source.peek() == '-') {
        signum = -1;
        _ = try source.next();
    }

    while (isDigit(source.peek() orelse 0)) {
        result = result * 10 + try parseDigit(source);
    }

    return result * signum;
}

fn parseNl(source: *Source) !void {
    if (source.peek() == '\r') {
        _ = try source.next();
    }

    try expect(source, '\n');
}

fn parsePoint(source: *Source) !Point {
    const x = try parseInt(source);

    try skipWhitespace(source);

    try expect(source, ',');

    try skipWhitespace(source);

    const y = try parseInt(source);

    return .{ .x = x, .y = y };
}

const PointDecl = struct {
    id: u8,
    point: Point,
};

fn parsePointDecl(source: *Source) !PointDecl {
    const id = try parseAlpha(source);

    try skipWhitespace(source);

    try expect(source, '=');

    try skipWhitespace(source);

    const point = try parsePoint(source);

    return .{ .id = id, .point = point };
}

fn parseRobot(source: *Source) !Robot {
    const position = try parsePointDecl(source);

    if (position.id != 'p') {
        return error.InvalidId;
    }

    try skipWhitespace(source);

    const velocity = try parsePointDecl(source);

    if (velocity.id != 'v') {
        return error.InvalidId;
    }

    return .{
        .position = position.point,
        .velocity = velocity.point,
    };
}

fn part1(robots: []Robot, bounds: Bounds) i64 {
    const ticks = 100;

    tickRobotsNTimes(robots, bounds, ticks);

    var quads = [_]i64{ 0, 0, 0, 0 };

    for (robots) |robot| {
        const quad = quadrantOf(bounds, robot.position) orelse continue;

        quads[quad] += 1;
    }

    return quads[0] * quads[1] * quads[2] * quads[3];
}

fn printMap(robots: []Robot, bounds: Bounds, out: anytype, allocator: std.mem.Allocator) !void {
    const mid_x = bounds.width >> 1;
    const mid_y = bounds.height >> 1;

    var points = std.AutoHashMap(Point, u64).init(allocator);

    for (robots) |robot| {
        const point = robot.position;

        const entry = try points.getOrPut(point);

        entry.value_ptr.* = if (entry.found_existing) entry.value_ptr.* + 1 else 1;
    }

    var y: i64 = 0;

    while (y < bounds.height) : (y += 1) {
        var x: i64 = 0;
        while (x < bounds.width) : (x += 1) {
            if (x == mid_x or y == mid_y) {
                try out.writeByte(' ');

                continue;
            }

            const point = .{ .x = x, .y = y };
            if (points.get(point)) |n| {
                try out.print("{d}", .{n});
            } else {
                try out.writeByte('.');
            }
        }

        try out.writeByte('\n');
    }
}

fn quadrantOf(bounds: Bounds, point: Point) ?u8 {
    assert(isOdd(bounds.width) and isOdd(bounds.height));

    const mid_x = bounds.width >> 1;
    const mid_y = bounds.height >> 1;

    if (point.x == mid_x or point.y == mid_y) {
        return null;
    }

    // 0 | 1
    // -----
    // 2 | 3
    if (point.x < mid_x) {
        if (point.y < mid_y) {
            return 0;
        } else {
            return 2;
        }
    } else {
        if (point.y < mid_y) {
            return 1;
        } else {
            return 3;
        }
    }
}

fn skipWhitespace(source: *Source) !void {
    while (source.peek()) |chr| {
        if (chr != ' ' and chr != '\t') {
            return;
        }

        _ = try source.next();
    }
}

fn tickRobotNTimes(robot: *Robot, bounds: Bounds, reps: i64) void {
    robot.position = .{
        .x = @mod(robot.position.x + robot.velocity.x * reps, bounds.width),
        .y = @mod(robot.position.y + robot.velocity.y * reps, bounds.height),
    };
}

fn tickRobotsNTimes(robots: []Robot, bounds: Bounds, ticks: i64) void {
    for (robots) |*robot| {
        tickRobotNTimes(robot, bounds, ticks);
    }
}

// Disclaimer: I really disliked this problem. I much rather lose my mental sanity tackling problems that in the end can
// be solved with precise solutions than resort to wonky heuristics or brute force as the main option. Sure sometimes it
// is the shortest path (see how I did Day 06), but that should not be the *correct* path - you're supposed to feel like
// you're cheating in the end.
//
// After putting all of that out of my systen, I admit that after realising that there was no deterministic solution I
// lost interest, so I ended up browsing the subreddit a bit, where I read a very nifty observation about the nature of
// this problem: when the bots create a tree shape, the resulting image is likely going to end up having pretty low entropy.
// It follows suit that this also implies it will be more compressible than the rest of the generated image set.
// Therefore, this function iterates this sequence of operations:
// 1. ticks the robots
// 2. creates a 1 bit image of the robots' positions
// 3. compresses the image using deflate (that thankfully Zig has a built-in implementation for)
// 4. compares if the size of the compressed image is somewhat smaller than the average size (I hardcoded 100 as a
//    delta, just because)
// This somewhat works fine, but YMMV. While I love that AoC problems may lead to multiple solutions, I just can't bring
// myself to liking the fact a solution for a set of inputs may not work for another set of inputs.
fn tickRobotsLowEntropy(robots: []Robot, bounds: Bounds, allocator: std.mem.Allocator) !usize {
    const bits = try allocator.alloc(u8, @bitCast(bounds.width * bounds.height));

    var compr = std.ArrayList(u8).init(allocator);

    var ticks: usize = 1;
    var avg: ?usize = null;

    while (true) : (ticks += 1) {
        tickRobotsNTimes(robots, bounds, 1);

        markBots(robots, bits, bounds);

        compr.clearRetainingCapacity();

        var fbs = std.io.fixedBufferStream(bits);
        try std.compress.flate.compress(fbs.reader(), compr.writer(), .{});

        const compr_size = compr.items.len;

        // I know for sure nothuing happens before tick 100
        if (avg == null) {
            avg = compr_size;

            continue;
        }

        if (ticks > 100) {
            if (compr_size < avg.? - 100) {
                return ticks;
            }
        }

        avg = (compr_size + ticks * avg.?) / (ticks + 1);
    }
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpalloc = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(gpalloc);
    defer arena.deinit();

    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    const progname = args.next() orelse {
        std.debug.panic("this cannot happen", .{});
    };

    const path = args.next() orelse {
        std.log.err("error: expected one argument\nusage: {s} INPUT [MAX_X] [MAX_Y]\n", .{progname});

        return 2;
    };

    const max_x = if (args.next()) |arg| try std.fmt.parseInt(i64, arg, 10) else 101;
    if (!isOdd(max_x) or max_x < 0) {
        std.log.err("error: max_x must be odd and positive\n", .{});

        return 2;
    }

    const max_y = if (args.next()) |arg| try std.fmt.parseInt(i64, arg, 10) else 103;
    if (!isOdd(max_y) or max_y < 0) {
        std.log.err("error: max_y must be odd and positive\n", .{});

        return 2;
    }

    if (args.next() != null) {
        std.log.err("error: too many arguments\nusage: {s} INPUT\n", .{progname});

        return 2;
    }

    const bounds = .{ .width = max_x, .height = max_y };

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const robots = try parseInput(path, allocator);

    try printMap(robots, bounds, stdout, allocator);

    try bw.flush();

    const p1bots = try allocator.dupe(Robot, robots);
    const part1_result = part1(p1bots, bounds);

    try printMap(p1bots, bounds, stdout, allocator);

    try stdout.print("part 1: {d}\n", .{part1_result});

    try bw.flush();

    const part2_result = try tickRobotsLowEntropy(robots, bounds, allocator);

    try stdout.print("part2: found interesting low entropy at tick {d}\n", .{part2_result});

    try printMap(robots, bounds, stdout, allocator);

    try bw.flush();

    return 0;
}
