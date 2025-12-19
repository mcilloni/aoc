// AoC 2025 Day 04

const std = @import("std");

const assert = std.debug.assert;

fn u8SliceToU64(slice: []const u8) u64 {
    var result: u64 = 0;

    for (slice) |digit| {
        result = result * 10 + @as(u64, digit);
    }

    return result;
}

fn joltageFor2(line: []const u8) u8 {
    const lineLen = line.len;
    assert(lineLen > 1);

    const maxVal = maxIn(line);

    if (maxVal.index == lineLen - 1) {
        const bestBefore = maxIn(line[0..maxVal.index]);

        return bestBefore.value * 10 + maxVal.value;
    } else {
        const bestAfter = maxIn(line[maxVal.index + 1 ..]);

        return maxVal.value * 10 + bestAfter.value;
    }
}

fn joltageFor12(line: []const u8) u64 {
    const lineLen = line.len;
    assert(lineLen >= 12);

    var buffer = std.mem.zeroes([12]u8);
    const picks = pick(buffer.len, line, &buffer);
    assert(picks.len == 12);

    return u8SliceToU64(picks);
}

fn pick(n: u8, in: []const u8, buffer: []u8) []const u8 {
    assert(buffer.len >= n);

    if (n == 0) {
        return &[0]u8{};
    }

    // first: if the input is shorter than or equal to n, just return it all and let the caller deal with it.
    // There's nothing to pick really
    if (in.len <= n) {
        return in;
    }

    // second: find the biggest element in the input. We ideally want to pick it and n-1 elements to its right (we know
    // that the largest digit will always lead to a maximum value)
    const maxElem = maxIn(in);
    const right = in[maxElem.index + 1 ..];

    var rightBuf = std.mem.zeroes([256]u8);
    const rightPicks = pick(n - 1, right, &rightBuf);
    assert(rightPicks.len <= n - 1);

    // third: check how much picks we got from the right side, and fill the n - rpicks left from the left side
    const remaining = n - @as(u8, @intCast(rightPicks.len)) - 1;

    var leftBuf = std.mem.zeroes([256]u8);
    const leftPicks = pick(remaining, in[0..maxElem.index], &leftBuf);
    assert(leftPicks.len + rightPicks.len + 1 == n);

    var list = std.ArrayList(u8).initBuffer(buffer);
    list.appendSliceAssumeCapacity(leftPicks);
    list.appendAssumeCapacity(maxElem.value);
    list.appendSliceAssumeCapacity(rightPicks);

    return list.items; // ok, list is the buffer
}

fn isDigit(chr: u8) bool {
    return chr >= '0' and chr <= '9';
}

const MaxElement = struct {
    value: u8,
    index: usize,
};

fn maxIn(slice: []const u8) MaxElement {
    assert(slice.len > 0);

    var result = std.mem.zeroes(MaxElement);

    // we prioritise the lowest index in case of ties
    for (slice, 0..) |item, index| {
        if (item > result.value) {
            result.value = item;
            result.index = index;
        }
    }

    return result;
}

// my 2 cents on the new Zig 0.15 api (aka, the Writergate thing): this is mental
// cheapeau to the Zig team for having the guts to literally break everybody's code in a dot release. At least have the
// decency to keep a snippet library online somewhere up to date whenever you break stuff like this, it took me ages to
// figure out how to do read a damn line from a file with the new API. Zig isn't that hard, but these kinds of things
// really make the standard library feel super unpolished and half-done
fn parseInput(path: []const u8, allocator: std.mem.Allocator) ![][]u8 {
    var file = try std.fs.cwd().openFile(path, .{});

    var buffer: [4096]u8 = undefined;
    var buffered_reader = file.readerStreaming(&buffer);
    const reader = &buffered_reader.interface;

    var lines = try std.ArrayList([]u8).initCapacity(allocator, 16);

    var bytes: std.Io.Writer.Allocating = .init(allocator);
    defer bytes.deinit();

    while (true) {
        _ = reader.streamDelimiter(&bytes.writer, '\n') catch |err| if (err == error.EndOfStream) {
            break;
        } else {
            return err;
        };

        _ = reader.toss(1);

        try lines.append(allocator, try processLine(try bytes.toOwnedSlice()));
        bytes.clearRetainingCapacity();
    }

    if (bytes.written().len > 0) {
        try lines.append(allocator, try processLine(try bytes.toOwnedSlice()));
    }

    return lines.toOwnedSlice(allocator);
}

fn processLine(line: []u8) ![]u8 {
    for (line) |*chr| {
        if (!isDigit(chr.*)) {
            return error.InvalidCharacter;
        }

        chr.* -= '0';
    }

    return line;
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer assert(gpa.deinit() == .ok);

    const gpalloc = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(gpalloc);
    defer arena.deinit();

    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    const progname = args.next() orelse {
        std.debug.panic("this cannot happen", .{});
    };

    const path = args.next() orelse {
        std.log.err("error: expected one argument\nusage: {s} INPUT\n", .{progname});

        return 2;
    };

    if (args.next() != null) {
        std.log.err("error: too many arguments\nusage: {s} INPUT\n", .{progname});

        return 2;
    }

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const lines = try parseInput(path, allocator);

    var tally2: u32 = 0;
    var tally12: u64 = 0;
    for (lines) |line| {
        const joltage2 = joltageFor2(line);
        const joltage12 = joltageFor12(line);
        try stdout.print("{any}: j2 = {d}, j12 = {d}\n", .{ line, joltage2, joltage12 });

        tally2 += joltage2;
        tally12 += joltage12;
    }

    try stdout.print("part1: {d}\n", .{tally2});
    try stdout.print("part2: {d}\n", .{tally12});

    try stdout.flush();

    return 0;
}
