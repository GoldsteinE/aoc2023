const std = @import("std");

const Direction = enum(u2) {
    up,
    right,
    down,
    left,
};

fn offset(self: Direction) struct { x: i8, y: i8 } {
    return switch (self) {
        .up => .{ .x = 0, .y = -1 },
        .right => .{ .x = 1, .y = 0 },
        .down => .{ .x = 0, .y = 1 },
        .left => .{ .x = -1, .y = 0 },
    };
}

fn inverse(self: Direction) Direction {
    return switch (self) {
        .up => .down,
        .right => .left,
        .down => .up,
        .left => .right,
    };
}

// Field order is important: we use it to initialize first node costs.
const Node = packed struct {
    direction: Direction,
    count: u6,
    x: u8,
    y: u8,
};

fn toKey(node: Node) usize {
    const Key = packed struct {
        node: Node,
        pad: u40,
    };
    return @bitCast(Key{ .node = node, .pad = 0 });
}

fn shiftInBounds(width: usize, height: usize, x: u8, y: u8, direction: Direction) ?struct { x: u8, y: u8 } {
    const delta = offset(direction);
    const wx: i16 = x;
    const wy: i16 = y;
    const nx = wx + delta.x;
    const ny = wy + delta.y;
    if (nx < 0 or ny < 0 or nx >= width or ny >= height) {
        return null;
    } else {
        // Safety: width and height must fit in u8, so `nx` and `ny` also fit.
        return .{ .x = @intCast(nx), .y = @intCast(ny) };
    }
}

const Queue = struct {
    buf: std.RingBuffer,

    fn push(self: *Queue, node: Node) !void {
        const bytes: [3]u8 = @bitCast(node);
        try self.buf.writeSlice(&bytes);
    }

    fn pop(self: *Queue) ?Node {
        return @bitCast([_]u8{
            self.buf.read() orelse return null,
            self.buf.read() orelse return null,
            self.buf.read() orelse return null,
        });
    }
};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var args = std.process.args();
    _ = args.next() orelse std.debug.panic("no arg 0", .{});
    const rawPart = args.next() orelse "1";
    const part: u8 = if (rawPart[0] == '1') 1 else 2;

    // We can get away with inefficient allocator, because we only ever make 3 allocations.
    const alloc = std.heap.page_allocator;

    // First one:
    const raw_input = try std.io.getStdIn().reader().readAllAlloc(alloc, 32768);
    defer alloc.free(raw_input);

    // Second one (we could probably get rid of this one?):
    const input = try std.mem.replaceOwned(u8, alloc, raw_input, &[_]u8{'\n'}, &[_]u8{});
    defer alloc.free(input);

    const width = std.mem.indexOfScalar(u8, raw_input, '\n') orelse std.debug.panic("no newline in input", .{});
    const height = input.len / width;
    if (width > 255 or height > 255) {
        std.debug.panic("width and height must fit in a byte", .{});
    }

    // And the final one: a queue for Dijkstra's algorithm.
    // The size for this one is tricky, but this would probably be enough.
    const buf = try std.RingBuffer.init(alloc, width * height * 4 * @sizeOf(Node));
    var queue = Queue{ .buf = buf };
    defer queue.buf.deinit(alloc);

    // We allocate costs statically, since their size is fixed.
    const costsHolder = struct {
        const costsSize = 1 << @bitSizeOf(Node);
        // First 256 are for 0:0, the rest are for other nodes.
        var costs: [costsSize]u16 = [_]u16{0} ** 256 ++ [_]u16{0xffff} ** (costsSize - 256);
    };
    var costs = &costsHolder.costs;

    try queue.push(Node{ .x = 0, .y = 0, .direction = Direction.down, .count = 0 });
    while (queue.pop()) |curr| {
        const selfCost = costs[toKey(curr)];
        for ([_]Direction{ .up, .right, .down, .left }) |direction| {
            if (curr.count != 0 and direction == inverse(curr.direction)) {
                continue;
            }

            const maxCount: u6 = if (part == 1) 3 else 10;
            if (curr.direction == direction and curr.count == maxCount) {
                continue;
            }

            const minCount: u6 = if (part == 1) 0 else 4;
            if (curr.count != 0 and curr.direction != direction and curr.count < minCount) {
                continue;
            }

            const newCoords = shiftInBounds(width, height, curr.x, curr.y, direction) orelse {
                continue;
            };
            const new = Node{
                .x = newCoords.x,
                .y = newCoords.y,
                .direction = direction,
                .count = if (direction == curr.direction) curr.count + 1 else 1,
            };
            const prevCost = costs[toKey(new)];
            const newCost = selfCost + input[curr.y * width + curr.x] - '0';
            if (newCost < prevCost) {
                costs[toKey(new)] = newCost;
                try queue.push(new);
            }
        }
    }

    var result: u16 = 0xffff;
    for ([_]Direction{ .up, .right, .down, .left }) |direction| {
        const start: u6 = if (part == 1) 0 else 4;
        const end: u6 = if (part == 1) 4 else 11;
        for (start..end) |count| {
            // Safety: width and height fit into u8 as per assert above. Count is <= 10.
            const candidate = Node{
                .x = @intCast(width - 1),
                .y = @intCast(height - 1),
                .direction = direction,
                .count = @intCast(count),
            };
            const cost = costs[toKey(candidate)];
            if (cost < result) {
                result = cost;
            }
        }
    }
    result += input[(height - 1) * (width + 1)];
    result -= input[0];
    try stdout.print("{d}\n", .{result});
}
