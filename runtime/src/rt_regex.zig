const std = @import("std");

pub const CompileError = error{
    BadPattern,
    UnbalancedParen,
    UnbalancedBracket,
    BadRepeat,
    OutOfMemory,
};

// Bring-up surface: only flags used today.
pub const REG_ICASE: i32 = 2;

const Unset: u32 = std.math.maxInt(u32);

pub const Program = struct {
    insts: []Inst,
    nsub: usize,
    ncap: usize,
    icase: bool,

    pub fn deinit(self: *Program, alloc: std.mem.Allocator) void {
        alloc.free(self.insts);
        alloc.destroy(self);
    }
};

pub const Inst = union(enum) {
    Char: struct { ch: u8, out: u32 },
    Any: struct { out: u32 },
    Class: struct { set: [16]u8, negated: bool, out: u32 },
    Split: struct { out: u32, out1: u32 },
    Jmp: struct { out: u32 },
    Save: struct { slot: u16, out: u32 },
    AssertBol: struct { out: u32 },
    AssertEol: struct { out: u32 },
    AssertWordBoundary: struct { out: u32 },
    Match: void,
};

pub fn compile(alloc: std.mem.Allocator, pattern: []const u8, cflags: i32) CompileError!*Program {
    var nodes: std.ArrayList(Node) = .empty;
    defer nodes.deinit(alloc);

    const icase = (cflags & REG_ICASE) != 0;
    var p = Parser{
        .alloc = alloc,
        .pattern = pattern,
        .nodes = &nodes,
        .icase = icase,
    };

    const root_id = try p.parseRegex();
    if (!p.atEnd()) return CompileError.BadPattern;

    // Wrap in implicit group 0 (whole match).
    const group0_id = try p.mkGroup(0, root_id);
    const nsub: usize = p.group_count;

    var b = Builder{ .alloc = alloc };
    defer b.deinit();

    var frag = try b.compileNode(nodes.items, group0_id);
    const match_idx: usize = try b.emit(.{ .Match = {} });
    b.patchAll(&frag.outs, @intCast(match_idx));

    const insts = try b.insts.toOwnedSlice(alloc);
    const prog = try alloc.create(Program);
    prog.* = .{
        .insts = insts,
        .nsub = nsub,
        .ncap = 2 * (nsub + 1),
        .icase = icase,
    };
    return prog;
}

pub fn execFirst(alloc: std.mem.Allocator, prog: *const Program, input: []const u8, caps_out: []i32) bool {
    if (caps_out.len < prog.ncap) return false;

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    var start: usize = 0;
    while (start <= input.len) : (start += 1) {
        if (runAt(a, prog, input, start, caps_out)) return true;
        _ = arena.reset(.retain_capacity);
    }

    return false;
}

// -----------------------------------------------------------------------------
// Parser (Java-ish subset, ASCII-only bring-up)
// -----------------------------------------------------------------------------

const NodeId = u32;

const Node = union(enum) {
    Empty: void,
    Lit: u8,
    Any: void,
    Class: struct { set: [16]u8, negated: bool },
    Concat: struct { a: NodeId, b: NodeId },
    Alt: struct { a: NodeId, b: NodeId },
    Repeat: struct { node: NodeId, kind: RepeatKind },
    Group: struct { id: usize, node: NodeId },
    AssertBol: void,
    AssertEol: void,
    AssertWordBoundary: void,
};

const RepeatKind = enum { Star, Plus, Maybe };

const Parser = struct {
    alloc: std.mem.Allocator,
    pattern: []const u8,
    i: usize = 0,
    nodes: *std.ArrayList(Node),
    icase: bool,
    group_count: usize = 0,

    fn atEnd(self: *const Parser) bool {
        return self.i >= self.pattern.len;
    }

    fn peek(self: *const Parser) ?u8 {
        if (self.i >= self.pattern.len) return null;
        return self.pattern[self.i];
    }

    fn next(self: *Parser) ?u8 {
        const ch = self.peek() orelse return null;
        self.i += 1;
        return ch;
    }

    fn accept(self: *Parser, ch: u8) bool {
        if (self.peek() == ch) {
            self.i += 1;
            return true;
        }
        return false;
    }

    fn mk(self: *Parser, n: Node) CompileError!NodeId {
        self.nodes.append(self.alloc, n) catch return CompileError.OutOfMemory;
        return @intCast(self.nodes.items.len - 1);
    }

    fn mkConcat(self: *Parser, a: NodeId, b: NodeId) CompileError!NodeId {
        return self.mk(.{ .Concat = .{ .a = a, .b = b } });
    }

    fn mkAlt(self: *Parser, a: NodeId, b: NodeId) CompileError!NodeId {
        return self.mk(.{ .Alt = .{ .a = a, .b = b } });
    }

    fn mkRepeat(self: *Parser, node: NodeId, kind: RepeatKind) CompileError!NodeId {
        return self.mk(.{ .Repeat = .{ .node = node, .kind = kind } });
    }

    fn mkGroup(self: *Parser, id: usize, node: NodeId) CompileError!NodeId {
        return self.mk(.{ .Group = .{ .id = id, .node = node } });
    }

    fn parseRegex(self: *Parser) CompileError!NodeId {
        return self.parseAlt();
    }

    fn parseAlt(self: *Parser) CompileError!NodeId {
        var left = try self.parseConcat();
        while (self.accept('|')) {
            const right = try self.parseConcat();
            left = try self.mkAlt(left, right);
        }
        return left;
    }

    fn parseConcat(self: *Parser) CompileError!NodeId {
        // Concatenation is implicit: parse as a left-associative chain.
        var have_any = false;
        var acc: NodeId = undefined;

        while (true) {
            const ch_opt = self.peek();
            if (ch_opt == null) break;
            const ch = ch_opt.?;
            if (ch == ')' or ch == '|') break;

            const node = try self.parseRepeat();
            if (!have_any) {
                acc = node;
                have_any = true;
            } else {
                acc = try self.mkConcat(acc, node);
            }
        }

        if (!have_any) return self.mk(.{ .Empty = {} });
        return acc;
    }

    fn parseRepeat(self: *Parser) CompileError!NodeId {
        var node = try self.parseAtom();

        const ch_opt = self.peek();
        if (ch_opt == null) return node;

        switch (ch_opt.?) {
            '*' => {
                _ = self.next();
                node = try self.mkRepeat(node, .Star);
            },
            '+' => {
                _ = self.next();
                node = try self.mkRepeat(node, .Plus);
            },
            '?' => {
                _ = self.next();
                node = try self.mkRepeat(node, .Maybe);
            },
            else => {},
        }

        return node;
    }

    fn parseAtom(self: *Parser) CompileError!NodeId {
        const ch = self.next() orelse return CompileError.BadPattern;
        return switch (ch) {
            '(' => blk: {
                self.group_count += 1;
                const id = self.group_count;
                const inner = try self.parseAlt();
                if (!self.accept(')')) return CompileError.UnbalancedParen;
                break :blk try self.mkGroup(id, inner);
            },
            ')' => CompileError.UnbalancedParen,
            '.' => self.mk(.{ .Any = {} }),
            '^' => self.mk(.{ .AssertBol = {} }),
            '$' => self.mk(.{ .AssertEol = {} }),
            '[' => self.parseClass(),
            '\\' => self.parseEscape(false),
            else => self.mk(.{ .Lit = ch }),
        };
    }

    fn parseEscape(self: *Parser, in_class: bool) CompileError!NodeId {
        const esc = self.next() orelse return CompileError.BadPattern;
        if (in_class) {
            // In a class, most escapes are treated as literals, except a few shorthands.
            return switch (esc) {
                'd' => self.mkClassFromBits(classDigits(self.icase), false),
                's' => self.mkClassFromBits(classWhitespace(self.icase), false),
                'w' => self.mkClassFromBits(classWord(self.icase), false),
                'b' => self.mk(.{ .Lit = 0x08 }), // backspace in Java character classes
                'n' => self.mk(.{ .Lit = '\n' }),
                'r' => self.mk(.{ .Lit = '\r' }),
                't' => self.mk(.{ .Lit = '\t' }),
                'f' => self.mk(.{ .Lit = 0x0C }),
                'v' => self.mk(.{ .Lit = 0x0B }),
                else => self.mk(.{ .Lit = esc }),
            };
        }

        return switch (esc) {
            'd' => self.mkClassFromBits(classDigits(self.icase), false),
            's' => self.mkClassFromBits(classWhitespace(self.icase), false),
            'w' => self.mkClassFromBits(classWord(self.icase), false),
            'b' => self.mk(.{ .AssertWordBoundary = {} }),
            'R' => self.mkR(),
            'n' => self.mk(.{ .Lit = '\n' }),
            'r' => self.mk(.{ .Lit = '\r' }),
            't' => self.mk(.{ .Lit = '\t' }),
            'f' => self.mk(.{ .Lit = 0x0C }),
            'v' => self.mk(.{ .Lit = 0x0B }),
            else => self.mk(.{ .Lit = esc }),
        };
    }

    fn mkClassFromBits(self: *Parser, bits: [16]u8, negated: bool) CompileError!NodeId {
        return self.mk(.{ .Class = .{ .set = bits, .negated = negated } });
    }

    fn parseClass(self: *Parser) CompileError!NodeId {
        var negated = false;
        if (self.accept('^')) negated = true;

        var set = std.mem.zeroes([16]u8);
        var have_any = false;

        var prev_char: ?u8 = null;
        var prev_was_range_dash = false;

        while (true) {
            const ch_opt = self.peek();
            if (ch_opt == null) return CompileError.UnbalancedBracket;
            const ch = ch_opt.?;

            if (ch == ']' and have_any) {
                _ = self.next();
                break;
            }

            var item_ch: u8 = undefined;
            var item_is_class = false;
            var item_class_bits: [16]u8 = undefined;

            if (ch == '\\') {
                _ = self.next();
                // parseEscape in-class.
                const before = self.i;
                const node_id = try self.parseEscape(true);
                // If escape produced a literal, extract it; otherwise merge its class.
                const node = self.nodes.items[node_id];
                switch (node) {
                    .Lit => |c| {
                        item_ch = c;
                    },
                    .Class => |cls| {
                        item_is_class = true;
                        item_class_bits = cls.set;
                        // Note: nested negation not supported.
                        _ = cls.negated;
                    },
                    else => {
                        self.i = before;
                        return CompileError.BadPattern;
                    },
                }
            } else {
                _ = self.next();
                item_ch = ch;
            }

            have_any = true;

            if (item_is_class) {
                // If we were building a range, treat '-' literally.
                if (prev_was_range_dash) {
                    bitsetAdd(&set, '-');
                    prev_was_range_dash = false;
                }
                // Merge class bits.
                var j: usize = 0;
                while (j < set.len) : (j += 1) set[j] |= item_class_bits[j];
                prev_char = null;
                continue;
            }

            if (prev_was_range_dash) {
                // Range: prev_char - item_ch
                const start = prev_char orelse item_ch;
                const end = item_ch;
                if (start <= end) {
                    var c: u8 = start;
                    while (c <= end) : (c += 1) {
                        bitsetAddIcase(&set, c, self.icase);
                        if (c == 255) break;
                    }
                } else {
                    var c: u8 = end;
                    while (c <= start) : (c += 1) {
                        bitsetAddIcase(&set, c, self.icase);
                        if (c == 255) break;
                    }
                }
                prev_char = null;
                prev_was_range_dash = false;
                continue;
            }

            if (item_ch == '-' and prev_char != null and self.peek() != null and self.peek().? != ']') {
                prev_was_range_dash = true;
                continue;
            }

            bitsetAddIcase(&set, item_ch, self.icase);
            prev_char = item_ch;
        }

        if (prev_was_range_dash) {
            // Trailing '-' is literal.
            bitsetAdd(&set, '-');
        }

        return self.mk(.{ .Class = .{ .set = set, .negated = negated } });
    }

    fn mkR(self: *Parser) CompileError!NodeId {
        // `\R` is a shorthand for Unicode linebreak sequences.
        // In ASCII bring-up, we support CRLF, LF, CR, VT, FF.
        const cr = try self.mk(.{ .Lit = '\r' });
        const lf = try self.mk(.{ .Lit = '\n' });
        const vt = try self.mk(.{ .Lit = 0x0B });
        const ff = try self.mk(.{ .Lit = 0x0C });
        const crlf = try self.mkConcat(cr, lf);
        return self.mkAlt(crlf, try self.mkAlt(lf, try self.mkAlt(cr, try self.mkAlt(vt, ff))));
    }
};

fn bitsetAdd(set: *[16]u8, ch: u8) void {
    if (ch >= 128) return;
    const idx: usize = ch >> 3;
    const bit: u8 = @as(u8, 1) << @intCast(ch & 7);
    set[idx] |= bit;
}

fn bitsetAddIcase(set: *[16]u8, ch: u8, icase: bool) void {
    bitsetAdd(set, ch);
    if (!icase) return;
    const lo = std.ascii.toLower(ch);
    const up = std.ascii.toUpper(ch);
    bitsetAdd(set, lo);
    bitsetAdd(set, up);
}

fn bitsetHas(set: [16]u8, ch: u8) bool {
    if (ch >= 128) return false;
    const idx: usize = ch >> 3;
    const bit: u8 = @as(u8, 1) << @intCast(ch & 7);
    return (set[idx] & bit) != 0;
}

fn classDigits(icase: bool) [16]u8 {
    _ = icase;
    var set = std.mem.zeroes([16]u8);
    var c: u8 = '0';
    while (c <= '9') : (c += 1) {
        bitsetAdd(&set, c);
    }
    return set;
}

fn classWhitespace(icase: bool) [16]u8 {
    _ = icase;
    var set = std.mem.zeroes([16]u8);
    bitsetAdd(&set, ' ');
    bitsetAdd(&set, '\t');
    bitsetAdd(&set, '\n');
    bitsetAdd(&set, '\r');
    bitsetAdd(&set, 0x0B);
    bitsetAdd(&set, 0x0C);
    return set;
}

fn classWord(icase: bool) [16]u8 {
    var set = std.mem.zeroes([16]u8);
    var c: u8 = '0';
    while (c <= '9') : (c += 1) bitsetAdd(&set, c);
    c = 'a';
    while (c <= 'z') : (c += 1) bitsetAddIcase(&set, c, icase);
    c = 'A';
    while (c <= 'Z') : (c += 1) bitsetAddIcase(&set, c, icase);
    bitsetAdd(&set, '_');
    return set;
}

fn isWordChar(ch: u8) bool {
    return (ch >= '0' and ch <= '9') or (ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z') or ch == '_';
}

fn isWordBoundary(input: []const u8, pos: usize) bool {
    const prev_word = if (pos == 0) false else isWordChar(input[pos - 1]);
    const next_word = if (pos >= input.len) false else isWordChar(input[pos]);
    return prev_word != next_word;
}

// -----------------------------------------------------------------------------
// Thompson-style compiler
// -----------------------------------------------------------------------------

const Patch = struct { inst: usize, slot: u1 };

const Frag = struct {
    start: usize,
    outs: std.ArrayListUnmanaged(Patch) = .{},

    fn deinit(self: *Frag, alloc: std.mem.Allocator) void {
        self.outs.deinit(alloc);
    }
};

const Builder = struct {
    alloc: std.mem.Allocator,
    insts: std.ArrayList(Inst) = .empty,

    fn deinit(self: *Builder) void {
        self.insts.deinit(self.alloc);
    }

    fn emit(self: *Builder, inst: Inst) CompileError!usize {
        self.insts.append(self.alloc, inst) catch return CompileError.OutOfMemory;
        return self.insts.items.len - 1;
    }

    fn list1(self: *Builder, p: Patch) CompileError!std.ArrayListUnmanaged(Patch) {
        var out: std.ArrayListUnmanaged(Patch) = .{};
        out.append(self.alloc, p) catch return CompileError.OutOfMemory;
        return out;
    }

    fn appendLists(self: *Builder, a: *std.ArrayListUnmanaged(Patch), b: *std.ArrayListUnmanaged(Patch)) CompileError!void {
        if (b.items.len == 0) return;
        a.appendSlice(self.alloc, b.items) catch return CompileError.OutOfMemory;
        b.clearAndFree(self.alloc);
    }

    fn patchAll(self: *Builder, outs: *std.ArrayListUnmanaged(Patch), target: u32) void {
        for (outs.items) |p| {
            const inst_ptr: *Inst = &self.insts.items[p.inst];
            instSetOut(inst_ptr, p.slot, target);
        }
        outs.clearAndFree(self.alloc);
    }

    fn compileNode(self: *Builder, nodes: []const Node, id: NodeId) CompileError!Frag {
        const node = nodes[@intCast(id)];
        switch (node) {
            .Empty => {
                const idx = try self.emit(.{ .Jmp = .{ .out = Unset } });
                return .{ .start = idx, .outs = try self.list1(.{ .inst = idx, .slot = 0 }) };
            },
            .Lit => |ch| {
                const idx = try self.emit(.{ .Char = .{ .ch = ch, .out = Unset } });
                return .{ .start = idx, .outs = try self.list1(.{ .inst = idx, .slot = 0 }) };
            },
            .Any => {
                const idx = try self.emit(.{ .Any = .{ .out = Unset } });
                return .{ .start = idx, .outs = try self.list1(.{ .inst = idx, .slot = 0 }) };
            },
            .Class => |cls| {
                const idx = try self.emit(.{ .Class = .{ .set = cls.set, .negated = cls.negated, .out = Unset } });
                return .{ .start = idx, .outs = try self.list1(.{ .inst = idx, .slot = 0 }) };
            },
            .Concat => |cat| {
                var f1 = try self.compileNode(nodes, cat.a);
                defer f1.deinit(self.alloc);
                const f2 = try self.compileNode(nodes, cat.b);
                // Patch f1 outs to f2 start.
                self.patchAll(&f1.outs, @intCast(f2.start));
                return .{ .start = f1.start, .outs = f2.outs };
            },
            .Alt => |alt| {
                var f1 = try self.compileNode(nodes, alt.a);
                var f2 = try self.compileNode(nodes, alt.b);
                const idx = try self.emit(.{ .Split = .{ .out = @intCast(f1.start), .out1 = @intCast(f2.start) } });
                try self.appendLists(&f1.outs, &f2.outs);
                f2.deinit(self.alloc);
                return .{ .start = idx, .outs = f1.outs };
            },
            .Repeat => |rep| {
                var sub = try self.compileNode(nodes, rep.node);
                defer sub.deinit(self.alloc);
                switch (rep.kind) {
                    .Star => {
                        const split_idx = try self.emit(.{ .Split = .{ .out = @intCast(sub.start), .out1 = Unset } });
                        self.patchAll(&sub.outs, @intCast(split_idx));
                        return .{ .start = split_idx, .outs = try self.list1(.{ .inst = split_idx, .slot = 1 }) };
                    },
                    .Plus => {
                        const split_idx = try self.emit(.{ .Split = .{ .out = @intCast(sub.start), .out1 = Unset } });
                        self.patchAll(&sub.outs, @intCast(split_idx));
                        return .{ .start = sub.start, .outs = try self.list1(.{ .inst = split_idx, .slot = 1 }) };
                    },
                    .Maybe => {
                        const split_idx = try self.emit(.{ .Split = .{ .out = @intCast(sub.start), .out1 = Unset } });
                        var outs = try self.list1(.{ .inst = split_idx, .slot = 1 });
                        try self.appendLists(&outs, &sub.outs);
                        return .{ .start = split_idx, .outs = outs };
                    },
                }
            },
            .Group => |g| {
                const start_slot: u16 = @intCast(2 * g.id);
                const end_slot: u16 = @intCast(2 * g.id + 1);

                const save_start = try self.emit(.{ .Save = .{ .slot = start_slot, .out = Unset } });
                var inner = try self.compileNode(nodes, g.node);
                defer inner.deinit(self.alloc);
                const save_end = try self.emit(.{ .Save = .{ .slot = end_slot, .out = Unset } });

                instSetOut(&self.insts.items[save_start], 0, @intCast(inner.start));
                self.patchAll(&inner.outs, @intCast(save_end));

                return .{ .start = save_start, .outs = try self.list1(.{ .inst = save_end, .slot = 0 }) };
            },
            .AssertBol => {
                const idx = try self.emit(.{ .AssertBol = .{ .out = Unset } });
                return .{ .start = idx, .outs = try self.list1(.{ .inst = idx, .slot = 0 }) };
            },
            .AssertEol => {
                const idx = try self.emit(.{ .AssertEol = .{ .out = Unset } });
                return .{ .start = idx, .outs = try self.list1(.{ .inst = idx, .slot = 0 }) };
            },
            .AssertWordBoundary => {
                const idx = try self.emit(.{ .AssertWordBoundary = .{ .out = Unset } });
                return .{ .start = idx, .outs = try self.list1(.{ .inst = idx, .slot = 0 }) };
            },
        }
    }
};

fn instSetOut(inst: *Inst, slot: u1, target: u32) void {
    switch (inst.*) {
        .Char => |*x| x.out = target,
        .Any => |*x| x.out = target,
        .Class => |*x| x.out = target,
        .Jmp => |*x| x.out = target,
        .Save => |*x| x.out = target,
        .AssertBol => |*x| x.out = target,
        .AssertEol => |*x| x.out = target,
        .AssertWordBoundary => |*x| x.out = target,
        .Split => |*x| {
            if (slot == 0) x.out = target else x.out1 = target;
        },
        .Match => {},
    }
}

// -----------------------------------------------------------------------------
// Executor
// -----------------------------------------------------------------------------

const Thread = struct {
    pc: u32,
    caps: []i32,
};

const ThreadList = struct {
    threads: std.ArrayList(Thread) = .empty,

    fn deinit(self: *ThreadList, alloc: std.mem.Allocator) void {
        self.threads.deinit(alloc);
    }

    fn clear(self: *ThreadList) void {
        self.threads.clearRetainingCapacity();
    }

    fn append(self: *ThreadList, alloc: std.mem.Allocator, t: Thread) void {
        self.threads.append(alloc, t) catch @panic("oom");
    }

    fn firstMatchIndex(self: *const ThreadList, prog: *const Program) ?usize {
        for (self.threads.items, 0..) |t, idx| {
            switch (prog.insts[@intCast(t.pc)]) {
                .Match => return idx,
                else => {},
            }
        }
        return null;
    }
};

fn runAt(alloc: std.mem.Allocator, prog: *const Program, input: []const u8, start: usize, caps_out: []i32) bool {
    const ncap = prog.ncap;
    const inst_count = prog.insts.len;

    var clist: ThreadList = .{};
    defer clist.deinit(alloc);
    var nlist: ThreadList = .{};
    defer nlist.deinit(alloc);

    const visited = alloc.alloc(bool, inst_count) catch @panic("oom");
    @memset(visited, false);

    const init_caps = alloc.alloc(i32, ncap) catch @panic("oom");
    @memset(init_caps, -1);

    addThread(alloc, prog, &clist, 0, init_caps, start, input, visited);

    var best_caps: ?[]i32 = null;

    var pos: usize = start;
    while (true) {
        if (clist.firstMatchIndex(prog)) |mi| {
            const caps = clist.threads.items[mi].caps;
            if (mi == 0) {
                std.mem.copyForwards(i32, caps_out[0..ncap], caps[0..ncap]);
                return true;
            }
            best_caps = caps;
        }

        if (pos >= input.len) break;

        nlist.clear();
        @memset(visited, false);

        const ch = input[pos];
        for (clist.threads.items) |t| {
            const inst = prog.insts[@intCast(t.pc)];
            switch (inst) {
                .Char => |x| {
                    if (matchChar(prog.icase, x.ch, ch)) {
                        addThread(alloc, prog, &nlist, x.out, t.caps, pos + 1, input, visited);
                    }
                },
                .Any => |x| {
                    if (matchAny(ch)) {
                        addThread(alloc, prog, &nlist, x.out, t.caps, pos + 1, input, visited);
                    }
                },
                .Class => |x| {
                    if (matchClass(x.set, x.negated, ch)) {
                        addThread(alloc, prog, &nlist, x.out, t.caps, pos + 1, input, visited);
                    }
                },
                .Match => {},
                else => {},
            }
        }

        if (nlist.threads.items.len == 0) break;
        // swap
        const tmp = clist;
        clist = nlist;
        nlist = tmp;
        pos += 1;
    }

    if (best_caps) |caps| {
        std.mem.copyForwards(i32, caps_out[0..ncap], caps[0..ncap]);
        return true;
    }

    return false;
}

fn addThread(
    alloc: std.mem.Allocator,
    prog: *const Program,
    list: *ThreadList,
    pc_raw: u32,
    caps: []const i32,
    pos: usize,
    input: []const u8,
    visited: []bool,
) void {
    const pc: usize = @intCast(pc_raw);
    if (pc >= prog.insts.len) return;
    if (visited[pc]) return;
    visited[pc] = true;

    const inst = prog.insts[pc];
    switch (inst) {
        .Split => |x| {
            addThread(alloc, prog, list, x.out, caps, pos, input, visited);
            addThread(alloc, prog, list, x.out1, caps, pos, input, visited);
        },
        .Jmp => |x| addThread(alloc, prog, list, x.out, caps, pos, input, visited),
        .Save => |x| {
            const new_caps = alloc.alloc(i32, caps.len) catch @panic("oom");
            std.mem.copyForwards(i32, new_caps, caps);
            if (@as(usize, x.slot) < new_caps.len) new_caps[@intCast(x.slot)] = @intCast(pos);
            addThread(alloc, prog, list, x.out, new_caps, pos, input, visited);
        },
        .AssertBol => |x| {
            if (pos == 0) addThread(alloc, prog, list, x.out, caps, pos, input, visited);
        },
        .AssertEol => |x| {
            if (pos == input.len) addThread(alloc, prog, list, x.out, caps, pos, input, visited);
        },
        .AssertWordBoundary => |x| {
            if (isWordBoundary(input, pos)) addThread(alloc, prog, list, x.out, caps, pos, input, visited);
        },
        else => list.append(alloc, .{ .pc = pc_raw, .caps = @constCast(caps) }),
    }
}

fn matchChar(icase: bool, pat: u8, ch: u8) bool {
    if (!icase) return pat == ch;
    return std.ascii.toLower(pat) == std.ascii.toLower(ch);
}

fn matchAny(ch: u8) bool {
    // Java default: '.' does not match line terminators (ASCII subset).
    return ch != '\n' and ch != '\r' and ch != 0x0B and ch != 0x0C;
}

fn matchClass(set: [16]u8, negated: bool, ch: u8) bool {
    const hit = bitsetHas(set, ch);
    return if (negated) !hit else hit;
}
