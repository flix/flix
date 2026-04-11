const std = @import("std");

pub const version: u32 = 0;

pub const WireError = error{
    UnexpectedEof,
    InvalidVersion,
    InvalidInput,
};

pub const DecodeError = WireError || std.mem.Allocator.Error;

pub const Header = struct {
    key: []const u8,
    value: []const u8,
};

pub const RequestView = struct {
    method: []const u8,
    url: []const u8,
    headers: []Header, // key/value pairs
    has_body: bool,
    body: []const u8,

    pub fn deinit(self: *RequestView, alloc: std.mem.Allocator) void {
        alloc.free(self.headers);
        self.* = undefined;
    }
};

pub const ResponseView = struct {
    ok: bool,
    status: i32,
    headers: []Header, // key/value pairs
    body: []const u8,
    err_kind: i32,
    err_msg: []const u8,

    pub fn deinit(self: *ResponseView, alloc: std.mem.Allocator) void {
        alloc.free(self.headers);
        self.* = undefined;
    }
};

fn ensure(cond: bool) WireError!void {
    if (!cond) return error.InvalidInput;
}

fn readBytes(blob: []const u8, idx: *usize, n: usize) WireError![]const u8 {
    if (idx.* + n > blob.len) return error.UnexpectedEof;
    const out = blob[idx.* .. idx.* + n];
    idx.* += n;
    return out;
}

fn readU8(blob: []const u8, idx: *usize) WireError!u8 {
    const b = try readBytes(blob, idx, 1);
    return b[0];
}

fn readU32(blob: []const u8, idx: *usize) WireError!u32 {
    const b = try readBytes(blob, idx, 4);
    const p: *const [4]u8 = @ptrCast(b.ptr);
    return std.mem.readInt(u32, p, .little);
}

fn readI32(blob: []const u8, idx: *usize) WireError!i32 {
    const b = try readBytes(blob, idx, 4);
    const p: *const [4]u8 = @ptrCast(b.ptr);
    return std.mem.readInt(i32, p, .little);
}

fn readString(blob: []const u8, idx: *usize) WireError![]const u8 {
    const len = try readU32(blob, idx);
    return readBytes(blob, idx, @intCast(len));
}

fn appendU8(out: *std.ArrayList(u8), alloc: std.mem.Allocator, x: u8) std.mem.Allocator.Error!void {
    try out.append(alloc, x);
}

fn appendU32(out: *std.ArrayList(u8), alloc: std.mem.Allocator, x: u32) std.mem.Allocator.Error!void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, x, .little);
    try out.appendSlice(alloc, &buf);
}

fn appendI32(out: *std.ArrayList(u8), alloc: std.mem.Allocator, x: i32) std.mem.Allocator.Error!void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(i32, &buf, x, .little);
    try out.appendSlice(alloc, &buf);
}

fn appendString(out: *std.ArrayList(u8), alloc: std.mem.Allocator, s: []const u8) std.mem.Allocator.Error!void {
    try appendU32(out, alloc, @intCast(s.len));
    try out.appendSlice(alloc, s);
}

pub fn decodeRequest(alloc: std.mem.Allocator, blob: []const u8) DecodeError!RequestView {
    var i: usize = 0;
    const v = try readU32(blob, &i);
    if (v != version) return error.InvalidVersion;

    const method = try readString(blob, &i);
    const url = try readString(blob, &i);

    const header_count = try readU32(blob, &i);
    try ensure((header_count % 2) == 0);
    const pair_count: usize = @intCast(header_count / 2);

    var headers = try alloc.alloc(Header, pair_count);
    errdefer alloc.free(headers);

    var p: usize = 0;
    while (p < pair_count) : (p += 1) {
        const k = try readString(blob, &i);
        const v_ = try readString(blob, &i);
        headers[p] = .{ .key = k, .value = v_ };
    }

    const has_body_raw = try readU8(blob, &i);
    try ensure(has_body_raw == 0 or has_body_raw == 1);
    const has_body = has_body_raw == 1;

    const body = try readString(blob, &i);
    if (!has_body) try ensure(body.len == 0);

    // No trailing bytes allowed in v0.
    try ensure(i == blob.len);

    return .{
        .method = method,
        .url = url,
        .headers = headers,
        .has_body = has_body,
        .body = body,
    };
}

pub fn decodeResponse(alloc: std.mem.Allocator, blob: []const u8) DecodeError!ResponseView {
    var i: usize = 0;
    const v = try readU32(blob, &i);
    if (v != version) return error.InvalidVersion;

    const ok_raw = try readU8(blob, &i);
    try ensure(ok_raw == 0 or ok_raw == 1);
    const ok = ok_raw == 1;

    const status = try readI32(blob, &i);

    const header_count = try readU32(blob, &i);
    try ensure((header_count % 2) == 0);
    const pair_count: usize = @intCast(header_count / 2);

    var headers = try alloc.alloc(Header, pair_count);
    errdefer alloc.free(headers);

    var p: usize = 0;
    while (p < pair_count) : (p += 1) {
        const k = try readString(blob, &i);
        const v_ = try readString(blob, &i);
        headers[p] = .{ .key = k, .value = v_ };
    }

    const body = try readString(blob, &i);
    const err_kind = try readI32(blob, &i);
    const err_msg = try readString(blob, &i);

    // No trailing bytes allowed in v0.
    try ensure(i == blob.len);

    return .{
        .ok = ok,
        .status = status,
        .headers = headers,
        .body = body,
        .err_kind = err_kind,
        .err_msg = err_msg,
    };
}

pub fn encodeRequest(
    alloc: std.mem.Allocator,
    method: []const u8,
    url: []const u8,
    headers: []const Header,
    has_body: bool,
    body: []const u8,
) std.mem.Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(alloc);

    try appendU32(&out, alloc, version);
    try appendString(&out, alloc, method);
    try appendString(&out, alloc, url);

    try appendU32(&out, alloc, @intCast(headers.len * 2));
    for (headers) |h| {
        try appendString(&out, alloc, h.key);
        try appendString(&out, alloc, h.value);
    }

    try appendU8(&out, alloc, if (has_body) 1 else 0);
    try appendString(&out, alloc, body);

    return out.toOwnedSlice(alloc);
}

pub fn encodeResponse(
    alloc: std.mem.Allocator,
    ok: bool,
    status: i32,
    headers: []const Header,
    body: []const u8,
    err_kind: i32,
    err_msg: []const u8,
) std.mem.Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(alloc);

    try appendU32(&out, alloc, version);
    try appendU8(&out, alloc, if (ok) 1 else 0);
    try appendI32(&out, alloc, status);

    try appendU32(&out, alloc, @intCast(headers.len * 2));
    for (headers) |h| {
        try appendString(&out, alloc, h.key);
        try appendString(&out, alloc, h.value);
    }

    try appendString(&out, alloc, body);
    try appendI32(&out, alloc, err_kind);
    try appendString(&out, alloc, err_msg);

    return out.toOwnedSlice(alloc);
}
