const std = @import("std");
const builtin = @import("builtin");

const async_wait = @import("async_wait_v0.zig");
const wire = @import("http_wire_v0.zig");

const IoErrorKind = struct {
    const connection_failed: i32 = 1;
    const interrupted: i32 = 2;
    const invalid_input: i32 = 4;
    const not_found: i32 = 6;
    const permission_denied: i32 = 9;
    const timeout: i32 = 10;
    const unsupported: i32 = 12;
    const unknown_host: i32 = 13;
    const other: i32 = 14;
};

pub const BlockingOutcome = union(enum) {
    completed: []u8,
    canceled: void,
};

pub const AsyncOutcome = enum(i32) {
    completed = 0,
    canceled = 1,
};

pub const AsyncAwaitResult = struct {
    outcome: AsyncOutcome,
    response_blob: ?[]u8,
};

fn freeResponseBlob(blob: []u8) void {
    std.heap.c_allocator.free(blob);
}

const HttpWaitState = async_wait.StickyCancelWait([]u8, freeResponseBlob);

pub const CancelToken = struct {
    mutex: std.Thread.Mutex = .{},
    cancel_requested: bool = false,
    current_connection: ?*std.http.Client.Connection = null,

    pub fn isCanceled(self: *CancelToken) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.cancel_requested;
    }

    pub fn requestCancel(self: *CancelToken) void {
        self.mutex.lock();
        if (!self.cancel_requested) {
            self.cancel_requested = true;
            if (self.current_connection) |conn| {
                conn.closing = true;
                std.posix.shutdown(conn.stream_reader.getStream().handle, .both) catch {};
            }
        }
        self.mutex.unlock();
    }

    pub fn publishConnection(self: *CancelToken, connection: *std.http.Client.Connection) bool {
        self.mutex.lock();
        const canceled = self.cancel_requested;
        if (!canceled) {
            self.current_connection = connection;
        } else {
            connection.closing = true;
        }
        self.mutex.unlock();
        return canceled;
    }

    pub fn clearConnection(self: *CancelToken, connection: ?*std.http.Client.Connection) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (connection) |conn| {
            if (self.current_connection) |current| {
                if (current == conn) {
                    self.current_connection = null;
                }
            }
        } else {
            self.current_connection = null;
        }
    }
};

pub const AsyncRequest = struct {
    wait: HttpWaitState = .{},
    req_blob: []u8,
    cancel_token: CancelToken = .{},
};

fn encodeErrorResponse(alloc: std.mem.Allocator, kind: i32, msg: []const u8) std.mem.Allocator.Error![]u8 {
    return wire.encodeResponse(alloc, false, 0, &.{}, "", kind, msg);
}

fn httpKindFromErr(err: anyerror) i32 {
    return switch (err) {
        error.UnexpectedCharacter, error.InvalidFormat, error.InvalidPort => IoErrorKind.invalid_input,
        error.UriMissingHost, error.UriHostTooLong => IoErrorKind.invalid_input,

        error.UnsupportedUriScheme => IoErrorKind.unsupported,
        error.HttpContentEncodingUnsupported => IoErrorKind.unsupported,
        error.UnsupportedCompressionMethod => IoErrorKind.unsupported,

        error.AccessDenied => IoErrorKind.permission_denied,
        error.ConnectionTimedOut => IoErrorKind.timeout,

        error.UnknownHostName,
        error.HostLacksNetworkAddresses,
        error.TemporaryNameServerFailure,
        error.NameServerFailure,
        => IoErrorKind.unknown_host,

        error.ConnectionRefused,
        error.NetworkUnreachable,
        error.ConnectionResetByPeer,
        error.UnexpectedConnectFailure,
        => IoErrorKind.connection_failed,

        error.FileNotFound => IoErrorKind.not_found,

        error.OutOfMemory => IoErrorKind.other,

        else => IoErrorKind.other,
    };
}

fn isPortableRedirectStatus(status: std.http.Status) bool {
    return switch (status) {
        .moved_permanently, .found, .see_other, .temporary_redirect, .permanent_redirect => true,
        else => false,
    };
}

fn wasCanceled(token_opt: ?*CancelToken) bool {
    return if (token_opt) |token| token.isCanceled() else false;
}

fn maybePublishConnection(token_opt: ?*CancelToken, connection: ?*std.http.Client.Connection) bool {
    if (token_opt) |token| {
        if (connection) |conn| {
            return token.publishConnection(conn);
        }
    }
    return false;
}

fn maybeClearConnection(token_opt: ?*CancelToken, connection: ?*std.http.Client.Connection) void {
    if (token_opt) |token| {
        token.clearConnection(connection);
    }
}

pub fn httpRequest(alloc: std.mem.Allocator, req_blob: []const u8, token_opt: ?*CancelToken) std.mem.Allocator.Error!BlockingOutcome {
    if (wasCanceled(token_opt)) return .canceled;

    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const scratch = arena.allocator();

    const decoded = wire.decodeRequest(scratch, req_blob) catch |err| {
        if (err == error.OutOfMemory) return error.OutOfMemory;
        const kind: i32 = switch (err) {
            error.InvalidInput => IoErrorKind.invalid_input,
            error.InvalidVersion => IoErrorKind.unsupported,
            error.UnexpectedEof => IoErrorKind.invalid_input,
            else => IoErrorKind.other,
        };
        return .{ .completed = try encodeErrorResponse(alloc, kind, @errorName(err)) };
    };

    if (!decoded.has_body and decoded.body.len != 0) {
        return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.invalid_input, "invalid input") };
    }

    const method_up = scratch.alloc(u8, decoded.method.len) catch return error.OutOfMemory;
    for (decoded.method, 0..) |ch, i| method_up[i] = std.ascii.toUpper(ch);
    const method0 = std.meta.stringToEnum(std.http.Method, method_up) orelse {
        return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.invalid_input, "invalid method") };
    };

    const uri0 = std.Uri.parse(decoded.url) catch |err| {
        return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.invalid_input, @errorName(err)) };
    };

    if (!std.ascii.eqlIgnoreCase(uri0.scheme, "http") and !std.ascii.eqlIgnoreCase(uri0.scheme, "https")) {
        return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.unsupported, "unsupported URL scheme") };
    }

    const headers = scratch.alloc(std.http.Header, decoded.headers.len) catch return error.OutOfMemory;
    for (decoded.headers, 0..) |h, i| {
        headers[i] = .{
            .name = h.key,
            .value = h.value,
        };
    }

    var client: std.http.Client = .{ .allocator = std.heap.c_allocator };
    defer client.deinit();

    const original_is_https = std.ascii.eqlIgnoreCase(uri0.scheme, "https");

    var cur_uri = uri0;
    var cur_method = method0;
    var cur_body: ?[]const u8 = if (decoded.has_body) decoded.body else null;
    var redirects_left: usize = 5;

    while (true) {
        if (wasCanceled(token_opt)) return .canceled;

        var req = client.request(cur_method, cur_uri, .{
            .redirect_behavior = .unhandled,
            .keep_alive = true,
            .headers = .{ .accept_encoding = .omit },
            .extra_headers = headers,
            .privileged_headers = &.{},
        }) catch |err| {
            if (wasCanceled(token_opt)) return .canceled;
            return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
        };
        defer req.deinit();

        if (maybePublishConnection(token_opt, req.connection)) {
            return .canceled;
        }
        defer maybeClearConnection(token_opt, req.connection);

        if (cur_body) |payload| {
            req.transfer_encoding = .{ .content_length = payload.len };
            var bw = req.sendBodyUnflushed(&.{}) catch |err| {
                if (wasCanceled(token_opt)) return .canceled;
                return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
            };
            bw.writer.writeAll(payload) catch |err| {
                if (wasCanceled(token_opt)) return .canceled;
                return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
            };
            bw.end() catch |err| {
                if (wasCanceled(token_opt)) return .canceled;
                return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
            };
            req.connection.?.flush() catch |err| {
                if (wasCanceled(token_opt)) return .canceled;
                return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
            };
        } else {
            req.sendBodiless() catch |err| {
                if (wasCanceled(token_opt)) return .canceled;
                return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
            };
        }

        var resp = req.receiveHead(&.{}) catch |err| {
            if (wasCanceled(token_opt)) return .canceled;
            return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
        };

        const status = resp.head.status;

        if (isPortableRedirectStatus(status)) {
            if (resp.head.location) |location| {
                const loc_uri = std.Uri.parse(location) catch std.Uri.parseAfterScheme("", location) catch |err| {
                    return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.invalid_input, @errorName(err)) };
                };
                const loc_scheme = loc_uri.scheme;

                if (original_is_https and loc_scheme.len != 0 and std.ascii.eqlIgnoreCase(loc_scheme, "http")) {
                    return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.permission_denied, "redirect disallowed: https -> http") };
                }

                if (loc_scheme.len != 0 and !std.ascii.eqlIgnoreCase(loc_scheme, "http") and !std.ascii.eqlIgnoreCase(loc_scheme, "https")) {
                    return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.unsupported, "unsupported redirect scheme") };
                }

                if (redirects_left == 0) {
                    return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.other, "redirect not followed") };
                }
                redirects_left -= 1;

                const base_path_len: usize = switch (cur_uri.path) {
                    .raw => |s| s.len,
                    .percent_encoded => |s| s.len,
                };
                const buf_len: usize = @max(@as(usize, 8192), location.len + base_path_len + 16);
                const buf = scratch.alloc(u8, buf_len) catch return error.OutOfMemory;
                var aux_buf: []u8 = buf;
                @memcpy(aux_buf[0..location.len], location);
                const new_uri = cur_uri.resolveInPlace(location.len, &aux_buf) catch |err| {
                    return .{ .completed = try encodeErrorResponse(alloc, IoErrorKind.other, @errorName(err)) };
                };

                if (status == .see_other or ((status == .moved_permanently or status == .found) and cur_method == .POST)) {
                    cur_method = .GET;
                    cur_body = null;
                }

                cur_uri = new_uri;
                continue;
            }
        }

        var resp_headers: std.ArrayList(wire.Header) = .empty;
        defer resp_headers.deinit(scratch);

        var it = resp.head.iterateHeaders();
        while (it.next()) |h| {
            const lower = scratch.alloc(u8, h.name.len) catch return error.OutOfMemory;
            for (h.name, 0..) |ch, i| lower[i] = std.ascii.toLower(ch);
            resp_headers.append(scratch, .{ .key = lower, .value = h.value }) catch return error.OutOfMemory;
        }

        const should_read_body =
            cur_method != .HEAD and
            status.class() != .informational and
            status != .no_content and
            status != .reset_content and
            status != .not_modified;

        const body: []const u8 = if (!should_read_body) "" else blk: {
            var aw = std.Io.Writer.Allocating.init(scratch);

            var transfer_buf: [64]u8 = undefined;
            const reader = req.reader.bodyReader(&transfer_buf, resp.head.transfer_encoding, resp.head.content_length);
            _ = reader.streamRemaining(&aw.writer) catch |err| {
                if (wasCanceled(token_opt)) return .canceled;
                return .{ .completed = try encodeErrorResponse(alloc, httpKindFromErr(err), @errorName(err)) };
            };
            break :blk aw.written();
        };

        return .{ .completed = try wire.encodeResponse(alloc, true, @intCast(@intFromEnum(status)), resp_headers.items, body, IoErrorKind.other, "") };
    }
}

fn freeAsyncRequest(req: *AsyncRequest) void {
    req.wait.deinit();
    std.heap.c_allocator.free(req.req_blob);
    std.heap.c_allocator.destroy(req);
}

fn workerRelease(req: *AsyncRequest) void {
    if (req.wait.releaseRef()) {
        freeAsyncRequest(req);
    }
}

fn asyncWorkerMain(req: *AsyncRequest) void {
    const outcome = httpRequest(std.heap.c_allocator, req.req_blob, &req.cancel_token) catch @panic("oom");

    switch (outcome) {
        .completed => |blob| req.wait.workerComplete(blob),
        .canceled => req.wait.workerCancel(),
    }

    workerRelease(req);
}

pub fn httpRequestAsyncStart(req_blob: []const u8) *AsyncRequest {
    const req = std.heap.c_allocator.create(AsyncRequest) catch @panic("oom");
    req.* = .{
        .req_blob = std.heap.c_allocator.dupe(u8, req_blob) catch @panic("oom"),
    };

    const thread = std.Thread.spawn(.{}, asyncWorkerMain, .{req}) catch @panic("failed to spawn async HTTP request thread");
    thread.detach();
    return req;
}

pub fn httpRequestAsyncCancel(req: *AsyncRequest) void {
    if (req.wait.requestCancel()) {
        req.cancel_token.requestCancel();
    }
}

pub fn httpRequestAsyncAwait(req: *AsyncRequest) AsyncAwaitResult {
    return switch (req.wait.await()) {
        .completed => |blob| .{ .outcome = .completed, .response_blob = blob },
        .canceled => .{ .outcome = .canceled, .response_blob = null },
    };
}

pub fn httpRequestAsyncResponseBlob(req: *AsyncRequest) ?[]const u8 {
    return req.wait.peekCompletedPayload();
}

pub fn httpRequestAsyncRelease(req: *AsyncRequest) void {
    if (req.wait.releaseRef()) {
        freeAsyncRequest(req);
    }
}

const TestConnection = struct {
    client: std.http.Client = .{ .allocator = std.testing.allocator },
    stream: std.net.Stream = undefined,
    read_buffer: [128]u8 = undefined,
    write_buffer: [128]u8 = undefined,
    connection: std.http.Client.Connection = undefined,

    fn init(self: *TestConnection) !void {
        if (builtin.os.tag == .windows) return error.SkipZigTest;

        const sock_flags = std.posix.SOCK.STREAM |
            (if (builtin.os.tag == .windows) 0 else std.posix.SOCK.CLOEXEC);
        const fd = try std.posix.socket(std.posix.AF.INET, sock_flags, std.posix.IPPROTO.TCP);
        self.stream = .{ .handle = fd };
        self.connection = .{
            .client = &self.client,
            .stream_writer = self.stream.writer(self.write_buffer[0..]),
            .stream_reader = self.stream.reader(self.read_buffer[0..]),
            .pool_node = .{},
            .port = 0,
            .host_len = 0,
            .proxied = false,
            .closing = false,
            .protocol = .plain,
        };
    }

    fn deinit(self: *TestConnection) void {
        self.stream.close();
    }
};

test "cancel token marks late-published connection as closing after cancellation" {
    var tc: TestConnection = .{};
    try tc.init();
    defer tc.deinit();

    var token = CancelToken{};
    token.requestCancel();

    try std.testing.expect(token.publishConnection(&tc.connection));
    try std.testing.expect(tc.connection.closing);
}

test "cancel token closes published connection when cancellation becomes visible later" {
    var tc: TestConnection = .{};
    try tc.init();
    defer tc.deinit();

    var token = CancelToken{};

    try std.testing.expect(!token.publishConnection(&tc.connection));
    try std.testing.expect(!tc.connection.closing);
    try std.testing.expect(token.current_connection == &tc.connection);

    token.requestCancel();

    try std.testing.expect(tc.connection.closing);
    token.clearConnection(&tc.connection);
    try std.testing.expect(token.current_connection == null);
}
