const std = @import("std");
const builtin = @import("builtin");
const xev = @import("vendor/libxev/src/main.zig");
const async_wait = @import("async_wait_v0.zig");
const http = @import("http_request_std_wire_v0.zig");

const alloc = std.heap.c_allocator;

const TimerWaitOutcome = enum(i32) {
    expired = 0,
    canceled = 1,
};

const TimerWait = struct {
    ms: u64,
    mutex: std.Thread.Mutex = .{},
    cond: std.Thread.Condition = .{},
    timer: xev.Timer,
    completion: xev.Completion = .{},
    cancel_completion: xev.Completion = .{},
    next: ?*TimerWait = null,
    cancel_next: ?*TimerWait = null,
    started: bool = false,
    completed: bool = false,
    cancel_requested: bool = false,
    cancel_enqueued: bool = false,
    cancel_submitted: bool = false,
    cancel_done: bool = true,
    outcome: TimerWaitOutcome = .expired,
};

const TcpConnectWaitPayload = union(enum(u8)) {
    socket_handle: usize,
    error_msg: []u8,
};

const TcpHandle = @TypeOf(@as(xev.TCP, undefined).fd);

fn tcpSocketHandleToBits(handle: TcpHandle) usize {
    return switch (@typeInfo(@TypeOf(handle))) {
        .int, .comptime_int => @as(usize, @intCast(handle)),
        .pointer => @intFromPtr(handle),
        else => @compileError("unsupported xev TCP socket handle type"),
    };
}

fn tcpSocketHandleFromBits(bits: usize) TcpHandle {
    return switch (@typeInfo(TcpHandle)) {
        .int, .comptime_int => @as(TcpHandle, @intCast(bits)),
        .pointer => @ptrFromInt(bits),
        else => @compileError("unsupported xev TCP socket handle type"),
    };
}

fn closeSocketHandle(bits: usize) void {
    if (bits == 0) return;
    var stream = std.net.Stream{ .handle = tcpSocketHandleFromBits(bits) };
    stream.close();
}

fn makeSocketBlocking(handle: TcpHandle) !void {
    if (builtin.os.tag == .windows) return;

    const fd: std.posix.fd_t = @intCast(handle);
    const flags = std.c.fcntl(fd, std.c.F.GETFL, @as(c_int, 0));
    if (flags < 0) return error.Unexpected;

    const nonblock_mask: c_int = @as(c_int, 1) << @bitOffsetOf(std.c.O, "NONBLOCK");
    const new_flags = flags & ~nonblock_mask;
    if (new_flags != flags and std.c.fcntl(fd, std.c.F.SETFL, new_flags) < 0) {
        return error.Unexpected;
    }
}

fn freeTcpConnectWaitPayload(payload: TcpConnectWaitPayload) void {
    switch (payload) {
        .socket_handle => |handle| closeSocketHandle(handle),
        .error_msg => |msg| alloc.free(msg),
    }
}

const TcpConnectWaitState = async_wait.StickyCancelWait(TcpConnectWaitPayload, freeTcpConnectWaitPayload);

const TcpConnectWait = struct {
    wait: TcpConnectWaitState = .{},
    addr: std.net.Address,
    state_mutex: std.Thread.Mutex = .{},
    tcp_initialized: bool = false,
    socket_finalized: bool = false,
    close_submitted: bool = false,
    cancel_enqueued: bool = false,
    next: ?*TcpConnectWait = null,
    cancel_next: ?*TcpConnectWait = null,
    tcp: xev.TCP = undefined,
    connect_completion: xev.Completion = .{},
    close_completion: xev.Completion = .{},
};

fn freeTcpConnectWait(req: *TcpConnectWait) void {
    req.wait.deinit();
    alloc.destroy(req);
}

fn tcpConnectWaitServiceRelease(req: *TcpConnectWait) void {
    if (req.wait.releaseRef()) {
        freeTcpConnectWait(req);
    }
}

fn tcpConnectWaitMarkSocketFinalized(req: *TcpConnectWait) void {
    req.state_mutex.lock();
    req.socket_finalized = true;
    req.state_mutex.unlock();
}

fn tcpConnectWaitCloseNow(req: *TcpConnectWait, handle: TcpHandle) void {
    var should_close = false;
    req.state_mutex.lock();
    if (!req.socket_finalized) {
        req.socket_finalized = true;
        should_close = true;
    }
    req.state_mutex.unlock();

    if (should_close) {
        closeSocketHandle(tcpSocketHandleToBits(handle));
    }
}

fn tcpConnectWaitScheduleClose(req: *TcpConnectWait, loop: *xev.Loop, tcp: xev.TCP) void {
    var should_submit = false;
    req.state_mutex.lock();
    if (req.tcp_initialized and !req.socket_finalized and !req.close_submitted) {
        req.close_submitted = true;
        should_submit = true;
    }
    req.state_mutex.unlock();

    if (!should_submit) return;

    req.wait.addRef();
    tcp.close(loop, &req.close_completion, TcpConnectWait, req, tcpConnectCloseCallback);
}

fn tcpConnectCloseCallback(
    req_opt: ?*TcpConnectWait,
    _: *xev.Loop,
    _: *xev.Completion,
    _: xev.TCP,
    result: xev.CloseError!void,
) xev.CallbackAction {
    const req = req_opt orelse @panic("TCP connect close callback missing request");
    _ = result catch {};

    tcpConnectWaitMarkSocketFinalized(req);
    req.wait.workerCancel();
    tcpConnectWaitServiceRelease(req);
    return .disarm;
}

fn tcpConnectCallback(
    req_opt: ?*TcpConnectWait,
    loop: *xev.Loop,
    _: *xev.Completion,
    tcp: xev.TCP,
    result: xev.ConnectError!void,
) xev.CallbackAction {
    const req = req_opt orelse @panic("TCP connect callback missing request");

    if (req.wait.isCancelVisible()) {
        if (result) |_| {
            tcpConnectWaitScheduleClose(req, loop, tcp);
        } else |_| {
            tcpConnectWaitMarkSocketFinalized(req);
        }
        req.wait.workerCancel();
        tcpConnectWaitServiceRelease(req);
        return .disarm;
    }

    _ = result catch |err| {
        tcpConnectWaitCloseNow(req, tcp.fd);
        const msg = alloc.dupe(u8, @errorName(err)) catch @panic("oom");
        req.wait.workerComplete(.{ .error_msg = msg });
        tcpConnectWaitServiceRelease(req);
        return .disarm;
    };

    makeSocketBlocking(tcp.fd) catch |err| {
        tcpConnectWaitCloseNow(req, tcp.fd);
        const msg = alloc.dupe(u8, @errorName(err)) catch @panic("oom");
        req.wait.workerComplete(.{ .error_msg = msg });
        tcpConnectWaitServiceRelease(req);
        return .disarm;
    };

    tcpConnectWaitMarkSocketFinalized(req);
    req.wait.workerComplete(.{ .socket_handle = tcpSocketHandleToBits(tcp.fd) });
    tcpConnectWaitServiceRelease(req);
    return .disarm;
}

const TcpConnectWaitOutcome = enum(i32) {
    completed = 0,
    canceled = 1,
};

const TcpConnectWaitPayloadKind = enum(i32) {
    none = 0,
    socket_handle = 1,
    error_msg = 2,
};

const XevService = struct {
    init_mutex: std.Thread.Mutex = .{},
    init_cond: std.Thread.Condition = .{},
    started: bool = false,
    ready: bool = false,
    loop: xev.Loop = undefined,
    notifier: xev.Async = undefined,
    notifier_completion: xev.Completion = .{},
    queue_mutex: std.Thread.Mutex = .{},
    timer_queue_head: ?*TimerWait = null,
    timer_queue_tail: ?*TimerWait = null,
    timer_cancel_queue_head: ?*TimerWait = null,
    timer_cancel_queue_tail: ?*TimerWait = null,
    connect_queue_head: ?*TcpConnectWait = null,
    connect_queue_tail: ?*TcpConnectWait = null,
    connect_cancel_queue_head: ?*TcpConnectWait = null,
    connect_cancel_queue_tail: ?*TcpConnectWait = null,
    thread: ?std.Thread = null,
};

var g_xev_service = XevService{};

fn timerCallback(
    req_opt: ?*TimerWait,
    _: *xev.Loop,
    _: *xev.Completion,
    result: xev.Timer.RunError!void,
) xev.CallbackAction {
    const req = req_opt orelse @panic("timer callback missing request");

    req.mutex.lock();
    defer req.mutex.unlock();

    req.completed = true;
    req.outcome = if (result) .expired else |err| switch (err) {
        error.Canceled => .canceled,
        else => unreachable,
    };
    if (!req.cancel_submitted) {
        req.cancel_done = true;
    }
    req.cond.broadcast();
    return .disarm;
}

fn timerCancelCallback(
    req_opt: ?*TimerWait,
    _: *xev.Loop,
    _: *xev.Completion,
    result: xev.Timer.CancelError!void,
) xev.CallbackAction {
    const req = req_opt orelse @panic("timer cancel callback missing request");
    _ = result catch {};

    req.mutex.lock();
    defer req.mutex.unlock();

    req.cancel_done = true;
    req.cond.broadcast();
    return .disarm;
}

fn popTimerStartRequest() ?*TimerWait {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    const req_opt = g_xev_service.timer_queue_head;
    if (req_opt) |req| {
        g_xev_service.timer_queue_head = req.next;
        if (g_xev_service.timer_queue_head == null) {
            g_xev_service.timer_queue_tail = null;
        }
        req.next = null;
    }
    return req_opt;
}

fn popTimerCancelRequest() ?*TimerWait {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    const req_opt = g_xev_service.timer_cancel_queue_head;
    if (req_opt) |req| {
        g_xev_service.timer_cancel_queue_head = req.cancel_next;
        if (g_xev_service.timer_cancel_queue_head == null) {
            g_xev_service.timer_cancel_queue_tail = null;
        }
        req.cancel_next = null;
    }
    return req_opt;
}

fn popTcpConnectStartRequest() ?*TcpConnectWait {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    const req_opt = g_xev_service.connect_queue_head;
    if (req_opt) |req| {
        g_xev_service.connect_queue_head = req.next;
        if (g_xev_service.connect_queue_head == null) {
            g_xev_service.connect_queue_tail = null;
        }
        req.next = null;
    }
    return req_opt;
}

fn popTcpConnectCancelRequest() ?*TcpConnectWait {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    const req_opt = g_xev_service.connect_cancel_queue_head;
    if (req_opt) |req| {
        g_xev_service.connect_cancel_queue_head = req.cancel_next;
        if (g_xev_service.connect_cancel_queue_head == null) {
            g_xev_service.connect_cancel_queue_tail = null;
        }
        req.cancel_next = null;
    }
    return req_opt;
}

fn processPendingTimerStarts(loop: *xev.Loop) void {
    while (popTimerStartRequest()) |req| {
        req.mutex.lock();
        if (req.cancel_requested) {
            req.completed = true;
            req.cancel_done = true;
            req.outcome = .canceled;
            req.cond.broadcast();
            req.mutex.unlock();
            continue;
        }

        req.started = true;
        req.cancel_done = true;
        req.mutex.unlock();

        req.timer.run(loop, &req.completion, req.ms, TimerWait, req, timerCallback);
    }
}

fn processPendingTimerCancels(loop: *xev.Loop) void {
    while (popTimerCancelRequest()) |req| {
        req.mutex.lock();
        req.cancel_enqueued = false;
        if (!req.cancel_requested or req.completed or !req.started or req.cancel_submitted) {
            req.mutex.unlock();
            continue;
        }

        req.cancel_submitted = true;
        req.cancel_done = false;
        req.mutex.unlock();

        req.timer.cancel(loop, &req.completion, &req.cancel_completion, TimerWait, req, timerCancelCallback);
    }
}

fn processPendingTcpConnectStarts(loop: *xev.Loop) void {
    while (popTcpConnectStartRequest()) |req| {
        if (req.wait.isCancelVisible()) {
            req.wait.workerCancel();
            tcpConnectWaitServiceRelease(req);
            continue;
        }

        const tcp = xev.TCP.init(req.addr) catch |err| {
            const msg = alloc.dupe(u8, @errorName(err)) catch @panic("oom");
            req.wait.workerComplete(.{ .error_msg = msg });
            tcpConnectWaitServiceRelease(req);
            continue;
        };

        req.state_mutex.lock();
        req.tcp_initialized = true;
        req.tcp = tcp;
        req.state_mutex.unlock();

        tcp.connect(loop, &req.connect_completion, req.addr, TcpConnectWait, req, tcpConnectCallback);
    }
}

fn processPendingTcpConnectCancels(loop: *xev.Loop) void {
    while (popTcpConnectCancelRequest()) |req| {
        var should_submit = false;
        var tcp: xev.TCP = undefined;

        req.state_mutex.lock();
        req.cancel_enqueued = false;
        if (req.tcp_initialized and !req.socket_finalized and !req.close_submitted) {
            req.close_submitted = true;
            tcp = req.tcp;
            should_submit = true;
        }
        req.state_mutex.unlock();

        if (!should_submit) continue;

        req.wait.addRef();
        tcp.close(loop, &req.close_completion, TcpConnectWait, req, tcpConnectCloseCallback);
    }
}

fn asyncCallback(
    _: ?*XevService,
    loop: *xev.Loop,
    _: *xev.Completion,
    result: xev.Async.WaitError!void,
) xev.CallbackAction {
    _ = result catch unreachable;
    processPendingTimerCancels(loop);
    processPendingTcpConnectCancels(loop);
    processPendingTimerStarts(loop);
    processPendingTcpConnectStarts(loop);
    return .rearm;
}

fn serviceThreadMain() void {
    g_xev_service.loop = xev.Loop.init(.{}) catch @panic("libxev loop init failed");
    g_xev_service.notifier = xev.Async.init() catch @panic("libxev async init failed");
    g_xev_service.notifier.wait(
        &g_xev_service.loop,
        &g_xev_service.notifier_completion,
        XevService,
        &g_xev_service,
        asyncCallback,
    );

    g_xev_service.init_mutex.lock();
    g_xev_service.ready = true;
    g_xev_service.init_cond.broadcast();
    g_xev_service.init_mutex.unlock();

    g_xev_service.loop.run(.until_done) catch @panic("libxev loop failed");
}

fn ensureXevService() void {
    g_xev_service.init_mutex.lock();
    defer g_xev_service.init_mutex.unlock();

    if (!g_xev_service.started) {
        g_xev_service.started = true;
        g_xev_service.thread = std.Thread.spawn(.{}, serviceThreadMain, .{}) catch @panic("failed to spawn libxev loop");
    }

    while (!g_xev_service.ready) {
        g_xev_service.init_cond.wait(&g_xev_service.init_mutex);
    }
}

fn enqueueTimerStart(req: *TimerWait) void {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    if (g_xev_service.timer_queue_tail) |tail| {
        tail.next = req;
    } else {
        g_xev_service.timer_queue_head = req;
    }
    g_xev_service.timer_queue_tail = req;
}

fn enqueueTimerCancel(req: *TimerWait) void {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    if (g_xev_service.timer_cancel_queue_tail) |tail| {
        tail.cancel_next = req;
    } else {
        g_xev_service.timer_cancel_queue_head = req;
    }
    g_xev_service.timer_cancel_queue_tail = req;
}

fn enqueueTcpConnectStart(req: *TcpConnectWait) void {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    if (g_xev_service.connect_queue_tail) |tail| {
        tail.next = req;
    } else {
        g_xev_service.connect_queue_head = req;
    }
    g_xev_service.connect_queue_tail = req;
}

fn enqueueTcpConnectCancel(req: *TcpConnectWait) void {
    g_xev_service.queue_mutex.lock();
    defer g_xev_service.queue_mutex.unlock();

    if (g_xev_service.connect_cancel_queue_tail) |tail| {
        tail.cancel_next = req;
    } else {
        g_xev_service.connect_cancel_queue_head = req;
    }
    g_xev_service.connect_cancel_queue_tail = req;
}

export fn flix_native_timer_wait_new(ms: u64) *anyopaque {
    ensureXevService();

    const timer = xev.Timer.init() catch @panic("libxev timer init failed");
    const req = alloc.create(TimerWait) catch @panic("oom");
    req.* = .{
        .ms = ms,
        .timer = timer,
    };

    enqueueTimerStart(req);
    g_xev_service.notifier.notify() catch @panic("failed to notify libxev loop");
    return req;
}

export fn flix_native_timer_wait_cancel(wait_ptr: *anyopaque) void {
    const req: *TimerWait = @ptrCast(@alignCast(wait_ptr));

    var should_enqueue = false;
    req.mutex.lock();
    if (!req.completed and !req.cancel_requested) {
        req.cancel_requested = true;
        if (req.started and !req.cancel_enqueued and !req.cancel_submitted) {
            req.cancel_enqueued = true;
            should_enqueue = true;
        }
    }
    req.mutex.unlock();

    if (should_enqueue) {
        enqueueTimerCancel(req);
        g_xev_service.notifier.notify() catch @panic("failed to notify libxev loop");
    }
}

export fn flix_native_timer_wait_await(wait_ptr: *anyopaque) i32 {
    const req: *TimerWait = @ptrCast(@alignCast(wait_ptr));
    req.mutex.lock();
    while (!req.completed or !req.cancel_done) {
        req.cond.wait(&req.mutex);
    }
    const outcome = req.outcome;
    req.mutex.unlock();
    return @intFromEnum(outcome);
}

export fn flix_native_timer_wait_release(wait_ptr: *anyopaque) void {
    const req: *TimerWait = @ptrCast(@alignCast(wait_ptr));
    req.timer.deinit();
    alloc.destroy(req);
}

export fn flix_native_timer_sleep(ms: u64) void {
    if (ms == 0) return;
    const wait = flix_native_timer_wait_new(ms);
    _ = flix_native_timer_wait_await(wait);
    flix_native_timer_wait_release(wait);
}

export fn flix_native_tcp_connect_wait_new(ip_bytes_ptr: [*]const u8, ip_bytes_len: usize, port: u16) ?*anyopaque {
    ensureXevService();

    const ip_bytes = ip_bytes_ptr[0..ip_bytes_len];
    var addr: std.net.Address = undefined;
    if (ip_bytes.len == 4) {
        var ip4: [4]u8 = undefined;
        std.mem.copyForwards(u8, ip4[0..], ip_bytes[0..4]);
        addr = std.net.Address.initIp4(ip4, port);
    } else if (ip_bytes.len == 16) {
        var ip6: [16]u8 = undefined;
        std.mem.copyForwards(u8, ip6[0..], ip_bytes[0..16]);
        addr = std.net.Address.initIp6(ip6, port, 0, 0);
    } else {
        return null;
    }

    const req = alloc.create(TcpConnectWait) catch @panic("oom");
    req.* = .{
        .addr = addr,
    };

    enqueueTcpConnectStart(req);
    g_xev_service.notifier.notify() catch @panic("failed to notify libxev loop");
    return req;
}

export fn flix_native_tcp_connect_wait_cancel(wait_ptr: *anyopaque) void {
    const req: *TcpConnectWait = @ptrCast(@alignCast(wait_ptr));
    if (!req.wait.requestCancel()) return;

    var should_enqueue = false;
    req.state_mutex.lock();
    if (req.tcp_initialized and !req.socket_finalized and !req.close_submitted and !req.cancel_enqueued) {
        req.cancel_enqueued = true;
        should_enqueue = true;
    }
    req.state_mutex.unlock();

    if (should_enqueue) {
        enqueueTcpConnectCancel(req);
        g_xev_service.notifier.notify() catch @panic("failed to notify libxev loop");
    }
}

export fn flix_native_tcp_connect_wait_await(wait_ptr: *anyopaque) i32 {
    const req: *TcpConnectWait = @ptrCast(@alignCast(wait_ptr));
    return switch (req.wait.await()) {
        .completed => @intFromEnum(TcpConnectWaitOutcome.completed),
        .canceled => @intFromEnum(TcpConnectWaitOutcome.canceled),
    };
}

export fn flix_native_tcp_connect_wait_payload_kind(wait_ptr: *anyopaque) i32 {
    const req: *TcpConnectWait = @ptrCast(@alignCast(wait_ptr));
    const payload = req.wait.peekCompletedPayload() orelse return @intFromEnum(TcpConnectWaitPayloadKind.none);
    return switch (payload) {
        .socket_handle => @intFromEnum(TcpConnectWaitPayloadKind.socket_handle),
        .error_msg => @intFromEnum(TcpConnectWaitPayloadKind.error_msg),
    };
}

export fn flix_native_tcp_connect_wait_take_socket_handle(wait_ptr: *anyopaque) usize {
    const req: *TcpConnectWait = @ptrCast(@alignCast(wait_ptr));
    const payload = req.wait.takeCompletedPayload() orelse return 0;
    return switch (payload) {
        .socket_handle => |handle| handle,
        .error_msg => |msg| {
            alloc.free(msg);
            return 0;
        },
    };
}

export fn flix_native_tcp_connect_wait_error_ptr(wait_ptr: *anyopaque) ?*anyopaque {
    const req: *TcpConnectWait = @ptrCast(@alignCast(wait_ptr));
    const payload = req.wait.peekCompletedPayload() orelse return null;
    return switch (payload) {
        .socket_handle => null,
        .error_msg => |msg| if (msg.len == 0) null else @ptrCast(@constCast(msg.ptr)),
    };
}

export fn flix_native_tcp_connect_wait_error_len(wait_ptr: *anyopaque) usize {
    const req: *TcpConnectWait = @ptrCast(@alignCast(wait_ptr));
    const payload = req.wait.peekCompletedPayload() orelse return 0;
    return switch (payload) {
        .socket_handle => 0,
        .error_msg => |msg| msg.len,
    };
}

export fn flix_native_tcp_connect_wait_release(wait_ptr: *anyopaque) void {
    const req: *TcpConnectWait = @ptrCast(@alignCast(wait_ptr));
    if (req.wait.releaseRef()) {
        freeTcpConnectWait(req);
    }
}

const HttpWaitOutcome = enum(i32) {
    completed = 0,
    canceled = 1,
};

export fn flix_native_http_wait_new(req_blob_ptr: [*]const u8, req_blob_len: usize) *anyopaque {
    const blob = req_blob_ptr[0..req_blob_len];
    return http.httpRequestAsyncStart(blob);
}

export fn flix_native_http_wait_cancel(wait_ptr: *anyopaque) void {
    const req: *http.AsyncRequest = @ptrCast(@alignCast(wait_ptr));
    http.httpRequestAsyncCancel(req);
}

export fn flix_native_http_wait_await(wait_ptr: *anyopaque) i32 {
    const req: *http.AsyncRequest = @ptrCast(@alignCast(wait_ptr));
    const result = http.httpRequestAsyncAwait(req);
    return switch (result.outcome) {
        .completed => @intFromEnum(HttpWaitOutcome.completed),
        .canceled => @intFromEnum(HttpWaitOutcome.canceled),
    };
}

export fn flix_native_http_wait_response_ptr(wait_ptr: *anyopaque) ?*anyopaque {
    const req: *http.AsyncRequest = @ptrCast(@alignCast(wait_ptr));
    return if (http.httpRequestAsyncResponseBlob(req)) |blob| @ptrCast(@constCast(blob.ptr)) else null;
}

export fn flix_native_http_wait_response_len(wait_ptr: *anyopaque) usize {
    const req: *http.AsyncRequest = @ptrCast(@alignCast(wait_ptr));
    return if (http.httpRequestAsyncResponseBlob(req)) |blob| blob.len else 0;
}

export fn flix_native_http_wait_release(wait_ptr: *anyopaque) void {
    const req: *http.AsyncRequest = @ptrCast(@alignCast(wait_ptr));
    http.httpRequestAsyncRelease(req);
}

test "tcp connect late success after visible cancellation stays canceled and closes socket" {
    var tpool = xev.ThreadPool.init(.{});
    defer tpool.deinit();
    defer tpool.shutdown();

    var loop = try xev.Loop.init(.{ .thread_pool = &tpool });
    defer loop.deinit();

    const addr = std.net.Address.initIp4(.{ 127, 0, 0, 1 }, 9);
    const tcp = try xev.TCP.init(addr);
    const handle_bits = tcpSocketHandleToBits(tcp.fd);

    var req = TcpConnectWait{
        .addr = addr,
    };
    defer req.wait.deinit();

    req.state_mutex.lock();
    req.tcp_initialized = true;
    req.tcp = tcp;
    req.state_mutex.unlock();

    try std.testing.expect(req.wait.requestCancel());
    try std.testing.expect(req.wait.isCancelVisible());

    const action = tcpConnectCallback(&req, &loop, &req.connect_completion, tcp, {});
    try std.testing.expectEqual(xev.CallbackAction.disarm, action);

    switch (req.wait.await()) {
        .canceled => {},
        .completed => return error.TestUnexpectedCompletion,
    }

    try std.testing.expect(req.close_submitted);
    try std.testing.expect(!req.socket_finalized);

    try loop.run(.until_done);

    try std.testing.expect(req.socket_finalized);
    try std.testing.expect(req.wait.peekCompletedPayload() == null);

    if (builtin.os.tag != .windows) {
        const fd: std.posix.fd_t = @intCast(handle_bits);
        const rc = std.c.fcntl(fd, std.c.F.GETFD, @as(c_int, 0));
        try std.testing.expectEqual(std.posix.E.BADF, std.posix.errno(rc));
    }
}
