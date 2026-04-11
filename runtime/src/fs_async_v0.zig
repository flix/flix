const std = @import("std");
const builtin = @import("builtin");
const async_wait = @import("async_wait_v0.zig");

pub const IOERR_INTERRUPTED: i64 = 2;
pub const IOERR_INVALID_PATH: i64 = 3;
pub const IOERR_NOT_DIRECTORY: i64 = 8;
pub const IOERR_UNSUPPORTED: i64 = 12;
pub const IOERR_OTHER: i64 = 14;

pub const FileOp = enum(u8) {
    read,
    read_lines,
    read_bytes,
    list,
    write,
    write_bytes,
    append,
    append_bytes,
};

pub const FileOpWaitOutcome = enum(i32) {
    completed = 0,
    canceled = 1,
};

pub const FileOpError = struct {
    kind: i64,
    msg: []u8,
};

pub const FileOpPayload = union(enum(u8)) {
    unit,
    bytes: []u8,
    names: [][]u8,
    err: FileOpError,
};

fn freeNames(names: [][]u8) void {
    for (names) |name| {
        std.heap.c_allocator.free(name);
    }
    std.heap.c_allocator.free(names);
}

pub fn fileOpPayloadDeinit(payload: FileOpPayload) void {
    switch (payload) {
        .unit => {},
        .bytes => |bytes| std.heap.c_allocator.free(bytes),
        .names => |names| freeNames(names),
        .err => |err| std.heap.c_allocator.free(err.msg),
    }
}

const FileOpWaitState = async_wait.StickyCancelWait(FileOpPayload, fileOpPayloadDeinit);

pub const FileOpWait = struct {
    wait: FileOpWaitState = .{},
    op: FileOp,
    path: []u8,
    data: ?[]u8 = null,
    cancel_pipe: ?[2]std.posix.fd_t = null,
};

fn fileOpNeedsData(op: FileOp) bool {
    return switch (op) {
        .write, .write_bytes, .append, .append_bytes => true,
        else => false,
    };
}

fn isInvalidPathError(err: anyerror) bool {
    return switch (err) {
        error.BadPathName,
        error.NameTooLong,
        error.InvalidUtf8,
        error.InvalidWtf8,
        => true,
        else => false,
    };
}

fn isUnsupportedError(err: anyerror) bool {
    return switch (err) {
        error.Unsupported,
        => true,
        else => false,
    };
}

fn fileKindForErr(err: anyerror) i64 {
    if (isInvalidPathError(err)) return IOERR_INVALID_PATH;
    if (isUnsupportedError(err)) return IOERR_UNSUPPORTED;
    return IOERR_OTHER;
}

fn freeFileOpWait(req: *FileOpWait) void {
    req.wait.deinit();
    if (req.cancel_pipe) |fds| {
        std.posix.close(fds[0]);
        std.posix.close(fds[1]);
    }
    if (req.data) |data| {
        std.heap.c_allocator.free(data);
    }
    std.heap.c_allocator.free(req.path);
    std.heap.c_allocator.destroy(req);
}

fn fileOpWaitWorkerRelease(req: *FileOpWait) void {
    if (req.wait.releaseRef()) {
        freeFileOpWait(req);
    }
}

fn fileOpWaitSignalCancel(req: *FileOpWait) void {
    if (req.cancel_pipe) |fds| {
        const one = [_]u8{1};
        _ = std.posix.write(fds[1], &one) catch {};
    }
}

fn readAllPath(req: *FileOpWait) !?[]u8 {
    if (builtin.os.tag == .windows) {
        if (req.wait.isCancelVisible()) return null;
        return std.fs.cwd().readFileAlloc(std.heap.c_allocator, req.path, std.math.maxInt(usize));
    }

    const fd = try std.posix.open(req.path, .{
        .ACCMODE = .RDONLY,
        .NONBLOCK = true,
        .CLOEXEC = true,
    }, 0);
    defer std.posix.close(fd);

    var file = std.fs.File{ .handle = fd };
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(std.heap.c_allocator);

    while (true) {
        if (req.wait.isCancelVisible()) return null;

        var poll_fds = [_]std.posix.pollfd{
            .{
                .fd = fd,
                .events = std.posix.POLL.IN | std.posix.POLL.HUP | std.posix.POLL.ERR,
                .revents = 0,
            },
            .{
                .fd = req.cancel_pipe.?[0],
                .events = std.posix.POLL.IN,
                .revents = 0,
            },
        };

        _ = try std.posix.poll(&poll_fds, -1);

        if ((poll_fds[1].revents & std.posix.POLL.IN) != 0) {
            return null;
        }

        if ((poll_fds[0].revents & (std.posix.POLL.IN | std.posix.POLL.HUP | std.posix.POLL.ERR)) == 0) {
            continue;
        }

        var buf: [8192]u8 = undefined;
        const n = file.read(&buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };

        if (n == 0) {
            break;
        }

        try out.appendSlice(std.heap.c_allocator, buf[0..n]);
    }

    return try out.toOwnedSlice(std.heap.c_allocator);
}

fn listNamesPath(req: *FileOpWait) !?[][]u8 {
    var dir = try std.fs.cwd().openDir(req.path, .{ .iterate = true });
    defer dir.close();

    var names: std.ArrayList([]u8) = .empty;
    errdefer {
        for (names.items) |name| {
            std.heap.c_allocator.free(name);
        }
        names.deinit(std.heap.c_allocator);
    }

    var it = dir.iterate();
    while (true) {
        if (req.wait.isCancelVisible()) return null;
        const entry_opt = try it.next();
        if (entry_opt == null) break;
        try names.append(std.heap.c_allocator, try std.heap.c_allocator.dupe(u8, entry_opt.?.name));
    }

    return try names.toOwnedSlice(std.heap.c_allocator);
}

fn writeAllPath(req: *FileOpWait) !?void {
    const data = req.data orelse unreachable;
    const is_append = switch (req.op) {
        .append, .append_bytes => true,
        else => false,
    };

    if (builtin.os.tag == .windows) {
        if (req.wait.isCancelVisible()) return null;

        var file = try std.fs.cwd().createFile(req.path, .{ .truncate = !is_append });
        defer file.close();

        if (is_append) {
            try file.seekFromEnd(0);
        }

        try file.writeAll(data);
        return {};
    }

    const fd = try std.posix.open(req.path, .{
        .ACCMODE = .WRONLY,
        .CREAT = true,
        .TRUNC = !is_append,
        .APPEND = is_append,
        .NONBLOCK = true,
        .CLOEXEC = true,
    }, 0o666);
    defer std.posix.close(fd);

    var written: usize = 0;
    while (written < data.len) {
        if (req.wait.isCancelVisible()) return null;

        var poll_fds = [_]std.posix.pollfd{
            .{
                .fd = fd,
                .events = std.posix.POLL.OUT | std.posix.POLL.HUP | std.posix.POLL.ERR,
                .revents = 0,
            },
            .{
                .fd = req.cancel_pipe.?[0],
                .events = std.posix.POLL.IN,
                .revents = 0,
            },
        };

        _ = try std.posix.poll(&poll_fds, -1);

        if ((poll_fds[1].revents & std.posix.POLL.IN) != 0) {
            return null;
        }

        if ((poll_fds[0].revents & (std.posix.POLL.OUT | std.posix.POLL.HUP | std.posix.POLL.ERR)) == 0) {
            continue;
        }

        const n = std.posix.write(fd, data[written..]) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) return error.Unexpected;
        written += n;
    }

    return {};
}

fn fileOpWaitWorkerMain(req: *FileOpWait) void {
    defer fileOpWaitWorkerRelease(req);

    switch (req.op) {
        .read, .read_lines, .read_bytes => {
            const bytes_opt = readAllPath(req) catch |err| {
                if (req.wait.isCancelVisible()) {
                    req.wait.workerCancel();
                    return;
                }
                const msg = std.heap.c_allocator.dupe(u8, @errorName(err)) catch @panic("oom");
                req.wait.workerComplete(.{ .err = .{
                    .kind = fileKindForErr(err),
                    .msg = msg,
                } });
                return;
            };

            if (bytes_opt) |bytes| {
                req.wait.workerComplete(.{ .bytes = bytes });
            } else {
                req.wait.workerCancel();
            }
        },

        .list => {
            const names_or_cancel = listNamesPath(req) catch |err| {
                if (req.wait.isCancelVisible()) {
                    req.wait.workerCancel();
                    return;
                }
                const kind: i64 = if (isInvalidPathError(err)) IOERR_INVALID_PATH else if (err == error.NotDir) IOERR_NOT_DIRECTORY else fileKindForErr(err);
                const msg = std.heap.c_allocator.dupe(u8, if (err == error.NotDir) "not a directory" else @errorName(err)) catch @panic("oom");
                req.wait.workerComplete(.{ .err = .{
                    .kind = kind,
                    .msg = msg,
                } });
                return;
            };

            if (names_or_cancel) |names| {
                req.wait.workerComplete(.{ .names = names });
            } else {
                req.wait.workerCancel();
            }
        },

        .write, .write_bytes, .append, .append_bytes => {
            const complete_or_cancel = writeAllPath(req) catch |err| {
                if (req.wait.isCancelVisible()) {
                    req.wait.workerCancel();
                    return;
                }
                const msg = std.heap.c_allocator.dupe(u8, @errorName(err)) catch @panic("oom");
                req.wait.workerComplete(.{ .err = .{
                    .kind = fileKindForErr(err),
                    .msg = msg,
                } });
                return;
            };

            if (complete_or_cancel == null) {
                req.wait.workerCancel();
            } else {
                req.wait.workerComplete(.unit);
            }
        },
    }
}

pub fn fileOpWaitNew(op: FileOp, path: []const u8) *FileOpWait {
    std.debug.assert(!fileOpNeedsData(op));
    const req = std.heap.c_allocator.create(FileOpWait) catch @panic("oom");
    req.* = .{
        .op = op,
        .path = std.heap.c_allocator.dupe(u8, path) catch @panic("oom"),
    };

    if (builtin.os.tag != .windows and op != .list) {
        req.cancel_pipe = std.posix.pipe2(.{ .CLOEXEC = true, .NONBLOCK = true }) catch @panic("failed to create fs cancel pipe");
    }

    const thread = std.Thread.spawn(.{}, fileOpWaitWorkerMain, .{req}) catch @panic("failed to spawn async filesystem worker");
    thread.detach();
    return req;
}

pub fn fileOpWriteWaitNew(op: FileOp, path: []const u8, data: []const u8) *FileOpWait {
    std.debug.assert(fileOpNeedsData(op));
    const req = std.heap.c_allocator.create(FileOpWait) catch @panic("oom");
    req.* = .{
        .op = op,
        .path = std.heap.c_allocator.dupe(u8, path) catch @panic("oom"),
        .data = std.heap.c_allocator.dupe(u8, data) catch @panic("oom"),
    };

    if (builtin.os.tag != .windows) {
        req.cancel_pipe = std.posix.pipe2(.{ .CLOEXEC = true, .NONBLOCK = true }) catch @panic("failed to create fs cancel pipe");
    }

    const thread = std.Thread.spawn(.{}, fileOpWaitWorkerMain, .{req}) catch @panic("failed to spawn async filesystem worker");
    thread.detach();
    return req;
}

pub fn fileOpWaitCancel(wait_ptr: *anyopaque) void {
    const req: *FileOpWait = @ptrCast(@alignCast(wait_ptr));
    if (req.wait.requestCancel()) {
        fileOpWaitSignalCancel(req);
    }
}

pub fn fileOpWaitAwait(wait_ptr: *anyopaque) FileOpWaitOutcome {
    const req: *FileOpWait = @ptrCast(@alignCast(wait_ptr));
    return switch (req.wait.await()) {
        .completed => .completed,
        .canceled => .canceled,
    };
}

pub fn fileOpWaitTakePayload(wait_ptr: *anyopaque) ?FileOpPayload {
    const req: *FileOpWait = @ptrCast(@alignCast(wait_ptr));
    return req.wait.takeCompletedPayload();
}

pub fn fileOpWaitRelease(wait_ptr: *anyopaque) void {
    const req: *FileOpWait = @ptrCast(@alignCast(wait_ptr));
    if (req.wait.releaseRef()) {
        freeFileOpWait(req);
    }
}

test "file op wait reads bytes from regular file" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var f = try tmp.dir.createFile("data.txt", .{});
    defer f.close();
    try f.writeAll("hello");

    const path = try std.fmt.allocPrint(std.testing.allocator, ".zig-cache/tmp/{s}/data.txt", .{tmp.sub_path});
    defer std.testing.allocator.free(path);

    const wait = fileOpWaitNew(.read_bytes, path);
    defer fileOpWaitRelease(wait);

    try std.testing.expectEqual(FileOpWaitOutcome.completed, fileOpWaitAwait(wait));
    const payload = fileOpWaitTakePayload(wait) orelse return error.TestUnexpectedNull;
    switch (payload) {
        .bytes => |bytes| {
            defer std.heap.c_allocator.free(bytes);
            try std.testing.expectEqualStrings("hello", bytes);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "file op wait fifo read cancels promptly" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const fifo_path = try std.fmt.allocPrint(std.testing.allocator, ".zig-cache/tmp/{s}/block.fifo", .{tmp.sub_path});
    defer std.testing.allocator.free(fifo_path);
    var child = std.process.Child.init(&.{ "mkfifo", fifo_path }, std.testing.allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    try child.spawn();
    const term = try child.wait();
    switch (term) {
        .Exited => |code| if (code != 0) return error.Unexpected,
        else => return error.Unexpected,
    }

    const WriterState = struct {
        path: []const u8,
        mutex: std.Thread.Mutex = .{},
        cond: std.Thread.Condition = .{},
        opened: bool = false,
        release: bool = false,
    };

    const writerMain = struct {
        fn run(state: *WriterState) void {
            const fd = std.posix.open(state.path, .{
                .ACCMODE = .WRONLY,
                .CLOEXEC = true,
            }, 0) catch @panic("failed to open fifo writer");
            defer std.posix.close(fd);

            state.mutex.lock();
            state.opened = true;
            state.cond.broadcast();
            while (!state.release) {
                state.cond.wait(&state.mutex);
            }
            state.mutex.unlock();
        }
    }.run;

    var writer = WriterState{ .path = fifo_path };
    const thread = try std.Thread.spawn(.{}, writerMain, .{&writer});
    defer thread.join();

    const wait = fileOpWaitNew(.read_bytes, fifo_path);
    defer fileOpWaitRelease(wait);

    writer.mutex.lock();
    while (!writer.opened) {
        writer.cond.wait(&writer.mutex);
    }
    writer.mutex.unlock();

    fileOpWaitCancel(wait);
    try std.testing.expectEqual(FileOpWaitOutcome.canceled, fileOpWaitAwait(wait));
    try std.testing.expect(fileOpWaitTakePayload(wait) == null);

    writer.mutex.lock();
    writer.release = true;
    writer.cond.broadcast();
    writer.mutex.unlock();
}

test "file op wait writes bytes to regular file" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const path = try std.fmt.allocPrint(std.testing.allocator, ".zig-cache/tmp/{s}/out.txt", .{tmp.sub_path});
    defer std.testing.allocator.free(path);

    const wait = fileOpWriteWaitNew(.write_bytes, path, "hello");
    defer fileOpWaitRelease(wait);

    try std.testing.expectEqual(FileOpWaitOutcome.completed, fileOpWaitAwait(wait));
    const payload = fileOpWaitTakePayload(wait) orelse return error.TestUnexpectedNull;
    defer fileOpPayloadDeinit(payload);
    switch (payload) {
        .unit => {},
        else => return error.TestUnexpectedResult,
    }

    const bytes = try tmp.dir.readFileAlloc(std.testing.allocator, "out.txt", 1024);
    defer std.testing.allocator.free(bytes);
    try std.testing.expectEqualStrings("hello", bytes);
}

test "file op wait fifo write cancels promptly" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const fifo_path = try std.fmt.allocPrint(std.testing.allocator, ".zig-cache/tmp/{s}/block-write.fifo", .{tmp.sub_path});
    defer std.testing.allocator.free(fifo_path);
    var child = std.process.Child.init(&.{ "mkfifo", fifo_path }, std.testing.allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    try child.spawn();
    const term = try child.wait();
    switch (term) {
        .Exited => |code| if (code != 0) return error.Unexpected,
        else => return error.Unexpected,
    }

    const ReaderState = struct {
        path: []const u8,
        mutex: std.Thread.Mutex = .{},
        cond: std.Thread.Condition = .{},
        opened: bool = false,
        release: bool = false,
    };

    const readerMain = struct {
        fn run(state: *ReaderState) void {
            const fd = std.posix.open(state.path, .{
                .ACCMODE = .RDONLY,
                .NONBLOCK = true,
                .CLOEXEC = true,
            }, 0) catch @panic("failed to open fifo reader");
            defer std.posix.close(fd);

            state.mutex.lock();
            state.opened = true;
            state.cond.broadcast();
            while (!state.release) {
                state.cond.wait(&state.mutex);
            }
            state.mutex.unlock();
        }
    }.run;

    var reader = ReaderState{ .path = fifo_path };
    const thread = try std.Thread.spawn(.{}, readerMain, .{&reader});
    defer thread.join();

    reader.mutex.lock();
    while (!reader.opened) {
        reader.cond.wait(&reader.mutex);
    }
    reader.mutex.unlock();

    const data = try std.testing.allocator.alloc(u8, 1024 * 1024);
    defer std.testing.allocator.free(data);
    @memset(data, 'x');

    const wait = fileOpWriteWaitNew(.write, fifo_path, data);
    defer fileOpWaitRelease(wait);

    std.Thread.sleep(50 * std.time.ns_per_ms);
    fileOpWaitCancel(wait);
    try std.testing.expectEqual(FileOpWaitOutcome.canceled, fileOpWaitAwait(wait));
    try std.testing.expect(fileOpWaitTakePayload(wait) == null);

    reader.mutex.lock();
    reader.release = true;
    reader.cond.broadcast();
    reader.mutex.unlock();
}
