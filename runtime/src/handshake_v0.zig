const std = @import("std");

const Atomic = std.atomic.Value;

const WorkerState = struct {
    seen_epoch: Atomic(u64) = .init(0),
    blocked: Atomic(bool) = .init(false),
    cooperations: Atomic(u32) = .init(0),
    stop_requested: Atomic(bool) = .init(false),
    entered_blocked: Atomic(bool) = .init(false),
    iterations: Atomic(u64) = .init(0),
};

const Handshake = struct {
    request_epoch: Atomic(u64) = .init(0),
    release_epoch: Atomic(u64) = .init(0),
    ack_count: Atomic(u32) = .init(0),
    stw: Atomic(bool) = .init(false),

    fn requestStw(self: *Handshake) u64 {
        self.ack_count.store(0, .release);
        self.stw.store(true, .release);
        const epoch = self.request_epoch.fetchAdd(1, .acq_rel) + 1;
        self.release_epoch.store(epoch - 1, .release);
        return epoch;
    }

    fn requestSoft(self: *Handshake) u64 {
        self.ack_count.store(0, .release);
        self.stw.store(false, .release);
        const epoch = self.request_epoch.fetchAdd(1, .acq_rel) + 1;
        self.release_epoch.store(epoch, .release);
        return epoch;
    }

    fn waitEpoch(self: *Handshake, epoch: u64, states: []WorkerState) void {
        while (true) {
            var target_count: u32 = 0;
            var all_seen = true;
            for (states) |*state| {
                if (state.blocked.load(.acquire)) continue;
                target_count += 1;
                if (state.seen_epoch.load(.acquire) < epoch) {
                    all_seen = false;
                    break;
                }
            }
            const acks = self.ack_count.load(.acquire);
            if (all_seen and acks >= target_count) return;
            std.atomic.spinLoopHint();
        }
    }

    fn waitStw(self: *Handshake, epoch: u64, states: []WorkerState) void {
        self.waitEpoch(epoch, states);
    }

    fn waitSoft(self: *Handshake, epoch: u64, states: []WorkerState) void {
        self.waitEpoch(epoch, states);
    }

    fn releaseStw(self: *Handshake, epoch: u64) void {
        self.release_epoch.store(epoch, .release);
        self.stw.store(false, .release);
    }
};

fn pollcheck(handshake: *Handshake, state: *WorkerState) void {
    const req = handshake.request_epoch.load(.acquire);
    if (req == state.seen_epoch.load(.acquire)) return;

    state.seen_epoch.store(req, .release);
    _ = state.cooperations.fetchAdd(1, .acq_rel);
    _ = handshake.ack_count.fetchAdd(1, .acq_rel);

    if (handshake.stw.load(.acquire)) {
        while (handshake.release_epoch.load(.acquire) < req) {
            std.atomic.spinLoopHint();
        }
    }
}

fn busyWorkerMain(handshake: *Handshake, state: *WorkerState) void {
    while (!state.stop_requested.load(.acquire)) {
        pollcheck(handshake, state);
        _ = state.iterations.fetchAdd(1, .acq_rel);
    }
}

fn blockedWorkerMain(handshake: *Handshake, state: *WorkerState) void {
    state.blocked.store(true, .release);
    state.entered_blocked.store(true, .release);

    std.Thread.sleep(50 * std.time.ns_per_ms);

    // Mirror the runtime's BlockedGuard.exitAndCooperate ordering: cooperate while
    // still marked blocked, then clear the blocked bit once the handshake has been observed.
    pollcheck(handshake, state);
    state.blocked.store(false, .release);

    while (!state.stop_requested.load(.acquire)) {
        pollcheck(handshake, state);
    }
}

fn waitUntil(comptime T: type, atom: *Atomic(T), expected: T, timeout_ms: u64) !void {
    const start = std.time.nanoTimestamp();
    const timeout_ns: i128 = @as(i128, timeout_ms) * @as(i128, std.time.ns_per_ms);
    while (atom.load(.acquire) != expected) {
        if (std.time.nanoTimestamp() - start > timeout_ns) return error.Timeout;
        std.atomic.spinLoopHint();
        std.Thread.sleep(1 * std.time.ns_per_ms);
    }
}

fn waitUntilAtLeast(comptime T: type, atom: *Atomic(T), expected: T, timeout_ms: u64) !void {
    const start = std.time.nanoTimestamp();
    const timeout_ns: i128 = @as(i128, timeout_ms) * @as(i128, std.time.ns_per_ms);
    while (atom.load(.acquire) < expected) {
        if (std.time.nanoTimestamp() - start > timeout_ns) return error.Timeout;
        std.atomic.spinLoopHint();
        std.Thread.sleep(1 * std.time.ns_per_ms);
    }
}

test "stw handshake reaches busy workers" {
    var handshake = Handshake{};
    var states = [_]WorkerState{.{}, .{}, .{}};
    const threads = [_]std.Thread{
        try std.Thread.spawn(.{}, busyWorkerMain, .{ &handshake, &states[0] }),
        try std.Thread.spawn(.{}, busyWorkerMain, .{ &handshake, &states[1] }),
        try std.Thread.spawn(.{}, busyWorkerMain, .{ &handshake, &states[2] }),
    };
    defer {
        for (&states) |*state| state.stop_requested.store(true, .release);
        for (threads) |thread| thread.join();
    }

    const epoch = handshake.requestStw();
    handshake.waitStw(epoch, states[0..]);

    try std.testing.expectEqual(@as(u32, 3), handshake.ack_count.load(.acquire));
    for (states) |state| {
        try std.testing.expectEqual(epoch, state.seen_epoch.load(.acquire));
        try std.testing.expectEqual(@as(u32, 1), state.cooperations.load(.acquire));
    }

    handshake.releaseStw(epoch);
}

test "stw handshake skips blocked worker and blocked worker cooperates after unblock" {
    var handshake = Handshake{};
    var states = [_]WorkerState{.{}, .{}};
    const threads = [_]std.Thread{
        try std.Thread.spawn(.{}, busyWorkerMain, .{ &handshake, &states[0] }),
        try std.Thread.spawn(.{}, blockedWorkerMain, .{ &handshake, &states[1] }),
    };
    defer {
        for (&states) |*state| state.stop_requested.store(true, .release);
        for (threads) |thread| thread.join();
    }

    try waitUntil(bool, &states[1].entered_blocked, true, 200);

    const epoch = handshake.requestStw();
    handshake.waitStw(epoch, states[0..]);

    try std.testing.expectEqual(@as(u32, 1), handshake.ack_count.load(.acquire));
    try std.testing.expectEqual(epoch, states[0].seen_epoch.load(.acquire));
    try std.testing.expectEqual(@as(u64, 0), states[1].seen_epoch.load(.acquire));

    handshake.releaseStw(epoch);

    try waitUntilAtLeast(u64, &states[1].seen_epoch, epoch, 500);
    try std.testing.expect(states[1].cooperations.load(.acquire) >= 1);
}

test "soft handshake reaches busy workers and does not park progress" {
    var handshake = Handshake{};
    var states = [_]WorkerState{.{}, .{}};
    const threads = [_]std.Thread{
        try std.Thread.spawn(.{}, busyWorkerMain, .{ &handshake, &states[0] }),
        try std.Thread.spawn(.{}, busyWorkerMain, .{ &handshake, &states[1] }),
    };
    defer {
        for (&states) |*state| state.stop_requested.store(true, .release);
        for (threads) |thread| thread.join();
    }

    const before0 = states[0].iterations.load(.acquire);
    const before1 = states[1].iterations.load(.acquire);

    const epoch = handshake.requestSoft();
    handshake.waitSoft(epoch, states[0..]);

    try std.testing.expectEqual(@as(u32, 2), handshake.ack_count.load(.acquire));
    for (states) |state| {
        try std.testing.expectEqual(epoch, state.seen_epoch.load(.acquire));
        try std.testing.expectEqual(@as(u32, 1), state.cooperations.load(.acquire));
    }

    try waitUntilAtLeast(u64, &states[0].iterations, before0 + 10, 500);
    try waitUntilAtLeast(u64, &states[1].iterations, before1 + 10, 500);
}

test "soft handshake skips blocked worker and blocked worker cooperates after unblock" {
    var handshake = Handshake{};
    var states = [_]WorkerState{.{}, .{}};
    const threads = [_]std.Thread{
        try std.Thread.spawn(.{}, busyWorkerMain, .{ &handshake, &states[0] }),
        try std.Thread.spawn(.{}, blockedWorkerMain, .{ &handshake, &states[1] }),
    };
    defer {
        for (&states) |*state| state.stop_requested.store(true, .release);
        for (threads) |thread| thread.join();
    }

    try waitUntil(bool, &states[1].entered_blocked, true, 200);
    const before = states[0].iterations.load(.acquire);

    const epoch = handshake.requestSoft();
    handshake.waitSoft(epoch, states[0..]);

    try std.testing.expectEqual(@as(u32, 1), handshake.ack_count.load(.acquire));
    try std.testing.expectEqual(epoch, states[0].seen_epoch.load(.acquire));
    try std.testing.expectEqual(@as(u64, 0), states[1].seen_epoch.load(.acquire));
    try waitUntilAtLeast(u64, &states[0].iterations, before + 10, 500);

    try waitUntilAtLeast(u64, &states[1].seen_epoch, epoch, 500);
    try std.testing.expect(states[1].cooperations.load(.acquire) >= 1);
}
