const std = @import("std");

pub fn StickyCancelWait(comptime Payload: type, comptime discardPayload: fn (Payload) void) type {
    return struct {
        const Self = @This();

        pub const Outcome = enum(i32) {
            completed = 0,
            canceled = 1,
        };

        pub const AwaitResult = union(Outcome) {
            completed: Payload,
            canceled: void,
        };

        mutex: std.Thread.Mutex = .{},
        cond: std.Thread.Condition = .{},
        ref_count: u32 = 2, // caller + worker
        cancel_visible: bool = false,
        done: bool = false,
        outcome: Outcome = .canceled,
        payload: ?Payload = null,

        pub fn deinit(self: *Self) void {
            if (self.payload) |payload| {
                discardPayload(payload);
                self.payload = null;
            }
        }

        pub fn releaseRef(self: *Self) bool {
            self.mutex.lock();
            defer self.mutex.unlock();
            self.ref_count -= 1;
            return self.ref_count == 0;
        }

        pub fn addRef(self: *Self) void {
            self.mutex.lock();
            defer self.mutex.unlock();
            self.ref_count += 1;
        }

        pub fn requestCancel(self: *Self) bool {
            self.mutex.lock();
            defer self.mutex.unlock();

            const should_cancel = !self.done and !self.cancel_visible;
            if (should_cancel) {
                self.cancel_visible = true;
                self.cond.broadcast();
            }
            return should_cancel;
        }

        pub fn isCancelVisible(self: *Self) bool {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.cancel_visible;
        }

        pub fn workerComplete(self: *Self, payload: Payload) void {
            var discard_payload = false;

            self.mutex.lock();
            if (self.cancel_visible) {
                self.payload = null;
                self.outcome = .canceled;
                discard_payload = true;
            } else {
                self.payload = payload;
                self.outcome = .completed;
            }
            self.done = true;
            self.cond.broadcast();
            self.mutex.unlock();

            if (discard_payload) {
                discardPayload(payload);
            }
        }

        pub fn workerCancel(self: *Self) void {
            self.mutex.lock();
            self.payload = null;
            self.outcome = .canceled;
            self.done = true;
            self.cond.broadcast();
            self.mutex.unlock();
        }

        pub fn await(self: *Self) AwaitResult {
            self.mutex.lock();
            while (!self.done and !self.cancel_visible) {
                self.cond.wait(&self.mutex);
            }

            const result: AwaitResult =
                if (self.cancel_visible)
                    .{ .canceled = {} }
                else switch (self.outcome) {
                    .completed => .{ .completed = self.payload.? },
                    .canceled => .{ .canceled = {} },
                };

            self.mutex.unlock();
            return result;
        }

        pub fn peekCompletedPayload(self: *Self) ?Payload {
            self.mutex.lock();
            defer self.mutex.unlock();

            if (self.cancel_visible or !self.done or self.outcome != .completed) {
                return null;
            }
            return self.payload;
        }

        pub fn takeCompletedPayload(self: *Self) ?Payload {
            self.mutex.lock();
            defer self.mutex.unlock();

            if (self.cancel_visible or !self.done or self.outcome != .completed) {
                return null;
            }

            const payload = self.payload;
            self.payload = null;
            return payload;
        }
    };
}

fn discardTestPayload(_: u32) void {}

test "sticky cancel wait stays canceled after late completion" {
    const Wait = StickyCancelWait(u32, discardTestPayload);

    var wait = Wait{};
    defer wait.deinit();

    try std.testing.expect(wait.requestCancel());
    wait.workerComplete(42);

    switch (wait.await()) {
        .canceled => {},
        .completed => return error.TestUnexpectedResult,
    }

    try std.testing.expect(wait.peekCompletedPayload() == null);
    try std.testing.expect(wait.takeCompletedPayload() == null);
}

test "sticky cancel wait transfers completed payload exactly once" {
    const Wait = StickyCancelWait(u32, discardTestPayload);

    var wait = Wait{};
    defer wait.deinit();

    wait.workerComplete(7);

    switch (wait.await()) {
        .completed => |payload| try std.testing.expectEqual(@as(u32, 7), payload),
        .canceled => return error.TestUnexpectedCancel,
    }

    try std.testing.expectEqual(@as(?u32, 7), wait.takeCompletedPayload());
    try std.testing.expect(wait.peekCompletedPayload() == null);
    try std.testing.expect(wait.takeCompletedPayload() == null);
}
