const std = @import("std");

const wire = @import("http_wire_v0.zig");
const impl = @import("http_request_std_wire_v0.zig");

pub const SysStatus = enum(u32) {
    ok = 0,
    err = 1,
    unsupported = 2,
};

pub const SysResult = extern struct {
    status: SysStatus,
    err_code: i32, // IoError.ErrorKind code (matches main/src/library/IoError.flix)
};

const IoErrorKind = struct {
    const invalid_input: i32 = 4;
    const invalid_data: i32 = 5;
    const unsupported: i32 = 12;
    const other: i32 = 14;
};

pub fn http_request(
    alloc: std.mem.Allocator,
    req: []const u8,
    resp_buf: []u8,
    out_resp_len: *u32,
) SysResult {
    out_resp_len.* = 0;

    const outcome = impl.httpRequest(alloc, req, null) catch {
        return .{ .status = .err, .err_code = IoErrorKind.other };
    };
    const resp_blob = switch (outcome) {
        .completed => |blob| blob,
        .canceled => return .{ .status = .err, .err_code = IoErrorKind.other },
    };
    defer alloc.free(resp_blob);

    if (resp_blob.len <= resp_buf.len) {
        @memcpy(resp_buf[0..resp_blob.len], resp_blob);
        out_resp_len.* = @intCast(resp_blob.len);
        return .{ .status = .ok, .err_code = 0 };
    }

    // Buffer too small: return a valid response blob describing the failure, if possible.
    const err_blob = wire.encodeResponse(
        alloc,
        false,
        0,
        &.{},
        "",
        IoErrorKind.invalid_data,
        "http response too large for buffer",
    ) catch {
        return .{ .status = .err, .err_code = IoErrorKind.other };
    };
    defer alloc.free(err_blob);

    if (err_blob.len <= resp_buf.len) {
        @memcpy(resp_buf[0..err_blob.len], err_blob);
        out_resp_len.* = @intCast(err_blob.len);
        return .{ .status = .ok, .err_code = 0 };
    }

    // Even the minimal error blob doesn't fit.
    return .{ .status = .err, .err_code = IoErrorKind.invalid_input };
}
