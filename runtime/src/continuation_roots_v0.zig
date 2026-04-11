const std = @import("std");

const StubFlixTypeInfo = extern struct {
    type_id: u32,
    size_bytes: u32,
    ptr_count: u32,
    ptr_offs: ?[*]const u32,
    trace: ?*anyopaque,
    invoke: ?*anyopaque,
    apply: ?*anyopaque,
    copy: ?*anyopaque,
};

const stub_ti = StubFlixTypeInfo{
    .type_id = 0,
    .size_bytes = 16,
    .ptr_count = 0,
    .ptr_offs = null,
    .trace = null,
    .invoke = null,
    .apply = null,
    .copy = null,
};

export const flix_ti_array_prim: StubFlixTypeInfo = stub_ti;
export const flix_ti_array_ptr: StubFlixTypeInfo = stub_ti;
export const flix_ti_string: StubFlixTypeInfo = stub_ti;
export const flix_ti_bigint: StubFlixTypeInfo = stub_ti;
export const flix_ti_bigdecimal: StubFlixTypeInfo = stub_ti;
export const flix_ti_suspension: StubFlixTypeInfo = stub_ti;

export fn flix_effect_name(eff_sym_id: i64) ?[*:0]const u8 {
    _ = eff_sym_id;
    return "stub";
}

export fn flix_op_name(eff_sym_id: i64, op_index: i64) ?[*:0]const u8 {
    _ = eff_sym_id;
    _ = op_index;
    return "stub";
}

export fn flix_native_timer_sleep(ms: u64) void {
    _ = ms;
}

export fn flix_native_timer_wait_new(ms: u64) *anyopaque {
    _ = ms;
    return @ptrFromInt(1);
}

export fn flix_native_timer_wait_cancel(wait: *anyopaque) void {
    _ = wait;
}

export fn flix_native_timer_wait_await(wait: *anyopaque) i32 {
    _ = wait;
    return 0;
}

export fn flix_native_timer_wait_release(wait: *anyopaque) void {
    _ = wait;
}

export fn flix_native_tcp_connect_wait_new(ip_bytes_ptr: [*]const u8, ip_bytes_len: usize, port: u16) ?*anyopaque {
    _ = ip_bytes_ptr;
    _ = ip_bytes_len;
    _ = port;
    return @ptrFromInt(1);
}

export fn flix_native_tcp_connect_wait_cancel(wait: *anyopaque) void {
    _ = wait;
}

export fn flix_native_tcp_connect_wait_await(wait: *anyopaque) i32 {
    _ = wait;
    return 0;
}

export fn flix_native_tcp_connect_wait_payload_kind(wait: *anyopaque) i32 {
    _ = wait;
    return 0;
}

export fn flix_native_tcp_connect_wait_take_socket_handle(wait: *anyopaque) usize {
    _ = wait;
    return 0;
}

export fn flix_native_tcp_connect_wait_error_ptr(wait: *anyopaque) ?*anyopaque {
    _ = wait;
    return null;
}

export fn flix_native_tcp_connect_wait_error_len(wait: *anyopaque) usize {
    _ = wait;
    return 0;
}

export fn flix_native_tcp_connect_wait_release(wait: *anyopaque) void {
    _ = wait;
}

export fn flix_native_http_wait_new(req_blob_ptr: [*]const u8, req_blob_len: usize) *anyopaque {
    _ = req_blob_ptr;
    _ = req_blob_len;
    return @ptrFromInt(1);
}

export fn flix_native_http_wait_cancel(wait: *anyopaque) void {
    _ = wait;
}

export fn flix_native_http_wait_await(wait: *anyopaque) i32 {
    _ = wait;
    return 0;
}

export fn flix_native_http_wait_response_ptr(wait: *anyopaque) ?*anyopaque {
    _ = wait;
    return null;
}

export fn flix_native_http_wait_response_len(wait: *anyopaque) usize {
    _ = wait;
    return 0;
}

export fn flix_native_http_wait_release(wait: *anyopaque) void {
    _ = wait;
}

comptime {
    _ = @import("flix_rt_llvm.zig");
}

test "pull in flix_rt_llvm continuation/runtime tests" {
    try std.testing.expect(true);
}
