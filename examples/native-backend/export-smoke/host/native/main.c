#include "export-smoke.h"
#include <stdio.h>
#include <string.h>

int main(void) {
  flix_init(0, NULL);
  flix_ctx_t *ctx = flix_ctx_new();
  if (!ctx) {
    fprintf(stderr, "flix_ctx_new returned NULL\n");
    return 1;
  }

  int32_t sum = 0;
  flix_exec_t add_r = flix_export_Api_add(ctx, (int32_t)1, (int32_t)2, &sum);
  if (add_r.tag != FLIX_EXEC_OK || sum != 3) {
    fprintf(stderr, "bad add result\n");
    return 2;
  }

  const uint8_t hello_bytes[] = { 'h', 'e', 'l', 'l', 'o' };
  flix_string_t hello = flix_string_from_utf8(ctx, hello_bytes, 5);
  flix_string_t echoed = 0;
  flix_exec_t echo_r = flix_export_Api_echo(ctx, hello, &echoed);
  if (echo_r.tag != FLIX_EXEC_OK) {
    fprintf(stderr, "bad echo tag: %lld\n", (long long)echo_r.tag);
    return 3;
  }
  int64_t echoed_len = 0;
  uint8_t *echoed_out = flix_string_to_utf8(ctx, echoed, &echoed_len);
  if (echoed_len != 5 || memcmp(echoed_out, hello_bytes, 5) != 0) {
    fprintf(stderr, "bad echo payload\n");
    return 4;
  }
  flix_free(echoed_out);
  flix_handle_release(ctx, echoed);

  const uint8_t data[] = { 0, 1, 2, 255 };
  flix_bytes_t arr = flix_bytes_from_slice(ctx, data, 4);
  int32_t arr_len = 0;
  flix_exec_t len_r = flix_export_Api_bytesLen(ctx, arr, &arr_len);
  if (len_r.tag != FLIX_EXEC_OK || arr_len != 4) {
    fprintf(stderr, "bad bytesLen result\n");
    return 5;
  }

  flix_bytes_t arr2 = 0;
  flix_exec_t id_r = flix_export_Api_bytesId(ctx, arr, &arr2);
  if (id_r.tag != FLIX_EXEC_OK) {
    fprintf(stderr, "bad bytesId tag: %lld\n", (long long)id_r.tag);
    return 6;
  }
  int64_t out_len = 0;
  uint8_t *out = flix_bytes_to_slice(ctx, arr2, &out_len);
  if (out_len != 4 || memcmp(out, data, 4) != 0) {
    fprintf(stderr, "bad bytesId payload\n");
    return 7;
  }
  flix_free(out);
  flix_handle_release(ctx, arr);
  flix_handle_release(ctx, arr2);

  flix_option_int32_t some_in;
  some_in.is_some = true;
  some_in.val = 41;
  flix_option_int32_t some_out;
  flix_exec_t some_r = flix_export_Api_maybeSucc(ctx, &some_in, &some_out);
  if (some_r.tag != FLIX_EXEC_OK || !some_out.is_some || some_out.val != 42) {
    fprintf(stderr, "bad maybeSucc(Some)\n");
    return 8;
  }

  flix_option_int32_t none_in;
  none_in.is_some = false;
  none_in.val = 0;
  flix_option_int32_t none_out;
  flix_exec_t none_r = flix_export_Api_maybeSucc(ctx, &none_in, &none_out);
  if (none_r.tag != FLIX_EXEC_OK || none_out.is_some) {
    fprintf(stderr, "bad maybeSucc(None)\n");
    return 9;
  }

  flix_tuple2_int32_string_t pair_in;
  pair_in.f0 = 7;
  pair_in.f1 = hello;
  flix_tuple2_string_int32_t pair_out;
  flix_exec_t pair_r = flix_export_Api_flipPair(ctx, &pair_in, &pair_out);
  if (pair_r.tag != FLIX_EXEC_OK) {
    fprintf(stderr, "bad flipPair tag: %lld\n", (long long)pair_r.tag);
    return 10;
  }
  int64_t pair_len = 0;
  uint8_t *pair_bytes = flix_string_to_utf8(ctx, pair_out.f0, &pair_len);
  if (pair_len != 5 || memcmp(pair_bytes, hello_bytes, 5) != 0 || pair_out.f1 != 7) {
    fprintf(stderr, "bad flipPair payload\n");
    return 11;
  }
  flix_free(pair_bytes);
  flix_handle_release(ctx, pair_out.f0);

  flix_result_int32_string_t even_out;
  flix_exec_t even_r = flix_export_Api_halfEven(ctx, 8, &even_out);
  if (even_r.tag != FLIX_EXEC_OK || !even_out.is_ok || even_out.ok != 4) {
    fprintf(stderr, "bad halfEven(8)\n");
    return 12;
  }

  flix_result_int32_string_t odd_out;
  flix_exec_t odd_r = flix_export_Api_halfEven(ctx, 7, &odd_out);
  if (odd_r.tag != FLIX_EXEC_OK || odd_out.is_ok) {
    fprintf(stderr, "bad halfEven(7) tag/is_ok\n");
    return 13;
  }
  int64_t odd_len = 0;
  uint8_t *odd_bytes = flix_string_to_utf8(ctx, odd_out.err, &odd_len);
  if (odd_len != 3 || memcmp(odd_bytes, "odd", 3) != 0) {
    fprintf(stderr, "bad halfEven(7) payload\n");
    return 14;
  }
  flix_free(odd_bytes);
  flix_handle_release(ctx, odd_out.err);

  flix_record_name_string_score_int32_t user_in;
  user_in.name = hello;
  user_in.score = 41;
  flix_record_label_string_score_int32_t badge_out;
  flix_exec_t badge_r = flix_export_Api_badge(ctx, &user_in, &badge_out);
  if (badge_r.tag != FLIX_EXEC_OK || badge_out.score != 42) {
    fprintf(stderr, "bad badge score\n");
    return 15;
  }
  int64_t badge_len = 0;
  uint8_t *badge_bytes = flix_string_to_utf8(ctx, badge_out.label, &badge_len);
  if (badge_len != 5 || memcmp(badge_bytes, hello_bytes, 5) != 0) {
    fprintf(stderr, "bad badge label\n");
    return 16;
  }
  flix_free(badge_bytes);
  flix_handle_release(ctx, badge_out.label);

  int32_t list_in_buf[] = { 1, 2, 3 };
  flix_list_int32_t list_in;
  list_in.len = 3;
  list_in.ptr = list_in_buf;
  flix_list_int32_t list_out;
  flix_exec_t list_r = flix_export_Api_prependAnswer(ctx, &list_in, &list_out);
  if (list_r.tag != FLIX_EXEC_OK || list_out.len != 4) {
    fprintf(stderr, "bad prependAnswer metadata\n");
    return 17;
  }
  if (list_out.ptr[0] != 42 || list_out.ptr[1] != 1 || list_out.ptr[2] != 2 || list_out.ptr[3] != 3) {
    fprintf(stderr, "bad prependAnswer payload\n");
    return 18;
  }
  flix_free(list_out.ptr);

  int32_t bump_in_buf[] = { 1, 2, 3 };
  flix_array_int32_t bump_in;
  bump_in.len = 3;
  bump_in.ptr = bump_in_buf;
  flix_array_int32_t bump_out;
  flix_exec_t bump_r = flix_export_Api_echoInts(ctx, &bump_in, &bump_out);
  if (bump_r.tag != FLIX_EXEC_OK || bump_out.len != 3) {
    fprintf(stderr, "bad echoInts metadata\n");
    return 19;
  }
  if (bump_out.ptr[0] != 1 || bump_out.ptr[1] != 2 || bump_out.ptr[2] != 3) {
    fprintf(stderr, "bad echoInts payload\n");
    return 20;
  }
  flix_free(bump_out.ptr);

  const uint8_t world_bytes[] = { 'w', 'o', 'r', 'l', 'd' };
  flix_string_t world = flix_string_from_utf8(ctx, world_bytes, 5);
  flix_string_t names_in_buf[] = { hello, world };
  flix_array_string_t names_in;
  names_in.len = 2;
  names_in.ptr = names_in_buf;
  flix_array_string_t names_out;
  flix_exec_t names_r = flix_export_Api_echoNames(ctx, &names_in, &names_out);
  if (names_r.tag != FLIX_EXEC_OK || names_out.len != 2) {
    fprintf(stderr, "bad echoNames metadata\n");
    return 21;
  }
  int64_t name0_len = 0;
  int64_t name1_len = 0;
  uint8_t *name0 = flix_string_to_utf8(ctx, names_out.ptr[0], &name0_len);
  uint8_t *name1 = flix_string_to_utf8(ctx, names_out.ptr[1], &name1_len);
  if (name0_len != 5 || memcmp(name0, hello_bytes, 5) != 0 || name1_len != 5 || memcmp(name1, world_bytes, 5) != 0) {
    fprintf(stderr, "bad echoNames payload\n");
    return 22;
  }
  flix_free(name0);
  flix_free(name1);
  flix_handle_release(ctx, names_out.ptr[0]);
  flix_handle_release(ctx, names_out.ptr[1]);
  flix_free(names_out.ptr);

  flix_record_name_string_score_int32_t users_in_buf[2];
  users_in_buf[0].name = hello;
  users_in_buf[0].score = 41;
  users_in_buf[1].name = world;
  users_in_buf[1].score = 9;
  flix_list_record_name_string_score_int32_t users_in;
  users_in.len = 2;
  users_in.ptr = users_in_buf;
  flix_list_record_label_string_score_int32_t users_out;
  flix_exec_t users_r = flix_export_Api_promoteUsers(ctx, &users_in, &users_out);
  if (users_r.tag != FLIX_EXEC_OK || users_out.len != 2) {
    fprintf(stderr, "bad promoteUsers metadata\n");
    return 23;
  }
  if (users_out.ptr[0].score != 42 || users_out.ptr[1].score != 10) {
    fprintf(stderr, "bad promoteUsers scores\n");
    return 24;
  }
  int64_t label0_len = 0;
  int64_t label1_len = 0;
  uint8_t *label0 = flix_string_to_utf8(ctx, users_out.ptr[0].label, &label0_len);
  uint8_t *label1 = flix_string_to_utf8(ctx, users_out.ptr[1].label, &label1_len);
  if (label0_len != 5 || memcmp(label0, hello_bytes, 5) != 0 || label1_len != 5 || memcmp(label1, world_bytes, 5) != 0) {
    fprintf(stderr, "bad promoteUsers labels\n");
    return 25;
  }
  flix_free(label0);
  flix_free(label1);
  flix_handle_release(ctx, users_out.ptr[0].label);
  flix_handle_release(ctx, users_out.ptr[1].label);
  flix_free(users_out.ptr);

  flix_array_record_name_string_score_int32_t user_arr_in;
  user_arr_in.len = 2;
  user_arr_in.ptr = users_in_buf;
  flix_array_record_name_string_score_int32_t user_arr_out;
  flix_exec_t user_arr_r = flix_export_Api_echoUserArray(ctx, &user_arr_in, &user_arr_out);
  if (user_arr_r.tag != FLIX_EXEC_OK || user_arr_out.len != 2) {
    fprintf(stderr, "bad echoUserArray metadata\n");
    return 26;
  }
  if (user_arr_out.ptr[0].score != 41 || user_arr_out.ptr[1].score != 9) {
    fprintf(stderr, "bad echoUserArray scores\n");
    return 27;
  }
  int64_t arr_name0_len = 0;
  int64_t arr_name1_len = 0;
  uint8_t *arr_name0 = flix_string_to_utf8(ctx, user_arr_out.ptr[0].name, &arr_name0_len);
  uint8_t *arr_name1 = flix_string_to_utf8(ctx, user_arr_out.ptr[1].name, &arr_name1_len);
  if (arr_name0_len != 5 || memcmp(arr_name0, hello_bytes, 5) != 0 || arr_name1_len != 5 || memcmp(arr_name1, world_bytes, 5) != 0) {
    fprintf(stderr, "bad echoUserArray names\n");
    return 28;
  }
  flix_free(arr_name0);
  flix_free(arr_name1);
  flix_handle_release(ctx, user_arr_out.ptr[0].name);
  flix_handle_release(ctx, user_arr_out.ptr[1].name);
  flix_free(user_arr_out.ptr);

  flix_tuple2_int32_string_t pairs_in_buf[2];
  pairs_in_buf[0].f0 = 7;
  pairs_in_buf[0].f1 = hello;
  pairs_in_buf[1].f0 = 9;
  pairs_in_buf[1].f1 = world;
  flix_list_tuple2_int32_string_t pairs_in;
  pairs_in.len = 2;
  pairs_in.ptr = pairs_in_buf;
  flix_list_tuple2_string_int32_t pairs_out;
  flix_exec_t pairs_r = flix_export_Api_flipPairs(ctx, &pairs_in, &pairs_out);
  if (pairs_r.tag != FLIX_EXEC_OK || pairs_out.len != 2) {
    fprintf(stderr, "bad flipPairs metadata\n");
    return 29;
  }
  int64_t pair0_len = 0;
  int64_t pair1_len = 0;
  uint8_t *pair0 = flix_string_to_utf8(ctx, pairs_out.ptr[0].f0, &pair0_len);
  uint8_t *pair1 = flix_string_to_utf8(ctx, pairs_out.ptr[1].f0, &pair1_len);
  if (pair0_len != 5 || memcmp(pair0, hello_bytes, 5) != 0 || pairs_out.ptr[0].f1 != 7 ||
      pair1_len != 5 || memcmp(pair1, world_bytes, 5) != 0 || pairs_out.ptr[1].f1 != 9) {
    fprintf(stderr, "bad flipPairs payload\n");
    return 30;
  }
  flix_free(pair0);
  flix_free(pair1);
  flix_handle_release(ctx, pairs_out.ptr[0].f0);
  flix_handle_release(ctx, pairs_out.ptr[1].f0);
  flix_free(pairs_out.ptr);

  flix_option_int32_t maybe_in_buf[3];
  maybe_in_buf[0].is_some = true;
  maybe_in_buf[0].val = 41;
  maybe_in_buf[1].is_some = false;
  maybe_in_buf[1].val = 0;
  maybe_in_buf[2].is_some = true;
  maybe_in_buf[2].val = 9;
  flix_array_option_int32_t maybe_in;
  maybe_in.len = 3;
  maybe_in.ptr = maybe_in_buf;
  flix_array_option_int32_t maybe_out;
  flix_exec_t maybe_r = flix_export_Api_echoMaybeInts(ctx, &maybe_in, &maybe_out);
  if (maybe_r.tag != FLIX_EXEC_OK || maybe_out.len != 3) {
    fprintf(stderr, "bad echoMaybeInts metadata\n");
    return 31;
  }
  if (!maybe_out.ptr[0].is_some || maybe_out.ptr[0].val != 41 ||
      maybe_out.ptr[1].is_some ||
      !maybe_out.ptr[2].is_some || maybe_out.ptr[2].val != 9) {
    fprintf(stderr, "bad echoMaybeInts payload\n");
    return 32;
  }
  flix_free(maybe_out.ptr);

  flix_string_t ignored = 0;
  flix_exec_t susp_r = flix_export_Api_suspendEcho(ctx, hello, &ignored);
  if (susp_r.tag != FLIX_EXEC_SUSPENDED) {
    fprintf(stderr, "bad suspendEcho tag: %lld\n", (long long)susp_r.tag);
    return 33;
  }

  flix_handle_t susp = (flix_handle_t)susp_r.payload;
  flix_request_Api_suspendEcho_t req;
  flix_export_request_Api_suspendEcho(ctx, susp, &req);
  flix_string_t arg0 = req.arg0;
  int64_t arg0_len = 0;
  uint8_t *arg0_out = flix_string_to_utf8(ctx, arg0, &arg0_len);
  if (arg0_len != 5 || memcmp(arg0_out, hello_bytes, 5) != 0) {
    fprintf(stderr, "bad suspension arg0\n");
    return 34;
  }
  flix_free(arg0_out);
  flix_handle_release(ctx, arg0);

  const uint8_t ok_bytes[] = { 'o', 'k' };
  flix_string_t ok = flix_string_from_utf8(ctx, ok_bytes, 2);
  flix_string_t resumed = 0;
  flix_exec_t resume_r = flix_export_resume_Api_suspendEcho(ctx, susp, ok, &resumed);
  if (resume_r.tag != FLIX_EXEC_OK) {
    fprintf(stderr, "bad suspendEcho resume tag: %lld\n", (long long)resume_r.tag);
    return 35;
  }
  int64_t resumed_len = 0;
  uint8_t *resumed_out = flix_string_to_utf8(ctx, resumed, &resumed_len);
  if (resumed_len != 2 || memcmp(resumed_out, ok_bytes, 2) != 0) {
    fprintf(stderr, "bad suspendEcho resume payload\n");
    return 36;
  }
  flix_free(resumed_out);
  flix_handle_release(ctx, ok);
  flix_handle_release(ctx, resumed);
  flix_handle_release(ctx, susp);
  flix_handle_release(ctx, world);
  flix_handle_release(ctx, hello);

  flix_ctx_free(ctx);
  printf("OK\n");
  return 0;
}
