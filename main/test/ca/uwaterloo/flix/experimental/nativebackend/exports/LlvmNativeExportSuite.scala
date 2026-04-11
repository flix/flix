/*
 * Copyright 2026 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{CompilationTarget, Options, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

class LlvmNativeExportSuite extends AnyFunSuite {

  private val ArtifactName = "export-smoke"

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  private val ExampleSourceFile: Path =
    Paths.get("examples/native-backend/export-smoke/src/Api.flix")

  test("llvm-native-export-static-library") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native export test)")

    val outDir = Files.createTempDirectory("flix-llvm-native-export-out-")
    try {
      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir, artifactName = ArtifactName))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted
      flix.addFile(ExampleSourceFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val llvmDir = outDir.resolve("llvm").toAbsolutePath.normalize()
      val sdkManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.nativeManifestPath(outDir)
      val sdkIncludeDir = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.nativeIncludeDir(outDir)
      val header = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.nativeHeaderPath(outDir, ArtifactName)
      val lib = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.nativeStaticLibraryPath(outDir, ArtifactName)
      val shared = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.nativeSharedLibraryPath(outDir, ArtifactName)
      if (!Files.exists(sdkManifest)) fail(s"Missing native export SDK manifest: $sdkManifest")
      if (!Files.exists(header)) fail(s"Missing exports header: $header")
      if (!Files.exists(lib)) fail(s"Missing static library: $lib")
      if (!Files.exists(shared)) fail(s"Missing shared library: $shared")
      val manifestText = Files.readString(sdkManifest, StandardCharsets.UTF_8)
      val headerText = Files.readString(header, StandardCharsets.UTF_8)
      assert(manifestText.contains("flix-export-sdk-v0"))
      assert(manifestText.contains("\"target\": \"native\""))
      assert(manifestText.contains("\"symbol\": \"Api.add\""))
      assert(headerText.contains("typedef struct flix_record_name_string_score_int32_t {"))
      assert(headerText.contains("typedef struct flix_list_record_name_string_score_int32_t {"))
      assert(headerText.contains("typedef struct flix_list_record_label_string_score_int32_t {"))
      assert(headerText.contains("typedef struct flix_list_int32_t {"))
      assert(headerText.contains("typedef struct flix_list_tuple2_int32_string_t {"))
      assert(headerText.contains("typedef struct flix_list_tuple2_string_int32_t {"))
      assert(headerText.contains("typedef struct flix_array_int32_t {"))
      assert(headerText.contains("typedef struct flix_array_option_int32_t {"))
      assert(headerText.contains("typedef struct flix_array_string_t {"))
      assert(headerText.contains("typedef struct flix_array_record_name_string_score_int32_t {"))
      assert(headerText.contains("flix_exec_t flix_export_Api_badge(flix_ctx_t* ctx, const flix_record_name_string_score_int32_t* a0, flix_record_label_string_score_int32_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_Api_prependAnswer(flix_ctx_t* ctx, const flix_list_int32_t* a0, flix_list_int32_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_Api_echoInts(flix_ctx_t* ctx, const flix_array_int32_t* a0, flix_array_int32_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_Api_echoNames(flix_ctx_t* ctx, const flix_array_string_t* a0, flix_array_string_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_Api_promoteUsers(flix_ctx_t* ctx, const flix_list_record_name_string_score_int32_t* a0, flix_list_record_label_string_score_int32_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_Api_echoUserArray(flix_ctx_t* ctx, const flix_array_record_name_string_score_int32_t* a0, flix_array_record_name_string_score_int32_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_Api_flipPairs(flix_ctx_t* ctx, const flix_list_tuple2_int32_string_t* a0, flix_list_tuple2_string_int32_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_Api_echoMaybeInts(flix_ctx_t* ctx, const flix_array_option_int32_t* a0, flix_array_option_int32_t* out);"))
      assert(headerText.contains("flix_exec_t flix_export_resume_Api_suspendEcho(flix_ctx_t* ctx, flix_handle_t susp, flix_string_t resume, flix_string_t* out);"))
      assert(headerText.contains("typedef struct flix_request_Api_suspendEcho_t {"))
      assert(headerText.contains("void flix_export_request_Api_suspendEcho(flix_ctx_t* ctx, flix_handle_t susp, flix_request_Api_suspendEcho_t* out);"))

      val cFile = Files.createTempFile(outDir, "flix-llvm-export-smoke-", ".c")
      val exeName = if (isWindows) "export_smoke.exe" else "export_smoke"
      val exe = llvmDir.resolve(exeName)

      val cProgram =
        """
          |#include "export-smoke.h"
          |#include <stdio.h>
          |#include <string.h>
          |
          |int main(void) {
          |  flix_init(0, NULL);
          |  flix_ctx_t* ctx = flix_ctx_new();
          |  if (!ctx) {
          |    fprintf(stderr, "flix_ctx_new returned NULL\n");
          |    return 1;
          |  }
          |
          |  // Int32 export.
          |  int32_t sum = 0;
          |  flix_exec_t add_r = flix_export_Api_add(ctx, (int32_t)1, (int32_t)2, &sum);
          |  if (add_r.tag != FLIX_EXEC_OK) {
          |    fprintf(stderr, "bad add tag: %lld\n", (long long)add_r.tag);
          |    return 1;
          |  }
          |  if (sum != 3) {
          |    fprintf(stderr, "bad add value: %d\n", (int)sum);
          |    return 2;
          |  }
          |
          |  // String roundtrip export.
          |  const uint8_t hello_bytes[] = { 'h', 'e', 'l', 'l', 'o' };
          |  flix_string_t hello = flix_string_from_utf8(ctx, hello_bytes, 5);
          |  flix_string_t echoed = 0;
          |  flix_exec_t echo_r = flix_export_Api_echo(ctx, hello, &echoed);
          |  if (echo_r.tag != FLIX_EXEC_OK) {
          |    fprintf(stderr, "bad echo tag: %lld\n", (long long)echo_r.tag);
          |    return 3;
          |  }
          |  int64_t echoed_len = 0;
          |  uint8_t* echoed_out = flix_string_to_utf8(ctx, echoed, &echoed_len);
          |  if (echoed_len != 5 || memcmp(echoed_out, hello_bytes, 5) != 0) {
          |    fprintf(stderr, "bad echo payload\n");
          |    return 4;
          |  }
          |  flix_free(echoed_out);
          |  flix_handle_release(ctx, echoed);
          |
          |  // Bytes roundtrip export.
          |  const uint8_t data[] = { 0, 1, 2, 255 };
          |  flix_bytes_t arr = flix_bytes_from_slice(ctx, data, 4);
          |  int32_t arr_len = 0;
          |  flix_exec_t len_r = flix_export_Api_bytesLen(ctx, arr, &arr_len);
          |  if (len_r.tag != FLIX_EXEC_OK) {
          |    fprintf(stderr, "bad bytesLen tag: %lld\n", (long long)len_r.tag);
          |    return 5;
          |  }
          |  if (arr_len != 4) {
          |    fprintf(stderr, "bad bytesLen value: %d\n", (int)arr_len);
          |    return 6;
          |  }
          |  flix_bytes_t arr2 = 0;
          |  flix_exec_t id_r = flix_export_Api_bytesId(ctx, arr, &arr2);
          |  if (id_r.tag != FLIX_EXEC_OK) {
          |    fprintf(stderr, "bad bytesId tag: %lld\n", (long long)id_r.tag);
          |    return 7;
          |  }
          |  int64_t out_len = 0;
          |  uint8_t* out = flix_bytes_to_slice(ctx, arr2, &out_len);
          |  if (out_len != 4 || memcmp(out, data, 4) != 0) {
          |    fprintf(stderr, "bad bytesId payload\n");
          |    return 8;
          |  }
          |  flix_free(out);
          |  flix_handle_release(ctx, arr);
          |  flix_handle_release(ctx, arr2);
          |
          |  // Option export.
          |  flix_option_int32_t some_in;
          |  some_in.is_some = true;
          |  some_in.val = 41;
          |  flix_option_int32_t some_out;
          |  flix_exec_t some_r = flix_export_Api_maybeSucc(ctx, &some_in, &some_out);
          |  if (some_r.tag != FLIX_EXEC_OK || !some_out.is_some || some_out.val != 42) {
          |    fprintf(stderr, "bad maybeSucc(Some): tag=%lld is_some=%d val=%d\n", (long long)some_r.tag, (int)some_out.is_some, (int)some_out.val);
          |    return 9;
          |  }
          |  flix_option_int32_t none_in;
          |  none_in.is_some = false;
          |  none_in.val = 0;
          |  flix_option_int32_t none_out;
          |  flix_exec_t none_r = flix_export_Api_maybeSucc(ctx, &none_in, &none_out);
          |  if (none_r.tag != FLIX_EXEC_OK || none_out.is_some) {
          |    fprintf(stderr, "bad maybeSucc(None)\n");
          |    return 10;
          |  }
          |
          |  // Tuple export.
          |  flix_tuple2_int32_string_t pair_in;
          |  pair_in.f0 = 7;
          |  pair_in.f1 = hello;
          |  flix_tuple2_string_int32_t pair_out;
          |  flix_exec_t pair_r = flix_export_Api_flipPair(ctx, &pair_in, &pair_out);
          |  if (pair_r.tag != FLIX_EXEC_OK) {
          |    fprintf(stderr, "bad flipPair tag: %lld\n", (long long)pair_r.tag);
          |    return 11;
          |  }
          |  int64_t pair_len = 0;
          |  uint8_t* pair_bytes = flix_string_to_utf8(ctx, pair_out.f0, &pair_len);
          |  if (pair_len != 5 || memcmp(pair_bytes, hello_bytes, 5) != 0 || pair_out.f1 != 7) {
          |    fprintf(stderr, "bad flipPair payload\n");
          |    return 12;
          |  }
          |  flix_free(pair_bytes);
          |  flix_handle_release(ctx, pair_out.f0);
          |
          |  // Result export.
          |  flix_result_int32_string_t even_out;
          |  flix_exec_t even_r = flix_export_Api_halfEven(ctx, 8, &even_out);
          |  if (even_r.tag != FLIX_EXEC_OK || !even_out.is_ok || even_out.ok != 4) {
          |    fprintf(stderr, "bad halfEven(8)\n");
          |    return 13;
          |  }
          |  flix_result_int32_string_t odd_out;
          |  flix_exec_t odd_r = flix_export_Api_halfEven(ctx, 7, &odd_out);
          |  if (odd_r.tag != FLIX_EXEC_OK || odd_out.is_ok) {
          |    fprintf(stderr, "bad halfEven(7) tag/is_ok\n");
          |    return 14;
          |  }
          |  int64_t odd_len = 0;
          |  uint8_t* odd_bytes = flix_string_to_utf8(ctx, odd_out.err, &odd_len);
          |  if (odd_len != 3 || memcmp(odd_bytes, "odd", 3) != 0) {
          |    fprintf(stderr, "bad halfEven(7) payload\n");
          |    return 15;
          |  }
          |  flix_free(odd_bytes);
          |  flix_handle_release(ctx, odd_out.err);
          |
          |  // Record export.
          |  flix_record_name_string_score_int32_t user_in;
          |  user_in.name = hello;
          |  user_in.score = 41;
          |  flix_record_label_string_score_int32_t badge_out;
          |  flix_exec_t badge_r = flix_export_Api_badge(ctx, &user_in, &badge_out);
          |  if (badge_r.tag != FLIX_EXEC_OK || badge_out.score != 42) {
          |    fprintf(stderr, "bad badge score\n");
          |    return 16;
          |  }
          |  int64_t badge_len = 0;
          |  uint8_t* badge_bytes = flix_string_to_utf8(ctx, badge_out.label, &badge_len);
          |  if (badge_len != 5 || memcmp(badge_bytes, hello_bytes, 5) != 0) {
          |    fprintf(stderr, "bad badge label\n");
          |    return 17;
          |  }
          |  flix_free(badge_bytes);
          |  flix_handle_release(ctx, badge_out.label);
          |
          |  // List export.
          |  int32_t list_in_buf[] = { 1, 2, 3 };
          |  flix_list_int32_t list_in;
          |  list_in.len = 3;
          |  list_in.ptr = list_in_buf;
          |  flix_list_int32_t list_out;
          |  flix_exec_t list_r = flix_export_Api_prependAnswer(ctx, &list_in, &list_out);
          |  if (list_r.tag != FLIX_EXEC_OK || list_out.len != 4) {
          |    fprintf(stderr, "bad prependAnswer metadata\n");
          |    return 18;
          |  }
          |  if (list_out.ptr[0] != 42 || list_out.ptr[1] != 1 || list_out.ptr[2] != 2 || list_out.ptr[3] != 3) {
          |    fprintf(stderr, "bad prependAnswer payload\n");
          |    return 19;
          |  }
          |  flix_free(list_out.ptr);
          |
          |  // Static array export.
          |  int32_t bump_in_buf[] = { 1, 2, 3 };
          |  flix_array_int32_t bump_in;
          |  bump_in.len = 3;
          |  bump_in.ptr = bump_in_buf;
          |  flix_array_int32_t bump_out;
          |  flix_exec_t bump_r = flix_export_Api_echoInts(ctx, &bump_in, &bump_out);
          |  if (bump_r.tag != FLIX_EXEC_OK || bump_out.len != 3) {
          |    fprintf(stderr, "bad echoInts metadata\n");
          |    return 20;
          |  }
          |  if (bump_out.ptr[0] != 1 || bump_out.ptr[1] != 2 || bump_out.ptr[2] != 3) {
          |    fprintf(stderr, "bad echoInts payload\n");
          |    return 21;
          |  }
          |  flix_free(bump_out.ptr);
          |
          |  const uint8_t world_bytes[] = { 'w', 'o', 'r', 'l', 'd' };
          |  flix_string_t world = flix_string_from_utf8(ctx, world_bytes, 5);
          |  flix_string_t names_in_buf[] = { hello, world };
          |  flix_array_string_t names_in;
          |  names_in.len = 2;
          |  names_in.ptr = names_in_buf;
          |  flix_array_string_t names_out;
          |  flix_exec_t names_r = flix_export_Api_echoNames(ctx, &names_in, &names_out);
          |  if (names_r.tag != FLIX_EXEC_OK || names_out.len != 2) {
          |    fprintf(stderr, "bad echoNames metadata\n");
          |    return 22;
          |  }
          |  int64_t name0_len = 0;
          |  int64_t name1_len = 0;
          |  uint8_t* name0 = flix_string_to_utf8(ctx, names_out.ptr[0], &name0_len);
          |  uint8_t* name1 = flix_string_to_utf8(ctx, names_out.ptr[1], &name1_len);
          |  if (name0_len != 5 || memcmp(name0, hello_bytes, 5) != 0 || name1_len != 5 || memcmp(name1, world_bytes, 5) != 0) {
          |    fprintf(stderr, "bad echoNames payload\n");
          |    return 23;
          |  }
          |  flix_free(name0);
          |  flix_free(name1);
          |  flix_handle_release(ctx, names_out.ptr[0]);
          |  flix_handle_release(ctx, names_out.ptr[1]);
          |  flix_free(names_out.ptr);
          |
          |  // Nested aggregate list export.
          |  flix_record_name_string_score_int32_t users_in_buf[2];
          |  users_in_buf[0].name = hello;
          |  users_in_buf[0].score = 41;
          |  users_in_buf[1].name = world;
          |  users_in_buf[1].score = 9;
          |  flix_list_record_name_string_score_int32_t users_in;
          |  users_in.len = 2;
          |  users_in.ptr = users_in_buf;
          |  flix_list_record_label_string_score_int32_t users_out;
          |  flix_exec_t users_r = flix_export_Api_promoteUsers(ctx, &users_in, &users_out);
          |  if (users_r.tag != FLIX_EXEC_OK || users_out.len != 2) {
          |    fprintf(stderr, "bad promoteUsers metadata\n");
          |    return 24;
          |  }
          |  if (users_out.ptr[0].score != 42 || users_out.ptr[1].score != 10) {
          |    fprintf(stderr, "bad promoteUsers scores\n");
          |    return 25;
          |  }
          |  int64_t label0_len = 0;
          |  int64_t label1_len = 0;
          |  uint8_t* label0 = flix_string_to_utf8(ctx, users_out.ptr[0].label, &label0_len);
          |  uint8_t* label1 = flix_string_to_utf8(ctx, users_out.ptr[1].label, &label1_len);
          |  if (label0_len != 5 || memcmp(label0, hello_bytes, 5) != 0 || label1_len != 5 || memcmp(label1, world_bytes, 5) != 0) {
          |    fprintf(stderr, "bad promoteUsers labels\n");
          |    return 26;
          |  }
          |  flix_free(label0);
          |  flix_free(label1);
          |  flix_handle_release(ctx, users_out.ptr[0].label);
          |  flix_handle_release(ctx, users_out.ptr[1].label);
          |  flix_free(users_out.ptr);
          |
          |  // Nested aggregate array export.
          |  flix_array_record_name_string_score_int32_t user_arr_in;
          |  user_arr_in.len = 2;
          |  user_arr_in.ptr = users_in_buf;
          |  flix_array_record_name_string_score_int32_t user_arr_out;
          |  flix_exec_t user_arr_r = flix_export_Api_echoUserArray(ctx, &user_arr_in, &user_arr_out);
          |  if (user_arr_r.tag != FLIX_EXEC_OK || user_arr_out.len != 2) {
          |    fprintf(stderr, "bad echoUserArray metadata\n");
          |    return 27;
          |  }
          |  if (user_arr_out.ptr[0].score != 41 || user_arr_out.ptr[1].score != 9) {
          |    fprintf(stderr, "bad echoUserArray scores\n");
          |    return 28;
          |  }
          |  int64_t arr_name0_len = 0;
          |  int64_t arr_name1_len = 0;
          |  uint8_t* arr_name0 = flix_string_to_utf8(ctx, user_arr_out.ptr[0].name, &arr_name0_len);
          |  uint8_t* arr_name1 = flix_string_to_utf8(ctx, user_arr_out.ptr[1].name, &arr_name1_len);
          |  if (arr_name0_len != 5 || memcmp(arr_name0, hello_bytes, 5) != 0 || arr_name1_len != 5 || memcmp(arr_name1, world_bytes, 5) != 0) {
          |    fprintf(stderr, "bad echoUserArray names\n");
          |    return 29;
          |  }
          |  flix_free(arr_name0);
          |  flix_free(arr_name1);
          |  flix_handle_release(ctx, user_arr_out.ptr[0].name);
          |  flix_handle_release(ctx, user_arr_out.ptr[1].name);
          |  flix_free(user_arr_out.ptr);
          |
          |  // Sequence-of-tuple export.
          |  flix_tuple2_int32_string_t pairs_in_buf[2];
          |  pairs_in_buf[0].f0 = 7;
          |  pairs_in_buf[0].f1 = hello;
          |  pairs_in_buf[1].f0 = 9;
          |  pairs_in_buf[1].f1 = world;
          |  flix_list_tuple2_int32_string_t pairs_in;
          |  pairs_in.len = 2;
          |  pairs_in.ptr = pairs_in_buf;
          |  flix_list_tuple2_string_int32_t pairs_out;
          |  flix_exec_t pairs_r = flix_export_Api_flipPairs(ctx, &pairs_in, &pairs_out);
          |  if (pairs_r.tag != FLIX_EXEC_OK || pairs_out.len != 2) {
          |    fprintf(stderr, "bad flipPairs metadata\n");
          |    return 30;
          |  }
          |  int64_t pair0_len = 0;
          |  int64_t pair1_len = 0;
          |  uint8_t* pair0 = flix_string_to_utf8(ctx, pairs_out.ptr[0].f0, &pair0_len);
          |  uint8_t* pair1 = flix_string_to_utf8(ctx, pairs_out.ptr[1].f0, &pair1_len);
          |  if (pair0_len != 5 || memcmp(pair0, hello_bytes, 5) != 0 || pairs_out.ptr[0].f1 != 7 ||
          |      pair1_len != 5 || memcmp(pair1, world_bytes, 5) != 0 || pairs_out.ptr[1].f1 != 9) {
          |    fprintf(stderr, "bad flipPairs payload\n");
          |    return 31;
          |  }
          |  flix_free(pair0);
          |  flix_free(pair1);
          |  flix_handle_release(ctx, pairs_out.ptr[0].f0);
          |  flix_handle_release(ctx, pairs_out.ptr[1].f0);
          |  flix_free(pairs_out.ptr);
          |
          |  // Sequence-of-option export.
          |  flix_option_int32_t maybe_in_buf[3];
          |  maybe_in_buf[0].is_some = true;
          |  maybe_in_buf[0].val = 41;
          |  maybe_in_buf[1].is_some = false;
          |  maybe_in_buf[1].val = 0;
          |  maybe_in_buf[2].is_some = true;
          |  maybe_in_buf[2].val = 9;
          |  flix_array_option_int32_t maybe_in;
          |  maybe_in.len = 3;
          |  maybe_in.ptr = maybe_in_buf;
          |  flix_array_option_int32_t maybe_out;
          |  flix_exec_t maybe_r = flix_export_Api_echoMaybeInts(ctx, &maybe_in, &maybe_out);
          |  if (maybe_r.tag != FLIX_EXEC_OK || maybe_out.len != 3) {
          |    fprintf(stderr, "bad echoMaybeInts metadata\n");
          |    return 32;
          |  }
          |  if (!maybe_out.ptr[0].is_some || maybe_out.ptr[0].val != 41 ||
          |      maybe_out.ptr[1].is_some ||
          |      !maybe_out.ptr[2].is_some || maybe_out.ptr[2].val != 9) {
          |    fprintf(stderr, "bad echoMaybeInts payload\n");
          |    return 33;
          |  }
          |  flix_free(maybe_out.ptr);
          |
          |  // Suspension + resume roundtrip export (host effect).
          |  flix_string_t ignored = 0;
          |  flix_exec_t susp_r = flix_export_Api_suspendEcho(ctx, hello, &ignored);
          |  if (susp_r.tag != FLIX_EXEC_SUSPENDED) {
          |    fprintf(stderr, "bad suspendEcho tag: %lld\n", (long long)susp_r.tag);
          |    return 34;
          |  }
          |  flix_handle_t susp = (flix_handle_t)susp_r.payload;
          |  flix_request_Api_suspendEcho_t req;
          |  flix_export_request_Api_suspendEcho(ctx, susp, &req);
          |  flix_string_t arg0 = req.arg0;
          |  int64_t arg0_len = 0;
          |  uint8_t* arg0_out = flix_string_to_utf8(ctx, arg0, &arg0_len);
          |  if (arg0_len != 5 || memcmp(arg0_out, hello_bytes, 5) != 0) {
          |    fprintf(stderr, "bad suspension arg0\n");
          |    return 35;
          |  }
          |  flix_free(arg0_out);
          |  flix_handle_release(ctx, arg0);
          |
          |  const uint8_t ok_bytes[] = { 'o', 'k' };
          |  flix_string_t ok = flix_string_from_utf8(ctx, ok_bytes, 2);
          |  flix_string_t resumed = 0;
          |  flix_exec_t resume_r = flix_export_resume_Api_suspendEcho(ctx, susp, ok, &resumed);
          |  if (resume_r.tag != FLIX_EXEC_OK) {
          |    fprintf(stderr, "bad suspendEcho resume tag: %lld\n", (long long)resume_r.tag);
          |    return 36;
          |  }
          |  int64_t resumed_len = 0;
          |  uint8_t* resumed_out = flix_string_to_utf8(ctx, resumed, &resumed_len);
          |  if (resumed_len != 2 || memcmp(resumed_out, ok_bytes, 2) != 0) {
          |    fprintf(stderr, "bad suspendEcho resume payload\n");
          |    return 37;
          |  }
          |  flix_free(resumed_out);
          |  flix_handle_release(ctx, ok);
          |  flix_handle_release(ctx, resumed);
          |  flix_handle_release(ctx, susp);
          |  flix_handle_release(ctx, world);
          |  flix_handle_release(ctx, hello);
          |
          |  flix_ctx_free(ctx);
          |  printf("OK\n");
          |  return 0;
          |}
          |""".stripMargin

      Files.writeString(cFile, cProgram, StandardCharsets.UTF_8)

      val compileCmd = zigCmd ::: List(
        "cc",
        "-I",
        sdkIncludeDir.toString,
        cFile.toString,
        lib.toString,
        "-o",
        exe.toString
      )
      val (ccExit, ccOutput) = exec(compileCmd, llvmDir)
      if (ccExit != 0) {
        fail(s"Failed to compile C export smoke test (exit $ccExit):\n${compileCmd.mkString(" ")}\n\n$ccOutput")
      }

      val (exit, output) = runExecutable(exe)
      if (exit != 0) {
        fail(s"Export smoke test failed with exit $exit:\n$output")
      }
      if (output.trim != "OK") {
        fail(s"Unexpected export smoke output:\n$output")
      }

      // Also verify that the shared library can be loaded dynamically and its symbols resolved.
      if (!isWindows) {
        val dynCFile = Files.createTempFile(outDir, "flix-llvm-export-dyn-", ".c")
        val dynExeName = if (isWindows) "export_dyn_smoke.exe" else "export_dyn_smoke"
        val dynExe = llvmDir.resolve(dynExeName)

        val dynProgram =
          """
            |#include "export-smoke.h"
            |#include <stdio.h>
            |#include <dlfcn.h>
            |#include <string.h>
            |
            |typedef void (*flix_init_fn)(int32_t argc, char** argv);
            |typedef flix_ctx_t* (*flix_ctx_new_fn)(void);
            |typedef void (*flix_ctx_free_fn)(flix_ctx_t* ctx);
            |typedef void (*flix_handle_release_fn)(flix_ctx_t* ctx, flix_handle_t h);
            |typedef flix_exec_t (*flix_add_fn)(flix_ctx_t* ctx, int32_t x, int32_t y, int32_t* out);
            |typedef flix_exec_t (*flix_echo_fn)(flix_ctx_t* ctx, flix_string_t s, flix_string_t* out);
            |typedef flix_exec_t (*flix_blen_fn)(flix_ctx_t* ctx, flix_bytes_t a, int32_t* out);
            |typedef flix_exec_t (*flix_bid_fn)(flix_ctx_t* ctx, flix_bytes_t a, flix_bytes_t* out);
            |typedef flix_exec_t (*flix_badge_fn)(flix_ctx_t* ctx, const flix_record_name_string_score_int32_t* a0, flix_record_label_string_score_int32_t* out);
            |typedef flix_exec_t (*flix_susp_echo_fn)(flix_ctx_t* ctx, flix_string_t s, flix_string_t* out);
            |typedef flix_exec_t (*flix_susp_echo_resume_fn)(flix_ctx_t* ctx, flix_handle_t susp, flix_string_t resume, flix_string_t* out);
            |typedef void (*flix_susp_echo_request_fn)(flix_ctx_t* ctx, flix_handle_t susp, flix_request_Api_suspendEcho_t* out);
            |typedef void (*flix_free_fn)(void* p);
            |typedef flix_string_t (*flix_string_from_utf8_fn)(flix_ctx_t* ctx, const uint8_t* bytes, int64_t len);
            |typedef uint8_t* (*flix_string_to_utf8_fn)(flix_ctx_t* ctx, flix_string_t str, int64_t* out_len);
            |typedef flix_i8_array_t (*flix_i8_array_from_bytes_fn)(flix_ctx_t* ctx, const uint8_t* bytes, int64_t len);
            |typedef uint8_t* (*flix_i8_array_to_bytes_fn)(flix_ctx_t* ctx, flix_i8_array_t arr, int64_t* out_len);
            |
            |int main(int argc, char** argv) {
            |  if (argc < 2) {
            |    fprintf(stderr, "missing shared library path\n");
            |    return 2;
            |  }
            |  void* lib = dlopen(argv[1], RTLD_NOW);
            |  if (!lib) {
            |    fprintf(stderr, "dlopen failed: %s\n", dlerror());
            |    return 3;
            |  }
            |
            |  flix_init_fn flix_init_ptr = (flix_init_fn)dlsym(lib, "flix_init");
            |  flix_ctx_new_fn ctx_new_ptr = (flix_ctx_new_fn)dlsym(lib, "flix_ctx_new");
            |  flix_ctx_free_fn ctx_free_ptr = (flix_ctx_free_fn)dlsym(lib, "flix_ctx_free");
            |  flix_handle_release_fn release_ptr = (flix_handle_release_fn)dlsym(lib, "flix_handle_release");
            |  flix_add_fn add_ptr = (flix_add_fn)dlsym(lib, "flix_export_Api_add");
            |  flix_echo_fn echo_ptr = (flix_echo_fn)dlsym(lib, "flix_export_Api_echo");
            |  flix_blen_fn blen_ptr = (flix_blen_fn)dlsym(lib, "flix_export_Api_bytesLen");
            |  flix_bid_fn bid_ptr = (flix_bid_fn)dlsym(lib, "flix_export_Api_bytesId");
            |  flix_badge_fn badge_ptr = (flix_badge_fn)dlsym(lib, "flix_export_Api_badge");
            |  flix_susp_echo_fn susp_echo_ptr = (flix_susp_echo_fn)dlsym(lib, "flix_export_Api_suspendEcho");
            |  flix_susp_echo_resume_fn susp_echo_resume_ptr = (flix_susp_echo_resume_fn)dlsym(lib, "flix_export_resume_Api_suspendEcho");
            |  flix_susp_echo_request_fn susp_echo_request_ptr = (flix_susp_echo_request_fn)dlsym(lib, "flix_export_request_Api_suspendEcho");
            |  flix_free_fn free_ptr = (flix_free_fn)dlsym(lib, "flix_free");
            |  flix_string_from_utf8_fn from_utf8_ptr = (flix_string_from_utf8_fn)dlsym(lib, "flix_string_from_utf8");
            |  flix_string_to_utf8_fn to_utf8_ptr = (flix_string_to_utf8_fn)dlsym(lib, "flix_string_to_utf8");
            |  flix_i8_array_from_bytes_fn arr_from_ptr = (flix_i8_array_from_bytes_fn)dlsym(lib, "flix_i8_array_from_bytes");
            |  flix_i8_array_to_bytes_fn arr_to_ptr = (flix_i8_array_to_bytes_fn)dlsym(lib, "flix_i8_array_to_bytes");
            |  if (!flix_init_ptr || !ctx_new_ptr || !ctx_free_ptr || !release_ptr ||
            |      !add_ptr || !echo_ptr || !blen_ptr || !bid_ptr || !badge_ptr || !susp_echo_ptr || !susp_echo_resume_ptr ||
            |      !susp_echo_request_ptr || !free_ptr || !from_utf8_ptr || !to_utf8_ptr || !arr_from_ptr || !arr_to_ptr) {
            |    fprintf(stderr, "dlsym failed\n");
            |    return 4;
            |  }
            |
            |  flix_init_ptr(0, NULL);
            |
            |  flix_ctx_t* ctx = ctx_new_ptr();
            |  if (!ctx) {
            |    fprintf(stderr, "flix_ctx_new returned NULL\n");
            |    return 5;
            |  }
            |
            |  int32_t sum = 0;
            |  flix_exec_t add_r = add_ptr(ctx, (int32_t)1, (int32_t)2, &sum);
            |  if (add_r.tag != FLIX_EXEC_OK || sum != 3) {
            |    fprintf(stderr, "bad add\n");
            |    return 6;
            |  }
            |
            |  const uint8_t hello_bytes[] = { 'h', 'e', 'l', 'l', 'o' };
            |  flix_string_t hello = from_utf8_ptr(ctx, hello_bytes, 5);
            |  flix_string_t echoed = 0;
            |  flix_exec_t echo_r = echo_ptr(ctx, hello, &echoed);
            |  if (echo_r.tag != FLIX_EXEC_OK) {
            |    fprintf(stderr, "bad echo\n");
            |    return 7;
            |  }
            |  int64_t echoed_len = 0;
            |  uint8_t* echoed_out = to_utf8_ptr(ctx, echoed, &echoed_len);
            |  if (echoed_len != 5 || memcmp(echoed_out, hello_bytes, 5) != 0) {
            |    fprintf(stderr, "bad echo payload\n");
            |    return 8;
            |  }
            |  free_ptr(echoed_out);
            |  release_ptr(ctx, echoed);
            |
            |  const uint8_t data[] = { 0, 1, 2, 255 };
            |  flix_bytes_t arr = arr_from_ptr(ctx, data, 4);
            |  int32_t arr_len = 0;
            |  flix_exec_t len_r = blen_ptr(ctx, arr, &arr_len);
            |  if (len_r.tag != FLIX_EXEC_OK || arr_len != 4) {
            |    fprintf(stderr, "bad bytesLen\n");
            |    return 9;
            |  }
            |  flix_bytes_t arr2 = 0;
            |  flix_exec_t id_r = bid_ptr(ctx, arr, &arr2);
            |  if (id_r.tag != FLIX_EXEC_OK) {
            |    fprintf(stderr, "bad bytesId\n");
            |    return 10;
            |  }
            |  int64_t out_len = 0;
            |  uint8_t* out = arr_to_ptr(ctx, arr2, &out_len);
            |  if (out_len != 4 || memcmp(out, data, 4) != 0) {
            |    fprintf(stderr, "bad bytesId payload\n");
            |    return 11;
            |  }
            |  free_ptr(out);
            |  release_ptr(ctx, arr);
            |  release_ptr(ctx, arr2);
            |
            |  flix_record_name_string_score_int32_t user_in;
            |  user_in.name = hello;
            |  user_in.score = 41;
            |  flix_record_label_string_score_int32_t badge_out;
            |  flix_exec_t badge_r = badge_ptr(ctx, &user_in, &badge_out);
            |  if (badge_r.tag != FLIX_EXEC_OK || badge_out.score != 42) {
            |    fprintf(stderr, "bad badge score\n");
            |    return 12;
            |  }
            |  int64_t badge_len = 0;
            |  uint8_t* badge_bytes = to_utf8_ptr(ctx, badge_out.label, &badge_len);
            |  if (badge_len != 5 || memcmp(badge_bytes, hello_bytes, 5) != 0) {
            |    fprintf(stderr, "bad badge label\n");
            |    return 13;
            |  }
            |  free_ptr(badge_bytes);
            |  release_ptr(ctx, badge_out.label);
            |
            |  flix_string_t ignored = 0;
            |  flix_exec_t susp_r = susp_echo_ptr(ctx, hello, &ignored);
            |  if (susp_r.tag != FLIX_EXEC_SUSPENDED) {
            |    fprintf(stderr, "bad suspendEcho\n");
            |    return 14;
            |  }
            |  flix_handle_t susp = (flix_handle_t)susp_r.payload;
            |  flix_request_Api_suspendEcho_t req;
            |  susp_echo_request_ptr(ctx, susp, &req);
            |  int64_t req_len = 0;
            |  uint8_t* req_out = to_utf8_ptr(ctx, req.arg0, &req_len);
            |  if (req_len != 5 || memcmp(req_out, hello_bytes, 5) != 0) {
            |    fprintf(stderr, "bad suspend request\n");
            |    return 15;
            |  }
            |  free_ptr(req_out);
            |  release_ptr(ctx, req.arg0);
            |  const uint8_t ok_bytes[] = { 'o', 'k' };
            |  flix_string_t ok = from_utf8_ptr(ctx, ok_bytes, 2);
            |  flix_string_t resumed = 0;
            |  flix_exec_t resume_r = susp_echo_resume_ptr(ctx, susp, ok, &resumed);
            |  if (resume_r.tag != FLIX_EXEC_OK) {
            |    fprintf(stderr, "bad suspend resume\n");
            |    return 16;
            |  }
            |  int64_t resumed_len = 0;
            |  uint8_t* resumed_out = to_utf8_ptr(ctx, resumed, &resumed_len);
            |  if (resumed_len != 2 || memcmp(resumed_out, ok_bytes, 2) != 0) {
            |    fprintf(stderr, "bad resume payload\n");
            |    return 17;
            |  }
            |  free_ptr(resumed_out);
            |  release_ptr(ctx, ok);
            |  release_ptr(ctx, resumed);
            |  release_ptr(ctx, susp);
            |  release_ptr(ctx, hello);
            |  ctx_free_ptr(ctx);
            |
            |  printf("OK\n");
            |  return 0;
            |}
            |""".stripMargin

        Files.writeString(dynCFile, dynProgram, StandardCharsets.UTF_8)

        val dynCompileCmd = zigCmd ::: List(
          "cc",
          "-I",
          sdkIncludeDir.toString,
          dynCFile.toString
        ) ::: (if (isMac) Nil else List("-ldl")) ::: List(
          "-o",
          dynExe.toString
        )

        val (dynCcExit, dynCcOutput) = exec(dynCompileCmd, llvmDir)
        if (dynCcExit != 0) {
          fail(s"Failed to compile dynamic-load export smoke test (exit $dynCcExit):\n${dynCompileCmd.mkString(" ")}\n\n$dynCcOutput")
        }

        val (dynExit, dynOutput) = runExecutable(dynExe, shared.toString)
        if (dynExit != 0) {
          fail(s"Dynamic-load export smoke test failed with exit $dynExit:\n$dynOutput")
        }
        if (dynOutput.trim != "OK") {
          fail(s"Unexpected dynamic-load export smoke output:\n$dynOutput")
        }
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  private def zigCmd: List[String] =
    ca.uwaterloo.flix.util.ZigToolchain.usableCommand.getOrElse(fail("usable zig command not found"))

  private def exec(cmd: List[String], cwd: Path): (Int, String) = {
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    pb.directory(cwd.toFile)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }

  private def runExecutable(executable: Path): (Int, String) = {
    val pb = new ProcessBuilder(List(executable.toString).asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }

  private def runExecutable(executable: Path, arg: String): (Int, String) = {
    val pb = new ProcessBuilder(List(executable.toString, arg).asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }

  private def isWindows: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("win")

  private def isMac: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("mac")

  private def deleteRecursive(root: Path): Unit = {
    if (!Files.exists(root)) return
    val stream = Files.walk(root)
    try {
      stream.iterator().asScala.toList.sortBy(_.getNameCount).reverse.foreach(p => Files.deleteIfExists(p))
    } finally {
      stream.close()
    }
  }

}
