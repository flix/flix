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

package ca.uwaterloo.flix.language.phase.llvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.AtomicOp
import ca.uwaterloo.flix.language.ast.LoweredAst.Expr
import ca.uwaterloo.flix.language.ast.SemanticOp.{BinaryOp, UnaryOp}
import ca.uwaterloo.flix.language.ast.shared.{Constant, ExpPosition}
import ca.uwaterloo.flix.language.ast.{ExnKindId, LoweredAst, Name, SemanticOp, SimpleType, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.language.phase.{ExportAbi, NativeImportAbi, WasmImportAbi, WasmImportInterface}
import ca.uwaterloo.flix.language.phase.llvm.LlvmIr.{Decl, Instr, Module as IrModule, Op, Terminator, Type, Value}
import ca.uwaterloo.flix.util.CompilationTarget

import scala.collection.mutable

/**
  * An experimental LLVM backend for Flix.
  *
  * It lowers [[LoweredAst]] to the LLVM/native/wasm runtime ABI used by the `llvm-native` and
  * `llvm-wasm` targets. The backend is still incomplete and not yet at JVM parity, but it is
  * usable for real programs.
  */
object LlvmBackend {

  /** A thin wrapper around the emitted LLVM IR module text. */
  case class Module(text: String)

  def run(root: LoweredAst.Root)(implicit flix: Flix): Module = flix.phase("LlvmBackend") {
    val ir = new Emitter(root, flix.options.target).emitModule()
    Module(LlvmPrinter.printModule(ir))
  }(DebugNoOp())

  private final class Emitter(root: LoweredAst.Root, target: CompilationTarget) {

    private val flixResultTypeName: String = "flix_result_t"
    private val flixResultType: Type = Type.Named(flixResultTypeName)

    private val flixObjTypeName: String = "flix_obj_t"
    private val flixObjType: Type = Type.Named(flixObjTypeName)

    private val flixTypeInfoTypeName: String = "flix_typeinfo_t"
    private val flixTypeInfoType: Type = Type.Named(flixTypeInfoTypeName)

    // flix_result_t tags (see docs/planning/native-backend/value-layout-v0.md).
    private val ResultTagValue: Long = 1L
    private val ResultTagThunk: Long = 2L
    private val ResultTagSuspension: Long = 3L
    private val ResultTagException: Long = 4L

    private val caseTagIds: Map[Symbol.CaseSym, Long] = computeCaseTagIds()
    private val cancelledKindId: Long = computeCancelledKindId()
    private val exnExnCaseSymOpt: Option[Symbol.CaseSym] = computeExnExnCaseSym()
    private val exnExnTagId: Long = exnExnCaseSymOpt.flatMap(caseTagIds.get).getOrElse(0L)
    private val portableListConsTagId: Long = computePortableListTagId("Cons")
    private val portableListNilTagId: Long = computePortableListTagId("Nil")
    private val exnExnTypeInfo: Value = exnExnCaseSymOpt match {
      case None => Value.Null(Type.Ptr)
      case Some(sym) => Value.Global(LlvmNames.tagTypeInfoName(sym), Type.Ptr)
    }
    private val effectSymIds: Map[Symbol.EffSym, Long] = LlvmEffectIds.effectSymIds(root)
    private val opIndices: Map[Symbol.OpSym, Int] = LlvmEffectIds.opIndices(root)
    private lazy val exportSuspensionSummaries: Map[Symbol.DefnSym, LlvmExportSuspensionAnalysis.Summary] =
      LlvmExportSuspensionAnalysis.compute(root)

    private var tmpId: Int = 0
    private var labelId: Int = 0

    private val extraFunctions = mutable.ArrayBuffer.empty[LlvmIr.Function]
    private val extraFunctionNames = mutable.Set.empty[String]

    private def needsResumableEvaluator(defn: LoweredAst.Def): Boolean =
      defn.pcPoints > 0 || ca.uwaterloo.flix.language.ast.Purity.isControlImpure(defn.exp.purity)

    private def addExtraFunction(f: LlvmIr.Function): Unit = {
      if (extraFunctionNames.add(f.name)) {
        extraFunctions.addOne(f)
      }
    }

    private def freshTmp(tpe: Type): Value.Local = {
      tmpId += 1
      Value.Local(s"t$tmpId", tpe)
    }

    private def freshLabel(prefix: String): String = {
      labelId += 1
      s"$prefix$labelId"
    }

    private case class ExnHandler(label: String, slotPtr: Value)

    private def hoistAllocaI64(fb: FunBuilder): Value.Local = {
      val slotPtr = freshTmp(Type.Ptr)
      fb.getBlock("entry") match {
        case Some(entry) =>
          entry.emitPrologueAssign(slotPtr, Op.Alloca(Type.I64))
          slotPtr
        case None =>
          throw new IllegalStateException("Missing LLVM entry block.")
      }
    }

    def emitModule(): IrModule = {
      val objBody = target match {
        case CompilationTarget.LlvmWasm => Type.Struct(List(Type.Ptr, Type.I32)) // pad header to 8 bytes for i64 payload alignment
        case _ => Type.Struct(List(Type.Ptr))
      }

      val typeDefs = List(
        LlvmIr.TypeDef(flixResultTypeName, Type.Struct(List(Type.I64, Type.I64))),
        LlvmIr.TypeDef(flixTypeInfoTypeName, Type.Struct(List(Type.I32, Type.I32, Type.I32, Type.Ptr, Type.Ptr, Type.Ptr, Type.Ptr, Type.Ptr))),
        LlvmIr.TypeDef(flixObjTypeName, objBody),
      )

      val mallocSizeTpe = if (target == CompilationTarget.LlvmWasm) Type.I32 else Type.I64

      val directImportDecls = target match {
        case CompilationTarget.LlvmNative =>
          root.defs.values.toList.flatMap { defn =>
            extractNativeImportBody(defn.exp).map { body =>
              val sig = defn.nativeImportSignature.getOrElse {
                throw new IllegalStateException(s"Missing native import signature for '${defn.sym}'.")
              }
              val callParams =
                (if (sig.requiresBridgeCtx) List(Type.Ptr) else Nil) ::: sig.params.map(nativeImportCallLlvmType)
              Decl.DeclareFun(nativeImportCallLlvmType(sig.result), body.spec.symbol, callParams)
            }
          }.distinct
        case CompilationTarget.LlvmWasm =>
          LlvmWasmImportsWriter.compute(root).flatMap { entry =>
            declareWasmImportFunction(entry)
          }.distinct
        case CompilationTarget.Jvm => Nil
      }

      val decls = (List(
        Decl.DeclareFun(Type.Void, "llvm.trap", Nil),
        Decl.DeclareFun(Type.Float, "llvm.pow.f32", List(Type.Float, Type.Float)),
        Decl.DeclareFun(Type.Double, "llvm.pow.f64", List(Type.Double, Type.Double)),
        Decl.DeclareFun(Type.Void, "flix_init", List(Type.I32, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_ctx_new", Nil),
        Decl.DeclareFun(Type.Void, "flix_ctx_free", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_float32_to_string", List(Type.Float)),
        Decl.DeclareFun(Type.Ptr, "flix_float64_to_string", List(Type.Double)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_from_i64", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_from_string", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_try_parse", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_to_string", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_neg", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_not", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_add", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_sub", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_mul", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_div", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_rem", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_shl", List(Type.Ptr, Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_shr", List(Type.Ptr, Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_and", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_or", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigint_xor", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_bigint_cmp", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_bigint_bit_length", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_bigint_hash", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_from_string", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_try_parse", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_to_string", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_to_plain_string", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_neg", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_add", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_sub", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_mul", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_div", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_bigdec_cmp", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_bigdec_hash", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_bigdec_scale", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_bigdec_precision", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_ceil", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_floor", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_round", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_bigdec_to_bigint", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_char_to_lower_case", List(Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_char_to_upper_case", List(Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_char_to_title_case", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_letter", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_digit", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_letter_or_digit", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_lower_case", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_upper_case", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_title_case", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_whitespace", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_defined", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_iso_control", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_mirrored", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_surrogate", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_char_is_surrogate_pair", List(Type.I32, Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_char_to_code_point", List(Type.I32, Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_char_get_numeric_value", List(Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_char_digit", List(Type.I32, Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_letter", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_digit", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_lower_case", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_upper_case", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_title_case", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_whitespace", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_alphabetic", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_defined", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_ideographic", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_iso_control", List(Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_codepoint_is_mirrored", List(Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_codepoint_to_lower_case", List(Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_codepoint_to_upper_case", List(Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_codepoint_to_title_case", List(Type.I32)),
        Decl.DeclareFun(Type.Ptr, "flix_codepoint_get_name", List(Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_codepoint_get_numeric_value", List(Type.I32)),
        Decl.DeclareFun(Type.Ptr, "flix_string_to_lower_case", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_string_to_upper_case", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_compile", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_compile_with_flags", List(Type.I32, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_try_compile", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_try_compile_with_flags", List(Type.I32, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_quote", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_pattern", List(Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_regex_flags", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_new_matcher", List(Type.Ptr, Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_regex_matcher_matches", List(Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_regex_matcher_find", List(Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_regex_matcher_find_from", List(Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.I1, "flix_regex_matcher_looking_at", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_matcher_replace_all", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_matcher_replace_first", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_regex_matcher_set_bounds", List(Type.Ptr, Type.Ptr, Type.Ptr, Type.I32, Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_regex_matcher_start", List(Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_regex_matcher_end", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_matcher_group", List(Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_regex_matcher_group_count", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_regex_split", List(Type.Ptr, Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_box_bool", List(Type.I1)),
        Decl.DeclareFun(Type.I64, "flix_box_char", List(Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_box_int8", List(Type.I8)),
        Decl.DeclareFun(Type.I64, "flix_box_int16", List(Type.I16)),
        Decl.DeclareFun(Type.I64, "flix_box_int32", List(Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_box_int64", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_box_float32", List(Type.Float)),
        Decl.DeclareFun(Type.I64, "flix_box_float64", List(Type.Double)),
        Decl.DeclareFun(Type.I64, "flix_unbox_bool", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_unbox_char", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_unbox_int8", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_unbox_int16", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_unbox_int32", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_unbox_int64", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_unbox_float32", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_unbox_float64", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_channel_new", List(Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_channel_put", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_channel_get", List(Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_channel_select", List(Type.Ptr, Type.I32, Type.I1)),
        Decl.DeclareFun(Type.I32, "flix_channel_select_index", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_channel_select_get", List(Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_channel_put_resumable", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_channel_get_resumable", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_channel_select_resumable", List(Type.Ptr, Type.Ptr, Type.I32, Type.I1)),
        Decl.DeclareFun(Type.Ptr, "flix_reentrant_lock_new", Nil),
        Decl.DeclareFun(Type.I64, "flix_reentrant_lock_lock", List(Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_reentrant_lock_try_lock", List(Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_reentrant_lock_unlock", List(Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_reentrant_lock_lock_resumable", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_condition_new", List(Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_condition_await", List(Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_condition_signal", List(Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_condition_signal_all", List(Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_condition_await_resumable", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_cyclic_barrier_new", List(Type.I32)),
        Decl.DeclareFun(Type.I32, "flix_cyclic_barrier_await", List(Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_cyclic_barrier_await_resumable", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_count_down_latch_new", List(Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_count_down_latch_await", List(Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_count_down_latch_count_down", List(Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_count_down_latch_await_resumable", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_semaphore_new", List(Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_semaphore_acquire", List(Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_semaphore_try_acquire", List(Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_semaphore_release", List(Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_semaphore_acquire_resumable", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_spawn", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_region_enter", List(Type.Ptr)),
        // Note: Passing `FlixResult` by-value is not ABI-stable across Zig/Clang on wasm, so we pass
        // tag/payload as separate i64s.
        Decl.DeclareFun(flixResultType, "flix_region_exit", List(Type.Ptr, Type.Ptr, Type.I64, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_region_malloc", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_region_alloc", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_region_remember_slot", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_store_ptr", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_print", List(Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_eprint", List(Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_println", List(Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_eprintln", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_readln", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_sleep_millis", List(Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_sleep_millis_resumable", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Void, "flix_exit", List(Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_new_id", List(Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_time_now_ms", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_file_exists", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_is_directory", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_is_regular_file", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_is_readable", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_is_symbolic_link", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_is_writable", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_is_executable", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_access_time", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_creation_time", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_modification_time", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_size", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_read", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_read_lines", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_read_bytes", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_list", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_read_resumable", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_read_lines_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_read_bytes_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_list_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_write", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_write_bytes", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_append", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_append_bytes", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_write_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_write_bytes_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_append_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_file_append_bytes_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_truncate", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_mkdir", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_mkdirs", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_file_mk_temp_dir", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_socket_read", List(Type.I64, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_tcp_socket_read_resumable", List(Type.Ptr, Type.I64, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_socket_write", List(Type.I64, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_tcp_socket_write_resumable", List(Type.Ptr, Type.I64, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_socket_connect", List(Type.Ptr, Type.I32)),
        Decl.DeclareFun(flixResultType, "flix_tcp_socket_connect_resumable", List(Type.Ptr, Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_socket_close", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_server_bind", List(Type.Ptr, Type.I32)),
        Decl.DeclareFun(flixResultType, "flix_tcp_server_accept_resumable", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_server_local_port", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_server_accept", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_tcp_server_close", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_process_stdin_write", List(Type.I64, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_process_stdin_write_resumable", List(Type.Ptr, Type.I64, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_process_exec", List(Type.Ptr, Type.I1, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_process_exit_value", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_process_is_alive", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_process_pid", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_process_stop", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_process_wait_for", List(Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_process_wait_for_resumable", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_process_wait_for_timeout", List(Type.I64, Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_process_wait_for_timeout_resumable", List(Type.Ptr, Type.I64, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_process_stdout_read", List(Type.I64, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_process_stdout_read_resumable", List(Type.Ptr, Type.I64, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_process_stderr_read", List(Type.I64, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_process_stderr_read_resumable", List(Type.Ptr, Type.I64, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_process_release", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_http_request", List(Type.Ptr, Type.Ptr, Type.Ptr, Type.Ptr, Type.I1, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_http_request_resumable", List(Type.Ptr, Type.Ptr, Type.Ptr, Type.Ptr, Type.I1, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_env_get_args", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_env_get_env_pairs", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_env_get_var", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_env_get_prop", List(Type.Ptr)),
        Decl.DeclareFun(Type.I32, "flix_env_virtual_processors", List(Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_frames_push", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_frames_reverse_onto", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_frame_copy", List(Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_trace_push", List(Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_trace_pop", Nil),
        Decl.DeclareFun(Type.Ptr, "flix_exn_with_trace", List(Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_handle_new", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_handle_get", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_handle_payload", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_handle_new_i64", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Void, "flix_handle_release", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_handle_unbox_i64", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_suspension_eff_sym_id", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_suspension_op_index", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_suspension_arg_count", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_suspension_arg_payload", List(Type.Ptr, Type.I64, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_export_tuple_new", List(Type.Ptr, Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_export_tuple_field", List(Type.Ptr, Type.I64, Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_export_tag_new", List(Type.Ptr, Type.I64, Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_export_tag_id", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_export_tag_field_i64", List(Type.Ptr, Type.I64, Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_export_tag_field_ptr", List(Type.Ptr, Type.I64, Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_export_list_length", List(Type.Ptr, Type.I64, Type.I64, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_export_array_new", List(Type.Ptr, Type.I32, Type.Ptr, Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_export_array_length", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.I64, "flix_export_array_element", List(Type.Ptr, Type.I64, Type.I32)),
        Decl.DeclareFun(Type.I64, "flix_string_from_utf8", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_string_to_utf8", List(Type.Ptr, Type.I64, Type.Ptr)),
        Decl.DeclareFun(Type.I64, "flix_i8_array_from_bytes", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_i8_array_to_bytes", List(Type.Ptr, Type.I64, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_exn_report_ptr", List(Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_suspension_report_ptr", List(Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_native_drive_result", List(Type.Ptr, Type.I64, Type.I64)),
        Decl.DeclareFun(Type.Void, "flix_gc_push_root_value_i64", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_gc_push_root_ptr", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_gc_pop_roots", List(Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Void, "flix_gc_pollcheck", List(Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_trace_ptr_array", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_trace_bigdecimal", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_trace_handler", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Void, "flix_trace_suspension", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.I1, "flix_cancel_requested", List(Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_cancel_exn", List(Type.Ptr, Type.I64, Type.Ptr, Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_install_handler", List(Type.Ptr, Type.I64, Type.Ptr, Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(flixResultType, "flix_resumption_rewind", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_resume_suspension", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_alloc", List(Type.Ptr, Type.Ptr)),
        Decl.DeclareFun(Type.Ptr, "flix_alloc_flex", List(Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "flix_region_alloc_flex", List(Type.Ptr, Type.Ptr, Type.Ptr, Type.I64)),
        Decl.DeclareFun(flixResultType, "flix_invoke_thunk", List(Type.Ptr, Type.Ptr, Type.I64, Type.I64)),
        Decl.DeclareFun(Type.Ptr, "malloc", List(mallocSizeTpe)),
        Decl.DeclareFun(Type.Void, "free", List(Type.Ptr))
      ) ++ directImportDecls).distinct

      // Pre-emit resumption invoke wrappers used to build continuation closures inside handler wrappers.
      //
      // This must happen before we snapshot `extraFunctions` into the final `functions` list.
      val resumptionInvokeArgTypes: Set[SimpleType] =
        root.effects.values.toList.flatMap(_.ops).map(_.tpe).toSet
      resumptionInvokeArgTypes.toList.sortBy(_.toString).foreach(getOrEmitResumptionInvokeWrapper)

      val defFunctions = root.defs.values.toList.map(emitDef)
      val closureWrappers = collectClosureSyms().toList.sortBy(_.toString).map(sym => emitClosureInvokeWrapper(root.defs(sym)))
      val thunkInvokeWrappers = collectThunkInvokeSyms().toList.sortBy(_.toString).map(sym => emitThunkInvokeWrapper(root.defs(sym)))
      val thunkApplyClosureWrappers = collectThunkApplyClosureArgTypes().toList.sortBy(_.toString).map(emitThunkApplyClosureWrapper)

      // Helpers for reporting unhandled suspensions (effect/op names).
      addExtraFunction(emitEffectNameLookup())
      addExtraFunction(emitOpNameLookup())

      // Exported function wrappers (C ABI).
      root.defs.values.toList.filter(_.ann.isExport).sortBy(_.sym.toString).foreach { defn =>
        addExtraFunction(emitExportWrapper(defn))
        val resumeType = LlvmExportSuspensionAnalysis.typedResumeType(defn.sym, exportSuspensionSummaries, root)
        addExtraFunction(emitExportResumeWrapper(defn, resumeType))
        LlvmExportSuspensionAnalysis.typedRequestOp(defn.sym, exportSuspensionSummaries, root)
          .zip(LlvmExportSuspensionAnalysis.typedRequestSignature(defn.sym, exportSuspensionSummaries, root))
          .foreach { case (op, sig) => addExtraFunction(emitExportRequestWrapper(defn, op, sig)) }
      }

      // Wasm-only: def-id based invocation dispatch used by the component runtime.
      if (target == CompilationTarget.LlvmWasm) {
        val wasmDefs = LlvmWasmDefs.compute(root)
        addExtraFunction(emitWasmInvokeDef(wasmDefs))
        addExtraFunction(emitWasmResumeOkDef(wasmDefs))
        addExtraFunction(emitWasmResumeThrowDef())
      }

      val extra = extraFunctions.toList.sortBy(_.name)
      val functions = (target, root.mainEntryPoint) match {
        case (CompilationTarget.LlvmNative, Some(mainSym)) =>
          defFunctions ::: extra ::: closureWrappers ::: thunkInvokeWrappers ::: thunkApplyClosureWrappers ::: List(emitNativeMainWrapper(mainSym))
        case _ =>
          defFunctions ::: extra ::: closureWrappers ::: thunkInvokeWrappers ::: thunkApplyClosureWrappers
      }

      val defTraceGlobals =
        root.defs.values.toList.sortBy(_.sym.toString).map { defn =>
          val bytes = defn.sym.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8) ++ Array(0.toByte)
          LlvmIr.GlobalDef.CString(LlvmNames.traceName(defn.sym), bytes)
        }

      val effectNameGlobals =
        root.effects.keys.toList.sortBy(_.toString).map { effSym =>
          val bytes = effSym.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8) ++ Array(0.toByte)
          LlvmIr.GlobalDef.CString(LlvmNames.effectName(effSym), bytes)
        }

      val opNameGlobals =
        root.effects.values.toList.flatMap(_.ops).toList.sortBy(_.sym.toString).map { op =>
          val bytes = op.sym.name.getBytes(java.nio.charset.StandardCharsets.UTF_8) ++ Array(0.toByte)
          LlvmIr.GlobalDef.CString(LlvmNames.opName(op.sym), bytes)
        }

      // Typeinfo globals for heap object shapes the current backend emits (thunks and frames).
      //
      // Pointer maps are deferred until we have a stable story for GC pointers vs non-GC pointers
      // (e.g. region pointers share `ptr` at the LLVM level). For now we set ptr_count=0 and ptr_offs=null.
      val headerBytes = 8L // by construction of `%flix_obj_t` above for both native and wasm

      case class TypeInfoSpec(name: String,
                              ptrOffsName: String,
                              sizeBytes: Long,
                              ptrOffs: List[Int],
                              trace: Option[String] = None,
                              invoke: Option[String],
                              apply: Option[String],
                              copy: Option[String])

      val arraySpecs = List(
        TypeInfoSpec(
          name = LlvmNames.arrayPrimTypeInfoName,
          ptrOffsName = LlvmNames.arrayPrimPtrOffsName,
          sizeBytes = 0L,
          ptrOffs = Nil,
          trace = None,
          invoke = None,
          apply = None,
          copy = None
        ),
        TypeInfoSpec(
          name = LlvmNames.arrayPtrTypeInfoName,
          ptrOffsName = LlvmNames.arrayPtrPtrOffsName,
          sizeBytes = 0L,
          ptrOffs = Nil,
          trace = Some("flix_trace_ptr_array"),
          invoke = None,
          apply = None,
          copy = None
        )
      )

      val stringSpecs = List(
        TypeInfoSpec(
          name = LlvmNames.stringTypeInfoName,
          ptrOffsName = s"${LlvmNames.stringTypeInfoName}_ptr_offs",
          sizeBytes = 0L,
          ptrOffs = Nil,
          trace = None,
          invoke = None,
          apply = None,
          copy = None
        )
      )

      val bigIntSpecs = List(
        TypeInfoSpec(
          name = LlvmNames.bigIntTypeInfoName,
          ptrOffsName = s"${LlvmNames.bigIntTypeInfoName}_ptr_offs",
          sizeBytes = 0L,
          ptrOffs = Nil,
          trace = None,
          invoke = None,
          apply = None,
          copy = None
        )
      )

      val bigDecimalSpecs = List(
        TypeInfoSpec(
          name = LlvmNames.bigDecimalTypeInfoName,
          ptrOffsName = s"${LlvmNames.bigDecimalTypeInfoName}_ptr_offs",
          sizeBytes = 0L,
          ptrOffs = Nil,
          trace = Some("flix_trace_bigdecimal"),
          invoke = None,
          apply = None,
          copy = None
        )
      )

      // Runtime-internal effect objects (Handler + Suspension) are flexible heap objects.
      // We provide custom trace hooks and no fixed pointer maps.
      val effectInternalSpecs = List(
        TypeInfoSpec(
          name = LlvmNames.handlerTypeInfoName,
          ptrOffsName = s"${LlvmNames.handlerTypeInfoName}_ptr_offs",
          sizeBytes = 0L,
          ptrOffs = Nil,
          trace = Some("flix_trace_handler"),
          invoke = None,
          apply = None,
          copy = None
        ),
        TypeInfoSpec(
          name = LlvmNames.suspensionTypeInfoName,
          ptrOffsName = s"${LlvmNames.suspensionTypeInfoName}_ptr_offs",
          sizeBytes = 0L,
          ptrOffs = Nil,
          trace = Some("flix_trace_suspension"),
          invoke = None,
          apply = None,
          copy = None
        )
      )

      val closureSpecs = collectClosureSyms().toList.map { sym =>
        val defn = root.defs(sym)
        val captured = defn.cparams.map(_.tpe)
        val ptrOffs = captured.zipWithIndex.collect {
          case (tpe, i) if isGcRootType(tpe) =>
            (headerBytes + i.toLong * 8L).toInt
        }
        val sizeBytes = headerBytes + captured.length.toLong * 8L
        TypeInfoSpec(
          name = LlvmNames.closureTypeInfoName(sym),
          ptrOffsName = LlvmNames.closurePtrOffsName(sym),
          sizeBytes = sizeBytes,
          ptrOffs = ptrOffs,
          invoke = Some(LlvmNames.closureInvokeName(sym)),
          apply = None,
          copy = None
        )
      }

      val thunkSpecs = collectThunkInvokeSyms().toList.map { sym =>
        val defn = root.defs(sym)
        val captured = (defn.cparams ::: defn.fparams).map(_.tpe)
        val ptrOffs = captured.zipWithIndex.collect {
          case (tpe, i) if isGcRootType(tpe) =>
            (headerBytes + i.toLong * 8L).toInt
        }
        val sizeBytes = headerBytes + captured.length.toLong * 8L
        TypeInfoSpec(
          name = LlvmNames.thunkTypeInfoName(sym),
          ptrOffsName = LlvmNames.thunkPtrOffsName(sym),
          sizeBytes = sizeBytes,
          ptrOffs = ptrOffs,
          invoke = Some(LlvmNames.thunkInvokeName(sym)),
          apply = None,
          copy = None
        )
      }

      // Continuation closures created by effect handler wrappers.
      //
      // Layout:
      //   payload[0] = resumption pointer bits (always a GC heap pointer)
      val kSpecs = resumptionInvokeArgTypes.toList.map { argTpe =>
        val ptrOffs = List((headerBytes + 0L * 8L).toInt)
        val sizeBytes = headerBytes + 1L * 8L
        TypeInfoSpec(
          name = LlvmNames.kTypeInfoName(argTpe),
          ptrOffsName = LlvmNames.kPtrOffsName(argTpe),
          sizeBytes = sizeBytes,
          ptrOffs = ptrOffs,
          invoke = Some(getOrEmitResumptionInvokeWrapper(argTpe)),
          apply = None,
          copy = None
        )
      }.groupBy(_.name).values.map(_.head).toList

      val thunkApplyCloSpecs = collectThunkApplyClosureArgTypes().toList.map { argTpe =>
        // payload[0] = closure pointer bits (always GC heap)
        // payload[1] = argument payload bits (GC heap iff argTpe is a GC root type)
        val ptrOffsBase = List((headerBytes + 0L * 8L).toInt)
        val ptrOffsArg = if (isGcRootType(argTpe)) List((headerBytes + 1L * 8L).toInt) else Nil
        val ptrOffs = ptrOffsBase ::: ptrOffsArg
        val sizeBytes = headerBytes + 2L * 8L
        TypeInfoSpec(
          name = LlvmNames.thunkApplyClosureTypeInfoName(argTpe),
          ptrOffsName = LlvmNames.thunkApplyClosurePtrOffsName(argTpe),
          sizeBytes = sizeBytes,
          ptrOffs = ptrOffs,
          invoke = Some(LlvmNames.thunkApplyClosureName(argTpe)),
          apply = None,
          copy = None
        )
      }

      val lazySpecs = collectLazyInnerTypes().toList.map { innerTpe =>
        // payload[0] = exp thunk pointer bits (always GC heap)
        // payload[1] = cached value payload bits (GC heap iff innerTpe is a GC root type)
        val ptrOffsBase = List((headerBytes + 0L * 8L).toInt)
        val ptrOffsVal = if (isGcRootType(innerTpe)) List((headerBytes + 1L * 8L).toInt) else Nil
        val ptrOffs = ptrOffsBase ::: ptrOffsVal
        val sizeBytes = headerBytes + 2L * 8L
        TypeInfoSpec(
          name = LlvmNames.lazyTypeInfoName(innerTpe),
          ptrOffsName = LlvmNames.lazyPtrOffsName(innerTpe),
          sizeBytes = sizeBytes,
          ptrOffs = ptrOffs,
          invoke = None,
          apply = None,
          copy = None
        )
      }

	        val tupleSpecs = collectTupleTypes().toList.map { tupTpe =>
	          val ptrOffs = tupTpe.tpes.zipWithIndex.collect {
	            case (tpe, i) if isGcRootType(tpe) =>
	              (headerBytes + i.toLong * 8L).toInt
	        }
	        val sizeBytes = headerBytes + tupTpe.tpes.length.toLong * 8L
	        TypeInfoSpec(
	          name = LlvmNames.tupleTypeInfoName(tupTpe),
	          ptrOffsName = LlvmNames.tuplePtrOffsName(tupTpe),
	          sizeBytes = sizeBytes,
	          ptrOffs = ptrOffs,
	          invoke = None,
	          apply = None,
	          copy = None
	        )
	      }

        val tagSpecs = root.enums.values.toList.flatMap { enm =>
          enm.cases.values.toList.map { caze =>
            // Layout: payload[0] = tag word (tag_id + reserved); payload[1..] = fields.
            val ptrOffs = caze.tpes.zipWithIndex.collect {
              case (tpe, i) if isGcRootType(tpe) =>
                (headerBytes + (1L + i.toLong) * 8L).toInt
            }
            val sizeBytes = headerBytes + (1L + caze.tpes.length.toLong) * 8L
            TypeInfoSpec(
              name = LlvmNames.tagTypeInfoName(caze.sym),
              ptrOffsName = LlvmNames.tagPtrOffsName(caze.sym),
              sizeBytes = sizeBytes,
              ptrOffs = ptrOffs,
              invoke = None,
              apply = None,
              copy = None
            )
          }
        }

      val structSpecs = root.structs.values.toList.map { st =>
        val ptrOffs = st.fields.zipWithIndex.collect {
          case (fld, i) if isGcRootType(fld.tpe) =>
            (headerBytes + i.toLong * 8L).toInt
        }
        val sizeBytes = headerBytes + st.fields.length.toLong * 8L
        TypeInfoSpec(
          name = LlvmNames.structTypeInfoName(st.sym),
          ptrOffsName = LlvmNames.structPtrOffsName(st.sym),
          sizeBytes = sizeBytes,
          ptrOffs = ptrOffs,
          invoke = None,
          apply = None,
          copy = None
        )
      }

      val recordSpecs = collectRecordTypes().toList.map { recTpe =>
        val fields = recordFields(recTpe)
        val ptrOffs = fields.zipWithIndex.collect {
          case ((_, tpe), i) if isGcRootType(tpe) =>
            (headerBytes + i.toLong * 8L).toInt
        }
        val sizeBytes = headerBytes + fields.length.toLong * 8L
        TypeInfoSpec(
          name = LlvmNames.recordTypeInfoName(recTpe),
          ptrOffsName = LlvmNames.recordPtrOffsName(recTpe),
          sizeBytes = sizeBytes,
          ptrOffs = ptrOffs,
          invoke = None,
          apply = None,
          copy = None
        )
      }

      val frameSpecs = root.defs.values.toList
        // Any def that lowering marked with pc-points, or that is otherwise control-impure, must
        // use the frame-based evaluator so suspend/resume and handler state stay aligned.
        .filter(needsResumableEvaluator)
        .map { defn =>
          val vars = (defn.cparams ::: defn.fparams).map(_.tpe) ::: defn.lparams.map(_.tpe)
          val ptrOffs = vars.zipWithIndex.collect {
            case (tpe, i) if isGcRootType(tpe) =>
              // payload[0] is pc; vars begin at payload[1]
              (headerBytes + (1L + i.toLong) * 8L).toInt
          }
          val sizeBytes = headerBytes + (1L + vars.length.toLong) * 8L
          TypeInfoSpec(
            name = LlvmNames.frameTypeInfoName(defn.sym),
            ptrOffsName = LlvmNames.framePtrOffsName(defn.sym),
            sizeBytes = sizeBytes,
            ptrOffs = ptrOffs,
            invoke = Some(LlvmNames.frameApplyName(defn.sym)),
            apply = None,
            copy = None
          )
        }

	      val typeInfoSpecs = (arraySpecs ::: stringSpecs ::: bigIntSpecs ::: bigDecimalSpecs ::: effectInternalSpecs ::: closureSpecs ::: thunkSpecs ::: kSpecs ::: thunkApplyCloSpecs ::: lazySpecs ::: tupleSpecs ::: tagSpecs ::: structSpecs ::: recordSpecs ::: frameSpecs).sortBy(_.name)

      val ptrOffsGlobals = typeInfoSpecs.collect {
        case spec if spec.ptrOffs.nonEmpty =>
          val arrTpe = Type.Array(spec.ptrOffs.length, Type.I32)
          val init = Value.ArrayConst(spec.ptrOffs.map(o => Value.IntConst(o.toLong, Type.I32)), arrTpe)
          LlvmIr.GlobalDef.Constant(spec.ptrOffsName, arrTpe, init, align = 4)
      }

      val typeInfoGlobals = typeInfoSpecs.zipWithIndex.map {
        case (spec, idx) =>
          val typeId = idx + 1
          val sizeI32 =
            if (spec.sizeBytes < 0 || spec.sizeBytes > Int.MaxValue) throw new IllegalStateException(s"Invalid typeinfo size: ${spec.sizeBytes}")
            else spec.sizeBytes.toInt

          val ptrCount = spec.ptrOffs.length
          val ptrOffsPtr = if (ptrCount == 0) Value.Null(Type.Ptr) else Value.Global(spec.ptrOffsName, Type.Ptr)

          val traceV = spec.trace match {
            case None => Value.Null(Type.Ptr)
            case Some(n) => Value.Global(n, Type.Ptr)
          }

          val invokeV = spec.invoke match {
            case None => Value.Null(Type.Ptr)
            case Some(n) => Value.Global(n, Type.Ptr)
          }
          val applyV = spec.apply match {
            case None => Value.Null(Type.Ptr)
            case Some(n) => Value.Global(n, Type.Ptr)
          }
          val copyV = spec.copy match {
            case None => Value.Null(Type.Ptr)
            case Some(n) => Value.Global(n, Type.Ptr)
          }

          val init = Value.StructConst(List(
            Value.IntConst(typeId.toLong, Type.I32),
            Value.IntConst(sizeI32.toLong, Type.I32),
            Value.IntConst(ptrCount.toLong, Type.I32),
            ptrOffsPtr,
            traceV,
            invokeV,
            applyV,
            copyV,
          ), flixTypeInfoType)

          val linkage =
            if (spec.name == LlvmNames.arrayPrimTypeInfoName ||
              spec.name == LlvmNames.arrayPtrTypeInfoName ||
              spec.name == LlvmNames.stringTypeInfoName ||
              spec.name == LlvmNames.bigIntTypeInfoName ||
              spec.name == LlvmNames.bigDecimalTypeInfoName ||
              spec.name == LlvmNames.handlerTypeInfoName ||
              spec.name == LlvmNames.suspensionTypeInfoName)
              LlvmIr.GlobalDef.Linkage.External
            else
              LlvmIr.GlobalDef.Linkage.Private

          LlvmIr.GlobalDef.Constant(spec.name, flixTypeInfoType, init, align = 8, linkage = linkage)
      }

      val globals = defTraceGlobals ::: effectNameGlobals ::: opNameGlobals ::: ptrOffsGlobals ::: typeInfoGlobals

      IrModule(sourceFilename = "flix", typeDefs = typeDefs, decls = decls, globals = globals, functions = functions)
    }

    private def computeCaseTagIds(): Map[Symbol.CaseSym, Long] = {
      root.enums.values.flatMap { enm =>
        val sortedCases = enm.cases.keys.toList.sortBy(_.name)
        sortedCases.zipWithIndex.map {
          case (sym, idx) => sym -> idx.toLong
        }
      }.toMap
    }

    private def computePortableListTagId(caseName: String): Long = {
      val caseSymOpt = root.enums.values
        .find(enm => enm.sym.text == "List" && enm.sym.namespace.isEmpty)
        .flatMap(enm => enm.cases.keys.find(_.name == caseName))
      caseSymOpt.flatMap(caseTagIds.get).getOrElse {
        throw new IllegalStateException(s"Missing portable List.$caseName case tag id.")
      }
    }

    private def portableEnumCaseSym(enumName: String, caseName: String): Symbol.CaseSym =
      root.enums.values
        .find(enm => enm.sym.text == enumName && enm.sym.namespace.isEmpty)
        .flatMap(enm => enm.cases.keys.find(_.name == caseName))
        .getOrElse {
          throw new IllegalStateException(s"Missing portable $enumName.$caseName case symbol.")
        }

    private def portableEnumSym(enumName: String): Symbol.EnumSym =
      root.enums.values
        .find(enm => enm.sym.text == enumName && enm.sym.namespace.isEmpty)
        .map(_.sym)
        .getOrElse {
          throw new IllegalStateException(s"Missing portable $enumName enum symbol.")
        }

    private def portableImportLoweredType(abiTpe: ExportAbi.AbiType): SimpleType = abiTpe match {
      case ExportAbi.AbiType.Unit => SimpleType.Unit
      case ExportAbi.AbiType.Bool => SimpleType.Bool
      case ExportAbi.AbiType.Int8 => SimpleType.Int8
      case ExportAbi.AbiType.Int16 => SimpleType.Int16
      case ExportAbi.AbiType.Int32 => SimpleType.Int32
      case ExportAbi.AbiType.Int64 => SimpleType.Int64
      case ExportAbi.AbiType.Float32 => SimpleType.Float32
      case ExportAbi.AbiType.Float64 => SimpleType.Float64
      case ExportAbi.AbiType.String => SimpleType.String
      case ExportAbi.AbiType.Bytes => SimpleType.mkArray(SimpleType.Int8)
      case ExportAbi.AbiType.List(_) => SimpleType.mkEnum(portableEnumSym("List"), Nil)
      case ExportAbi.AbiType.Array(elmTpe) => SimpleType.mkArray(portableImportLoweredType(elmTpe))
      case ExportAbi.AbiType.Tuple(elmTpes) => SimpleType.mkTuple(elmTpes.map(portableImportLoweredType))
      case ExportAbi.AbiType.Option(_) => SimpleType.mkEnum(portableEnumSym("Option"), Nil)
      case ExportAbi.AbiType.Result(_, _) => SimpleType.mkEnum(portableEnumSym("Result"), Nil)
      case ExportAbi.AbiType.Record(fields) =>
        fields.reverse.foldLeft[SimpleType](SimpleType.RecordEmpty) {
          case (rest, (label, fieldTpe)) => SimpleType.RecordExtend(label, portableImportLoweredType(fieldTpe), rest)
        }
    }

    private def computeCancelledKindId(): Long = {
      // Cancellation is represented as `Exn` with payload type `Cancelled` (stdlib).
      // We compute a stable kind id from the payload type.
      val cancelledSymOpt = root.structs.keys.find(sym => sym.text == "CancelledPayload" && sym.namespace.isEmpty)
      cancelledSymOpt match {
        case None => 0L
        case Some(sym) => ExnKindId.of(SimpleType.Struct(sym, List(SimpleType.Region))).toLong
      }
    }

    private def computeExnExnCaseSym(): Option[Symbol.CaseSym] = {
      root.enums.values
        .find(enm => enm.sym.text == "Exn" && enm.sym.namespace.isEmpty)
        .flatMap(enm => enm.cases.keys.find(_.name == "Exn"))
    }

    private def emitEffectNameLookup(): LlvmIr.Function = {
      val params = List(LlvmIr.Param("effSymId", Type.I64))
      val effSymId = Value.Local("effSymId", Type.I64)

      val sortedEffects = effectSymIds.toList.sortBy(_._2)
      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      if (sortedEffects.isEmpty) {
        fb.current.setTerminator(Terminator.Ret(Type.Ptr, Value.Null(Type.Ptr)))
        return LlvmIr.Function("flix_effect_name", Type.Ptr, params, fb.result())
      }

      var currentCheck = entry
      var i = 0
      while (i < sortedEffects.length) {
        val (effSym, id) = sortedEffects(i)
        val matchLabel = s"match_$id"
        val nextLabel = if (i == sortedEffects.length - 1) "default" else s"check_${sortedEffects(i + 1)._2}"
        val cmp = Value.Local(s"cmp_$id", Type.I1)

        currentCheck.emitAssign(cmp, Op.ICmp("eq", effSymId, Value.IntConst(id, Type.I64)))
        currentCheck.setTerminator(Terminator.CondBr(cmp, matchLabel, nextLabel))

        val matchBlock = fb.newBlock(matchLabel)
        fb.setCurrent(matchBlock)
        matchBlock.setTerminator(Terminator.Ret(Type.Ptr, Value.Global(LlvmNames.effectName(effSym), Type.Ptr)))

        if (i < sortedEffects.length - 1) {
          val nextCheck = fb.newBlock(nextLabel)
          currentCheck = nextCheck
          fb.setCurrent(nextCheck)
        }

        i += 1
      }

      val defaultBlock = fb.newBlock("default")
      fb.setCurrent(defaultBlock)
      defaultBlock.setTerminator(Terminator.Ret(Type.Ptr, Value.Null(Type.Ptr)))

      LlvmIr.Function("flix_effect_name", Type.Ptr, params, fb.result())
    }

    private def emitOpNameLookup(): LlvmIr.Function = {
      val params = List(LlvmIr.Param("effSymId", Type.I64), LlvmIr.Param("opIndex", Type.I64))
      val effSymId = Value.Local("effSymId", Type.I64)
      val opIndex = Value.Local("opIndex", Type.I64)

      val sortedEffects = effectSymIds.toList.sortBy(_._2)
      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      if (sortedEffects.isEmpty) {
        fb.current.setTerminator(Terminator.Ret(Type.Ptr, Value.Null(Type.Ptr)))
        return LlvmIr.Function("flix_op_name", Type.Ptr, params, fb.result())
      }

      def emitOpDispatch(effSym: Symbol.EffSym, effId: Long, dispatchLabel: String): Unit = {
        val eff = root.effects(effSym)
        val ops = eff.ops

        val dispatchEntry = fb.newBlock(dispatchLabel)
        fb.setCurrent(dispatchEntry)

        if (ops.isEmpty) {
          dispatchEntry.setTerminator(Terminator.Ret(Type.Ptr, Value.Null(Type.Ptr)))
          return
        }

        var currentCheck = dispatchEntry
        var j = 0
        while (j < ops.length) {
          val op = ops(j)
          val opIdx = j.toLong
          val retLabel = s"eff_${effId}_ret_op_$opIdx"
          val nextLabel = if (j == ops.length - 1) "default" else s"eff_${effId}_check_op_${opIdx + 1}"
          val cmp = Value.Local(s"cmp_eff_${effId}_op_$opIdx", Type.I1)

          currentCheck.emitAssign(cmp, Op.ICmp("eq", opIndex, Value.IntConst(opIdx, Type.I64)))
          currentCheck.setTerminator(Terminator.CondBr(cmp, retLabel, nextLabel))

          val retBlock = fb.newBlock(retLabel)
          fb.setCurrent(retBlock)
          retBlock.setTerminator(Terminator.Ret(Type.Ptr, Value.Global(LlvmNames.opName(op.sym), Type.Ptr)))

          if (j < ops.length - 1) {
            val nextCheck = fb.newBlock(nextLabel)
            currentCheck = nextCheck
            fb.setCurrent(nextCheck)
          }

          j += 1
        }
      }

      var currentCheck = entry
      var i = 0
      while (i < sortedEffects.length) {
        val (effSym, id) = sortedEffects(i)
        val dispatchLabel = s"dispatch_$id"
        val nextLabel = if (i == sortedEffects.length - 1) "default" else s"check_${sortedEffects(i + 1)._2}"
        val cmp = Value.Local(s"cmp_eff_$id", Type.I1)

        currentCheck.emitAssign(cmp, Op.ICmp("eq", effSymId, Value.IntConst(id, Type.I64)))
        currentCheck.setTerminator(Terminator.CondBr(cmp, dispatchLabel, nextLabel))

        emitOpDispatch(effSym, id, dispatchLabel)

        if (i < sortedEffects.length - 1) {
          val nextCheck = fb.newBlock(nextLabel)
          currentCheck = nextCheck
          fb.setCurrent(nextCheck)
        }

        i += 1
      }

      val defaultBlock = fb.newBlock("default")
      fb.setCurrent(defaultBlock)
      defaultBlock.setTerminator(Terminator.Ret(Type.Ptr, Value.Null(Type.Ptr)))

      LlvmIr.Function("flix_op_name", Type.Ptr, params, fb.result())
    }

    private def emitExportWrapper(defn: LoweredAst.Def): LlvmIr.Function = {
      val sig = defn.exportedSignature.getOrElse {
        throw new IllegalStateException(s"Missing portable export signature for '${defn.sym}'.")
      }
      val wrapperName = LlvmNames.exportName(defn.sym)
      val defName = LlvmNames.defName(defn.sym)
      val loweredParams = defn.cparams ::: defn.fparams
      if (sig.params.length > loweredParams.length) {
        throw new IllegalStateException(s"Portable export signature arity exceeds lowered arity for '${defn.sym}'.")
      }
      val exposedParams = loweredParams.take(sig.params.length)
      val hiddenParams = loweredParams.drop(sig.params.length)
      hiddenParams.foreach { p =>
        if (p.tpe != SimpleType.Unit) {
          throw new IllegalStateException(s"Unexpected hidden non-Unit export parameter for '${defn.sym}': ${p.tpe}.")
        }
      }
      val outParamOpt = sig.result match {
        case ExportAbi.AbiType.Unit => None
        case _ => Some(LlvmIr.Param("out", Type.Ptr))
      }

      val params = LlvmIr.Param("ctx", Type.Ptr) :: (exposedParams.zipWithIndex.map {
        case (_, i) => LlvmIr.Param(LlvmNames.paramName(i), exportParamSurfaceTypeOf(sig.params(i)))
      } ::: outParamOpt.toList)

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val outPtrOpt = outParamOpt.map(_ => Value.Local("out", Type.Ptr))
      val abiArgs = sig.params.zipWithIndex.map {
        case (tpe, i) => Value.Local(LlvmNames.paramName(i), exportParamSurfaceTypeOf(tpe))
      }

      val argsWithCleanup = exposedParams.zip(sig.params).zip(abiArgs).map {
        case ((p, abiTpe), v) if ExportAbi.isAggregate(abiTpe) =>
          val aggValue = freshTmp(exportSurfaceTypeOf(abiTpe))
          fb.current.emitAssign(aggValue, Op.Load(exportSurfaceTypeOf(abiTpe), v))
          val encoded = emitEncodeExportAbiValue(ctxPtr, aggValue, abiTpe, fb)
          val handle = encoded.handle
          val tmpPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(tmpPtr, Op.Call(Type.Ptr, "flix_handle_get", List(ctxPtr, handle)))
          val loweredArg = llvmTypeOf(p.tpe) match {
            case Type.Ptr =>
              tmpPtr
            case Type.I64 =>
              val bits = freshTmp(Type.I64)
              fb.current.emitAssign(bits, Op.Cast("ptrtoint", Type.I64, tmpPtr))
              bits
            case other =>
              fb.current.emitTrap()
              Value.Undef(other)
          }
          (loweredArg, Some(handle))
        case ((p, _), v) if isHandleAbiType(p.tpe) =>
          val handle = castValue(v, Type.I64, fb)
          val tmpPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(tmpPtr, Op.Call(Type.Ptr, "flix_handle_get", List(ctxPtr, handle)))
          val loweredArg = llvmTypeOf(p.tpe) match {
            case Type.Ptr =>
              tmpPtr
            case Type.I64 =>
              val bits = freshTmp(Type.I64)
              fb.current.emitAssign(bits, Op.Cast("ptrtoint", Type.I64, tmpPtr))
              bits
            case other =>
              fb.current.emitTrap()
              Value.Undef(other)
          }
          (loweredArg, None)
        case (_, v) =>
          (v, None)
      }
      val args = argsWithCleanup.map(_._1) ::: hiddenParams.map(p => defaultValueFor(p.tpe))

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, defName, ctxPtr :: args))
      argsWithCleanup.foreach {
        case (_, Some(handle)) => emitReleaseExportHandle(ctxPtr, handle, fb)
        case _ => ()
      }

      val r0 = unwindThunkToResult(callTmp, ctxPtr, fb)
      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, r0, index = 0))
      val payload = freshTmp(Type.I64)
      fb.current.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, r0, index = 1))

      val isValue = freshTmp(Type.I1)
      fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

      val valueLabel = freshLabel("export_value")
      val notValueLabel = freshLabel("export_not_value")
      val endLabel = freshLabel("export_end")
      fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, notValueLabel))

      val incomings = mutable.ArrayBuffer.empty[(Value, String)]

      val valueBlock = fb.newBlock(valueLabel)
      fb.setCurrent(valueBlock)
      outPtrOpt.foreach(outPtr => emitStoreExportOkValue(ctxPtr, outPtr, payload, defn.unboxedType.tpe, sig.result, fb))
      val okResult = packResultTagged(ResultTagValue, Value.IntConst(0L, Type.I64), fb)
      val okPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((okResult, okPred))

      val notValueBlock = fb.newBlock(notValueLabel)
      fb.setCurrent(notValueBlock)
      val isExn = freshTmp(Type.I1)
      fb.current.emitAssign(isExn, Op.ICmp("eq", tag, Value.IntConst(ResultTagException, Type.I64)))

      val exnLabel = freshLabel("export_exn")
      val suspCheckLabel = freshLabel("export_susp_check")
      fb.current.setTerminator(Terminator.CondBr(isExn, exnLabel, suspCheckLabel))

      val exnBlock = fb.newBlock(exnLabel)
      fb.setCurrent(exnBlock)
      val exnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(exnPtr, Op.Cast("inttoptr", Type.Ptr, payload))
      val exnHandle = freshTmp(Type.I64)
      fb.current.emitAssign(exnHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, exnPtr)))
      val exnResult = packResultTagged(ResultTagException, exnHandle, fb)
      val exnPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((exnResult, exnPred))

      val suspCheckBlock = fb.newBlock(suspCheckLabel)
      fb.setCurrent(suspCheckBlock)
      val isSusp = freshTmp(Type.I1)
      fb.current.emitAssign(isSusp, Op.ICmp("eq", tag, Value.IntConst(ResultTagSuspension, Type.I64)))
      val suspLabel = freshLabel("export_susp")
      val otherLabel = freshLabel("export_other")
      fb.current.setTerminator(Terminator.CondBr(isSusp, suspLabel, otherLabel))

      val suspBlock = fb.newBlock(suspLabel)
      fb.setCurrent(suspBlock)
      val suspPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(suspPtr, Op.Cast("inttoptr", Type.Ptr, payload))
      val suspHandle = freshTmp(Type.I64)
      fb.current.emitAssign(suspHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, suspPtr)))
      val suspResult = packResultTagged(ResultTagSuspension, suspHandle, fb)
      val suspPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((suspResult, suspPred))

      val otherBlock = fb.newBlock(otherLabel)
      fb.setCurrent(otherBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val r = freshTmp(flixResultType)
      endBlock.emitPhi(r, incomings.toList)
      fb.current.setTerminator(Terminator.Ret(flixResultType, r))

      LlvmIr.Function(wrapperName, flixResultType, params, fb.result())
    }

    /**
      * Emits the def-id based invocation dispatch used by the wasm component runtime.
      *
      * The current dispatch implementation:
      *   - compares `defId` in a linear chain (no LLVM `switch` yet),
      *   - checks `argc` matches the expected arity,
      *   - unboxes each argument from raw `i64` payload bits into the lowered ABI types,
      *   - calls the lowered definition, then unwinds thunks to a stable non-thunk result.
      *
      * The runtime currently:
      *   - assumes incoming WIT `value` resources match the expected kinds,
      *   - allocate `value`/`suspension` resources for the returned result payload.
      */
    private def emitWasmInvokeDef(entries: List[LlvmWasmDefs.Entry]): LlvmIr.Function = {
      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("defId", Type.I64),
        LlvmIr.Param("args", Type.Ptr),
        LlvmIr.Param("argc", Type.I32),
      )

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val defId = Value.Local("defId", Type.I64)
      val argsPtr = Value.Local("args", Type.Ptr)
      val argc = Value.Local("argc", Type.I32)

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      if (entries.isEmpty) {
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)
        return LlvmIr.Function("flix_wasm_invoke_def", flixResultType, params, fb.result())
      }

      var currentCheck = entry

      entries.zipWithIndex.foreach { case (e, idx) =>
        val caseLabel = s"def_${e.defId}"
        val nextLabel = if (idx == entries.length - 1) "default" else s"check_def_${entries(idx + 1).defId}"
        val cmp = Value.Local(s"cmp_def_${e.defId}", Type.I1)

        currentCheck.emitAssign(cmp, Op.ICmp("eq", defId, Value.IntConst(e.defId, Type.I64)))
        currentCheck.setTerminator(Terminator.CondBr(cmp, caseLabel, nextLabel))

        val caseBlock = fb.newBlock(caseLabel)
        fb.setCurrent(caseBlock)

        val sig = e.signature
        val loweredParams = e.defn.cparams ::: e.defn.fparams
        if (sig.params.length > loweredParams.length) {
          throw new IllegalStateException(s"Portable export signature arity exceeds lowered arity for '${e.sym}'.")
        }
        val exposedParams = loweredParams.take(sig.params.length)
        val hiddenParams = loweredParams.drop(sig.params.length)
        hiddenParams.foreach { p =>
          if (p.tpe != SimpleType.Unit) {
            throw new IllegalStateException(s"Unexpected hidden non-Unit wasm export parameter for '${e.sym}': ${p.tpe}.")
          }
        }

        val expected = exposedParams.length
        val arityOk = Value.Local(s"arity_ok_${e.defId}", Type.I1)
        caseBlock.emitAssign(arityOk, Op.ICmp("eq", argc, Value.IntConst(expected.toLong, Type.I32)))

        val callLabel = s"def_call_${e.defId}"
        caseBlock.setTerminator(Terminator.CondBr(arityOk, callLabel, "default"))

        val callBlock = fb.newBlock(callLabel)
        fb.setCurrent(callBlock)

        val exposedArgs: List[Value] = exposedParams.zipWithIndex.map {
          case (p, idx) =>
            val eltPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(eltPtr, Op.Gep(Type.I64, argsPtr, Value.IntConst(idx.toLong, Type.I64)))
            val bits = freshTmp(Type.I64)
            fb.current.emitAssign(bits, Op.Load(Type.I64, eltPtr))
            unboxFromI64(bits, p.tpe, fb)
        }
        val hiddenArgs: List[Value] = hiddenParams.map(_ => Value.IntConst(0L, Type.I64))
        val args: List[Value] = exposedArgs ::: hiddenArgs

        val callTmp = freshTmp(flixResultType)
        fb.current.emitAssign(callTmp, Op.Call(flixResultType, LlvmNames.defName(e.sym), ctxPtr :: args))
        val r0 = unwindThunkToResult(callTmp, ctxPtr, fb)
        val r = wrapResultForWasmRuntime(r0, ctxPtr, fb, wasmRuntimeValuePayloadIsPtr(e.signature))
        fb.current.setTerminator(Terminator.Ret(flixResultType, r))

        // Fallthrough continuation (for the next check).
        if (idx < entries.length - 1) {
          val nextCheck = fb.newBlock(nextLabel)
          currentCheck = nextCheck
          fb.setCurrent(nextCheck)
        }
      }

      // Default case: invalid def-id or arity.
      val defaultBlock = fb.newBlock("default")
      fb.setCurrent(defaultBlock)
      defaultBlock.emitTrap()
      defaultBlock.setTerminator(Terminator.Unreachable)

      LlvmIr.Function("flix_wasm_invoke_def", flixResultType, params, fb.result())
    }

    /**
      * Converts an internal (raw) flixResult payload into a wasm-runtime friendly result where the payload is a
      * stable handle id (for VALUE/EXCEPTION/SUSPENSION).
      *
      * The wasm component runtime stores these handles inside tasks and returns them to the host as `value`
      * resources. The host can then unbox values via WIT helpers.
      */
    private def wrapResultForWasmRuntime(r0: Value, ctxPtr: Value, fb: FunBuilder, valuePayloadIsPtr: Boolean): Value = {
      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, r0, index = 0))
      val payload = freshTmp(Type.I64)
      fb.current.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, r0, index = 1))

      val isValue = freshTmp(Type.I1)
      fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

      val valueLabel = freshLabel("wasm_wrap_value")
      val notValueLabel = freshLabel("wasm_wrap_not_value")
      fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, notValueLabel))

      val endLabel = freshLabel("wasm_wrap_end")

      val incomings = mutable.ArrayBuffer.empty[(Value, String)]

      // VALUE => materialize a stable handle for the returned Flix value.
      val valueBlock = fb.newBlock(valueLabel)
      fb.setCurrent(valueBlock)
      val valueHandle = freshTmp(Type.I64)
      if (valuePayloadIsPtr) {
        val valuePtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(valuePtr, Op.Cast("inttoptr", Type.Ptr, payload))
        fb.current.emitAssign(valueHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, valuePtr)))
      } else {
        fb.current.emitAssign(valueHandle, Op.Call(Type.I64, "flix_handle_new_i64", List(ctxPtr, payload)))
      }
      val valueResult = packResultTagged(ResultTagValue, valueHandle, fb)
      val valuePred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((valueResult, valuePred))

      // Not VALUE: EXCEPTION/SUSPENSION are pointer payloads; wrap them as pointer handles.
      val notValueBlock = fb.newBlock(notValueLabel)
      fb.setCurrent(notValueBlock)
      val isExn = freshTmp(Type.I1)
      fb.current.emitAssign(isExn, Op.ICmp("eq", tag, Value.IntConst(ResultTagException, Type.I64)))

      val exnLabel = freshLabel("wasm_wrap_exn")
      val suspCheckLabel = freshLabel("wasm_wrap_susp_check")
      fb.current.setTerminator(Terminator.CondBr(isExn, exnLabel, suspCheckLabel))

      val exnBlock = fb.newBlock(exnLabel)
      fb.setCurrent(exnBlock)
      val exnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(exnPtr, Op.Cast("inttoptr", Type.Ptr, payload))
      val exnHandle = freshTmp(Type.I64)
      fb.current.emitAssign(exnHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, exnPtr)))
      val exnResult = packResultTagged(ResultTagException, exnHandle, fb)
      val exnPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((exnResult, exnPred))

      val suspCheckBlock = fb.newBlock(suspCheckLabel)
      fb.setCurrent(suspCheckBlock)
      val isSusp = freshTmp(Type.I1)
      fb.current.emitAssign(isSusp, Op.ICmp("eq", tag, Value.IntConst(ResultTagSuspension, Type.I64)))

      val suspLabel = freshLabel("wasm_wrap_susp")
      val otherLabel = freshLabel("wasm_wrap_other")
      fb.current.setTerminator(Terminator.CondBr(isSusp, suspLabel, otherLabel))

      val suspBlock = fb.newBlock(suspLabel)
      fb.setCurrent(suspBlock)
      val suspPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(suspPtr, Op.Cast("inttoptr", Type.Ptr, payload))
      val suspHandle = freshTmp(Type.I64)
      fb.current.emitAssign(suspHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, suspPtr)))
      val suspResult = packResultTagged(ResultTagSuspension, suspHandle, fb)
      val suspPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((suspResult, suspPred))

      val otherBlock = fb.newBlock(otherLabel)
      fb.setCurrent(otherBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val r = freshTmp(flixResultType)
      endBlock.emitPhi(r, incomings.toList)
      r
    }

    /**
      * Resumes a suspension with a value, returning a wasm-runtime friendly handle-result.
      *
      * Signature matches the Zig runtime's `extern fn flix_wasm_resume_ok_def`.
      */
    private def emitWasmResumeOkDef(entries: List[LlvmWasmDefs.Entry]): LlvmIr.Function = {
      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("defId", Type.I64),
        LlvmIr.Param("susp", Type.I64),
        LlvmIr.Param("resume", Type.I64),
      )

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val suspHandle = Value.Local("susp", Type.I64)
      val resumeHandle = Value.Local("resume", Type.I64)

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val suspPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(suspPtr, Op.Call(Type.Ptr, "flix_handle_get", List(ctxPtr, suspHandle)))

      val resumePayload = freshTmp(Type.I64)
      fb.current.emitAssign(resumePayload, Op.Call(Type.I64, "flix_handle_payload", List(ctxPtr, resumeHandle)))

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_resume_suspension", List(ctxPtr, suspPtr, resumePayload)))

      val r0 = unwindThunkToResult(callTmp, ctxPtr, fb)

      if (entries.isEmpty) {
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)
      } else {
        var currentCheck = fb.current
        entries.zipWithIndex.foreach { case (e, idx) =>
          val caseLabel = s"resume_def_${e.defId}"
          val nextLabel = if (idx == entries.length - 1) "resume_default" else s"resume_check_def_${entries(idx + 1).defId}"
          val cmp = Value.Local(s"resume_cmp_def_${e.defId}", Type.I1)

          currentCheck.emitAssign(cmp, Op.ICmp("eq", Value.Local("defId", Type.I64), Value.IntConst(e.defId, Type.I64)))
          currentCheck.setTerminator(Terminator.CondBr(cmp, caseLabel, nextLabel))

          val caseBlock = fb.newBlock(caseLabel)
          fb.setCurrent(caseBlock)
          val r = wrapResultForWasmRuntime(r0, ctxPtr, fb, wasmRuntimeValuePayloadIsPtr(e.signature))
          fb.current.setTerminator(Terminator.Ret(flixResultType, r))

          if (idx < entries.length - 1) {
            val nextCheck = fb.newBlock(nextLabel)
            currentCheck = nextCheck
            fb.setCurrent(nextCheck)
          }
        }

        val defaultBlock = fb.newBlock("resume_default")
        fb.setCurrent(defaultBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)
      }

      LlvmIr.Function("flix_wasm_resume_ok_def", flixResultType, params, fb.result())
    }

    private def wasmRuntimeValuePayloadIsPtr(sig: ExportAbi.Signature): Boolean =
      sig.result != ExportAbi.AbiType.Unit

    /**
      * Resumes a suspension by throwing an exception.
      *
      * Current behavior: abort the task with the given exception payload rather than injecting it
      * at the suspension point.
      *
      * Signature matches the Zig runtime's `extern fn flix_wasm_resume_throw_def`.
      */
    private def emitWasmResumeThrowDef(): LlvmIr.Function = {
      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("defId", Type.I64),
        LlvmIr.Param("susp", Type.I64),
        LlvmIr.Param("exn", Type.I64),
      )

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val exnHandle = Value.Local("exn", Type.I64)

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val exnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(exnPtr, Op.Call(Type.Ptr, "flix_handle_get", List(ctxPtr, exnHandle)))
      val outHandle = freshTmp(Type.I64)
      fb.current.emitAssign(outHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, exnPtr)))

      val r = packResultTagged(ResultTagException, outHandle, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, r))

      LlvmIr.Function("flix_wasm_resume_throw_def", flixResultType, params, fb.result())
    }

    private def emitExportResumeWrapper(defn: LoweredAst.Def, resumeTypeOpt: Option[ExportAbi.AbiType]): LlvmIr.Function = {
      val sig = defn.exportedSignature.getOrElse {
        throw new IllegalStateException(s"Missing portable export signature for '${defn.sym}'.")
      }
      val wrapperName = LlvmNames.exportResumeName(defn.sym)
      val outParamOpt = sig.result match {
        case ExportAbi.AbiType.Unit => None
        case _ => Some(LlvmIr.Param("out", Type.Ptr))
      }
      val resumeParamTpe = resumeTypeOpt match {
        case Some(tpe) => exportParamSurfaceTypeOf(tpe)
        case None => Type.I64
      }

      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("susp", Type.I64),
        LlvmIr.Param("resume", resumeParamTpe)
      ) ::: outParamOpt.toList

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val suspHandle = Value.Local("susp", Type.I64)
      val resumeArg = Value.Local("resume", resumeParamTpe)
      val outPtrOpt = outParamOpt.map(_ => Value.Local("out", Type.Ptr))

      val suspPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(suspPtr, Op.Call(Type.Ptr, "flix_handle_get", List(ctxPtr, suspHandle)))

      val resumePayload = freshTmp(Type.I64)
      val resumeHandleWithCleanup = resumeTypeOpt match {
        case None =>
          ExportHandle(castValue(resumeArg, Type.I64, fb), owned = false)
        case Some(abiTpe) =>
          val encoded = abiTpe match {
            case agg if ExportAbi.isAggregate(agg) =>
              val aggValue = freshTmp(exportSurfaceTypeOf(agg))
              fb.current.emitAssign(aggValue, Op.Load(exportSurfaceTypeOf(agg), resumeArg))
              emitEncodeExportAbiValue(ctxPtr, aggValue, agg, fb)
            case other =>
              emitEncodeExportAbiValue(ctxPtr, resumeArg, other, fb)
          }
          encoded
      }
      fb.current.emitAssign(resumePayload, Op.Call(Type.I64, "flix_handle_payload", List(ctxPtr, resumeHandleWithCleanup.handle)))
      if (resumeHandleWithCleanup.owned) emitReleaseExportHandle(ctxPtr, resumeHandleWithCleanup.handle, fb)

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_resume_suspension", List(ctxPtr, suspPtr, resumePayload)))

      val r0 = unwindThunkToResult(callTmp, ctxPtr, fb)
      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, r0, index = 0))
      val payload = freshTmp(Type.I64)
      fb.current.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, r0, index = 1))

      val isValue = freshTmp(Type.I1)
      fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

      val valueLabel = freshLabel("resume_value")
      val notValueLabel = freshLabel("resume_not_value")
      val endLabel = freshLabel("resume_end")
      fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, notValueLabel))

      val incomings = mutable.ArrayBuffer.empty[(Value, String)]

      val valueBlock = fb.newBlock(valueLabel)
      fb.setCurrent(valueBlock)
      outPtrOpt.foreach(outPtr => emitStoreExportOkValue(ctxPtr, outPtr, payload, defn.unboxedType.tpe, sig.result, fb))
      val okResult = packResultTagged(ResultTagValue, Value.IntConst(0L, Type.I64), fb)
      val okPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((okResult, okPred))

      val notValueBlock = fb.newBlock(notValueLabel)
      fb.setCurrent(notValueBlock)
      val isExn = freshTmp(Type.I1)
      fb.current.emitAssign(isExn, Op.ICmp("eq", tag, Value.IntConst(ResultTagException, Type.I64)))

      val exnLabel = freshLabel("resume_exn")
      val suspCheckLabel = freshLabel("resume_susp_check")
      fb.current.setTerminator(Terminator.CondBr(isExn, exnLabel, suspCheckLabel))

      val exnBlock = fb.newBlock(exnLabel)
      fb.setCurrent(exnBlock)
      val exnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(exnPtr, Op.Cast("inttoptr", Type.Ptr, payload))
      val exnHandle = freshTmp(Type.I64)
      fb.current.emitAssign(exnHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, exnPtr)))
      val exnResult = packResultTagged(ResultTagException, exnHandle, fb)
      val exnPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((exnResult, exnPred))

      val suspCheckBlock = fb.newBlock(suspCheckLabel)
      fb.setCurrent(suspCheckBlock)
      val isSusp = freshTmp(Type.I1)
      fb.current.emitAssign(isSusp, Op.ICmp("eq", tag, Value.IntConst(ResultTagSuspension, Type.I64)))
      val suspLabel = freshLabel("resume_susp")
      val otherLabel = freshLabel("resume_other")
      fb.current.setTerminator(Terminator.CondBr(isSusp, suspLabel, otherLabel))

      val suspBlock = fb.newBlock(suspLabel)
      fb.setCurrent(suspBlock)
      val suspPtr2 = freshTmp(Type.Ptr)
      fb.current.emitAssign(suspPtr2, Op.Cast("inttoptr", Type.Ptr, payload))
      val suspHandle2 = freshTmp(Type.I64)
      fb.current.emitAssign(suspHandle2, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, suspPtr2)))
      val suspResult = packResultTagged(ResultTagSuspension, suspHandle2, fb)
      val suspPred = fb.current.label
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((suspResult, suspPred))

      val otherBlock = fb.newBlock(otherLabel)
      fb.setCurrent(otherBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val r = freshTmp(flixResultType)
      endBlock.emitPhi(r, incomings.toList)
      fb.current.setTerminator(Terminator.Ret(flixResultType, r))

      LlvmIr.Function(wrapperName, flixResultType, params, fb.result())
    }

    private def emitExportRequestWrapper(defn: LoweredAst.Def, op: LoweredAst.Op, sig: ExportAbi.Signature): LlvmIr.Function = {
      val wrapperName = LlvmNames.exportRequestName(defn.sym)
      val requestTpe = requestSurfaceTypeOf(sig)
      val expectedEffId = effectSymIds.getOrElse(op.sym.eff, throw new IllegalStateException(s"missing effect id for ${op.sym.eff}"))
      val expectedOpIndex = opIndices.getOrElse(op.sym, throw new IllegalStateException(s"missing op index for ${op.sym}"))
      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("susp", Type.I64),
        LlvmIr.Param("out", Type.Ptr)
      )

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val suspHandle = Value.Local("susp", Type.I64)
      val outPtr = Value.Local("out", Type.Ptr)

      val effSymId = freshTmp(Type.I64)
      fb.current.emitAssign(effSymId, Op.Call(Type.I64, "flix_suspension_eff_sym_id", List(ctxPtr, suspHandle)))
      val effOk = freshTmp(Type.I1)
      fb.current.emitAssign(effOk, Op.ICmp("eq", effSymId, Value.IntConst(expectedEffId, Type.I64)))
      val effOkLabel = freshLabel("request_eff_ok")
      val badEffLabel = freshLabel("request_bad_eff")
      fb.current.setTerminator(Terminator.CondBr(effOk, effOkLabel, badEffLabel))

      val effOkBlock = fb.newBlock(effOkLabel)
      fb.setCurrent(effOkBlock)
      val opIndex = freshTmp(Type.I64)
      fb.current.emitAssign(opIndex, Op.Call(Type.I64, "flix_suspension_op_index", List(ctxPtr, suspHandle)))
      val opOk = freshTmp(Type.I1)
      fb.current.emitAssign(opOk, Op.ICmp("eq", opIndex, Value.IntConst(expectedOpIndex.toLong, Type.I64)))
      val opOkLabel = freshLabel("request_op_ok")
      val badOpLabel = freshLabel("request_bad_op")
      fb.current.setTerminator(Terminator.CondBr(opOk, opOkLabel, badOpLabel))

      val opOkBlock = fb.newBlock(opOkLabel)
      fb.setCurrent(opOkBlock)
      val argc = freshTmp(Type.I64)
      fb.current.emitAssign(argc, Op.Call(Type.I64, "flix_suspension_arg_count", List(ctxPtr, suspHandle)))
      val argcOk = freshTmp(Type.I1)
      fb.current.emitAssign(argcOk, Op.ICmp("eq", argc, Value.IntConst(sig.params.length.toLong, Type.I64)))
      val okLabel = freshLabel("request_ok")
      val badLabel = freshLabel("request_bad_arity")
      fb.current.setTerminator(Terminator.CondBr(argcOk, okLabel, badLabel))

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)

      val values = if (sig.params.isEmpty) {
        List(Value.IntConst(0L, Type.I8))
      } else {
        sig.params.zip(op.fparams).zipWithIndex.map {
          case ((abiTpe, fparam), idx) =>
            val payload = freshTmp(Type.I64)
            fb.current.emitAssign(payload, Op.Call(Type.I64, "flix_suspension_arg_payload", List(ctxPtr, suspHandle, Value.IntConst(idx.toLong, Type.I64))))
            val tmpOutPtr = freshTmp(Type.Ptr)
            val fieldTpe = requestFieldSurfaceTypeOf(abiTpe)
            fb.current.emitAssign(tmpOutPtr, Op.Alloca(fieldTpe))
            emitStoreExportOkValue(ctxPtr, tmpOutPtr, payload, fparam.tpe, abiTpe, fb)
            val fieldValue = freshTmp(fieldTpe)
            fb.current.emitAssign(fieldValue, Op.Load(fieldTpe, tmpOutPtr))
            fieldValue
        }
      }
      val requestValue = buildRequestStructValue(sig, values, fb)
      fb.current.emitStore(requestValue, outPtr)
      fb.current.setTerminator(Terminator.Ret(Type.Void, Value.Undef(Type.Void)))

      val badEffBlock = fb.newBlock(badEffLabel)
      fb.setCurrent(badEffBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val badOpBlock = fb.newBlock(badOpLabel)
      fb.setCurrent(badOpBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      LlvmIr.Function(wrapperName, Type.Void, params, fb.result())
    }

    private def recordFields(tpe: SimpleType): List[(String, SimpleType)] = {
      @scala.annotation.tailrec
      def loop(t: SimpleType, acc: List[(String, SimpleType)]): List[(String, SimpleType)] = t match {
        case SimpleType.RecordEmpty =>
          acc.reverse
        case SimpleType.RecordExtend(label, value, rest) =>
          loop(rest, (label, value) :: acc)
        case other =>
          throw new IllegalStateException(s"Unexpected record type: '$other'.")
      }
      loop(tpe, Nil)
    }

    private def extTagId(label: Name.Label): Long =
      fnv1a64(label.name)

    private def fnv1a64(s: String): Long = {
      // Stable 64-bit FNV-1a hash used for extensible tag ids.
      var h = 0xcbf29ce484222325L
      val prime = 0x100000001b3L
      val bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      var i = 0
      while (i < bytes.length) {
        h ^= (bytes(i) & 0xff).toLong
        h *= prime
        i += 1
      }
      h
    }

    private case class NativeImportBody(spec: ca.uwaterloo.flix.language.ast.NativeImportSpec, resultTpe: SimpleType, boxed: Boolean)
    private case class WasmImportBody(spec: ca.uwaterloo.flix.language.ast.WasmImportSpec, interfaceId: WasmImportInterface.Id, cSymbol: String, resultTpe: SimpleType, boxed: Boolean)

    private def extractNativeImportBody(exp0: Expr): Option[NativeImportBody] = exp0 match {
      case Expr.NativeImport(spec, tpe, _, _) =>
        Some(NativeImportBody(spec, tpe, boxed = false))
      case Expr.ApplyAtomic(AtomicOp.Box, List(Expr.NativeImport(spec, tpe, _, _)), _, _, _, _) =>
        Some(NativeImportBody(spec, tpe, boxed = true))
      case _ =>
        None
    }

    private def extractWasmImportBody(exp0: Expr): Option[WasmImportBody] = exp0 match {
      case Expr.WasmImport(spec, tpe, _, _) =>
        WasmImportInterface.parse(spec.interface).map(id => WasmImportBody(spec, id, id.cFunctionName(spec.func), tpe, boxed = false))
      case Expr.ApplyAtomic(AtomicOp.Box, List(Expr.WasmImport(spec, tpe, _, _)), _, _, _, _) =>
        WasmImportInterface.parse(spec.interface).map(id => WasmImportBody(spec, id, id.cFunctionName(spec.func), tpe, boxed = true))
      case _ =>
        None
    }

    private def emitDef(defn: LoweredAst.Def): LlvmIr.Function = {
      extractNativeImportBody(defn.exp) match {
        case Some(body) =>
          emitNativeImportDef(defn, body)
        case None =>
          extractWasmImportBody(defn.exp) match {
            case Some(body) =>
              emitWasmImportDef(defn, body)
            case None =>
              if (needsResumableEvaluator(defn)) emitDefControlImpure(defn)
              else emitDefControlPure(defn)
          }
      }
    }

    private def emitNativeImportDef(defn: LoweredAst.Def, body: NativeImportBody): LlvmIr.Function = {
      if (target != CompilationTarget.LlvmNative) {
        throw new IllegalStateException(s"Unexpected native import '${defn.sym}' on target '$target'.")
      }
      emitNativeDirectImportDef(defn, body.resultTpe, body.boxed, body.spec.symbol)
    }

    private def emitWasmImportDef(defn: LoweredAst.Def, body: WasmImportBody): LlvmIr.Function = {
      if (target != CompilationTarget.LlvmWasm) {
        throw new IllegalStateException(s"Unexpected wasm import '${defn.sym}' on target '$target'.")
      }
      emitWasmDirectImportDef(defn, body)
    }

    private def emitNativeDirectImportDef(defn: LoweredAst.Def, resultTpe: SimpleType, boxed: Boolean, cSymbol: String): LlvmIr.Function = {
      if (defn.cparams.nonEmpty || defn.lparams.nonEmpty) {
        throw new IllegalStateException(s"Unexpected closure/local params on direct import '${defn.sym}'.")
      }
      val sig = defn.nativeImportSignature.getOrElse {
        throw new IllegalStateException(s"Missing native import signature for '${defn.sym}'.")
      }

      val fnName = LlvmNames.defName(defn.sym)
      val params = LlvmIr.Param("ctx", Type.Ptr) :: defn.fparams.zipWithIndex.map {
        case (fp, i) => LlvmIr.Param(LlvmNames.paramName(i), llvmTypeOf(fp.tpe))
      }

      val fb = new FunBuilder()
      fb.traceEnabled = true
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)
      fb.current.emitCallVoid("flix_trace_push", List(Value.Global(LlvmNames.traceName(defn.sym), Type.Ptr)))

      val ctxPtr = Value.Local("ctx", Type.Ptr)

      val loopLabel = freshLabel("loop")
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)

      fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
      val isCancelled = freshTmp(Type.I1)
      fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))

      val pollOkLabel = freshLabel("poll_ok")
      val pollCancelLabel = freshLabel("poll_cancel")
      fb.current.setTerminator(Terminator.CondBr(isCancelled, pollCancelLabel, pollOkLabel))

      val pollCancelBlock = fb.newBlock(pollCancelLabel)
      fb.setCurrent(pollCancelBlock)
      val cancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
      val tracedCancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
      val cancelBits = freshTmp(Type.I64)
      fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
      val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, cancelResult))

      val pollOkBlock = fb.newBlock(pollOkLabel)
      fb.setCurrent(pollOkBlock)

      val preparedArgs = defn.fparams.zip(sig.params).zipWithIndex.map {
        case ((fp, abiTpe), i) =>
          prepareNativeImportArg(ctxPtr, Value.Local(LlvmNames.paramName(i), llvmTypeOf(fp.tpe)), fp.tpe, abiTpe, fb)
      }
      val args = (if (sig.requiresBridgeCtx) List(ctxPtr) else Nil) ::: preparedArgs.map(_.arg)

      val rawResultValue = sig.result match {
        case NativeImportAbi.AbiType.Unit =>
          fb.current.emitCallVoid(cSymbol, args)
          emitConstant(Constant.Unit, ctxPtr, fb)

        case abiTpe =>
          val callTpe = nativeImportCallLlvmType(abiTpe)
          val tmp = freshTmp(callTpe)
          fb.current.emitAssign(tmp, Op.Call(callTpe, cSymbol, args))
          tmp
      }
      preparedArgs.foreach(_.cleanupHandle.foreach(emitReleaseExportHandle(ctxPtr, _, fb)))

      val loweredResultValue = decodeNativeImportResult(ctxPtr, resultTpe, sig.result, rawResultValue, fb)

      val resultValue =
        if (boxed) emitApplyAtomic(AtomicOp.Box, List(resultTpe), List(loweredResultValue), defn.tpe, ctxPtr, fb, None)
        else loweredResultValue

      val packed = packResult(resultValue, defn.tpe, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, packed))
      LlvmIr.Function(fnName, flixResultType, params, fb.result())
    }

    private def emitWasmDirectImportDef(defn: LoweredAst.Def, body: WasmImportBody): LlvmIr.Function = {
      if (defn.cparams.nonEmpty || defn.lparams.nonEmpty) {
        throw new IllegalStateException(s"Unexpected closure/local params on wasm import '${defn.sym}'.")
      }
      val sig = defn.wasmImportSignature.getOrElse {
        throw new IllegalStateException(s"Missing portable wasm import signature for '${defn.sym}'.")
      }

      val fnName = LlvmNames.defName(defn.sym)
      val resultByRef = WasmImportAbi.isByRefBoundaryType(sig.result)
      val params = LlvmIr.Param("ctx", Type.Ptr) :: defn.fparams.zipWithIndex.map {
        case (param, i) => LlvmIr.Param(LlvmNames.paramName(i), llvmTypeOf(param.tpe))
      }

      val fb = new FunBuilder()
      fb.traceEnabled = true
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)
      fb.current.emitCallVoid("flix_trace_push", List(Value.Global(LlvmNames.traceName(defn.sym), Type.Ptr)))

      val ctxPtr = Value.Local("ctx", Type.Ptr)

      val loopLabel = freshLabel("loop")
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)

      fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
      val isCancelled = freshTmp(Type.I1)
      fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))

      val pollOkLabel = freshLabel("poll_ok")
      val pollCancelLabel = freshLabel("poll_cancel")
      fb.current.setTerminator(Terminator.CondBr(isCancelled, pollCancelLabel, pollOkLabel))

      val pollCancelBlock = fb.newBlock(pollCancelLabel)
      fb.setCurrent(pollCancelBlock)
      val cancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
      val tracedCancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
      val cancelBits = freshTmp(Type.I64)
      fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
      val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, cancelResult))

      val pollOkBlock = fb.newBlock(pollOkLabel)
      fb.setCurrent(pollOkBlock)

      val preparedArgs = defn.fparams.zip(sig.params).zipWithIndex.map {
        case ((param, abiTpe), i) =>
          val raw = Value.Local(LlvmNames.paramName(i), llvmTypeOf(param.tpe))
          emitWasmImportArgument(ctxPtr, raw, param.tpe, abiTpe, body.interfaceId, fb)
      }
      val importArgs = preparedArgs.map(_.arg)

      val rawResultValue =
        if (resultByRef) {
          val outPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(outPtr, Op.Alloca(wasmImportSurfaceTypeOf(sig.result)))
          fb.current.emitCallVoid(body.cSymbol, importArgs :+ outPtr)
          val outVal =
            if (sig.result == ExportAbi.AbiType.Unit) Value.Undef(wasmImportSurfaceTypeOf(sig.result))
            else {
              val tmp = freshTmp(wasmImportSurfaceTypeOf(sig.result))
              fb.current.emitAssign(tmp, Op.Load(wasmImportSurfaceTypeOf(sig.result), outPtr))
              tmp
          }
          val lowered = emitWasmImportResult(ctxPtr, outVal, body.resultTpe, sig.result, fb)
          emitWasmImportFree(body.interfaceId, sig.result, outPtr, fb)
          lowered
        } else {
          val callTpe = wasmImportScalarLlvmType(sig.result)
          val tmp = freshTmp(callTpe)
          fb.current.emitAssign(tmp, Op.Call(callTpe, body.cSymbol, importArgs))
          tmp
        }

      preparedArgs.foreach(_.cleanup.foreach { case (interfaceId, abiTpe, ptr) =>
        emitWasmImportFree(interfaceId, abiTpe, ptr, fb)
      })

      val resultValue =
        if (body.boxed) emitApplyAtomic(AtomicOp.Box, List(body.resultTpe), List(rawResultValue), defn.tpe, ctxPtr, fb, None)
        else rawResultValue

      val packed = packResult(resultValue, defn.tpe, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, packed))
      LlvmIr.Function(fnName, flixResultType, params, fb.result())
    }

	    private def emitDefControlPure(defn: LoweredAst.Def): LlvmIr.Function = {
	      val fnName = LlvmNames.defName(defn.sym)

	      val params = LlvmIr.Param("ctx", Type.Ptr) :: (defn.cparams ::: defn.fparams).zipWithIndex.map {
	        case (p, i) => LlvmIr.Param(LlvmNames.paramName(i), llvmTypeOf(p.tpe))
	      }

	      val fb = new FunBuilder()
	      var rootsToPop: Long = 0L
	      fb.traceEnabled = true
	      val entry = fb.newBlock("entry")
	      fb.setCurrent(entry)
	      fb.current.emitCallVoid("flix_trace_push", List(Value.Global(LlvmNames.traceName(defn.sym), Type.Ptr)))

	      // Bind closure parameters (cparams). Root GC heap values via stack slots.
	      var env: Map[Symbol.VarSym, Value] = Map.empty
	      var slotTypes: Map[Symbol.VarSym, Type] = Map.empty
	      defn.cparams.zipWithIndex.foreach {
	        case (p, i) =>
	          val paramTpe = llvmTypeOf(p.tpe)
	          val paramVal = Value.Local(LlvmNames.paramName(i), paramTpe)

	          if (isGcRootType(p.tpe)) {
	            val slotPtr = freshTmp(Type.Ptr)
	            fb.current.emitAssign(slotPtr, Op.Alloca(paramTpe))
	            fb.current.emitStore(paramVal, slotPtr)
	            fb.current.emitCallVoid(rootPushNameOf(paramTpe), List(Value.Local("ctx", Type.Ptr), slotPtr))
	            rootsToPop += 1
	            env = env.updated(p.sym, slotPtr)
	            slotTypes = slotTypes.updated(p.sym, paramTpe)
	          } else {
	            env = env.updated(p.sym, paramVal)
	          }
	      }

	      // Bind function parameters (fparams) via stack slots so ApplySelfTail can update them.
	      defn.fparams.zipWithIndex.foreach {
	        case (p, j) =>
	          val idx = defn.cparams.length + j
	          val paramTpe = llvmTypeOf(p.tpe)
	          val paramVal = Value.Local(LlvmNames.paramName(idx), paramTpe)

	          val slotPtr = freshTmp(Type.Ptr)
	          fb.current.emitAssign(slotPtr, Op.Alloca(paramTpe))
	          fb.current.emitStore(paramVal, slotPtr)
	          if (isGcRootType(p.tpe)) {
	            fb.current.emitCallVoid(rootPushNameOf(paramTpe), List(Value.Local("ctx", Type.Ptr), slotPtr))
	            rootsToPop += 1
	          }

	          env = env.updated(p.sym, slotPtr)
	          slotTypes = slotTypes.updated(p.sym, paramTpe)
	      }

      // Bind locals (lparams) via stack slots.
      //
      // This is important for GC readiness: it ensures that locals have stable addresses and can
      // later be registered with an explicit root stack (shadow stack) without needing LLVM
      // statepoints/stackmaps in the current backend.
	      defn.lparams.foreach {
	        case LoweredAst.LocalParam(sym, tpe) =>
	          val localTpe = llvmTypeOf(tpe)

	          val slotPtr = freshTmp(Type.Ptr)
	          fb.current.emitAssign(slotPtr, Op.Alloca(localTpe))
	          fb.current.emitStore(zeroValueOf(localTpe), slotPtr)
	          if (isGcRootType(tpe)) {
	            fb.current.emitCallVoid(rootPushNameOf(localTpe), List(Value.Local("ctx", Type.Ptr), slotPtr))
	            rootsToPop += 1
	          }

	          env = env.updated(sym, slotPtr)
	          slotTypes = slotTypes.updated(sym, localTpe)
	      }

	      val ctxPtr = Value.Local("ctx", Type.Ptr)
	      fb.rootsToPop = rootsToPop

	      val loopLabel = freshLabel("loop")
	      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)

      // Pollcheck on the self-tail loop backedge (GC/cancellation handshake foundation).
      fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
      val isCancelled = freshTmp(Type.I1)
      fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))

      val pollOkLabel = freshLabel("poll_ok")
      val pollCancelLabel = freshLabel("poll_cancel")
      fb.current.setTerminator(Terminator.CondBr(isCancelled, pollCancelLabel, pollOkLabel))

      val pollCancelBlock = fb.newBlock(pollCancelLabel)
      fb.setCurrent(pollCancelBlock)
      val cancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
      val tracedCancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
      val cancelBits = freshTmp(Type.I64)
      fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
      val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, cancelResult))

      val pollOkBlock = fb.newBlock(pollOkLabel)
      fb.setCurrent(pollOkBlock)

      val value = emitExpr(defn.exp, env, ctxPtr, fb, lenv = Map.empty, slotTypes = slotTypes, selfTailLabel = Some(loopLabel))
      if (!fb.current.isTerminated) {
        val packed = packResult(value, defn.tpe, fb)
        fb.current.setTerminator(Terminator.Ret(flixResultType, packed))
      }

      LlvmIr.Function(fnName, flixResultType, params, fb.result())
    }

    private def emitDefControlImpure(defn: LoweredAst.Def): LlvmIr.Function = {
      val fnName = LlvmNames.defName(defn.sym)
      addExtraFunction(emitFrameApplyFunction(defn))

      val params = LlvmIr.Param("ctx", Type.Ptr) :: (defn.cparams ::: defn.fparams).zipWithIndex.map {
        case (p, i) => LlvmIr.Param(LlvmNames.paramName(i), llvmTypeOf(p.tpe))
      }

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)

      val framePtr = freshTmp(Type.Ptr)
      val frameTi = Value.Global(LlvmNames.frameTypeInfoName(defn.sym), Type.Ptr)
      fb.current.emitAssign(framePtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, frameTi)))

      // payload[0] = pc = 0.
      storeObjI64Slot(framePtr, Value.IntConst(0L, Type.I64), Value.IntConst(0L, Type.I64), fb)

      // payload[1..] = cparams, fparams, lparams.
      val varsBase = 1L
      (defn.cparams ::: defn.fparams).zipWithIndex.foreach {
        case (p, i) =>
          val idx = Value.IntConst(varsBase + i.toLong, Type.I64)
          val paramVal = Value.Local(LlvmNames.paramName(i), llvmTypeOf(p.tpe))
          val payload = boxToI64(paramVal, p.tpe, fb)
          storeObjI64Slot(framePtr, idx, payload, fb)
      }

      val localsBase = varsBase + (defn.cparams.length + defn.fparams.length).toLong
      defn.lparams.zipWithIndex.foreach {
        case (_, i) =>
          val idx = Value.IntConst(localsBase + i.toLong, Type.I64)
          storeObjI64Slot(framePtr, idx, Value.IntConst(0L, Type.I64), fb)
      }

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, LlvmNames.frameApplyName(defn.sym), List(ctxPtr, framePtr, Value.IntConst(ResultTagValue, Type.I64), Value.IntConst(0L, Type.I64))))
      fb.current.setTerminator(Terminator.Ret(flixResultType, callTmp))

      LlvmIr.Function(fnName, flixResultType, params, fb.result())
    }

	    private def emitFrameApplyFunction(defn: LoweredAst.Def): LlvmIr.Function = {
	      val fnName = LlvmNames.frameApplyName(defn.sym)
      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("self", Type.Ptr),
        LlvmIr.Param("arg_tag", Type.I64),
        LlvmIr.Param("arg0", Type.I64)
      )

      val fb = new FunBuilder()
      fb.traceEnabled = true
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)
      fb.current.emitCallVoid("flix_trace_push", List(Value.Global(LlvmNames.traceName(defn.sym), Type.Ptr)))

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val framePtr = Value.Local("self", Type.Ptr)
      val resumeTag = Value.Local("arg_tag", Type.I64)
      val resumePayload = Value.Local("arg0", Type.I64)

      // Root the current frame pointer for the duration of the apply.
      // The effectful evaluator stores its live state in the frame object, so keeping the frame
      // alive is the key to GC-readiness at safepoints.
      val frameSlotPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(frameSlotPtr, Op.Alloca(Type.Ptr))
      fb.current.emitStore(framePtr, frameSlotPtr)
      fb.current.emitCallVoid(rootPushNameOf(Type.Ptr), List(ctxPtr, frameSlotPtr))
      fb.rootsToPop = 1L

      val pcPayload = loadObjI64Slot(framePtr, Value.IntConst(0L, Type.I64), fb)

      val pcLabel = (i: Int) => s"pc_${i}"
      val badLabel = freshLabel("pc_bad")

      // Dispatch chain on `pcPayload`.
      var i = 0
      var cur = fb.current
      while (i <= defn.pcPoints) {
        val isPc = freshTmp(Type.I1)
        cur.emitAssign(isPc, Op.ICmp("eq", pcPayload, Value.IntConst(i.toLong, Type.I64)))
        val nextLabel = if (i == defn.pcPoints) badLabel else freshLabel(s"pc_test_${i + 1}_")
        cur.setTerminator(Terminator.CondBr(isPc, pcLabel(i), nextLabel))
        if (i < defn.pcPoints) {
          cur = fb.newBlock(nextLabel)
        }
        i += 1
      }

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      // Pre-create pc blocks.
      val pcBlocks: Map[Int, BlockBuilder] = (0 to defn.pcPoints).map { id =>
        id -> fb.newBlock(pcLabel(id))
      }.toMap

      // Variable slots mapping: payload[0]=pc, payload[1..] are vars.
      val base = 1L
      val slotIndexOf: Map[Symbol.VarSym, Long] = {
        val m = mutable.Map.empty[Symbol.VarSym, Long]
        defn.cparams.zipWithIndex.foreach { case (p, j) => m.put(p.sym, base + j.toLong) }
        val fBase = base + defn.cparams.length.toLong
        defn.fparams.zipWithIndex.foreach { case (p, j) => m.put(p.sym, fBase + j.toLong) }
        val lBase = fBase + defn.fparams.length.toLong
        defn.lparams.zipWithIndex.foreach { case (lp, j) => m.put(lp.sym, lBase + j.toLong) }
        m.toMap
      }

      // Compile from pc_0.
      fb.setCurrent(pcBlocks(0))
      // Pollcheck and cancellation at entry to the effectful evaluator.
      fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
      val entryCancelled = freshTmp(Type.I1)
      fb.current.emitAssign(entryCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))

      val entryOkLabel = freshLabel("pc0_ok")
      val entryCancelLabel = freshLabel("pc0_cancel")
      fb.current.setTerminator(Terminator.CondBr(entryCancelled, entryCancelLabel, entryOkLabel))

      val entryCancelBlock = fb.newBlock(entryCancelLabel)
      fb.setCurrent(entryCancelBlock)
      val cancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
      val tracedCancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
      val cancelBits = freshTmp(Type.I64)
      fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
      val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, cancelResult))

      val entryOkBlock = fb.newBlock(entryOkLabel)
      fb.setCurrent(entryOkBlock)
      val value = emitExprControlImpure(defn.exp, ctxPtr, fb, framePtr, slotIndexOf, lenv = Map.empty, resumeTag = resumeTag, resumePayload = resumePayload, pcBlocks = pcBlocks)
      if (!fb.current.isTerminated) {
        val packed = packResult(value, defn.tpe, fb)
        fb.current.setTerminator(Terminator.Ret(flixResultType, packed))
      }

      // Any pc blocks not filled are traps (should not happen if pcPoints is consistent).
      (1 to defn.pcPoints).foreach { id =>
        val b = pcBlocks(id)
        if (!b.isTerminated) {
          fb.setCurrent(b)
          fb.current.emitTrap()
          fb.current.setTerminator(Terminator.Unreachable)
        }
      }

      LlvmIr.Function(fnName, flixResultType, params, fb.result())
    }

    private def collectClosureSyms(): Set[Symbol.DefnSym] = {
      val syms = mutable.Set.empty[Symbol.DefnSym]

      def visitExp(e: Expr): Unit = e match {
        case Expr.Cst(_, _) => ()
        case Expr.NativeImport(_, _, _, _) => ()
        case Expr.WasmImport(_, _, _, _) => ()
        case Expr.Var(_, _, _) => ()

        case Expr.Let(_, e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.Stmt(e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.IfThenElse(e1, e2, e3, _, _, _) =>
          visitExp(e1)
          visitExp(e2)
          visitExp(e3)

        case Expr.Branch(e0, branches, _, _, _) =>
          visitExp(e0)
          branches.values.foreach(visitExp)

        case Expr.JumpTo(_, _, _, _) => ()

        case Expr.ApplyAtomic(op, exps, _, _, _, _) =>
          op match {
            case AtomicOp.Closure(sym) => syms += sym
            case _ => ()
          }
          exps.foreach(visitExp)

        case Expr.ApplyClo(e1, e2, _, _, _, _, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.ApplyDef(_, exps, _, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplyOp(_, exps, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplySelfTail(_, actuals, _, _, _) =>
          actuals.foreach(visitExp)

        case Expr.Region(_, e0, _, _, _, _) =>
          visitExp(e0)

        case Expr.TryCatch(e0, rules, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.RunWith(e0, _, rules, _, _, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.NewObject(_, _, _, _, methods, _) =>
          methods.foreach(m => visitExp(m.exp))
      }

      root.defs.values.foreach(defn => visitExp(defn.exp))
      syms.toSet
    }

    private def collectThunkInvokeSyms(): Set[Symbol.DefnSym] = {
      val syms = mutable.Set.empty[Symbol.DefnSym]

      def visitExp(e: Expr): Unit = e match {
        case Expr.Cst(_, _) => ()
        case Expr.NativeImport(_, _, _, _) => ()
        case Expr.WasmImport(_, _, _, _) => ()
        case Expr.Var(_, _, _) => ()

        case Expr.Let(_, e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.Stmt(e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.IfThenElse(e1, e2, e3, _, _, _) =>
          visitExp(e1)
          visitExp(e2)
          visitExp(e3)

        case Expr.Branch(e0, branches, _, _, _) =>
          visitExp(e0)
          branches.values.foreach(visitExp)

        case Expr.JumpTo(_, _, _, _) => ()

        case Expr.ApplyAtomic(_, exps, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplyClo(e1, e2, _, _, _, _, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.ApplyDef(sym, exps, ct, _, _, _, _) =>
          if (ct == ExpPosition.Tail) syms += sym
          exps.foreach(visitExp)

        case Expr.ApplyOp(_, exps, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplySelfTail(_, actuals, _, _, _) =>
          actuals.foreach(visitExp)

        case Expr.Region(_, e0, _, _, _, _) =>
          visitExp(e0)

        case Expr.TryCatch(e0, rules, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.RunWith(e0, _, rules, _, _, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.NewObject(_, _, _, _, methods, _) =>
          methods.foreach(m => visitExp(m.exp))
      }

      root.defs.values.foreach(defn => visitExp(defn.exp))
      syms.toSet
    }

    private def collectThunkApplyClosureArgTypes(): Set[SimpleType] = {
      val tpes = mutable.Set.empty[SimpleType]

      def visitExp(e: Expr): Unit = e match {
        case Expr.Cst(_, _) => ()
        case Expr.NativeImport(_, _, _, _) => ()
        case Expr.WasmImport(_, _, _, _) => ()
        case Expr.Var(_, _, _) => ()

        case Expr.Let(_, e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.Stmt(e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.IfThenElse(e1, e2, e3, _, _, _) =>
          visitExp(e1)
          visitExp(e2)
          visitExp(e3)

        case Expr.Branch(e0, branches, _, _, _) =>
          visitExp(e0)
          branches.values.foreach(visitExp)

        case Expr.JumpTo(_, _, _, _) => ()

        case Expr.ApplyAtomic(_, exps, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplyClo(e1, e2, ct, _, _, _, _) =>
          if (ct == ExpPosition.Tail) tpes += e2.tpe
          visitExp(e1)
          visitExp(e2)

        case Expr.ApplyDef(_, exps, _, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplyOp(_, exps, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplySelfTail(_, actuals, _, _, _) =>
          actuals.foreach(visitExp)

        case Expr.Region(_, e0, _, _, _, _) =>
          visitExp(e0)

        case Expr.TryCatch(e0, rules, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.RunWith(e0, _, rules, _, _, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.NewObject(_, _, _, _, methods, _) =>
          methods.foreach(m => visitExp(m.exp))
      }

      root.defs.values.foreach(defn => visitExp(defn.exp))
      tpes.toSet
    }

    private def collectLazyInnerTypes(): Set[SimpleType] = {
      val tpes = mutable.Set.empty[SimpleType]

      def visitExp(e: Expr): Unit = e match {
        case Expr.Cst(_, _) => ()
        case Expr.NativeImport(_, _, _, _) => ()
        case Expr.WasmImport(_, _, _, _) => ()
        case Expr.Var(_, _, _) => ()

        case Expr.Let(_, e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.Stmt(e1, e2, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.IfThenElse(e1, e2, e3, _, _, _) =>
          visitExp(e1)
          visitExp(e2)
          visitExp(e3)

        case Expr.Branch(e0, branches, _, _, _) =>
          visitExp(e0)
          branches.values.foreach(visitExp)

        case Expr.JumpTo(_, _, _, _) => ()

        case Expr.ApplyAtomic(op, exps, _, tpe, _, _) =>
          op match {
            case AtomicOp.Lazy =>
              tpe match {
                case SimpleType.Lazy(inner) => tpes += inner
                case _ => ()
              }
            case _ => ()
          }
          exps.foreach(visitExp)

        case Expr.ApplyClo(e1, e2, _, _, _, _, _) =>
          visitExp(e1)
          visitExp(e2)

        case Expr.ApplyDef(_, exps, _, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplyOp(_, exps, _, _, _, _) =>
          exps.foreach(visitExp)

        case Expr.ApplySelfTail(_, actuals, _, _, _) =>
          actuals.foreach(visitExp)

        case Expr.Region(_, e0, _, _, _, _) =>
          visitExp(e0)

        case Expr.TryCatch(e0, rules, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.RunWith(e0, _, rules, _, _, _, _, _) =>
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.NewObject(_, _, _, _, methods, _) =>
          methods.foreach(m => visitExp(m.exp))
      }

      root.defs.values.foreach(defn => visitExp(defn.exp))
      tpes.toSet
    }

    private def collectTupleTypes(): Set[SimpleType.Tuple] = {
      val tpes = mutable.Set.empty[SimpleType.Tuple]
      tpes ++= collectPortableAbiLoweredTypes().collect {
        case tup: SimpleType.Tuple => tup
      }

      def record(tpe: SimpleType): Unit = tpe match {
        case tup: SimpleType.Tuple => tpes += tup
        case _ => ()
      }

      def recordBackendTuple(op: AtomicOp): Unit = op match {
        case AtomicOp.Unary(SemanticOp.ParseOp.Int8FromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int8))
        case AtomicOp.Unary(SemanticOp.ParseOp.Int16FromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int16))
        case AtomicOp.Unary(SemanticOp.ParseOp.Int32FromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32))
        case AtomicOp.Unary(SemanticOp.ParseOp.Int64FromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64))
        case AtomicOp.Unary(SemanticOp.ParseOp.Float32FromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Float32))
        case AtomicOp.Unary(SemanticOp.ParseOp.Float64FromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Float64))
        case AtomicOp.Unary(SemanticOp.ParseOp.BigIntFromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.BigInt))
        case AtomicOp.Unary(SemanticOp.ParseOp.BigDecimalFromString) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.BigDecimal))
        case AtomicOp.Unary(SemanticOp.ParseOp.Int32Parse) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32))
        case AtomicOp.Unary(SemanticOp.ParseOp.Int64Parse) =>
          tpes += SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64))
        case _ => ()
      }

      def visitExp(e: Expr): Unit = e match {
        case Expr.Cst(_, _) => record(e.tpe)
        case Expr.NativeImport(_, _, _, _) => record(e.tpe)
        case Expr.WasmImport(_, _, _, _) => record(e.tpe)
        case Expr.Var(_, _, _) => record(e.tpe)

        case Expr.Let(_, e1, e2, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)

        case Expr.Stmt(e1, e2, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)

        case Expr.IfThenElse(e1, e2, e3, _, _, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)
          visitExp(e3)

        case Expr.Branch(e0, branches, _, _, _) =>
          record(e.tpe)
          visitExp(e0)
          branches.values.foreach(visitExp)

        case Expr.JumpTo(_, _, _, _) =>
          record(e.tpe)

        case Expr.ApplyAtomic(op, exps, _, _, _, _) =>
          record(e.tpe)
          recordBackendTuple(op)
          exps.foreach(visitExp)

        case Expr.ApplyClo(e1, e2, _, _, _, _, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)

        case Expr.ApplyDef(_, exps, _, _, _, _, _) =>
          record(e.tpe)
          exps.foreach(visitExp)

        case Expr.ApplyOp(_, exps, _, _, _, _) =>
          record(e.tpe)
          exps.foreach(visitExp)

        case Expr.ApplySelfTail(_, actuals, _, _, _) =>
          record(e.tpe)
          actuals.foreach(visitExp)

        case Expr.Region(_, e0, _, _, _, _) =>
          record(e.tpe)
          visitExp(e0)

        case Expr.TryCatch(e0, rules, _, _, _) =>
          record(e.tpe)
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.RunWith(e0, _, rules, _, _, _, _, _) =>
          record(e.tpe)
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.NewObject(_, _, _, _, methods, _) =>
          record(e.tpe)
          methods.foreach(m => visitExp(m.exp))
      }

      root.defs.values.foreach(defn => visitExp(defn.exp))
      tpes.toSet
    }

    private def collectRecordTypes(): Set[SimpleType] = {
      val tpes = mutable.Set.empty[SimpleType]
      tpes ++= collectPortableAbiLoweredTypes().collect {
        case rec@SimpleType.RecordExtend(_, _, _) => rec
      }

      def record(tpe: SimpleType): Unit = tpe match {
        case SimpleType.RecordEmpty =>
          () // represented as null pointer; no typeinfo required
        case _: SimpleType.RecordExtend =>
          tpes += tpe
        case _ =>
          ()
      }

      def visitExp(e: Expr): Unit = e match {
        case Expr.Cst(_, _) => record(e.tpe)
        case Expr.NativeImport(_, _, _, _) => record(e.tpe)
        case Expr.WasmImport(_, _, _, _) => record(e.tpe)
        case Expr.Var(_, _, _) => record(e.tpe)

        case Expr.Let(_, e1, e2, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)

        case Expr.Stmt(e1, e2, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)

        case Expr.IfThenElse(e1, e2, e3, _, _, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)
          visitExp(e3)

        case Expr.Branch(e0, branches, _, _, _) =>
          record(e.tpe)
          visitExp(e0)
          branches.values.foreach(visitExp)

        case Expr.JumpTo(_, _, _, _) =>
          record(e.tpe)

        case Expr.ApplyAtomic(_, exps, _, _, _, _) =>
          record(e.tpe)
          exps.foreach(visitExp)

        case Expr.ApplyClo(e1, e2, _, _, _, _, _) =>
          record(e.tpe)
          visitExp(e1)
          visitExp(e2)

        case Expr.ApplyDef(_, exps, _, _, _, _, _) =>
          record(e.tpe)
          exps.foreach(visitExp)

        case Expr.ApplyOp(_, exps, _, _, _, _) =>
          record(e.tpe)
          exps.foreach(visitExp)

        case Expr.ApplySelfTail(_, actuals, _, _, _) =>
          record(e.tpe)
          actuals.foreach(visitExp)

        case Expr.Region(_, e0, _, _, _, _) =>
          record(e.tpe)
          visitExp(e0)

        case Expr.TryCatch(e0, rules, _, _, _) =>
          record(e.tpe)
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.RunWith(e0, _, rules, _, _, _, _, _) =>
          record(e.tpe)
          visitExp(e0)
          rules.foreach(r => visitExp(r.exp))

        case Expr.NewObject(_, _, _, _, methods, _) =>
          record(e.tpe)
          methods.foreach(m => visitExp(m.exp))
      }

      root.defs.values.foreach(defn => visitExp(defn.exp))
      tpes.toSet
    }

    private def collectPortableAbiLoweredTypes(): Set[SimpleType] = {
      val exported = root.defs.values.toList.flatMap(_.exportedSignature)
      val wasmImported = root.defs.values.toList.flatMap(_.wasmImportSignature)
      val nativeImported = root.defs.values.toList.flatMap(_.nativeImportSignature).flatMap { sig =>
        (sig.params :+ sig.result).collect {
          case NativeImportAbi.AbiType.Portable(tpe) => tpe
        }
      }

      val abiTypes =
        (exported ::: wasmImported).flatMap(sig => sig.params :+ sig.result) ::: nativeImported

      abiTypes
        .flatMap(ExportAbi.flattenAbiType)
        .map(portableImportLoweredType)
        .toSet
    }

    private def emitClosureInvokeWrapper(defn: LoweredAst.Def): LlvmIr.Function = {
      val wrapperName = LlvmNames.closureInvokeName(defn.sym)
      val defName = LlvmNames.defName(defn.sym)

      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("self", Type.Ptr),
        LlvmIr.Param("arg_tag", Type.I64),
        LlvmIr.Param("arg0", Type.I64)
      )

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      if (defn.fparams.length != 1) {
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Ret(flixResultType, Value.Undef(flixResultType)))
        return LlvmIr.Function(wrapperName, flixResultType, params, fb.result())
      }

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val selfPtr = Value.Local("self", Type.Ptr)
      val arg0Payload = Value.Local("arg0", Type.I64)
      var rootsToPop = 0L

      val capturedArgs = defn.cparams.zipWithIndex.map {
        case (cp, i) =>
          val payload = loadObjI64Slot(selfPtr, Value.IntConst(i.toLong, Type.I64), fb)
          val value = unboxFromI64(payload, cp.tpe, fb)
          if (isGcRootType(cp.tpe)) {
            val slotPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(slotPtr, Op.Alloca(llvmTypeOf(cp.tpe)))
            fb.current.emitStore(value, slotPtr)
            fb.current.emitCallVoid(rootPushNameOf(llvmTypeOf(cp.tpe)), List(ctxPtr, slotPtr))
            rootsToPop += 1
          }
          value
      }

      val arg0 = {
        val value = unboxFromI64(arg0Payload, defn.fparams.head.tpe, fb)
        if (isGcRootType(defn.fparams.head.tpe)) {
          val slotPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(slotPtr, Op.Alloca(llvmTypeOf(defn.fparams.head.tpe)))
          fb.current.emitStore(value, slotPtr)
          fb.current.emitCallVoid(rootPushNameOf(llvmTypeOf(defn.fparams.head.tpe)), List(ctxPtr, slotPtr))
          rootsToPop += 1
        }
        value
      }

      fb.rootsToPop = rootsToPop

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, defName, ctxPtr :: (capturedArgs :+ arg0)))
      fb.current.setTerminator(Terminator.Ret(flixResultType, callTmp))

      LlvmIr.Function(wrapperName, flixResultType, params, fb.result())
    }

    private def emitThunkInvokeWrapper(defn: LoweredAst.Def): LlvmIr.Function = {
      val wrapperName = LlvmNames.thunkInvokeName(defn.sym)
      val defName = LlvmNames.defName(defn.sym)

      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("self", Type.Ptr),
        LlvmIr.Param("arg_tag", Type.I64),
        LlvmIr.Param("arg0", Type.I64), // dummy
      )

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val selfPtr = Value.Local("self", Type.Ptr)
      var rootsToPop = 0L

      val allParams = defn.cparams ::: defn.fparams
      val args = allParams.zipWithIndex.map {
        case (p, i) =>
          val payload = loadObjI64Slot(selfPtr, Value.IntConst(i.toLong, Type.I64), fb)
          val value = unboxFromI64(payload, p.tpe, fb)
          if (isGcRootType(p.tpe)) {
            val slotPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(slotPtr, Op.Alloca(llvmTypeOf(p.tpe)))
            fb.current.emitStore(value, slotPtr)
            fb.current.emitCallVoid(rootPushNameOf(llvmTypeOf(p.tpe)), List(ctxPtr, slotPtr))
            rootsToPop += 1
          }
          value
      }

      fb.rootsToPop = rootsToPop

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, defName, ctxPtr :: args))
      fb.current.setTerminator(Terminator.Ret(flixResultType, callTmp))

      LlvmIr.Function(wrapperName, flixResultType, params, fb.result())
    }

    private def emitThunkApplyClosureWrapper(argTpe: SimpleType): LlvmIr.Function = {
      val wrapperName = LlvmNames.thunkApplyClosureName(argTpe)

      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("self", Type.Ptr),
        LlvmIr.Param("arg_tag", Type.I64),
        LlvmIr.Param("arg0", Type.I64), // dummy
      )

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val selfPtr = Value.Local("self", Type.Ptr)
      var rootsToPop = 0L

      // Captured layout:
      //   payload[0] = closure pointer bits (i64)
      //   payload[1] = argument payload bits (i64)
      val cloBits = loadObjI64Slot(selfPtr, Value.IntConst(0L, Type.I64), fb)

      val cloPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(cloPtr, Op.Cast("inttoptr", Type.Ptr, cloBits))
      val cloRootSlot = freshTmp(Type.Ptr)
      fb.current.emitAssign(cloRootSlot, Op.Alloca(Type.Ptr))
      fb.current.emitStore(cloPtr, cloRootSlot)
      fb.current.emitCallVoid(rootPushNameOf(Type.Ptr), List(ctxPtr, cloRootSlot))
      rootsToPop += 1

      val argBits = loadObjI64Slot(selfPtr, Value.IntConst(1L, Type.I64), fb)
      if (isGcRootType(argTpe)) {
        val argValue = unboxFromI64(argBits, argTpe, fb)
        val argRootSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(argRootSlot, Op.Alloca(llvmTypeOf(argTpe)))
        fb.current.emitStore(argValue, argRootSlot)
        fb.current.emitCallVoid(rootPushNameOf(llvmTypeOf(argTpe)), List(ctxPtr, argRootSlot))
        rootsToPop += 1
      }

      fb.rootsToPop = rootsToPop

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_invoke_thunk", List(ctxPtr, cloPtr, Value.IntConst(ResultTagValue, Type.I64), argBits)))
      fb.current.setTerminator(Terminator.Ret(flixResultType, callTmp))

      LlvmIr.Function(wrapperName, flixResultType, params, fb.result())
    }

    private def unboxPayloadToType(payload: Value, tpe: Type, fb: FunBuilder): Value = tpe match {
      case Type.I1 =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I1, payload))
        tmp
      case Type.I8 =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I8, payload))
        tmp
      case Type.I16 =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I16, payload))
        tmp
      case Type.I32 =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, payload))
        tmp
      case Type.I64 =>
        payload
      case Type.Float =>
        val asI32 = freshTmp(Type.I32)
        fb.current.emitAssign(asI32, Op.Cast("trunc", Type.I32, payload))
        val asF32 = freshTmp(Type.Float)
        fb.current.emitAssign(asF32, Op.Cast("bitcast", Type.Float, asI32))
        asF32
      case Type.Double =>
        val asF64 = freshTmp(Type.Double)
        fb.current.emitAssign(asF64, Op.Cast("bitcast", Type.Double, payload))
        asF64
      case Type.Ptr =>
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Cast("inttoptr", Type.Ptr, payload))
        tmp
      case _ =>
        fb.current.emitTrap()
        Value.Undef(tpe)
    }

    private def emitNativeMainWrapper(mainSym: Symbol.DefnSym): LlvmIr.Function = {
      val flixMainName = LlvmNames.defName(mainSym)
      val mainDef = root.defs(mainSym)

      val params = List(
        LlvmIr.Param("argc", Type.I32),
        LlvmIr.Param("argv", Type.Ptr)
      )

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      fb.current.emitCallVoid("flix_init", List(Value.Local("argc", Type.I32), Value.Local("argv", Type.Ptr)))

      // Construct a runtime context for the duration of the program.
      // Note: We intentionally do not free it here (process teardown reclaims it),
      // and detached native threads may still be running after `main` returns.
      val ctxPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(ctxPtr, Op.Call(Type.Ptr, "flix_ctx_new", Nil))
      val mainArgs = (mainDef.cparams ::: mainDef.fparams).map(p => defaultValueFor(p.tpe))
      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, flixMainName, ctxPtr :: mainArgs))

      // Ensure we run to completion even if main returns a THUNK.
      val r = unwindThunkToResult(callTmp, ctxPtr, fb)

      val initialTag = freshTmp(Type.I64)
      fb.current.emitAssign(initialTag, Op.ExtractValue(Type.I64, flixResultType, r, index = 0))
      val initialPayload = freshTmp(Type.I64)
      fb.current.emitAssign(initialPayload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))

      val driven = freshTmp(flixResultType)
      fb.current.emitAssign(driven, Op.Call(flixResultType, "flix_native_drive_result", List(ctxPtr, initialTag, initialPayload)))

      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, driven, index = 0))

      val isValue = freshTmp(Type.I1)
      fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

      val valueLabel = freshLabel("main_value")
      val notValueLabel = freshLabel("main_not_value")
      fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, notValueLabel))

      val valueBlock = fb.newBlock(valueLabel)
      fb.setCurrent(valueBlock)
      fb.current.setTerminator(Terminator.Ret(Type.I32, Value.IntConst(0L, Type.I32)))

      val notValueBlock = fb.newBlock(notValueLabel)
      fb.setCurrent(notValueBlock)
      val isExn = freshTmp(Type.I1)
      fb.current.emitAssign(isExn, Op.ICmp("eq", tag, Value.IntConst(ResultTagException, Type.I64)))

      val exnLabel = freshLabel("main_exn")
      val suspCheckLabel = freshLabel("main_susp_check")
      val badLabel = freshLabel("main_bad")
      fb.current.setTerminator(Terminator.CondBr(isExn, exnLabel, suspCheckLabel))

      val exnBlock = fb.newBlock(exnLabel)
      fb.setCurrent(exnBlock)
      val payload = freshTmp(Type.I64)
      fb.current.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, driven, index = 1))
      val exnPtr = castValue(payload, Type.Ptr, fb)
      fb.current.emitCallVoid("flix_exn_report_ptr", List(exnPtr))
      fb.current.setTerminator(Terminator.Ret(Type.I32, Value.IntConst(1L, Type.I32)))

      val suspCheckBlock = fb.newBlock(suspCheckLabel)
      fb.setCurrent(suspCheckBlock)
      val isSusp = freshTmp(Type.I1)
      fb.current.emitAssign(isSusp, Op.ICmp("eq", tag, Value.IntConst(ResultTagSuspension, Type.I64)))

      val suspLabel = freshLabel("main_susp")
      fb.current.setTerminator(Terminator.CondBr(isSusp, suspLabel, badLabel))

      val suspBlock = fb.newBlock(suspLabel)
      fb.setCurrent(suspBlock)
      val suspBits = freshTmp(Type.I64)
      fb.current.emitAssign(suspBits, Op.ExtractValue(Type.I64, flixResultType, driven, index = 1))
      val suspPtr = castValue(suspBits, Type.Ptr, fb)
      fb.current.emitCallVoid("flix_suspension_report_ptr", List(suspPtr))
      fb.current.setTerminator(Terminator.Ret(Type.I32, Value.IntConst(1L, Type.I32)))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      LlvmIr.Function("main", Type.I32, params, fb.result())
    }

    private def defaultValueFor(tpe: SimpleType): Value = llvmTypeOf(tpe) match {
      case Type.I1 => Value.IntConst(0L, Type.I1)
      case Type.I8 => Value.IntConst(0L, Type.I8)
      case Type.I16 => Value.IntConst(0L, Type.I16)
      case Type.I32 => Value.IntConst(0L, Type.I32)
      case Type.I64 => Value.IntConst(0L, Type.I64)
      case Type.Float => Value.Float32Const(0)
      case Type.Double => Value.Float64Const(0L)
      case Type.Ptr => Value.Null(Type.Ptr)
      case other => Value.Undef(other)
    }

    private def isHandleAbiType(tpe: SimpleType): Boolean = tpe match {
      case SimpleType.String => true
      case SimpleType.Array(SimpleType.Int8) => true
      // The current LLVM pipeline boxes most non-primitive values as `Object` (i64 payload bits).
      // For `@Export` we use handles for such values at the public ABI boundary.
      case SimpleType.Object => true
      case _ => false
    }

    private case class ExportHandle(handle: Value, owned: Boolean)
    private case class NativeImportPreparedArg(arg: Value, cleanupHandle: Option[Value])
    private case class WasmImportPreparedArg(arg: Value, cleanup: Option[(WasmImportInterface.Id, ExportAbi.AbiType, Value)])

    private def emitStoreExportOkValue(ctxPtr: Value, outPtr: Value, payload: Value, loweredTpe: SimpleType, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Unit = abiTpe match {
      case ExportAbi.AbiType.Unit =>
        ()

      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes =>
        val ptr = freshTmp(Type.Ptr)
        fb.current.emitAssign(ptr, Op.Cast("inttoptr", Type.Ptr, payload))
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, ptr)))
        fb.current.emitStore(handle, outPtr)

      case ExportAbi.AbiType.Bool | ExportAbi.AbiType.Int8 | ExportAbi.AbiType.Int16 |
           ExportAbi.AbiType.Int32 | ExportAbi.AbiType.Int64 | ExportAbi.AbiType.Float32 |
           ExportAbi.AbiType.Float64 =>
        val bits = exportUnboxValuePayload(payload, loweredTpe, fb)
        val value = unboxFromI64(bits, loweredTpe, fb)
        fb.current.emitStore(value, outPtr)

      case _ =>
        val ptr = freshTmp(Type.Ptr)
        fb.current.emitAssign(ptr, Op.Cast("inttoptr", Type.Ptr, payload))
        val topHandle = freshTmp(Type.I64)
        fb.current.emitAssign(topHandle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, ptr)))
        val value = emitDecodeExportHandle(ctxPtr, topHandle, abiTpe, fb)
        fb.current.emitStore(value, outPtr)
    }

    private def emitReleaseExportHandle(ctxPtr: Value, handle: Value, fb: FunBuilder): Unit =
      fb.current.emitCallVoid("flix_handle_release", List(ctxPtr, handle))

    private def prepareNativeImportArg(ctxPtr: Value, value: Value, loweredTpe: SimpleType, abiTpe: NativeImportAbi.AbiType, fb: FunBuilder): NativeImportPreparedArg = abiTpe match {
      case NativeImportAbi.AbiType.String | NativeImportAbi.AbiType.Bytes =>
        val ptr = llvmTypeOf(loweredTpe) match {
          case Type.Ptr => castValue(value, Type.Ptr, fb)
          case other =>
            fb.current.emitTrap()
            Value.Undef(other)
        }
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, ptr)))
        NativeImportPreparedArg(handle, Some(handle))

      case NativeImportAbi.AbiType.Portable(portableTpe) =>
        val encoded = emitEncodeExportAbiValue(ctxPtr, value, portableTpe, fb)
        NativeImportPreparedArg(encoded.handle, if (encoded.owned) Some(encoded.handle) else None)

      case _ =>
        NativeImportPreparedArg(castValue(value, nativeImportCallLlvmType(abiTpe), fb), None)
    }

    private def decodeNativeImportResult(ctxPtr: Value, loweredTpe: SimpleType, abiTpe: NativeImportAbi.AbiType, value: Value, fb: FunBuilder): Value = abiTpe match {
      case NativeImportAbi.AbiType.String | NativeImportAbi.AbiType.Bytes =>
        val handle = castValue(value, Type.I64, fb)
        val portableTpe =
          if (abiTpe == NativeImportAbi.AbiType.String) ExportAbi.AbiType.String
          else ExportAbi.AbiType.Bytes
        emitOwnedImportHandleToLoweredValue(ctxPtr, ExportHandle(handle, owned = true), loweredTpe, portableTpe, fb)

      case NativeImportAbi.AbiType.Portable(portableTpe) =>
        val handle = castValue(value, Type.I64, fb)
        emitOwnedImportHandleToLoweredValue(ctxPtr, ExportHandle(handle, owned = true), loweredTpe, portableTpe, fb)

      case _ =>
        value
    }

    private def emitEncodeExportAbiValue(ctxPtr: Value, value: Value, abiTpe: ExportAbi.AbiType, fb: FunBuilder): ExportHandle =
      emitEncodeExportAbiValue(ctxPtr, value, abiTpe, stringBytesOwned = false, fb)

    private def emitEncodeExportAbiValue(ctxPtr: Value, value: Value, abiTpe: ExportAbi.AbiType, stringBytesOwned: Boolean, fb: FunBuilder): ExportHandle = abiTpe match {
      case ExportAbi.AbiType.Unit =>
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new_i64", List(ctxPtr, Value.IntConst(0L, Type.I64))))
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes =>
        ExportHandle(castValue(value, Type.I64, fb), owned = stringBytesOwned)

      case leaf @ (ExportAbi.AbiType.Bool | ExportAbi.AbiType.Int8 | ExportAbi.AbiType.Int16 |
                   ExportAbi.AbiType.Int32 | ExportAbi.AbiType.Int64 | ExportAbi.AbiType.Float32 |
                   ExportAbi.AbiType.Float64) =>
        val bits = boxToI64(castValue(value, exportSurfaceTypeOf(leaf), fb), simpleTypeOfExportLeaf(leaf), fb)
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new_i64", List(ctxPtr, bits)))
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.List(elmTpe) =>
        val aggType = exportSurfaceTypeOf(abiTpe)
        val len = freshTmp(Type.I64)
        fb.current.emitAssign(len, Op.ExtractValue(Type.I64, aggType, value, 0))
        val elemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemsPtr, Op.ExtractValue(Type.Ptr, aggType, value, 1))

        val nilHandle = freshTmp(Type.I64)
        fb.current.emitAssign(nilHandle, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(portableListNilTagId, Type.I64), Value.Null(Type.Ptr), Value.IntConst(0L, Type.I32))))
        val currentSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(currentSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(nilHandle, currentSlot)

        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(len, idxSlot)

        val loopCheckLabel = freshLabel("export_list_encode_check")
        val loopBodyLabel = freshLabel("export_list_encode_body")
        val loopEndLabel = freshLabel("export_list_encode_end")
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopCheck = fb.newBlock(loopCheckLabel)
        fb.setCurrent(loopCheck)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("sgt", idx, Value.IntConst(0L, Type.I64)))
        fb.current.setTerminator(Terminator.CondBr(more, loopBodyLabel, loopEndLabel))

        val loopBody = fb.newBlock(loopBodyLabel)
        fb.setCurrent(loopBody)
        val idxPrev = freshTmp(Type.I64)
        fb.current.emitAssign(idxPrev, Op.Bin("sub", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxPrev, idxSlot)

        val currentHandle = freshTmp(Type.I64)
        fb.current.emitAssign(currentHandle, Op.Load(Type.I64, currentSlot))

        val elmValue = emitLoadSequenceElement(elemsPtr, idxPrev, elmTpe, fb)
        val elmHandle = emitEncodeExportAbiValue(ctxPtr, elmValue, elmTpe, stringBytesOwned, fb)
        val handlesPtr = emitTempI64Array(List(elmHandle.handle, currentHandle), fb)
        val nextHandle = freshTmp(Type.I64)
        fb.current.emitAssign(nextHandle, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(portableListConsTagId, Type.I64), handlesPtr, Value.IntConst(2L, Type.I32))))
        if (elmHandle.owned) emitReleaseExportHandle(ctxPtr, elmHandle.handle, fb)
        emitReleaseExportHandle(ctxPtr, currentHandle, fb)
        fb.current.emitStore(nextHandle, currentSlot)
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopEnd = fb.newBlock(loopEndLabel)
        fb.setCurrent(loopEnd)
        val finalHandle = freshTmp(Type.I64)
        fb.current.emitAssign(finalHandle, Op.Load(Type.I64, currentSlot))
        ExportHandle(finalHandle, owned = true)

      case ExportAbi.AbiType.Array(elmTpe) =>
        val aggType = exportSurfaceTypeOf(abiTpe)
        val len = freshTmp(Type.I64)
        fb.current.emitAssign(len, Op.ExtractValue(Type.I64, aggType, value, 0))
        val elemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemsPtr, Op.ExtractValue(Type.Ptr, aggType, value, 1))

        val handlesBuf = {
          val bytes = freshTmp(Type.I64)
          fb.current.emitAssign(bytes, Op.Bin("mul", Type.I64, len, Value.IntConst(8L, Type.I64)))
          emitMallocBytes(bytes, fb)
        }

        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), idxSlot)

        val fillCheckLabel = freshLabel("export_array_encode_fill_check")
        val fillBodyLabel = freshLabel("export_array_encode_fill_body")
        val fillEndLabel = freshLabel("export_array_encode_fill_end")
        fb.current.setTerminator(Terminator.Br(fillCheckLabel))

        val fillCheck = fb.newBlock(fillCheckLabel)
        fb.setCurrent(fillCheck)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("slt", idx, len))
        fb.current.setTerminator(Terminator.CondBr(more, fillBodyLabel, fillEndLabel))

        val fillBody = fb.newBlock(fillBodyLabel)
        fb.setCurrent(fillBody)
        val elmValue = emitLoadSequenceElement(elemsPtr, idx, elmTpe, fb)
        val elmHandle = emitEncodeExportAbiValue(ctxPtr, elmValue, elmTpe, stringBytesOwned, fb)
        val slotPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, handlesBuf, idx))
        fb.current.emitStore(elmHandle.handle, slotPtr)

        val idxNext = freshTmp(Type.I64)
        fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxNext, idxSlot)
        fb.current.setTerminator(Terminator.Br(fillCheckLabel))

        val fillEnd = fb.newBlock(fillEndLabel)
        fb.setCurrent(fillEnd)
        val arrHandle = freshTmp(Type.I64)
        fb.current.emitAssign(arrHandle, Op.Call(Type.I64, "flix_export_array_new", List(
          ctxPtr,
          Value.IntConst(if (elmTpe.isPointerLike) 1L else 0L, Type.I32),
          handlesBuf,
          castValue(len, Type.I32, fb)
        )))

        if (exportEncodingProducesOwnedHandle(elmTpe)) {
          val relIdxSlot = freshTmp(Type.Ptr)
          fb.current.emitAssign(relIdxSlot, Op.Alloca(Type.I64))
          fb.current.emitStore(Value.IntConst(0L, Type.I64), relIdxSlot)
          val relCheckLabel = freshLabel("export_array_encode_release_check")
          val relBodyLabel = freshLabel("export_array_encode_release_body")
          val relEndLabel = freshLabel("export_array_encode_release_end")
          fb.current.setTerminator(Terminator.Br(relCheckLabel))

          val relCheck = fb.newBlock(relCheckLabel)
          fb.setCurrent(relCheck)
          val relIdx = freshTmp(Type.I64)
          fb.current.emitAssign(relIdx, Op.Load(Type.I64, relIdxSlot))
          val relMore = freshTmp(Type.I1)
          fb.current.emitAssign(relMore, Op.ICmp("slt", relIdx, len))
          fb.current.setTerminator(Terminator.CondBr(relMore, relBodyLabel, relEndLabel))

          val relBody = fb.newBlock(relBodyLabel)
          fb.setCurrent(relBody)
          val relPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(relPtr, Op.Gep(Type.I64, handlesBuf, relIdx))
          val relHandle = freshTmp(Type.I64)
          fb.current.emitAssign(relHandle, Op.Load(Type.I64, relPtr))
          emitReleaseExportHandle(ctxPtr, relHandle, fb)
          val relIdxNext = freshTmp(Type.I64)
          fb.current.emitAssign(relIdxNext, Op.Bin("add", Type.I64, relIdx, Value.IntConst(1L, Type.I64)))
          fb.current.emitStore(relIdxNext, relIdxSlot)
          fb.current.setTerminator(Terminator.Br(relCheckLabel))

          val relEnd = fb.newBlock(relEndLabel)
          fb.setCurrent(relEnd)
        }

        emitFreePtr(handlesBuf, fb)
        ExportHandle(arrHandle, owned = true)

      case ExportAbi.AbiType.Tuple(elms) =>
        val aggType = exportSurfaceTypeOf(abiTpe)
        val childHandles = elms.zipWithIndex.map {
          case (elmTpe, idx) =>
            val field = freshTmp(exportAggregateFieldTypeOf(elmTpe))
            fb.current.emitAssign(field, Op.ExtractValue(exportAggregateFieldTypeOf(elmTpe), aggType, value, idx))
            emitEncodeExportAbiValue(ctxPtr, field, elmTpe, stringBytesOwned, fb)
        }
        val handlesPtr = emitTempI64Array(childHandles.map(_.handle), fb)
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_export_tuple_new", List(ctxPtr, handlesPtr, Value.IntConst(elms.length.toLong, Type.I32))))
        childHandles.foreach {
          case ExportHandle(h, true) => emitReleaseExportHandle(ctxPtr, h, fb)
          case _ => ()
        }
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Record(fields) =>
        val aggType = exportSurfaceTypeOf(abiTpe)
        val childHandles = fields.zipWithIndex.map {
          case ((_, fieldTpe), idx) =>
            val field = freshTmp(exportAggregateFieldTypeOf(fieldTpe))
            fb.current.emitAssign(field, Op.ExtractValue(exportAggregateFieldTypeOf(fieldTpe), aggType, value, idx))
            emitEncodeExportAbiValue(ctxPtr, field, fieldTpe, stringBytesOwned, fb)
        }
        val handlesPtr = emitTempI64Array(childHandles.map(_.handle), fb)
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_export_tuple_new", List(ctxPtr, handlesPtr, Value.IntConst(fields.length.toLong, Type.I32))))
        childHandles.foreach {
          case ExportHandle(h, true) => emitReleaseExportHandle(ctxPtr, h, fb)
          case _ => ()
        }
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Option(elmTpe) =>
        val aggType = exportSurfaceTypeOf(abiTpe)
        val rawIsSome = freshTmp(Type.I8)
        fb.current.emitAssign(rawIsSome, Op.ExtractValue(Type.I8, aggType, value, 0))
        val isSome = freshTmp(Type.I1)
        fb.current.emitAssign(isSome, Op.ICmp("ne", rawIsSome, Value.IntConst(0L, Type.I8)))
        val someLabel = freshLabel("export_opt_some")
        val noneLabel = freshLabel("export_opt_none")
        val endLabel = freshLabel("export_opt_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isSome, someLabel, noneLabel))

        val someBlock = fb.newBlock(someLabel)
        fb.setCurrent(someBlock)
        val payload = freshTmp(exportAggregateFieldTypeOf(elmTpe))
        fb.current.emitAssign(payload, Op.ExtractValue(exportAggregateFieldTypeOf(elmTpe), aggType, value, 1))
        val someHandle = emitEncodeExportAbiValue(ctxPtr, payload, elmTpe, stringBytesOwned, fb)
        val someArrayPtr = emitTempI64Array(List(someHandle.handle), fb)
        val someTagged = freshTmp(Type.I64)
        fb.current.emitAssign(someTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(1L, Type.I64), someArrayPtr, Value.IntConst(1L, Type.I32))))
        if (someHandle.owned) emitReleaseExportHandle(ctxPtr, someHandle.handle, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((someTagged, someLabel))

        val noneBlock = fb.newBlock(noneLabel)
        fb.setCurrent(noneBlock)
        val noneTagged = freshTmp(Type.I64)
        fb.current.emitAssign(noneTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(0L, Type.I64), Value.Null(Type.Ptr), Value.IntConst(0L, Type.I32))))
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((noneTagged, noneLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val handle = freshTmp(Type.I64)
        endBlock.emitPhi(handle, incomings.toList)
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Result(okTpe, errTpe) =>
        val aggType = exportSurfaceTypeOf(abiTpe)
        val rawIsOk = freshTmp(Type.I8)
        fb.current.emitAssign(rawIsOk, Op.ExtractValue(Type.I8, aggType, value, 0))
        val isOk = freshTmp(Type.I1)
        fb.current.emitAssign(isOk, Op.ICmp("ne", rawIsOk, Value.IntConst(0L, Type.I8)))
        val okLabel = freshLabel("export_result_ok")
        val errLabel = freshLabel("export_result_err")
        val endLabel = freshLabel("export_result_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isOk, okLabel, errLabel))

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)
        val okValue = freshTmp(exportAggregateFieldTypeOf(okTpe))
        fb.current.emitAssign(okValue, Op.ExtractValue(exportAggregateFieldTypeOf(okTpe), aggType, value, 1))
        val okHandle = emitEncodeExportAbiValue(ctxPtr, okValue, okTpe, stringBytesOwned, fb)
        val okArrayPtr = emitTempI64Array(List(okHandle.handle), fb)
        val okTagged = freshTmp(Type.I64)
        fb.current.emitAssign(okTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(1L, Type.I64), okArrayPtr, Value.IntConst(1L, Type.I32))))
        if (okHandle.owned) emitReleaseExportHandle(ctxPtr, okHandle.handle, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((okTagged, okLabel))

        val errBlock = fb.newBlock(errLabel)
        fb.setCurrent(errBlock)
        val errValue = freshTmp(exportAggregateFieldTypeOf(errTpe))
        fb.current.emitAssign(errValue, Op.ExtractValue(exportAggregateFieldTypeOf(errTpe), aggType, value, 2))
        val errHandle = emitEncodeExportAbiValue(ctxPtr, errValue, errTpe, stringBytesOwned, fb)
        val errArrayPtr = emitTempI64Array(List(errHandle.handle), fb)
        val errTagged = freshTmp(Type.I64)
        fb.current.emitAssign(errTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(0L, Type.I64), errArrayPtr, Value.IntConst(1L, Type.I32))))
        if (errHandle.owned) emitReleaseExportHandle(ctxPtr, errHandle.handle, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((errTagged, errLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val handle = freshTmp(Type.I64)
        endBlock.emitPhi(handle, incomings.toList)
        ExportHandle(handle, owned = true)
    }

    private def emitExportTagFieldHandle(ctxPtr: Value, taggedHandle: Value, idx: Long, fieldTpe: ExportAbi.AbiType, fb: FunBuilder): Value = {
      val helperName = if (fieldTpe.isPointerLike) "flix_export_tag_field_ptr" else "flix_export_tag_field_i64"
      val out = freshTmp(Type.I64)
      fb.current.emitAssign(out, Op.Call(Type.I64, helperName, List(ctxPtr, taggedHandle, Value.IntConst(idx, Type.I32))))
      out
    }

    private def emitDecodeExportHandle(ctxPtr: Value, handle: Value, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value = abiTpe match {
      case ExportAbi.AbiType.Unit =>
        Value.IntConst(0L, Type.I64)

      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes =>
        handle

      case leaf @ (ExportAbi.AbiType.Bool | ExportAbi.AbiType.Int8 | ExportAbi.AbiType.Int16 |
                   ExportAbi.AbiType.Int32 | ExportAbi.AbiType.Int64 | ExportAbi.AbiType.Float32 |
                   ExportAbi.AbiType.Float64) =>
        val bits = freshTmp(Type.I64)
        fb.current.emitAssign(bits, Op.Call(Type.I64, "flix_handle_unbox_i64", List(ctxPtr, handle)))
        emitReleaseExportHandle(ctxPtr, handle, fb)
        unboxFromI64(bits, simpleTypeOfExportLeaf(leaf), fb)

      case ExportAbi.AbiType.List(elmTpe) =>
        val len = freshTmp(Type.I64)
        fb.current.emitAssign(len, Op.Call(Type.I64, "flix_export_list_length", List(ctxPtr, handle, Value.IntConst(portableListNilTagId, Type.I64), Value.IntConst(portableListConsTagId, Type.I64))))

        val elemsBuf = {
          val elemBytes = emitSizeOfType(exportAggregateFieldTypeOf(elmTpe), fb)
          val totalBytes = freshTmp(Type.I64)
          fb.current.emitAssign(totalBytes, Op.Bin("mul", Type.I64, len, elemBytes))
          emitMallocBytes(totalBytes, fb)
        }

        val currentSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(currentSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(handle, currentSlot)

        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), idxSlot)

        val loopCheckLabel = freshLabel("decode_list_check")
        val loopBodyLabel = freshLabel("decode_list_body")
        val loopEndLabel = freshLabel("decode_list_end")
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopCheck = fb.newBlock(loopCheckLabel)
        fb.setCurrent(loopCheck)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("slt", idx, len))
        fb.current.setTerminator(Terminator.CondBr(more, loopBodyLabel, loopEndLabel))

        val loopBody = fb.newBlock(loopBodyLabel)
        fb.setCurrent(loopBody)
        val currentHandle = freshTmp(Type.I64)
        fb.current.emitAssign(currentHandle, Op.Load(Type.I64, currentSlot))
        val headHandle = emitExportTagFieldHandle(ctxPtr, currentHandle, 0L, elmTpe, fb)
        val tailHandle = emitExportTagFieldHandle(ctxPtr, currentHandle, 1L, abiTpe, fb)
        val elmValue = emitDecodeExportHandle(ctxPtr, headHandle, elmTpe, fb)
        emitStoreSequenceElement(elemsBuf, idx, elmTpe, elmValue, fb)
        emitReleaseExportHandle(ctxPtr, currentHandle, fb)
        fb.current.emitStore(tailHandle, currentSlot)
        val idxNext = freshTmp(Type.I64)
        fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxNext, idxSlot)
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopEnd = fb.newBlock(loopEndLabel)
        fb.setCurrent(loopEnd)
        val finalHandle = freshTmp(Type.I64)
        fb.current.emitAssign(finalHandle, Op.Load(Type.I64, currentSlot))
        emitReleaseExportHandle(ctxPtr, finalHandle, fb)
        buildExportStructValue(abiTpe, List(len, elemsBuf), fb)

      case ExportAbi.AbiType.Array(elmTpe) =>
        val len = freshTmp(Type.I64)
        fb.current.emitAssign(len, Op.Call(Type.I64, "flix_export_array_length", List(ctxPtr, handle)))

        val elemsBuf = {
          val elemBytes = emitSizeOfType(exportAggregateFieldTypeOf(elmTpe), fb)
          val totalBytes = freshTmp(Type.I64)
          fb.current.emitAssign(totalBytes, Op.Bin("mul", Type.I64, len, elemBytes))
          emitMallocBytes(totalBytes, fb)
        }

        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), idxSlot)

        val loopCheckLabel = freshLabel("decode_array_check")
        val loopBodyLabel = freshLabel("decode_array_body")
        val loopEndLabel = freshLabel("decode_array_end")
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopCheck = fb.newBlock(loopCheckLabel)
        fb.setCurrent(loopCheck)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("slt", idx, len))
        fb.current.setTerminator(Terminator.CondBr(more, loopBodyLabel, loopEndLabel))

        val loopBody = fb.newBlock(loopBodyLabel)
        fb.setCurrent(loopBody)
        val elmHandle = freshTmp(Type.I64)
        fb.current.emitAssign(elmHandle, Op.Call(Type.I64, "flix_export_array_element", List(ctxPtr, handle, castValue(idx, Type.I32, fb))))
        val elmValue = emitDecodeExportHandle(ctxPtr, elmHandle, elmTpe, fb)
        emitStoreSequenceElement(elemsBuf, idx, elmTpe, elmValue, fb)
        val idxNext = freshTmp(Type.I64)
        fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxNext, idxSlot)
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopEnd = fb.newBlock(loopEndLabel)
        fb.setCurrent(loopEnd)
        emitReleaseExportHandle(ctxPtr, handle, fb)
        buildExportStructValue(abiTpe, List(len, elemsBuf), fb)

      case ExportAbi.AbiType.Tuple(elms) =>
        val fields = elms.zipWithIndex.map {
          case (elmTpe, idx) =>
            val fieldHandle = freshTmp(Type.I64)
            fb.current.emitAssign(fieldHandle, Op.Call(Type.I64, "flix_export_tuple_field", List(ctxPtr, handle, Value.IntConst(idx.toLong, Type.I32))))
            emitDecodeExportHandle(ctxPtr, fieldHandle, elmTpe, fb)
        }
        emitReleaseExportHandle(ctxPtr, handle, fb)
        buildExportStructValue(abiTpe, fields, fb)

      case ExportAbi.AbiType.Record(fields) =>
        val values = fields.zipWithIndex.map {
          case ((_, fieldTpe), idx) =>
            val fieldHandle = freshTmp(Type.I64)
            fb.current.emitAssign(fieldHandle, Op.Call(Type.I64, "flix_export_tuple_field", List(ctxPtr, handle, Value.IntConst(idx.toLong, Type.I32))))
            emitDecodeExportHandle(ctxPtr, fieldHandle, fieldTpe, fb)
        }
        emitReleaseExportHandle(ctxPtr, handle, fb)
        buildExportStructValue(abiTpe, values, fb)

      case ExportAbi.AbiType.Option(elmTpe) =>
        val tagId = freshTmp(Type.I64)
        fb.current.emitAssign(tagId, Op.Call(Type.I64, "flix_export_tag_id", List(ctxPtr, handle)))
        val isSome = freshTmp(Type.I1)
        fb.current.emitAssign(isSome, Op.ICmp("eq", tagId, Value.IntConst(1L, Type.I64)))
        val someLabel = freshLabel("decode_opt_some")
        val noneLabel = freshLabel("decode_opt_none")
        val endLabel = freshLabel("decode_opt_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isSome, someLabel, noneLabel))

        val someBlock = fb.newBlock(someLabel)
        fb.setCurrent(someBlock)
        val someHandle = emitExportTagFieldHandle(ctxPtr, handle, 0L, elmTpe, fb)
        val someValue = emitDecodeExportHandle(ctxPtr, someHandle, elmTpe, fb)
        emitReleaseExportHandle(ctxPtr, handle, fb)
        val someStruct = buildExportStructValue(abiTpe, List(Value.IntConst(1L, Type.I8), someValue), fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((someStruct, someLabel))

        val noneBlock = fb.newBlock(noneLabel)
        fb.setCurrent(noneBlock)
        emitReleaseExportHandle(ctxPtr, handle, fb)
        val noneStruct = buildExportStructValue(abiTpe, List(Value.IntConst(0L, Type.I8), zeroExportAggregateFieldValue(elmTpe)), fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((noneStruct, noneLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val out = freshTmp(exportSurfaceTypeOf(abiTpe))
        endBlock.emitPhi(out, incomings.toList)
        out

      case ExportAbi.AbiType.Result(okTpe, errTpe) =>
        val tagId = freshTmp(Type.I64)
        fb.current.emitAssign(tagId, Op.Call(Type.I64, "flix_export_tag_id", List(ctxPtr, handle)))
        val isOk = freshTmp(Type.I1)
        fb.current.emitAssign(isOk, Op.ICmp("eq", tagId, Value.IntConst(1L, Type.I64)))
        val okLabel = freshLabel("decode_result_ok")
        val errLabel = freshLabel("decode_result_err")
        val endLabel = freshLabel("decode_result_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isOk, okLabel, errLabel))

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)
        val okFieldHandle = emitExportTagFieldHandle(ctxPtr, handle, 0L, okTpe, fb)
        val okValue = emitDecodeExportHandle(ctxPtr, okFieldHandle, okTpe, fb)
        emitReleaseExportHandle(ctxPtr, handle, fb)
        val okStruct = buildExportStructValue(abiTpe, List(Value.IntConst(1L, Type.I8), okValue, zeroExportAggregateFieldValue(errTpe)), fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((okStruct, okLabel))

        val errBlock = fb.newBlock(errLabel)
        fb.setCurrent(errBlock)
        val errFieldHandle = emitExportTagFieldHandle(ctxPtr, handle, 0L, errTpe, fb)
        val errValue = emitDecodeExportHandle(ctxPtr, errFieldHandle, errTpe, fb)
        emitReleaseExportHandle(ctxPtr, handle, fb)
        val errStruct = buildExportStructValue(abiTpe, List(Value.IntConst(0L, Type.I8), zeroExportAggregateFieldValue(okTpe), errValue), fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((errStruct, errLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val out = freshTmp(exportSurfaceTypeOf(abiTpe))
        endBlock.emitPhi(out, incomings.toList)
        out
    }

    private def emitTempI64Array(values: List[Value], fb: FunBuilder): Value =
      if (values.isEmpty) Value.Null(Type.Ptr)
      else {
        val storageTy = Type.Struct(List.fill(values.length)(Type.I64))
        val storagePtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(storagePtr, Op.Alloca(storageTy))
        values.zipWithIndex.foreach {
          case (value, idx) =>
            val slotPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, storagePtr, Value.IntConst(idx.toLong, Type.I64)))
            fb.current.emitStore(value, slotPtr)
        }
        storagePtr
      }

    private def emitTempPtrArray(values: List[Value], fb: FunBuilder): Value =
      if (values.isEmpty) Value.Null(Type.Ptr)
      else {
        val storageTy = Type.Struct(List.fill(values.length)(Type.Ptr))
        val storagePtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(storagePtr, Op.Alloca(storageTy))
        values.zipWithIndex.foreach {
          case (value, idx) =>
            val slotPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(slotPtr, Op.Gep(Type.Ptr, storagePtr, Value.IntConst(idx.toLong, Type.I64)))
            fb.current.emitStore(castValue(value, Type.Ptr, fb), slotPtr)
        }
        storagePtr
      }

    private def emitMallocBytes(sizeBytes0: Value, fb: FunBuilder): Value = {
      val sizeBytes = castValue(sizeBytes0, Type.I64, fb)
      val mallocSize = castValue(sizeBytes, if (target == CompilationTarget.LlvmWasm) Type.I32 else Type.I64, fb)
      val ptr = freshTmp(Type.Ptr)
      fb.current.emitAssign(ptr, Op.Call(Type.Ptr, "malloc", List(mallocSize)))
      ptr
    }

    private def emitFreePtr(ptr0: Value, fb: FunBuilder): Unit = {
      val ptr = castValue(ptr0, Type.Ptr, fb)
      fb.current.emitCallVoid("free", List(ptr))
    }

    private def emitSizeOfType(tpe: Type, fb: FunBuilder): Value = {
      val nextPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(nextPtr, Op.Gep(tpe, Value.Null(Type.Ptr), Value.IntConst(1L, Type.I64)))
      val sizeBytes = freshTmp(Type.I64)
      fb.current.emitAssign(sizeBytes, Op.Cast("ptrtoint", Type.I64, nextPtr))
      sizeBytes
    }

    private def emitLoadSequenceElement(seqPtr0: Value, idx0: Value, elmTpe: ExportAbi.AbiType, fb: FunBuilder): Value = {
      val seqPtr = castValue(seqPtr0, Type.Ptr, fb)
      val idx = castValue(idx0, Type.I64, fb)
      val fieldTpe = exportAggregateFieldTypeOf(elmTpe)
      val elemPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(elemPtr, Op.Gep(fieldTpe, seqPtr, idx))
      val value = freshTmp(fieldTpe)
      fb.current.emitAssign(value, Op.Load(fieldTpe, elemPtr))
      value
    }

    private def emitStoreSequenceElement(seqPtr0: Value, idx0: Value, elmTpe: ExportAbi.AbiType, value0: Value, fb: FunBuilder): Unit = {
      val seqPtr = castValue(seqPtr0, Type.Ptr, fb)
      val idx = castValue(idx0, Type.I64, fb)
      val fieldTpe = exportAggregateFieldTypeOf(elmTpe)
      val elemPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(elemPtr, Op.Gep(fieldTpe, seqPtr, idx))
      fb.current.emitStore(castValue(value0, fieldTpe, fb), elemPtr)
    }

    private def buildExportStructValue(abiTpe: ExportAbi.AbiType, values: List[Value], fb: FunBuilder): Value = {
      val aggTpe = exportSurfaceTypeOf(abiTpe)
      val fieldTypes = exportAggregateFieldTypesOf(abiTpe)
      values.zip(fieldTypes).zipWithIndex.foldLeft(Value.Undef(aggTpe): Value) {
        case (agg, ((value, fieldTpe), idx)) =>
          val coerced = castValue(value, fieldTpe, fb)
          val next = freshTmp(aggTpe)
          fb.current.emitAssign(next, Op.InsertValue(aggTpe, agg, coerced, idx))
          next
      }
    }

    private def buildRequestStructValue(sig: ExportAbi.Signature, values: List[Value], fb: FunBuilder): Value = {
      val requestTpe = requestSurfaceTypeOf(sig)
      val fieldTypes =
        if (sig.params.isEmpty) List(Type.I8)
        else sig.params.map(requestFieldSurfaceTypeOf)
      values.zip(fieldTypes).zipWithIndex.foldLeft(Value.Undef(requestTpe): Value) {
        case (agg, ((value, fieldTpe), idx)) =>
          val coerced = castValue(value, fieldTpe, fb)
          val next = freshTmp(requestTpe)
          fb.current.emitAssign(next, Op.InsertValue(requestTpe, agg, coerced, idx))
          next
      }
    }

    private def declareWasmImportFunction(entry: LlvmWasmImportsWriter.Entry): List[Decl] = {
      val params = entry.signature.params.map(wasmImportParamSurfaceTypeOf)
      val fnDecl =
        if (WasmImportAbi.isByRefBoundaryType(entry.signature.result)) {
          Decl.DeclareFun(Type.Void, entry.interfaceId.cFunctionName(entry.spec.func), params :+ Type.Ptr)
        } else {
          Decl.DeclareFun(wasmImportScalarLlvmType(entry.signature.result), entry.interfaceId.cFunctionName(entry.spec.func), params)
        }
      val freeDecls = (entry.signature.params :+ entry.signature.result).distinct.flatMap { tpe =>
        wasmImportFreeHelperName(entry.interfaceId, tpe).map(name => Decl.DeclareFun(Type.Void, name, List(Type.Ptr)))
      }
      fnDecl :: freeDecls
    }

    private def emitWasmImportArgument(ctxPtr: Value, rawValue: Value, loweredTpe: SimpleType, abiTpe: ExportAbi.AbiType, interfaceId: WasmImportInterface.Id, fb: FunBuilder): WasmImportPreparedArg =
      if (WasmImportAbi.isByRefBoundaryType(abiTpe)) {
        val surface = emitLoweredValueToWasmImportSurface(ctxPtr, rawValue, loweredTpe, abiTpe, fb)
        val ptr = freshTmp(Type.Ptr)
        fb.current.emitAssign(ptr, Op.Alloca(wasmImportSurfaceTypeOf(abiTpe)))
        fb.current.emitStore(surface, ptr)
        WasmImportPreparedArg(ptr, Some((interfaceId, abiTpe, ptr)))
      } else {
        WasmImportPreparedArg(castValue(rawValue, wasmImportScalarLlvmType(abiTpe), fb), None)
      }

    private def emitWasmImportResult(ctxPtr: Value, value: Value, loweredTpe: SimpleType, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value =
      abiTpe match {
        case ExportAbi.AbiType.Unit =>
          emitConstant(Constant.Unit, ctxPtr, fb)
        case _ if !WasmImportAbi.isByRefBoundaryType(abiTpe) =>
          castValue(value, llvmTypeOf(loweredTpe), fb)
        case _ =>
          val handle = emitEncodeWasmImportAbiValue(ctxPtr, value, abiTpe, fb)
          emitOwnedImportHandleToLoweredValue(ctxPtr, handle, loweredTpe, abiTpe, fb)
      }

    private def emitLoweredValueToWasmImportSurface(ctxPtr: Value, rawValue: Value, loweredTpe: SimpleType, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value = {
      val exportValue = emitLoweredValueToExportSurface(ctxPtr, rawValue, loweredTpe, abiTpe, fb)
      emitExportSurfaceToWasmImportSurface(ctxPtr, exportValue, abiTpe, fb)
    }

    private def emitLoweredValueToExportSurface(ctxPtr: Value, rawValue: Value, loweredTpe: SimpleType, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value = abiTpe match {
      case ExportAbi.AbiType.Unit =>
        Value.IntConst(0L, exportSurfaceTypeOf(abiTpe))

      case ExportAbi.AbiType.Bool | ExportAbi.AbiType.Int8 | ExportAbi.AbiType.Int16 |
           ExportAbi.AbiType.Int32 | ExportAbi.AbiType.Int64 | ExportAbi.AbiType.Float32 |
           ExportAbi.AbiType.Float64 =>
        castValue(rawValue, exportSurfaceTypeOf(abiTpe), fb)

      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes =>
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, castValue(rawValue, Type.Ptr, fb))))
        handle

      case _ =>
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new", List(ctxPtr, castValue(rawValue, Type.Ptr, fb))))
        emitDecodeExportHandle(ctxPtr, handle, abiTpe, fb)
    }

    private def emitExportSurfaceToWasmImportSurface(ctxPtr: Value, value: Value, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value = abiTpe match {
      case ExportAbi.AbiType.Unit =>
        Value.Undef(wasmImportSurfaceTypeOf(abiTpe))

      case ExportAbi.AbiType.Bool | ExportAbi.AbiType.Int8 | ExportAbi.AbiType.Int16 |
           ExportAbi.AbiType.Int32 | ExportAbi.AbiType.Int64 | ExportAbi.AbiType.Float32 |
           ExportAbi.AbiType.Float64 =>
        castValue(value, wasmImportSurfaceTypeOf(abiTpe), fb)

      case ExportAbi.AbiType.String =>
        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Alloca(Type.I64))
        val bytesPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(bytesPtr, Op.Call(Type.Ptr, "flix_string_to_utf8", List(ctxPtr, castValue(value, Type.I64, fb), lenPtr)))
        val len64 = freshTmp(Type.I64)
        fb.current.emitAssign(len64, Op.Load(Type.I64, lenPtr))
        emitReleaseExportHandle(ctxPtr, castValue(value, Type.I64, fb), fb)
        buildWasmImportStructValue(abiTpe, List(bytesPtr, castValue(len64, Type.I32, fb)), fb)

      case ExportAbi.AbiType.Bytes =>
        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Alloca(Type.I64))
        val bytesPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(bytesPtr, Op.Call(Type.Ptr, "flix_i8_array_to_bytes", List(ctxPtr, castValue(value, Type.I64, fb), lenPtr)))
        val len64 = freshTmp(Type.I64)
        fb.current.emitAssign(len64, Op.Load(Type.I64, lenPtr))
        emitReleaseExportHandle(ctxPtr, castValue(value, Type.I64, fb), fb)
        buildWasmImportStructValue(abiTpe, List(bytesPtr, castValue(len64, Type.I32, fb)), fb)

      case ExportAbi.AbiType.List(elmTpe) =>
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val len64 = freshTmp(Type.I64)
        fb.current.emitAssign(len64, Op.ExtractValue(Type.I64, aggTpe, value, 0))
        val exportElemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(exportElemsPtr, Op.ExtractValue(Type.Ptr, aggTpe, value, 1))
        val wasmElemsPtr = emitAllocWasmImportSequenceBuffer(len64, elmTpe, fb)
        emitConvertExportSequenceToWasm(ctxPtr, exportElemsPtr, wasmElemsPtr, len64, elmTpe, fb)
        emitFreePtr(exportElemsPtr, fb)
        buildWasmImportStructValue(abiTpe, List(wasmElemsPtr, castValue(len64, Type.I32, fb)), fb)

      case ExportAbi.AbiType.Array(elmTpe) =>
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val len64 = freshTmp(Type.I64)
        fb.current.emitAssign(len64, Op.ExtractValue(Type.I64, aggTpe, value, 0))
        val exportElemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(exportElemsPtr, Op.ExtractValue(Type.Ptr, aggTpe, value, 1))
        val wasmElemsPtr = emitAllocWasmImportSequenceBuffer(len64, elmTpe, fb)
        emitConvertExportSequenceToWasm(ctxPtr, exportElemsPtr, wasmElemsPtr, len64, elmTpe, fb)
        emitFreePtr(exportElemsPtr, fb)
        buildWasmImportStructValue(abiTpe, List(wasmElemsPtr, castValue(len64, Type.I32, fb)), fb)

      case ExportAbi.AbiType.Tuple(elms) =>
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val fields = elms.zipWithIndex.map {
          case (elmTpe, idx) =>
            val field = freshTmp(exportAggregateFieldTypeOf(elmTpe))
            fb.current.emitAssign(field, Op.ExtractValue(exportAggregateFieldTypeOf(elmTpe), aggTpe, value, idx))
            emitExportSurfaceToWasmImportField(ctxPtr, field, elmTpe, fb)
        }
        buildWasmImportStructValue(abiTpe, fields, fb)

      case ExportAbi.AbiType.Record(fields) =>
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val values = fields.zipWithIndex.map {
          case ((_, fieldTpe), idx) =>
            val field = freshTmp(exportAggregateFieldTypeOf(fieldTpe))
            fb.current.emitAssign(field, Op.ExtractValue(exportAggregateFieldTypeOf(fieldTpe), aggTpe, value, idx))
            emitExportSurfaceToWasmImportField(ctxPtr, field, fieldTpe, fb)
        }
        buildWasmImportStructValue(abiTpe, values, fb)

      case ExportAbi.AbiType.Option(elmTpe) =>
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val rawTag = freshTmp(Type.I8)
        fb.current.emitAssign(rawTag, Op.ExtractValue(Type.I8, aggTpe, value, 0))
        val payload = freshTmp(exportAggregateFieldTypeOf(elmTpe))
        fb.current.emitAssign(payload, Op.ExtractValue(exportAggregateFieldTypeOf(elmTpe), aggTpe, value, 1))
        buildWasmImportStructValue(abiTpe, List(rawTag, emitExportSurfaceToWasmImportField(ctxPtr, payload, elmTpe, fb)), fb)

      case ExportAbi.AbiType.Result(okTpe, errTpe) =>
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val rawTag = freshTmp(Type.I8)
        fb.current.emitAssign(rawTag, Op.ExtractValue(Type.I8, aggTpe, value, 0))
        val okField = freshTmp(exportAggregateFieldTypeOf(okTpe))
        fb.current.emitAssign(okField, Op.ExtractValue(exportAggregateFieldTypeOf(okTpe), aggTpe, value, 1))
        val errField = freshTmp(exportAggregateFieldTypeOf(errTpe))
        fb.current.emitAssign(errField, Op.ExtractValue(exportAggregateFieldTypeOf(errTpe), aggTpe, value, 2))
        buildWasmImportStructValue(abiTpe, List(rawTag, emitExportSurfaceToWasmImportField(ctxPtr, okField, okTpe, fb), emitExportSurfaceToWasmImportField(ctxPtr, errField, errTpe, fb)), fb)
    }

    private def emitExportSurfaceToWasmImportField(ctxPtr: Value, value: Value, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value =
      castValue(emitExportSurfaceToWasmImportSurface(ctxPtr, value, abiTpe, fb), wasmImportAggregateFieldTypeOf(abiTpe), fb)

    private def emitEncodeWasmImportAbiValue(ctxPtr: Value, value: Value, abiTpe: ExportAbi.AbiType, fb: FunBuilder): ExportHandle = abiTpe match {
      case ExportAbi.AbiType.Unit =>
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new_i64", List(ctxPtr, Value.IntConst(0L, Type.I64))))
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Bool | ExportAbi.AbiType.Int8 | ExportAbi.AbiType.Int16 |
           ExportAbi.AbiType.Int32 | ExportAbi.AbiType.Int64 | ExportAbi.AbiType.Float32 |
           ExportAbi.AbiType.Float64 =>
        val bits = boxToI64(castValue(value, wasmImportScalarLlvmType(abiTpe), fb), simpleTypeOfExportLeaf(abiTpe), fb)
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_handle_new_i64", List(ctxPtr, bits)))
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.String =>
        val seqTpe = wasmImportSurfaceTypeOf(abiTpe)
        val bytesPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(bytesPtr, Op.ExtractValue(Type.Ptr, seqTpe, value, 0))
        val len32 = freshTmp(Type.I32)
        fb.current.emitAssign(len32, Op.ExtractValue(Type.I32, seqTpe, value, 1))
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_string_from_utf8", List(ctxPtr, bytesPtr, castValue(len32, Type.I64, fb))))
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Bytes =>
        val seqTpe = wasmImportSurfaceTypeOf(abiTpe)
        val bytesPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(bytesPtr, Op.ExtractValue(Type.Ptr, seqTpe, value, 0))
        val len32 = freshTmp(Type.I32)
        fb.current.emitAssign(len32, Op.ExtractValue(Type.I32, seqTpe, value, 1))
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_i8_array_from_bytes", List(ctxPtr, bytesPtr, castValue(len32, Type.I64, fb))))
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.List(elmTpe) =>
        val aggType = wasmImportSurfaceTypeOf(abiTpe)
        val elemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemsPtr, Op.ExtractValue(Type.Ptr, aggType, value, 0))
        val len32 = freshTmp(Type.I32)
        fb.current.emitAssign(len32, Op.ExtractValue(Type.I32, aggType, value, 1))
        val len64 = castValue(len32, Type.I64, fb)

        val nilHandle = freshTmp(Type.I64)
        fb.current.emitAssign(nilHandle, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(portableListNilTagId, Type.I64), Value.Null(Type.Ptr), Value.IntConst(0L, Type.I32))))
        val currentSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(currentSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(nilHandle, currentSlot)

        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(len64, idxSlot)

        val loopCheckLabel = freshLabel("wasm_import_list_encode_check")
        val loopBodyLabel = freshLabel("wasm_import_list_encode_body")
        val loopEndLabel = freshLabel("wasm_import_list_encode_end")
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopCheck = fb.newBlock(loopCheckLabel)
        fb.setCurrent(loopCheck)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("sgt", idx, Value.IntConst(0L, Type.I64)))
        fb.current.setTerminator(Terminator.CondBr(more, loopBodyLabel, loopEndLabel))

        val loopBody = fb.newBlock(loopBodyLabel)
        fb.setCurrent(loopBody)
        val idxPrev = freshTmp(Type.I64)
        fb.current.emitAssign(idxPrev, Op.Bin("sub", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxPrev, idxSlot)

        val currentHandle = freshTmp(Type.I64)
        fb.current.emitAssign(currentHandle, Op.Load(Type.I64, currentSlot))

        val elmValue = emitLoadWasmImportSequenceElement(elemsPtr, idxPrev, elmTpe, fb)
        val elmHandle = emitEncodeWasmImportAbiValue(ctxPtr, elmValue, elmTpe, fb)
        val handlesPtr = emitTempI64Array(List(elmHandle.handle, currentHandle), fb)
        val nextHandle = freshTmp(Type.I64)
        fb.current.emitAssign(nextHandle, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(portableListConsTagId, Type.I64), handlesPtr, Value.IntConst(2L, Type.I32))))
        if (elmHandle.owned) emitReleaseExportHandle(ctxPtr, elmHandle.handle, fb)
        emitReleaseExportHandle(ctxPtr, currentHandle, fb)
        fb.current.emitStore(nextHandle, currentSlot)
        fb.current.setTerminator(Terminator.Br(loopCheckLabel))

        val loopEnd = fb.newBlock(loopEndLabel)
        fb.setCurrent(loopEnd)
        val finalHandle = freshTmp(Type.I64)
        fb.current.emitAssign(finalHandle, Op.Load(Type.I64, currentSlot))
        ExportHandle(finalHandle, owned = true)

      case ExportAbi.AbiType.Array(elmTpe) =>
        val aggType = wasmImportSurfaceTypeOf(abiTpe)
        val elemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemsPtr, Op.ExtractValue(Type.Ptr, aggType, value, 0))
        val len32 = freshTmp(Type.I32)
        fb.current.emitAssign(len32, Op.ExtractValue(Type.I32, aggType, value, 1))
        val len64 = castValue(len32, Type.I64, fb)

        val bytes = freshTmp(Type.I64)
        fb.current.emitAssign(bytes, Op.Bin("mul", Type.I64, len64, Value.IntConst(8L, Type.I64)))
        val handlesBuf = emitMallocBytes(bytes, fb)
        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), idxSlot)

        val fillCheckLabel = freshLabel("wasm_import_array_encode_fill_check")
        val fillBodyLabel = freshLabel("wasm_import_array_encode_fill_body")
        val fillEndLabel = freshLabel("wasm_import_array_encode_fill_end")
        fb.current.setTerminator(Terminator.Br(fillCheckLabel))

        val fillCheck = fb.newBlock(fillCheckLabel)
        fb.setCurrent(fillCheck)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("slt", idx, len64))
        fb.current.setTerminator(Terminator.CondBr(more, fillBodyLabel, fillEndLabel))

        val fillBody = fb.newBlock(fillBodyLabel)
        fb.setCurrent(fillBody)
        val elmValue = emitLoadWasmImportSequenceElement(elemsPtr, idx, elmTpe, fb)
        val elmHandle = emitEncodeWasmImportAbiValue(ctxPtr, elmValue, elmTpe, fb)
        val slotPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, handlesBuf, idx))
        fb.current.emitStore(elmHandle.handle, slotPtr)
        val idxNext = freshTmp(Type.I64)
        fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxNext, idxSlot)
        fb.current.setTerminator(Terminator.Br(fillCheckLabel))

        val fillEnd = fb.newBlock(fillEndLabel)
        fb.setCurrent(fillEnd)
        val arrHandle = freshTmp(Type.I64)
        fb.current.emitAssign(arrHandle, Op.Call(Type.I64, "flix_export_array_new", List(ctxPtr, Value.IntConst(if (elmTpe.isPointerLike) 1L else 0L, Type.I32), handlesBuf, len32)))
        if (exportEncodingProducesOwnedHandle(elmTpe)) {
          emitReleaseWasmImportHandleArray(ctxPtr, handlesBuf, len64, fb)
        }
        emitFreePtr(handlesBuf, fb)
        ExportHandle(arrHandle, owned = true)

      case ExportAbi.AbiType.Tuple(elms) =>
        val aggType = wasmImportSurfaceTypeOf(abiTpe)
        val childHandles = elms.zipWithIndex.map {
          case (elmTpe, idx) =>
            val field = freshTmp(wasmImportAggregateFieldTypeOf(elmTpe))
            fb.current.emitAssign(field, Op.ExtractValue(wasmImportAggregateFieldTypeOf(elmTpe), aggType, value, idx))
            emitEncodeWasmImportAbiValue(ctxPtr, field, elmTpe, fb)
        }
        val handlesPtr = emitTempI64Array(childHandles.map(_.handle), fb)
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_export_tuple_new", List(ctxPtr, handlesPtr, Value.IntConst(elms.length.toLong, Type.I32))))
        childHandles.foreach { case ExportHandle(h, true) => emitReleaseExportHandle(ctxPtr, h, fb); case _ => () }
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Record(fields) =>
        val aggType = wasmImportSurfaceTypeOf(abiTpe)
        val childHandles = fields.zipWithIndex.map {
          case ((_, fieldTpe), idx) =>
            val field = freshTmp(wasmImportAggregateFieldTypeOf(fieldTpe))
            fb.current.emitAssign(field, Op.ExtractValue(wasmImportAggregateFieldTypeOf(fieldTpe), aggType, value, idx))
            emitEncodeWasmImportAbiValue(ctxPtr, field, fieldTpe, fb)
        }
        val handlesPtr = emitTempI64Array(childHandles.map(_.handle), fb)
        val handle = freshTmp(Type.I64)
        fb.current.emitAssign(handle, Op.Call(Type.I64, "flix_export_tuple_new", List(ctxPtr, handlesPtr, Value.IntConst(fields.length.toLong, Type.I32))))
        childHandles.foreach { case ExportHandle(h, true) => emitReleaseExportHandle(ctxPtr, h, fb); case _ => () }
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Option(elmTpe) =>
        val aggType = wasmImportSurfaceTypeOf(abiTpe)
        val rawIsSome = freshTmp(Type.I8)
        fb.current.emitAssign(rawIsSome, Op.ExtractValue(Type.I8, aggType, value, 0))
        val isSome = freshTmp(Type.I1)
        fb.current.emitAssign(isSome, Op.ICmp("ne", rawIsSome, Value.IntConst(0L, Type.I8)))
        val someLabel = freshLabel("wasm_import_opt_some")
        val noneLabel = freshLabel("wasm_import_opt_none")
        val endLabel = freshLabel("wasm_import_opt_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isSome, someLabel, noneLabel))

        val someBlock = fb.newBlock(someLabel)
        fb.setCurrent(someBlock)
        val payload = freshTmp(wasmImportAggregateFieldTypeOf(elmTpe))
        fb.current.emitAssign(payload, Op.ExtractValue(wasmImportAggregateFieldTypeOf(elmTpe), aggType, value, 1))
        val someHandle = emitEncodeWasmImportAbiValue(ctxPtr, payload, elmTpe, fb)
        val someArrayPtr = emitTempI64Array(List(someHandle.handle), fb)
        val someTagged = freshTmp(Type.I64)
        fb.current.emitAssign(someTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(1L, Type.I64), someArrayPtr, Value.IntConst(1L, Type.I32))))
        if (someHandle.owned) emitReleaseExportHandle(ctxPtr, someHandle.handle, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((someTagged, someLabel))

        val noneBlock = fb.newBlock(noneLabel)
        fb.setCurrent(noneBlock)
        val noneTagged = freshTmp(Type.I64)
        fb.current.emitAssign(noneTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(0L, Type.I64), Value.Null(Type.Ptr), Value.IntConst(0L, Type.I32))))
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((noneTagged, noneLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val handle = freshTmp(Type.I64)
        endBlock.emitPhi(handle, incomings.toList)
        ExportHandle(handle, owned = true)

      case ExportAbi.AbiType.Result(okTpe, errTpe) =>
        val aggType = wasmImportSurfaceTypeOf(abiTpe)
        val rawIsOk = freshTmp(Type.I8)
        fb.current.emitAssign(rawIsOk, Op.ExtractValue(Type.I8, aggType, value, 0))
        val isOk = freshTmp(Type.I1)
        fb.current.emitAssign(isOk, Op.ICmp("ne", rawIsOk, Value.IntConst(0L, Type.I8)))
        val okLabel = freshLabel("wasm_import_result_ok")
        val errLabel = freshLabel("wasm_import_result_err")
        val endLabel = freshLabel("wasm_import_result_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isOk, okLabel, errLabel))

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)
        val okValue = freshTmp(wasmImportAggregateFieldTypeOf(okTpe))
        fb.current.emitAssign(okValue, Op.ExtractValue(wasmImportAggregateFieldTypeOf(okTpe), aggType, value, 1))
        val okHandle = emitEncodeWasmImportAbiValue(ctxPtr, okValue, okTpe, fb)
        val okArrayPtr = emitTempI64Array(List(okHandle.handle), fb)
        val okTagged = freshTmp(Type.I64)
        fb.current.emitAssign(okTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(1L, Type.I64), okArrayPtr, Value.IntConst(1L, Type.I32))))
        if (okHandle.owned) emitReleaseExportHandle(ctxPtr, okHandle.handle, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((okTagged, okLabel))

        val errBlock = fb.newBlock(errLabel)
        fb.setCurrent(errBlock)
        val errValue = freshTmp(wasmImportAggregateFieldTypeOf(errTpe))
        fb.current.emitAssign(errValue, Op.ExtractValue(wasmImportAggregateFieldTypeOf(errTpe), aggType, value, 2))
        val errHandle = emitEncodeWasmImportAbiValue(ctxPtr, errValue, errTpe, fb)
        val errArrayPtr = emitTempI64Array(List(errHandle.handle), fb)
        val errTagged = freshTmp(Type.I64)
        fb.current.emitAssign(errTagged, Op.Call(Type.I64, "flix_export_tag_new", List(ctxPtr, Value.IntConst(0L, Type.I64), errArrayPtr, Value.IntConst(1L, Type.I32))))
        if (errHandle.owned) emitReleaseExportHandle(ctxPtr, errHandle.handle, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((errTagged, errLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val handle = freshTmp(Type.I64)
        endBlock.emitPhi(handle, incomings.toList)
        ExportHandle(handle, owned = true)
    }

    private def emitOwnedImportHandleToLoweredValue(ctxPtr: Value, handle: ExportHandle, loweredTpe: SimpleType, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value = {
      val exportValue = emitDecodeExportHandle(ctxPtr, handle.handle, abiTpe, fb)
      emitExportSurfaceToLoweredValue(ctxPtr, exportValue, loweredTpe, abiTpe, fb)
    }

    private def emitExportSurfaceToLoweredValue(ctxPtr: Value, value: Value, loweredTpe: SimpleType, abiTpe: ExportAbi.AbiType, fb: FunBuilder): Value = abiTpe match {
      case ExportAbi.AbiType.Unit =>
        emitConstant(Constant.Unit, ctxPtr, fb)

      case ExportAbi.AbiType.Bool | ExportAbi.AbiType.Int8 | ExportAbi.AbiType.Int16 |
           ExportAbi.AbiType.Int32 | ExportAbi.AbiType.Int64 | ExportAbi.AbiType.Float32 |
           ExportAbi.AbiType.Float64 =>
        castValue(value, llvmTypeOf(loweredTpe), fb)

      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes =>
        val handle = castValue(value, Type.I64, fb)
        val ptr = freshTmp(Type.Ptr)
        fb.current.emitAssign(ptr, Op.Call(Type.Ptr, "flix_handle_get", List(ctxPtr, handle)))
        emitReleaseExportHandle(ctxPtr, handle, fb)
        castValue(ptr, llvmTypeOf(loweredTpe), fb)

      case ExportAbi.AbiType.List(elmAbiTpe) =>
        loweredTpe match {
          case SimpleType.Enum(sym, _) if sym.text == "List" && sym.namespace.isEmpty => ()
          case other => throw new IllegalStateException(s"Unexpected lowered List type: '$other'.")
        }
        val elmLoweredTpe = portableImportLoweredType(elmAbiTpe)
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val len = freshTmp(Type.I64)
        fb.current.emitAssign(len, Op.ExtractValue(Type.I64, aggTpe, value, 0))
        val elemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemsPtr, Op.ExtractValue(Type.Ptr, aggTpe, value, 1))

        val nilCaseSym = portableEnumCaseSym("List", "Nil")
        val currentSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(currentSlot, Op.Alloca(Type.Ptr))
        fb.current.emitStore(emitAllocPortableTag(nilCaseSym, Nil, ctxPtr, fb), currentSlot)

        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(len, idxSlot)

        val checkLabel = freshLabel("native_import_list_check")
        val bodyLabel = freshLabel("native_import_list_body")
        val endLabel = freshLabel("native_import_list_end")
        fb.current.setTerminator(Terminator.Br(checkLabel))

        val checkBlock = fb.newBlock(checkLabel)
        fb.setCurrent(checkBlock)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("sgt", idx, Value.IntConst(0L, Type.I64)))
        fb.current.setTerminator(Terminator.CondBr(more, bodyLabel, endLabel))

        val bodyBlock = fb.newBlock(bodyLabel)
        fb.setCurrent(bodyBlock)
        val idxPrev = freshTmp(Type.I64)
        fb.current.emitAssign(idxPrev, Op.Bin("sub", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxPrev, idxSlot)
        val currentTail = freshTmp(Type.Ptr)
        fb.current.emitAssign(currentTail, Op.Load(Type.Ptr, currentSlot))
        val elmExportValue = emitLoadSequenceElement(elemsPtr, idxPrev, elmAbiTpe, fb)
        val elmLoweredValue = emitExportSurfaceToLoweredValue(ctxPtr, elmExportValue, elmLoweredTpe, elmAbiTpe, fb)
        val consCaseSym = portableEnumCaseSym("List", "Cons")
        val consPayloads = List(
          (elmLoweredValue, elmLoweredTpe),
          (currentTail, loweredTpe),
        )
        val nextCons = emitAllocPortableTag(consCaseSym, consPayloads, ctxPtr, fb)
        fb.current.emitStore(nextCons, currentSlot)
        fb.current.setTerminator(Terminator.Br(checkLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val out = freshTmp(Type.Ptr)
        fb.current.emitAssign(out, Op.Load(Type.Ptr, currentSlot))
        emitFreePtr(elemsPtr, fb)
        out

      case ExportAbi.AbiType.Array(elmAbiTpe) =>
        loweredTpe match {
          case SimpleType.Array(_) => ()
          case other => throw new IllegalStateException(s"Unexpected lowered Array type: '$other'.")
        }
        val elmLoweredTpe = portableImportLoweredType(elmAbiTpe)
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val len = freshTmp(Type.I64)
        fb.current.emitAssign(len, Op.ExtractValue(Type.I64, aggTpe, value, 0))
        val elemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemsPtr, Op.ExtractValue(Type.Ptr, aggTpe, value, 1))

        val isPtrArray = isGcRootType(elmLoweredTpe)
        val isInt8Array = !isPtrArray && elmLoweredTpe == SimpleType.Int8

        val sizeBytes =
          if (isInt8Array) {
            val sz = freshTmp(Type.I64)
            fb.current.emitAssign(sz, Op.Bin("add", Type.I64, len, Value.IntConst(16L, Type.I64)))
            sz
          } else {
            val elmsBytes = freshTmp(Type.I64)
            fb.current.emitAssign(elmsBytes, Op.Bin("mul", Type.I64, len, Value.IntConst(8L, Type.I64)))
            val sz = freshTmp(Type.I64)
            fb.current.emitAssign(sz, Op.Bin("add", Type.I64, elmsBytes, Value.IntConst(16L, Type.I64)))
            sz
          }

        val arrTiName = if (isPtrArray) LlvmNames.arrayPtrTypeInfoName else LlvmNames.arrayPrimTypeInfoName
        val arrTi = Value.Global(arrTiName, Type.Ptr)
        val arrPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(arrPtr, Op.Call(Type.Ptr, "flix_region_alloc_flex", List(ctxPtr, Value.Null(Type.Ptr), arrTi, sizeBytes)))

        val lenI32 = freshTmp(Type.I32)
        fb.current.emitAssign(lenI32, Op.Cast("trunc", Type.I32, len))
        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(8L, Type.I64)))
        fb.current.emitStore(lenI32, lenPtr)
        val elemSizePtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemSizePtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(12L, Type.I64)))
        fb.current.emitStore(if (isInt8Array) Value.IntConst(1L, Type.I32) else Value.IntConst(8L, Type.I32), elemSizePtr)
        val outElemsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(outElemsPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(16L, Type.I64)))

        val idxSlot = freshTmp(Type.Ptr)
        fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), idxSlot)
        val fillCheckLabel = freshLabel("native_import_array_fill_check")
        val fillBodyLabel = freshLabel("native_import_array_fill_body")
        val fillEndLabel = freshLabel("native_import_array_fill_end")
        fb.current.setTerminator(Terminator.Br(fillCheckLabel))

        val fillCheckBlock = fb.newBlock(fillCheckLabel)
        fb.setCurrent(fillCheckBlock)
        val idx = freshTmp(Type.I64)
        fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
        val more = freshTmp(Type.I1)
        fb.current.emitAssign(more, Op.ICmp("slt", idx, len))
        fb.current.setTerminator(Terminator.CondBr(more, fillBodyLabel, fillEndLabel))

        val fillBodyBlock = fb.newBlock(fillBodyLabel)
        fb.setCurrent(fillBodyBlock)
        val elmExportValue = emitLoadSequenceElement(elemsPtr, idx, elmAbiTpe, fb)
        val elmLoweredValue = emitExportSurfaceToLoweredValue(ctxPtr, elmExportValue, elmLoweredTpe, elmAbiTpe, fb)
        val slotPtr =
          if (isInt8Array) {
            val ptr = freshTmp(Type.Ptr)
            fb.current.emitAssign(ptr, Op.Gep(Type.I8, outElemsPtr, idx))
            ptr
          } else {
            val ptr = freshTmp(Type.Ptr)
            fb.current.emitAssign(ptr, Op.Gep(Type.I64, outElemsPtr, idx))
            ptr
          }
        if (isInt8Array) {
          fb.current.emitStore(castValue(elmLoweredValue, Type.I8, fb), slotPtr)
        } else {
          val payload = boxToI64(elmLoweredValue, elmLoweredTpe, fb)
          if (isPtrArray) emitStorePtrLike(ctxPtr, slotPtr, payload, fb)
          else fb.current.emitStore(payload, slotPtr)
        }
        val idxNext = freshTmp(Type.I64)
        fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idx, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(idxNext, idxSlot)
        fb.current.setTerminator(Terminator.Br(fillCheckLabel))

        val fillEndBlock = fb.newBlock(fillEndLabel)
        fb.setCurrent(fillEndBlock)
        emitFreePtr(elemsPtr, fb)
        arrPtr

      case ExportAbi.AbiType.Tuple(elmAbiTpes) =>
        val elmLoweredTpes = loweredTpe match {
          case SimpleType.Tuple(elms) if elms.length == elmAbiTpes.length => elms
          case other => throw new IllegalStateException(s"Unexpected lowered Tuple type: '$other'.")
        }
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val payloads = elmAbiTpes.zip(elmLoweredTpes).zipWithIndex.map {
          case ((elmAbiTpe, elmLoweredTpe), idx) =>
            val field = freshTmp(exportAggregateFieldTypeOf(elmAbiTpe))
            fb.current.emitAssign(field, Op.ExtractValue(exportAggregateFieldTypeOf(elmAbiTpe), aggTpe, value, idx))
            val loweredField = emitExportSurfaceToLoweredValue(ctxPtr, field, elmLoweredTpe, elmAbiTpe, fb)
            boxToI64(loweredField, elmLoweredTpe, fb)
        }
        val tupPtr = freshTmp(Type.Ptr)
        val tupTi = Value.Global(LlvmNames.tupleTypeInfoName(loweredTpe.asInstanceOf[SimpleType.Tuple]), Type.Ptr)
        fb.current.emitAssign(tupPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, tupTi)))
        payloads.zipWithIndex.foreach {
          case (payload, idx) => storeObjI64Slot(tupPtr, Value.IntConst(idx.toLong, Type.I64), payload, fb)
        }
        tupPtr

      case ExportAbi.AbiType.Record(fieldAbiTpes) =>
        val loweredFields = recordFields(loweredTpe)
        if (loweredFields.map(_._1) != fieldAbiTpes.map(_._1)) {
          throw new IllegalStateException(s"Mismatched lowered/export record fields for '$loweredTpe'.")
        }
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val payloads = fieldAbiTpes.zip(loweredFields).zipWithIndex.map {
          case (((_, fieldAbiTpe), (_, fieldLoweredTpe)), idx) =>
            val field = freshTmp(exportAggregateFieldTypeOf(fieldAbiTpe))
            fb.current.emitAssign(field, Op.ExtractValue(exportAggregateFieldTypeOf(fieldAbiTpe), aggTpe, value, idx))
            val loweredField = emitExportSurfaceToLoweredValue(ctxPtr, field, fieldLoweredTpe, fieldAbiTpe, fb)
            boxToI64(loweredField, fieldLoweredTpe, fb)
        }
        val recPtr = freshTmp(Type.Ptr)
        val recTi = Value.Global(LlvmNames.recordTypeInfoName(loweredTpe), Type.Ptr)
        fb.current.emitAssign(recPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, recTi)))
        payloads.zipWithIndex.foreach {
          case (payload, idx) => storeObjI64Slot(recPtr, Value.IntConst(idx.toLong, Type.I64), payload, fb)
        }
        recPtr

      case ExportAbi.AbiType.Option(elmAbiTpe) =>
        loweredTpe match {
          case SimpleType.Enum(sym, _) if sym.text == "Option" && sym.namespace.isEmpty => ()
          case other => throw new IllegalStateException(s"Unexpected lowered Option type: '$other'.")
        }
        val elmLoweredTpe = portableImportLoweredType(elmAbiTpe)
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val rawTag = freshTmp(Type.I8)
        fb.current.emitAssign(rawTag, Op.ExtractValue(Type.I8, aggTpe, value, 0))
        val isSome = freshTmp(Type.I1)
        fb.current.emitAssign(isSome, Op.ICmp("ne", rawTag, Value.IntConst(0L, Type.I8)))
        val someLabel = freshLabel("native_import_option_some")
        val noneLabel = freshLabel("native_import_option_none")
        val endLabel = freshLabel("native_import_option_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isSome, someLabel, noneLabel))

        val someBlock = fb.newBlock(someLabel)
        fb.setCurrent(someBlock)
        val payload = freshTmp(exportAggregateFieldTypeOf(elmAbiTpe))
        fb.current.emitAssign(payload, Op.ExtractValue(exportAggregateFieldTypeOf(elmAbiTpe), aggTpe, value, 1))
        val loweredPayload = emitExportSurfaceToLoweredValue(ctxPtr, payload, elmLoweredTpe, elmAbiTpe, fb)
        val someValue = emitAllocPortableTag(portableEnumCaseSym("Option", "Some"), List((loweredPayload, elmLoweredTpe)), ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((someValue, someLabel))

        val noneBlock = fb.newBlock(noneLabel)
        fb.setCurrent(noneBlock)
        val noneValue = emitAllocPortableTag(portableEnumCaseSym("Option", "None"), Nil, ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((noneValue, noneLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val out = freshTmp(Type.Ptr)
        endBlock.emitPhi(out, incomings.toList)
        out

      case ExportAbi.AbiType.Result(okAbiTpe, errAbiTpe) =>
        loweredTpe match {
          case SimpleType.Enum(sym, _) if sym.text == "Result" && sym.namespace.isEmpty => ()
          case other => throw new IllegalStateException(s"Unexpected lowered Result type: '$other'.")
        }
        val okLoweredTpe = portableImportLoweredType(okAbiTpe)
        val errLoweredTpe = portableImportLoweredType(errAbiTpe)
        val aggTpe = exportSurfaceTypeOf(abiTpe)
        val rawTag = freshTmp(Type.I8)
        fb.current.emitAssign(rawTag, Op.ExtractValue(Type.I8, aggTpe, value, 0))
        val isOk = freshTmp(Type.I1)
        fb.current.emitAssign(isOk, Op.ICmp("ne", rawTag, Value.IntConst(0L, Type.I8)))
        val okLabel = freshLabel("native_import_result_ok")
        val errLabel = freshLabel("native_import_result_err")
        val endLabel = freshLabel("native_import_result_end")
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        fb.current.setTerminator(Terminator.CondBr(isOk, okLabel, errLabel))

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)
        val okField = freshTmp(exportAggregateFieldTypeOf(okAbiTpe))
        fb.current.emitAssign(okField, Op.ExtractValue(exportAggregateFieldTypeOf(okAbiTpe), aggTpe, value, 1))
        val okLoweredValue = emitExportSurfaceToLoweredValue(ctxPtr, okField, okLoweredTpe, okAbiTpe, fb)
        val okValue = emitAllocPortableTag(portableEnumCaseSym("Result", "Ok"), List((okLoweredValue, okLoweredTpe)), ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((okValue, okLabel))

        val errBlock = fb.newBlock(errLabel)
        fb.setCurrent(errBlock)
        val errField = freshTmp(exportAggregateFieldTypeOf(errAbiTpe))
        fb.current.emitAssign(errField, Op.ExtractValue(exportAggregateFieldTypeOf(errAbiTpe), aggTpe, value, 2))
        val errLoweredValue = emitExportSurfaceToLoweredValue(ctxPtr, errField, errLoweredTpe, errAbiTpe, fb)
        val errValue = emitAllocPortableTag(portableEnumCaseSym("Result", "Err"), List((errLoweredValue, errLoweredTpe)), ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings += ((errValue, errLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val out = freshTmp(Type.Ptr)
        endBlock.emitPhi(out, incomings.toList)
        out
    }

    private def emitAllocPortableTag(caseSym: Symbol.CaseSym, payloads: List[(Value, SimpleType)], ctxPtr: Value, fb: FunBuilder): Value = {
      val objPtr = freshTmp(Type.Ptr)
      val tagTi = Value.Global(LlvmNames.tagTypeInfoName(caseSym), Type.Ptr)
      fb.current.emitAssign(objPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, tagTi)))
      val tagId = caseTagIds.getOrElse(caseSym, throw new IllegalStateException(s"Missing case tag id for '$caseSym'."))
      storeObjI64Slot(objPtr, Value.IntConst(0L, Type.I64), Value.IntConst(tagId, Type.I64), fb)
      payloads.zipWithIndex.foreach {
        case ((payload, payloadTpe), idx) =>
          storeObjI64Slot(objPtr, Value.IntConst((idx + 1).toLong, Type.I64), boxToI64(payload, payloadTpe, fb), fb)
      }
      objPtr
    }

    private def emitConvertExportSequenceToWasm(ctxPtr: Value, exportSeqPtr: Value, wasmSeqPtr: Value, len64: Value, elmTpe: ExportAbi.AbiType, fb: FunBuilder): Unit = {
      val idxSlot = freshTmp(Type.Ptr)
      fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), idxSlot)
      val checkLabel = freshLabel("wasm_import_seq_check")
      val bodyLabel = freshLabel("wasm_import_seq_body")
      val endLabel = freshLabel("wasm_import_seq_end")
      fb.current.setTerminator(Terminator.Br(checkLabel))

      val check = fb.newBlock(checkLabel)
      fb.setCurrent(check)
      val idx = freshTmp(Type.I64)
      fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
      val more = freshTmp(Type.I1)
      fb.current.emitAssign(more, Op.ICmp("slt", idx, castValue(len64, Type.I64, fb)))
      fb.current.setTerminator(Terminator.CondBr(more, bodyLabel, endLabel))

      val body = fb.newBlock(bodyLabel)
      fb.setCurrent(body)
      val exportElm = emitLoadSequenceElement(exportSeqPtr, idx, elmTpe, fb)
      val wasmElm = emitExportSurfaceToWasmImportField(ctxPtr, exportElm, elmTpe, fb)
      emitStoreWasmImportSequenceElement(wasmSeqPtr, idx, elmTpe, wasmElm, fb)
      val idxNext = freshTmp(Type.I64)
      fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idx, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(idxNext, idxSlot)
      fb.current.setTerminator(Terminator.Br(checkLabel))

      val end = fb.newBlock(endLabel)
      fb.setCurrent(end)
    }

    private def emitReleaseWasmImportHandleArray(ctxPtr: Value, handlesBuf: Value, len64: Value, fb: FunBuilder): Unit = {
      val idxSlot = freshTmp(Type.Ptr)
      fb.current.emitAssign(idxSlot, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), idxSlot)
      val checkLabel = freshLabel("wasm_import_release_check")
      val bodyLabel = freshLabel("wasm_import_release_body")
      val endLabel = freshLabel("wasm_import_release_end")
      fb.current.setTerminator(Terminator.Br(checkLabel))

      val check = fb.newBlock(checkLabel)
      fb.setCurrent(check)
      val idx = freshTmp(Type.I64)
      fb.current.emitAssign(idx, Op.Load(Type.I64, idxSlot))
      val more = freshTmp(Type.I1)
      fb.current.emitAssign(more, Op.ICmp("slt", idx, len64))
      fb.current.setTerminator(Terminator.CondBr(more, bodyLabel, endLabel))

      val body = fb.newBlock(bodyLabel)
      fb.setCurrent(body)
      val ptr = freshTmp(Type.Ptr)
      fb.current.emitAssign(ptr, Op.Gep(Type.I64, handlesBuf, idx))
      val handle = freshTmp(Type.I64)
      fb.current.emitAssign(handle, Op.Load(Type.I64, ptr))
      emitReleaseExportHandle(ctxPtr, handle, fb)
      val idxNext = freshTmp(Type.I64)
      fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idx, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(idxNext, idxSlot)
      fb.current.setTerminator(Terminator.Br(checkLabel))

      val end = fb.newBlock(endLabel)
      fb.setCurrent(end)
    }

    private def emitAllocWasmImportSequenceBuffer(len64: Value, elmTpe: ExportAbi.AbiType, fb: FunBuilder): Value = {
      val elemBytes = emitSizeOfType(wasmImportAggregateFieldTypeOf(elmTpe), fb)
      val totalBytes = freshTmp(Type.I64)
      fb.current.emitAssign(totalBytes, Op.Bin("mul", Type.I64, castValue(len64, Type.I64, fb), elemBytes))
      emitMallocBytes(totalBytes, fb)
    }

    private def emitWasmImportFree(interfaceId: WasmImportInterface.Id, abiTpe: ExportAbi.AbiType, ptr0: Value, fb: FunBuilder): Unit =
      wasmImportFreeHelperName(interfaceId, abiTpe).foreach { name =>
        fb.current.emitCallVoid(name, List(castValue(ptr0, Type.Ptr, fb)))
      }

    private def exportEncodingProducesOwnedHandle(tpe: ExportAbi.AbiType): Boolean = tpe match {
      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes => false
      case _ => true
    }

    private def zeroExportAbiValue(tpe: ExportAbi.AbiType): Value = tpe match {
      case ExportAbi.AbiType.Unit => Value.IntConst(0L, Type.I64)
      case ExportAbi.AbiType.Bool => Value.IntConst(0L, Type.I1)
      case ExportAbi.AbiType.Int8 => Value.IntConst(0L, Type.I8)
      case ExportAbi.AbiType.Int16 => Value.IntConst(0L, Type.I16)
      case ExportAbi.AbiType.Int32 => Value.IntConst(0L, Type.I32)
      case ExportAbi.AbiType.Int64 => Value.IntConst(0L, Type.I64)
      case ExportAbi.AbiType.Float32 => Value.Float32Const(0)
      case ExportAbi.AbiType.Float64 => Value.Float64Const(0L)
      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes => Value.IntConst(0L, Type.I64)
      case ExportAbi.AbiType.List(_) | ExportAbi.AbiType.Array(_) =>
        Value.StructConst(List(Value.IntConst(0L, Type.I64), Value.Null(Type.Ptr)), exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Tuple(elms) =>
        Value.StructConst(elms.map(zeroExportAggregateFieldValue), exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Record(fields) =>
        Value.StructConst(fields.map { case (_, fieldTpe) => zeroExportAggregateFieldValue(fieldTpe) }, exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Option(elm) =>
        Value.StructConst(List(Value.IntConst(0L, Type.I8), zeroExportAggregateFieldValue(elm)), exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Result(ok, err) =>
        Value.StructConst(List(Value.IntConst(0L, Type.I8), zeroExportAggregateFieldValue(ok), zeroExportAggregateFieldValue(err)), exportSurfaceTypeOf(tpe))
    }

    private def zeroExportAggregateFieldValue(tpe: ExportAbi.AbiType): Value = tpe match {
      case ExportAbi.AbiType.Bool => Value.IntConst(0L, Type.I8)
      case ExportAbi.AbiType.List(_) | ExportAbi.AbiType.Array(_) =>
        Value.StructConst(List(Value.IntConst(0L, Type.I64), Value.Null(Type.Ptr)), exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Tuple(elms) =>
        Value.StructConst(elms.map(zeroExportAggregateFieldValue), exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Record(fields) =>
        Value.StructConst(fields.map { case (_, fieldTpe) => zeroExportAggregateFieldValue(fieldTpe) }, exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Option(elm) =>
        Value.StructConst(List(Value.IntConst(0L, Type.I8), zeroExportAggregateFieldValue(elm)), exportSurfaceTypeOf(tpe))
      case ExportAbi.AbiType.Result(ok, err) =>
        Value.StructConst(List(Value.IntConst(0L, Type.I8), zeroExportAggregateFieldValue(ok), zeroExportAggregateFieldValue(err)), exportSurfaceTypeOf(tpe))
      case other => zeroExportAbiValue(other)
    }

    private def exportAggregateFieldTypesOf(tpe: ExportAbi.AbiType): List[Type] = tpe match {
      case ExportAbi.AbiType.List(_) | ExportAbi.AbiType.Array(_) => List(Type.I64, Type.Ptr)
      case ExportAbi.AbiType.Tuple(elms) => elms.map(exportAggregateFieldTypeOf)
      case ExportAbi.AbiType.Record(fields) => fields.map { case (_, fieldTpe) => exportAggregateFieldTypeOf(fieldTpe) }
      case ExportAbi.AbiType.Option(elm) => List(Type.I8, exportAggregateFieldTypeOf(elm))
      case ExportAbi.AbiType.Result(ok, err) => List(Type.I8, exportAggregateFieldTypeOf(ok), exportAggregateFieldTypeOf(err))
      case other => throw new IllegalStateException(s"Unexpected non-aggregate export ABI type: $other")
    }

    private def exportAggregateFieldTypeOf(tpe: ExportAbi.AbiType): Type = tpe match {
      case ExportAbi.AbiType.Bool => Type.I8
      case ExportAbi.AbiType.List(_) | ExportAbi.AbiType.Array(_) | ExportAbi.AbiType.Tuple(_) | ExportAbi.AbiType.Record(_) | ExportAbi.AbiType.Option(_) | ExportAbi.AbiType.Result(_, _) =>
        exportSurfaceTypeOf(tpe)
      case other => exportSurfaceTypeOf(other)
    }

    private def requestFieldSurfaceTypeOf(tpe: ExportAbi.AbiType): Type =
      exportAggregateFieldTypeOf(tpe)

    private def exportSurfaceTypeOf(tpe: ExportAbi.AbiType): Type = tpe match {
      case ExportAbi.AbiType.Unit => Type.I64
      case ExportAbi.AbiType.Bool => Type.I1
      case ExportAbi.AbiType.Int8 => Type.I8
      case ExportAbi.AbiType.Int16 => Type.I16
      case ExportAbi.AbiType.Int32 => Type.I32
      case ExportAbi.AbiType.Int64 => Type.I64
      case ExportAbi.AbiType.Float32 => Type.Float
      case ExportAbi.AbiType.Float64 => Type.Double
      case ExportAbi.AbiType.String => Type.I64
      case ExportAbi.AbiType.Bytes => Type.I64
      case ExportAbi.AbiType.List(_) | ExportAbi.AbiType.Array(_) => Type.Struct(List(Type.I64, Type.Ptr))
      case ExportAbi.AbiType.Tuple(elms) => Type.Struct(elms.map(exportAggregateFieldTypeOf))
      case ExportAbi.AbiType.Record(fields) => Type.Struct(fields.map { case (_, fieldTpe) => exportAggregateFieldTypeOf(fieldTpe) })
      case ExportAbi.AbiType.Option(elm) => Type.Struct(List(Type.I8, exportAggregateFieldTypeOf(elm)))
      case ExportAbi.AbiType.Result(ok, err) => Type.Struct(List(Type.I8, exportAggregateFieldTypeOf(ok), exportAggregateFieldTypeOf(err)))
    }

    private def exportParamSurfaceTypeOf(tpe: ExportAbi.AbiType): Type = tpe match {
      case agg if ExportAbi.isAggregate(agg) => Type.Ptr
      case other => exportSurfaceTypeOf(other)
    }

    private def wasmImportScalarLlvmType(tpe: ExportAbi.AbiType): Type = tpe match {
      case ExportAbi.AbiType.Bool => Type.I1
      case ExportAbi.AbiType.Int8 => Type.I8
      case ExportAbi.AbiType.Int16 => Type.I16
      case ExportAbi.AbiType.Int32 => Type.I32
      case ExportAbi.AbiType.Int64 => Type.I64
      case ExportAbi.AbiType.Float32 => Type.Float
      case ExportAbi.AbiType.Float64 => Type.Double
      case other => throw new IllegalStateException(s"Unexpected non-scalar wasm import ABI type: $other")
    }

    private def wasmImportAggregateFieldTypesOf(tpe: ExportAbi.AbiType): List[Type] = tpe match {
      case ExportAbi.AbiType.Unit => Nil
      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes | ExportAbi.AbiType.List(_) | ExportAbi.AbiType.Array(_) => List(Type.Ptr, Type.I32)
      case ExportAbi.AbiType.Tuple(elms) => elms.map(wasmImportAggregateFieldTypeOf)
      case ExportAbi.AbiType.Record(fields) => fields.map { case (_, fieldTpe) => wasmImportAggregateFieldTypeOf(fieldTpe) }
      case ExportAbi.AbiType.Option(elm) => List(Type.I8, wasmImportAggregateFieldTypeOf(elm))
      case ExportAbi.AbiType.Result(ok, err) => List(Type.I8, wasmImportAggregateFieldTypeOf(ok), wasmImportAggregateFieldTypeOf(err))
      case other => throw new IllegalStateException(s"Unexpected non-byref wasm import ABI type: $other")
    }

    private def wasmImportAggregateFieldTypeOf(tpe: ExportAbi.AbiType): Type = tpe match {
      case ExportAbi.AbiType.Bool => Type.I8
      case t if WasmImportAbi.isByRefBoundaryType(t) => wasmImportSurfaceTypeOf(t)
      case other => wasmImportScalarLlvmType(other)
    }

    private def wasmImportSurfaceTypeOf(tpe: ExportAbi.AbiType): Type = tpe match {
      case t if !WasmImportAbi.isByRefBoundaryType(t) => wasmImportScalarLlvmType(t)
      case other => Type.Struct(wasmImportAggregateFieldTypesOf(other))
    }

    private def wasmImportParamSurfaceTypeOf(tpe: ExportAbi.AbiType): Type =
      if (WasmImportAbi.isByRefBoundaryType(tpe)) Type.Ptr else wasmImportScalarLlvmType(tpe)

    private def wasmImportHelperCTypeName(interfaceId: WasmImportInterface.Id, tpe: ExportAbi.AbiType): String = tpe match {
      case ExportAbi.AbiType.String => "flix_string"
      case ExportAbi.AbiType.Bytes => "flix_list_u8"
      case ExportAbi.AbiType.Unit => "flix_tuple0"
      case other =>
        s"${WasmImportInterface.sanitize(interfaceId.namespace)}_${WasmImportInterface.sanitize(interfaceId.packageName)}_${WasmImportInterface.sanitize(interfaceId.interfaceName)}_${WasmImportInterface.sanitize(WasmComponentAbi.witTypeOf(other))}"
    }

    private def wasmImportFreeHelperName(interfaceId: WasmImportInterface.Id, tpe: ExportAbi.AbiType): Option[String] = tpe match {
      case ExportAbi.AbiType.Unit => None
      case t if !wasmImportNeedsFreeHelper(t) => None
      case other => Some(s"${wasmImportHelperCTypeName(interfaceId, other)}_free")
    }

    private def wasmImportNeedsFreeHelper(tpe: ExportAbi.AbiType): Boolean = tpe match {
      case ExportAbi.AbiType.String | ExportAbi.AbiType.Bytes | ExportAbi.AbiType.List(_) | ExportAbi.AbiType.Array(_) => true
      case ExportAbi.AbiType.Tuple(elms) => elms.exists(wasmImportNeedsFreeHelper)
      case ExportAbi.AbiType.Record(fields) => fields.exists { case (_, fieldTpe) => wasmImportNeedsFreeHelper(fieldTpe) }
      case ExportAbi.AbiType.Option(elm) => wasmImportNeedsFreeHelper(elm)
      case ExportAbi.AbiType.Result(ok, err) => wasmImportNeedsFreeHelper(ok) || wasmImportNeedsFreeHelper(err)
      case _ => false
    }

    private def requestSurfaceTypeOf(sig: ExportAbi.Signature): Type =
      if (sig.params.isEmpty) Type.Struct(List(Type.I8))
      else Type.Struct(sig.params.map(requestFieldSurfaceTypeOf))

    private def simpleTypeOfExportLeaf(tpe: ExportAbi.AbiType): SimpleType = tpe match {
      case ExportAbi.AbiType.Unit => SimpleType.Unit
      case ExportAbi.AbiType.Bool => SimpleType.Bool
      case ExportAbi.AbiType.Int8 => SimpleType.Int8
      case ExportAbi.AbiType.Int16 => SimpleType.Int16
      case ExportAbi.AbiType.Int32 => SimpleType.Int32
      case ExportAbi.AbiType.Int64 => SimpleType.Int64
      case ExportAbi.AbiType.Float32 => SimpleType.Float32
      case ExportAbi.AbiType.Float64 => SimpleType.Float64
      case other => throw new IllegalStateException(s"Unexpected non-leaf export ABI type: $other")
    }

    private def exportAbiTypeOf(tpe: SimpleType): Type =
      if (isHandleAbiType(tpe)) Type.I64 else llvmTypeOf(tpe)

    private def llvmTypeOf(tpe: SimpleType): Type = tpe match {
      case SimpleType.Bool => Type.I1
      case SimpleType.Char => Type.I32
      case SimpleType.Int8 => Type.I8
      case SimpleType.Int16 => Type.I16
      case SimpleType.Int32 => Type.I32
      case SimpleType.Int64 => Type.I64
      case SimpleType.Float32 => Type.Float
      case SimpleType.Float64 => Type.Double
      case SimpleType.AnyType => Type.I64
      case SimpleType.Unit => Type.I64
      case SimpleType.Null => Type.Ptr
      case SimpleType.Object => Type.I64
      case _ => Type.Ptr
    }

    private def nativeImportCallLlvmType(tpe: NativeImportAbi.AbiType): Type = tpe match {
      case NativeImportAbi.AbiType.Unit => Type.Void
      case NativeImportAbi.AbiType.Bool => Type.I1
      case NativeImportAbi.AbiType.Int8 => Type.I8
      case NativeImportAbi.AbiType.Int16 => Type.I16
      case NativeImportAbi.AbiType.Int32 => Type.I32
      case NativeImportAbi.AbiType.Int64 => Type.I64
      case NativeImportAbi.AbiType.Float32 => Type.Float
      case NativeImportAbi.AbiType.Float64 => Type.Double
      case NativeImportAbi.AbiType.String => Type.I64
      case NativeImportAbi.AbiType.Bytes => Type.I64
      case NativeImportAbi.AbiType.Portable(_) => Type.I64
    }

    private def emitLoadWasmImportSequenceElement(seqPtr0: Value, idx0: Value, elmTpe: ExportAbi.AbiType, fb: FunBuilder): Value = {
      val seqPtr = castValue(seqPtr0, Type.Ptr, fb)
      val idx = castValue(idx0, Type.I64, fb)
      val fieldTpe = wasmImportAggregateFieldTypeOf(elmTpe)
      val elemPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(elemPtr, Op.Gep(fieldTpe, seqPtr, idx))
      val value = freshTmp(fieldTpe)
      fb.current.emitAssign(value, Op.Load(fieldTpe, elemPtr))
      value
    }

    private def emitStoreWasmImportSequenceElement(seqPtr0: Value, idx0: Value, elmTpe: ExportAbi.AbiType, value0: Value, fb: FunBuilder): Unit = {
      val seqPtr = castValue(seqPtr0, Type.Ptr, fb)
      val idx = castValue(idx0, Type.I64, fb)
      val fieldTpe = wasmImportAggregateFieldTypeOf(elmTpe)
      val elemPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(elemPtr, Op.Gep(fieldTpe, seqPtr, idx))
      fb.current.emitStore(castValue(value0, fieldTpe, fb), elemPtr)
    }

    private def buildWasmImportStructValue(abiTpe: ExportAbi.AbiType, values: List[Value], fb: FunBuilder): Value = {
      val aggTpe = wasmImportSurfaceTypeOf(abiTpe)
      val fieldTypes = wasmImportAggregateFieldTypesOf(abiTpe)
      values.zip(fieldTypes).zipWithIndex.foldLeft(Value.Undef(aggTpe): Value) {
        case (agg, ((value, fieldTpe), idx)) =>
          val coerced = castValue(value, fieldTpe, fb)
          val next = freshTmp(aggTpe)
          fb.current.emitAssign(next, Op.InsertValue(aggTpe, agg, coerced, idx))
          next
      }
    }

    private def zeroValueOf(tpe: Type): Value = tpe match {
      case Type.I1 | Type.I8 | Type.I16 | Type.I32 | Type.I64 =>
        Value.IntConst(0L, tpe)
      case Type.Float =>
        // +0.0f bit pattern
        Value.Float32Const(0)
      case Type.Double =>
        // +0.0 bit pattern
        Value.Float64Const(0L)
      case Type.Ptr =>
        Value.Null(Type.Ptr)
      case other =>
        // Should not be needed for the current backend (locals are only primitive/I64/Ptr).
        Value.Undef(other)
    }

    private def packResult(v: Value, tpe: SimpleType, fb: FunBuilder): Value = {
      val payload = boxToI64(v, tpe, fb)
      packResultTagged(ResultTagValue, payload, fb)
    }

    private def packThunkResult(thunkPtr0: Value, fb: FunBuilder): Value = {
      val thunkPtr = castValue(thunkPtr0, Type.Ptr, fb)
      val payload = castValue(thunkPtr, Type.I64, fb)
      packResultTagged(ResultTagThunk, payload, fb)
    }

    private def packResultTagged(tag: Long, payload0: Value, fb: FunBuilder): Value = {
      val payload = castValue(payload0, Type.I64, fb)

      val r0 = freshTmp(flixResultType)
      fb.current.emitAssign(r0, Op.InsertValue(flixResultType, Value.Undef(flixResultType), Value.IntConst(tag, Type.I64), index = 0))

      val r1 = freshTmp(flixResultType)
      fb.current.emitAssign(r1, Op.InsertValue(flixResultType, r0, payload, index = 1))

      r1
    }

    /**
      * Unwinds thunks until we reach a non-thunk result (VALUE/SUSPENSION/EXCEPTION) and returns it.
      */
    private def unwindThunkToResult(result0: Value, ctxPtr: Value, fb: FunBuilder): Value = {
      val entryLabel = fb.current.label

      val loopLabel = freshLabel("unwind_loop")
      val thunkLabel = freshLabel("unwind_thunk")
      val bodyLabel = freshLabel("unwind_body")
      val endLabel = freshLabel("unwind_end")

      // Pre-header: jump to the loop.
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val nextResult = freshTmp(flixResultType)

      // Loop header.
      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)

      val curResult = freshTmp(flixResultType)
      loopBlock.emitPhi(curResult, List((result0, entryLabel), (nextResult, bodyLabel)))

      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, curResult, index = 0))

      val isThunk = freshTmp(Type.I1)
      fb.current.emitAssign(isThunk, Op.ICmp("eq", tag, Value.IntConst(ResultTagThunk, Type.I64)))
      fb.current.setTerminator(Terminator.CondBr(isThunk, thunkLabel, endLabel))

      // Thunk path: root the thunk pointer across pollcheck before invoking it.
      val thunkBlock = fb.newBlock(thunkLabel)
      fb.setCurrent(thunkBlock)

      val payload = freshTmp(Type.I64)
      thunkBlock.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, curResult, index = 1))

      val thunkPtr = freshTmp(Type.Ptr)
      thunkBlock.emitAssign(thunkPtr, Op.Cast("inttoptr", Type.Ptr, payload))

      val thunkSlotPtr = freshTmp(Type.Ptr)
      thunkBlock.emitAssign(thunkSlotPtr, Op.Alloca(Type.Ptr))
      thunkBlock.emitStore(thunkPtr, thunkSlotPtr)
      thunkBlock.emitCallVoid(rootPushNameOf(Type.Ptr), List(ctxPtr, thunkSlotPtr))

      thunkBlock.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
      thunkBlock.emitCallVoid("flix_gc_pop_roots", List(ctxPtr, Value.IntConst(1L, Type.I64)))

      val isCancelled = freshTmp(Type.I1)
      thunkBlock.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))

      val cancelLabel = freshLabel("unwind_cancel")
      thunkBlock.setTerminator(Terminator.CondBr(isCancelled, cancelLabel, bodyLabel))

      val cancelBlock = fb.newBlock(cancelLabel)
      fb.setCurrent(cancelBlock)
      val cancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
      val tracedCancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
      val cancelBits = freshTmp(Type.I64)
      fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
      val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
      fb.current.setTerminator(Terminator.Br(endLabel))

      // Loop body: invoke the thunk and iterate.
      val bodyBlock = fb.newBlock(bodyLabel)
      fb.setCurrent(bodyBlock)

      // Invoke the thunk, but write directly into `nextResult` for the phi.
      bodyBlock.emitAssign(nextResult, Op.Call(flixResultType, "flix_invoke_thunk", List(ctxPtr, thunkPtr, Value.IntConst(ResultTagValue, Type.I64), Value.IntConst(0L, Type.I64))))
      bodyBlock.setTerminator(Terminator.Br(loopLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val outResult = freshTmp(flixResultType)
      endBlock.emitPhi(outResult, List((curResult, loopLabel), (cancelResult, cancelLabel)))
      outResult
    }

    /**
      * Unwinds suspension-free thunks until we reach a VALUE result and returns its payload bits.
      *
      * Current behavior:
      *   - Traps on SUSPENSION or EXCEPTION.
      */
    private def unwindThunkToValuePayload(result0: Value, ctxPtr: Value, fb: FunBuilder): Value = {
      val r = unwindThunkToResult(result0, ctxPtr, fb)

      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, r, index = 0))
      val isValue = freshTmp(Type.I1)
      fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

      val valueLabel = freshLabel("unwind_value")
      val badLabel = freshLabel("unwind_bad")
      fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, badLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val valueBlock = fb.newBlock(valueLabel)
      fb.setCurrent(valueBlock)

      val valuePayload = freshTmp(Type.I64)
      valueBlock.emitAssign(valuePayload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
      valuePayload
    }

    /**
      * Unwinds thunks until we reach a VALUE result and returns its payload bits.
      *
      * Current behavior:
      *   - Propagates EXCEPTION according to `exnHandlerOpt` (branch or return).
      *   - Traps on SUSPENSION.
      */
    private def unwindThunkToValuePayloadOrPropagateExn(result0: Value,
                                                        ctxPtr: Value,
                                                        fb: FunBuilder,
                                                        exnHandlerOpt: Option[ExnHandler]): Value = {
      val r = unwindThunkToResult(result0, ctxPtr, fb)

      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, r, index = 0))
      val isValue = freshTmp(Type.I1)
      fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

      val valueLabel = freshLabel("unwind_value")
      val notValueLabel = freshLabel("unwind_not_value")
      fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, notValueLabel))

      val notValueBlock = fb.newBlock(notValueLabel)
      fb.setCurrent(notValueBlock)
      val isExn = freshTmp(Type.I1)
      fb.current.emitAssign(isExn, Op.ICmp("eq", tag, Value.IntConst(ResultTagException, Type.I64)))
      val exnLabel = freshLabel("unwind_exn")
      val badLabel = freshLabel("unwind_bad")
      fb.current.setTerminator(Terminator.CondBr(isExn, exnLabel, badLabel))

      val exnBlock = fb.newBlock(exnLabel)
      fb.setCurrent(exnBlock)
      exnHandlerOpt match {
        case Some(ExnHandler(label, slotPtr)) =>
          val payload = freshTmp(Type.I64)
          fb.current.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
          fb.current.emitStore(payload, slotPtr)
          fb.current.setTerminator(Terminator.Br(label))
        case None =>
          fb.current.setTerminator(Terminator.Ret(flixResultType, r))
      }

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val valueBlock = fb.newBlock(valueLabel)
      fb.setCurrent(valueBlock)
      val valuePayload = freshTmp(Type.I64)
      valueBlock.emitAssign(valuePayload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
      valuePayload
    }

    /**
      * Invokes a thunk object using the runtime thunk dispatcher (typeinfo.invoke).
      */
    private def emitInvokeThunk(thunkPtr0: Value, ctxPtr: Value, fb: FunBuilder): Value = {
      val thunkPtr = castValue(thunkPtr0, Type.Ptr, fb)
      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_invoke_thunk", List(ctxPtr, thunkPtr, Value.IntConst(ResultTagValue, Type.I64), Value.IntConst(0L, Type.I64))))
      callTmp
    }

    private def boxToI64(v: Value, tpe: SimpleType, fb: FunBuilder): Value = tpe match {
      case SimpleType.Unit =>
        Value.IntConst(0L, Type.I64)

      case SimpleType.Bool =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("zext", Type.I64, v))
        tmp

      case SimpleType.Char =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("zext", Type.I64, v))
        tmp

      case SimpleType.Int8 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, v))
        tmp

      case SimpleType.Int16 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, v))
        tmp

      case SimpleType.Int32 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, v))
        tmp

      case SimpleType.Int64 =>
        v

      case SimpleType.Float32 =>
        val asI32 = freshTmp(Type.I32)
        fb.current.emitAssign(asI32, Op.Cast("bitcast", Type.I32, v))

        val asI64 = freshTmp(Type.I64)
        fb.current.emitAssign(asI64, Op.Cast("zext", Type.I64, asI32))
        asI64

      case SimpleType.Float64 =>
        val asI64 = freshTmp(Type.I64)
        fb.current.emitAssign(asI64, Op.Cast("bitcast", Type.I64, v))
        asI64

      case SimpleType.Null =>
        Value.IntConst(0L, Type.I64)

      case SimpleType.AnyType =>
        v.tpe match {
          case Type.I64 => v
          case _ => castValue(v, Type.I64, fb)
        }

      case SimpleType.Object =>
        v.tpe match {
          case Type.I64 => v
          case _ => castValue(v, Type.I64, fb)
        }

      case _ =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("ptrtoint", Type.I64, v))
        tmp
    }

    private def unboxFromI64(payload: Value, tpe: SimpleType, fb: FunBuilder): Value = tpe match {
      case SimpleType.Unit =>
        Value.IntConst(0L, Type.I64)

      case SimpleType.Bool =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I1, payload))
        tmp

      case SimpleType.Char =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, payload))
        tmp

      case SimpleType.Int8 =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I8, payload))
        tmp

      case SimpleType.Int16 =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I16, payload))
        tmp

      case SimpleType.Int32 =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, payload))
        tmp

      case SimpleType.Int64 =>
        payload

      case SimpleType.Float32 =>
        val asI32 = freshTmp(Type.I32)
        fb.current.emitAssign(asI32, Op.Cast("trunc", Type.I32, payload))

        val asF32 = freshTmp(Type.Float)
        fb.current.emitAssign(asF32, Op.Cast("bitcast", Type.Float, asI32))
        asF32

      case SimpleType.Float64 =>
        val asF64 = freshTmp(Type.Double)
        fb.current.emitAssign(asF64, Op.Cast("bitcast", Type.Double, payload))
        asF64

      case SimpleType.Null =>
        Value.Null(Type.Ptr)

      case SimpleType.AnyType =>
        payload

      case SimpleType.Object =>
        payload

      case _ =>
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Cast("inttoptr", Type.Ptr, payload))
        tmp
    }

    /**
      * Converts a boxed (erased) value payload to the export ABI payload for an immediate return type.
      *
      * Internal invariant (post-Eraser): defs return boxed values (typically `Object`) and primitives are boxed as
      * heap objects. For `@Export` we want to expose immediate values as raw `i64` payload bits.
      */
    private def exportUnboxValuePayload(payload: Value, unboxedTpe: SimpleType, fb: FunBuilder): Value = unboxedTpe match {
      case SimpleType.Unit =>
        // Unit is represented as 0 and does not allocate a box object.
        Value.IntConst(0L, Type.I64)

      case SimpleType.Bool =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_bool", List(payload)))
        tmp

      case SimpleType.Char =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_char", List(payload)))
        tmp

      case SimpleType.Int8 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int8", List(payload)))
        tmp

      case SimpleType.Int16 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int16", List(payload)))
        tmp

      case SimpleType.Int32 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int32", List(payload)))
        tmp

      case SimpleType.Int64 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int64", List(payload)))
        tmp

      case SimpleType.Float32 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_float32", List(payload)))
        tmp

      case SimpleType.Float64 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_float64", List(payload)))
        tmp

      case _ =>
        // Should never be called for non-immediates.
        payload
    }

	    private def isImmediateType(tpe: SimpleType): Boolean = tpe match {
	      case SimpleType.Unit | SimpleType.Bool | SimpleType.Char |
	           SimpleType.Int8 | SimpleType.Int16 | SimpleType.Int32 | SimpleType.Int64 |
	           SimpleType.Float32 | SimpleType.Float64 =>
	        true
	      case _ =>
	        false
	    }

	    private def isPointerLikeType(tpe: SimpleType): Boolean =
	      !isImmediateType(tpe)

		    /**
		      * Returns true iff a value of the given type is a GC-heap object pointer that must be
		      * reported via the explicit root stack (shadow stack).
		      *
		      * Note: `SimpleType` does not encode region information. This means we cannot reliably
		      * distinguish “GC heap pointers” from region-arena pointers at this phase.
		      *
		      * For correctness, we conservatively treat every non-immediate value as a GC root *candidate*.
		      * The current GC uses membership checks (tracked allocation set) to ignore non-GC pointers.
		      *
		      * Region→heap edges are still tracked via remembered sets; rooting a region pointer itself
		      * is harmless but does not replace remembered-set scanning.
		      */
		    private def isGcRootType(tpe: SimpleType): Boolean = tpe match {
		      case SimpleType.Void => false
		      case t if isImmediateType(t) => false
		      case _ => true
		    }

	    private def rootPushNameOf(valueTpe: Type): String = valueTpe match {
	      case Type.I64 => "flix_gc_push_root_value_i64"
	      case Type.Ptr => "flix_gc_push_root_ptr"
	      case other => throw new IllegalStateException(s"Unexpected root slot type: '$other'.")
	    }

    private def emitStorePtrLike(ctxPtr: Value, slotPtr: Value, payload: Value, fb: FunBuilder): Unit = {
      fb.current.emitCallVoid("flix_store_ptr", List(ctxPtr, slotPtr, payload))
    }

    private def emitExpr(exp0: Expr,
                         env: Map[Symbol.VarSym, Value],
                         ctxPtr: Value,
                         fb: FunBuilder,
                         lenv: Map[Symbol.LabelSym, String],
                         slotTypes: Map[Symbol.VarSym, Type],
                         selfTailLabel: Option[String],
                         exnHandlerOpt: Option[ExnHandler] = None): Value = exp0 match {
      case Expr.Cst(cst, _) =>
        emitConstant(cst, ctxPtr, fb)

      case Expr.Var(sym, tpe, _) =>
        slotTypes.get(sym) match {
          case None =>
            env.getOrElse(sym, Value.Undef(llvmTypeOf(tpe)))
          case Some(valueTpe) =>
            val slotPtr = env.getOrElse(sym, Value.Undef(Type.Ptr))
            val tmp = freshTmp(valueTpe)
            fb.current.emitAssign(tmp, Op.Load(valueTpe, slotPtr))
            tmp
        }

      case Expr.Let(sym, exp1, exp2, _) =>
        val v1 = emitExpr(exp1, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
        if (fb.current.isTerminated) {
          Value.Undef(llvmTypeOf(exp0.tpe))
        } else {
          slotTypes.get(sym) match {
            case Some(valueTpe) =>
              val slotPtr = env.getOrElse(sym, Value.Undef(Type.Ptr))
              val v1Coerced = coerceValue(v1, valueTpe, fb)
              fb.current.emitStore(v1Coerced, slotPtr)
              emitExpr(exp2, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
            case None =>
              val env1 = env.updated(sym, v1)
              emitExpr(exp2, env1, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
          }
        }

      case Expr.Stmt(exp1, exp2, _) =>
        emitExpr(exp1, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
        if (fb.current.isTerminated) Value.Undef(llvmTypeOf(exp0.tpe))
        else emitExpr(exp2, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)

      case Expr.Region(sym, exp, _, tpe, _, _) =>
        val joinTpe = llvmTypeOf(tpe)
        val endLabel = freshLabel("region_end")
        val handlerLabel = freshLabel("region_exn")

        // Enter the region and bind it for allocations/spawn in the body.
        val regionPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(regionPtr, Op.Call(Type.Ptr, "flix_region_enter", List(ctxPtr)))
        slotTypes.get(sym) match {
          case Some(_) =>
            val slotPtr = env.getOrElse(sym, Value.Undef(Type.Ptr))
            fb.current.emitStore(regionPtr, slotPtr)
          case None => ()
        }

        val exnSlotPtr = hoistAllocaI64(fb)
        val innerHandler = ExnHandler(handlerLabel, exnSlotPtr)

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        // Region body (may branch to handlerLabel via innerHandler).
        val bodyValue = emitExpr(exp, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, Some(innerHandler))
        if (!fb.current.isTerminated) {
          val bodyOutcome = packResult(bodyValue, tpe, fb)
          val bodyTag = freshTmp(Type.I64)
          fb.current.emitAssign(bodyTag, Op.ExtractValue(Type.I64, flixResultType, bodyOutcome, index = 0))
          val bodyPayload = freshTmp(Type.I64)
          fb.current.emitAssign(bodyPayload, Op.ExtractValue(Type.I64, flixResultType, bodyOutcome, index = 1))
          val exitTmp = freshTmp(flixResultType)
          fb.current.emitAssign(exitTmp, Op.Call(flixResultType, "flix_region_exit", List(ctxPtr, regionPtr, bodyTag, bodyPayload)))

          val payloadBits = unwindThunkToValuePayloadOrPropagateExn(exitTmp, ctxPtr, fb, exnHandlerOpt)
          val v = unboxFromI64(payloadBits, tpe, fb)
          val vCoerced = coerceValue(v, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vCoerced, predLabel))
        }

        // Exception handler: exit the region and propagate the chosen exception.
        val handlerBlock = fb.newBlock(handlerLabel)
        fb.setCurrent(handlerBlock)

        val exnBits = freshTmp(Type.I64)
        fb.current.emitAssign(exnBits, Op.Load(Type.I64, exnSlotPtr))
        val exnOutcome = packResultTagged(ResultTagException, exnBits, fb)
        val exnTag = freshTmp(Type.I64)
        fb.current.emitAssign(exnTag, Op.ExtractValue(Type.I64, flixResultType, exnOutcome, index = 0))
        val exnPayload = freshTmp(Type.I64)
        fb.current.emitAssign(exnPayload, Op.ExtractValue(Type.I64, flixResultType, exnOutcome, index = 1))

        val exitTmp2 = freshTmp(flixResultType)
        fb.current.emitAssign(exitTmp2, Op.Call(flixResultType, "flix_region_exit", List(ctxPtr, regionPtr, exnTag, exnPayload)))

        val payloadBits2 = unwindThunkToValuePayloadOrPropagateExn(exitTmp2, ctxPtr, fb, exnHandlerOpt)
        if (!fb.current.isTerminated) {
          val v = unboxFromI64(payloadBits2, tpe, fb)
          val vCoerced = coerceValue(v, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vCoerced, predLabel))
        }

        // Join.
        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        if (incomings.isEmpty) {
          endBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          endBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, _, _) =>
        val c0 = emitExpr(exp1, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
        if (fb.current.isTerminated) {
          return Value.Undef(llvmTypeOf(exp0.tpe))
        }
        val cond = coerceToI1(c0, fb)

        val thenLabel = freshLabel("then")
        val elseLabel = freshLabel("else")
        val endLabel = freshLabel("ifend")

        fb.current.setTerminator(Terminator.CondBr(cond, thenLabel, elseLabel))

        val joinTpe = llvmTypeOf(tpe)
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        // Then.
        val thenBlock = fb.newBlock(thenLabel)
        fb.setCurrent(thenBlock)
        val vThen = emitExpr(exp2, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
        if (!fb.current.isTerminated) {
          val vThenCoerced = coerceValue(vThen, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vThenCoerced, predLabel))
        }

        // Else.
        val elseBlock = fb.newBlock(elseLabel)
        fb.setCurrent(elseBlock)
        val vElse = emitExpr(exp3, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
        if (!fb.current.isTerminated) {
          val vElseCoerced = coerceValue(vElse, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vElseCoerced, predLabel))
        }

        // Join.
        val joinBlock = fb.newBlock(endLabel)
        fb.setCurrent(joinBlock)

        if (incomings.isEmpty) {
          // Both branches terminate => no control flow reaches the join.
          joinBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          joinBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.ApplyAtomic(op, exps, _, tpe, _, _) =>
        val argTpes = exps.map(_.tpe)
        emitExprs(exps, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt) match {
          case None => Value.Undef(llvmTypeOf(tpe))
          case Some(args) => emitApplyAtomic(op, argTpes, args, tpe, ctxPtr, fb, exnHandlerOpt)
        }

      case Expr.ApplyDef(sym, exps, ct, _, tpe, _, _) =>
        val fnName = LlvmNames.defName(sym)
        emitExprs(exps, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt) match {
          case None => Value.Undef(llvmTypeOf(tpe))
          case Some(args) =>
            ct match {
              case ExpPosition.Tail =>
                val thunkArgs = args.zip(exps).map {
                  case (v, e) => boxToI64(v, e.tpe, fb)
                }

                val thunkPtr = freshTmp(Type.Ptr)
                val thunkTi = Value.Global(LlvmNames.thunkTypeInfoName(sym), Type.Ptr)
                fb.current.emitAssign(thunkPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, thunkTi)))

                thunkArgs.zipWithIndex.foreach {
                  case (payload, i) =>
                    storeObjI64Slot(thunkPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
                }

                val result = packThunkResult(thunkPtr, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, result))
                Value.Undef(llvmTypeOf(tpe))

              case ExpPosition.NonTail =>
                val callTmp = freshTmp(flixResultType)
                fb.current.emitAssign(callTmp, Op.Call(flixResultType, fnName, ctxPtr :: args))

                val payload = unwindThunkToValuePayloadOrPropagateExn(callTmp, ctxPtr, fb, exnHandlerOpt)
                unboxFromI64(payload, tpe, fb)
            }
        }

      case Expr.ApplyClo(exp1, exp2, ct, _, tpe, _, _) =>
        val clo = emitExpr(exp1, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
        if (fb.current.isTerminated) {
          Value.Undef(llvmTypeOf(tpe))
        } else {
          val arg = emitExpr(exp2, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
          if (fb.current.isTerminated) {
            Value.Undef(llvmTypeOf(tpe))
          } else {
            ct match {
              case ExpPosition.Tail =>
                val thunkArgs = List(
                  castValue(clo, Type.I64, fb),
                  boxToI64(arg, exp2.tpe, fb)
                )

                val thunkPtr = freshTmp(Type.Ptr)
                val thunkTi = Value.Global(LlvmNames.thunkApplyClosureTypeInfoName(exp2.tpe), Type.Ptr)
                fb.current.emitAssign(thunkPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, thunkTi)))

                thunkArgs.zipWithIndex.foreach {
                  case (payload, i) =>
                    storeObjI64Slot(thunkPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
                }

                val result = packThunkResult(thunkPtr, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, result))
                Value.Undef(llvmTypeOf(tpe))

              case ExpPosition.NonTail =>
                val argBits = boxToI64(arg, exp2.tpe, fb)
                val callTmp = freshTmp(flixResultType)
                fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_invoke_thunk", List(ctxPtr, castValue(clo, Type.Ptr, fb), Value.IntConst(ResultTagValue, Type.I64), argBits)))

                val payload = unwindThunkToValuePayloadOrPropagateExn(callTmp, ctxPtr, fb, exnHandlerOpt)
                unboxFromI64(payload, tpe, fb)
            }
          }
        }

      case Expr.ApplySelfTail(sym, actuals, _, _, _) =>
        emitExprs(actuals, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt) match {
          case None =>
            Value.Undef(llvmTypeOf(exp0.tpe))
          case Some(args) =>
            val defn = root.defs(sym)
            defn.fparams.zip(args).foreach {
              case (fp, arg) =>
                val expectedTpe = slotTypes.getOrElse(fp.sym, llvmTypeOf(fp.tpe))
                val slotPtr = env.getOrElse(fp.sym, Value.Undef(Type.Ptr))
                val coercedArg = castValue(arg, expectedTpe, fb)
                fb.current.emitStore(coercedArg, slotPtr)
            }

            selfTailLabel match {
              case Some(lbl) => fb.current.setTerminator(Terminator.Br(lbl))
              case None =>
                fb.current.emitTrap()
                fb.current.setTerminator(Terminator.Unreachable)
            }

            Value.Undef(llvmTypeOf(exp0.tpe))
        }

      case Expr.Branch(exp, branches, tpe, _, _) =>
        val joinTpe = llvmTypeOf(tpe)
        val endLabel = freshLabel("branch_end")

        val branchLabels = branches.keys.map { sym =>
          sym -> freshLabel("branch")
        }.toMap
        val lenv1 = lenv ++ branchLabels

        val entryValue = emitExpr(exp, env, ctxPtr, fb, lenv1, slotTypes, selfTailLabel, exnHandlerOpt)

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        if (!fb.current.isTerminated) {
          val vEntry = coerceValue(entryValue, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vEntry, predLabel))
        }

        // Compile each branch block. Keep deterministic ordering by label id.
        val sortedBranches = branches.toList.sortBy(_._1.id)
        sortedBranches.foreach {
          case (sym, brExp) =>
            val label = branchLabels(sym)
            val b = fb.newBlock(label)
            fb.setCurrent(b)
            val v = emitExpr(brExp, env, ctxPtr, fb, lenv1, slotTypes, selfTailLabel, exnHandlerOpt)
            if (!fb.current.isTerminated) {
              val vCoerced = coerceValue(v, joinTpe, fb)
              val predLabel = fb.current.label
              fb.current.setTerminator(Terminator.Br(endLabel))
              incomings.addOne((vCoerced, predLabel))
            }
        }

        // Join.
        val joinBlock = fb.newBlock(endLabel)
        fb.setCurrent(joinBlock)
        if (incomings.isEmpty) {
          // No branch returns a value => no control flow reaches the join.
          joinBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          joinBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.JumpTo(sym, _, _, _) =>
        lenv.get(sym) match {
          case Some(lbl) =>
            // Pollcheck on (potential) loop backedges.
            fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
            val isCancelled = freshTmp(Type.I1)
            fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))

            val okLabel = freshLabel("jump_ok")
            val cancelLabel = freshLabel("jump_cancel")
            fb.current.setTerminator(Terminator.CondBr(isCancelled, cancelLabel, okLabel))

            val cancelBlock = fb.newBlock(cancelLabel)
            fb.setCurrent(cancelBlock)

            val cancelExnPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
            val tracedCancelExnPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
            val cancelBits = freshTmp(Type.I64)
            fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))

            exnHandlerOpt match {
              case Some(ExnHandler(handlerLabel, slotPtr)) =>
                fb.current.emitStore(cancelBits, slotPtr)
                fb.current.setTerminator(Terminator.Br(handlerLabel))
              case None =>
                val r = packResultTagged(ResultTagException, cancelBits, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, r))
            }

            val okBlock = fb.newBlock(okLabel)
            fb.setCurrent(okBlock)
            fb.current.setTerminator(Terminator.Br(lbl))
          case None =>
            fb.current.emitTrap()
            fb.current.setTerminator(Terminator.Unreachable)
        }
        Value.Undef(llvmTypeOf(exp0.tpe))

      case Expr.TryCatch(exp, rules, tpe, _, _) =>
        val joinTpe = llvmTypeOf(tpe)
        val endLabel = freshLabel("try_end")
        val handlerLabel = freshLabel("try_exn")

        val exnSlotPtr = hoistAllocaI64(fb)
        val innerHandler = ExnHandler(handlerLabel, exnSlotPtr)

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        // Try block (may branch to handlerLabel via innerHandler).
        val tryValue = emitExpr(exp, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, Some(innerHandler))
        if (!fb.current.isTerminated) {
          val vTry = coerceValue(tryValue, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vTry, predLabel))
        }

        // Handler entry: load exception payload bits and compute kind id.
        val handlerBlock = fb.newBlock(handlerLabel)
        fb.setCurrent(handlerBlock)

        val exnBits = freshTmp(Type.I64)
        fb.current.emitAssign(exnBits, Op.Load(Type.I64, exnSlotPtr))

        val exnPtr = castValue(exnBits, Type.Ptr, fb)
        val kindBits = loadObjI64Slot(exnPtr, Value.IntConst(1L, Type.I64), fb)

        def isCatchAll(catchTpe: SimpleType): Boolean = catchTpe match {
          case SimpleType.Enum(sym, Nil) => sym.text == "Exn" && sym.namespace.isEmpty
          case _ => false
        }

        // Ordered dispatch chain.
        val it = rules.iterator
        var done = false
        while (it.hasNext && !done) {
          val rule = it.next()
          val bodyLabel = freshLabel("catch_body")
          val nextLabel = freshLabel("catch_next")

          rule.catchTpe match {
            case catchTpe if isCatchAll(catchTpe) =>
              fb.current.setTerminator(Terminator.Br(bodyLabel))
              done = true
            case catchTpe =>
              val cmp = freshTmp(Type.I1)
              fb.current.emitAssign(cmp, Op.ICmp("eq", kindBits, Value.IntConst(ExnKindId.of(catchTpe).toLong, Type.I64)))
              fb.current.setTerminator(Terminator.CondBr(cmp, bodyLabel, nextLabel))
          }

          // Body block.
          val bodyBlock = fb.newBlock(bodyLabel)
          fb.setCurrent(bodyBlock)

          slotTypes.get(rule.sym) match {
            case Some(_) =>
              val slotPtr = env.getOrElse(rule.sym, Value.Undef(Type.Ptr))
              fb.current.emitStore(exnBits, slotPtr)
            case None => ()
          }
          val vBody = emitExpr(rule.exp, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
          if (!fb.current.isTerminated) {
            val vCoerced = coerceValue(vBody, joinTpe, fb)
            val predLabel = fb.current.label
            fb.current.setTerminator(Terminator.Br(endLabel))
            incomings.addOne((vCoerced, predLabel))
          }

          // Next test block (if any).
          if (!done) {
            val nextBlock = fb.newBlock(nextLabel)
            fb.setCurrent(nextBlock)
          }
        }

        if (!done) {
          // No rule matched: propagate exception to the outer handler (if present) or return it.
          exnHandlerOpt match {
            case Some(ExnHandler(label, slotPtr)) =>
              fb.current.emitStore(exnBits, slotPtr)
              fb.current.setTerminator(Terminator.Br(label))
            case None =>
              val r = packResultTagged(ResultTagException, exnBits, fb)
              fb.current.setTerminator(Terminator.Ret(flixResultType, r))
          }
        }

        // Join.
        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        if (incomings.isEmpty) {
          endBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          endBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.RunWith(exp, effUse, rules, ct, pcPointId, tpe, _, _) =>
        emitRunWithExpression(exp, effUse.sym, rules, ct, pcPointId, tpe, env, slotTypes, selfTailLabel, ctxPtr, fb, None, Map.empty, lenv, Value.IntConst(ResultTagValue, Type.I64), Value.Undef(Type.I64), Map.empty, exnHandlerOpt)

      case _ =>
        // Currently unsupported: emit a fail-fast trap.
        fb.current.emitTrap()
        Value.Undef(llvmTypeOf(exp0.tpe))
    }

    private def emitExprControlImpure(exp0: Expr,
                                     ctxPtr: Value,
                                     fb: FunBuilder,
                                     framePtr: Value,
                                     slotIndexOf: Map[Symbol.VarSym, Long],
                                     lenv: Map[Symbol.LabelSym, String],
                                     resumeTag: Value,
                                     resumePayload: Value,
                                     pcBlocks: Map[Int, BlockBuilder],
                                     exnHandlerOpt: Option[ExnHandler] = None): Value = exp0 match {
      case Expr.Cst(cst, _) =>
        emitConstant(cst, ctxPtr, fb)

      case Expr.Var(sym, tpe, _) =>
        val idx = slotIndexOf.getOrElse(sym, -1L)
        if (idx < 0) {
          fb.current.emitTrap()
          Value.Undef(llvmTypeOf(tpe))
        } else {
          val payload = loadObjI64Slot(framePtr, Value.IntConst(idx, Type.I64), fb)
          unboxFromI64(payload, tpe, fb)
        }

      case Expr.Let(sym, exp1, exp2, _) =>
        val v1 = emitExprControlImpure(exp1, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
        if (fb.current.isTerminated) {
          Value.Undef(llvmTypeOf(exp0.tpe))
        } else {
          val idx = slotIndexOf.getOrElse(sym, -1L)
          if (idx < 0) {
            fb.current.emitTrap()
            Value.Undef(llvmTypeOf(exp0.tpe))
          } else {
            val payload = boxToI64(v1, exp1.tpe, fb)
            storeObjI64Slot(framePtr, Value.IntConst(idx, Type.I64), payload, fb)
            emitExprControlImpure(exp2, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
          }
        }

      case Expr.Stmt(exp1, exp2, _) =>
        emitExprControlImpure(exp1, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
        if (fb.current.isTerminated) Value.Undef(llvmTypeOf(exp0.tpe))
        else emitExprControlImpure(exp2, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

      case Expr.Region(sym, exp, pcPointId, tpe, _, _) =>
        val joinTpe = llvmTypeOf(tpe)
        val endLabel = freshLabel("region_end")
        val handlerLabel = freshLabel("region_exn")

        val idx = slotIndexOf.getOrElse(sym, -1L)
        if (idx < 0) {
          fb.current.emitTrap()
          return Value.Undef(joinTpe)
        }

        // Enter region and bind it in the frame slot for the body.
        val regionPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(regionPtr, Op.Call(Type.Ptr, "flix_region_enter", List(ctxPtr)))
        val regionBits = boxToI64(regionPtr, SimpleType.Region, fb)
        storeObjI64Slot(framePtr, Value.IntConst(idx, Type.I64), regionBits, fb)

        val exnSlotPtr = hoistAllocaI64(fb)
        val innerHandler = ExnHandler(handlerLabel, exnSlotPtr)

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        def emitRegionExitAttempt(bodyTag: Value, bodyPayload: Value): Unit = {
          val exitTmp = freshTmp(flixResultType)
          // The region scope may cross suspension/resumption boundaries. The SSA `regionPtr` is not
          // reliable across resumptions, so reload the region pointer from the frame slot.
          val regionBits0 = loadObjI64Slot(framePtr, Value.IntConst(idx, Type.I64), fb)
          val regionPtr0 = unboxFromI64(regionBits0, SimpleType.Region, fb)
          fb.current.emitAssign(exitTmp, Op.Call(flixResultType, "flix_region_exit", List(ctxPtr, regionPtr0, bodyTag, bodyPayload)))

          if (pcPointId <= 0) {
            val payloadBits = unwindThunkToValuePayloadOrPropagateExn(exitTmp, ctxPtr, fb, exnHandlerOpt)
            if (!fb.current.isTerminated) {
              val v = unboxFromI64(payloadBits, tpe, fb)
              val vCoerced = coerceValue(v, joinTpe, fb)
              val predLabel = fb.current.label
              fb.current.setTerminator(Terminator.Br(endLabel))
              incomings.addOne((vCoerced, predLabel))
            }
            return
          }

          // On cooperative runtimes (wasm), region exit may itself suspend while joining children.
          // This uses `pcPointId` to resume by retrying `flix_region_exit` (which is idempotent once
          // the region is in Closing state).
          val r = unwindThunkToResult(exitTmp, ctxPtr, fb)
          val tag = freshTmp(Type.I64)
          fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, r, index = 0))

          val isValue = freshTmp(Type.I1)
          fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

          val valueLabel = freshLabel("region_exit_value")
          val notValueLabel = freshLabel("region_exit_not_value")
          fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, notValueLabel))

          val valueBlock = fb.newBlock(valueLabel)
          fb.setCurrent(valueBlock)
          val payloadBits = freshTmp(Type.I64)
          valueBlock.emitAssign(payloadBits, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
          val v = unboxFromI64(payloadBits, tpe, fb)
          val vCoerced = coerceValue(v, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vCoerced, predLabel))

          val notValueBlock = fb.newBlock(notValueLabel)
          fb.setCurrent(notValueBlock)
          val isSusp = freshTmp(Type.I1)
          fb.current.emitAssign(isSusp, Op.ICmp("eq", tag, Value.IntConst(ResultTagSuspension, Type.I64)))
          val suspLabel = freshLabel("region_exit_susp")
          val exnLabel = freshLabel("region_exit_exn")
          fb.current.setTerminator(Terminator.CondBr(isSusp, suspLabel, exnLabel))

          val suspBlock = fb.newBlock(suspLabel)
          fb.setCurrent(suspBlock)

          // Attach current frame as a prefix frame and return the suspension.
          val suspPayload = freshTmp(Type.I64)
          fb.current.emitAssign(suspPayload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
          val suspPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(suspPtr, Op.Cast("inttoptr", Type.Ptr, suspPayload))

          val oldPrefixBits = loadObjI64Slot(suspPtr, Value.IntConst(2L, Type.I64), fb)
          val oldPrefixPtr = castValue(oldPrefixBits, Type.Ptr, fb)

          storeObjI64Slot(framePtr, Value.IntConst(0L, Type.I64), Value.IntConst(pcPointId.toLong, Type.I64), fb)

          val newPrefixPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(newPrefixPtr, Op.Call(Type.Ptr, "flix_frames_push", List(framePtr, oldPrefixPtr)))
          val newPrefixBits = freshTmp(Type.I64)
          fb.current.emitAssign(newPrefixBits, Op.Cast("ptrtoint", Type.I64, newPrefixPtr))
          storeObjI64Slot(suspPtr, Value.IntConst(2L, Type.I64), newPrefixBits, fb)

          fb.current.setTerminator(Terminator.Ret(flixResultType, r))

          val exnBlock = fb.newBlock(exnLabel)
          fb.setCurrent(exnBlock)
          val isExn = freshTmp(Type.I1)
          fb.current.emitAssign(isExn, Op.ICmp("eq", tag, Value.IntConst(ResultTagException, Type.I64)))
          val exnOkLabel = freshLabel("region_exit_exn_ok")
          val exnBadLabel = freshLabel("region_exit_exn_bad")
          fb.current.setTerminator(Terminator.CondBr(isExn, exnOkLabel, exnBadLabel))

          val exnOkBlock = fb.newBlock(exnOkLabel)
          fb.setCurrent(exnOkBlock)
          exnHandlerOpt match {
            case Some(ExnHandler(label, slotPtr)) =>
              val exnPayloadBits = freshTmp(Type.I64)
              fb.current.emitAssign(exnPayloadBits, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
              fb.current.emitStore(exnPayloadBits, slotPtr)
              fb.current.setTerminator(Terminator.Br(label))
            case None =>
              fb.current.setTerminator(Terminator.Ret(flixResultType, r))
          }

          val exnBadBlock = fb.newBlock(exnBadLabel)
          fb.setCurrent(exnBadBlock)
          fb.current.emitTrap()
          fb.current.setTerminator(Terminator.Unreachable)
        }

        if (pcPointId > 0) {
          // Populate the resume pc block for region-exit join.
          val resumeBlock = pcBlocks.getOrElse(pcPointId, throw new IllegalStateException(s"missing pc block: $pcPointId"))
          if (resumeBlock.isTerminated) {
            throw new IllegalStateException(s"pc block $pcPointId already terminated")
          }
          val saved = fb.current
          fb.setCurrent(resumeBlock)

          // Region exit has already stored the body outcome on the first attempt, so we can pass dummy args.
          emitRegionExitAttempt(Value.IntConst(0L, Type.I64), Value.IntConst(0L, Type.I64))

          fb.setCurrent(saved)
        }

        // Region body (may branch to handlerLabel via innerHandler).
        val bodyValue = emitExprControlImpure(exp, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, Some(innerHandler))
        if (!fb.current.isTerminated) {
          val bodyOutcome = packResult(bodyValue, tpe, fb)
          val bodyTag = freshTmp(Type.I64)
          fb.current.emitAssign(bodyTag, Op.ExtractValue(Type.I64, flixResultType, bodyOutcome, index = 0))
          val bodyPayload = freshTmp(Type.I64)
          fb.current.emitAssign(bodyPayload, Op.ExtractValue(Type.I64, flixResultType, bodyOutcome, index = 1))
          emitRegionExitAttempt(bodyTag, bodyPayload)
        }

        // Exception handler: exit the region and propagate the chosen exception.
        val handlerBlock = fb.newBlock(handlerLabel)
        fb.setCurrent(handlerBlock)

        val exnBits = freshTmp(Type.I64)
        fb.current.emitAssign(exnBits, Op.Load(Type.I64, exnSlotPtr))
        val exnOutcome = packResultTagged(ResultTagException, exnBits, fb)
        val exnTag = freshTmp(Type.I64)
        fb.current.emitAssign(exnTag, Op.ExtractValue(Type.I64, flixResultType, exnOutcome, index = 0))
        val exnPayload = freshTmp(Type.I64)
        fb.current.emitAssign(exnPayload, Op.ExtractValue(Type.I64, flixResultType, exnOutcome, index = 1))
        emitRegionExitAttempt(exnTag, exnPayload)

        // Join.
        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        if (incomings.isEmpty) {
          endBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          endBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, _, _) =>
        val c0 = emitExprControlImpure(exp1, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
        if (fb.current.isTerminated) return Value.Undef(llvmTypeOf(exp0.tpe))
        val cond = coerceToI1(c0, fb)

        val thenLabel = freshLabel("then")
        val elseLabel = freshLabel("else")
        val endLabel = freshLabel("ifend")

        fb.current.setTerminator(Terminator.CondBr(cond, thenLabel, elseLabel))

        val joinTpe = llvmTypeOf(tpe)
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        val thenBlock = fb.newBlock(thenLabel)
        fb.setCurrent(thenBlock)
        val vThen = emitExprControlImpure(exp2, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
        if (!fb.current.isTerminated) {
          val vThenCoerced = coerceValue(vThen, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vThenCoerced, predLabel))
        }

        val elseBlock = fb.newBlock(elseLabel)
        fb.setCurrent(elseBlock)
        val vElse = emitExprControlImpure(exp3, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
        if (!fb.current.isTerminated) {
          val vElseCoerced = coerceValue(vElse, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vElseCoerced, predLabel))
        }

        val joinBlock = fb.newBlock(endLabel)
        fb.setCurrent(joinBlock)
        if (incomings.isEmpty) {
          joinBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          joinBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.ApplyAtomic(op, exps, pcPointId, tpe, purity, _) =>
        val argTpes = exps.map(_.tpe)
        emitExprsControlImpure(exps, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt) match {
          case None => Value.Undef(llvmTypeOf(tpe))
          case Some(args) =>
            target match {
              case CompilationTarget.LlvmNative if pcPointId > 0 =>
                op match {
                  case AtomicOp.Unary(SemanticOp.IoOp.SleepMillis) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Int64))))
                    val ms = castValue(x, Type.I64, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_sleep_millis_resumable", List(ctxPtr, ms)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.HttpRequest) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val method = loadTupleElement(x, 0L, SimpleType.String, fb)
                    val url = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val headers = loadTupleElement(x, 2L, SimpleType.Array(SimpleType.String), fb)
                    val hasBody = loadTupleElement(x, 3L, SimpleType.Bool, fb)
                    val body = loadTupleElement(x, 4L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_http_request_resumable", List(ctxPtr, method, url, headers, hasBody, body)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileRead) =>
                    val path = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.String))))
                    val pathPtr = castValue(path, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_read_resumable", List(ctxPtr, pathPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileReadLines) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val rc = loadTupleElement(x, 0L, SimpleType.Region, fb)
                    val path = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_read_lines_resumable", List(ctxPtr, rc, path)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileReadBytes) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val rc = loadTupleElement(x, 0L, SimpleType.Region, fb)
                    val path = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_read_bytes_resumable", List(ctxPtr, rc, path)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileList) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val rc = loadTupleElement(x, 0L, SimpleType.Region, fb)
                    val path = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_list_resumable", List(ctxPtr, rc, path)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileWrite) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val data = loadTupleElement(x, 0L, SimpleType.String, fb)
                    val path = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_write_resumable", List(ctxPtr, data, path)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileWriteBytes) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val bytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
                    val path = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_write_bytes_resumable", List(ctxPtr, bytes, path)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileAppend) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val data = loadTupleElement(x, 0L, SimpleType.String, fb)
                    val path = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_append_resumable", List(ctxPtr, data, path)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.FileAppendBytes) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val bytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
                    val path = loadTupleElement(x, 1L, SimpleType.String, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_file_append_bytes_resumable", List(ctxPtr, bytes, path)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.TcpSocketConnect) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val ipBytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
                    val port = loadTupleElement(x, 1L, SimpleType.Int32, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_tcp_socket_connect_resumable", List(ctxPtr, ipBytes, port)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.TcpSocketRead) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
                    val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_tcp_socket_read_resumable", List(ctxPtr, id, buf)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.TcpSocketWrite) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
                    val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_tcp_socket_write_resumable", List(ctxPtr, id, buf)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.TcpServerAccept) =>
                    val id = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val serverId = castValue(id, Type.I64, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_tcp_server_accept_resumable", List(ctxPtr, serverId)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.ProcessWaitFor) =>
                    val id = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Int64))))
                    val processId = castValue(id, Type.I64, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_process_wait_for_resumable", List(ctxPtr, processId)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.ProcessWaitForTimeout) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
                    val timeoutMs = loadTupleElement(x, 1L, SimpleType.Int64, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_process_wait_for_timeout_resumable", List(ctxPtr, id, timeoutMs)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.ProcessStdinWrite) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
                    val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_process_stdin_write_resumable", List(ctxPtr, id, buf)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.ProcessStdoutRead) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
                    val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_process_stdout_read_resumable", List(ctxPtr, id, buf)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.Unary(SemanticOp.IoOp.ProcessStderrRead) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
                    val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_process_stderr_read_resumable", List(ctxPtr, id, buf)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ChannelPut =>
                    val chan0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val v0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I64))
                    val vTpe = argTpes.drop(1).headOption.getOrElse(SimpleType.Object)

                    val chanPtr = castValue(chan0, Type.Ptr, fb)
                    val payload = boxToI64(v0, vTpe, fb)

                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_channel_put_resumable", List(ctxPtr, chanPtr, payload)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ChannelGet =>
                    val chan0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val chanPtr = castValue(chan0, Type.Ptr, fb)

                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_channel_get_resumable", List(ctxPtr, chanPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ChannelSelect =>
                    val blocking0 = args.lastOption.getOrElse(Value.Undef(Type.I1))
                    val channelArgs = args.dropRight(1)
                    val channelsPtr = emitTempPtrArray(channelArgs, fb)
                    val count = Value.IntConst(channelArgs.length.toLong, Type.I32)
                    val blocking = castValue(blocking0, Type.I1, fb)

                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_channel_select_resumable", List(ctxPtr, channelsPtr, count, blocking)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ReentrantLockLock =>
                    val lock0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val lockPtr = castValue(lock0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_reentrant_lock_lock_resumable", List(ctxPtr, lockPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ConditionAwait =>
                    val condition0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val conditionPtr = castValue(condition0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_condition_await_resumable", List(ctxPtr, conditionPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.CyclicBarrierAwait =>
                    val barrier0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val barrierPtr = castValue(barrier0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_cyclic_barrier_await_resumable", List(ctxPtr, barrierPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.CountDownLatchAwait =>
                    val latch0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val latchPtr = castValue(latch0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_count_down_latch_await_resumable", List(ctxPtr, latchPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.SemaphoreAcquire =>
                    val sem0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val semPtr = castValue(sem0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_semaphore_acquire_resumable", List(ctxPtr, semPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case _ =>
                    emitApplyAtomic(op, argTpes, args, tpe, ctxPtr, fb, exnHandlerOpt)
                }
              case CompilationTarget.LlvmWasm if pcPointId > 0 =>
                op match {
                  case AtomicOp.Unary(sop) if isSuspendableWasmIoOp(sop) =>
                    val x = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
                    emitApplyWasmIoOpSuspension(sop, x, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
                  case AtomicOp.ChannelPut =>
                    val chan0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val v0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I64))
                    val vTpe = argTpes.drop(1).headOption.getOrElse(SimpleType.Object)

                    val chanPtr = castValue(chan0, Type.Ptr, fb)
                    val payload = boxToI64(v0, vTpe, fb)

                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_channel_put_resumable", List(ctxPtr, chanPtr, payload)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ChannelGet =>
                    val chan0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val chanPtr = castValue(chan0, Type.Ptr, fb)

                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_channel_get_resumable", List(ctxPtr, chanPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ChannelSelect =>
                    val blocking0 = args.lastOption.getOrElse(Value.Undef(Type.I1))
                    val channelArgs = args.dropRight(1)
                    val channelsPtr = emitTempPtrArray(channelArgs, fb)
                    val count = Value.IntConst(channelArgs.length.toLong, Type.I32)
                    val blocking = castValue(blocking0, Type.I1, fb)

                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_channel_select_resumable", List(ctxPtr, channelsPtr, count, blocking)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ReentrantLockLock =>
                    val lock0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val lockPtr = castValue(lock0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_reentrant_lock_lock_resumable", List(ctxPtr, lockPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.ConditionAwait =>
                    val condition0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val conditionPtr = castValue(condition0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_condition_await_resumable", List(ctxPtr, conditionPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.CyclicBarrierAwait =>
                    val barrier0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val barrierPtr = castValue(barrier0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_cyclic_barrier_await_resumable", List(ctxPtr, barrierPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.CountDownLatchAwait =>
                    val latch0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val latchPtr = castValue(latch0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_count_down_latch_await_resumable", List(ctxPtr, latchPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case AtomicOp.SemaphoreAcquire =>
                    val sem0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
                    val semPtr = castValue(sem0, Type.Ptr, fb)
                    val callTmp = freshTmp(flixResultType)
                    fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_semaphore_acquire_resumable", List(ctxPtr, semPtr)))
                    emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

                  case _ =>
                    emitApplyAtomic(op, argTpes, args, tpe, ctxPtr, fb, exnHandlerOpt)
                }
              case _ =>
                emitApplyAtomic(op, argTpes, args, tpe, ctxPtr, fb, exnHandlerOpt)
            }
        }

      case Expr.ApplyDef(sym, exps, ct, pcPointId, tpe, _, _) =>
        val fnName = LlvmNames.defName(sym)
        emitExprsControlImpure(exps, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt) match {
          case None => Value.Undef(llvmTypeOf(tpe))
          case Some(args) =>
            ct match {
              case ExpPosition.Tail =>
                val thunkArgs = args.zip(exps).map {
                  case (v, e) => boxToI64(v, e.tpe, fb)
                }

                val thunkPtr = freshTmp(Type.Ptr)
                val thunkTi = Value.Global(LlvmNames.thunkTypeInfoName(sym), Type.Ptr)
                fb.current.emitAssign(thunkPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, thunkTi)))

                thunkArgs.zipWithIndex.foreach {
                  case (payload, i) =>
                    storeObjI64Slot(thunkPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
                }

                val result = packThunkResult(thunkPtr, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, result))
                Value.Undef(llvmTypeOf(tpe))

              case ExpPosition.NonTail =>
                val callTmp = freshTmp(flixResultType)
                fb.current.emitAssign(callTmp, Op.Call(flixResultType, fnName, ctxPtr :: args))

                if (pcPointId > 0) {
                  emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
                } else {
                  val payload = unwindThunkToValuePayloadOrPropagateExn(callTmp, ctxPtr, fb, exnHandlerOpt)
                  unboxFromI64(payload, tpe, fb)
                }
            }
        }

      case Expr.ApplyClo(exp1, exp2, ct, pcPointId, tpe, purity, _) =>
        // In the resumable evaluator, blocks that follow a suspension point are only reachable from
        // the corresponding resume pc-block, and thus do not dominate values computed earlier in the
        // pre-suspension path. For applications where the argument may suspend, we must ensure that
        // the closure value is (re)materialized in the post-resume path.
        //
        // We rely on the (current) LoweredAst invariant that `exp1` is pure/atomic in such cases
        // (e.g. a `Var` or `AtomicOp.Closure`) and thus safe to evaluate after the argument.
        val argMaySuspend = maySuspend(exp2)
        val reorderOk = ca.uwaterloo.flix.language.ast.Purity.isPure(exp1.purity)
        if (argMaySuspend && !reorderOk) {
          // Correct implementation requires spilling `exp1` into the frame across the suspension.
          // We do not support that yet; fail fast rather than miscompiling.
          fb.current.emitTrap()
          Value.Undef(llvmTypeOf(tpe))
        } else {
          val (clo, arg) =
            if (argMaySuspend) {
              val a = emitExprControlImpure(exp2, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
              if (fb.current.isTerminated) {
                (Value.Undef(Type.Ptr), Value.Undef(llvmTypeOf(exp2.tpe)))
              } else {
                val c = emitExprControlImpure(exp1, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
                (c, a)
              }
            } else {
              val c = emitExprControlImpure(exp1, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
              if (fb.current.isTerminated) {
                (Value.Undef(Type.Ptr), Value.Undef(llvmTypeOf(exp2.tpe)))
              } else {
                val a = emitExprControlImpure(exp2, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
                (c, a)
              }
            }

          if (fb.current.isTerminated) {
            Value.Undef(llvmTypeOf(tpe))
          } else {
            ct match {
              case ExpPosition.Tail =>
                val thunkArgs = List(
                  castValue(clo, Type.I64, fb),
                  boxToI64(arg, exp2.tpe, fb)
                )

                val thunkPtr = freshTmp(Type.Ptr)
                val thunkTi = Value.Global(LlvmNames.thunkApplyClosureTypeInfoName(exp2.tpe), Type.Ptr)
                fb.current.emitAssign(thunkPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, thunkTi)))

                thunkArgs.zipWithIndex.foreach {
                  case (payload, i) =>
                    storeObjI64Slot(thunkPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
                }

                val result = packThunkResult(thunkPtr, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, result))
                Value.Undef(llvmTypeOf(tpe))

              case ExpPosition.NonTail =>
                val argBits = boxToI64(arg, exp2.tpe, fb)
                val callTmp = freshTmp(flixResultType)
                  fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_invoke_thunk", List(ctxPtr, castValue(clo, Type.Ptr, fb), Value.IntConst(ResultTagValue, Type.I64), argBits)))

                if (pcPointId > 0) {
                  emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
                } else {
                  val payload = unwindThunkToValuePayloadOrPropagateExn(callTmp, ctxPtr, fb, exnHandlerOpt)
                  unboxFromI64(payload, tpe, fb)
                }
            }
          }
        }

      case Expr.ApplyOp(sym, exps, pcPointId, tpe, _, _) =>
        emitApplyOpSuspension(sym, exps, pcPointId, tpe, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

      case Expr.ApplySelfTail(sym, actuals, _, _, _) =>
        emitExprsControlImpure(actuals, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt) match {
          case None => Value.Undef(llvmTypeOf(exp0.tpe))
          case Some(args) =>
            val defn = root.defs(sym)
            defn.fparams.zip(args).foreach {
              case (fp, arg0) =>
                val idx = slotIndexOf.getOrElse(fp.sym, -1L)
                if (idx >= 0) {
                  val payload = boxToI64(arg0, fp.tpe, fb)
                  storeObjI64Slot(framePtr, Value.IntConst(idx, Type.I64), payload, fb)
                }
            }
            storeObjI64Slot(framePtr, Value.IntConst(0L, Type.I64), Value.IntConst(0L, Type.I64), fb)
            fb.current.setTerminator(Terminator.Br("pc_0"))
            Value.Undef(llvmTypeOf(exp0.tpe))
        }

      case Expr.Branch(exp, branches, tpe, _, _) =>
        val joinTpe = llvmTypeOf(tpe)
        val endLabel = freshLabel("branch_end")

        val branchLabels = branches.keys.map { sym =>
          sym -> freshLabel("branch")
        }.toMap
        val lenv1 = lenv ++ branchLabels

        val entryValue = emitExprControlImpure(exp, ctxPtr, fb, framePtr, slotIndexOf, lenv1, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        if (!fb.current.isTerminated) {
          val vEntry = coerceValue(entryValue, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vEntry, predLabel))
        }

        val sortedBranches = branches.toList.sortBy(_._1.id)
        sortedBranches.foreach {
          case (sym, brExp) =>
            val label = branchLabels(sym)
            val b = fb.newBlock(label)
            fb.setCurrent(b)
            val v = emitExprControlImpure(brExp, ctxPtr, fb, framePtr, slotIndexOf, lenv1, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
            if (!fb.current.isTerminated) {
              val vCoerced = coerceValue(v, joinTpe, fb)
              val predLabel = fb.current.label
              fb.current.setTerminator(Terminator.Br(endLabel))
              incomings.addOne((vCoerced, predLabel))
            }
        }

        val joinBlock = fb.newBlock(endLabel)
        fb.setCurrent(joinBlock)
        if (incomings.isEmpty) {
          joinBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          joinBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.JumpTo(sym, _, _, _) =>
        lenv.get(sym) match {
          case Some(lbl) =>
            // Pollcheck on (potential) loop backedges.
            fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
            val isCancelled = freshTmp(Type.I1)
            fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))

            val okLabel = freshLabel("jump_ok")
            val cancelLabel = freshLabel("jump_cancel")
            fb.current.setTerminator(Terminator.CondBr(isCancelled, cancelLabel, okLabel))

            val cancelBlock = fb.newBlock(cancelLabel)
            fb.setCurrent(cancelBlock)

            val cancelExnPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
            val tracedCancelExnPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
            val cancelBits = freshTmp(Type.I64)
            fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))

            exnHandlerOpt match {
              case Some(ExnHandler(handlerLabel, slotPtr)) =>
                fb.current.emitStore(cancelBits, slotPtr)
                fb.current.setTerminator(Terminator.Br(handlerLabel))
              case None =>
                val r = packResultTagged(ResultTagException, cancelBits, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, r))
            }

            val okBlock = fb.newBlock(okLabel)
            fb.setCurrent(okBlock)
            fb.current.setTerminator(Terminator.Br(lbl))
          case None =>
            fb.current.emitTrap()
            fb.current.setTerminator(Terminator.Unreachable)
        }
        Value.Undef(llvmTypeOf(exp0.tpe))

      case Expr.TryCatch(exp, rules, tpe, _, _) =>
        val joinTpe = llvmTypeOf(tpe)
        val endLabel = freshLabel("try_end")
        val handlerLabel = freshLabel("try_exn")

        val exnSlotPtr = hoistAllocaI64(fb)
        val innerHandler = ExnHandler(handlerLabel, exnSlotPtr)

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        // Try block (may branch to handlerLabel via innerHandler).
        val tryValue = emitExprControlImpure(exp, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, Some(innerHandler))
        if (!fb.current.isTerminated) {
          val vTry = coerceValue(tryValue, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((vTry, predLabel))
        }

        // Handler entry: load exception payload bits and compute kind id.
        val handlerBlock = fb.newBlock(handlerLabel)
        fb.setCurrent(handlerBlock)

        val exnBits = freshTmp(Type.I64)
        fb.current.emitAssign(exnBits, Op.Load(Type.I64, exnSlotPtr))

        val exnPtr = castValue(exnBits, Type.Ptr, fb)
        val kindBits = loadObjI64Slot(exnPtr, Value.IntConst(1L, Type.I64), fb)

        def isCatchAll(catchTpe: SimpleType): Boolean = catchTpe match {
          case SimpleType.Enum(sym, Nil) => sym.text == "Exn" && sym.namespace.isEmpty
          case _ => false
        }

        // Ordered dispatch chain.
        val it = rules.iterator
        var done = false
        while (it.hasNext && !done) {
          val rule = it.next()
          val bodyLabel = freshLabel("catch_body")
          val nextLabel = freshLabel("catch_next")

          rule.catchTpe match {
            case catchTpe if isCatchAll(catchTpe) =>
              fb.current.setTerminator(Terminator.Br(bodyLabel))
              done = true
            case catchTpe =>
              val cmp = freshTmp(Type.I1)
              fb.current.emitAssign(cmp, Op.ICmp("eq", kindBits, Value.IntConst(ExnKindId.of(catchTpe).toLong, Type.I64)))
              fb.current.setTerminator(Terminator.CondBr(cmp, bodyLabel, nextLabel))
          }

          // Body block.
          val bodyBlock = fb.newBlock(bodyLabel)
          fb.setCurrent(bodyBlock)

          // Bind the exception value (Exn) to the catch binder.
          val idx = slotIndexOf.getOrElse(rule.sym, -1L)
          if (idx < 0) {
            fb.current.emitTrap()
            fb.current.setTerminator(Terminator.Unreachable)
          } else {
            storeObjI64Slot(framePtr, Value.IntConst(idx, Type.I64), exnBits, fb)
            val vBody = emitExprControlImpure(rule.exp, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
            if (!fb.current.isTerminated) {
              val vCoerced = coerceValue(vBody, joinTpe, fb)
              val predLabel = fb.current.label
              fb.current.setTerminator(Terminator.Br(endLabel))
              incomings.addOne((vCoerced, predLabel))
            }
          }

          // Next test block (if any).
          if (!done) {
            val nextBlock = fb.newBlock(nextLabel)
            fb.setCurrent(nextBlock)
          }
        }

        if (!done) {
          // No rule matched: propagate exception to the outer handler (if present) or return it.
          exnHandlerOpt match {
            case Some(ExnHandler(label, slotPtr)) =>
              fb.current.emitStore(exnBits, slotPtr)
              fb.current.setTerminator(Terminator.Br(label))
            case None =>
              val r = packResultTagged(ResultTagException, exnBits, fb)
              fb.current.setTerminator(Terminator.Ret(flixResultType, r))
          }
        }

        // Join.
        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        if (incomings.isEmpty) {
          endBlock.setTerminator(Terminator.Unreachable)
          Value.Undef(joinTpe)
        } else {
          val phiDest = freshTmp(joinTpe)
          endBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case Expr.RunWith(exp, effUse, rules, ct, pcPointId, tpe, _, _) =>
        emitRunWithExpression(exp, effUse.sym, rules, ct, pcPointId, tpe, Map.empty, Map.empty, None, ctxPtr, fb, Some(framePtr), slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)

      case _ =>
        fb.current.emitTrap()
        Value.Undef(llvmTypeOf(exp0.tpe))
    }

    private def emitExprsControlImpure(exps: List[Expr],
                                      ctxPtr: Value,
                                      fb: FunBuilder,
                                      framePtr: Value,
                                      slotIndexOf: Map[Symbol.VarSym, Long],
                                      lenv: Map[Symbol.LabelSym, String],
                                      resumeTag: Value,
                                      resumePayload: Value,
                                      pcBlocks: Map[Int, BlockBuilder],
                                      exnHandlerOpt: Option[ExnHandler] = None): Option[List[Value]] = {
      val buf = mutable.ListBuffer.empty[Value]
      val it = exps.iterator
      while (it.hasNext && !fb.current.isTerminated) {
        buf.addOne(emitExprControlImpure(it.next(), ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt))
      }
      if (fb.current.isTerminated) None else Some(buf.toList)
    }

    /**
      * Returns `true` if evaluating `exp0` may suspend and resume via a pc-point (wasm target).
      *
      * This is used to avoid emitting LLVM IR that uses SSA values computed on a pre-suspension
      * path in blocks that are only reachable from the corresponding resume pc-block.
      */
    private def maySuspend(exp0: Expr): Boolean = exp0 match {
      case Expr.Cst(_, _) => false
      case Expr.NativeImport(_, _, _, _) => false
      case Expr.WasmImport(_, _, _, _) => false
      case Expr.Var(_, _, _) => false
      case Expr.ApplyAtomic(_, exps, pcPointId, _, _, _) =>
        pcPointId > 0 || exps.exists(maySuspend)
      case Expr.ApplyClo(exp1, exp2, _, pcPointId, _, _, _) =>
        pcPointId > 0 || maySuspend(exp1) || maySuspend(exp2)
      case Expr.ApplyDef(_, exps, _, pcPointId, _, _, _) =>
        pcPointId > 0 || exps.exists(maySuspend)
      case Expr.ApplyOp(_, exps, pcPointId, _, _, _) =>
        pcPointId > 0 || exps.exists(maySuspend)
      case Expr.ApplySelfTail(_, actuals, _, _, _) =>
        actuals.exists(maySuspend)
      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
        maySuspend(exp1) || maySuspend(exp2) || maySuspend(exp3)
      case Expr.Branch(exp, branches, _, _, _) =>
        maySuspend(exp) || branches.values.exists(maySuspend)
      case Expr.JumpTo(_, _, _, _) =>
        false
      case Expr.Let(_, exp1, exp2, _) =>
        maySuspend(exp1) || maySuspend(exp2)
      case Expr.Stmt(exp1, exp2, _) =>
        maySuspend(exp1) || maySuspend(exp2)
      case Expr.Region(_, exp, pcPointId, _, _, _) =>
        pcPointId > 0 || maySuspend(exp)
      case Expr.TryCatch(exp, rules, _, _, _) =>
        maySuspend(exp) || rules.exists(r => maySuspend(r.exp))
      case Expr.RunWith(exp, _, rules, _, pcPointId, _, _, _) =>
        pcPointId > 0 || maySuspend(exp) || rules.exists(r => maySuspend(r.exp))
      case Expr.NewObject(_, _, _, _, methods, _) =>
        methods.exists(m => maySuspend(m.exp))
    }

    private def emitCallAndHandleSuspension(result0: Value,
                                           pcPointId: Int,
                                           expectedTpe: SimpleType,
                                           ctxPtr: Value,
                                           fb: FunBuilder,
                                           framePtr: Value,
                                           resumeTag: Value,
                                           resumePayload: Value,
                                           pcBlocks: Map[Int, BlockBuilder],
                                           exnHandlerOpt: Option[ExnHandler] = None): Value = {
      val r = unwindThunkToResult(result0, ctxPtr, fb)

      val tag = freshTmp(Type.I64)
      fb.current.emitAssign(tag, Op.ExtractValue(Type.I64, flixResultType, r, index = 0))

      val isValue = freshTmp(Type.I1)
      fb.current.emitAssign(isValue, Op.ICmp("eq", tag, Value.IntConst(ResultTagValue, Type.I64)))

      val valueLabel = freshLabel("call_value")
      val notValueLabel = freshLabel("call_not_value")
      fb.current.setTerminator(Terminator.CondBr(isValue, valueLabel, notValueLabel))

      val afterLabel = freshLabel("call_after")

      // Resume pc block.
      val resumeBlock = pcBlocks.getOrElse(pcPointId, throw new IllegalStateException(s"missing pc block: $pcPointId"))
      if (resumeBlock.isTerminated) {
        throw new IllegalStateException(s"pc block $pcPointId already terminated")
      }
      val (resumeValue, resumeValuePredLabel) = {
        val saved = fb.current
        fb.setCurrent(resumeBlock)

        val resumeIsValue = freshTmp(Type.I1)
        fb.current.emitAssign(resumeIsValue, Op.ICmp("eq", resumeTag, Value.IntConst(ResultTagValue, Type.I64)))
        val resumeValueLabel = freshLabel("resume_value")
        val resumeNotValueLabel = freshLabel("resume_not_value")
        fb.current.setTerminator(Terminator.CondBr(resumeIsValue, resumeValueLabel, resumeNotValueLabel))

        val resumeValueBlock = fb.newBlock(resumeValueLabel)
        fb.setCurrent(resumeValueBlock)

        // Pollcheck and cancellation at resume entry for normal value resumption.
        if (isGcRootType(expectedTpe)) {
          val resumeSlotPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(resumeSlotPtr, Op.Alloca(Type.I64))
          fb.current.emitStore(resumePayload, resumeSlotPtr)
          fb.current.emitCallVoid(rootPushNameOf(Type.I64), List(ctxPtr, resumeSlotPtr))
        }

        fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
        if (isGcRootType(expectedTpe)) {
          fb.current.emitCallVoid("flix_gc_pop_roots", List(ctxPtr, Value.IntConst(1L, Type.I64)))
        }

        val isCancelled = freshTmp(Type.I1)
        fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))
        val cancelLabel = freshLabel("resume_cancel")
        val resumeContinueLabel = freshLabel("resume_continue")
        fb.current.setTerminator(Terminator.CondBr(isCancelled, cancelLabel, resumeContinueLabel))

        val cancelBlock = fb.newBlock(cancelLabel)
        fb.setCurrent(cancelBlock)
        val cancelExnPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
        val tracedCancelExnPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
        val cancelBits = freshTmp(Type.I64)
        fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
        exnHandlerOpt match {
          case Some(ExnHandler(label, slotPtr)) =>
            fb.current.emitStore(cancelBits, slotPtr)
            fb.current.setTerminator(Terminator.Br(label))
          case None =>
            val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
            fb.current.setTerminator(Terminator.Ret(flixResultType, cancelResult))
        }

        val resumeContinueBlock = fb.newBlock(resumeContinueLabel)
        fb.setCurrent(resumeContinueBlock)
        val v = unboxFromI64(resumePayload, expectedTpe, fb)
        fb.current.setTerminator(Terminator.Br(afterLabel))

        val resumeNotValueBlock = fb.newBlock(resumeNotValueLabel)
        fb.setCurrent(resumeNotValueBlock)
        val resumeIsExn = freshTmp(Type.I1)
        fb.current.emitAssign(resumeIsExn, Op.ICmp("eq", resumeTag, Value.IntConst(ResultTagException, Type.I64)))
        val resumeExnLabel = freshLabel("resume_exn")
        val resumeBadLabel = freshLabel("resume_bad")
        fb.current.setTerminator(Terminator.CondBr(resumeIsExn, resumeExnLabel, resumeBadLabel))

        val resumeExnBlock = fb.newBlock(resumeExnLabel)
        fb.setCurrent(resumeExnBlock)
        exnHandlerOpt match {
          case Some(ExnHandler(label, slotPtr)) =>
            fb.current.emitStore(resumePayload, slotPtr)
            fb.current.setTerminator(Terminator.Br(label))
          case None =>
            val resumeExnResult = packResultTagged(ResultTagException, resumePayload, fb)
            fb.current.setTerminator(Terminator.Ret(flixResultType, resumeExnResult))
        }

        val resumeBadBlock = fb.newBlock(resumeBadLabel)
        fb.setCurrent(resumeBadBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)

        fb.setCurrent(saved)
        (v, resumeContinueBlock.label)
      }

      // VALUE path.
      val valueBlock = fb.newBlock(valueLabel)
      fb.setCurrent(valueBlock)
      val payload = freshTmp(Type.I64)
      fb.current.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
      if (isGcRootType(expectedTpe)) {
        val valueSlotPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(valueSlotPtr, Op.Alloca(Type.I64))
        fb.current.emitStore(payload, valueSlotPtr)
        fb.current.emitCallVoid(rootPushNameOf(Type.I64), List(ctxPtr, valueSlotPtr))
      }
      fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
      if (isGcRootType(expectedTpe)) {
        fb.current.emitCallVoid("flix_gc_pop_roots", List(ctxPtr, Value.IntConst(1L, Type.I64)))
      }

      val isCancelledValue = freshTmp(Type.I1)
      fb.current.emitAssign(isCancelledValue, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))
      val valueCancelLabel = freshLabel("call_value_cancel")
      val valueOkLabel = freshLabel("call_value_ok")
      fb.current.setTerminator(Terminator.CondBr(isCancelledValue, valueCancelLabel, valueOkLabel))

      val valueCancelBlock = fb.newBlock(valueCancelLabel)
      fb.setCurrent(valueCancelBlock)
      val valueCancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(valueCancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
      val tracedValueCancelExnPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(tracedValueCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(valueCancelExnPtr)))
      val valueCancelBits = freshTmp(Type.I64)
      fb.current.emitAssign(valueCancelBits, Op.Cast("ptrtoint", Type.I64, tracedValueCancelExnPtr))
      exnHandlerOpt match {
        case Some(ExnHandler(label, slotPtr)) =>
          fb.current.emitStore(valueCancelBits, slotPtr)
          fb.current.setTerminator(Terminator.Br(label))
        case None =>
          val valueCancelResult = packResultTagged(ResultTagException, valueCancelBits, fb)
          fb.current.setTerminator(Terminator.Ret(flixResultType, valueCancelResult))
      }

      val valueOkBlock = fb.newBlock(valueOkLabel)
      fb.setCurrent(valueOkBlock)
      val valueValue = unboxFromI64(payload, expectedTpe, fb)
      fb.current.setTerminator(Terminator.Br(afterLabel))

      // Non-VALUE path.
      val notValueBlock = fb.newBlock(notValueLabel)
      fb.setCurrent(notValueBlock)
      val isSusp = freshTmp(Type.I1)
      fb.current.emitAssign(isSusp, Op.ICmp("eq", tag, Value.IntConst(ResultTagSuspension, Type.I64)))
      val suspLabel = freshLabel("call_susp")
      val exnLabel = freshLabel("call_exn")
      fb.current.setTerminator(Terminator.CondBr(isSusp, suspLabel, exnLabel))

      val suspBlock = fb.newBlock(suspLabel)
      fb.setCurrent(suspBlock)
      // Attach current frame as a prefix frame and return the suspension.
      val suspPayload = freshTmp(Type.I64)
      fb.current.emitAssign(suspPayload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
      val suspPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(suspPtr, Op.Cast("inttoptr", Type.Ptr, suspPayload))

      val oldPrefixBits = loadObjI64Slot(suspPtr, Value.IntConst(2L, Type.I64), fb)
      val oldPrefixPtr = castValue(oldPrefixBits, Type.Ptr, fb)

      storeObjI64Slot(framePtr, Value.IntConst(0L, Type.I64), Value.IntConst(pcPointId.toLong, Type.I64), fb)

      val newPrefixPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(newPrefixPtr, Op.Call(Type.Ptr, "flix_frames_push", List(framePtr, oldPrefixPtr)))
      val newPrefixBits = freshTmp(Type.I64)
      fb.current.emitAssign(newPrefixBits, Op.Cast("ptrtoint", Type.I64, newPrefixPtr))
      storeObjI64Slot(suspPtr, Value.IntConst(2L, Type.I64), newPrefixBits, fb)

      fb.current.setTerminator(Terminator.Ret(flixResultType, r))

      val exnBlock = fb.newBlock(exnLabel)
      fb.setCurrent(exnBlock)
      val isExn = freshTmp(Type.I1)
      fb.current.emitAssign(isExn, Op.ICmp("eq", tag, Value.IntConst(ResultTagException, Type.I64)))
      val exnOkLabel = freshLabel("call_exn_ok")
      val exnBadLabel = freshLabel("call_exn_bad")
      fb.current.setTerminator(Terminator.CondBr(isExn, exnOkLabel, exnBadLabel))

      val exnOkBlock = fb.newBlock(exnOkLabel)
      fb.setCurrent(exnOkBlock)
      exnHandlerOpt match {
        case Some(ExnHandler(label, slotPtr)) =>
          val payload = freshTmp(Type.I64)
          fb.current.emitAssign(payload, Op.ExtractValue(Type.I64, flixResultType, r, index = 1))
          fb.current.emitStore(payload, slotPtr)
          fb.current.setTerminator(Terminator.Br(label))
        case None =>
          fb.current.setTerminator(Terminator.Ret(flixResultType, r))
      }

      val exnBadBlock = fb.newBlock(exnBadLabel)
      fb.setCurrent(exnBadBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      // Join.
      val afterBlock = fb.newBlock(afterLabel)
      fb.setCurrent(afterBlock)
      val joinTpe = llvmTypeOf(expectedTpe)
      val phiDest = freshTmp(joinTpe)
      afterBlock.emitPhi(phiDest, List((valueValue, valueOkBlock.label), (resumeValue, resumeValuePredLabel)))
      phiDest
    }

    private def emitApplyOpSuspension(sym: Symbol.OpSym,
                                     exps: List[Expr],
                                     pcPointId: Int,
                                     tpe: SimpleType,
                                     ctxPtr: Value,
                                     fb: FunBuilder,
                                     framePtr: Value,
                                     slotIndexOf: Map[Symbol.VarSym, Long],
                                     lenv: Map[Symbol.LabelSym, String],
                                     resumeTag: Value,
                                     resumePayload: Value,
                                     pcBlocks: Map[Int, BlockBuilder],
                                     exnHandlerOpt: Option[ExnHandler] = None): Value = {
      val effId = effectSymIds.getOrElse(sym.eff, 0L)
      val opIndex = opIndices.getOrElse(sym, -1)
      if (effId == 0L || opIndex < 0) {
        fb.current.emitTrap()
        return Value.Undef(llvmTypeOf(tpe))
      }

      emitExprsControlImpure(exps, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt) match {
        case None => Value.Undef(llvmTypeOf(tpe))
        case Some(args) =>
          val argPayloads = args.zip(exps).map {
            case (v, e) => boxToI64(v, e.tpe, fb)
          }

          // Set pc on the current frame.
          storeObjI64Slot(framePtr, Value.IntConst(0L, Type.I64), Value.IntConst(pcPointId.toLong, Type.I64), fb)

          // Create prefix frames list with this frame.
          val prefixPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(prefixPtr, Op.Call(Type.Ptr, "flix_frames_push", List(framePtr, Value.Null(Type.Ptr))))
          val prefixBits = freshTmp(Type.I64)
          fb.current.emitAssign(prefixBits, Op.Cast("ptrtoint", Type.I64, prefixPtr))

          // Allocate suspension object.
          val slots = 5L + argPayloads.length.toLong
          val payloadBytes = Value.IntConst(slots * 8L, Type.I64)
          val sizeBytes = freshTmp(Type.I64)
          fb.current.emitAssign(sizeBytes, Op.Bin("add", Type.I64, payloadBytes, Value.IntConst(flixObjHeaderBytes, Type.I64)))
          val suspPtr = freshTmp(Type.Ptr)
          val suspTi = Value.Global(LlvmNames.suspensionTypeInfoName, Type.Ptr)
          fb.current.emitAssign(suspPtr, Op.Call(Type.Ptr, "flix_alloc_flex", List(ctxPtr, suspTi, sizeBytes)))

          storeObjI64Slot(suspPtr, Value.IntConst(0L, Type.I64), Value.IntConst(effId, Type.I64), fb)
          storeObjI64Slot(suspPtr, Value.IntConst(1L, Type.I64), Value.IntConst(opIndex.toLong, Type.I64), fb)
          storeObjI64Slot(suspPtr, Value.IntConst(2L, Type.I64), prefixBits, fb)
          storeObjI64Slot(suspPtr, Value.IntConst(3L, Type.I64), Value.IntConst(0L, Type.I64), fb)
          storeObjI64Slot(suspPtr, Value.IntConst(4L, Type.I64), Value.IntConst(argPayloads.length.toLong, Type.I64), fb)
          argPayloads.zipWithIndex.foreach {
            case (p, i) =>
              storeObjI64Slot(suspPtr, Value.IntConst(5L + i.toLong, Type.I64), p, fb)
          }

          val suspBits = freshTmp(Type.I64)
          fb.current.emitAssign(suspBits, Op.Cast("ptrtoint", Type.I64, suspPtr))
          val r = packResultTagged(ResultTagSuspension, suspBits, fb)
          fb.current.setTerminator(Terminator.Ret(flixResultType, r))

          // Resume pc block.
          val afterLabel = freshLabel("do_after")
          val resumeBlock = pcBlocks.getOrElse(pcPointId, throw new IllegalStateException(s"missing pc block: $pcPointId"))
          if (resumeBlock.isTerminated) {
            throw new IllegalStateException(s"pc block $pcPointId already terminated")
          }
          val (resumedValue, resumedValuePredLabel) = {
            val saved = fb.current
            fb.setCurrent(resumeBlock)
            val resumeIsValue = freshTmp(Type.I1)
            fb.current.emitAssign(resumeIsValue, Op.ICmp("eq", resumeTag, Value.IntConst(ResultTagValue, Type.I64)))
            val resumeValueLabel = freshLabel("resume_value")
            val resumeNotValueLabel = freshLabel("resume_not_value")
            fb.current.setTerminator(Terminator.CondBr(resumeIsValue, resumeValueLabel, resumeNotValueLabel))

            val resumeValueBlock = fb.newBlock(resumeValueLabel)
            fb.setCurrent(resumeValueBlock)
            if (isGcRootType(tpe)) {
              val resumeSlotPtr = freshTmp(Type.Ptr)
              fb.current.emitAssign(resumeSlotPtr, Op.Alloca(Type.I64))
              fb.current.emitStore(resumePayload, resumeSlotPtr)
              fb.current.emitCallVoid(rootPushNameOf(Type.I64), List(ctxPtr, resumeSlotPtr))
            }

            fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
            if (isGcRootType(tpe)) {
              fb.current.emitCallVoid("flix_gc_pop_roots", List(ctxPtr, Value.IntConst(1L, Type.I64)))
            }

            val isCancelled = freshTmp(Type.I1)
            fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))
            val cancelLabel = freshLabel("resume_cancel")
            val resumeContinueLabel = freshLabel("resume_continue")
            fb.current.setTerminator(Terminator.CondBr(isCancelled, cancelLabel, resumeContinueLabel))

            val cancelBlock = fb.newBlock(cancelLabel)
            fb.setCurrent(cancelBlock)
            val cancelExnPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
            val tracedCancelExnPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
            val cancelBits = freshTmp(Type.I64)
            fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
            exnHandlerOpt match {
              case Some(ExnHandler(label, slotPtr)) =>
                fb.current.emitStore(cancelBits, slotPtr)
                fb.current.setTerminator(Terminator.Br(label))
              case None =>
                val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, cancelResult))
            }

            val resumeContinueBlock = fb.newBlock(resumeContinueLabel)
            fb.setCurrent(resumeContinueBlock)
            val v = unboxFromI64(resumePayload, tpe, fb)
            fb.current.setTerminator(Terminator.Br(afterLabel))

            val resumeNotValueBlock = fb.newBlock(resumeNotValueLabel)
            fb.setCurrent(resumeNotValueBlock)
            val resumeIsExn = freshTmp(Type.I1)
            fb.current.emitAssign(resumeIsExn, Op.ICmp("eq", resumeTag, Value.IntConst(ResultTagException, Type.I64)))
            val resumeExnLabel = freshLabel("resume_exn")
            val resumeBadLabel = freshLabel("resume_bad")
            fb.current.setTerminator(Terminator.CondBr(resumeIsExn, resumeExnLabel, resumeBadLabel))

            val resumeExnBlock = fb.newBlock(resumeExnLabel)
            fb.setCurrent(resumeExnBlock)
            exnHandlerOpt match {
              case Some(ExnHandler(label, slotPtr)) =>
                fb.current.emitStore(resumePayload, slotPtr)
                fb.current.setTerminator(Terminator.Br(label))
              case None =>
                val resumeExnResult = packResultTagged(ResultTagException, resumePayload, fb)
                fb.current.setTerminator(Terminator.Ret(flixResultType, resumeExnResult))
            }

            val resumeBadBlock = fb.newBlock(resumeBadLabel)
            fb.setCurrent(resumeBadBlock)
            fb.current.emitTrap()
            fb.current.setTerminator(Terminator.Unreachable)

            fb.setCurrent(saved)
            (v, resumeContinueBlock.label)
          }

          val afterBlock = fb.newBlock(afterLabel)
          fb.setCurrent(afterBlock)
          val joinTpe = llvmTypeOf(tpe)
          val phiDest = freshTmp(joinTpe)
          afterBlock.emitPhi(phiDest, List((resumedValue, resumedValuePredLabel)))
          phiDest
      }
    }

    /**
      * Creates a resumption-less suspension for a portable IO primop on wasm.
      *
      * The suspension is represented using the same runtime `Suspension` object layout as effect operations:
      * - effSymId is `0` (reserved for host-backed IO ops),
      * - opIndex is a stable numeric tag for the IO primop,
      * - args are boxed payloads required to compute the WIT request and to build the resume value.
      *
      * This relies on [[flix_resume_suspension]]'s behavior when `resumption` is null:
      * resuming yields the `resumePayload` directly and then applies prefix frames.
      */
    private def emitApplyWasmIoOpSuspension(sop: SemanticOp.UnaryOp,
                                           x: Value,
                                           pcPointId: Int,
                                           tpe: SimpleType,
                                           ctxPtr: Value,
                                           fb: FunBuilder,
                                           framePtr: Value,
                                           resumeTag: Value,
                                           resumePayload: Value,
                                           pcBlocks: Map[Int, BlockBuilder],
                                           exnHandlerOpt: Option[ExnHandler] = None): Value = {
      val opIndex = wasmIoOpIndex(sop)
      if (opIndex <= 0L) {
        fb.current.emitTrap()
        return Value.Undef(llvmTypeOf(tpe))
      }

      val argPayloads: List[Value] = sop match {
        case SemanticOp.IoOp.Readln =>
          // Console input is host-backed on wasm; the operand is `Unit` and carries no payload.
          Nil

        case SemanticOp.IoOp.SleepMillis =>
          val ms = castValue(x, Type.I64, fb)
          List(boxToI64(ms, SimpleType.Int64, fb))

        case SemanticOp.IoOp.FileExists |
             SemanticOp.IoOp.FileIsDirectory |
             SemanticOp.IoOp.FileIsRegularFile |
             SemanticOp.IoOp.FileIsReadable |
             SemanticOp.IoOp.FileIsSymbolicLink |
             SemanticOp.IoOp.FileIsWritable |
             SemanticOp.IoOp.FileIsExecutable |
             SemanticOp.IoOp.FileAccessTime |
             SemanticOp.IoOp.FileCreationTime |
             SemanticOp.IoOp.FileModificationTime |
             SemanticOp.IoOp.FileSize |
             SemanticOp.IoOp.FileRead |
             SemanticOp.IoOp.FileTruncate |
             SemanticOp.IoOp.FileMkDir |
             SemanticOp.IoOp.FileMkDirs |
             SemanticOp.IoOp.FileMkTempDir =>
          val path = castValue(x, Type.Ptr, fb)
          List(boxToI64(path, SimpleType.String, fb))

        case SemanticOp.IoOp.FileReadLines |
             SemanticOp.IoOp.FileReadBytes |
             SemanticOp.IoOp.FileList =>
          val rc = loadTupleElement(x, 0L, SimpleType.Region, fb)
          val path = loadTupleElement(x, 1L, SimpleType.String, fb)
          List(boxToI64(rc, SimpleType.Region, fb), boxToI64(path, SimpleType.String, fb))

        case SemanticOp.IoOp.FileWrite |
             SemanticOp.IoOp.FileAppend =>
          val data = loadTupleElement(x, 0L, SimpleType.String, fb)
          val path = loadTupleElement(x, 1L, SimpleType.String, fb)
          List(boxToI64(data, SimpleType.String, fb), boxToI64(path, SimpleType.String, fb))

        case SemanticOp.IoOp.FileWriteBytes |
             SemanticOp.IoOp.FileAppendBytes =>
          val bytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
          val path = loadTupleElement(x, 1L, SimpleType.String, fb)
          List(
            boxToI64(bytes, SimpleType.Array(SimpleType.Int8), fb),
            boxToI64(path, SimpleType.String, fb)
          )

        case SemanticOp.IoOp.TcpSocketRead |
             SemanticOp.IoOp.TcpSocketWrite |
             SemanticOp.IoOp.ProcessStdinWrite |
             SemanticOp.IoOp.ProcessStdoutRead |
             SemanticOp.IoOp.ProcessStderrRead =>
          val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
          val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
          List(
            boxToI64(id, SimpleType.Int64, fb),
            boxToI64(buf, SimpleType.Array(SimpleType.Int8), fb)
          )

        case SemanticOp.IoOp.TcpSocketConnect |
             SemanticOp.IoOp.TcpServerBind =>
          val ipBytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
          val port = loadTupleElement(x, 1L, SimpleType.Int32, fb)
          List(
            boxToI64(ipBytes, SimpleType.Array(SimpleType.Int8), fb),
            boxToI64(port, SimpleType.Int32, fb)
          )

        case SemanticOp.IoOp.TcpSocketClose |
             SemanticOp.IoOp.TcpServerLocalPort |
             SemanticOp.IoOp.TcpServerAccept |
             SemanticOp.IoOp.TcpServerClose |
             SemanticOp.IoOp.ProcessExitValue |
             SemanticOp.IoOp.ProcessIsAlive |
             SemanticOp.IoOp.ProcessPid |
             SemanticOp.IoOp.ProcessStop |
             SemanticOp.IoOp.ProcessWaitFor |
             SemanticOp.IoOp.ProcessRelease =>
          val id = castValue(x, Type.I64, fb)
          List(boxToI64(id, SimpleType.Int64, fb))

        case SemanticOp.IoOp.ProcessWaitForTimeout =>
          val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
          val timeoutMs = loadTupleElement(x, 1L, SimpleType.Int64, fb)
          List(boxToI64(id, SimpleType.Int64, fb), boxToI64(timeoutMs, SimpleType.Int64, fb))

        case SemanticOp.IoOp.ProcessExec =>
          val argv = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.String), fb)
          val hasCwd = loadTupleElement(x, 1L, SimpleType.Bool, fb)
          val cwdStr = loadTupleElement(x, 2L, SimpleType.String, fb)
          val envPairs = loadTupleElement(x, 3L, SimpleType.Array(SimpleType.String), fb)
          List(
            boxToI64(argv, SimpleType.Array(SimpleType.String), fb),
            boxToI64(hasCwd, SimpleType.Bool, fb),
            boxToI64(cwdStr, SimpleType.String, fb),
            boxToI64(envPairs, SimpleType.Array(SimpleType.String), fb),
          )

        case SemanticOp.IoOp.HttpRequest =>
          val method = loadTupleElement(x, 0L, SimpleType.String, fb)
          val url = loadTupleElement(x, 1L, SimpleType.String, fb)
          val headers = loadTupleElement(x, 2L, SimpleType.Array(SimpleType.String), fb)
          val hasBody = loadTupleElement(x, 3L, SimpleType.Bool, fb)
          val body = loadTupleElement(x, 4L, SimpleType.String, fb)
          List(
            boxToI64(method, SimpleType.String, fb),
            boxToI64(url, SimpleType.String, fb),
            boxToI64(headers, SimpleType.Array(SimpleType.String), fb),
            boxToI64(hasBody, SimpleType.Bool, fb),
            boxToI64(body, SimpleType.String, fb),
          )

        case _ =>
          fb.current.emitTrap()
          return Value.Undef(llvmTypeOf(tpe))
      }

      // Set pc on the current frame.
      storeObjI64Slot(framePtr, Value.IntConst(0L, Type.I64), Value.IntConst(pcPointId.toLong, Type.I64), fb)

      // Create prefix frames list with this frame.
      val prefixPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(prefixPtr, Op.Call(Type.Ptr, "flix_frames_push", List(framePtr, Value.Null(Type.Ptr))))
      val prefixBits = freshTmp(Type.I64)
      fb.current.emitAssign(prefixBits, Op.Cast("ptrtoint", Type.I64, prefixPtr))

      // Allocate suspension object.
      val slots = 5L + argPayloads.length.toLong
      val payloadBytes = Value.IntConst(slots * 8L, Type.I64)
      val sizeBytes = freshTmp(Type.I64)
      fb.current.emitAssign(sizeBytes, Op.Bin("add", Type.I64, payloadBytes, Value.IntConst(flixObjHeaderBytes, Type.I64)))
      val suspPtr = freshTmp(Type.Ptr)
      val suspTi = Value.Global(LlvmNames.suspensionTypeInfoName, Type.Ptr)
      fb.current.emitAssign(suspPtr, Op.Call(Type.Ptr, "flix_alloc_flex", List(ctxPtr, suspTi, sizeBytes)))

      storeObjI64Slot(suspPtr, Value.IntConst(0L, Type.I64), Value.IntConst(0L, Type.I64), fb) // effSymId = 0 (Wasm IO)
      storeObjI64Slot(suspPtr, Value.IntConst(1L, Type.I64), Value.IntConst(opIndex, Type.I64), fb)
      storeObjI64Slot(suspPtr, Value.IntConst(2L, Type.I64), prefixBits, fb)
      storeObjI64Slot(suspPtr, Value.IntConst(3L, Type.I64), Value.IntConst(0L, Type.I64), fb) // resumption = null
      storeObjI64Slot(suspPtr, Value.IntConst(4L, Type.I64), Value.IntConst(argPayloads.length.toLong, Type.I64), fb)
      argPayloads.zipWithIndex.foreach {
        case (p, i) =>
          storeObjI64Slot(suspPtr, Value.IntConst(5L + i.toLong, Type.I64), p, fb)
      }

      val suspBits = freshTmp(Type.I64)
      fb.current.emitAssign(suspBits, Op.Cast("ptrtoint", Type.I64, suspPtr))
      val suspResult = packResultTagged(ResultTagSuspension, suspBits, fb)
      fb.current.setTerminator(Terminator.Ret(flixResultType, suspResult))

      // Resume pc block.
      val afterLabel = freshLabel("io_after")
      val resumeBlock = pcBlocks.getOrElse(pcPointId, throw new IllegalStateException(s"missing pc block: $pcPointId"))
      if (resumeBlock.isTerminated) {
        throw new IllegalStateException(s"pc block $pcPointId already terminated")
      }

      val (resumedValue, resumedValuePredLabel) = {
        val saved = fb.current
        fb.setCurrent(resumeBlock)

        val resumeIsValue = freshTmp(Type.I1)
        fb.current.emitAssign(resumeIsValue, Op.ICmp("eq", resumeTag, Value.IntConst(ResultTagValue, Type.I64)))
        val resumeValueLabel = freshLabel("resume_value")
        val resumeNotValueLabel = freshLabel("resume_not_value")
        fb.current.setTerminator(Terminator.CondBr(resumeIsValue, resumeValueLabel, resumeNotValueLabel))

        val resumeValueBlock = fb.newBlock(resumeValueLabel)
        fb.setCurrent(resumeValueBlock)

        if (isGcRootType(tpe)) {
          val resumeSlotPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(resumeSlotPtr, Op.Alloca(Type.I64))
          fb.current.emitStore(resumePayload, resumeSlotPtr)
          fb.current.emitCallVoid(rootPushNameOf(Type.I64), List(ctxPtr, resumeSlotPtr))
        }

        fb.current.emitCallVoid("flix_gc_pollcheck", List(ctxPtr))
        if (isGcRootType(tpe)) {
          fb.current.emitCallVoid("flix_gc_pop_roots", List(ctxPtr, Value.IntConst(1L, Type.I64)))
        }

        val isCancelled = freshTmp(Type.I1)
        fb.current.emitAssign(isCancelled, Op.Call(Type.I1, "flix_cancel_requested", List(ctxPtr)))
        val cancelLabel = freshLabel("resume_cancel")
        val resumeContinueLabel = freshLabel("resume_continue")
        fb.current.setTerminator(Terminator.CondBr(isCancelled, cancelLabel, resumeContinueLabel))

        val cancelBlock = fb.newBlock(cancelLabel)
        fb.setCurrent(cancelBlock)
        val cancelExnPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(cancelExnPtr, Op.Call(Type.Ptr, "flix_cancel_exn", List(ctxPtr, Value.IntConst(cancelledKindId, Type.I64), exnExnTypeInfo, Value.IntConst(exnExnTagId, Type.I64))))
        val tracedCancelExnPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(tracedCancelExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(cancelExnPtr)))
        val cancelBits = freshTmp(Type.I64)
        fb.current.emitAssign(cancelBits, Op.Cast("ptrtoint", Type.I64, tracedCancelExnPtr))
        val cancelResult = packResultTagged(ResultTagException, cancelBits, fb)
        fb.current.setTerminator(Terminator.Ret(flixResultType, cancelResult))

        val resumeContinueBlock = fb.newBlock(resumeContinueLabel)
        fb.setCurrent(resumeContinueBlock)
        val v = unboxFromI64(resumePayload, tpe, fb)
        fb.current.setTerminator(Terminator.Br(afterLabel))

        val resumeNotValueBlock = fb.newBlock(resumeNotValueLabel)
        fb.setCurrent(resumeNotValueBlock)
        val resumeIsExn = freshTmp(Type.I1)
        fb.current.emitAssign(resumeIsExn, Op.ICmp("eq", resumeTag, Value.IntConst(ResultTagException, Type.I64)))
        val resumeExnLabel = freshLabel("resume_exn")
        val resumeBadLabel = freshLabel("resume_bad")
        fb.current.setTerminator(Terminator.CondBr(resumeIsExn, resumeExnLabel, resumeBadLabel))

        val resumeExnBlock = fb.newBlock(resumeExnLabel)
        fb.setCurrent(resumeExnBlock)
        exnHandlerOpt match {
          case Some(ExnHandler(label, slotPtr)) =>
            fb.current.emitStore(resumePayload, slotPtr)
            fb.current.setTerminator(Terminator.Br(label))
          case None =>
            val resumeExnResult = packResultTagged(ResultTagException, resumePayload, fb)
            fb.current.setTerminator(Terminator.Ret(flixResultType, resumeExnResult))
        }

        val resumeBadBlock = fb.newBlock(resumeBadLabel)
        fb.setCurrent(resumeBadBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)

        fb.setCurrent(saved)
        (v, resumeContinueBlock.label)
      }

      val afterBlock = fb.newBlock(afterLabel)
      fb.setCurrent(afterBlock)
      val joinTpe = llvmTypeOf(tpe)
      val phiDest = freshTmp(joinTpe)
      afterBlock.emitPhi(phiDest, List((resumedValue, resumedValuePredLabel)))
      phiDest
    }

    private def isSuspendableWasmIoOp(sop: SemanticOp.UnaryOp): Boolean =
      target == CompilationTarget.LlvmWasm && wasmIoOpIndex(sop) > 0L

    private def wasmIoOpIndex(sop: SemanticOp.UnaryOp): Long = sop match {
      case SemanticOp.IoOp.SleepMillis => 1L
      case SemanticOp.IoOp.HttpRequest => 2L
      case SemanticOp.IoOp.FileExists => 3L
      case SemanticOp.IoOp.FileIsDirectory => 4L
      case SemanticOp.IoOp.FileIsRegularFile => 5L
      case SemanticOp.IoOp.FileIsReadable => 6L
      case SemanticOp.IoOp.FileIsSymbolicLink => 7L
      case SemanticOp.IoOp.FileIsWritable => 8L
      case SemanticOp.IoOp.FileIsExecutable => 9L
      case SemanticOp.IoOp.FileAccessTime => 10L
      case SemanticOp.IoOp.FileCreationTime => 11L
      case SemanticOp.IoOp.FileModificationTime => 12L
      case SemanticOp.IoOp.FileSize => 13L
      case SemanticOp.IoOp.FileRead => 14L
      case SemanticOp.IoOp.FileReadLines => 15L
      case SemanticOp.IoOp.FileReadBytes => 16L
      case SemanticOp.IoOp.FileList => 17L
      case SemanticOp.IoOp.FileWrite => 18L
      case SemanticOp.IoOp.FileWriteBytes => 19L
      case SemanticOp.IoOp.FileAppend => 20L
      case SemanticOp.IoOp.FileAppendBytes => 21L
      case SemanticOp.IoOp.FileTruncate => 22L
      case SemanticOp.IoOp.FileMkDir => 23L
      case SemanticOp.IoOp.FileMkDirs => 24L
      case SemanticOp.IoOp.FileMkTempDir => 25L
      case SemanticOp.IoOp.ProcessExec => 26L
      case SemanticOp.IoOp.ProcessExitValue => 27L
      case SemanticOp.IoOp.ProcessIsAlive => 28L
      case SemanticOp.IoOp.ProcessPid => 29L
      case SemanticOp.IoOp.ProcessStop => 30L
      case SemanticOp.IoOp.ProcessWaitFor => 31L
      case SemanticOp.IoOp.ProcessWaitForTimeout => 32L
      case SemanticOp.IoOp.ProcessStdinWrite => 33L
      case SemanticOp.IoOp.ProcessStdoutRead => 34L
      case SemanticOp.IoOp.ProcessStderrRead => 35L
      case SemanticOp.IoOp.ProcessRelease => 36L
      case SemanticOp.IoOp.TcpSocketConnect => 37L
      case SemanticOp.IoOp.TcpSocketRead => 38L
      case SemanticOp.IoOp.TcpSocketWrite => 39L
      case SemanticOp.IoOp.TcpSocketClose => 40L
      case SemanticOp.IoOp.TcpServerBind => 41L
      case SemanticOp.IoOp.TcpServerAccept => 42L
      case SemanticOp.IoOp.TcpServerLocalPort => 43L
      case SemanticOp.IoOp.TcpServerClose => 44L
      case SemanticOp.IoOp.Readln => 45L
      case _ => 0L
    }

    private def emitRunWithExpression(exp: Expr,
                                     effSym: Symbol.EffSym,
                                     rules: List[LoweredAst.HandlerRule],
                                     ct: ExpPosition,
                                     pcPointId: Int,
                                     tpe: SimpleType,
                                     env: Map[Symbol.VarSym, Value],
                                     slotTypes: Map[Symbol.VarSym, Type],
                                     selfTailLabel: Option[String],
                                     ctxPtr: Value,
                                     fb: FunBuilder,
                                     framePtrOpt: Option[Value],
                                     slotIndexOf: Map[Symbol.VarSym, Long],
                                     lenv: Map[Symbol.LabelSym, String],
                                     resumeTag: Value,
                                     resumePayload: Value,
                                     pcBlocks: Map[Int, BlockBuilder],
                                     exnHandlerOpt: Option[ExnHandler] = None): Value = {
      val effId = effectSymIds.getOrElse(effSym, 0L)
      val eff = root.effects.getOrElse(effSym, throw new IllegalStateException(s"missing effect: $effSym"))

      val thunkPtr = framePtrOpt match {
        case None =>
          emitExpr(exp, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
        case Some(framePtr) =>
          emitExprControlImpure(exp, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
      }
      if (fb.current.isTerminated) return Value.Undef(llvmTypeOf(tpe))

      val thunkRootSlot = freshTmp(Type.Ptr)
      fb.current.emitAssign(thunkRootSlot, Op.Alloca(Type.Ptr))
      fb.current.emitStore(castValue(thunkPtr, Type.Ptr, fb), thunkRootSlot)
      fb.current.emitCallVoid("flix_gc_push_root_ptr", List(ctxPtr, thunkRootSlot))

      val opCount = eff.ops.length
      val handlerSlots = 2L + 2L * opCount.toLong
      val handlerPayloadBytes = Value.IntConst(handlerSlots * 8L, Type.I64)
      val handlerSizeBytes = freshTmp(Type.I64)
      fb.current.emitAssign(handlerSizeBytes, Op.Bin("add", Type.I64, handlerPayloadBytes, Value.IntConst(flixObjHeaderBytes, Type.I64)))
      val handlerPtr = freshTmp(Type.Ptr)
      val handlerTi = Value.Global(LlvmNames.handlerTypeInfoName, Type.Ptr)
      fb.current.emitAssign(handlerPtr, Op.Call(Type.Ptr, "flix_alloc_flex", List(ctxPtr, handlerTi, handlerSizeBytes)))

      val handlerRootSlot = freshTmp(Type.Ptr)
      fb.current.emitAssign(handlerRootSlot, Op.Alloca(Type.Ptr))
      fb.current.emitStore(handlerPtr, handlerRootSlot)
      fb.current.emitCallVoid("flix_gc_push_root_ptr", List(ctxPtr, handlerRootSlot))

      storeObjI64Slot(handlerPtr, Value.IntConst(0L, Type.I64), Value.IntConst(effId, Type.I64), fb)
      storeObjI64Slot(handlerPtr, Value.IntConst(1L, Type.I64), Value.IntConst(opCount.toLong, Type.I64), fb)

      val ruleMap = rules.map(r => r.op.sym -> r).toMap
      eff.ops.zipWithIndex.foreach {
        case (op, idx) =>
          val rule = ruleMap.getOrElse(op.sym, throw new IllegalStateException(s"missing handler rule for op: ${op.sym}"))

          val cloPtr = framePtrOpt match {
            case None =>
              emitExpr(rule.exp, env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt)
            case Some(framePtr) =>
              emitExprControlImpure(rule.exp, ctxPtr, fb, framePtr, slotIndexOf, lenv, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
          }

          val cloBits = castValue(cloPtr, Type.I64, fb)
          storeObjI64Slot(handlerPtr, Value.IntConst((2L + idx.toLong * 2L + 1L), Type.I64), cloBits, fb)

          val closureSym = findClosureSym(rule.exp).getOrElse(throw new IllegalStateException("expected handler rule closure"))
          val wrapperName = getOrEmitEffectOpWrapper(op.sym, closureSym)
          val wrapperBits = freshTmp(Type.I64)
          fb.current.emitAssign(wrapperBits, Op.Cast("ptrtoint", Type.I64, Value.Global(wrapperName, Type.Ptr)))
          storeObjI64Slot(handlerPtr, Value.IntConst((2L + idx.toLong * 2L), Type.I64), wrapperBits, fb)
      }

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_install_handler", List(ctxPtr, Value.IntConst(effId, Type.I64), handlerPtr, Value.Null(Type.Ptr), castValue(thunkPtr, Type.Ptr, fb))))
      fb.current.emitCallVoid("flix_gc_pop_roots", List(ctxPtr, Value.IntConst(2L, Type.I64)))

      ct match {
        case ExpPosition.Tail =>
          fb.current.setTerminator(Terminator.Ret(flixResultType, callTmp))
          Value.Undef(llvmTypeOf(tpe))
        case ExpPosition.NonTail =>
          framePtrOpt match {
            case Some(framePtr) if pcPointId > 0 =>
              emitCallAndHandleSuspension(callTmp, pcPointId, tpe, ctxPtr, fb, framePtr, resumeTag, resumePayload, pcBlocks, exnHandlerOpt)
            case _ =>
              val payload = unwindThunkToValuePayloadOrPropagateExn(callTmp, ctxPtr, fb, exnHandlerOpt)
              unboxFromI64(payload, tpe, fb)
          }
      }
    }

    private def findClosureSym(exp0: Expr): Option[Symbol.DefnSym] = exp0 match {
      case Expr.ApplyAtomic(AtomicOp.Closure(sym), _, _, _, _, _) => Some(sym)
      case Expr.ApplyAtomic(AtomicOp.Box | AtomicOp.Unbox | AtomicOp.Cast, exps, _, _, _, _) if exps.length == 1 =>
        findClosureSym(exps.head)
      case _ => None
    }

    private def getOrEmitResumptionInvokeWrapper(argTpe: SimpleType): String = {
      val name = s"flix_k_invoke_${LlvmNamesInternal.mangle(llvmTypeOf(argTpe).render)}"
      if (extraFunctionNames.contains(name)) return name
      addExtraFunction(emitResumptionInvokeWrapper(name, argTpe))
      name
    }

    private def emitResumptionInvokeWrapper(name: String, argTpe: SimpleType): LlvmIr.Function = {
      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("self", Type.Ptr),
        LlvmIr.Param("arg_tag", Type.I64),
        LlvmIr.Param("arg0", Type.I64)
      )

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val selfPtr = Value.Local("self", Type.Ptr)
      val arg0Payload = Value.Local("arg0", Type.I64)

      val resBits = loadObjI64Slot(selfPtr, Value.IntConst(0L, Type.I64), fb)
      val resPtr = castValue(resBits, Type.Ptr, fb)
      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_resumption_rewind", List(ctxPtr, resPtr, arg0Payload)))
      fb.current.setTerminator(Terminator.Ret(flixResultType, callTmp))

      LlvmIr.Function(name, flixResultType, params, fb.result())
    }

    private def getOrEmitEffectOpWrapper(opSym: Symbol.OpSym, closureSym: Symbol.DefnSym): String = {
      val name = s"flix_eff_wrap_${LlvmNamesInternal.mangle(opSym.toString)}_${LlvmNamesInternal.mangle(closureSym.toString)}"
      if (extraFunctionNames.contains(name)) return name
      addExtraFunction(emitEffectOpWrapper(name, opSym, closureSym))
      name
    }

    private def emitEffectOpWrapper(name: String, opSym: Symbol.OpSym, closureSym: Symbol.DefnSym): LlvmIr.Function = {
      val params = List(
        LlvmIr.Param("ctx", Type.Ptr),
        LlvmIr.Param("handler", Type.Ptr),
        LlvmIr.Param("resumption", Type.Ptr),
        LlvmIr.Param("suspension", Type.Ptr)
      )

      val fb = new FunBuilder()
      val entry = fb.newBlock("entry")
      fb.setCurrent(entry)

      val ctxPtr = Value.Local("ctx", Type.Ptr)
      val handlerPtr = Value.Local("handler", Type.Ptr)
      val resumptionPtr = Value.Local("resumption", Type.Ptr)
      val suspensionPtr = Value.Local("suspension", Type.Ptr)

      val opIdx = opIndices.getOrElse(opSym, throw new IllegalStateException(s"missing op index: $opSym"))
      val opDef = root.effects(opSym.eff).ops(opIdx)

      // Load handler rule closure pointer from handler slots.
      val closureSlot = 2L + 2L * opIdx.toLong + 1L
      val cloBits = loadObjI64Slot(handlerPtr, Value.IntConst(closureSlot, Type.I64), fb)
      val cloPtr = castValue(cloBits, Type.Ptr, fb)

      // Allocate continuation closure that captures the resumption.
      getOrEmitResumptionInvokeWrapper(opDef.tpe)
      val kPtr = freshTmp(Type.Ptr)
      val kTi = Value.Global(LlvmNames.kTypeInfoName(opDef.tpe), Type.Ptr)
      fb.current.emitAssign(kPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, kTi)))

      // Keep the continuation closure alive across the handler rule call. The callee allocates its
      // own frame before storing parameters, so a raw `kPtr` argument is not an honest GC root.
      val kRootSlot = freshTmp(Type.Ptr)
      fb.current.emitAssign(kRootSlot, Op.Alloca(Type.Ptr))
      fb.current.emitStore(kPtr, kRootSlot)
      fb.current.emitCallVoid(rootPushNameOf(Type.Ptr), List(ctxPtr, kRootSlot))
      fb.rootsToPop = 1L

      val resBits = freshTmp(Type.I64)
      fb.current.emitAssign(resBits, Op.Cast("ptrtoint", Type.I64, resumptionPtr))
      storeObjI64Slot(kPtr, Value.IntConst(0L, Type.I64), resBits, fb)

      val closureDef = root.defs(closureSym)
      val capturedArgs = closureDef.cparams.zipWithIndex.map {
        case (cp, i) =>
          val payload = loadObjI64Slot(cloPtr, Value.IntConst(i.toLong, Type.I64), fb)
          unboxFromI64(payload, cp.tpe, fb)
      }

      val opArgs = opDef.fparams.zipWithIndex.map {
        case (fp, i) =>
          val payload = loadObjI64Slot(suspensionPtr, Value.IntConst((5L + i.toLong), Type.I64), fb)
          unboxFromI64(payload, fp.tpe, fb)
      }

      val expectedFormalCount = if (opDef.fparams.isEmpty) 2 else opDef.fparams.length + 1
      if (closureDef.fparams.length != expectedFormalCount) {
        throw new IllegalStateException(s"Unexpected handler rule arity for '$closureSym'. Expected $expectedFormalCount. Actual ${closureDef.fparams.length}.")
      }

      val formalArgs = if (opDef.fparams.isEmpty) {
        val unitArg = Value.IntConst(0L, Type.I64)
        val kArgTpe = llvmTypeOf(closureDef.fparams(1).tpe)
        val kArg = castValue(kPtr, kArgTpe, fb)
        List(unitArg, kArg)
      } else {
        val kArgTpe = llvmTypeOf(closureDef.fparams.last.tpe)
        val kArg = castValue(kPtr, kArgTpe, fb)
        opArgs :+ kArg
      }

      val callTmp = freshTmp(flixResultType)
      fb.current.emitAssign(callTmp, Op.Call(flixResultType, LlvmNames.defName(closureSym), ctxPtr :: (capturedArgs ::: formalArgs)))
      fb.current.setTerminator(Terminator.Ret(flixResultType, callTmp))

      LlvmIr.Function(name, flixResultType, params, fb.result())
    }

    private def emitExprs(exps: List[Expr],
                          env: Map[Symbol.VarSym, Value],
                          ctxPtr: Value,
                          fb: FunBuilder,
                          lenv: Map[Symbol.LabelSym, String],
                          slotTypes: Map[Symbol.VarSym, Type],
                          selfTailLabel: Option[String],
                          exnHandlerOpt: Option[ExnHandler] = None): Option[List[Value]] = {
      val buf = mutable.ListBuffer.empty[Value]
      val it = exps.iterator
      while (it.hasNext && !fb.current.isTerminated) {
        buf.addOne(emitExpr(it.next(), env, ctxPtr, fb, lenv, slotTypes, selfTailLabel, exnHandlerOpt))
      }
      if (fb.current.isTerminated) None else Some(buf.toList)
    }

    private def coerceToI1(v: Value, fb: FunBuilder): Value = {
      v.tpe match {
        case Type.I1 => v
        case Type.Ptr =>
          val tmp = freshTmp(Type.I1)
          fb.current.emitAssign(tmp, Op.ICmp("ne", v, Value.Null(Type.Ptr)))
          tmp
        case t if isIntType(t) =>
          val tmp = freshTmp(Type.I1)
          fb.current.emitAssign(tmp, Op.ICmp("ne", v, Value.IntConst(0L, t)))
          tmp
        case _ =>
          fb.current.emitTrap()
          Value.Undef(Type.I1)
      }
    }

    private def coerceValue(v: Value, expectedTpe: Type, fb: FunBuilder): Value = {
      if (v.tpe == expectedTpe) return v
      (v.tpe, expectedTpe) match {
        case (Type.I1, Type.I64) =>
          val tmp = freshTmp(Type.I64)
          fb.current.emitAssign(tmp, Op.Cast("zext", Type.I64, v))
          tmp
        case (Type.I32, Type.I64) =>
          val tmp = freshTmp(Type.I64)
          fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, v))
          tmp
        case (Type.I64, Type.I32) =>
          val tmp = freshTmp(Type.I32)
          fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, v))
          tmp
        case (Type.I64, Type.I1) =>
          val tmp = freshTmp(Type.I1)
          fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I1, v))
          tmp
        case _ =>
          v
      }
    }

    private def emitConstant(cst: Constant, ctxPtr: Value, fb: FunBuilder): Value = cst match {
      case Constant.Unit =>
        Value.IntConst(0L, Type.I64)

      case Constant.Bool(lit) =>
        Value.IntConst(if (lit) 1L else 0L, Type.I1)

      case Constant.Char(lit) =>
        Value.IntConst(lit.toLong, Type.I32)

      case Constant.Int8(lit) =>
        Value.IntConst(lit.toLong, Type.I8)

      case Constant.Int16(lit) =>
        Value.IntConst(lit.toLong, Type.I16)

      case Constant.Int32(lit) =>
        Value.IntConst(lit.toLong, Type.I32)

      case Constant.Int64(lit) =>
        Value.IntConst(lit, Type.I64)

      case Constant.BigInt(lit) =>
        try {
          val longVal = lit.longValueExact()
          val tmp = freshTmp(Type.Ptr)
          fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_from_i64", List(ctxPtr, Value.IntConst(longVal, Type.I64))))
          tmp
        } catch {
          case _: java.lang.ArithmeticException =>
            val strPtr = emitConstant(Constant.Str(lit.toString), ctxPtr, fb)
            val tmp = freshTmp(Type.Ptr)
            fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_from_string", List(ctxPtr, strPtr)))
            tmp
        }

      case Constant.BigDecimal(lit) =>
        val strPtr = emitConstant(Constant.Str(lit.toString), ctxPtr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_from_string", List(ctxPtr, strPtr)))
        tmp

      case Constant.Float32(lit) =>
        Value.Float32Const(java.lang.Float.floatToRawIntBits(lit))

      case Constant.Float64(lit) =>
        Value.Float64Const(java.lang.Double.doubleToRawLongBits(lit))

      case Constant.Null =>
        Value.Null(Type.Ptr)

      case Constant.Static =>
        // Represent the static region as null.
        Value.Null(Type.Ptr)

      case Constant.RecordEmpty =>
        // Represent the empty record as null.
        Value.Null(Type.Ptr)

      case Constant.Str(lit) =>
        val len = lit.length.toLong
        val lenI64 = Value.IntConst(len, Type.I64)

        val bytesChars = freshTmp(Type.I64)
        fb.current.emitAssign(bytesChars, Op.Bin("mul", Type.I64, lenI64, Value.IntConst(2L, Type.I64)))
        val sizeBytes = freshTmp(Type.I64)
        fb.current.emitAssign(sizeBytes, Op.Bin("add", Type.I64, bytesChars, Value.IntConst(flixStringDataOffsetBytes, Type.I64)))

        val strPtr = freshTmp(Type.Ptr)
        val strTi = Value.Global(LlvmNames.stringTypeInfoName, Type.Ptr)
        fb.current.emitAssign(strPtr, Op.Call(Type.Ptr, "flix_alloc_flex", List(ctxPtr, strTi, sizeBytes)))

        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, strPtr, Value.IntConst(flixStringLenOffsetBytes, Type.I64)))
        fb.current.emitStore(Value.IntConst(len, Type.I32), lenPtr)

        val reservedPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(reservedPtr, Op.Gep(Type.I8, strPtr, Value.IntConst(flixStringReservedOffsetBytes, Type.I64)))
        fb.current.emitStore(Value.IntConst(0L, Type.I32), reservedPtr)

        val basePtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(basePtr, Op.Gep(Type.I8, strPtr, Value.IntConst(flixStringDataOffsetBytes, Type.I64)))

        var i = 0
        while (i < lit.length) {
          val cuPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(cuPtr, Op.Gep(Type.I16, basePtr, Value.IntConst(i.toLong, Type.I64)))
          fb.current.emitStore(Value.IntConst(lit.charAt(i).toLong, Type.I16), cuPtr)
          i += 1
        }

        strPtr

      case _ =>
        fb.current.emitTrap()
        Value.Undef(Type.Ptr)
    }

    private def loadI64Slot(basePtr0: Value, idx: Value, fb: FunBuilder): Value = {
      val basePtr = castValue(basePtr0, Type.Ptr, fb)
      val slotPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, basePtr, idx))
      val payload = freshTmp(Type.I64)
      fb.current.emitAssign(payload, Op.Load(Type.I64, slotPtr))
      payload
    }

    private def storeI64Slot(basePtr0: Value, idx: Value, payload: Value, fb: FunBuilder): Unit = {
      val basePtr = castValue(basePtr0, Type.Ptr, fb)
      val slotPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, basePtr, idx))
      fb.current.emitStore(payload, slotPtr)
    }

    private def objPayloadBase(objPtr0: Value, fb: FunBuilder): Value = {
      val objPtr = castValue(objPtr0, Type.Ptr, fb)
      val payloadBase = freshTmp(Type.Ptr)
      fb.current.emitAssign(payloadBase, Op.Gep(flixObjType, objPtr, Value.IntConst(1L, Type.I64)))
      payloadBase
    }

    private def objPayloadI64SlotPtr(objPtr0: Value, idx: Value, fb: FunBuilder): Value = {
      val payloadBase = objPayloadBase(objPtr0, fb)
      val slotPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, payloadBase, idx))
      slotPtr
    }

    private def loadObjI64Slot(objPtr0: Value, idx: Value, fb: FunBuilder): Value = {
      val payloadBase = objPayloadBase(objPtr0, fb)
      val slotPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, payloadBase, idx))
      val payload = freshTmp(Type.I64)
      fb.current.emitAssign(payload, Op.Load(Type.I64, slotPtr))
      payload
    }

    private def storeObjI64Slot(objPtr0: Value, idx: Value, payload: Value, fb: FunBuilder): Unit = {
      val payloadBase = objPayloadBase(objPtr0, fb)
      val slotPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, payloadBase, idx))
      fb.current.emitStore(payload, slotPtr)
    }

    private def loadTupleElement(tuplePtr0: Value, idx: Long, tpe: SimpleType, fb: FunBuilder): Value = {
      val payload = loadObjI64Slot(tuplePtr0, Value.IntConst(idx, Type.I64), fb)
      unboxFromI64(payload, tpe, fb)
    }

    private def allocTuple2(tupleTpe: SimpleType.Tuple, payload0: Value, payload1: Value, ctxPtr: Value, fb: FunBuilder): Value = {
      val tupPtr = freshTmp(Type.Ptr)
      val tupTi = Value.Global(LlvmNames.tupleTypeInfoName(tupleTpe), Type.Ptr)
      fb.current.emitAssign(tupPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, tupTi)))
      storeObjI64Slot(tupPtr, Value.IntConst(0L, Type.I64), payload0, fb)
      storeObjI64Slot(tupPtr, Value.IntConst(1L, Type.I64), payload1, fb)
      tupPtr
    }

    private val flixObjHeaderBytes: Long = 8L
    private val flixStringLenOffsetBytes: Long = 8L
    private val flixStringReservedOffsetBytes: Long = 12L
    private val flixStringDataOffsetBytes: Long = 16L

    private def stringLenI32(strPtr0: Value, fb: FunBuilder): Value = {
      val strPtr = castValue(strPtr0, Type.Ptr, fb)
      val lenPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, strPtr, Value.IntConst(flixStringLenOffsetBytes, Type.I64)))
      val lenI32 = freshTmp(Type.I32)
      fb.current.emitAssign(lenI32, Op.Load(Type.I32, lenPtr))
      lenI32
    }

    private def stringLenI64(strPtr0: Value, fb: FunBuilder): Value = {
      val lenI32 = stringLenI32(strPtr0, fb)
      val lenI64 = freshTmp(Type.I64)
      fb.current.emitAssign(lenI64, Op.Cast("zext", Type.I64, lenI32))
      lenI64
    }

    private def stringCodeUnitPtr(strPtr0: Value, idxI64: Value, fb: FunBuilder): Value = {
      val strPtr = castValue(strPtr0, Type.Ptr, fb)
      val basePtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(basePtr, Op.Gep(Type.I8, strPtr, Value.IntConst(flixStringDataOffsetBytes, Type.I64)))
      val cuPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(cuPtr, Op.Gep(Type.I16, basePtr, idxI64))
      cuPtr
    }

    private def stringCharPayloadI64(strPtr0: Value, idxI64: Value, fb: FunBuilder): Value = {
      val cuPtr = stringCodeUnitPtr(strPtr0, idxI64, fb)
      val cuI16 = freshTmp(Type.I16)
      fb.current.emitAssign(cuI16, Op.Load(Type.I16, cuPtr))
      val cuI64 = freshTmp(Type.I64)
      fb.current.emitAssign(cuI64, Op.Cast("zext", Type.I64, cuI16))
      cuI64
    }

    private def storeStringCharPayload(strPtr0: Value, idxI64: Value, payloadI64: Value, fb: FunBuilder): Unit = {
      val cuPtr = stringCodeUnitPtr(strPtr0, idxI64, fb)
      val cuI16 = freshTmp(Type.I16)
      fb.current.emitAssign(cuI16, Op.Cast("trunc", Type.I16, payloadI64))
      fb.current.emitStore(cuI16, cuPtr)
    }

    private def allocString(lenI64: Value, ctxPtr: Value, fb: FunBuilder): Value = {
      val bytesChars = freshTmp(Type.I64)
      fb.current.emitAssign(bytesChars, Op.Bin("mul", Type.I64, lenI64, Value.IntConst(2L, Type.I64)))
      val sizeBytes = freshTmp(Type.I64)
      fb.current.emitAssign(sizeBytes, Op.Bin("add", Type.I64, bytesChars, Value.IntConst(flixStringDataOffsetBytes, Type.I64)))

      val strPtr = freshTmp(Type.Ptr)
      val strTi = Value.Global(LlvmNames.stringTypeInfoName, Type.Ptr)
      fb.current.emitAssign(strPtr, Op.Call(Type.Ptr, "flix_alloc_flex", List(ctxPtr, strTi, sizeBytes)))

      val lenI32 = freshTmp(Type.I32)
      fb.current.emitAssign(lenI32, Op.Cast("trunc", Type.I32, lenI64))

      val lenPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, strPtr, Value.IntConst(flixStringLenOffsetBytes, Type.I64)))
      fb.current.emitStore(lenI32, lenPtr)

      val reservedPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(reservedPtr, Op.Gep(Type.I8, strPtr, Value.IntConst(flixStringReservedOffsetBytes, Type.I64)))
      fb.current.emitStore(Value.IntConst(0L, Type.I32), reservedPtr)

      strPtr
    }

    private def emitStringEquals(str1Ptr0: Value, str2Ptr0: Value, fb: FunBuilder): Value = {
      val str1Ptr = castValue(str1Ptr0, Type.Ptr, fb)
      val str2Ptr = castValue(str2Ptr0, Type.Ptr, fb)

      val len1 = stringLenI64(str1Ptr, fb)
      val len2 = stringLenI64(str2Ptr, fb)

      val lenEq = freshTmp(Type.I1)
      fb.current.emitAssign(lenEq, Op.ICmp("eq", len1, len2))

      val lenOkLabel = freshLabel("seq_len_ok")
      val lenBadLabel = freshLabel("seq_len_bad")
      val loopLabel = freshLabel("seq_loop")
      val bodyLabel = freshLabel("seq_body")
      val contLabel = freshLabel("seq_cont")
      val mismatchLabel = freshLabel("seq_mismatch")
      val doneLabel = freshLabel("seq_done")
      val endLabel = freshLabel("seq_end")

      fb.current.setTerminator(Terminator.CondBr(lenEq, lenOkLabel, lenBadLabel))

      val resultIncomings = mutable.ArrayBuffer.empty[(Value, String)]

      val lenBadBlock = fb.newBlock(lenBadLabel)
      fb.setCurrent(lenBadBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      resultIncomings.addOne((Value.IntConst(0L, Type.I1), lenBadLabel))

      val lenOkBlock = fb.newBlock(lenOkLabel)
      fb.setCurrent(lenOkBlock)
      val iPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)
      val iVal = freshTmp(Type.I64)
      fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
      val more = freshTmp(Type.I1)
      fb.current.emitAssign(more, Op.ICmp("slt", iVal, len1))
      fb.current.setTerminator(Terminator.CondBr(more, bodyLabel, doneLabel))

      val bodyBlock = fb.newBlock(bodyLabel)
      fb.setCurrent(bodyBlock)
      val c1 = stringCharPayloadI64(str1Ptr, iVal, fb)
      val c2 = stringCharPayloadI64(str2Ptr, iVal, fb)
      val chEq = freshTmp(Type.I1)
      fb.current.emitAssign(chEq, Op.ICmp("eq", c1, c2))
      fb.current.setTerminator(Terminator.CondBr(chEq, contLabel, mismatchLabel))

      val contBlock = fb.newBlock(contLabel)
      fb.setCurrent(contBlock)
      val iNext = freshTmp(Type.I64)
      fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(iNext, iPtr)
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val mismatchBlock = fb.newBlock(mismatchLabel)
      fb.setCurrent(mismatchBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      resultIncomings.addOne((Value.IntConst(0L, Type.I1), mismatchLabel))

      val doneBlock = fb.newBlock(doneLabel)
      fb.setCurrent(doneBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      resultIncomings.addOne((Value.IntConst(1L, Type.I1), doneLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val phi = freshTmp(Type.I1)
      endBlock.emitPhi(phi, resultIncomings.toList)
      phi
    }

    private def emitApplyAtomic(op: AtomicOp,
                               argTpes: List[SimpleType],
                               args: List[Value],
                               resultTpe: SimpleType,
                               ctxPtr: Value,
                               fb: FunBuilder,
                               exnHandlerOpt: Option[ExnHandler] = None): Value = op match {
      case AtomicOp.Closure(sym) =>
        val captured = args.zip(argTpes).map {
          case (v, tpe) => boxToI64(v, tpe, fb)
        }
        val cloPtr = freshTmp(Type.Ptr)
        val cloTi = Value.Global(LlvmNames.closureTypeInfoName(sym), Type.Ptr)
        fb.current.emitAssign(cloPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, cloTi)))

        captured.zipWithIndex.foreach {
          case (payload, i) =>
            storeObjI64Slot(cloPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
        }

        cloPtr

      case AtomicOp.Tuple =>
        resultTpe match {
          case tupTpe: SimpleType.Tuple =>
            val elms = args.zip(argTpes).map {
              case (v, tpe) => boxToI64(v, tpe, fb)
            }

            val tupPtr = freshTmp(Type.Ptr)
            val tupTi = Value.Global(LlvmNames.tupleTypeInfoName(tupTpe), Type.Ptr)
            fb.current.emitAssign(tupPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, tupTi)))

            elms.zipWithIndex.foreach {
              case (payload, i) =>
                storeObjI64Slot(tupPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
            }

            tupPtr
          case _ =>
            fb.current.emitTrap()
            Value.Undef(Type.Ptr)
        }

      case AtomicOp.Index(idx) =>
        val tuplePtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val tuplePtr = castValue(tuplePtr0, Type.Ptr, fb)
        val payload = loadObjI64Slot(tuplePtr, Value.IntConst(idx.toLong, Type.I64), fb)
        unboxFromI64(payload, resultTpe, fb)

      case AtomicOp.RecordSelect(label) =>
        val recordTpe = argTpes.headOption.getOrElse(SimpleType.RecordEmpty)
        val recordPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val recordPtr = castValue(recordPtr0, Type.Ptr, fb)

        val fields = recordFields(recordTpe)
        val idx = fields.indexWhere(_._1 == label.name)
        if (idx < 0) {
          fb.current.emitTrap()
          Value.Undef(llvmTypeOf(resultTpe))
        } else {
          val payload = loadObjI64Slot(recordPtr, Value.IntConst(idx.toLong, Type.I64), fb)
          unboxFromI64(payload, resultTpe, fb)
        }

      case AtomicOp.RecordExtend(label) =>
        val v0 = args.headOption.getOrElse(Value.Undef(Type.I64))
        val vTpe = argTpes.headOption.getOrElse(SimpleType.Object)
        val rest0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.Ptr))
        val restPtr = castValue(rest0, Type.Ptr, fb)
        val restTpe = argTpes.drop(1).headOption.getOrElse(SimpleType.RecordEmpty)

        val resultFields = recordFields(resultTpe)
        val restFields = recordFields(restTpe)
        val restIndex = restFields.zipWithIndex.map { case ((l, _), i) => l -> i }.toMap

        val recPtr = freshTmp(Type.Ptr)
        val recTi = Value.Global(LlvmNames.recordTypeInfoName(resultTpe), Type.Ptr)
        fb.current.emitAssign(recPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, recTi)))

        val vPayload = boxToI64(v0, vTpe, fb)

        resultFields.zipWithIndex.foreach {
          case ((fldLabel, _), i) =>
            if (fldLabel == label.name) {
              storeObjI64Slot(recPtr, Value.IntConst(i.toLong, Type.I64), vPayload, fb)
            } else {
              restIndex.get(fldLabel) match {
                case None =>
                  fb.current.emitTrap()
                case Some(oldIdx) =>
                  val payload = loadObjI64Slot(restPtr, Value.IntConst(oldIdx.toLong, Type.I64), fb)
                  storeObjI64Slot(recPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
              }
            }
        }

        recPtr

      case AtomicOp.RecordRestrict(label) =>
        val recordTpe = argTpes.headOption.getOrElse(SimpleType.RecordEmpty)
        val recordPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val recordPtr = castValue(recordPtr0, Type.Ptr, fb)

        val oldFields = recordFields(recordTpe)
        val newFields = recordFields(resultTpe)

        if (newFields.isEmpty) {
          Value.Null(Type.Ptr)
        } else {
          val oldIndex = oldFields.zipWithIndex.map { case ((l, _), i) => l -> i }.toMap

          val recPtr = freshTmp(Type.Ptr)
          val recTi = Value.Global(LlvmNames.recordTypeInfoName(resultTpe), Type.Ptr)
          fb.current.emitAssign(recPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, recTi)))

          newFields.zipWithIndex.foreach {
            case ((fldLabel, _), i) =>
              oldIndex.get(fldLabel) match {
                case None =>
                  fb.current.emitTrap()
                case Some(oldIdx) =>
                  val payload = loadObjI64Slot(recordPtr, Value.IntConst(oldIdx.toLong, Type.I64), fb)
                  storeObjI64Slot(recPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
              }
          }

          recPtr
        }

      case AtomicOp.Tag(sym) =>
        val tagId = caseTagIds.get(sym)
        val payloads = args.zip(argTpes).map {
          case (v, tpe) => boxToI64(v, tpe, fb)
        }

        tagId match {
          case None =>
            fb.current.emitTrap()
            Value.Undef(Type.Ptr)

          case Some(id) =>
            // Layout: header + payload[0]=tag word, payload[1..]=fields.
            val objPtr = freshTmp(Type.Ptr)
            val tagTi = Value.Global(LlvmNames.tagTypeInfoName(sym), Type.Ptr)
            fb.current.emitAssign(objPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, tagTi)))

            storeObjI64Slot(objPtr, Value.IntConst(0L, Type.I64), Value.IntConst(id, Type.I64), fb)
            payloads.zipWithIndex.foreach {
              case (payload, i) =>
                storeObjI64Slot(objPtr, Value.IntConst((i + 1).toLong, Type.I64), payload, fb)
            }

            objPtr
        }

      case AtomicOp.Is(sym) =>
        val tagId = caseTagIds.get(sym)
        val objPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val objPtr = castValue(objPtr0, Type.Ptr, fb)

        tagId match {
          case None =>
            fb.current.emitTrap()
            Value.Undef(Type.I1)

          case Some(id) =>
            val tagVal = loadObjI64Slot(objPtr, Value.IntConst(0L, Type.I64), fb)

            val cmp = freshTmp(Type.I1)
            fb.current.emitAssign(cmp, Op.ICmp("eq", tagVal, Value.IntConst(id, Type.I64)))
            cmp
        }

      case AtomicOp.Untag(sym, idx) =>
        val tagId = caseTagIds.get(sym)
        val objPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val objPtr = castValue(objPtr0, Type.Ptr, fb)

        tagId match {
          case None =>
            fb.current.emitTrap()
            Value.Undef(llvmTypeOf(resultTpe))

          case Some(id) =>
            // Defensive: trap if the tag doesn't match.
            val tagVal = loadObjI64Slot(objPtr, Value.IntConst(0L, Type.I64), fb)

            val ok = freshTmp(Type.I1)
            fb.current.emitAssign(ok, Op.ICmp("eq", tagVal, Value.IntConst(id, Type.I64)))

            val thenLabel = freshLabel("untag_ok")
            val elseLabel = freshLabel("untag_bad")
            val endLabel = freshLabel("untag_end")

            fb.current.setTerminator(Terminator.CondBr(ok, thenLabel, elseLabel))

            val joinTpe = llvmTypeOf(resultTpe)
            val incomings = mutable.ArrayBuffer.empty[(Value, String)]

            // Ok path.
            val okBlock = fb.newBlock(thenLabel)
            fb.setCurrent(okBlock)
            val slot = (idx + 1).toLong
            val payload = loadObjI64Slot(objPtr, Value.IntConst(slot, Type.I64), fb)
            val valueOk = unboxFromI64(payload, resultTpe, fb)
            if (!fb.current.isTerminated) {
              val v = coerceValue(valueOk, joinTpe, fb)
              fb.current.setTerminator(Terminator.Br(endLabel))
              incomings.addOne((v, thenLabel))
            }

            // Bad path.
            val badBlock = fb.newBlock(elseLabel)
            fb.setCurrent(badBlock)
            fb.current.emitTrap()
            fb.current.setTerminator(Terminator.Unreachable)

            // Join.
            val joinBlock = fb.newBlock(endLabel)
            fb.setCurrent(joinBlock)
            if (incomings.isEmpty) Value.Undef(joinTpe)
            else {
              val phiDest = freshTmp(joinTpe)
              joinBlock.emitPhi(phiDest, incomings.toList)
              phiDest
            }
        }

      case AtomicOp.ExtTag(label) =>
        val tagId = extTagId(label)
        val payloads = args.zip(argTpes).map {
          case (v, tpe) => boxToI64(v, tpe, fb)
        }

        val slots = 1L + payloads.length.toLong
        val sizeBytes = Value.IntConst(slots * 8L, Type.I64)

	        val mallocSize = castValue(sizeBytes, if (target == CompilationTarget.LlvmWasm) Type.I32 else Type.I64, fb)
	        val objPtr = freshTmp(Type.Ptr)
	        fb.current.emitAssign(objPtr, Op.Call(Type.Ptr, "malloc", List(mallocSize)))

        val tagPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(tagPtr, Op.Gep(Type.I64, objPtr, Value.IntConst(0L, Type.I64)))
        fb.current.emitStore(Value.IntConst(tagId, Type.I64), tagPtr)

        payloads.zipWithIndex.foreach {
          case (payload, i) =>
            val slotPtr = freshTmp(Type.Ptr)
            fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, objPtr, Value.IntConst((i + 1).toLong, Type.I64)))
            fb.current.emitStore(payload, slotPtr)
        }

        objPtr

      case AtomicOp.ExtIs(label) =>
        val tagId = extTagId(label)
        val objPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val objPtr = castValue(objPtr0, Type.Ptr, fb)

        val tagPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(tagPtr, Op.Gep(Type.I64, objPtr, Value.IntConst(0L, Type.I64)))

        val tagVal = freshTmp(Type.I64)
        fb.current.emitAssign(tagVal, Op.Load(Type.I64, tagPtr))

        val cmp = freshTmp(Type.I1)
        fb.current.emitAssign(cmp, Op.ICmp("eq", tagVal, Value.IntConst(tagId, Type.I64)))
        cmp

      case AtomicOp.ExtUntag(label, idx) =>
        val tagId = extTagId(label)
        val objPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val objPtr = castValue(objPtr0, Type.Ptr, fb)

        val tagPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(tagPtr, Op.Gep(Type.I64, objPtr, Value.IntConst(0L, Type.I64)))

        val tagVal = freshTmp(Type.I64)
        fb.current.emitAssign(tagVal, Op.Load(Type.I64, tagPtr))

        val ok = freshTmp(Type.I1)
        fb.current.emitAssign(ok, Op.ICmp("eq", tagVal, Value.IntConst(tagId, Type.I64)))

        val okLabel = freshLabel("extuntag_ok")
        val badLabel = freshLabel("extuntag_bad")
        fb.current.setTerminator(Terminator.CondBr(ok, okLabel, badLabel))

        val badBlock = fb.newBlock(badLabel)
        fb.setCurrent(badBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)

        val slot = (idx + 1).toLong
        val slotPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, objPtr, Value.IntConst(slot, Type.I64)))
        val payload = freshTmp(Type.I64)
        fb.current.emitAssign(payload, Op.Load(Type.I64, slotPtr))
        unboxFromI64(payload, resultTpe, fb)

      case AtomicOp.ArrayLit =>
        val (rcPtr, elms, elmTpes) = argTpes.headOption match {
          case Some(SimpleType.Region) =>
            val rc0 = args.headOption.getOrElse(Value.Null(Type.Ptr))
            (castValue(rc0, Type.Ptr, fb), args.drop(1), argTpes.drop(1))
          case _ =>
            (Value.Null(Type.Ptr), args, argTpes)
        }

        val elemTpe = resultTpe match {
          case SimpleType.Array(t) => t
          case _ => SimpleType.Object
        }
        // We only track/write-barrier GC heap pointers (not region-scoped pointers).
        val isPtrArray = isGcRootType(elemTpe)
        val isInt8Array = !isPtrArray && elemTpe == SimpleType.Int8

        val (len, sizeBytes, elemSizeI32) = if (isInt8Array) {
          val len0 = elms.length.toLong
          (len0, Value.IntConst(16L + len0, Type.I64), Value.IntConst(1L, Type.I32))
        } else {
          val len0 = elms.length.toLong
          (len0, Value.IntConst(16L + len0 * 8L, Type.I64), Value.IntConst(8L, Type.I32))
        }

        val arrTiName = if (isPtrArray) LlvmNames.arrayPtrTypeInfoName else LlvmNames.arrayPrimTypeInfoName
        val arrTi = Value.Global(arrTiName, Type.Ptr)

        val arrPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(arrPtr, Op.Call(Type.Ptr, "flix_region_alloc_flex", List(ctxPtr, rcPtr, arrTi, sizeBytes)))

        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(8L, Type.I64)))
        fb.current.emitStore(Value.IntConst(len, Type.I32), lenPtr)

        val elemSizePtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemSizePtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(12L, Type.I64)))
        fb.current.emitStore(elemSizeI32, elemSizePtr)

        val elmsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elmsPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(16L, Type.I64)))

        if (isInt8Array) {
          elms.zipWithIndex.foreach {
            case (v, i) =>
              val slotPtr = freshTmp(Type.Ptr)
              fb.current.emitAssign(slotPtr, Op.Gep(Type.I8, elmsPtr, Value.IntConst(i.toLong, Type.I64)))
              val byteVal = castValue(v, Type.I8, fb)
              fb.current.emitStore(byteVal, slotPtr)
          }
        } else {
          val payloads = elms.zip(elmTpes).map {
            case (v, tpe) => boxToI64(v, tpe, fb)
          }
          payloads.zipWithIndex.foreach {
            case (payload, i) =>
              val slotPtr = freshTmp(Type.Ptr)
              fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, elmsPtr, Value.IntConst(i.toLong, Type.I64)))
              if (isPtrArray) emitStorePtrLike(ctxPtr, slotPtr, payload, fb)
              else fb.current.emitStore(payload, slotPtr)
          }
        }

        arrPtr

      case AtomicOp.ArrayNew =>
        val (rcPtr, default0, defaultTpe, len0) = argTpes.headOption match {
          case Some(SimpleType.Region) =>
            val rc0 = args.headOption.getOrElse(Value.Null(Type.Ptr))
            val d0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I64))
            val l0 = args.drop(2).headOption.getOrElse(Value.Undef(Type.I32))
            val dt = argTpes.drop(1).headOption.getOrElse(SimpleType.Object)
            (castValue(rc0, Type.Ptr, fb), d0, dt, l0)
          case _ =>
            val d0 = args.headOption.getOrElse(Value.Undef(Type.I64))
            val l0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I32))
            val dt = argTpes.headOption.getOrElse(SimpleType.Object)
            (Value.Null(Type.Ptr), d0, dt, l0)
        }

        // We only track/write-barrier GC heap pointers (not region-scoped pointers).
        val isPtrArray = isGcRootType(defaultTpe)
        val isInt8Array = !isPtrArray && defaultTpe == SimpleType.Int8

        val lenI64 = castValue(len0, Type.I64, fb)
        val negative = freshTmp(Type.I1)
        fb.current.emitAssign(negative, Op.ICmp("slt", lenI64, Value.IntConst(0L, Type.I64)))

        val okLabel = freshLabel("arr_ok")
        val badLabel = freshLabel("arr_bad")
        val contLabel = freshLabel("arr_cont")

        fb.current.setTerminator(Terminator.CondBr(negative, badLabel, okLabel))

        // Negative length.
        val badBlock = fb.newBlock(badLabel)
        fb.setCurrent(badBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)

        // Ok.
        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)

        val sizeBytes = if (isInt8Array) {
          val sz = freshTmp(Type.I64)
          fb.current.emitAssign(sz, Op.Bin("add", Type.I64, lenI64, Value.IntConst(16L, Type.I64)))
          sz
        } else {
          val elmsBytes = freshTmp(Type.I64)
          fb.current.emitAssign(elmsBytes, Op.Bin("mul", Type.I64, lenI64, Value.IntConst(8L, Type.I64)))

          val sz = freshTmp(Type.I64)
          fb.current.emitAssign(sz, Op.Bin("add", Type.I64, elmsBytes, Value.IntConst(16L, Type.I64)))
          sz
        }

        val arrTiName = if (isPtrArray) LlvmNames.arrayPtrTypeInfoName else LlvmNames.arrayPrimTypeInfoName
        val arrTi = Value.Global(arrTiName, Type.Ptr)

        val arrPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(arrPtr, Op.Call(Type.Ptr, "flix_region_alloc_flex", List(ctxPtr, rcPtr, arrTi, sizeBytes)))

        val lenI32 = freshTmp(Type.I32)
        fb.current.emitAssign(lenI32, Op.Cast("trunc", Type.I32, lenI64))

        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(8L, Type.I64)))
        fb.current.emitStore(lenI32, lenPtr)

        val elemSizePtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elemSizePtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(12L, Type.I64)))
        if (isInt8Array) fb.current.emitStore(Value.IntConst(1L, Type.I32), elemSizePtr)
        else fb.current.emitStore(Value.IntConst(8L, Type.I32), elemSizePtr)

        val elmsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elmsPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(16L, Type.I64)))

        val defaultPayload = if (isInt8Array) Value.Undef(Type.I64) else boxToI64(default0, defaultTpe, fb)
        val defaultByte = if (isInt8Array) castValue(default0, Type.I8, fb) else Value.Undef(Type.I8)

        // Initialize elements with the default value.
        val iPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

        val loopLabel = freshLabel("arr_loop")
        val bodyLabel = freshLabel("arr_body")
        val endLabel = freshLabel("arr_end")

        fb.current.setTerminator(Terminator.Br(loopLabel))

        val loopBlock = fb.newBlock(loopLabel)
        fb.setCurrent(loopBlock)
        val iVal = freshTmp(Type.I64)
        fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
        val cond = freshTmp(Type.I1)
        fb.current.emitAssign(cond, Op.ICmp("slt", iVal, lenI64))
        fb.current.setTerminator(Terminator.CondBr(cond, bodyLabel, endLabel))

        val bodyBlock = fb.newBlock(bodyLabel)
        fb.setCurrent(bodyBlock)
        val slotPtr = freshTmp(Type.Ptr)
        if (isInt8Array) {
          fb.current.emitAssign(slotPtr, Op.Gep(Type.I8, elmsPtr, iVal))
          fb.current.emitStore(defaultByte, slotPtr)
        } else {
          fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, elmsPtr, iVal))
          if (isPtrArray) emitStorePtrLike(ctxPtr, slotPtr, defaultPayload, fb)
          else fb.current.emitStore(defaultPayload, slotPtr)
        }
        val iNext = freshTmp(Type.I64)
        fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(iNext, iPtr)
        fb.current.setTerminator(Terminator.Br(loopLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        fb.current.setTerminator(Terminator.Br(contLabel))

        val contBlock = fb.newBlock(contLabel)
        fb.setCurrent(contBlock)
        arrPtr

      case AtomicOp.ArrayLoad =>
        val arr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val idx0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I32))
        val arrPtr = castValue(arr0, Type.Ptr, fb)
        val idxI64 = castValue(idx0, Type.I64, fb)

        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(8L, Type.I64)))
        val lenI32 = freshTmp(Type.I32)
        fb.current.emitAssign(lenI32, Op.Load(Type.I32, lenPtr))
        val lenI64 = freshTmp(Type.I64)
        fb.current.emitAssign(lenI64, Op.Cast("zext", Type.I64, lenI32))

        val neg = freshTmp(Type.I1)
        fb.current.emitAssign(neg, Op.ICmp("slt", idxI64, Value.IntConst(0L, Type.I64)))
        val ge = freshTmp(Type.I1)
        fb.current.emitAssign(ge, Op.ICmp("sge", idxI64, lenI64))
        val oob = freshTmp(Type.I1)
        fb.current.emitAssign(oob, Op.Bin("or", Type.I1, neg, ge))

        val okLabel = freshLabel("aload_ok")
        val badLabel = freshLabel("aload_bad")
        val endLabel = freshLabel("aload_end")

        fb.current.setTerminator(Terminator.CondBr(oob, badLabel, okLabel))

        val badBlock = fb.newBlock(badLabel)
        fb.setCurrent(badBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)

        val elmsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elmsPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(16L, Type.I64)))
        val valueOk = if (resultTpe == SimpleType.Int8) {
          val bytePtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(bytePtr, Op.Gep(Type.I8, elmsPtr, idxI64))
          val byteVal = freshTmp(Type.I8)
          fb.current.emitAssign(byteVal, Op.Load(Type.I8, bytePtr))
          byteVal
        } else {
          val slotPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, elmsPtr, idxI64))
          val payload = freshTmp(Type.I64)
          fb.current.emitAssign(payload, Op.Load(Type.I64, slotPtr))
          unboxFromI64(payload, resultTpe, fb)
        }
        if (!fb.current.isTerminated) {
          fb.current.setTerminator(Terminator.Br(endLabel))
        }

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        valueOk

      case AtomicOp.ArrayStore =>
        val arr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val idx0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I32))
        val v0 = args.drop(2).headOption.getOrElse(Value.Undef(Type.I64))
        val vTpe = argTpes.drop(2).headOption.getOrElse(SimpleType.Object)

        val arrPtr = castValue(arr0, Type.Ptr, fb)
        val idxI64 = castValue(idx0, Type.I64, fb)

        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(8L, Type.I64)))
        val lenI32 = freshTmp(Type.I32)
        fb.current.emitAssign(lenI32, Op.Load(Type.I32, lenPtr))
        val lenI64 = freshTmp(Type.I64)
        fb.current.emitAssign(lenI64, Op.Cast("zext", Type.I64, lenI32))

        val neg = freshTmp(Type.I1)
        fb.current.emitAssign(neg, Op.ICmp("slt", idxI64, Value.IntConst(0L, Type.I64)))
        val ge = freshTmp(Type.I1)
        fb.current.emitAssign(ge, Op.ICmp("sge", idxI64, lenI64))
        val oob = freshTmp(Type.I1)
        fb.current.emitAssign(oob, Op.Bin("or", Type.I1, neg, ge))

        val okLabel = freshLabel("astore_ok")
        val badLabel = freshLabel("astore_bad")
        val endLabel = freshLabel("astore_end")

        fb.current.setTerminator(Terminator.CondBr(oob, badLabel, okLabel))

        val badBlock = fb.newBlock(badLabel)
        fb.setCurrent(badBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)

        val elmsPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(elmsPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(16L, Type.I64)))
        if (vTpe == SimpleType.Int8) {
          val bytePtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(bytePtr, Op.Gep(Type.I8, elmsPtr, idxI64))
          val byteVal = castValue(v0, Type.I8, fb)
          fb.current.emitStore(byteVal, bytePtr)
        } else {
          val slotPtr = freshTmp(Type.Ptr)
          fb.current.emitAssign(slotPtr, Op.Gep(Type.I64, elmsPtr, idxI64))
          val payload = boxToI64(v0, vTpe, fb)
          if (isGcRootType(vTpe)) emitStorePtrLike(ctxPtr, slotPtr, payload, fb)
          else fb.current.emitStore(payload, slotPtr)
        }
        fb.current.setTerminator(Terminator.Br(endLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        Value.IntConst(0L, Type.I64)

      case AtomicOp.ArrayLength =>
        val arr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val arrPtr = castValue(arr0, Type.Ptr, fb)

        val lenPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lenPtr, Op.Gep(Type.I8, arrPtr, Value.IntConst(8L, Type.I64)))

        val lenI32 = freshTmp(Type.I32)
        fb.current.emitAssign(lenI32, Op.Load(Type.I32, lenPtr))
        lenI32

      case AtomicOp.StructNew(sym, mutability, _) =>
        val struct = root.structs(sym)
        val fieldCount = struct.fields.length
        val (rcPtr, fieldArgs, fieldTpes) = mutability match {
          case ca.uwaterloo.flix.language.ast.shared.Mutability.Immutable =>
            (Value.Null(Type.Ptr), args, argTpes)
          case ca.uwaterloo.flix.language.ast.shared.Mutability.Mutable =>
            val rc0 = args.headOption.getOrElse(Value.Null(Type.Ptr))
            val rc = castValue(rc0, Type.Ptr, fb)
            (rc, args.drop(1), argTpes.drop(1))
        }

        if (fieldArgs.length != fieldCount) {
          fb.current.emitTrap()
          Value.Undef(Type.Ptr)
        } else {
          val payloads = fieldArgs.zip(fieldTpes).map {
            case (v, tpe) => boxToI64(v, tpe, fb)
          }

          val structFieldTpes = struct.fields.map(_.tpe)

          val objPtr = freshTmp(Type.Ptr)
          val structTi = Value.Global(LlvmNames.structTypeInfoName(sym), Type.Ptr)
          mutability match {
            case ca.uwaterloo.flix.language.ast.shared.Mutability.Mutable =>
              fb.current.emitAssign(objPtr, Op.Call(Type.Ptr, "flix_region_alloc", List(ctxPtr, rcPtr, structTi)))
            case ca.uwaterloo.flix.language.ast.shared.Mutability.Immutable =>
              fb.current.emitAssign(objPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, structTi)))
          }

          payloads.zipWithIndex.foreach {
            case (payload, i) =>
              mutability match {
                case ca.uwaterloo.flix.language.ast.shared.Mutability.Mutable if isGcRootType(structFieldTpes(i)) =>
                  val slotPtr = objPayloadI64SlotPtr(objPtr, Value.IntConst(i.toLong, Type.I64), fb)
                  emitStorePtrLike(ctxPtr, slotPtr, payload, fb)
                  fb.current.emitCallVoid("flix_region_remember_slot", List(ctxPtr, rcPtr, slotPtr))
                case _ =>
                  storeObjI64Slot(objPtr, Value.IntConst(i.toLong, Type.I64), payload, fb)
              }
          }

          objPtr
        }

      case AtomicOp.StructGet(field) =>
        val structPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val structPtr = castValue(structPtr0, Type.Ptr, fb)

        val struct = root.structs(field.structSym)
        val idx = struct.fields.indexWhere(_.sym == field)
        if (idx < 0) {
          fb.current.emitTrap()
          Value.Undef(llvmTypeOf(resultTpe))
        } else {
          val payload = loadObjI64Slot(structPtr, Value.IntConst(idx.toLong, Type.I64), fb)
          unboxFromI64(payload, resultTpe, fb)
        }

      case AtomicOp.StructPut(field) =>
        val structPtr0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val v0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I64))
        val vTpe = argTpes.drop(1).headOption.getOrElse(SimpleType.Object)

        val structPtr = castValue(structPtr0, Type.Ptr, fb)

        val struct = root.structs(field.structSym)
        val idx = struct.fields.indexWhere(_.sym == field)
        if (idx < 0) {
          fb.current.emitTrap()
        } else {
          val slotPtr = objPayloadI64SlotPtr(structPtr, Value.IntConst(idx.toLong, Type.I64), fb)
          val payload = boxToI64(v0, vTpe, fb)
          if (isGcRootType(struct.fields(idx).tpe)) emitStorePtrLike(ctxPtr, slotPtr, payload, fb)
          else fb.current.emitStore(payload, slotPtr)
        }
        Value.IntConst(0L, Type.I64)

      case AtomicOp.Lazy =>
        val exp0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val expTpe = argTpes.headOption.getOrElse(SimpleType.Object)

        val expPayload = boxToI64(exp0, expTpe, fb)

        val innerTpe = resultTpe match {
          case SimpleType.Lazy(t) => t
          case _ => SimpleType.Object
        }

        // Layout: payload[0] = exp payload (i64), payload[1] = value payload (i64, valid iff exp == 0).
        val lazyPtr = freshTmp(Type.Ptr)
        val lazyTi = Value.Global(LlvmNames.lazyTypeInfoName(innerTpe), Type.Ptr)
        fb.current.emitAssign(lazyPtr, Op.Call(Type.Ptr, "flix_alloc", List(ctxPtr, lazyTi)))

        storeObjI64Slot(lazyPtr, Value.IntConst(0L, Type.I64), expPayload, fb)
        storeObjI64Slot(lazyPtr, Value.IntConst(1L, Type.I64), Value.IntConst(0L, Type.I64), fb)

        lazyPtr

      case AtomicOp.Force =>
        val lazy0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val lazyPtr = castValue(lazy0, Type.Ptr, fb)

        val expPayload = loadObjI64Slot(lazyPtr, Value.IntConst(0L, Type.I64), fb)

        val isForced = freshTmp(Type.I1)
        fb.current.emitAssign(isForced, Op.ICmp("eq", expPayload, Value.IntConst(0L, Type.I64)))

        val forcedLabel = freshLabel("force_done")
        val computeLabel = freshLabel("force_compute")
        val endLabel = freshLabel("force_end")

        fb.current.setTerminator(Terminator.CondBr(isForced, forcedLabel, computeLabel))

        val joinTpe = llvmTypeOf(resultTpe)
        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        // Already forced: load cached value.
        val forcedBlock = fb.newBlock(forcedLabel)
        fb.setCurrent(forcedBlock)
        val cachedPayload = loadObjI64Slot(lazyPtr, Value.IntConst(1L, Type.I64), fb)
        val cachedValue = unboxFromI64(cachedPayload, resultTpe, fb)
        if (!fb.current.isTerminated) {
          val v = coerceValue(cachedValue, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((v, predLabel))
        }

        // Not yet forced: invoke thunk and cache.
        val computeBlock = fb.newBlock(computeLabel)
        fb.setCurrent(computeBlock)

        val cloPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(cloPtr, Op.Cast("inttoptr", Type.Ptr, expPayload))

        val callTmp = freshTmp(flixResultType)
        fb.current.emitAssign(callTmp, Op.Call(flixResultType, "flix_invoke_thunk", List(ctxPtr, cloPtr, Value.IntConst(ResultTagValue, Type.I64), Value.IntConst(0L, Type.I64))))
        val payloadTmp = unwindThunkToValuePayloadOrPropagateExn(callTmp, ctxPtr, fb, exnHandlerOpt)

        // Cache the value and mark as forced.
        storeObjI64Slot(lazyPtr, Value.IntConst(1L, Type.I64), payloadTmp, fb)
        storeObjI64Slot(lazyPtr, Value.IntConst(0L, Type.I64), Value.IntConst(0L, Type.I64), fb)

        val computedValue = unboxFromI64(payloadTmp, resultTpe, fb)
        if (!fb.current.isTerminated) {
          val v = coerceValue(computedValue, joinTpe, fb)
          val predLabel = fb.current.label
          fb.current.setTerminator(Terminator.Br(endLabel))
          incomings.addOne((v, predLabel))
        }

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        if (incomings.isEmpty) Value.Undef(joinTpe)
        else {
          val phiDest = freshTmp(joinTpe)
          endBlock.emitPhi(phiDest, incomings.toList)
          phiDest
        }

      case AtomicOp.Spawn =>
        val clo0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val rc0 = args.drop(1).headOption.getOrElse(Value.Null(Type.Ptr))

        val cloPtr = castValue(clo0, Type.Ptr, fb)
        val rcPtr = castValue(rc0, Type.Ptr, fb)

        val callTmp = freshTmp(Type.I64)
        fb.current.emitAssign(callTmp, Op.Call(Type.I64, "flix_spawn", List(ctxPtr, rcPtr, cloPtr)))
        Value.IntConst(0L, Type.I64)

      case AtomicOp.ChannelNew =>
        val cap0 = args.headOption.getOrElse(Value.Undef(Type.I32))
        val capI32 = castValue(cap0, Type.I32, fb)

        val chanPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(chanPtr, Op.Call(Type.Ptr, "flix_channel_new", List(capI32)))
        chanPtr

      case AtomicOp.ChannelPut =>
        val chan0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val v0 = args.drop(1).headOption.getOrElse(Value.Undef(Type.I64))
        val vTpe = argTpes.drop(1).headOption.getOrElse(SimpleType.Object)

        val chanPtr = castValue(chan0, Type.Ptr, fb)
        val payload = boxToI64(v0, vTpe, fb)

        val callTmp = freshTmp(Type.I64)
        fb.current.emitAssign(callTmp, Op.Call(Type.I64, "flix_channel_put", List(chanPtr, payload)))
        callTmp

      case AtomicOp.ChannelGet =>
        val chan0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val chanPtr = castValue(chan0, Type.Ptr, fb)

        val payload = freshTmp(Type.I64)
        fb.current.emitAssign(payload, Op.Call(Type.I64, "flix_channel_get", List(chanPtr)))
        unboxFromI64(payload, resultTpe, fb)

      case AtomicOp.ChannelSelect =>
        val blocking0 = args.lastOption.getOrElse(Value.Undef(Type.I1))
        val channelArgs = args.dropRight(1)
        val channelsPtr = emitTempPtrArray(channelArgs, fb)
        val count = Value.IntConst(channelArgs.length.toLong, Type.I32)
        val blocking = castValue(blocking0, Type.I1, fb)

        val token = freshTmp(Type.I64)
        fb.current.emitAssign(token, Op.Call(Type.I64, "flix_channel_select", List(channelsPtr, count, blocking)))
        token

      case AtomicOp.ChannelSelectIndex =>
        val token0 = args.headOption.getOrElse(Value.Undef(Type.I64))
        val token = castValue(token0, Type.I64, fb)
        val index = freshTmp(Type.I32)
        fb.current.emitAssign(index, Op.Call(Type.I32, "flix_channel_select_index", List(token)))
        index

      case AtomicOp.ChannelSelectGet =>
        val token0 = args.headOption.getOrElse(Value.Undef(Type.I64))
        val token = castValue(token0, Type.I64, fb)
        val payload = freshTmp(Type.I64)
        fb.current.emitAssign(payload, Op.Call(Type.I64, "flix_channel_select_get", List(token)))
        unboxFromI64(payload, resultTpe, fb)

      case AtomicOp.ReentrantLockNew =>
        val lockPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(lockPtr, Op.Call(Type.Ptr, "flix_reentrant_lock_new", Nil))
        lockPtr

      case AtomicOp.ReentrantLockLock =>
        val lock0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val lockPtr = castValue(lock0, Type.Ptr, fb)
        val callTmp = freshTmp(Type.I64)
        fb.current.emitAssign(callTmp, Op.Call(Type.I64, "flix_reentrant_lock_lock", List(lockPtr)))
        Value.IntConst(0L, Type.I64)

      case AtomicOp.ReentrantLockTryLock =>
        val lock0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val lockPtr = castValue(lock0, Type.Ptr, fb)
        val acquired = freshTmp(Type.I1)
        fb.current.emitAssign(acquired, Op.Call(Type.I1, "flix_reentrant_lock_try_lock", List(lockPtr)))
        acquired

      case AtomicOp.ReentrantLockUnlock =>
        val lock0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val lockPtr = castValue(lock0, Type.Ptr, fb)
        val released = freshTmp(Type.I1)
        fb.current.emitAssign(released, Op.Call(Type.I1, "flix_reentrant_lock_unlock", List(lockPtr)))
        released

      case AtomicOp.ConditionNew =>
        val lock0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val lockPtr = castValue(lock0, Type.Ptr, fb)
        val conditionPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(conditionPtr, Op.Call(Type.Ptr, "flix_condition_new", List(lockPtr)))
        conditionPtr

      case AtomicOp.ConditionAwait =>
        val condition0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val conditionPtr = castValue(condition0, Type.Ptr, fb)
        val result = freshTmp(Type.I32)
        fb.current.emitAssign(result, Op.Call(Type.I32, "flix_condition_await", List(conditionPtr)))
        result

      case AtomicOp.ConditionSignal =>
        val condition0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val conditionPtr = castValue(condition0, Type.Ptr, fb)
        val signaled = freshTmp(Type.I1)
        fb.current.emitAssign(signaled, Op.Call(Type.I1, "flix_condition_signal", List(conditionPtr)))
        signaled

      case AtomicOp.ConditionSignalAll =>
        val condition0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val conditionPtr = castValue(condition0, Type.Ptr, fb)
        val signaled = freshTmp(Type.I1)
        fb.current.emitAssign(signaled, Op.Call(Type.I1, "flix_condition_signal_all", List(conditionPtr)))
        signaled

      case AtomicOp.CyclicBarrierNew =>
        val parties0 = args.headOption.getOrElse(Value.Undef(Type.I32))
        val parties = castValue(parties0, Type.I32, fb)
        val barrierPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(barrierPtr, Op.Call(Type.Ptr, "flix_cyclic_barrier_new", List(parties)))
        barrierPtr

      case AtomicOp.CyclicBarrierAwait =>
        val barrier0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val barrierPtr = castValue(barrier0, Type.Ptr, fb)
        val result = freshTmp(Type.I32)
        fb.current.emitAssign(result, Op.Call(Type.I32, "flix_cyclic_barrier_await", List(barrierPtr)))
        result

      case AtomicOp.CountDownLatchNew =>
        val count0 = args.headOption.getOrElse(Value.Undef(Type.I32))
        val count = castValue(count0, Type.I32, fb)
        val latchPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(latchPtr, Op.Call(Type.Ptr, "flix_count_down_latch_new", List(count)))
        latchPtr

      case AtomicOp.CountDownLatchAwait =>
        val latch0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val latchPtr = castValue(latch0, Type.Ptr, fb)
        val result = freshTmp(Type.I64)
        fb.current.emitAssign(result, Op.Call(Type.I64, "flix_count_down_latch_await", List(latchPtr)))
        Value.IntConst(0L, Type.I64)

      case AtomicOp.CountDownLatchCountDown =>
        val latch0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val latchPtr = castValue(latch0, Type.Ptr, fb)
        val result = freshTmp(Type.I64)
        fb.current.emitAssign(result, Op.Call(Type.I64, "flix_count_down_latch_count_down", List(latchPtr)))
        Value.IntConst(0L, Type.I64)

      case AtomicOp.SemaphoreNew =>
        val permits0 = args.headOption.getOrElse(Value.Undef(Type.I32))
        val permits = castValue(permits0, Type.I32, fb)
        val semPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(semPtr, Op.Call(Type.Ptr, "flix_semaphore_new", List(permits)))
        semPtr

      case AtomicOp.SemaphoreAcquire =>
        val sem0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val semPtr = castValue(sem0, Type.Ptr, fb)
        val result = freshTmp(Type.I64)
        fb.current.emitAssign(result, Op.Call(Type.I64, "flix_semaphore_acquire", List(semPtr)))
        Value.IntConst(0L, Type.I64)

      case AtomicOp.SemaphoreTryAcquire =>
        val sem0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val semPtr = castValue(sem0, Type.Ptr, fb)
        val acquired = freshTmp(Type.I1)
        fb.current.emitAssign(acquired, Op.Call(Type.I1, "flix_semaphore_try_acquire", List(semPtr)))
        acquired

      case AtomicOp.SemaphoreRelease =>
        val sem0 = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        val semPtr = castValue(sem0, Type.Ptr, fb)
        val result = freshTmp(Type.I64)
        fb.current.emitAssign(result, Op.Call(Type.I64, "flix_semaphore_release", List(semPtr)))
        Value.IntConst(0L, Type.I64)

      case AtomicOp.InvokeMethod(method) if method.getDeclaringClass.getName == "java.lang.String" && method.getName == "equals" =>
        // String.equals(Object): in Flix this is used to implement string equality.
        // If the argument is not a string we return false (matches JVM semantics).
        val recv = args.headOption.getOrElse(Value.Undef(Type.Ptr))
        argTpes.drop(1).headOption match {
          case Some(SimpleType.String) =>
            val other = args.drop(1).headOption.getOrElse(Value.Undef(Type.Ptr))
            emitStringEquals(recv, other, fb)
          case _ =>
            Value.IntConst(0L, Type.I1)
        }

      case AtomicOp.Throw =>
        val exnVal = args.headOption.getOrElse(Value.Undef(llvmTypeOf(SimpleType.Object)))
        val exnTpe = argTpes.headOption.getOrElse(SimpleType.Object)
        val payload0 = boxToI64(exnVal, exnTpe, fb)

        // Attach a portable logical stack trace on first throw.
        val exnPtr = castValue(payload0, Type.Ptr, fb)
        val tracedExnPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(tracedExnPtr, Op.Call(Type.Ptr, "flix_exn_with_trace", List(exnPtr)))
        val payload = freshTmp(Type.I64)
        fb.current.emitAssign(payload, Op.Cast("ptrtoint", Type.I64, tracedExnPtr))

        exnHandlerOpt match {
          case Some(ExnHandler(label, slotPtr)) =>
            fb.current.emitStore(payload, slotPtr)
            fb.current.setTerminator(Terminator.Br(label))
            Value.Undef(llvmTypeOf(resultTpe))
          case None =>
            val r = packResultTagged(ResultTagException, payload, fb)
            fb.current.setTerminator(Terminator.Ret(flixResultType, r))
            Value.Undef(llvmTypeOf(resultTpe))
        }

      case AtomicOp.HoleError(_) | AtomicOp.MatchError | AtomicOp.CastError(_, _) =>
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)
        Value.Undef(llvmTypeOf(resultTpe))

      case AtomicOp.Unary(SemanticOp.ExnOp.KindId) =>
        val tpe = argTpes.headOption.getOrElse(SimpleType.AnyType)
        Value.IntConst(ExnKindId.of(tpe).toLong, Type.I32)

      case AtomicOp.Unary(sop) =>
        emitUnary(sop, args.headOption.getOrElse(Value.Undef(llvmTypeOf(resultTpe))), ctxPtr, fb)

      case AtomicOp.Binary(sop) =>
        val a = args.headOption.getOrElse(Value.Undef(Type.I64))
        val b = args.drop(1).headOption.getOrElse(Value.Undef(Type.I64))
        emitBinary(sop, a, b, ctxPtr, fb)

      case AtomicOp.Box =>
        val v = args.headOption.getOrElse(Value.Undef(llvmTypeOf(argTpes.headOption.getOrElse(SimpleType.Object))))
        val tpe = argTpes.headOption.getOrElse(SimpleType.Object)
        tpe match {
          case SimpleType.Bool =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_bool", List(castValue(v, Type.I1, fb))))
            tmp
          case SimpleType.Char =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_char", List(castValue(v, Type.I32, fb))))
            tmp
          case SimpleType.Int8 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_int8", List(castValue(v, Type.I8, fb))))
            tmp
          case SimpleType.Int16 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_int16", List(castValue(v, Type.I16, fb))))
            tmp
          case SimpleType.Int32 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_int32", List(castValue(v, Type.I32, fb))))
            tmp
          case SimpleType.Int64 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_int64", List(castValue(v, Type.I64, fb))))
            tmp
          case SimpleType.Float32 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_float32", List(castValue(v, Type.Float, fb))))
            tmp
          case SimpleType.Float64 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_box_float64", List(castValue(v, Type.Double, fb))))
            tmp
          case _ =>
            boxToI64(v, tpe, fb)
        }

      case AtomicOp.Unbox =>
        val payload = args.headOption.getOrElse(Value.Undef(Type.I64))
        val i64Payload = payload.tpe match {
          case Type.I64 => payload
          case Type.Ptr =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Cast("ptrtoint", Type.I64, payload))
            tmp
          case t if isIntType(t) =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, payload))
            tmp
          case _ =>
            fb.current.emitTrap()
            Value.Undef(Type.I64)
        }
        resultTpe match {
          case SimpleType.Bool =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_bool", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case SimpleType.Char =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_char", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case SimpleType.Int8 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int8", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case SimpleType.Int16 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int16", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case SimpleType.Int32 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int32", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case SimpleType.Int64 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_int64", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case SimpleType.Float32 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_float32", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case SimpleType.Float64 =>
            val tmp = freshTmp(Type.I64)
            fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_unbox_float64", List(i64Payload)))
            unboxFromI64(tmp, resultTpe, fb)
          case _ =>
            // Unboxing a non-primitive is an erased cast: interpret the bits as the expected value.
            unboxFromI64(i64Payload, resultTpe, fb)
        }

      case AtomicOp.Cast =>
        val v = args.headOption.getOrElse(Value.Undef(llvmTypeOf(resultTpe)))
        val expected = llvmTypeOf(resultTpe)
        castValue(v, expected, fb)

      case _ =>
        fb.current.emitTrap()
        Value.Undef(llvmTypeOf(resultTpe))
    }

    private def emitUnary(op: UnaryOp, x: Value, ctxPtr: Value, fb: FunBuilder): Value = op match {
      case SemanticOp.BoolOp.Not =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I1, x, Value.IntConst(1L, Type.I1)))
        tmp

      case SemanticOp.CharOp.ToUpperCase =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_char_to_upper_case", List(x)))
        tmp

      case SemanticOp.CharOp.ToLowerCase =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_char_to_lower_case", List(x)))
        tmp

      case SemanticOp.CharOp.ToTitleCase =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_char_to_title_case", List(x)))
        tmp

      case SemanticOp.CharOp.IsLetter =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_letter", List(x)))
        tmp

      case SemanticOp.CharOp.IsDigit =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_digit", List(x)))
        tmp

      case SemanticOp.CharOp.IsLetterOrDigit =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_letter_or_digit", List(x)))
        tmp

      case SemanticOp.CharOp.IsLowerCase =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_lower_case", List(x)))
        tmp

      case SemanticOp.CharOp.IsUpperCase =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_upper_case", List(x)))
        tmp

      case SemanticOp.CharOp.IsTitleCase =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_title_case", List(x)))
        tmp

      case SemanticOp.CharOp.IsWhitespace =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_whitespace", List(x)))
        tmp

      case SemanticOp.CharOp.IsDefined =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_defined", List(x)))
        tmp

      case SemanticOp.CharOp.IsISOControl =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_iso_control", List(x)))
        tmp

      case SemanticOp.CharOp.IsMirrored =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_mirrored", List(x)))
        tmp

      case SemanticOp.CharOp.IsSurrogate =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_surrogate", List(x)))
        tmp

      case SemanticOp.CharOp.GetNumericValue =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_char_get_numeric_value", List(x)))
        tmp

      case SemanticOp.Int8Op.Neg =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I8, Value.IntConst(0L, Type.I8), x))
        tmp

      case SemanticOp.Int8Op.Not =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I8, x, Value.IntConst(-1L, Type.I8)))
        tmp

      case SemanticOp.Int16Op.Neg =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I16, Value.IntConst(0L, Type.I16), x))
        tmp

      case SemanticOp.Int16Op.Not =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I16, x, Value.IntConst(-1L, Type.I16)))
        tmp

      case SemanticOp.Int32Op.Neg =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I32, Value.IntConst(0L, Type.I32), x))
        tmp

      case SemanticOp.Int32Op.Not =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I32, x, Value.IntConst(-1L, Type.I32)))
        tmp

      case SemanticOp.Float32Op.Neg =>
        val bits = freshTmp(Type.I32)
        fb.current.emitAssign(bits, Op.Cast("bitcast", Type.I32, x))

        val flipped = freshTmp(Type.I32)
        fb.current.emitAssign(flipped, Op.Bin("xor", Type.I32, bits, Value.IntConst(Int.MinValue.toLong, Type.I32)))

        val res = freshTmp(Type.Float)
        fb.current.emitAssign(res, Op.Cast("bitcast", Type.Float, flipped))
        res

      case SemanticOp.Int64Op.Neg =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I64, Value.IntConst(0L, Type.I64), x))
        tmp

      case SemanticOp.Int64Op.Not =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I64, x, Value.IntConst(-1L, Type.I64)))
        tmp

      case SemanticOp.BigIntOp.Neg =>
        val tmp = freshTmp(Type.Ptr)
        val bigintPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_neg", List(ctxPtr, bigintPtr)))
        tmp

      case SemanticOp.BigIntOp.Not =>
        val tmp = freshTmp(Type.Ptr)
        val bigintPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_not", List(ctxPtr, bigintPtr)))
        tmp

      case SemanticOp.BigIntOp.BitLength =>
        val tmp = freshTmp(Type.I32)
        val bigintPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_bigint_bit_length", List(ctxPtr, bigintPtr)))
        tmp

      case SemanticOp.BigIntOp.FromInt64 =>
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_from_i64", List(ctxPtr, castValue(x, Type.I64, fb))))
        tmp

      case SemanticOp.BigDecimalOp.Neg =>
        val tmp = freshTmp(Type.Ptr)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_neg", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Scale =>
        val tmp = freshTmp(Type.I32)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_bigdec_scale", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Precision =>
        val tmp = freshTmp(Type.I32)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_bigdec_precision", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Ceil =>
        val tmp = freshTmp(Type.Ptr)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_ceil", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Floor =>
        val tmp = freshTmp(Type.Ptr)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_floor", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Round =>
        val tmp = freshTmp(Type.Ptr)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_round", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.BigDecimalOp.ToBigInt =>
        val tmp = freshTmp(Type.Ptr)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_to_bigint", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.BigDecimalOp.ToPlainString =>
        val tmp = freshTmp(Type.Ptr)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_to_plain_string", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.CodePointOp.IsLetter =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_letter", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsDigit =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_digit", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsLowerCase =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_lower_case", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsUpperCase =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_upper_case", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsTitleCase =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_title_case", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsWhitespace =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_whitespace", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsAlphabetic =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_alphabetic", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsDefined =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_defined", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsIdeographic =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_ideographic", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsISOControl =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_iso_control", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.IsMirrored =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_codepoint_is_mirrored", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.ToLowerCase =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_codepoint_to_lower_case", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.ToUpperCase =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_codepoint_to_upper_case", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.ToTitleCase =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_codepoint_to_title_case", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.GetName =>
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_codepoint_get_name", List(ctxPtr, castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.CodePointOp.GetNumericValue =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_codepoint_get_numeric_value", List(castValue(x, Type.I32, fb))))
        tmp

      case SemanticOp.Float64Op.Neg =>
        val bits = freshTmp(Type.I64)
        fb.current.emitAssign(bits, Op.Cast("bitcast", Type.I64, x))

        val flipped = freshTmp(Type.I64)
        fb.current.emitAssign(flipped, Op.Bin("xor", Type.I64, bits, Value.IntConst(Long.MinValue, Type.I64)))

        val res = freshTmp(Type.Double)
        fb.current.emitAssign(res, Op.Cast("bitcast", Type.Double, flipped))
        res

      case SemanticOp.ConvertOp.Int8ToInt16 =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I16, x))
        tmp

      case SemanticOp.ConvertOp.Int8ToInt32 =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I32, x))
        tmp

      case SemanticOp.ConvertOp.Int8ToInt64 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, x))
        tmp

      case SemanticOp.ConvertOp.Int8ToFloat32 =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Float, x))
        tmp

      case SemanticOp.ConvertOp.Int8ToFloat64 =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Double, x))
        tmp

      case SemanticOp.ConvertOp.Int16ToInt8 =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I8, x))
        tmp

      case SemanticOp.ConvertOp.Int16ToInt32 =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I32, x))
        tmp

      case SemanticOp.ConvertOp.Int16ToInt64 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, x))
        tmp

      case SemanticOp.ConvertOp.Int16ToFloat32 =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Float, x))
        tmp

      case SemanticOp.ConvertOp.Int16ToFloat64 =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Double, x))
        tmp

      case SemanticOp.ConvertOp.Int32ToInt8 =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I8, x))
        tmp

      case SemanticOp.ConvertOp.Int32ToInt16 =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I16, x))
        tmp

      case SemanticOp.ConvertOp.Int32ToInt64 =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I64, x))
        tmp

      case SemanticOp.ConvertOp.Int32ToFloat32 =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Float, x))
        tmp

      case SemanticOp.ConvertOp.Int32ToFloat64 =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Double, x))
        tmp

      case SemanticOp.ConvertOp.Int64ToInt8 =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I8, x))
        tmp

      case SemanticOp.ConvertOp.Int64ToInt16 =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I16, x))
        tmp

      case SemanticOp.ConvertOp.Int64ToInt32 =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, x))
        tmp

      case SemanticOp.ConvertOp.Int64ToFloat32 =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Float, x))
        tmp

      case SemanticOp.ConvertOp.Int64ToFloat64 =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Cast("sitofp", Type.Double, x))
        tmp

      case SemanticOp.ConvertOp.Float32ToInt8 =>
        val i32 = fpToInt32Saturating(x, fb)
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I8, i32))
        tmp

      case SemanticOp.ConvertOp.Float32ToInt16 =>
        val i32 = fpToInt32Saturating(x, fb)
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I16, i32))
        tmp

      case SemanticOp.ConvertOp.Float32ToInt32 =>
        fpToInt32Saturating(x, fb)

      case SemanticOp.ConvertOp.Float32ToInt64 =>
        fpToInt64Saturating(x, fb)

      case SemanticOp.ConvertOp.Float32ToFloat64 =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Cast("fpext", Type.Double, x))
        tmp

      case SemanticOp.ConvertOp.Float64ToInt8 =>
        val i32 = fpToInt32Saturating(x, fb)
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I8, i32))
        tmp

      case SemanticOp.ConvertOp.Float64ToInt16 =>
        val i32 = fpToInt32Saturating(x, fb)
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I16, i32))
        tmp

      case SemanticOp.ConvertOp.Float64ToInt32 =>
        fpToInt32Saturating(x, fb)

      case SemanticOp.ConvertOp.Float64ToInt64 =>
        fpToInt64Saturating(x, fb)

      case SemanticOp.ConvertOp.Float64ToFloat32 =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Cast("fptrunc", Type.Float, x))
        tmp

      case SemanticOp.PlatformOp.FileSeparator =>
        emitConstant(Constant.Str(java.io.File.separator), ctxPtr, fb)

      case SemanticOp.PlatformOp.PathSeparator =>
        emitConstant(Constant.Str(java.io.File.pathSeparator), ctxPtr, fb)

      case SemanticOp.PlatformOp.LineSeparator =>
        emitConstant(Constant.Str(System.lineSeparator()), ctxPtr, fb)

      case SemanticOp.ObjectOp.IsNull =>
        val asI64 = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("eq", asI64, Value.IntConst(0L, Type.I64)))
        tmp

      case SemanticOp.RegexOp.FlagCanonEq =>
        Value.IntConst(128L, Type.I32)

      case SemanticOp.RegexOp.FlagCaseInsensitive =>
        Value.IntConst(2L, Type.I32)

      case SemanticOp.RegexOp.FlagComments =>
        Value.IntConst(4L, Type.I32)

      case SemanticOp.RegexOp.FlagDotall =>
        Value.IntConst(32L, Type.I32)

      case SemanticOp.RegexOp.FlagLiteral =>
        Value.IntConst(16L, Type.I32)

      case SemanticOp.RegexOp.FlagMultiline =>
        Value.IntConst(8L, Type.I32)

      case SemanticOp.RegexOp.FlagUnicodeCase =>
        Value.IntConst(64L, Type.I32)

      case SemanticOp.RegexOp.FlagUnicodeCharacterClass =>
        Value.IntConst(256L, Type.I32)

      case SemanticOp.RegexOp.FlagUnixLines =>
        Value.IntConst(1L, Type.I32)

      case SemanticOp.RegexOp.Compile =>
        val pat = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_compile", List(pat)))
        tmp

      case SemanticOp.RegexOp.CompileWithFlags =>
        val flags = loadTupleElement(x, 0L, SimpleType.Int32, fb)
        val pat = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_compile_with_flags", List(flags, pat)))
        tmp

      case SemanticOp.RegexOp.TryCompile =>
        val pat = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_try_compile", List(pat)))
        tmp

      case SemanticOp.RegexOp.TryCompileWithFlags =>
        val flags = loadTupleElement(x, 0L, SimpleType.Int32, fb)
        val pat = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_try_compile_with_flags", List(flags, pat)))
        tmp

      case SemanticOp.RegexOp.Quote =>
        val in = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_quote", List(in)))
        tmp

      case SemanticOp.RegexOp.Pattern =>
        val rgx = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_pattern", List(rgx)))
        tmp

      case SemanticOp.RegexOp.Flags =>
        val rgx = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_regex_flags", List(rgx)))
        tmp

      case SemanticOp.RegexOp.NewMatcher =>
        // Argument: (rc, rgx, input).
        val rcPtr = castValue(loadTupleElement(x, 0L, SimpleType.Region, fb), Type.Ptr, fb)
        val rgx = loadTupleElement(x, 1L, SimpleType.Regex, fb)
        val in = loadTupleElement(x, 2L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_new_matcher", List(ctxPtr, rcPtr, rgx, in)))
        tmp

      case SemanticOp.RegexOp.MatcherMatches =>
        // Argument: (rc, matcher). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_regex_matcher_matches", List(m)))
        tmp

      case SemanticOp.RegexOp.MatcherFind =>
        // Argument: (rc, matcher). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_regex_matcher_find", List(m)))
        tmp

      case SemanticOp.RegexOp.MatcherFindFrom =>
        // Argument: (rc, matcher, pos). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val pos = loadTupleElement(x, 2L, SimpleType.Int32, fb)
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_regex_matcher_find_from", List(m, pos)))
        tmp

      case SemanticOp.RegexOp.MatcherLookingAt =>
        // Argument: (rc, matcher). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_regex_matcher_looking_at", List(m)))
        tmp

      case SemanticOp.RegexOp.MatcherReplaceAll =>
        // Argument: (rc, matcher, replacement). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val repl = loadTupleElement(x, 2L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_matcher_replace_all", List(m, repl)))
        tmp

      case SemanticOp.RegexOp.MatcherReplaceFirst =>
        // Argument: (rc, matcher, replacement). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val repl = loadTupleElement(x, 2L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_matcher_replace_first", List(m, repl)))
        tmp

      case SemanticOp.RegexOp.MatcherSetBounds =>
        // Argument: (rc, matcher, start, end).
        val rcPtr = castValue(loadTupleElement(x, 0L, SimpleType.Region, fb), Type.Ptr, fb)
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val start = loadTupleElement(x, 2L, SimpleType.Int32, fb)
        val end = loadTupleElement(x, 3L, SimpleType.Int32, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_regex_matcher_set_bounds", List(ctxPtr, rcPtr, m, start, end)))
        tmp

      case SemanticOp.RegexOp.MatcherStart =>
        // Argument: (rc, matcher). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_regex_matcher_start", List(m)))
        tmp

      case SemanticOp.RegexOp.MatcherEnd =>
        // Argument: (rc, matcher). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_regex_matcher_end", List(m)))
        tmp

      case SemanticOp.RegexOp.MatcherGroup =>
        // Argument: (rc, matcher, idx). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val idx = loadTupleElement(x, 2L, SimpleType.Int32, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_matcher_group", List(m, idx)))
        tmp

      case SemanticOp.RegexOp.MatcherGroupCount =>
        // Argument: (rc, matcher). rc currently ignored.
        val m = loadTupleElement(x, 1L, SimpleType.RegexMatcher, fb)
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_regex_matcher_group_count", List(m)))
        tmp

      case SemanticOp.RegexOp.Split =>
        // Argument: (rc, rgx, input).
        val rcPtr = castValue(loadTupleElement(x, 0L, SimpleType.Region, fb), Type.Ptr, fb)
        val rgx = loadTupleElement(x, 1L, SimpleType.Regex, fb)
        val in = loadTupleElement(x, 2L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_regex_split", List(ctxPtr, rcPtr, rgx, in)))
        tmp

      case SemanticOp.ToStringOp.CharToString =>
        emitCharToString(x, ctxPtr, fb)

      case SemanticOp.ToStringOp.Int8ToString =>
        emitIntToStringNoMin(castValue(x, Type.I64, fb), ctxPtr, fb)

      case SemanticOp.ToStringOp.Int16ToString =>
        emitIntToStringNoMin(castValue(x, Type.I64, fb), ctxPtr, fb)

      case SemanticOp.ToStringOp.Int32ToString =>
        emitIntToStringNoMin(castValue(x, Type.I64, fb), ctxPtr, fb)

      case SemanticOp.ToStringOp.Int64ToString =>
        val xi64 = castValue(x, Type.I64, fb)
        val isMin = freshTmp(Type.I1)
        fb.current.emitAssign(isMin, Op.ICmp("eq", xi64, Value.IntConst(Long.MinValue, Type.I64)))

        val minLabel = freshLabel("i64tos_min")
        val notLabel = freshLabel("i64tos_not")
        val endLabel = freshLabel("i64tos_end")

        fb.current.setTerminator(Terminator.CondBr(isMin, minLabel, notLabel))

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        val minBlock = fb.newBlock(minLabel)
        fb.setCurrent(minBlock)
        val minStr = emitConstant(Constant.Str("-9223372036854775808"), ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((minStr, minLabel))

        val notBlock = fb.newBlock(notLabel)
        fb.setCurrent(notBlock)
        val s = emitIntToStringNoMin(xi64, ctxPtr, fb)
        val sLabel = fb.current.label
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((s, sLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val phi = freshTmp(Type.Ptr)
        endBlock.emitPhi(phi, incomings.toList)
        phi

      case SemanticOp.ToStringOp.BigIntToString =>
        val tmp = freshTmp(Type.Ptr)
        val bigintPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_to_string", List(ctxPtr, bigintPtr)))
        tmp

      case SemanticOp.ToStringOp.BigDecimalToString =>
        val tmp = freshTmp(Type.Ptr)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_to_string", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.ToStringOp.Float32ToString =>
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_float32_to_string", List(x)))
        tmp

      case SemanticOp.ToStringOp.Float64ToString =>
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_float64_to_string", List(x)))
        tmp

      case SemanticOp.StringBuilderOp.New =>
        emitStringBuilderNew(ctxPtr, x, fb)

      case SemanticOp.StringBuilderOp.AppendString =>
        emitStringBuilderAppendString(ctxPtr, x, fb)

      case SemanticOp.StringBuilderOp.AppendCodePoint =>
        emitStringBuilderAppendCodePoint(ctxPtr, x, fb)

      case SemanticOp.StringBuilderOp.CharAt =>
        emitStringBuilderCharAt(x, fb)

      case SemanticOp.StringBuilderOp.Length =>
        emitStringBuilderLength(x, fb)

      case SemanticOp.StringBuilderOp.SetLength =>
        emitStringBuilderSetLength(ctxPtr, x, fb)

      case SemanticOp.StringBuilderOp.ToString =>
        emitStringBuilderToString(ctxPtr, x, fb)

      case SemanticOp.ParseOp.Int8FromString =>
        emitParseIntTuple(x, Value.IntConst(10L, Type.I64), -128L, 127L, SimpleType.Int8, ctxPtr, fb)

      case SemanticOp.ParseOp.Int16FromString =>
        emitParseIntTuple(x, Value.IntConst(10L, Type.I64), -32768L, 32767L, SimpleType.Int16, ctxPtr, fb)

      case SemanticOp.ParseOp.Int32FromString =>
        emitParseIntTuple(x, Value.IntConst(10L, Type.I64), Int.MinValue.toLong, Int.MaxValue.toLong, SimpleType.Int32, ctxPtr, fb)

      case SemanticOp.ParseOp.Int64FromString =>
        emitParseIntTuple(x, Value.IntConst(10L, Type.I64), Long.MinValue, Long.MaxValue, SimpleType.Int64, ctxPtr, fb)

      case SemanticOp.ParseOp.Int32Parse =>
        val radixI64 = castValue(loadTupleElement(x, 0, SimpleType.Int32, fb), Type.I64, fb)
        val sPtr = loadTupleElement(x, 1, SimpleType.String, fb)
        emitParseIntTuple(sPtr, radixI64, Int.MinValue.toLong, Int.MaxValue.toLong, SimpleType.Int32, ctxPtr, fb)

      case SemanticOp.ParseOp.Int64Parse =>
        val radixI64 = castValue(loadTupleElement(x, 0, SimpleType.Int32, fb), Type.I64, fb)
        val sPtr = loadTupleElement(x, 1, SimpleType.String, fb)
        emitParseIntTuple(sPtr, radixI64, Long.MinValue, Long.MaxValue, SimpleType.Int64, ctxPtr, fb)

      case SemanticOp.ParseOp.Float32FromString =>
        emitParseFloatTuple(x, is32 = true, ctxPtr, fb)

      case SemanticOp.ParseOp.Float64FromString =>
        emitParseFloatTuple(x, is32 = false, ctxPtr, fb)

      case SemanticOp.ParseOp.BigIntFromString =>
        val strPtr = castValue(x, Type.Ptr, fb)
        val parsedPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(parsedPtr, Op.Call(Type.Ptr, "flix_bigint_try_parse", List(ctxPtr, strPtr)))

        val ok = freshTmp(Type.I1)
        fb.current.emitAssign(ok, Op.ICmp("ne", parsedPtr, Value.Null(Type.Ptr)))

        val okLabel = freshLabel("parsebigint_ok")
        val failLabel = freshLabel("parsebigint_fail")
        val endLabel = freshLabel("parsebigint_end")
        fb.current.setTerminator(Terminator.CondBr(ok, okLabel, failLabel))

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        val tupleTpe = SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.BigInt))

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)
        val okPayload = Value.IntConst(1L, Type.I64)
        val valuePayload = boxToI64(parsedPtr, SimpleType.BigInt, fb)
        val okTuple = allocTuple2(tupleTpe, okPayload, valuePayload, ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((okTuple, okLabel))

        val failBlock = fb.newBlock(failLabel)
        fb.setCurrent(failBlock)
        val zeroPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(zeroPtr, Op.Call(Type.Ptr, "flix_bigint_from_i64", List(ctxPtr, Value.IntConst(0L, Type.I64))))
        val failPayload = Value.IntConst(0L, Type.I64)
        val zeroPayload = boxToI64(zeroPtr, SimpleType.BigInt, fb)
        val failTuple = allocTuple2(tupleTpe, failPayload, zeroPayload, ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((failTuple, failLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val phi = freshTmp(Type.Ptr)
        endBlock.emitPhi(phi, incomings.toList)
        phi

      case SemanticOp.ParseOp.BigDecimalFromString =>
        val strPtr = castValue(x, Type.Ptr, fb)
        val parsedPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(parsedPtr, Op.Call(Type.Ptr, "flix_bigdec_try_parse", List(ctxPtr, strPtr)))

        val ok = freshTmp(Type.I1)
        fb.current.emitAssign(ok, Op.ICmp("ne", parsedPtr, Value.Null(Type.Ptr)))

        val okLabel = freshLabel("parsebigdec_ok")
        val failLabel = freshLabel("parsebigdec_fail")
        val endLabel = freshLabel("parsebigdec_end")
        fb.current.setTerminator(Terminator.CondBr(ok, okLabel, failLabel))

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]
        val tupleTpe = SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.BigDecimal))

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)
        val okPayload = Value.IntConst(1L, Type.I64)
        val valuePayload = boxToI64(parsedPtr, SimpleType.BigDecimal, fb)
        val okTuple = allocTuple2(tupleTpe, okPayload, valuePayload, ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((okTuple, okLabel))

        val failBlock = fb.newBlock(failLabel)
        fb.setCurrent(failBlock)
        val zeroStr = emitConstant(Constant.Str("0"), ctxPtr, fb)
        val zeroPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(zeroPtr, Op.Call(Type.Ptr, "flix_bigdec_from_string", List(ctxPtr, zeroStr)))
        val failPayload = Value.IntConst(0L, Type.I64)
        val zeroPayload = boxToI64(zeroPtr, SimpleType.BigDecimal, fb)
        val failTuple = allocTuple2(tupleTpe, failPayload, zeroPayload, ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((failTuple, failLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val phi = freshTmp(Type.Ptr)
        endBlock.emitPhi(phi, incomings.toList)
        phi

      case SemanticOp.StringOp.Length =>
        stringLenI32(x, fb)

      case SemanticOp.StringOp.ToLowerCase =>
        val inPtr = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_string_to_lower_case", List(ctxPtr, inPtr)))
        tmp

      case SemanticOp.StringOp.ToUpperCase =>
        val inPtr = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_string_to_upper_case", List(ctxPtr, inPtr)))
        tmp

      case SemanticOp.HashOp.CharHash =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, x))
        tmp

      case SemanticOp.HashOp.Float32Hash =>
        val isNaN = freshTmp(Type.I1)
        fb.current.emitAssign(isNaN, Op.FCmp("uno", x, x))

        val nanLabel = freshLabel("f32hash_nan")
        val notNanLabel = freshLabel("f32hash_notnan")
        val endLabel = freshLabel("f32hash_end")

        fb.current.setTerminator(Terminator.CondBr(isNaN, nanLabel, notNanLabel))

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        val nanBlock = fb.newBlock(nanLabel)
        fb.setCurrent(nanBlock)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((Value.IntConst(0x7fc00000L, Type.I32), nanLabel))

        val notNanBlock = fb.newBlock(notNanLabel)
        fb.setCurrent(notNanBlock)
        val bits = freshTmp(Type.I32)
        fb.current.emitAssign(bits, Op.Cast("bitcast", Type.I32, x))
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((bits, notNanLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val phi = freshTmp(Type.I32)
        endBlock.emitPhi(phi, incomings.toList)
        phi

      case SemanticOp.HashOp.Float64Hash =>
        val isNaN = freshTmp(Type.I1)
        fb.current.emitAssign(isNaN, Op.FCmp("uno", x, x))

        val nanLabel = freshLabel("f64hash_nan")
        val notNanLabel = freshLabel("f64hash_notnan")
        val endLabel = freshLabel("f64hash_end")

        fb.current.setTerminator(Terminator.CondBr(isNaN, nanLabel, notNanLabel))

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        val nanBlock = fb.newBlock(nanLabel)
        fb.setCurrent(nanBlock)
        val bitsNan = Value.IntConst(0x7ff8000000000000L, Type.I64)
        val shiftNan = freshTmp(Type.I64)
        fb.current.emitAssign(shiftNan, Op.Bin("lshr", Type.I64, bitsNan, Value.IntConst(32L, Type.I64)))
        val xoredNan = freshTmp(Type.I64)
        fb.current.emitAssign(xoredNan, Op.Bin("xor", Type.I64, bitsNan, shiftNan))
        val hashNan = freshTmp(Type.I32)
        fb.current.emitAssign(hashNan, Op.Cast("trunc", Type.I32, xoredNan))
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((hashNan, nanLabel))

        val notNanBlock = fb.newBlock(notNanLabel)
        fb.setCurrent(notNanBlock)
        val bits = freshTmp(Type.I64)
        fb.current.emitAssign(bits, Op.Cast("bitcast", Type.I64, x))
        val shift = freshTmp(Type.I64)
        fb.current.emitAssign(shift, Op.Bin("lshr", Type.I64, bits, Value.IntConst(32L, Type.I64)))
        val xored = freshTmp(Type.I64)
        fb.current.emitAssign(xored, Op.Bin("xor", Type.I64, bits, shift))
        val hash = freshTmp(Type.I32)
        fb.current.emitAssign(hash, Op.Cast("trunc", Type.I32, xored))
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((hash, notNanLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val phi = freshTmp(Type.I32)
        endBlock.emitPhi(phi, incomings.toList)
        phi

      case SemanticOp.HashOp.Int8Hash =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I32, x))
        tmp

      case SemanticOp.HashOp.Int16Hash =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("sext", Type.I32, x))
        tmp

      case SemanticOp.HashOp.Int32Hash =>
        x

      case SemanticOp.HashOp.Int64Hash =>
        val shifted = freshTmp(Type.I64)
        fb.current.emitAssign(shifted, Op.Bin("lshr", Type.I64, x, Value.IntConst(32L, Type.I64)))
        val xored = freshTmp(Type.I64)
        fb.current.emitAssign(xored, Op.Bin("xor", Type.I64, x, shifted))
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, xored))
        tmp

      case SemanticOp.HashOp.BigIntHash =>
        val tmp = freshTmp(Type.I32)
        val bigintPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_bigint_hash", List(ctxPtr, bigintPtr)))
        tmp

      case SemanticOp.HashOp.BigDecimalHash =>
        val tmp = freshTmp(Type.I32)
        val bigdecPtr = castValue(x, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_bigdec_hash", List(ctxPtr, bigdecPtr)))
        tmp

      case SemanticOp.HashOp.StringHash =>
        val strPtr = castValue(x, Type.Ptr, fb)
        val lenI64 = stringLenI64(strPtr, fb)

        val iPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

        val hPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(hPtr, Op.Alloca(Type.I32))
        fb.current.emitStore(Value.IntConst(0L, Type.I32), hPtr)

        val loopLabel = freshLabel("strhash_loop")
        val bodyLabel = freshLabel("strhash_body")
        val endLabel = freshLabel("strhash_end")

        fb.current.setTerminator(Terminator.Br(loopLabel))

        val loopBlock = fb.newBlock(loopLabel)
        fb.setCurrent(loopBlock)
        val iVal = freshTmp(Type.I64)
        fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
        val cond = freshTmp(Type.I1)
        fb.current.emitAssign(cond, Op.ICmp("slt", iVal, lenI64))
        fb.current.setTerminator(Terminator.CondBr(cond, bodyLabel, endLabel))

        val bodyBlock = fb.newBlock(bodyLabel)
        fb.setCurrent(bodyBlock)
        val payload = stringCharPayloadI64(strPtr, iVal, fb)
        val ch = freshTmp(Type.I32)
        fb.current.emitAssign(ch, Op.Cast("trunc", Type.I32, payload))

        val hVal = freshTmp(Type.I32)
        fb.current.emitAssign(hVal, Op.Load(Type.I32, hPtr))
        val hMul = freshTmp(Type.I32)
        fb.current.emitAssign(hMul, Op.Bin("mul", Type.I32, hVal, Value.IntConst(31L, Type.I32)))
        val hNext = freshTmp(Type.I32)
        fb.current.emitAssign(hNext, Op.Bin("add", Type.I32, hMul, ch))
        fb.current.emitStore(hNext, hPtr)

        val iNext = freshTmp(Type.I64)
        fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(iNext, iPtr)
        fb.current.setTerminator(Terminator.Br(loopLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val res = freshTmp(Type.I32)
        fb.current.emitAssign(res, Op.Load(Type.I32, hPtr))
        res

      case SemanticOp.IoOp.Print =>
        val s = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_print", List(s)))
        tmp

      case SemanticOp.IoOp.EPrint =>
        val s = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_eprint", List(s)))
        tmp

      case SemanticOp.IoOp.Println =>
        val s = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_println", List(s)))
        tmp

      case SemanticOp.IoOp.EPrintln =>
        val s = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_eprintln", List(s)))
        tmp

      case SemanticOp.IoOp.Readln =>
        val unit = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_readln", List(unit)))
        tmp

      case SemanticOp.IoOp.SleepMillis =>
        val ms = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_sleep_millis", List(ms)))
        tmp

      case SemanticOp.IoOp.Exit =>
        val code = castValue(x, Type.I32, fb)
        fb.current.emitCallVoid("flix_exit", List(code))
        fb.current.setTerminator(Terminator.Unreachable)
        Value.Undef(Type.I64)

      case SemanticOp.IoOp.NewId =>
        val unit = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_new_id", List(unit)))
        tmp

      case SemanticOp.IoOp.TimeNowMillis =>
        val unit = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Call(Type.I64, "flix_time_now_ms", List(unit)))
        tmp

      case SemanticOp.IoOp.FileExists =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_exists", List(path)))
        tmp

      case SemanticOp.IoOp.FileIsDirectory =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_is_directory", List(path)))
        tmp

      case SemanticOp.IoOp.FileIsRegularFile =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_is_regular_file", List(path)))
        tmp

      case SemanticOp.IoOp.FileIsReadable =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_is_readable", List(path)))
        tmp

      case SemanticOp.IoOp.FileIsSymbolicLink =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_is_symbolic_link", List(path)))
        tmp

      case SemanticOp.IoOp.FileIsWritable =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_is_writable", List(path)))
        tmp

      case SemanticOp.IoOp.FileIsExecutable =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_is_executable", List(path)))
        tmp

      case SemanticOp.IoOp.FileAccessTime =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_access_time", List(path)))
        tmp

      case SemanticOp.IoOp.FileCreationTime =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_creation_time", List(path)))
        tmp

      case SemanticOp.IoOp.FileModificationTime =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_modification_time", List(path)))
        tmp

      case SemanticOp.IoOp.FileSize =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_size", List(path)))
        tmp

      case SemanticOp.IoOp.FileRead =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_read", List(path)))
        tmp

      case SemanticOp.IoOp.FileReadLines =>
        val rc = loadTupleElement(x, 0L, SimpleType.Region, fb)
        val path = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_read_lines", List(ctxPtr, rc, path)))
        tmp

      case SemanticOp.IoOp.FileReadBytes =>
        val rc = loadTupleElement(x, 0L, SimpleType.Region, fb)
        val path = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_read_bytes", List(ctxPtr, rc, path)))
        tmp

      case SemanticOp.IoOp.FileList =>
        val rc = loadTupleElement(x, 0L, SimpleType.Region, fb)
        val path = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_list", List(ctxPtr, rc, path)))
        tmp

      case SemanticOp.IoOp.FileWrite =>
        val data = loadTupleElement(x, 0L, SimpleType.String, fb)
        val path = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_write", List(data, path)))
        tmp

      case SemanticOp.IoOp.FileWriteBytes =>
        val bytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
        val path = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_write_bytes", List(bytes, path)))
        tmp

      case SemanticOp.IoOp.FileAppend =>
        val data = loadTupleElement(x, 0L, SimpleType.String, fb)
        val path = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_append", List(data, path)))
        tmp

      case SemanticOp.IoOp.FileAppendBytes =>
        val bytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
        val path = loadTupleElement(x, 1L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_append_bytes", List(bytes, path)))
        tmp

      case SemanticOp.IoOp.FileTruncate =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_truncate", List(path)))
        tmp

      case SemanticOp.IoOp.FileMkDir =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_mkdir", List(path)))
        tmp

      case SemanticOp.IoOp.FileMkDirs =>
        val path = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_mkdirs", List(path)))
        tmp

      case SemanticOp.IoOp.FileMkTempDir =>
        val prefix = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_file_mk_temp_dir", List(prefix)))
        tmp

      case SemanticOp.IoOp.TcpSocketRead =>
        val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
        val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_socket_read", List(id, buf)))
        tmp

      case SemanticOp.IoOp.TcpSocketWrite =>
        val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
        val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_socket_write", List(id, buf)))
        tmp

      case SemanticOp.IoOp.TcpSocketConnect =>
        val ipBytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
        val port = loadTupleElement(x, 1L, SimpleType.Int32, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_socket_connect", List(ipBytes, port)))
        tmp

      case SemanticOp.IoOp.TcpSocketClose =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_socket_close", List(id)))
        tmp

      case SemanticOp.IoOp.TcpServerBind =>
        val ipBytes = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.Int8), fb)
        val port = loadTupleElement(x, 1L, SimpleType.Int32, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_server_bind", List(ipBytes, port)))
        tmp

      case SemanticOp.IoOp.TcpServerLocalPort =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_server_local_port", List(id)))
        tmp

      case SemanticOp.IoOp.TcpServerAccept =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_server_accept", List(id)))
        tmp

      case SemanticOp.IoOp.TcpServerClose =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_tcp_server_close", List(id)))
        tmp

      case SemanticOp.IoOp.ProcessStdinWrite =>
        val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
        val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_stdin_write", List(id, buf)))
        tmp

      case SemanticOp.IoOp.ProcessExec =>
        val argv = loadTupleElement(x, 0L, SimpleType.Array(SimpleType.String), fb)
        val hasCwd = loadTupleElement(x, 1L, SimpleType.Bool, fb)
        val cwdStr = loadTupleElement(x, 2L, SimpleType.String, fb)
        val envPairs = loadTupleElement(x, 3L, SimpleType.Array(SimpleType.String), fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_exec", List(argv, hasCwd, cwdStr, envPairs)))
        tmp

      case SemanticOp.IoOp.ProcessExitValue =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_exit_value", List(id)))
        tmp

      case SemanticOp.IoOp.ProcessIsAlive =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_is_alive", List(id)))
        tmp

      case SemanticOp.IoOp.ProcessPid =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_pid", List(id)))
        tmp

      case SemanticOp.IoOp.ProcessStop =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_stop", List(id)))
        tmp

      case SemanticOp.IoOp.ProcessWaitFor =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_wait_for", List(id)))
        tmp

      case SemanticOp.IoOp.ProcessWaitForTimeout =>
        val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
        val timeoutMs = loadTupleElement(x, 1L, SimpleType.Int64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_wait_for_timeout", List(id, timeoutMs)))
        tmp

      case SemanticOp.IoOp.ProcessStdoutRead =>
        val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
        val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_stdout_read", List(id, buf)))
        tmp

      case SemanticOp.IoOp.ProcessStderrRead =>
        val id = loadTupleElement(x, 0L, SimpleType.Int64, fb)
        val buf = loadTupleElement(x, 1L, SimpleType.Array(SimpleType.Int8), fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_stderr_read", List(id, buf)))
        tmp

      case SemanticOp.IoOp.ProcessRelease =>
        val id = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_process_release", List(id)))
        tmp

      case SemanticOp.IoOp.HttpRequest =>
        val method = loadTupleElement(x, 0L, SimpleType.String, fb)
        val url = loadTupleElement(x, 1L, SimpleType.String, fb)
        val headers = loadTupleElement(x, 2L, SimpleType.Array(SimpleType.String), fb)
        val hasBody = loadTupleElement(x, 3L, SimpleType.Bool, fb)
        val body = loadTupleElement(x, 4L, SimpleType.String, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_http_request", List(ctxPtr, method, url, headers, hasBody, body)))
        tmp

      case SemanticOp.IoOp.EnvGetArgs =>
        val region = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_env_get_args", List(ctxPtr, region)))
        tmp

      case SemanticOp.IoOp.EnvGetEnvPairs =>
        val region = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_env_get_env_pairs", List(ctxPtr, region)))
        tmp

      case SemanticOp.IoOp.EnvGetVar =>
        val name = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_env_get_var", List(name)))
        tmp

      case SemanticOp.IoOp.EnvGetProp =>
        val name = castValue(x, Type.Ptr, fb)
        val tmp = freshTmp(Type.Ptr)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_env_get_prop", List(name)))
        tmp

      case SemanticOp.IoOp.EnvVirtualProcessors =>
        val unit = castValue(x, Type.I64, fb)
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_env_virtual_processors", List(unit)))
        tmp

      case _ =>
        fb.current.emitTrap()
        Value.Undef(x.tpe)
    }

    private def emitCharToString(ch: Value, ctxPtr: Value, fb: FunBuilder): Value = {
      val payload = boxToI64(ch, SimpleType.Char, fb)
      val strPtr = allocString(Value.IntConst(1L, Type.I64), ctxPtr, fb)
      storeStringCharPayload(strPtr, Value.IntConst(0L, Type.I64), payload, fb)
      strPtr
    }

    private def emitIntToStringNoMin(xI64: Value, ctxPtr: Value, fb: FunBuilder): Value = {
      val isNeg = freshTmp(Type.I1)
      fb.current.emitAssign(isNeg, Op.ICmp("slt", xI64, Value.IntConst(0L, Type.I64)))

      val absPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(absPtr, Op.Alloca(Type.I64))

      val negLabel = freshLabel("itos_neg")
      val posLabel = freshLabel("itos_pos")
      val absLabel = freshLabel("itos_abs")

      fb.current.setTerminator(Terminator.CondBr(isNeg, negLabel, posLabel))

      val negBlock = fb.newBlock(negLabel)
      fb.setCurrent(negBlock)
      val absNeg = freshTmp(Type.I64)
      fb.current.emitAssign(absNeg, Op.Bin("sub", Type.I64, Value.IntConst(0L, Type.I64), xI64))
      fb.current.emitStore(absNeg, absPtr)
      fb.current.setTerminator(Terminator.Br(absLabel))

      val posBlock = fb.newBlock(posLabel)
      fb.setCurrent(posBlock)
      fb.current.emitStore(xI64, absPtr)
      fb.current.setTerminator(Terminator.Br(absLabel))

      val absBlock = fb.newBlock(absLabel)
      fb.setCurrent(absBlock)
      val absVal = freshTmp(Type.I64)
      fb.current.emitAssign(absVal, Op.Load(Type.I64, absPtr))

      // Count digits in `absVal` (absVal >= 0).
      val nPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(nPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(absVal, nPtr)

      val digitsPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(digitsPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(1L, Type.I64), digitsPtr)

      val countLoopLabel = freshLabel("itos_count_loop")
      val countBodyLabel = freshLabel("itos_count_body")
      val countDoneLabel = freshLabel("itos_count_done")
      fb.current.setTerminator(Terminator.Br(countLoopLabel))

      val countLoopBlock = fb.newBlock(countLoopLabel)
      fb.setCurrent(countLoopBlock)
      val nVal = freshTmp(Type.I64)
      fb.current.emitAssign(nVal, Op.Load(Type.I64, nPtr))
      val cond = freshTmp(Type.I1)
      fb.current.emitAssign(cond, Op.ICmp("sge", nVal, Value.IntConst(10L, Type.I64)))
      fb.current.setTerminator(Terminator.CondBr(cond, countBodyLabel, countDoneLabel))

      val countBodyBlock = fb.newBlock(countBodyLabel)
      fb.setCurrent(countBodyBlock)
      val nNext = freshTmp(Type.I64)
      fb.current.emitAssign(nNext, Op.Bin("sdiv", Type.I64, nVal, Value.IntConst(10L, Type.I64)))
      fb.current.emitStore(nNext, nPtr)
      val dVal = freshTmp(Type.I64)
      fb.current.emitAssign(dVal, Op.Load(Type.I64, digitsPtr))
      val dNext = freshTmp(Type.I64)
      fb.current.emitAssign(dNext, Op.Bin("add", Type.I64, dVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(dNext, digitsPtr)
      fb.current.setTerminator(Terminator.Br(countLoopLabel))

      val countDoneBlock = fb.newBlock(countDoneLabel)
      fb.setCurrent(countDoneBlock)
      val digits = freshTmp(Type.I64)
      fb.current.emitAssign(digits, Op.Load(Type.I64, digitsPtr))

      val signOffset = freshTmp(Type.I64)
      fb.current.emitAssign(signOffset, Op.Cast("zext", Type.I64, isNeg))

      val totalLen = freshTmp(Type.I64)
      fb.current.emitAssign(totalLen, Op.Bin("add", Type.I64, digits, signOffset))

      val strPtr = allocString(totalLen, ctxPtr, fb)

      // Fill digits from the end.
      val kPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(kPtr, Op.Alloca(Type.I64))
      val k0 = freshTmp(Type.I64)
      fb.current.emitAssign(k0, Op.Bin("sub", Type.I64, totalLen, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(k0, kPtr)

      val mPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(mPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(absVal, mPtr)

      val fillLoopLabel = freshLabel("itos_fill_loop")
      val fillBodyLabel = freshLabel("itos_fill_body")
      val fillDoneLabel = freshLabel("itos_fill_done")
      fb.current.setTerminator(Terminator.Br(fillLoopLabel))

      val fillLoopBlock = fb.newBlock(fillLoopLabel)
      fb.setCurrent(fillLoopBlock)
      val kVal = freshTmp(Type.I64)
      fb.current.emitAssign(kVal, Op.Load(Type.I64, kPtr))
      val cond2 = freshTmp(Type.I1)
      fb.current.emitAssign(cond2, Op.ICmp("sge", kVal, signOffset))
      fb.current.setTerminator(Terminator.CondBr(cond2, fillBodyLabel, fillDoneLabel))

      val fillBodyBlock = fb.newBlock(fillBodyLabel)
      fb.setCurrent(fillBodyBlock)
      val mVal = freshTmp(Type.I64)
      fb.current.emitAssign(mVal, Op.Load(Type.I64, mPtr))
      val digit = freshTmp(Type.I64)
      fb.current.emitAssign(digit, Op.Bin("srem", Type.I64, mVal, Value.IntConst(10L, Type.I64)))
      val mNext = freshTmp(Type.I64)
      fb.current.emitAssign(mNext, Op.Bin("sdiv", Type.I64, mVal, Value.IntConst(10L, Type.I64)))
      fb.current.emitStore(mNext, mPtr)

      val ch = freshTmp(Type.I64)
      fb.current.emitAssign(ch, Op.Bin("add", Type.I64, digit, Value.IntConst(48L, Type.I64)))

      storeStringCharPayload(strPtr, kVal, ch, fb)

      val kNext = freshTmp(Type.I64)
      fb.current.emitAssign(kNext, Op.Bin("sub", Type.I64, kVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(kNext, kPtr)
      fb.current.setTerminator(Terminator.Br(fillLoopLabel))

      val fillDoneBlock = fb.newBlock(fillDoneLabel)
      fb.setCurrent(fillDoneBlock)

      val signLabel = freshLabel("itos_sign")
      val endLabel = freshLabel("itos_end")
      fb.current.setTerminator(Terminator.CondBr(isNeg, signLabel, endLabel))

      val signBlock = fb.newBlock(signLabel)
      fb.setCurrent(signBlock)
      storeStringCharPayload(strPtr, Value.IntConst(0L, Type.I64), Value.IntConst(45L, Type.I64), fb) // '-'
      fb.current.setTerminator(Terminator.Br(endLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      strPtr
    }

    private def sbLenI64(sbPtr0: Value, fb: FunBuilder): Value =
      loadI64Slot(sbPtr0, Value.IntConst(0L, Type.I64), fb)

    private def sbCapI64(sbPtr0: Value, fb: FunBuilder): Value =
      loadI64Slot(sbPtr0, Value.IntConst(1L, Type.I64), fb)

    private def sbDataPtr(sbPtr0: Value, fb: FunBuilder): Value = {
      val bits = loadI64Slot(sbPtr0, Value.IntConst(2L, Type.I64), fb)
      castValue(bits, Type.Ptr, fb)
    }

    private def sbStoreLen(sbPtr0: Value, lenI64: Value, fb: FunBuilder): Unit =
      storeI64Slot(sbPtr0, Value.IntConst(0L, Type.I64), lenI64, fb)

    private def sbStoreCap(sbPtr0: Value, capI64: Value, fb: FunBuilder): Unit =
      storeI64Slot(sbPtr0, Value.IntConst(1L, Type.I64), capI64, fb)

    private def sbStoreDataPtr(sbPtr0: Value, dataPtr: Value, fb: FunBuilder): Unit = {
      val bits = freshTmp(Type.I64)
      fb.current.emitAssign(bits, Op.Cast("ptrtoint", Type.I64, dataPtr))
      storeI64Slot(sbPtr0, Value.IntConst(2L, Type.I64), bits, fb)
    }

    private def sbEnsureCapacity(ctxPtr: Value, rcPtr: Value, sbPtr0: Value, neededLenI64: Value, fb: FunBuilder): Unit = {
      val sbPtr = castValue(sbPtr0, Type.Ptr, fb)
      val cap = sbCapI64(sbPtr, fb)
      val enough = freshTmp(Type.I1)
      fb.current.emitAssign(enough, Op.ICmp("sge", cap, neededLenI64))

      val okLabel = freshLabel("sbcap_ok")
      val growLabel = freshLabel("sbcap_grow")
      val endLabel = freshLabel("sbcap_end")
      fb.current.setTerminator(Terminator.CondBr(enough, okLabel, growLabel))

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))

      val growBlock = fb.newBlock(growLabel)
      fb.setCurrent(growBlock)

      // Compute newCap by doubling until it can hold neededLen.
      val newCapPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(newCapPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(cap, newCapPtr)

      val capLoopLabel = freshLabel("sbcap_loop")
      val capBodyLabel = freshLabel("sbcap_body")
      val capDoneLabel = freshLabel("sbcap_done")
      fb.current.setTerminator(Terminator.Br(capLoopLabel))

      val capLoopBlock = fb.newBlock(capLoopLabel)
      fb.setCurrent(capLoopBlock)
      val ncVal = freshTmp(Type.I64)
      fb.current.emitAssign(ncVal, Op.Load(Type.I64, newCapPtr))
      val needMore = freshTmp(Type.I1)
      fb.current.emitAssign(needMore, Op.ICmp("slt", ncVal, neededLenI64))
      fb.current.setTerminator(Terminator.CondBr(needMore, capBodyLabel, capDoneLabel))

      val capBodyBlock = fb.newBlock(capBodyLabel)
      fb.setCurrent(capBodyBlock)
      val ncNext = freshTmp(Type.I64)
      fb.current.emitAssign(ncNext, Op.Bin("mul", Type.I64, ncVal, Value.IntConst(2L, Type.I64)))
      fb.current.emitStore(ncNext, newCapPtr)
      fb.current.setTerminator(Terminator.Br(capLoopLabel))

      val capDoneBlock = fb.newBlock(capDoneLabel)
      fb.setCurrent(capDoneBlock)
      val newCap = freshTmp(Type.I64)
      fb.current.emitAssign(newCap, Op.Load(Type.I64, newCapPtr))

      val sizeBytes = freshTmp(Type.I64)
      fb.current.emitAssign(sizeBytes, Op.Bin("mul", Type.I64, newCap, Value.IntConst(8L, Type.I64)))
      val newBuf = freshTmp(Type.Ptr)
      fb.current.emitAssign(newBuf, Op.Call(Type.Ptr, "flix_region_malloc", List(ctxPtr, rcPtr, sizeBytes)))

      // Copy existing data.
      val oldBuf = sbDataPtr(sbPtr, fb)
      val len = sbLenI64(sbPtr, fb)

      val iPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

      val copyLoopLabel = freshLabel("sbcap_copy_loop")
      val copyBodyLabel = freshLabel("sbcap_copy_body")
      val copyDoneLabel = freshLabel("sbcap_copy_done")
      fb.current.setTerminator(Terminator.Br(copyLoopLabel))

      val copyLoopBlock = fb.newBlock(copyLoopLabel)
      fb.setCurrent(copyLoopBlock)
      val iVal = freshTmp(Type.I64)
      fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
      val copyCond = freshTmp(Type.I1)
      fb.current.emitAssign(copyCond, Op.ICmp("slt", iVal, len))
      fb.current.setTerminator(Terminator.CondBr(copyCond, copyBodyLabel, copyDoneLabel))

      val copyBodyBlock = fb.newBlock(copyBodyLabel)
      fb.setCurrent(copyBodyBlock)
      val payload = loadI64Slot(oldBuf, iVal, fb)
      storeI64Slot(newBuf, iVal, payload, fb)
      val iNext = freshTmp(Type.I64)
      fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(iNext, iPtr)
      fb.current.setTerminator(Terminator.Br(copyLoopLabel))

      val copyDoneBlock = fb.newBlock(copyDoneLabel)
      fb.setCurrent(copyDoneBlock)

      // Install the new buffer. The current implementation still leaks the old buffer.
      sbStoreDataPtr(sbPtr, newBuf, fb)
      sbStoreCap(sbPtr, newCap, fb)

      fb.current.setTerminator(Terminator.Br(endLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
    }

    private def emitStringBuilderNew(ctxPtr: Value, rc0: Value, fb: FunBuilder): Value = {
      val rcPtr = castValue(rc0, Type.Ptr, fb)

      // Layout: [0]=len (i64), [1]=cap (i64), [2]=dataPtr bits (i64).
      val handlePtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(handlePtr, Op.Call(Type.Ptr, "flix_region_malloc", List(ctxPtr, rcPtr, Value.IntConst(24L, Type.I64))))

      val initCap = Value.IntConst(16L, Type.I64)
      val bufBytes = freshTmp(Type.I64)
      fb.current.emitAssign(bufBytes, Op.Bin("mul", Type.I64, initCap, Value.IntConst(8L, Type.I64)))
      val bufPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(bufPtr, Op.Call(Type.Ptr, "flix_region_malloc", List(ctxPtr, rcPtr, bufBytes)))

      sbStoreLen(handlePtr, Value.IntConst(0L, Type.I64), fb)
      sbStoreCap(handlePtr, initCap, fb)
      sbStoreDataPtr(handlePtr, bufPtr, fb)
      handlePtr
    }

    private def emitStringBuilderAppendString(ctxPtr: Value, argsTuple: Value, fb: FunBuilder): Value = {
      val rcPtr = castValue(loadTupleElement(argsTuple, 0, SimpleType.Region, fb), Type.Ptr, fb)
      val sbPtr = loadTupleElement(argsTuple, 1, SimpleType.StringBuilderHandle, fb)
      val sPtr = loadTupleElement(argsTuple, 2, SimpleType.String, fb)

      val sbLen = sbLenI64(sbPtr, fb)
      val sLen = stringLenI64(sPtr, fb)

      val newLen = freshTmp(Type.I64)
      fb.current.emitAssign(newLen, Op.Bin("add", Type.I64, sbLen, sLen))

      sbEnsureCapacity(ctxPtr, rcPtr, sbPtr, newLen, fb)

      val dataPtr = sbDataPtr(sbPtr, fb)

      val iPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

      val loopLabel = freshLabel("sb_append_loop")
      val bodyLabel = freshLabel("sb_append_body")
      val endLabel = freshLabel("sb_append_end")
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)
      val iVal = freshTmp(Type.I64)
      fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
      val cond = freshTmp(Type.I1)
      fb.current.emitAssign(cond, Op.ICmp("slt", iVal, sLen))
      fb.current.setTerminator(Terminator.CondBr(cond, bodyLabel, endLabel))

      val bodyBlock = fb.newBlock(bodyLabel)
      fb.setCurrent(bodyBlock)
      val chPayload = stringCharPayloadI64(sPtr, iVal, fb)
      val dstIdx = freshTmp(Type.I64)
      fb.current.emitAssign(dstIdx, Op.Bin("add", Type.I64, sbLen, iVal))
      storeI64Slot(dataPtr, dstIdx, chPayload, fb)
      val iNext = freshTmp(Type.I64)
      fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(iNext, iPtr)
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      sbStoreLen(sbPtr, newLen, fb)
      Value.IntConst(0L, Type.I64)
    }

    private def emitStringBuilderAppendCodePoint(ctxPtr: Value, argsTuple: Value, fb: FunBuilder): Value = {
      val rcPtr = castValue(loadTupleElement(argsTuple, 0, SimpleType.Region, fb), Type.Ptr, fb)
      val sbPtr = loadTupleElement(argsTuple, 1, SimpleType.StringBuilderHandle, fb)
      val cpI64 = castValue(loadTupleElement(argsTuple, 2, SimpleType.Int32, fb), Type.I64, fb)

      val isNeg = freshTmp(Type.I1)
      fb.current.emitAssign(isNeg, Op.ICmp("slt", cpI64, Value.IntConst(0L, Type.I64)))
      val tooBig = freshTmp(Type.I1)
      fb.current.emitAssign(tooBig, Op.ICmp("sgt", cpI64, Value.IntConst(0x10ffffL, Type.I64)))
      val invalid = freshTmp(Type.I1)
      fb.current.emitAssign(invalid, Op.Bin("or", Type.I1, isNeg, tooBig))

      val okLabel = freshLabel("sbcp_ok")
      val badLabel = freshLabel("sbcp_bad")
      val contLabel = freshLabel("sbcp_cont")
      fb.current.setTerminator(Terminator.CondBr(invalid, badLabel, okLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)

      val sbLen = sbLenI64(sbPtr, fb)
      val isSupplementary = freshTmp(Type.I1)
      fb.current.emitAssign(isSupplementary, Op.ICmp("sge", cpI64, Value.IntConst(0x10000L, Type.I64)))

      val oneLabel = freshLabel("sbcp_one")
      val twoLabel = freshLabel("sbcp_two")
      fb.current.setTerminator(Terminator.CondBr(isSupplementary, twoLabel, oneLabel))

      val oneBlock = fb.newBlock(oneLabel)
      fb.setCurrent(oneBlock)
      val newLen1 = freshTmp(Type.I64)
      fb.current.emitAssign(newLen1, Op.Bin("add", Type.I64, sbLen, Value.IntConst(1L, Type.I64)))
      sbEnsureCapacity(ctxPtr, rcPtr, sbPtr, newLen1, fb)
      val dataPtr1 = sbDataPtr(sbPtr, fb)
      storeI64Slot(dataPtr1, sbLen, cpI64, fb)
      sbStoreLen(sbPtr, newLen1, fb)
      fb.current.setTerminator(Terminator.Br(contLabel))

      val twoBlock = fb.newBlock(twoLabel)
      fb.setCurrent(twoBlock)
      val newLen2 = freshTmp(Type.I64)
      fb.current.emitAssign(newLen2, Op.Bin("add", Type.I64, sbLen, Value.IntConst(2L, Type.I64)))
      sbEnsureCapacity(ctxPtr, rcPtr, sbPtr, newLen2, fb)
      val dataPtr2 = sbDataPtr(sbPtr, fb)

      val cpPrime = freshTmp(Type.I64)
      fb.current.emitAssign(cpPrime, Op.Bin("sub", Type.I64, cpI64, Value.IntConst(0x10000L, Type.I64)))
      val hi = freshTmp(Type.I64)
      fb.current.emitAssign(hi, Op.Bin("add", Type.I64,
        Value.IntConst(0xd800L, Type.I64),
        {
          val shifted = freshTmp(Type.I64)
          fb.current.emitAssign(shifted, Op.Bin("lshr", Type.I64, cpPrime, Value.IntConst(10L, Type.I64)))
          shifted
        }
      ))
      val lo = freshTmp(Type.I64)
      fb.current.emitAssign(lo, Op.Bin("add", Type.I64,
        Value.IntConst(0xdc00L, Type.I64),
        {
          val masked = freshTmp(Type.I64)
          fb.current.emitAssign(masked, Op.Bin("and", Type.I64, cpPrime, Value.IntConst(0x3ffL, Type.I64)))
          masked
        }
      ))

      storeI64Slot(dataPtr2, sbLen, hi, fb)
      val sbLenPlus1 = freshTmp(Type.I64)
      fb.current.emitAssign(sbLenPlus1, Op.Bin("add", Type.I64, sbLen, Value.IntConst(1L, Type.I64)))
      storeI64Slot(dataPtr2, sbLenPlus1, lo, fb)
      sbStoreLen(sbPtr, newLen2, fb)
      fb.current.setTerminator(Terminator.Br(contLabel))

      val contBlock = fb.newBlock(contLabel)
      fb.setCurrent(contBlock)
      Value.IntConst(0L, Type.I64)
    }

    private def emitStringBuilderCharAt(argsTuple: Value, fb: FunBuilder): Value = {
      val sbPtr = loadTupleElement(argsTuple, 1, SimpleType.StringBuilderHandle, fb)
      val idxI64 = castValue(loadTupleElement(argsTuple, 2, SimpleType.Int32, fb), Type.I64, fb)
      val lenI64 = sbLenI64(sbPtr, fb)

      val neg = freshTmp(Type.I1)
      fb.current.emitAssign(neg, Op.ICmp("slt", idxI64, Value.IntConst(0L, Type.I64)))
      val ge = freshTmp(Type.I1)
      fb.current.emitAssign(ge, Op.ICmp("sge", idxI64, lenI64))
      val oob = freshTmp(Type.I1)
      fb.current.emitAssign(oob, Op.Bin("or", Type.I1, neg, ge))

      val okLabel = freshLabel("sbcharat_ok")
      val badLabel = freshLabel("sbcharat_bad")
      val endLabel = freshLabel("sbcharat_end")
      fb.current.setTerminator(Terminator.CondBr(oob, badLabel, okLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)
      val dataPtr = sbDataPtr(sbPtr, fb)
      val payload = loadI64Slot(dataPtr, idxI64, fb)
      val ch = unboxFromI64(payload, SimpleType.Char, fb)
      fb.current.setTerminator(Terminator.Br(endLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      ch
    }

    private def emitStringBuilderLength(argsTuple: Value, fb: FunBuilder): Value = {
      val sbPtr = loadTupleElement(argsTuple, 1, SimpleType.StringBuilderHandle, fb)
      val lenI64 = sbLenI64(sbPtr, fb)
      val lenI32 = freshTmp(Type.I32)
      fb.current.emitAssign(lenI32, Op.Cast("trunc", Type.I32, lenI64))
      lenI32
    }

    private def emitStringBuilderSetLength(ctxPtr: Value, argsTuple: Value, fb: FunBuilder): Value = {
      val rcPtr = castValue(loadTupleElement(argsTuple, 0, SimpleType.Region, fb), Type.Ptr, fb)
      val sbPtr = loadTupleElement(argsTuple, 1, SimpleType.StringBuilderHandle, fb)
      val newLenI64 = castValue(loadTupleElement(argsTuple, 2, SimpleType.Int32, fb), Type.I64, fb)

      val isNeg = freshTmp(Type.I1)
      fb.current.emitAssign(isNeg, Op.ICmp("slt", newLenI64, Value.IntConst(0L, Type.I64)))

      val okLabel = freshLabel("sblen_ok")
      val badLabel = freshLabel("sblen_bad")
      val contLabel = freshLabel("sblen_cont")
      fb.current.setTerminator(Terminator.CondBr(isNeg, badLabel, okLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)

      val oldLen = sbLenI64(sbPtr, fb)
      val needsGrow = freshTmp(Type.I1)
      fb.current.emitAssign(needsGrow, Op.ICmp("sgt", newLenI64, oldLen))

      val growLabel = freshLabel("sblen_grow")
      val shrinkLabel = freshLabel("sblen_shrink")
      fb.current.setTerminator(Terminator.CondBr(needsGrow, growLabel, shrinkLabel))

      val shrinkBlock = fb.newBlock(shrinkLabel)
      fb.setCurrent(shrinkBlock)
      sbStoreLen(sbPtr, newLenI64, fb)
      fb.current.setTerminator(Terminator.Br(contLabel))

      val growBlock = fb.newBlock(growLabel)
      fb.setCurrent(growBlock)
      sbEnsureCapacity(ctxPtr, rcPtr, sbPtr, newLenI64, fb)
      val dataPtr = sbDataPtr(sbPtr, fb)

      val iPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(oldLen, iPtr)

      val loopLabel = freshLabel("sblen_fill_loop")
      val bodyLabel = freshLabel("sblen_fill_body")
      val endLabel = freshLabel("sblen_fill_end")
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)
      val iVal = freshTmp(Type.I64)
      fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
      val cond = freshTmp(Type.I1)
      fb.current.emitAssign(cond, Op.ICmp("slt", iVal, newLenI64))
      fb.current.setTerminator(Terminator.CondBr(cond, bodyLabel, endLabel))

      val bodyBlock = fb.newBlock(bodyLabel)
      fb.setCurrent(bodyBlock)
      storeI64Slot(dataPtr, iVal, Value.IntConst(0L, Type.I64), fb)
      val iNext = freshTmp(Type.I64)
      fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(iNext, iPtr)
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val endFillBlock = fb.newBlock(endLabel)
      fb.setCurrent(endFillBlock)
      sbStoreLen(sbPtr, newLenI64, fb)
      fb.current.setTerminator(Terminator.Br(contLabel))

      val contBlock = fb.newBlock(contLabel)
      fb.setCurrent(contBlock)
      Value.IntConst(0L, Type.I64)
    }

    private def emitStringBuilderToString(ctxPtr: Value, argsTuple: Value, fb: FunBuilder): Value = {
      val sbPtr = loadTupleElement(argsTuple, 1, SimpleType.StringBuilderHandle, fb)
      val lenI64 = sbLenI64(sbPtr, fb)
      val dataPtr = sbDataPtr(sbPtr, fb)

      val strPtr = allocString(lenI64, ctxPtr, fb)

      val iPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

      val loopLabel = freshLabel("sbtos_loop")
      val bodyLabel = freshLabel("sbtos_body")
      val endLabel = freshLabel("sbtos_end")
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)
      val iVal = freshTmp(Type.I64)
      fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
      val cond = freshTmp(Type.I1)
      fb.current.emitAssign(cond, Op.ICmp("slt", iVal, lenI64))
      fb.current.setTerminator(Terminator.CondBr(cond, bodyLabel, endLabel))

      val bodyBlock = fb.newBlock(bodyLabel)
      fb.setCurrent(bodyBlock)
      val payload = loadI64Slot(dataPtr, iVal, fb)
      storeStringCharPayload(strPtr, iVal, payload, fb)
      val iNext = freshTmp(Type.I64)
      fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(iNext, iPtr)
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      strPtr
    }

    private def emitTrimBounds(strPtr0: Value, fb: FunBuilder): (Value, Value) = {
      val strPtr = castValue(strPtr0, Type.Ptr, fb)
      val lenI64 = stringLenI64(strPtr, fb)

      val iPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

      val lLoopLabel = freshLabel("triml_loop")
      val lCheckLabel = freshLabel("triml_check")
      val lIncLabel = freshLabel("triml_inc")
      val lDoneLabel = freshLabel("triml_done")
      fb.current.setTerminator(Terminator.Br(lLoopLabel))

      val lLoopBlock = fb.newBlock(lLoopLabel)
      fb.setCurrent(lLoopBlock)
      val iVal = freshTmp(Type.I64)
      fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
      val inRange = freshTmp(Type.I1)
      fb.current.emitAssign(inRange, Op.ICmp("slt", iVal, lenI64))
      fb.current.setTerminator(Terminator.CondBr(inRange, lCheckLabel, lDoneLabel))

      val lCheckBlock = fb.newBlock(lCheckLabel)
      fb.setCurrent(lCheckBlock)
      val ch = stringCharPayloadI64(strPtr, iVal, fb)
      val isWs = freshTmp(Type.I1)
      fb.current.emitAssign(isWs, Op.ICmp("sle", ch, Value.IntConst(32L, Type.I64)))
      fb.current.setTerminator(Terminator.CondBr(isWs, lIncLabel, lDoneLabel))

      val lIncBlock = fb.newBlock(lIncLabel)
      fb.setCurrent(lIncBlock)
      val iNext = freshTmp(Type.I64)
      fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(iNext, iPtr)
      fb.current.setTerminator(Terminator.Br(lLoopLabel))

      val lDoneBlock = fb.newBlock(lDoneLabel)
      fb.setCurrent(lDoneBlock)
      val start = freshTmp(Type.I64)
      fb.current.emitAssign(start, Op.Load(Type.I64, iPtr))

      val jPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(jPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(lenI64, jPtr)

      val rLoopLabel = freshLabel("trimr_loop")
      val rCheckLabel = freshLabel("trimr_check")
      val rDecLabel = freshLabel("trimr_dec")
      val rDoneLabel = freshLabel("trimr_done")
      fb.current.setTerminator(Terminator.Br(rLoopLabel))

      val rLoopBlock = fb.newBlock(rLoopLabel)
      fb.setCurrent(rLoopBlock)
      val jVal = freshTmp(Type.I64)
      fb.current.emitAssign(jVal, Op.Load(Type.I64, jPtr))
      val gtStart = freshTmp(Type.I1)
      fb.current.emitAssign(gtStart, Op.ICmp("sgt", jVal, start))
      fb.current.setTerminator(Terminator.CondBr(gtStart, rCheckLabel, rDoneLabel))

      val rCheckBlock = fb.newBlock(rCheckLabel)
      fb.setCurrent(rCheckBlock)
      val jMinus1 = freshTmp(Type.I64)
      fb.current.emitAssign(jMinus1, Op.Bin("sub", Type.I64, jVal, Value.IntConst(1L, Type.I64)))
      val ch2 = stringCharPayloadI64(strPtr, jMinus1, fb)
      val isWs2 = freshTmp(Type.I1)
      fb.current.emitAssign(isWs2, Op.ICmp("sle", ch2, Value.IntConst(32L, Type.I64)))
      fb.current.setTerminator(Terminator.CondBr(isWs2, rDecLabel, rDoneLabel))

      val rDecBlock = fb.newBlock(rDecLabel)
      fb.setCurrent(rDecBlock)
      val jNext = freshTmp(Type.I64)
      fb.current.emitAssign(jNext, Op.Bin("sub", Type.I64, jVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(jNext, jPtr)
      fb.current.setTerminator(Terminator.Br(rLoopLabel))

      val rDoneBlock = fb.newBlock(rDoneLabel)
      fb.setCurrent(rDoneBlock)
      val end = freshTmp(Type.I64)
      fb.current.emitAssign(end, Op.Load(Type.I64, jPtr))
      (start, end)
    }

    private def emitParseIntTuple(strPtr0: Value, radixI64: Value, minVal: Long, maxVal: Long, valueTpe: SimpleType, ctxPtr: Value, fb: FunBuilder): Value = {
      val strPtr = castValue(strPtr0, Type.Ptr, fb)
      val (start, end) = emitTrimBounds(strPtr, fb)

      val okPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(okPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), okPtr)

      val resPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(resPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), resPtr)

      val badLow = freshTmp(Type.I1)
      fb.current.emitAssign(badLow, Op.ICmp("slt", radixI64, Value.IntConst(2L, Type.I64)))
      val badHigh = freshTmp(Type.I1)
      fb.current.emitAssign(badHigh, Op.ICmp("sgt", radixI64, Value.IntConst(36L, Type.I64)))
      val radixBad = freshTmp(Type.I1)
      fb.current.emitAssign(radixBad, Op.Bin("or", Type.I1, badLow, badHigh))

      val empty = freshTmp(Type.I1)
      fb.current.emitAssign(empty, Op.ICmp("sge", start, end))

      val fail0 = freshTmp(Type.I1)
      fb.current.emitAssign(fail0, Op.Bin("or", Type.I1, radixBad, empty))

      val failLabel = freshLabel("parsei_fail")
      val signLabel = freshLabel("parsei_sign")
      val endLabel = freshLabel("parsei_end")
      fb.current.setTerminator(Terminator.CondBr(fail0, failLabel, signLabel))

      val failBlock = fb.newBlock(failLabel)
      fb.setCurrent(failBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))

      val signBlock = fb.newBlock(signLabel)
      fb.setCurrent(signBlock)

      val idxPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(idxPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(start, idxPtr)

      val negPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(negPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), negPtr)

      val idxVal0 = freshTmp(Type.I64)
      fb.current.emitAssign(idxVal0, Op.Load(Type.I64, idxPtr))
      val ch0 = stringCharPayloadI64(strPtr, idxVal0, fb)
      val isMinus = freshTmp(Type.I1)
      fb.current.emitAssign(isMinus, Op.ICmp("eq", ch0, Value.IntConst(45L, Type.I64)))
      val isPlus = freshTmp(Type.I1)
      fb.current.emitAssign(isPlus, Op.ICmp("eq", ch0, Value.IntConst(43L, Type.I64)))
      val isSign = freshTmp(Type.I1)
      fb.current.emitAssign(isSign, Op.Bin("or", Type.I1, isMinus, isPlus))

      val consumeLabel = freshLabel("parsei_consume")
      val afterSignLabel = freshLabel("parsei_aftersign")
      fb.current.setTerminator(Terminator.CondBr(isSign, consumeLabel, afterSignLabel))

      val consumeBlock = fb.newBlock(consumeLabel)
      fb.setCurrent(consumeBlock)
      val minusLabel = freshLabel("parsei_minus")
      val plusLabel = freshLabel("parsei_plus")
      val setIdxLabel = freshLabel("parsei_setidx")
      fb.current.setTerminator(Terminator.CondBr(isMinus, minusLabel, plusLabel))

      val minusBlock = fb.newBlock(minusLabel)
      fb.setCurrent(minusBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), negPtr)
      fb.current.setTerminator(Terminator.Br(setIdxLabel))

      val plusBlock = fb.newBlock(plusLabel)
      fb.setCurrent(plusBlock)
      fb.current.setTerminator(Terminator.Br(setIdxLabel))

      val setIdxBlock = fb.newBlock(setIdxLabel)
      fb.setCurrent(setIdxBlock)
      val idxNext = freshTmp(Type.I64)
      fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idxVal0, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(idxNext, idxPtr)
      fb.current.setTerminator(Terminator.Br(afterSignLabel))

      val afterSignBlock = fb.newBlock(afterSignLabel)
      fb.setCurrent(afterSignBlock)
      val idxVal1 = freshTmp(Type.I64)
      fb.current.emitAssign(idxVal1, Op.Load(Type.I64, idxPtr))
      val emptyAfterSign = freshTmp(Type.I1)
      fb.current.emitAssign(emptyAfterSign, Op.ICmp("sge", idxVal1, end))

      val setupLabel = freshLabel("parsei_setup")
      fb.current.setTerminator(Terminator.CondBr(emptyAfterSign, failLabel, setupLabel))

      val setupBlock = fb.newBlock(setupLabel)
      fb.setCurrent(setupBlock)

      val negFlag = freshTmp(Type.I1)
      fb.current.emitAssign(negFlag, Op.Load(Type.I1, negPtr))

      val limitPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(limitPtr, Op.Alloca(Type.I64))

      val limitNegLabel = freshLabel("parsei_limit_neg")
      val limitPosLabel = freshLabel("parsei_limit_pos")
      val limitDoneLabel = freshLabel("parsei_limit_done")
      fb.current.setTerminator(Terminator.CondBr(negFlag, limitNegLabel, limitPosLabel))

      val limitNegBlock = fb.newBlock(limitNegLabel)
      fb.setCurrent(limitNegBlock)
      fb.current.emitStore(Value.IntConst(minVal, Type.I64), limitPtr)
      fb.current.setTerminator(Terminator.Br(limitDoneLabel))

      val limitPosBlock = fb.newBlock(limitPosLabel)
      fb.setCurrent(limitPosBlock)
      fb.current.emitStore(Value.IntConst(-maxVal, Type.I64), limitPtr)
      fb.current.setTerminator(Terminator.Br(limitDoneLabel))

      val limitDoneBlock = fb.newBlock(limitDoneLabel)
      fb.setCurrent(limitDoneBlock)
      val limitVal = freshTmp(Type.I64)
      fb.current.emitAssign(limitVal, Op.Load(Type.I64, limitPtr))
      val multmin = freshTmp(Type.I64)
      fb.current.emitAssign(multmin, Op.Bin("sdiv", Type.I64, limitVal, radixI64))

      fb.current.emitStore(Value.IntConst(0L, Type.I64), resPtr)

      val hasDigitPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(hasDigitPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), hasDigitPtr)

      val digitPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(digitPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(-1L, Type.I64), digitPtr)

      val loopLabel = freshLabel("parsei_loop")
      val bodyLabel = freshLabel("parsei_body")
      val afterLoopLabel = freshLabel("parsei_after")
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val loopBlock = fb.newBlock(loopLabel)
      fb.setCurrent(loopBlock)
      val idxVal = freshTmp(Type.I64)
      fb.current.emitAssign(idxVal, Op.Load(Type.I64, idxPtr))
      val inRange = freshTmp(Type.I1)
      fb.current.emitAssign(inRange, Op.ICmp("slt", idxVal, end))
      fb.current.setTerminator(Terminator.CondBr(inRange, bodyLabel, afterLoopLabel))

      val bodyBlock = fb.newBlock(bodyLabel)
      fb.setCurrent(bodyBlock)
      val ch = stringCharPayloadI64(strPtr, idxVal, fb)

      val isNumLo = freshTmp(Type.I1)
      fb.current.emitAssign(isNumLo, Op.ICmp("sge", ch, Value.IntConst(48L, Type.I64)))
      val isNumHi = freshTmp(Type.I1)
      fb.current.emitAssign(isNumHi, Op.ICmp("sle", ch, Value.IntConst(57L, Type.I64)))
      val isNum = freshTmp(Type.I1)
      fb.current.emitAssign(isNum, Op.Bin("and", Type.I1, isNumLo, isNumHi))

      val isLowerLo = freshTmp(Type.I1)
      fb.current.emitAssign(isLowerLo, Op.ICmp("sge", ch, Value.IntConst(97L, Type.I64)))
      val isLowerHi = freshTmp(Type.I1)
      fb.current.emitAssign(isLowerHi, Op.ICmp("sle", ch, Value.IntConst(122L, Type.I64)))
      val isLower = freshTmp(Type.I1)
      fb.current.emitAssign(isLower, Op.Bin("and", Type.I1, isLowerLo, isLowerHi))

      val isUpperLo = freshTmp(Type.I1)
      fb.current.emitAssign(isUpperLo, Op.ICmp("sge", ch, Value.IntConst(65L, Type.I64)))
      val isUpperHi = freshTmp(Type.I1)
      fb.current.emitAssign(isUpperHi, Op.ICmp("sle", ch, Value.IntConst(90L, Type.I64)))
      val isUpper = freshTmp(Type.I1)
      fb.current.emitAssign(isUpper, Op.Bin("and", Type.I1, isUpperLo, isUpperHi))

      val digitNumLabel = freshLabel("parsei_digit_num")
      val digitCheckLowerLabel = freshLabel("parsei_digit_check_lower")
      val digitLowerLabel = freshLabel("parsei_digit_lower")
      val digitCheckUpperLabel = freshLabel("parsei_digit_check_upper")
      val digitUpperLabel = freshLabel("parsei_digit_upper")
      val digitBadLabel = freshLabel("parsei_digit_bad")
      val digitDoneLabel = freshLabel("parsei_digit_done")

      fb.current.setTerminator(Terminator.CondBr(isNum, digitNumLabel, digitCheckLowerLabel))

      val digitNumBlock = fb.newBlock(digitNumLabel)
      fb.setCurrent(digitNumBlock)
      val digitNum = freshTmp(Type.I64)
      fb.current.emitAssign(digitNum, Op.Bin("sub", Type.I64, ch, Value.IntConst(48L, Type.I64)))
      fb.current.emitStore(digitNum, digitPtr)
      fb.current.setTerminator(Terminator.Br(digitDoneLabel))

      val digitCheckLowerBlock = fb.newBlock(digitCheckLowerLabel)
      fb.setCurrent(digitCheckLowerBlock)
      fb.current.setTerminator(Terminator.CondBr(isLower, digitLowerLabel, digitCheckUpperLabel))

      val digitLowerBlock = fb.newBlock(digitLowerLabel)
      fb.setCurrent(digitLowerBlock)
      val digitLower0 = freshTmp(Type.I64)
      fb.current.emitAssign(digitLower0, Op.Bin("sub", Type.I64, ch, Value.IntConst(97L, Type.I64)))
      val digitLower = freshTmp(Type.I64)
      fb.current.emitAssign(digitLower, Op.Bin("add", Type.I64, digitLower0, Value.IntConst(10L, Type.I64)))
      fb.current.emitStore(digitLower, digitPtr)
      fb.current.setTerminator(Terminator.Br(digitDoneLabel))

      val digitCheckUpperBlock = fb.newBlock(digitCheckUpperLabel)
      fb.setCurrent(digitCheckUpperBlock)
      fb.current.setTerminator(Terminator.CondBr(isUpper, digitUpperLabel, digitBadLabel))

      val digitUpperBlock = fb.newBlock(digitUpperLabel)
      fb.setCurrent(digitUpperBlock)
      val digitUpper0 = freshTmp(Type.I64)
      fb.current.emitAssign(digitUpper0, Op.Bin("sub", Type.I64, ch, Value.IntConst(65L, Type.I64)))
      val digitUpper = freshTmp(Type.I64)
      fb.current.emitAssign(digitUpper, Op.Bin("add", Type.I64, digitUpper0, Value.IntConst(10L, Type.I64)))
      fb.current.emitStore(digitUpper, digitPtr)
      fb.current.setTerminator(Terminator.Br(digitDoneLabel))

      val digitBadBlock = fb.newBlock(digitBadLabel)
      fb.setCurrent(digitBadBlock)
      fb.current.emitStore(Value.IntConst(-1L, Type.I64), digitPtr)
      fb.current.setTerminator(Terminator.Br(digitDoneLabel))

      val digitDoneBlock = fb.newBlock(digitDoneLabel)
      fb.setCurrent(digitDoneBlock)
      val digitVal = freshTmp(Type.I64)
      fb.current.emitAssign(digitVal, Op.Load(Type.I64, digitPtr))

      val nonNeg = freshTmp(Type.I1)
      fb.current.emitAssign(nonNeg, Op.ICmp("sge", digitVal, Value.IntConst(0L, Type.I64)))
      val ltRadix = freshTmp(Type.I1)
      fb.current.emitAssign(ltRadix, Op.ICmp("slt", digitVal, radixI64))
      val digitOk = freshTmp(Type.I1)
      fb.current.emitAssign(digitOk, Op.Bin("and", Type.I1, nonNeg, ltRadix))

      val overflow1Label = freshLabel("parsei_ovf1")
      val updateLabel = freshLabel("parsei_update")
      fb.current.setTerminator(Terminator.CondBr(digitOk, overflow1Label, failLabel))

      val overflow1Block = fb.newBlock(overflow1Label)
      fb.setCurrent(overflow1Block)
      val resVal = freshTmp(Type.I64)
      fb.current.emitAssign(resVal, Op.Load(Type.I64, resPtr))
      val ovf1 = freshTmp(Type.I1)
      fb.current.emitAssign(ovf1, Op.ICmp("slt", resVal, multmin))

      val overflow2Label = freshLabel("parsei_ovf2")
      fb.current.setTerminator(Terminator.CondBr(ovf1, failLabel, overflow2Label))

      val overflow2Block = fb.newBlock(overflow2Label)
      fb.setCurrent(overflow2Block)
      val resMul = freshTmp(Type.I64)
      fb.current.emitAssign(resMul, Op.Bin("mul", Type.I64, resVal, radixI64))
      val limitPlusDigit = freshTmp(Type.I64)
      fb.current.emitAssign(limitPlusDigit, Op.Bin("add", Type.I64, limitVal, digitVal))
      val ovf2 = freshTmp(Type.I1)
      fb.current.emitAssign(ovf2, Op.ICmp("slt", resMul, limitPlusDigit))
      fb.current.setTerminator(Terminator.CondBr(ovf2, failLabel, updateLabel))

      val updateBlock = fb.newBlock(updateLabel)
      fb.setCurrent(updateBlock)
      val resNext = freshTmp(Type.I64)
      fb.current.emitAssign(resNext, Op.Bin("sub", Type.I64, resMul, digitVal))
      fb.current.emitStore(resNext, resPtr)
      val idxNext2 = freshTmp(Type.I64)
      fb.current.emitAssign(idxNext2, Op.Bin("add", Type.I64, idxVal, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(idxNext2, idxPtr)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), hasDigitPtr)
      fb.current.setTerminator(Terminator.Br(loopLabel))

      val afterLoopBlock = fb.newBlock(afterLoopLabel)
      fb.setCurrent(afterLoopBlock)
      val hasDigit = freshTmp(Type.I1)
      fb.current.emitAssign(hasDigit, Op.Load(Type.I1, hasDigitPtr))

      val finalizeLabel = freshLabel("parsei_finalize")
      fb.current.setTerminator(Terminator.CondBr(hasDigit, finalizeLabel, failLabel))

      val finalizeBlock = fb.newBlock(finalizeLabel)
      fb.setCurrent(finalizeBlock)
      val resNeg = freshTmp(Type.I64)
      fb.current.emitAssign(resNeg, Op.Load(Type.I64, resPtr))
      val negFlag2 = freshTmp(Type.I1)
      fb.current.emitAssign(negFlag2, Op.Load(Type.I1, negPtr))

      val keepLabel = freshLabel("parsei_keep")
      val flipLabel = freshLabel("parsei_flip")
      val doneLabel = freshLabel("parsei_done")
      fb.current.setTerminator(Terminator.CondBr(negFlag2, keepLabel, flipLabel))

      val keepBlock = fb.newBlock(keepLabel)
      fb.setCurrent(keepBlock)
      fb.current.emitStore(resNeg, resPtr)
      fb.current.setTerminator(Terminator.Br(doneLabel))

      val flipBlock = fb.newBlock(flipLabel)
      fb.setCurrent(flipBlock)
      val resPos = freshTmp(Type.I64)
      fb.current.emitAssign(resPos, Op.Bin("sub", Type.I64, Value.IntConst(0L, Type.I64), resNeg))
      fb.current.emitStore(resPos, resPtr)
      fb.current.setTerminator(Terminator.Br(doneLabel))

      val doneBlock = fb.newBlock(doneLabel)
      fb.setCurrent(doneBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), okPtr)
      fb.current.setTerminator(Terminator.Br(endLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val ok = freshTmp(Type.I1)
      fb.current.emitAssign(ok, Op.Load(Type.I1, okPtr))
      val okPayload = freshTmp(Type.I64)
      fb.current.emitAssign(okPayload, Op.Cast("zext", Type.I64, ok))
      val resPayload = freshTmp(Type.I64)
      fb.current.emitAssign(resPayload, Op.Load(Type.I64, resPtr))
      val tupleTpe = SimpleType.mkTuple(List(SimpleType.Bool, valueTpe))
      allocTuple2(tupleTpe, okPayload, resPayload, ctxPtr, fb)
    }

    private def emitParseFloatTuple(strPtr0: Value, is32: Boolean, ctxPtr: Value, fb: FunBuilder): Value = {
      val strPtr = castValue(strPtr0, Type.Ptr, fb)
      val (start, end) = emitTrimBounds(strPtr, fb)

      val ten = Value.Float64Const(java.lang.Double.doubleToRawLongBits(10.0))
      val zeroD = Value.Float64Const(java.lang.Double.doubleToRawLongBits(0.0))
      val nanD = Value.Float64Const(java.lang.Double.doubleToRawLongBits(java.lang.Double.NaN))
      val posInfD = Value.Float64Const(java.lang.Double.doubleToRawLongBits(java.lang.Double.POSITIVE_INFINITY))
      val negInfD = Value.Float64Const(java.lang.Double.doubleToRawLongBits(java.lang.Double.NEGATIVE_INFINITY))

      val okPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(okPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), okPtr)

      val valuePtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(valuePtr, Op.Alloca(Type.Double))
      fb.current.emitStore(zeroD, valuePtr)

      val idxPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(idxPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(start, idxPtr)

      val negPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(negPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), negPtr)

      val hasDigitPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(hasDigitPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), hasDigitPtr)

      val fracDigitsPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(fracDigitsPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), fracDigitsPtr)

      val empty = freshTmp(Type.I1)
      fb.current.emitAssign(empty, Op.ICmp("sge", start, end))

      val failLabel = freshLabel("parsef_fail")
      val specialLabel = freshLabel("parsef_special")
      val signLabel = freshLabel("parsef_sign")
      val endLabel = freshLabel("parsef_end")
      fb.current.setTerminator(Terminator.CondBr(empty, failLabel, specialLabel))

      val failBlock = fb.newBlock(failLabel)
      fb.setCurrent(failBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))

      // Special values: NaN / Infinity.
      val specialBlock = fb.newBlock(specialLabel)
      fb.setCurrent(specialBlock)
      val len = freshTmp(Type.I64)
      fb.current.emitAssign(len, Op.Bin("sub", Type.I64, end, start))

      val len3Check = freshTmp(Type.I1)
      fb.current.emitAssign(len3Check, Op.ICmp("eq", len, Value.IntConst(3L, Type.I64)))
      val len3Label = freshLabel("parsef_len3")
      val len4CheckLabel = freshLabel("parsef_len4_check")
      fb.current.setTerminator(Terminator.CondBr(len3Check, len3Label, len4CheckLabel))

      // len == 3: "NaN"
      val len3Block = fb.newBlock(len3Label)
      fb.setCurrent(len3Block)
      val n0 = stringCharPayloadI64(strPtr, start, fb)
      val n1Idx = freshTmp(Type.I64)
      fb.current.emitAssign(n1Idx, Op.Bin("add", Type.I64, start, Value.IntConst(1L, Type.I64)))
      val n1 = stringCharPayloadI64(strPtr, n1Idx, fb)
      val n2Idx = freshTmp(Type.I64)
      fb.current.emitAssign(n2Idx, Op.Bin("add", Type.I64, start, Value.IntConst(2L, Type.I64)))
      val n2 = stringCharPayloadI64(strPtr, n2Idx, fb)

      val n0Ok = freshTmp(Type.I1)
      fb.current.emitAssign(n0Ok, Op.ICmp("eq", n0, Value.IntConst(78L, Type.I64))) // 'N'
      val n1Ok = freshTmp(Type.I1)
      fb.current.emitAssign(n1Ok, Op.ICmp("eq", n1, Value.IntConst(97L, Type.I64))) // 'a'
      val n2Ok = freshTmp(Type.I1)
      fb.current.emitAssign(n2Ok, Op.ICmp("eq", n2, Value.IntConst(78L, Type.I64))) // 'N'
      val nanOk3a = freshTmp(Type.I1)
      fb.current.emitAssign(nanOk3a, Op.Bin("and", Type.I1, n0Ok, n1Ok))
      val nanOk3 = freshTmp(Type.I1)
      fb.current.emitAssign(nanOk3, Op.Bin("and", Type.I1, nanOk3a, n2Ok))

      val nanHitLabel = freshLabel("parsef_nan_hit")
      fb.current.setTerminator(Terminator.CondBr(nanOk3, nanHitLabel, signLabel))

      // len == 4: [+|-] "NaN"
      val len4CheckBlock = fb.newBlock(len4CheckLabel)
      fb.setCurrent(len4CheckBlock)
      val len4Check = freshTmp(Type.I1)
      fb.current.emitAssign(len4Check, Op.ICmp("eq", len, Value.IntConst(4L, Type.I64)))
      val len4Label = freshLabel("parsef_len4")
      val len8CheckLabel = freshLabel("parsef_len8_check")
      fb.current.setTerminator(Terminator.CondBr(len4Check, len4Label, len8CheckLabel))

      val len4Block = fb.newBlock(len4Label)
      fb.setCurrent(len4Block)
      val s0 = stringCharPayloadI64(strPtr, start, fb)
      val s0IsMinus = freshTmp(Type.I1)
      fb.current.emitAssign(s0IsMinus, Op.ICmp("eq", s0, Value.IntConst(45L, Type.I64))) // '-'
      val s0IsPlus = freshTmp(Type.I1)
      fb.current.emitAssign(s0IsPlus, Op.ICmp("eq", s0, Value.IntConst(43L, Type.I64))) // '+'
      val s0IsSign = freshTmp(Type.I1)
      fb.current.emitAssign(s0IsSign, Op.Bin("or", Type.I1, s0IsMinus, s0IsPlus))

      val n1sIdx = freshTmp(Type.I64)
      fb.current.emitAssign(n1sIdx, Op.Bin("add", Type.I64, start, Value.IntConst(1L, Type.I64)))
      val n1s = stringCharPayloadI64(strPtr, n1sIdx, fb)
      val n2sIdx = freshTmp(Type.I64)
      fb.current.emitAssign(n2sIdx, Op.Bin("add", Type.I64, start, Value.IntConst(2L, Type.I64)))
      val n2s = stringCharPayloadI64(strPtr, n2sIdx, fb)
      val n3sIdx = freshTmp(Type.I64)
      fb.current.emitAssign(n3sIdx, Op.Bin("add", Type.I64, start, Value.IntConst(3L, Type.I64)))
      val n3s = stringCharPayloadI64(strPtr, n3sIdx, fb)

      val n1sOk = freshTmp(Type.I1)
      fb.current.emitAssign(n1sOk, Op.ICmp("eq", n1s, Value.IntConst(78L, Type.I64))) // 'N'
      val n2sOk = freshTmp(Type.I1)
      fb.current.emitAssign(n2sOk, Op.ICmp("eq", n2s, Value.IntConst(97L, Type.I64))) // 'a'
      val n3sOk = freshTmp(Type.I1)
      fb.current.emitAssign(n3sOk, Op.ICmp("eq", n3s, Value.IntConst(78L, Type.I64))) // 'N'
      val nanOk4a = freshTmp(Type.I1)
      fb.current.emitAssign(nanOk4a, Op.Bin("and", Type.I1, n1sOk, n2sOk))
      val nanOk4b = freshTmp(Type.I1)
      fb.current.emitAssign(nanOk4b, Op.Bin("and", Type.I1, nanOk4a, n3sOk))
      val nanOk4 = freshTmp(Type.I1)
      fb.current.emitAssign(nanOk4, Op.Bin("and", Type.I1, s0IsSign, nanOk4b))
      fb.current.setTerminator(Terminator.CondBr(nanOk4, nanHitLabel, signLabel))

      val nanHitBlock = fb.newBlock(nanHitLabel)
      fb.setCurrent(nanHitBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), okPtr)
      fb.current.emitStore(nanD, valuePtr)
      fb.current.setTerminator(Terminator.Br(endLabel))

      // len == 8: "Infinity"
      val len8CheckBlock = fb.newBlock(len8CheckLabel)
      fb.setCurrent(len8CheckBlock)
      val len8Check = freshTmp(Type.I1)
      fb.current.emitAssign(len8Check, Op.ICmp("eq", len, Value.IntConst(8L, Type.I64)))
      val len8Label = freshLabel("parsef_len8")
      val len9CheckLabel = freshLabel("parsef_len9_check")
      fb.current.setTerminator(Terminator.CondBr(len8Check, len8Label, len9CheckLabel))

      val posInfHitLabel = freshLabel("parsef_posinf_hit")
      val negInfHitLabel = freshLabel("parsef_neginf_hit")

      val len8Block = fb.newBlock(len8Label)
      fb.setCurrent(len8Block)
      val i0 = stringCharPayloadI64(strPtr, start, fb)
      val i1Idx = freshTmp(Type.I64)
      fb.current.emitAssign(i1Idx, Op.Bin("add", Type.I64, start, Value.IntConst(1L, Type.I64)))
      val i1 = stringCharPayloadI64(strPtr, i1Idx, fb)
      val i2Idx = freshTmp(Type.I64)
      fb.current.emitAssign(i2Idx, Op.Bin("add", Type.I64, start, Value.IntConst(2L, Type.I64)))
      val i2 = stringCharPayloadI64(strPtr, i2Idx, fb)
      val i3Idx = freshTmp(Type.I64)
      fb.current.emitAssign(i3Idx, Op.Bin("add", Type.I64, start, Value.IntConst(3L, Type.I64)))
      val i3 = stringCharPayloadI64(strPtr, i3Idx, fb)
      val i4Idx = freshTmp(Type.I64)
      fb.current.emitAssign(i4Idx, Op.Bin("add", Type.I64, start, Value.IntConst(4L, Type.I64)))
      val i4 = stringCharPayloadI64(strPtr, i4Idx, fb)
      val i5Idx = freshTmp(Type.I64)
      fb.current.emitAssign(i5Idx, Op.Bin("add", Type.I64, start, Value.IntConst(5L, Type.I64)))
      val i5 = stringCharPayloadI64(strPtr, i5Idx, fb)
      val i6Idx = freshTmp(Type.I64)
      fb.current.emitAssign(i6Idx, Op.Bin("add", Type.I64, start, Value.IntConst(6L, Type.I64)))
      val i6 = stringCharPayloadI64(strPtr, i6Idx, fb)
      val i7Idx = freshTmp(Type.I64)
      fb.current.emitAssign(i7Idx, Op.Bin("add", Type.I64, start, Value.IntConst(7L, Type.I64)))
      val i7 = stringCharPayloadI64(strPtr, i7Idx, fb)

      val inf0 = freshTmp(Type.I1)
      fb.current.emitAssign(inf0, Op.ICmp("eq", i0, Value.IntConst(73L, Type.I64))) // 'I'
      val inf1 = freshTmp(Type.I1)
      fb.current.emitAssign(inf1, Op.ICmp("eq", i1, Value.IntConst(110L, Type.I64))) // 'n'
      val inf2 = freshTmp(Type.I1)
      fb.current.emitAssign(inf2, Op.ICmp("eq", i2, Value.IntConst(102L, Type.I64))) // 'f'
      val inf3 = freshTmp(Type.I1)
      fb.current.emitAssign(inf3, Op.ICmp("eq", i3, Value.IntConst(105L, Type.I64))) // 'i'
      val inf4 = freshTmp(Type.I1)
      fb.current.emitAssign(inf4, Op.ICmp("eq", i4, Value.IntConst(110L, Type.I64))) // 'n'
      val inf5 = freshTmp(Type.I1)
      fb.current.emitAssign(inf5, Op.ICmp("eq", i5, Value.IntConst(105L, Type.I64))) // 'i'
      val inf6 = freshTmp(Type.I1)
      fb.current.emitAssign(inf6, Op.ICmp("eq", i6, Value.IntConst(116L, Type.I64))) // 't'
      val inf7 = freshTmp(Type.I1)
      fb.current.emitAssign(inf7, Op.ICmp("eq", i7, Value.IntConst(121L, Type.I64))) // 'y'
      val infA = freshTmp(Type.I1)
      fb.current.emitAssign(infA, Op.Bin("and", Type.I1, inf0, inf1))
      val infB = freshTmp(Type.I1)
      fb.current.emitAssign(infB, Op.Bin("and", Type.I1, infA, inf2))
      val infC = freshTmp(Type.I1)
      fb.current.emitAssign(infC, Op.Bin("and", Type.I1, infB, inf3))
      val infD = freshTmp(Type.I1)
      fb.current.emitAssign(infD, Op.Bin("and", Type.I1, infC, inf4))
      val infE = freshTmp(Type.I1)
      fb.current.emitAssign(infE, Op.Bin("and", Type.I1, infD, inf5))
      val infF = freshTmp(Type.I1)
      fb.current.emitAssign(infF, Op.Bin("and", Type.I1, infE, inf6))
      val infOk8 = freshTmp(Type.I1)
      fb.current.emitAssign(infOk8, Op.Bin("and", Type.I1, infF, inf7))
      fb.current.setTerminator(Terminator.CondBr(infOk8, posInfHitLabel, signLabel))

      // len == 9: [+|-] "Infinity"
      val len9CheckBlock = fb.newBlock(len9CheckLabel)
      fb.setCurrent(len9CheckBlock)
      val len9Check = freshTmp(Type.I1)
      fb.current.emitAssign(len9Check, Op.ICmp("eq", len, Value.IntConst(9L, Type.I64)))
      val len9Label = freshLabel("parsef_len9")
      fb.current.setTerminator(Terminator.CondBr(len9Check, len9Label, signLabel))

      val len9Block = fb.newBlock(len9Label)
      fb.setCurrent(len9Block)
      val si0 = stringCharPayloadI64(strPtr, start, fb)
      val si0IsMinus = freshTmp(Type.I1)
      fb.current.emitAssign(si0IsMinus, Op.ICmp("eq", si0, Value.IntConst(45L, Type.I64))) // '-'
      val si0IsPlus = freshTmp(Type.I1)
      fb.current.emitAssign(si0IsPlus, Op.ICmp("eq", si0, Value.IntConst(43L, Type.I64))) // '+'
      val si0IsSign = freshTmp(Type.I1)
      fb.current.emitAssign(si0IsSign, Op.Bin("or", Type.I1, si0IsMinus, si0IsPlus))

      val signedInfCheckLabel = freshLabel("parsef_signed_inf_check")
      fb.current.setTerminator(Terminator.CondBr(si0IsSign, signedInfCheckLabel, signLabel))

      val signedInfCheckBlock = fb.newBlock(signedInfCheckLabel)
      fb.setCurrent(signedInfCheckBlock)
      val sStart = freshTmp(Type.I64)
      fb.current.emitAssign(sStart, Op.Bin("add", Type.I64, start, Value.IntConst(1L, Type.I64)))
      val si1 = stringCharPayloadI64(strPtr, sStart, fb)
      val si2Idx = freshTmp(Type.I64)
      fb.current.emitAssign(si2Idx, Op.Bin("add", Type.I64, start, Value.IntConst(2L, Type.I64)))
      val si2 = stringCharPayloadI64(strPtr, si2Idx, fb)
      val si3Idx = freshTmp(Type.I64)
      fb.current.emitAssign(si3Idx, Op.Bin("add", Type.I64, start, Value.IntConst(3L, Type.I64)))
      val si3 = stringCharPayloadI64(strPtr, si3Idx, fb)
      val si4Idx = freshTmp(Type.I64)
      fb.current.emitAssign(si4Idx, Op.Bin("add", Type.I64, start, Value.IntConst(4L, Type.I64)))
      val si4 = stringCharPayloadI64(strPtr, si4Idx, fb)
      val si5Idx = freshTmp(Type.I64)
      fb.current.emitAssign(si5Idx, Op.Bin("add", Type.I64, start, Value.IntConst(5L, Type.I64)))
      val si5 = stringCharPayloadI64(strPtr, si5Idx, fb)
      val si6Idx = freshTmp(Type.I64)
      fb.current.emitAssign(si6Idx, Op.Bin("add", Type.I64, start, Value.IntConst(6L, Type.I64)))
      val si6 = stringCharPayloadI64(strPtr, si6Idx, fb)
      val si7Idx = freshTmp(Type.I64)
      fb.current.emitAssign(si7Idx, Op.Bin("add", Type.I64, start, Value.IntConst(7L, Type.I64)))
      val si7 = stringCharPayloadI64(strPtr, si7Idx, fb)
      val si8Idx = freshTmp(Type.I64)
      fb.current.emitAssign(si8Idx, Op.Bin("add", Type.I64, start, Value.IntConst(8L, Type.I64)))
      val si8 = stringCharPayloadI64(strPtr, si8Idx, fb)

      val sInf0 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf0, Op.ICmp("eq", si1, Value.IntConst(73L, Type.I64))) // 'I'
      val sInf1 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf1, Op.ICmp("eq", si2, Value.IntConst(110L, Type.I64))) // 'n'
      val sInf2 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf2, Op.ICmp("eq", si3, Value.IntConst(102L, Type.I64))) // 'f'
      val sInf3 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf3, Op.ICmp("eq", si4, Value.IntConst(105L, Type.I64))) // 'i'
      val sInf4 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf4, Op.ICmp("eq", si5, Value.IntConst(110L, Type.I64))) // 'n'
      val sInf5 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf5, Op.ICmp("eq", si6, Value.IntConst(105L, Type.I64))) // 'i'
      val sInf6 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf6, Op.ICmp("eq", si7, Value.IntConst(116L, Type.I64))) // 't'
      val sInf7 = freshTmp(Type.I1)
      fb.current.emitAssign(sInf7, Op.ICmp("eq", si8, Value.IntConst(121L, Type.I64))) // 'y'
      val sInfA = freshTmp(Type.I1)
      fb.current.emitAssign(sInfA, Op.Bin("and", Type.I1, sInf0, sInf1))
      val sInfB = freshTmp(Type.I1)
      fb.current.emitAssign(sInfB, Op.Bin("and", Type.I1, sInfA, sInf2))
      val sInfC = freshTmp(Type.I1)
      fb.current.emitAssign(sInfC, Op.Bin("and", Type.I1, sInfB, sInf3))
      val sInfD = freshTmp(Type.I1)
      fb.current.emitAssign(sInfD, Op.Bin("and", Type.I1, sInfC, sInf4))
      val sInfE = freshTmp(Type.I1)
      fb.current.emitAssign(sInfE, Op.Bin("and", Type.I1, sInfD, sInf5))
      val sInfF = freshTmp(Type.I1)
      fb.current.emitAssign(sInfF, Op.Bin("and", Type.I1, sInfE, sInf6))
      val sInfOk = freshTmp(Type.I1)
      fb.current.emitAssign(sInfOk, Op.Bin("and", Type.I1, sInfF, sInf7))

      val signedInfOkLabel = freshLabel("parsef_signed_inf_ok")
      fb.current.setTerminator(Terminator.CondBr(sInfOk, signedInfOkLabel, signLabel))

      val signedInfOkBlock = fb.newBlock(signedInfOkLabel)
      fb.setCurrent(signedInfOkBlock)
      fb.current.setTerminator(Terminator.CondBr(si0IsMinus, negInfHitLabel, posInfHitLabel))

      val posInfHitBlock = fb.newBlock(posInfHitLabel)
      fb.setCurrent(posInfHitBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), okPtr)
      fb.current.emitStore(posInfD, valuePtr)
      fb.current.setTerminator(Terminator.Br(endLabel))

      val negInfHitBlock = fb.newBlock(negInfHitLabel)
      fb.setCurrent(negInfHitBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), okPtr)
      fb.current.emitStore(negInfD, valuePtr)
      fb.current.setTerminator(Terminator.Br(endLabel))

      val signBlock = fb.newBlock(signLabel)
      fb.setCurrent(signBlock)

      val idxVal0 = freshTmp(Type.I64)
      fb.current.emitAssign(idxVal0, Op.Load(Type.I64, idxPtr))
      val ch0 = stringCharPayloadI64(strPtr, idxVal0, fb)
      val isMinus = freshTmp(Type.I1)
      fb.current.emitAssign(isMinus, Op.ICmp("eq", ch0, Value.IntConst(45L, Type.I64)))
      val isPlus = freshTmp(Type.I1)
      fb.current.emitAssign(isPlus, Op.ICmp("eq", ch0, Value.IntConst(43L, Type.I64)))
      val isSign = freshTmp(Type.I1)
      fb.current.emitAssign(isSign, Op.Bin("or", Type.I1, isMinus, isPlus))

      val consumeLabel = freshLabel("parsef_consume")
      val afterSignLabel = freshLabel("parsef_aftersign")
      fb.current.setTerminator(Terminator.CondBr(isSign, consumeLabel, afterSignLabel))

      val consumeBlock = fb.newBlock(consumeLabel)
      fb.setCurrent(consumeBlock)
      val minusLabel = freshLabel("parsef_minus")
      val plusLabel = freshLabel("parsef_plus")
      val setIdxLabel = freshLabel("parsef_setidx")
      fb.current.setTerminator(Terminator.CondBr(isMinus, minusLabel, plusLabel))

      val minusBlock = fb.newBlock(minusLabel)
      fb.setCurrent(minusBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), negPtr)
      fb.current.setTerminator(Terminator.Br(setIdxLabel))

      val plusBlock = fb.newBlock(plusLabel)
      fb.setCurrent(plusBlock)
      fb.current.setTerminator(Terminator.Br(setIdxLabel))

      val setIdxBlock = fb.newBlock(setIdxLabel)
      fb.setCurrent(setIdxBlock)
      val idxNext = freshTmp(Type.I64)
      fb.current.emitAssign(idxNext, Op.Bin("add", Type.I64, idxVal0, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(idxNext, idxPtr)
      fb.current.setTerminator(Terminator.Br(afterSignLabel))

      val afterSignBlock = fb.newBlock(afterSignLabel)
      fb.setCurrent(afterSignBlock)
      val idxVal1 = freshTmp(Type.I64)
      fb.current.emitAssign(idxVal1, Op.Load(Type.I64, idxPtr))
      val emptyAfterSign = freshTmp(Type.I1)
      fb.current.emitAssign(emptyAfterSign, Op.ICmp("sge", idxVal1, end))

      val intLoopLabel = freshLabel("parsef_int_loop")
      fb.current.setTerminator(Terminator.CondBr(emptyAfterSign, failLabel, intLoopLabel))

      // Integer digits.
      val intLoopBlock = fb.newBlock(intLoopLabel)
      fb.setCurrent(intLoopBlock)
      val iIdx = freshTmp(Type.I64)
      fb.current.emitAssign(iIdx, Op.Load(Type.I64, idxPtr))
      val inRangeInt = freshTmp(Type.I1)
      fb.current.emitAssign(inRangeInt, Op.ICmp("slt", iIdx, end))
      val intCheckLabel = freshLabel("parsef_int_check")
      val afterIntLabel = freshLabel("parsef_after_int")
      fb.current.setTerminator(Terminator.CondBr(inRangeInt, intCheckLabel, afterIntLabel))

      val intCheckBlock = fb.newBlock(intCheckLabel)
      fb.setCurrent(intCheckBlock)
      val ch = stringCharPayloadI64(strPtr, iIdx, fb)
      val isNumLo = freshTmp(Type.I1)
      fb.current.emitAssign(isNumLo, Op.ICmp("sge", ch, Value.IntConst(48L, Type.I64)))
      val isNumHi = freshTmp(Type.I1)
      fb.current.emitAssign(isNumHi, Op.ICmp("sle", ch, Value.IntConst(57L, Type.I64)))
      val isDigit = freshTmp(Type.I1)
      fb.current.emitAssign(isDigit, Op.Bin("and", Type.I1, isNumLo, isNumHi))
      val intBodyLabel = freshLabel("parsef_int_body")
      fb.current.setTerminator(Terminator.CondBr(isDigit, intBodyLabel, afterIntLabel))

      val intBodyBlock = fb.newBlock(intBodyLabel)
      fb.setCurrent(intBodyBlock)
      val digitI64 = freshTmp(Type.I64)
      fb.current.emitAssign(digitI64, Op.Bin("sub", Type.I64, ch, Value.IntConst(48L, Type.I64)))
      val digitD = freshTmp(Type.Double)
      fb.current.emitAssign(digitD, Op.Cast("sitofp", Type.Double, digitI64))
      val v0 = freshTmp(Type.Double)
      fb.current.emitAssign(v0, Op.Load(Type.Double, valuePtr))
      val vMul = freshTmp(Type.Double)
      fb.current.emitAssign(vMul, Op.Bin("fmul", Type.Double, v0, ten))
      val vNext = freshTmp(Type.Double)
      fb.current.emitAssign(vNext, Op.Bin("fadd", Type.Double, vMul, digitD))
      fb.current.emitStore(vNext, valuePtr)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), hasDigitPtr)
      val iNext = freshTmp(Type.I64)
      fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iIdx, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(iNext, idxPtr)
      fb.current.setTerminator(Terminator.Br(intLoopLabel))

      // Optional fractional part.
      val afterIntBlock = fb.newBlock(afterIntLabel)
      fb.setCurrent(afterIntBlock)
      val dotIdx = freshTmp(Type.I64)
      fb.current.emitAssign(dotIdx, Op.Load(Type.I64, idxPtr))
      val inRangeDot = freshTmp(Type.I1)
      fb.current.emitAssign(inRangeDot, Op.ICmp("slt", dotIdx, end))
      val dotCheckLabel = freshLabel("parsef_dot_check")
      val afterFracLabel = freshLabel("parsef_after_frac")
      fb.current.setTerminator(Terminator.CondBr(inRangeDot, dotCheckLabel, afterFracLabel))

      val dotCheckBlock = fb.newBlock(dotCheckLabel)
      fb.setCurrent(dotCheckBlock)
      val dotCh = stringCharPayloadI64(strPtr, dotIdx, fb)
      val isDot = freshTmp(Type.I1)
      fb.current.emitAssign(isDot, Op.ICmp("eq", dotCh, Value.IntConst(46L, Type.I64)))
      val dotConsumeLabel = freshLabel("parsef_dot_consume")
      fb.current.setTerminator(Terminator.CondBr(isDot, dotConsumeLabel, afterFracLabel))

      val dotConsumeBlock = fb.newBlock(dotConsumeLabel)
      fb.setCurrent(dotConsumeBlock)
      val dotNext = freshTmp(Type.I64)
      fb.current.emitAssign(dotNext, Op.Bin("add", Type.I64, dotIdx, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(dotNext, idxPtr)

      val fracLoopLabel = freshLabel("parsef_frac_loop")
      fb.current.setTerminator(Terminator.Br(fracLoopLabel))

      val fracLoopBlock = fb.newBlock(fracLoopLabel)
      fb.setCurrent(fracLoopBlock)
      val fIdx = freshTmp(Type.I64)
      fb.current.emitAssign(fIdx, Op.Load(Type.I64, idxPtr))
      val inRangeFrac = freshTmp(Type.I1)
      fb.current.emitAssign(inRangeFrac, Op.ICmp("slt", fIdx, end))
      val fracCheckLabel = freshLabel("parsef_frac_check")
      fb.current.setTerminator(Terminator.CondBr(inRangeFrac, fracCheckLabel, afterFracLabel))

      val fracCheckBlock = fb.newBlock(fracCheckLabel)
      fb.setCurrent(fracCheckBlock)
      val fCh = stringCharPayloadI64(strPtr, fIdx, fb)
      val fNumLo = freshTmp(Type.I1)
      fb.current.emitAssign(fNumLo, Op.ICmp("sge", fCh, Value.IntConst(48L, Type.I64)))
      val fNumHi = freshTmp(Type.I1)
      fb.current.emitAssign(fNumHi, Op.ICmp("sle", fCh, Value.IntConst(57L, Type.I64)))
      val fIsDigit = freshTmp(Type.I1)
      fb.current.emitAssign(fIsDigit, Op.Bin("and", Type.I1, fNumLo, fNumHi))
      val fracBodyLabel = freshLabel("parsef_frac_body")
      fb.current.setTerminator(Terminator.CondBr(fIsDigit, fracBodyLabel, afterFracLabel))

      val fracBodyBlock = fb.newBlock(fracBodyLabel)
      fb.setCurrent(fracBodyBlock)
      val fDigitI64 = freshTmp(Type.I64)
      fb.current.emitAssign(fDigitI64, Op.Bin("sub", Type.I64, fCh, Value.IntConst(48L, Type.I64)))
      val fDigitD = freshTmp(Type.Double)
      fb.current.emitAssign(fDigitD, Op.Cast("sitofp", Type.Double, fDigitI64))
      val fv0 = freshTmp(Type.Double)
      fb.current.emitAssign(fv0, Op.Load(Type.Double, valuePtr))
      val fvMul = freshTmp(Type.Double)
      fb.current.emitAssign(fvMul, Op.Bin("fmul", Type.Double, fv0, ten))
      val fvNext = freshTmp(Type.Double)
      fb.current.emitAssign(fvNext, Op.Bin("fadd", Type.Double, fvMul, fDigitD))
      fb.current.emitStore(fvNext, valuePtr)
      val fd0 = freshTmp(Type.I64)
      fb.current.emitAssign(fd0, Op.Load(Type.I64, fracDigitsPtr))
      val fdNext = freshTmp(Type.I64)
      fb.current.emitAssign(fdNext, Op.Bin("add", Type.I64, fd0, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(fdNext, fracDigitsPtr)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), hasDigitPtr)
      val fIdxNext = freshTmp(Type.I64)
      fb.current.emitAssign(fIdxNext, Op.Bin("add", Type.I64, fIdx, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(fIdxNext, idxPtr)
      fb.current.setTerminator(Terminator.Br(fracLoopLabel))

      // After fraction: require at least one digit overall.
      val afterFracBlock = fb.newBlock(afterFracLabel)
      fb.setCurrent(afterFracBlock)
      val hasDigit = freshTmp(Type.I1)
      fb.current.emitAssign(hasDigit, Op.Load(Type.I1, hasDigitPtr))

      val scaleFracCheckLabel = freshLabel("parsef_scale_check")
      fb.current.setTerminator(Terminator.CondBr(hasDigit, scaleFracCheckLabel, failLabel))

      val scaleFracCheckBlock = fb.newBlock(scaleFracCheckLabel)
      fb.setCurrent(scaleFracCheckBlock)
      val fd = freshTmp(Type.I64)
      fb.current.emitAssign(fd, Op.Load(Type.I64, fracDigitsPtr))
      val hasFrac = freshTmp(Type.I1)
      fb.current.emitAssign(hasFrac, Op.ICmp("sgt", fd, Value.IntConst(0L, Type.I64)))

      val scaleFracLabel = freshLabel("parsef_scale")
      val expCheckLabel = freshLabel("parsef_exp_check")
      fb.current.setTerminator(Terminator.CondBr(hasFrac, scaleFracLabel, expCheckLabel))

      val scaleFracBlock = fb.newBlock(scaleFracLabel)
      fb.setCurrent(scaleFracBlock)
      val fdD = freshTmp(Type.Double)
      fb.current.emitAssign(fdD, Op.Cast("sitofp", Type.Double, fd))
      val pow10 = freshTmp(Type.Double)
      fb.current.emitAssign(pow10, Op.Call(Type.Double, "llvm.pow.f64", List(ten, fdD)))
      val vRaw = freshTmp(Type.Double)
      fb.current.emitAssign(vRaw, Op.Load(Type.Double, valuePtr))
      val vScaled = freshTmp(Type.Double)
      fb.current.emitAssign(vScaled, Op.Bin("fdiv", Type.Double, vRaw, pow10))
      fb.current.emitStore(vScaled, valuePtr)
      fb.current.setTerminator(Terminator.Br(expCheckLabel))

      // Optional exponent.
      val expCheckBlock = fb.newBlock(expCheckLabel)
      fb.setCurrent(expCheckBlock)
      val eIdx = freshTmp(Type.I64)
      fb.current.emitAssign(eIdx, Op.Load(Type.I64, idxPtr))
      val inRangeE = freshTmp(Type.I1)
      fb.current.emitAssign(inRangeE, Op.ICmp("slt", eIdx, end))
      val expCharLabel = freshLabel("parsef_exp_char")
      val finishLabel = freshLabel("parsef_finish")
      fb.current.setTerminator(Terminator.CondBr(inRangeE, expCharLabel, finishLabel))

      val expCharBlock = fb.newBlock(expCharLabel)
      fb.setCurrent(expCharBlock)
      val eCh = stringCharPayloadI64(strPtr, eIdx, fb)
      val isLowerE = freshTmp(Type.I1)
      fb.current.emitAssign(isLowerE, Op.ICmp("eq", eCh, Value.IntConst(101L, Type.I64))) // 'e'
      val isUpperE = freshTmp(Type.I1)
      fb.current.emitAssign(isUpperE, Op.ICmp("eq", eCh, Value.IntConst(69L, Type.I64))) // 'E'
      val isE = freshTmp(Type.I1)
      fb.current.emitAssign(isE, Op.Bin("or", Type.I1, isLowerE, isUpperE))

      val expConsumeLabel = freshLabel("parsef_exp_consume")
      fb.current.setTerminator(Terminator.CondBr(isE, expConsumeLabel, finishLabel))

      val expConsumeBlock = fb.newBlock(expConsumeLabel)
      fb.setCurrent(expConsumeBlock)
      val eNext = freshTmp(Type.I64)
      fb.current.emitAssign(eNext, Op.Bin("add", Type.I64, eIdx, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(eNext, idxPtr)

      val expNegPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(expNegPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), expNegPtr)

      val expValPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(expValPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(Value.IntConst(0L, Type.I64), expValPtr)

      val expHasPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(expHasPtr, Op.Alloca(Type.I1))
      fb.current.emitStore(Value.IntConst(0L, Type.I1), expHasPtr)

      val expIdx0 = freshTmp(Type.I64)
      fb.current.emitAssign(expIdx0, Op.Load(Type.I64, idxPtr))
      val expInRange0 = freshTmp(Type.I1)
      fb.current.emitAssign(expInRange0, Op.ICmp("slt", expIdx0, end))
      val expSignCheckLabel = freshLabel("parsef_exp_sign_check")
      fb.current.setTerminator(Terminator.CondBr(expInRange0, expSignCheckLabel, failLabel))

      val expSignCheckBlock = fb.newBlock(expSignCheckLabel)
      fb.setCurrent(expSignCheckBlock)
      val expCh0 = stringCharPayloadI64(strPtr, expIdx0, fb)
      val expIsMinus = freshTmp(Type.I1)
      fb.current.emitAssign(expIsMinus, Op.ICmp("eq", expCh0, Value.IntConst(45L, Type.I64)))
      val expIsPlus = freshTmp(Type.I1)
      fb.current.emitAssign(expIsPlus, Op.ICmp("eq", expCh0, Value.IntConst(43L, Type.I64)))
      val expIsSign = freshTmp(Type.I1)
      fb.current.emitAssign(expIsSign, Op.Bin("or", Type.I1, expIsMinus, expIsPlus))

      val expSignConsumeLabel = freshLabel("parsef_exp_sign_consume")
      val expDigitsLoopLabel = freshLabel("parsef_exp_digits_loop")
      fb.current.setTerminator(Terminator.CondBr(expIsSign, expSignConsumeLabel, expDigitsLoopLabel))

      val expSignConsumeBlock = fb.newBlock(expSignConsumeLabel)
      fb.setCurrent(expSignConsumeBlock)
      val expMinusLabel = freshLabel("parsef_exp_minus")
      val expPlusLabel = freshLabel("parsef_exp_plus")
      val expSetIdxLabel = freshLabel("parsef_exp_setidx")
      fb.current.setTerminator(Terminator.CondBr(expIsMinus, expMinusLabel, expPlusLabel))

      val expMinusBlock = fb.newBlock(expMinusLabel)
      fb.setCurrent(expMinusBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), expNegPtr)
      fb.current.setTerminator(Terminator.Br(expSetIdxLabel))

      val expPlusBlock = fb.newBlock(expPlusLabel)
      fb.setCurrent(expPlusBlock)
      fb.current.setTerminator(Terminator.Br(expSetIdxLabel))

      val expSetIdxBlock = fb.newBlock(expSetIdxLabel)
      fb.setCurrent(expSetIdxBlock)
      val expIdxNext0 = freshTmp(Type.I64)
      fb.current.emitAssign(expIdxNext0, Op.Bin("add", Type.I64, expIdx0, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(expIdxNext0, idxPtr)
      fb.current.setTerminator(Terminator.Br(expDigitsLoopLabel))

      // Exponent digits loop.
      val expDigitsLoopBlock = fb.newBlock(expDigitsLoopLabel)
      fb.setCurrent(expDigitsLoopBlock)
      val expIdx = freshTmp(Type.I64)
      fb.current.emitAssign(expIdx, Op.Load(Type.I64, idxPtr))
      val expInRange = freshTmp(Type.I1)
      fb.current.emitAssign(expInRange, Op.ICmp("slt", expIdx, end))
      val expDigitCheckLabel = freshLabel("parsef_exp_digit_check")
      val expAfterDigitsLabel = freshLabel("parsef_exp_after_digits")
      fb.current.setTerminator(Terminator.CondBr(expInRange, expDigitCheckLabel, expAfterDigitsLabel))

      val expDigitCheckBlock = fb.newBlock(expDigitCheckLabel)
      fb.setCurrent(expDigitCheckBlock)
      val expCh = stringCharPayloadI64(strPtr, expIdx, fb)
      val expNumLo = freshTmp(Type.I1)
      fb.current.emitAssign(expNumLo, Op.ICmp("sge", expCh, Value.IntConst(48L, Type.I64)))
      val expNumHi = freshTmp(Type.I1)
      fb.current.emitAssign(expNumHi, Op.ICmp("sle", expCh, Value.IntConst(57L, Type.I64)))
      val expIsDigit = freshTmp(Type.I1)
      fb.current.emitAssign(expIsDigit, Op.Bin("and", Type.I1, expNumLo, expNumHi))
      val expDigitBodyLabel = freshLabel("parsef_exp_digit_body")
      fb.current.setTerminator(Terminator.CondBr(expIsDigit, expDigitBodyLabel, expAfterDigitsLabel))

      val expDigitBodyBlock = fb.newBlock(expDigitBodyLabel)
      fb.setCurrent(expDigitBodyBlock)
      val expDigitI64 = freshTmp(Type.I64)
      fb.current.emitAssign(expDigitI64, Op.Bin("sub", Type.I64, expCh, Value.IntConst(48L, Type.I64)))
      val expVal0 = freshTmp(Type.I64)
      fb.current.emitAssign(expVal0, Op.Load(Type.I64, expValPtr))
      val expMul = freshTmp(Type.I64)
      fb.current.emitAssign(expMul, Op.Bin("mul", Type.I64, expVal0, Value.IntConst(10L, Type.I64)))
      val expValNext = freshTmp(Type.I64)
      fb.current.emitAssign(expValNext, Op.Bin("add", Type.I64, expMul, expDigitI64))
      fb.current.emitStore(expValNext, expValPtr)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), expHasPtr)
      val expIdxNext = freshTmp(Type.I64)
      fb.current.emitAssign(expIdxNext, Op.Bin("add", Type.I64, expIdx, Value.IntConst(1L, Type.I64)))
      fb.current.emitStore(expIdxNext, idxPtr)
      fb.current.setTerminator(Terminator.Br(expDigitsLoopLabel))

      val expAfterDigitsBlock = fb.newBlock(expAfterDigitsLabel)
      fb.setCurrent(expAfterDigitsBlock)
      val expHas = freshTmp(Type.I1)
      fb.current.emitAssign(expHas, Op.Load(Type.I1, expHasPtr))

      val expApplyLabel = freshLabel("parsef_exp_apply")
      fb.current.setTerminator(Terminator.CondBr(expHas, expApplyLabel, failLabel))

      val expApplyBlock = fb.newBlock(expApplyLabel)
      fb.setCurrent(expApplyBlock)
      val expValRaw = freshTmp(Type.I64)
      fb.current.emitAssign(expValRaw, Op.Load(Type.I64, expValPtr))
      val expNeg = freshTmp(Type.I1)
      fb.current.emitAssign(expNeg, Op.Load(Type.I1, expNegPtr))

      val expSignedPtr = freshTmp(Type.Ptr)
      fb.current.emitAssign(expSignedPtr, Op.Alloca(Type.I64))
      fb.current.emitStore(expValRaw, expSignedPtr)

      val expNegLabel2 = freshLabel("parsef_exp_neg")
      val expSignedDoneLabel = freshLabel("parsef_exp_signed_done")
      fb.current.setTerminator(Terminator.CondBr(expNeg, expNegLabel2, expSignedDoneLabel))

      val expNegBlock2 = fb.newBlock(expNegLabel2)
      fb.setCurrent(expNegBlock2)
      val expSigned = freshTmp(Type.I64)
      fb.current.emitAssign(expSigned, Op.Bin("sub", Type.I64, Value.IntConst(0L, Type.I64), expValRaw))
      fb.current.emitStore(expSigned, expSignedPtr)
      fb.current.setTerminator(Terminator.Br(expSignedDoneLabel))

      val expSignedDoneBlock = fb.newBlock(expSignedDoneLabel)
      fb.setCurrent(expSignedDoneBlock)
      val expFinal = freshTmp(Type.I64)
      fb.current.emitAssign(expFinal, Op.Load(Type.I64, expSignedPtr))
      val expD = freshTmp(Type.Double)
      fb.current.emitAssign(expD, Op.Cast("sitofp", Type.Double, expFinal))
      val powExp = freshTmp(Type.Double)
      fb.current.emitAssign(powExp, Op.Call(Type.Double, "llvm.pow.f64", List(ten, expD)))
      val vPreExp = freshTmp(Type.Double)
      fb.current.emitAssign(vPreExp, Op.Load(Type.Double, valuePtr))
      val vPostExp = freshTmp(Type.Double)
      fb.current.emitAssign(vPostExp, Op.Bin("fmul", Type.Double, vPreExp, powExp))
      fb.current.emitStore(vPostExp, valuePtr)
      fb.current.setTerminator(Terminator.Br(finishLabel))

      // Finish: must consume all characters.
      val finishBlock = fb.newBlock(finishLabel)
      fb.setCurrent(finishBlock)
      val finalIdx = freshTmp(Type.I64)
      fb.current.emitAssign(finalIdx, Op.Load(Type.I64, idxPtr))
      val atEnd = freshTmp(Type.I1)
      fb.current.emitAssign(atEnd, Op.ICmp("eq", finalIdx, end))

      val signApplyLabel = freshLabel("parsef_sign_apply")
      fb.current.setTerminator(Terminator.CondBr(atEnd, signApplyLabel, failLabel))

      val signApplyBlock = fb.newBlock(signApplyLabel)
      fb.setCurrent(signApplyBlock)
      val neg = freshTmp(Type.I1)
      fb.current.emitAssign(neg, Op.Load(Type.I1, negPtr))

      val negApplyLabel = freshLabel("parsef_neg_apply")
      val posApplyLabel = freshLabel("parsef_pos_apply")
      val okSetLabel = freshLabel("parsef_ok_set")
      fb.current.setTerminator(Terminator.CondBr(neg, negApplyLabel, posApplyLabel))

      val negApplyBlock = fb.newBlock(negApplyLabel)
      fb.setCurrent(negApplyBlock)
      val vRaw2 = freshTmp(Type.Double)
      fb.current.emitAssign(vRaw2, Op.Load(Type.Double, valuePtr))
      val vNeg = freshTmp(Type.Double)
      fb.current.emitAssign(vNeg, Op.Bin("fsub", Type.Double, zeroD, vRaw2))
      fb.current.emitStore(vNeg, valuePtr)
      fb.current.setTerminator(Terminator.Br(okSetLabel))

      val posApplyBlock = fb.newBlock(posApplyLabel)
      fb.setCurrent(posApplyBlock)
      fb.current.setTerminator(Terminator.Br(okSetLabel))

      val okSetBlock = fb.newBlock(okSetLabel)
      fb.setCurrent(okSetBlock)
      fb.current.emitStore(Value.IntConst(1L, Type.I1), okPtr)
      fb.current.setTerminator(Terminator.Br(endLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val ok = freshTmp(Type.I1)
      fb.current.emitAssign(ok, Op.Load(Type.I1, okPtr))
      val okPayload = freshTmp(Type.I64)
      fb.current.emitAssign(okPayload, Op.Cast("zext", Type.I64, ok))

      val vFinalD = freshTmp(Type.Double)
      fb.current.emitAssign(vFinalD, Op.Load(Type.Double, valuePtr))

      val valuePayload = if (is32) {
        val vF32 = freshTmp(Type.Float)
        fb.current.emitAssign(vF32, Op.Cast("fptrunc", Type.Float, vFinalD))
        boxToI64(vF32, SimpleType.Float32, fb)
      } else {
        boxToI64(vFinalD, SimpleType.Float64, fb)
      }

      val valueTpe = if (is32) SimpleType.Float32 else SimpleType.Float64
      val tupleTpe = SimpleType.mkTuple(List(SimpleType.Bool, valueTpe))
      allocTuple2(tupleTpe, okPayload, valuePayload, ctxPtr, fb)
    }

    private def emitBinary(op: BinaryOp, a: Value, b: Value, ctxPtr: Value, fb: FunBuilder): Value = op match {
      case SemanticOp.BoolOp.And =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Bin("and", Type.I1, a, b))
        tmp

      case SemanticOp.BoolOp.Or =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Bin("or", Type.I1, a, b))
        tmp

      case SemanticOp.BoolOp.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("eq", a, b))
        tmp

      case SemanticOp.BoolOp.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ne", a, b))
        tmp

      case SemanticOp.Int32Op.Add =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("add", Type.I32, a, b))
        tmp

      case SemanticOp.Int32Op.Sub =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I32, a, b))
        tmp

      case SemanticOp.Int32Op.Mul =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("mul", Type.I32, a, b))
        tmp

      case SemanticOp.Int32Op.Div =>
        emitInt32Div(a, b, fb)

      case SemanticOp.Int32Op.Rem =>
        emitInt32Rem(a, b, fb)

      case SemanticOp.Int32Op.Exp =>
        emitIntExp(a, b, resultBits = 32, fb)

      case SemanticOp.Int32Op.And =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("and", Type.I32, a, b))
        tmp

      case SemanticOp.Int32Op.Or =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("or", Type.I32, a, b))
        tmp

      case SemanticOp.Int32Op.Xor =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I32, a, b))
        tmp

      case SemanticOp.Int32Op.Shl =>
        emitMaskedShiftLeft(Type.I32, a, b, maskBits = 5, fb)

      case SemanticOp.Int32Op.Shr =>
        emitMaskedShiftRight(Type.I32, a, b, maskBits = 5, fb)

      case SemanticOp.Int32Op.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("eq", a, b))
        tmp

      case SemanticOp.Int32Op.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ne", a, b))
        tmp

      case SemanticOp.Int32Op.Lt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("slt", a, b))
        tmp

      case SemanticOp.Int32Op.Le =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sle", a, b))
        tmp

      case SemanticOp.Int32Op.Gt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sgt", a, b))
        tmp

      case SemanticOp.Int32Op.Ge =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sge", a, b))
        tmp

      case SemanticOp.Int64Op.Add =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("add", Type.I64, a, b))
        tmp

      case SemanticOp.Int64Op.Sub =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I64, a, b))
        tmp

      case SemanticOp.Int64Op.Mul =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("mul", Type.I64, a, b))
        tmp

      case SemanticOp.Int64Op.Div =>
        emitInt64Div(a, b, fb)

      case SemanticOp.Int64Op.Rem =>
        emitInt64Rem(a, b, fb)

      case SemanticOp.Int64Op.Exp =>
        emitIntExp(a, b, resultBits = 64, fb)

      case SemanticOp.Int64Op.And =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("and", Type.I64, a, b))
        tmp

      case SemanticOp.Int64Op.Or =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("or", Type.I64, a, b))
        tmp

      case SemanticOp.Int64Op.Xor =>
        val tmp = freshTmp(Type.I64)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I64, a, b))
        tmp

      case SemanticOp.Int64Op.Shl =>
        emitMaskedShiftLeft(Type.I64, a, b, maskBits = 6, fb)

      case SemanticOp.Int64Op.Shr =>
        emitMaskedShiftRight(Type.I64, a, b, maskBits = 6, fb)

      case SemanticOp.Int64Op.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("eq", a, b))
        tmp

      case SemanticOp.Int64Op.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ne", a, b))
        tmp

      case SemanticOp.Int64Op.Lt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("slt", a, b))
        tmp

      case SemanticOp.Int64Op.Le =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sle", a, b))
        tmp

      case SemanticOp.Int64Op.Gt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sgt", a, b))
        tmp

      case SemanticOp.Int64Op.Ge =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sge", a, b))
        tmp

      case SemanticOp.BigIntOp.Add =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_add", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Sub =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_sub", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Mul =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_mul", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Div =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_div", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Rem =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_rem", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Shl =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val shift = castValue(b, Type.I32, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_shl", List(ctxPtr, aPtr, shift)))
        tmp

      case SemanticOp.BigIntOp.Shr =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val shift = castValue(b, Type.I32, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_shr", List(ctxPtr, aPtr, shift)))
        tmp

      case SemanticOp.BigIntOp.And =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_and", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Or =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_or", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Xor =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigint_xor", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigIntOp.Cmp =>
        val tmp = freshTmp(Type.I32)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_bigint_cmp", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Add =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_add", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Sub =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_sub", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Mul =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_mul", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Div =>
        val tmp = freshTmp(Type.Ptr)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.Ptr, "flix_bigdec_div", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.BigDecimalOp.Cmp =>
        val tmp = freshTmp(Type.I32)
        val aPtr = castValue(a, Type.Ptr, fb)
        val bPtr = castValue(b, Type.Ptr, fb)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_bigdec_cmp", List(ctxPtr, aPtr, bPtr)))
        tmp

      case SemanticOp.Int8Op.Add =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("add", Type.I8, a, b))
        tmp

      case SemanticOp.Int8Op.Sub =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I8, a, b))
        tmp

      case SemanticOp.Int8Op.Mul =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("mul", Type.I8, a, b))
        tmp

      case SemanticOp.Int8Op.Div =>
        emitSmallIntDiv(Type.I8, a, b, fb)

      case SemanticOp.Int8Op.Rem =>
        emitSmallIntRem(Type.I8, a, b, fb)

      case SemanticOp.Int8Op.Exp =>
        emitSmallIntExp(Type.I8, a, b, fb)

      case SemanticOp.Int8Op.And =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("and", Type.I8, a, b))
        tmp

      case SemanticOp.Int8Op.Or =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("or", Type.I8, a, b))
        tmp

      case SemanticOp.Int8Op.Xor =>
        val tmp = freshTmp(Type.I8)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I8, a, b))
        tmp

      case SemanticOp.Int8Op.Shl =>
        emitSmallIntShiftLeft(Type.I8, a, b, fb)

      case SemanticOp.Int8Op.Shr =>
        emitSmallIntShiftRight(Type.I8, a, b, fb)

      case SemanticOp.Int8Op.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("eq", a, b))
        tmp

      case SemanticOp.Int8Op.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ne", a, b))
        tmp

      case SemanticOp.Int8Op.Lt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("slt", a, b))
        tmp

      case SemanticOp.Int8Op.Le =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sle", a, b))
        tmp

      case SemanticOp.Int8Op.Gt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sgt", a, b))
        tmp

      case SemanticOp.Int8Op.Ge =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sge", a, b))
        tmp

      case SemanticOp.Int16Op.Add =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("add", Type.I16, a, b))
        tmp

      case SemanticOp.Int16Op.Sub =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("sub", Type.I16, a, b))
        tmp

      case SemanticOp.Int16Op.Mul =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("mul", Type.I16, a, b))
        tmp

      case SemanticOp.Int16Op.Div =>
        emitSmallIntDiv(Type.I16, a, b, fb)

      case SemanticOp.Int16Op.Rem =>
        emitSmallIntRem(Type.I16, a, b, fb)

      case SemanticOp.Int16Op.Exp =>
        emitSmallIntExp(Type.I16, a, b, fb)

      case SemanticOp.Int16Op.And =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("and", Type.I16, a, b))
        tmp

      case SemanticOp.Int16Op.Or =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("or", Type.I16, a, b))
        tmp

      case SemanticOp.Int16Op.Xor =>
        val tmp = freshTmp(Type.I16)
        fb.current.emitAssign(tmp, Op.Bin("xor", Type.I16, a, b))
        tmp

      case SemanticOp.Int16Op.Shl =>
        emitSmallIntShiftLeft(Type.I16, a, b, fb)

      case SemanticOp.Int16Op.Shr =>
        emitSmallIntShiftRight(Type.I16, a, b, fb)

      case SemanticOp.Int16Op.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("eq", a, b))
        tmp

      case SemanticOp.Int16Op.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ne", a, b))
        tmp

      case SemanticOp.Int16Op.Lt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("slt", a, b))
        tmp

      case SemanticOp.Int16Op.Le =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sle", a, b))
        tmp

      case SemanticOp.Int16Op.Gt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sgt", a, b))
        tmp

      case SemanticOp.Int16Op.Ge =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("sge", a, b))
        tmp

      case SemanticOp.CharOp.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("eq", a, b))
        tmp

      case SemanticOp.CharOp.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ne", a, b))
        tmp

      case SemanticOp.CharOp.Lt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ult", a, b))
        tmp

      case SemanticOp.CharOp.Le =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ule", a, b))
        tmp

      case SemanticOp.CharOp.Gt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("ugt", a, b))
        tmp

      case SemanticOp.CharOp.Ge =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.ICmp("uge", a, b))
        tmp

      case SemanticOp.CharOp.IsSurrogatePair =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.Call(Type.I1, "flix_char_is_surrogate_pair", List(a, b)))
        tmp

      case SemanticOp.CharOp.ToCodePoint =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_char_to_code_point", List(a, b)))
        tmp

      case SemanticOp.CharOp.Digit =>
        val tmp = freshTmp(Type.I32)
        fb.current.emitAssign(tmp, Op.Call(Type.I32, "flix_char_digit", List(a, b)))
        tmp

      case SemanticOp.CharOp.ForDigit =>
        // Matches Java's Character.forDigit: ASCII only ('0'..'9','a'..'z') or '\\u0000' if not representable.
        val n = a
        val radix = b

        val radixLo = freshTmp(Type.I1)
        fb.current.emitAssign(radixLo, Op.ICmp("slt", radix, Value.IntConst(2L, Type.I32)))
        val radixHi = freshTmp(Type.I1)
        fb.current.emitAssign(radixHi, Op.ICmp("sgt", radix, Value.IntConst(36L, Type.I32)))
        val radixBad = freshTmp(Type.I1)
        fb.current.emitAssign(radixBad, Op.Bin("or", Type.I1, radixLo, radixHi))

        val badLabel = freshLabel("cfordigit_bad")
        val nCheckLabel = freshLabel("cfordigit_ncheck")
        val endLabel = freshLabel("cfordigit_end")
        fb.current.setTerminator(Terminator.CondBr(radixBad, badLabel, nCheckLabel))

        val incomings = mutable.ArrayBuffer.empty[(Value, String)]

        val badBlock = fb.newBlock(badLabel)
        fb.setCurrent(badBlock)
        val zeroChar = Value.IntConst(0L, Type.I32)
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((zeroChar, badLabel))

        val nCheckBlock = fb.newBlock(nCheckLabel)
        fb.setCurrent(nCheckBlock)
        val nNeg = freshTmp(Type.I1)
        fb.current.emitAssign(nNeg, Op.ICmp("slt", n, Value.IntConst(0L, Type.I32)))
        val nGe = freshTmp(Type.I1)
        fb.current.emitAssign(nGe, Op.ICmp("sge", n, radix))
        val nBad = freshTmp(Type.I1)
        fb.current.emitAssign(nBad, Op.Bin("or", Type.I1, nNeg, nGe))

        val computeLabel = freshLabel("cfordigit_compute")
        fb.current.setTerminator(Terminator.CondBr(nBad, badLabel, computeLabel))

        val computeBlock = fb.newBlock(computeLabel)
        fb.setCurrent(computeBlock)
        val lt10 = freshTmp(Type.I1)
        fb.current.emitAssign(lt10, Op.ICmp("slt", n, Value.IntConst(10L, Type.I32)))
        val digitLabel = freshLabel("cfordigit_digit")
        val alphaLabel = freshLabel("cfordigit_alpha")
        fb.current.setTerminator(Terminator.CondBr(lt10, digitLabel, alphaLabel))

        val digitBlock = fb.newBlock(digitLabel)
        fb.setCurrent(digitBlock)
        val chDigit = freshTmp(Type.I32)
        fb.current.emitAssign(chDigit, Op.Bin("add", Type.I32, n, Value.IntConst(48L, Type.I32))) // '0'
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((chDigit, digitLabel))

        val alphaBlock = fb.newBlock(alphaLabel)
        fb.setCurrent(alphaBlock)
        val n10 = freshTmp(Type.I32)
        fb.current.emitAssign(n10, Op.Bin("sub", Type.I32, n, Value.IntConst(10L, Type.I32)))
        val chAlpha = freshTmp(Type.I32)
        fb.current.emitAssign(chAlpha, Op.Bin("add", Type.I32, n10, Value.IntConst(97L, Type.I32))) // 'a'
        fb.current.setTerminator(Terminator.Br(endLabel))
        incomings.addOne((chAlpha, alphaLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        val phi = freshTmp(Type.I32)
        endBlock.emitPhi(phi, incomings.toList)
        phi

      case SemanticOp.StringOp.Concat =>
        val s1Ptr = castValue(a, Type.Ptr, fb)
        val s2Ptr = castValue(b, Type.Ptr, fb)

        val len1 = stringLenI64(s1Ptr, fb)
        val len2 = stringLenI64(s2Ptr, fb)

        val newLen = freshTmp(Type.I64)
        fb.current.emitAssign(newLen, Op.Bin("add", Type.I64, len1, len2))

        val outPtr = allocString(newLen, ctxPtr, fb)

        val iPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

        val loop1Label = freshLabel("sconcat_loop1")
        val body1Label = freshLabel("sconcat_body1")
        val end1Label = freshLabel("sconcat_end1")
        val loop2Label = freshLabel("sconcat_loop2")
        val body2Label = freshLabel("sconcat_body2")
        val end2Label = freshLabel("sconcat_end2")
        val contLabel = freshLabel("sconcat_cont")

        fb.current.setTerminator(Terminator.Br(loop1Label))

        val loop1Block = fb.newBlock(loop1Label)
        fb.setCurrent(loop1Block)
        val iVal = freshTmp(Type.I64)
        fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
        val cond1 = freshTmp(Type.I1)
        fb.current.emitAssign(cond1, Op.ICmp("slt", iVal, len1))
        fb.current.setTerminator(Terminator.CondBr(cond1, body1Label, end1Label))

        val body1Block = fb.newBlock(body1Label)
        fb.setCurrent(body1Block)
        val payload1 = stringCharPayloadI64(s1Ptr, iVal, fb)
        storeStringCharPayload(outPtr, iVal, payload1, fb)
        val iNext1 = freshTmp(Type.I64)
        fb.current.emitAssign(iNext1, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(iNext1, iPtr)
        fb.current.setTerminator(Terminator.Br(loop1Label))

        val end1Block = fb.newBlock(end1Label)
        fb.setCurrent(end1Block)
        fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)
        fb.current.setTerminator(Terminator.Br(loop2Label))

        val loop2Block = fb.newBlock(loop2Label)
        fb.setCurrent(loop2Block)
        val jVal = freshTmp(Type.I64)
        fb.current.emitAssign(jVal, Op.Load(Type.I64, iPtr))
        val cond2 = freshTmp(Type.I1)
        fb.current.emitAssign(cond2, Op.ICmp("slt", jVal, len2))
        fb.current.setTerminator(Terminator.CondBr(cond2, body2Label, end2Label))

        val body2Block = fb.newBlock(body2Label)
        fb.setCurrent(body2Block)
        val payload2 = stringCharPayloadI64(s2Ptr, jVal, fb)
        val dstIdx2 = freshTmp(Type.I64)
        fb.current.emitAssign(dstIdx2, Op.Bin("add", Type.I64, len1, jVal))
        storeStringCharPayload(outPtr, dstIdx2, payload2, fb)

        val jNext2 = freshTmp(Type.I64)
        fb.current.emitAssign(jNext2, Op.Bin("add", Type.I64, jVal, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(jNext2, iPtr)
        fb.current.setTerminator(Terminator.Br(loop2Label))

        val end2Block = fb.newBlock(end2Label)
        fb.setCurrent(end2Block)
        fb.current.setTerminator(Terminator.Br(contLabel))

        val contBlock = fb.newBlock(contLabel)
        fb.setCurrent(contBlock)
        outPtr

      case SemanticOp.StringOp.CharAt =>
        val strPtr = castValue(a, Type.Ptr, fb)
        val idxI64 = castValue(b, Type.I64, fb)
        val lenI64 = stringLenI64(strPtr, fb)

        val neg = freshTmp(Type.I1)
        fb.current.emitAssign(neg, Op.ICmp("slt", idxI64, Value.IntConst(0L, Type.I64)))
        val ge = freshTmp(Type.I1)
        fb.current.emitAssign(ge, Op.ICmp("sge", idxI64, lenI64))
        val oob = freshTmp(Type.I1)
        fb.current.emitAssign(oob, Op.Bin("or", Type.I1, neg, ge))

        val okLabel = freshLabel("scharat_ok")
        val badLabel = freshLabel("scharat_bad")
        val endLabel = freshLabel("scharat_end")

        fb.current.setTerminator(Terminator.CondBr(oob, badLabel, okLabel))

        val badBlock = fb.newBlock(badLabel)
        fb.setCurrent(badBlock)
        fb.current.emitTrap()
        fb.current.setTerminator(Terminator.Unreachable)

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)
        val payload = stringCharPayloadI64(strPtr, idxI64, fb)
        val ch = unboxFromI64(payload, SimpleType.Char, fb)
        if (!fb.current.isTerminated) fb.current.setTerminator(Terminator.Br(endLabel))

        val endBlock = fb.newBlock(endLabel)
        fb.setCurrent(endBlock)
        ch

      case SemanticOp.StringOp.Repeat =>
        val strPtr0 = castValue(a, Type.Ptr, fb)
        val nI64 = castValue(b, Type.I64, fb)
        val lenI64 = stringLenI64(strPtr0, fb)

        val isNeg = freshTmp(Type.I1)
        fb.current.emitAssign(isNeg, Op.ICmp("slt", nI64, Value.IntConst(0L, Type.I64)))

        val negLabel = freshLabel("srep_neg")
        val okLabel = freshLabel("srep_ok")
        val contLabel = freshLabel("srep_cont")

        fb.current.setTerminator(Terminator.CondBr(isNeg, negLabel, okLabel))

        val negBlock = fb.newBlock(negLabel)
        fb.setCurrent(negBlock)
        val emptyPtr = allocString(Value.IntConst(0L, Type.I64), ctxPtr, fb)
        fb.current.setTerminator(Terminator.Br(contLabel))

        val okBlock = fb.newBlock(okLabel)
        fb.setCurrent(okBlock)

        val newLen = freshTmp(Type.I64)
        fb.current.emitAssign(newLen, Op.Bin("mul", Type.I64, lenI64, nI64))
        val outPtr = allocString(newLen, ctxPtr, fb)

        val rPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(rPtr, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), rPtr)

        val iPtr = freshTmp(Type.Ptr)
        fb.current.emitAssign(iPtr, Op.Alloca(Type.I64))
        fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)

        val loopRLabel = freshLabel("srep_loop_r")
        val bodyRLabel = freshLabel("srep_body_r")
        val endRLabel = freshLabel("srep_end_r")
        val loopILabel = freshLabel("srep_loop_i")
        val bodyILabel = freshLabel("srep_body_i")
        val endILabel = freshLabel("srep_end_i")

        fb.current.setTerminator(Terminator.Br(loopRLabel))

        val loopRBlock = fb.newBlock(loopRLabel)
        fb.setCurrent(loopRBlock)
        val rVal = freshTmp(Type.I64)
        fb.current.emitAssign(rVal, Op.Load(Type.I64, rPtr))
        val condR = freshTmp(Type.I1)
        fb.current.emitAssign(condR, Op.ICmp("slt", rVal, nI64))
        fb.current.setTerminator(Terminator.CondBr(condR, bodyRLabel, endRLabel))

        val bodyRBlock = fb.newBlock(bodyRLabel)
        fb.setCurrent(bodyRBlock)
        fb.current.emitStore(Value.IntConst(0L, Type.I64), iPtr)
        fb.current.setTerminator(Terminator.Br(loopILabel))

        val loopIBlock = fb.newBlock(loopILabel)
        fb.setCurrent(loopIBlock)
        val iVal = freshTmp(Type.I64)
        fb.current.emitAssign(iVal, Op.Load(Type.I64, iPtr))
        val condI = freshTmp(Type.I1)
        fb.current.emitAssign(condI, Op.ICmp("slt", iVal, lenI64))
        fb.current.setTerminator(Terminator.CondBr(condI, bodyILabel, endILabel))

        val bodyIBlock = fb.newBlock(bodyILabel)
        fb.setCurrent(bodyIBlock)
        val payload = stringCharPayloadI64(strPtr0, iVal, fb)

        val base = freshTmp(Type.I64)
        fb.current.emitAssign(base, Op.Bin("mul", Type.I64, rVal, lenI64))
        val dstIdx0 = freshTmp(Type.I64)
        fb.current.emitAssign(dstIdx0, Op.Bin("add", Type.I64, base, iVal))
        storeStringCharPayload(outPtr, dstIdx0, payload, fb)

        val iNext = freshTmp(Type.I64)
        fb.current.emitAssign(iNext, Op.Bin("add", Type.I64, iVal, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(iNext, iPtr)
        fb.current.setTerminator(Terminator.Br(loopILabel))

        val endIBlock = fb.newBlock(endILabel)
        fb.setCurrent(endIBlock)
        val rNext = freshTmp(Type.I64)
        fb.current.emitAssign(rNext, Op.Bin("add", Type.I64, rVal, Value.IntConst(1L, Type.I64)))
        fb.current.emitStore(rNext, rPtr)
        fb.current.setTerminator(Terminator.Br(loopRLabel))

        val endRBlock = fb.newBlock(endRLabel)
        fb.setCurrent(endRBlock)
        fb.current.setTerminator(Terminator.Br(contLabel))

        val contBlock = fb.newBlock(contLabel)
        fb.setCurrent(contBlock)

        val phi = freshTmp(Type.Ptr)
        // Note: the ok path reaches contLabel from endRLabel, not okLabel.
        contBlock.emitPhi(phi, List((emptyPtr, negLabel), (outPtr, endRLabel)))
        phi

      case SemanticOp.Float32Op.Add =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Bin("fadd", Type.Float, a, b))
        tmp

      case SemanticOp.Float32Op.Sub =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Bin("fsub", Type.Float, a, b))
        tmp

      case SemanticOp.Float32Op.Mul =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Bin("fmul", Type.Float, a, b))
        tmp

      case SemanticOp.Float32Op.Div =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Bin("fdiv", Type.Float, a, b))
        tmp

      case SemanticOp.Float32Op.Exp =>
        val tmp = freshTmp(Type.Float)
        fb.current.emitAssign(tmp, Op.Call(Type.Float, "llvm.pow.f32", List(a, b)))
        tmp

      case SemanticOp.Float32Op.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("oeq", a, b))
        tmp

      case SemanticOp.Float32Op.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("une", a, b))
        tmp

      case SemanticOp.Float32Op.Lt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("olt", a, b))
        tmp

      case SemanticOp.Float32Op.Le =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("ole", a, b))
        tmp

      case SemanticOp.Float32Op.Gt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("ogt", a, b))
        tmp

      case SemanticOp.Float32Op.Ge =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("oge", a, b))
        tmp

      case SemanticOp.Float64Op.Add =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Bin("fadd", Type.Double, a, b))
        tmp

      case SemanticOp.Float64Op.Sub =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Bin("fsub", Type.Double, a, b))
        tmp

      case SemanticOp.Float64Op.Mul =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Bin("fmul", Type.Double, a, b))
        tmp

      case SemanticOp.Float64Op.Div =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Bin("fdiv", Type.Double, a, b))
        tmp

      case SemanticOp.Float64Op.Exp =>
        val tmp = freshTmp(Type.Double)
        fb.current.emitAssign(tmp, Op.Call(Type.Double, "llvm.pow.f64", List(a, b)))
        tmp

      case SemanticOp.Float64Op.Eq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("oeq", a, b))
        tmp

      case SemanticOp.Float64Op.Neq =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("une", a, b))
        tmp

      case SemanticOp.Float64Op.Lt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("olt", a, b))
        tmp

      case SemanticOp.Float64Op.Le =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("ole", a, b))
        tmp

      case SemanticOp.Float64Op.Gt =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("ogt", a, b))
        tmp

      case SemanticOp.Float64Op.Ge =>
        val tmp = freshTmp(Type.I1)
        fb.current.emitAssign(tmp, Op.FCmp("oge", a, b))
        tmp

      case _ =>
        fb.current.emitTrap()
        Value.Undef(Type.I64)
    }

    private def isIntType(tpe: Type): Boolean = tpe match {
      case Type.I1 | Type.I8 | Type.I16 | Type.I32 | Type.I64 => true
      case _ => false
    }

    private def castValue(v: Value, expectedTpe: Type, fb: FunBuilder): Value = {
      if (v.tpe == expectedTpe) return v

      (v.tpe, expectedTpe) match {
        case (Type.Ptr, Type.I64) =>
          val tmp = freshTmp(Type.I64)
          fb.current.emitAssign(tmp, Op.Cast("ptrtoint", Type.I64, v))
          tmp

        case (Type.I64, Type.Ptr) =>
          val tmp = freshTmp(Type.Ptr)
          fb.current.emitAssign(tmp, Op.Cast("inttoptr", Type.Ptr, v))
          tmp

        case (from, to) if isIntType(from) && isIntType(to) =>
          val tmp = freshTmp(to)
          val opcode = (from, to) match {
            case (Type.I1, Type.I64) => "zext"
            case (Type.I1, _) => "zext"
            case (_, Type.I1) => "trunc"
            case _ => if (intBits(from) < intBits(to)) "sext" else "trunc"
          }
          fb.current.emitAssign(tmp, Op.Cast(opcode, to, v))
          tmp

        case _ =>
          v
      }
    }

    private def emitMaskedShiftLeft(valueTpe: Type, a: Value, b: Value, maskBits: Int, fb: FunBuilder): Value = {
      val mask = (1L << maskBits) - 1L
      val masked = freshTmp(Type.I32)
      fb.current.emitAssign(masked, Op.Bin("and", Type.I32, b, Value.IntConst(mask, Type.I32)))

      val shiftAmount = valueTpe match {
        case Type.I32 => masked
        case Type.I64 =>
          val tmp = freshTmp(Type.I64)
          fb.current.emitAssign(tmp, Op.Cast("zext", Type.I64, masked))
          tmp
        case other =>
          fb.current.emitTrap()
          return Value.Undef(other)
      }

      val tmp = freshTmp(valueTpe)
      fb.current.emitAssign(tmp, Op.Bin("shl", valueTpe, a, shiftAmount))
      tmp
    }

    private def emitMaskedShiftRight(valueTpe: Type, a: Value, b: Value, maskBits: Int, fb: FunBuilder): Value = {
      val mask = (1L << maskBits) - 1L
      val masked = freshTmp(Type.I32)
      fb.current.emitAssign(masked, Op.Bin("and", Type.I32, b, Value.IntConst(mask, Type.I32)))

      val shiftAmount = valueTpe match {
        case Type.I32 => masked
        case Type.I64 =>
          val tmp = freshTmp(Type.I64)
          fb.current.emitAssign(tmp, Op.Cast("zext", Type.I64, masked))
          tmp
        case other =>
          fb.current.emitTrap()
          return Value.Undef(other)
      }

      val tmp = freshTmp(valueTpe)
      fb.current.emitAssign(tmp, Op.Bin("ashr", valueTpe, a, shiftAmount))
      tmp
    }

    private def emitSmallIntShiftLeft(valueTpe: Type, a: Value, b: Value, fb: FunBuilder): Value = {
      val a32 = freshTmp(Type.I32)
      fb.current.emitAssign(a32, Op.Cast("sext", Type.I32, a))

      val masked = freshTmp(Type.I32)
      fb.current.emitAssign(masked, Op.Bin("and", Type.I32, b, Value.IntConst(31L, Type.I32)))

      val shifted = freshTmp(Type.I32)
      fb.current.emitAssign(shifted, Op.Bin("shl", Type.I32, a32, masked))

      val tmp = freshTmp(valueTpe)
      fb.current.emitAssign(tmp, Op.Cast("trunc", valueTpe, shifted))
      tmp
    }

    private def emitSmallIntShiftRight(valueTpe: Type, a: Value, b: Value, fb: FunBuilder): Value = {
      val a32 = freshTmp(Type.I32)
      fb.current.emitAssign(a32, Op.Cast("sext", Type.I32, a))

      val masked = freshTmp(Type.I32)
      fb.current.emitAssign(masked, Op.Bin("and", Type.I32, b, Value.IntConst(31L, Type.I32)))

      val shifted = freshTmp(Type.I32)
      fb.current.emitAssign(shifted, Op.Bin("ashr", Type.I32, a32, masked))

      val tmp = freshTmp(valueTpe)
      fb.current.emitAssign(tmp, Op.Cast("trunc", valueTpe, shifted))
      tmp
    }

    private def emitSmallIntDiv(valueTpe: Type, a: Value, b: Value, fb: FunBuilder): Value = {
      val isZero = freshTmp(Type.I1)
      fb.current.emitAssign(isZero, Op.ICmp("eq", b, Value.IntConst(0L, valueTpe)))

      val okLabel = freshLabel("div_ok")
      val badLabel = freshLabel("div_bad")
      fb.current.setTerminator(Terminator.CondBr(isZero, badLabel, okLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)

      val a32 = freshTmp(Type.I32)
      fb.current.emitAssign(a32, Op.Cast("sext", Type.I32, a))
      val b32 = freshTmp(Type.I32)
      fb.current.emitAssign(b32, Op.Cast("sext", Type.I32, b))
      val div32 = freshTmp(Type.I32)
      fb.current.emitAssign(div32, Op.Bin("sdiv", Type.I32, a32, b32))
      val tmp = freshTmp(valueTpe)
      fb.current.emitAssign(tmp, Op.Cast("trunc", valueTpe, div32))
      tmp
    }

    private def emitSmallIntRem(valueTpe: Type, a: Value, b: Value, fb: FunBuilder): Value = {
      val isZero = freshTmp(Type.I1)
      fb.current.emitAssign(isZero, Op.ICmp("eq", b, Value.IntConst(0L, valueTpe)))

      val okLabel = freshLabel("rem_ok")
      val badLabel = freshLabel("rem_bad")
      fb.current.setTerminator(Terminator.CondBr(isZero, badLabel, okLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)

      val a32 = freshTmp(Type.I32)
      fb.current.emitAssign(a32, Op.Cast("sext", Type.I32, a))
      val b32 = freshTmp(Type.I32)
      fb.current.emitAssign(b32, Op.Cast("sext", Type.I32, b))
      val rem32 = freshTmp(Type.I32)
      fb.current.emitAssign(rem32, Op.Bin("srem", Type.I32, a32, b32))
      val tmp = freshTmp(valueTpe)
      fb.current.emitAssign(tmp, Op.Cast("trunc", valueTpe, rem32))
      tmp
    }

    private def emitSmallIntExp(valueTpe: Type, a: Value, b: Value, fb: FunBuilder): Value = {
      val a32 = freshTmp(Type.I32)
      fb.current.emitAssign(a32, Op.Cast("sext", Type.I32, a))
      val b32 = freshTmp(Type.I32)
      fb.current.emitAssign(b32, Op.Cast("sext", Type.I32, b))

      val aD = freshTmp(Type.Double)
      fb.current.emitAssign(aD, Op.Cast("sitofp", Type.Double, a32))
      val bD = freshTmp(Type.Double)
      fb.current.emitAssign(bD, Op.Cast("sitofp", Type.Double, b32))

      val powD = freshTmp(Type.Double)
      fb.current.emitAssign(powD, Op.Call(Type.Double, "llvm.pow.f64", List(aD, bD)))

      val asI32 = fpToInt32Saturating(powD, fb)

      val tmp = freshTmp(valueTpe)
      fb.current.emitAssign(tmp, Op.Cast("trunc", valueTpe, asI32))
      tmp
    }

    private def emitInt32Div(a: Value, b: Value, fb: FunBuilder): Value = {
      val isZero = freshTmp(Type.I1)
      fb.current.emitAssign(isZero, Op.ICmp("eq", b, Value.IntConst(0L, Type.I32)))

      val okLabel = freshLabel("idiv_ok")
      val badLabel = freshLabel("idiv_bad")
      fb.current.setTerminator(Terminator.CondBr(isZero, badLabel, okLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)

      val a64 = freshTmp(Type.I64)
      fb.current.emitAssign(a64, Op.Cast("sext", Type.I64, a))
      val b64 = freshTmp(Type.I64)
      fb.current.emitAssign(b64, Op.Cast("sext", Type.I64, b))
      val div64 = freshTmp(Type.I64)
      fb.current.emitAssign(div64, Op.Bin("sdiv", Type.I64, a64, b64))
      val tmp = freshTmp(Type.I32)
      fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, div64))
      tmp
    }

    private def emitInt32Rem(a: Value, b: Value, fb: FunBuilder): Value = {
      val isZero = freshTmp(Type.I1)
      fb.current.emitAssign(isZero, Op.ICmp("eq", b, Value.IntConst(0L, Type.I32)))

      val okLabel = freshLabel("irem_ok")
      val badLabel = freshLabel("irem_bad")
      fb.current.setTerminator(Terminator.CondBr(isZero, badLabel, okLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val okBlock = fb.newBlock(okLabel)
      fb.setCurrent(okBlock)

      val a64 = freshTmp(Type.I64)
      fb.current.emitAssign(a64, Op.Cast("sext", Type.I64, a))
      val b64 = freshTmp(Type.I64)
      fb.current.emitAssign(b64, Op.Cast("sext", Type.I64, b))
      val rem64 = freshTmp(Type.I64)
      fb.current.emitAssign(rem64, Op.Bin("srem", Type.I64, a64, b64))
      val tmp = freshTmp(Type.I32)
      fb.current.emitAssign(tmp, Op.Cast("trunc", Type.I32, rem64))
      tmp
    }

    private def emitInt64Div(a: Value, b: Value, fb: FunBuilder): Value = {
      val isZero = freshTmp(Type.I1)
      fb.current.emitAssign(isZero, Op.ICmp("eq", b, Value.IntConst(0L, Type.I64)))

      val checkLabel = freshLabel("ldiv_check")
      val badLabel = freshLabel("ldiv_bad")
      fb.current.setTerminator(Terminator.CondBr(isZero, badLabel, checkLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val checkBlock = fb.newBlock(checkLabel)
      fb.setCurrent(checkBlock)

      val isMin = freshTmp(Type.I1)
      fb.current.emitAssign(isMin, Op.ICmp("eq", a, Value.IntConst(Long.MinValue, Type.I64)))
      val isNegOne = freshTmp(Type.I1)
      fb.current.emitAssign(isNegOne, Op.ICmp("eq", b, Value.IntConst(-1L, Type.I64)))
      val overflow = freshTmp(Type.I1)
      fb.current.emitAssign(overflow, Op.Bin("and", Type.I1, isMin, isNegOne))

      val overflowLabel = freshLabel("ldiv_ovf")
      val computeLabel = freshLabel("ldiv_compute")
      val endLabel = freshLabel("ldiv_end")
      fb.current.setTerminator(Terminator.CondBr(overflow, overflowLabel, computeLabel))

      val incomings = mutable.ArrayBuffer.empty[(Value, String)]

      val ovfBlock = fb.newBlock(overflowLabel)
      fb.setCurrent(ovfBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(Long.MinValue, Type.I64), overflowLabel))

      val computeBlock = fb.newBlock(computeLabel)
      fb.setCurrent(computeBlock)
      val div = freshTmp(Type.I64)
      fb.current.emitAssign(div, Op.Bin("sdiv", Type.I64, a, b))
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((div, computeLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val phi = freshTmp(Type.I64)
      endBlock.emitPhi(phi, incomings.toList)
      phi
    }

    private def emitInt64Rem(a: Value, b: Value, fb: FunBuilder): Value = {
      val isZero = freshTmp(Type.I1)
      fb.current.emitAssign(isZero, Op.ICmp("eq", b, Value.IntConst(0L, Type.I64)))

      val checkLabel = freshLabel("lrem_check")
      val badLabel = freshLabel("lrem_bad")
      fb.current.setTerminator(Terminator.CondBr(isZero, badLabel, checkLabel))

      val badBlock = fb.newBlock(badLabel)
      fb.setCurrent(badBlock)
      fb.current.emitTrap()
      fb.current.setTerminator(Terminator.Unreachable)

      val checkBlock = fb.newBlock(checkLabel)
      fb.setCurrent(checkBlock)

      val isNegOne = freshTmp(Type.I1)
      fb.current.emitAssign(isNegOne, Op.ICmp("eq", b, Value.IntConst(-1L, Type.I64)))

      val zeroLabel = freshLabel("lrem_zero")
      val computeLabel = freshLabel("lrem_compute")
      val endLabel = freshLabel("lrem_end")
      fb.current.setTerminator(Terminator.CondBr(isNegOne, zeroLabel, computeLabel))

      val incomings = mutable.ArrayBuffer.empty[(Value, String)]

      val zeroBlock = fb.newBlock(zeroLabel)
      fb.setCurrent(zeroBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(0L, Type.I64), zeroLabel))

      val computeBlock = fb.newBlock(computeLabel)
      fb.setCurrent(computeBlock)
      val rem = freshTmp(Type.I64)
      fb.current.emitAssign(rem, Op.Bin("srem", Type.I64, a, b))
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((rem, computeLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val phi = freshTmp(Type.I64)
      endBlock.emitPhi(phi, incomings.toList)
      phi
    }

    private def emitIntExp(a: Value, b: Value, resultBits: Int, fb: FunBuilder): Value = {
      val aD = freshTmp(Type.Double)
      fb.current.emitAssign(aD, Op.Cast("sitofp", Type.Double, a))
      val bD = freshTmp(Type.Double)
      fb.current.emitAssign(bD, Op.Cast("sitofp", Type.Double, b))

      val powD = freshTmp(Type.Double)
      fb.current.emitAssign(powD, Op.Call(Type.Double, "llvm.pow.f64", List(aD, bD)))

      resultBits match {
        case 32 => fpToInt32Saturating(powD, fb)
        case 64 => fpToInt64Saturating(powD, fb)
        case _ =>
          fb.current.emitTrap()
          Value.Undef(Type.I64)
      }
    }

    private def fpToInt32Saturating(x: Value, fb: FunBuilder): Value = {
      val (maxPlusOne, minValue) = x.tpe match {
        case Type.Float =>
          val maxPlusOne = Value.Float32Const(java.lang.Float.floatToRawIntBits(2147483648.0f))
          val minValue = Value.Float32Const(java.lang.Float.floatToRawIntBits(-2147483648.0f))
          (maxPlusOne, minValue)
        case Type.Double =>
          val maxPlusOne = Value.Float64Const(java.lang.Double.doubleToRawLongBits(2147483648.0))
          val minValue = Value.Float64Const(java.lang.Double.doubleToRawLongBits(-2147483648.0))
          (maxPlusOne, minValue)
        case _ =>
          fb.current.emitTrap()
          return Value.Undef(Type.I32)
      }

      val isNaN = freshTmp(Type.I1)
      fb.current.emitAssign(isNaN, Op.FCmp("uno", x, x))

      val nanLabel = freshLabel("fp_nan")
      val notNanLabel = freshLabel("fp_notnan")
      val endLabel = freshLabel("fp_end")
      fb.current.setTerminator(Terminator.CondBr(isNaN, nanLabel, notNanLabel))

      val incomings = mutable.ArrayBuffer.empty[(Value, String)]

      val nanBlock = fb.newBlock(nanLabel)
      fb.setCurrent(nanBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(0L, Type.I32), nanLabel))

      val notNanBlock = fb.newBlock(notNanLabel)
      fb.setCurrent(notNanBlock)

      val tooBig = freshTmp(Type.I1)
      fb.current.emitAssign(tooBig, Op.FCmp("oge", x, maxPlusOne))

      val bigLabel = freshLabel("fp_big")
      val checkSmallLabel = freshLabel("fp_check_small")
      fb.current.setTerminator(Terminator.CondBr(tooBig, bigLabel, checkSmallLabel))

      val bigBlock = fb.newBlock(bigLabel)
      fb.setCurrent(bigBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(Int.MaxValue.toLong, Type.I32), bigLabel))

      val checkSmallBlock = fb.newBlock(checkSmallLabel)
      fb.setCurrent(checkSmallBlock)

      val tooSmall = freshTmp(Type.I1)
      fb.current.emitAssign(tooSmall, Op.FCmp("olt", x, minValue))

      val smallLabel = freshLabel("fp_small")
      val inRangeLabel = freshLabel("fp_inrange")
      fb.current.setTerminator(Terminator.CondBr(tooSmall, smallLabel, inRangeLabel))

      val smallBlock = fb.newBlock(smallLabel)
      fb.setCurrent(smallBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(Int.MinValue.toLong, Type.I32), smallLabel))

      val inRangeBlock = fb.newBlock(inRangeLabel)
      fb.setCurrent(inRangeBlock)
      val asI32 = freshTmp(Type.I32)
      fb.current.emitAssign(asI32, Op.Cast("fptosi", Type.I32, x))
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((asI32, inRangeLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val phi = freshTmp(Type.I32)
      endBlock.emitPhi(phi, incomings.toList)
      phi
    }

    private def fpToInt64Saturating(x: Value, fb: FunBuilder): Value = {
      val (maxPlusOne, minValue) = x.tpe match {
        case Type.Float =>
          val maxPlusOne = Value.Float32Const(java.lang.Float.floatToRawIntBits(9223372036854775808.0f))
          val minValue = Value.Float32Const(java.lang.Float.floatToRawIntBits(-9223372036854775808.0f))
          (maxPlusOne, minValue)
        case Type.Double =>
          val maxPlusOne = Value.Float64Const(java.lang.Double.doubleToRawLongBits(9223372036854775808.0))
          val minValue = Value.Float64Const(java.lang.Double.doubleToRawLongBits(-9223372036854775808.0))
          (maxPlusOne, minValue)
        case _ =>
          fb.current.emitTrap()
          return Value.Undef(Type.I64)
      }

      val isNaN = freshTmp(Type.I1)
      fb.current.emitAssign(isNaN, Op.FCmp("uno", x, x))

      val nanLabel = freshLabel("fp_nan")
      val notNanLabel = freshLabel("fp_notnan")
      val endLabel = freshLabel("fp_end")
      fb.current.setTerminator(Terminator.CondBr(isNaN, nanLabel, notNanLabel))

      val incomings = mutable.ArrayBuffer.empty[(Value, String)]

      val nanBlock = fb.newBlock(nanLabel)
      fb.setCurrent(nanBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(0L, Type.I64), nanLabel))

      val notNanBlock = fb.newBlock(notNanLabel)
      fb.setCurrent(notNanBlock)

      val tooBig = freshTmp(Type.I1)
      fb.current.emitAssign(tooBig, Op.FCmp("oge", x, maxPlusOne))

      val bigLabel = freshLabel("fp_big")
      val checkSmallLabel = freshLabel("fp_check_small")
      fb.current.setTerminator(Terminator.CondBr(tooBig, bigLabel, checkSmallLabel))

      val bigBlock = fb.newBlock(bigLabel)
      fb.setCurrent(bigBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(Long.MaxValue, Type.I64), bigLabel))

      val checkSmallBlock = fb.newBlock(checkSmallLabel)
      fb.setCurrent(checkSmallBlock)

      val tooSmall = freshTmp(Type.I1)
      fb.current.emitAssign(tooSmall, Op.FCmp("olt", x, minValue))

      val smallLabel = freshLabel("fp_small")
      val inRangeLabel = freshLabel("fp_inrange")
      fb.current.setTerminator(Terminator.CondBr(tooSmall, smallLabel, inRangeLabel))

      val smallBlock = fb.newBlock(smallLabel)
      fb.setCurrent(smallBlock)
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((Value.IntConst(Long.MinValue, Type.I64), smallLabel))

      val inRangeBlock = fb.newBlock(inRangeLabel)
      fb.setCurrent(inRangeBlock)
      val asI64 = freshTmp(Type.I64)
      fb.current.emitAssign(asI64, Op.Cast("fptosi", Type.I64, x))
      fb.current.setTerminator(Terminator.Br(endLabel))
      incomings.addOne((asI64, inRangeLabel))

      val endBlock = fb.newBlock(endLabel)
      fb.setCurrent(endBlock)
      val phi = freshTmp(Type.I64)
      endBlock.emitPhi(phi, incomings.toList)
      phi
    }

    private def intBits(tpe: Type): Int = tpe match {
      case Type.I1 => 1
      case Type.I8 => 8
      case Type.I16 => 16
      case Type.I32 => 32
      case Type.I64 => 64
      case _ => 0
    }

    /**
      * Local function builder utilities.
      */
		    private final class FunBuilder {
		      private val blocks = mutable.ArrayBuffer.empty[BlockBuilder]
		      private val blockMap = mutable.Map.empty[String, BlockBuilder]

		      var current: BlockBuilder = _
		      var traceEnabled: Boolean = false
		      var rootsToPop: Long = 0L

		      def newBlock(label: String): BlockBuilder = {
		        val b = new BlockBuilder(label, this)
		        blocks.addOne(b)
		        blockMap.put(label, b)
	        b
	      }

      def setCurrent(b: BlockBuilder): Unit = {
        current = b
      }

      def getBlock(label: String): Option[BlockBuilder] = blockMap.get(label)

      def result(): List[LlvmIr.Block] = {
        blocks.toList.map(_.toBlock)
      }
	    }

	    private final class BlockBuilder(val label: String, fb: FunBuilder) {
	      private val phis = mutable.ArrayBuffer.empty[Instr.Phi]
	      private val instrs = mutable.ArrayBuffer.empty[Instr]
	      private var term: Option[Terminator] = None

      def isTerminated: Boolean = term.nonEmpty

	      def emitAssign(dest: Value.Local, op: Op): Unit = {
	        op match {
	          case Op.Alloca(_) if label != "entry" =>
	            // Avoid `alloca` in loop bodies (including the self-tail loop), which would grow the
	            // stack each iteration and eventually crash (stack overflow). We hoist all `alloca`
	            // instructions into the entry block.
	            fb.getBlock("entry") match {
	              case Some(entry) =>
	                entry.emitPrologueAssign(dest, op)
	              case None =>
	                // Should not happen: every function builder creates an "entry" block first.
	                ensureNotTerminated()
	                instrs.addOne(Instr.Assign(dest, op))
	            }
	          case _ =>
	            ensureNotTerminated()
	            instrs.addOne(Instr.Assign(dest, op))
	        }
	      }

      /**
        * Inserts an assignment at the beginning of the instruction stream (after any phi nodes).
        *
        * This is used to hoist `alloca`-style temporaries into a dominating block even after the
        * block has been terminated.
        */
      def emitPrologueAssign(dest: Value.Local, op: Op): Unit = {
        instrs.insert(0, Instr.Assign(dest, op))
      }

      def emitStore(value: Value, addr: Value): Unit = {
        ensureNotTerminated()
        instrs.addOne(Instr.Store(value, addr))
      }

      def emitCallVoid(name: String, args: List[Value]): Unit = {
        ensureNotTerminated()
        instrs.addOne(Instr.CallVoid(name, args))
      }

      def emitPhi(dest: Value.Local, incomings: List[(Value, String)]): Unit = {
        ensureNotTerminated()
        if (instrs.nonEmpty) {
          throw new IllegalStateException(s"Phi inserted after non-phi instructions in block '$label'.")
        }
        phis.addOne(Instr.Phi(dest, incomings))
      }

      def emitTrap(): Unit = {
        ensureNotTerminated()
        instrs.addOne(Instr.CallVoid("llvm.trap", Nil))
      }

		      def setTerminator(t: Terminator): Unit = {
		        ensureNotTerminated()
		        t match {
		          case Terminator.Ret(_, _) =>
		            if (fb.rootsToPop > 0) {
		              instrs.addOne(Instr.CallVoid("flix_gc_pop_roots", List(Value.Local("ctx", Type.Ptr), Value.IntConst(fb.rootsToPop, Type.I64))))
		            }
		            if (fb.traceEnabled) {
		              instrs.addOne(Instr.CallVoid("flix_trace_pop", Nil))
		            }
		          case _ => ()
		        }
		        term = Some(t)
		      }

      private def ensureNotTerminated(): Unit = {
        if (term.nonEmpty) {
          throw new IllegalStateException(s"Cannot emit into terminated block '$label'.")
        }
      }

      def toBlock: LlvmIr.Block = {
        val t = term.getOrElse {
          throw new IllegalStateException(s"Block '$label' missing terminator.")
        }
        LlvmIr.Block(label, phis.toList, instrs.toList, t)
      }
    }
  }

	    private object LlvmNames {
	      def defName(sym: Symbol.DefnSym): String =
	        s"flix_${LlvmNamesInternal.mangle(sym.toString)}"

      def exportName(sym: Symbol.DefnSym): String =
        s"flix_export_${LlvmNamesInternal.mangle(sym.toString)}"

      def exportResumeName(sym: Symbol.DefnSym): String =
        s"flix_export_resume_${LlvmNamesInternal.mangle(sym.toString)}"

      def exportRequestName(sym: Symbol.DefnSym): String =
        s"flix_export_request_${LlvmNamesInternal.mangle(sym.toString)}"

	      def frameApplyName(sym: Symbol.DefnSym): String =
	        s"flix_frame_apply_${LlvmNamesInternal.mangle(sym.toString)}"

	      def traceName(sym: Symbol.DefnSym): String =
	        s"flix_trace_name_${LlvmNamesInternal.mangle(sym.toString)}"

      def effectName(sym: Symbol.EffSym): String =
        s"flix_effect_name_${LlvmNamesInternal.mangle(sym.toString)}"

      def opName(sym: Symbol.OpSym): String =
        s"flix_op_name_${LlvmNamesInternal.mangle(sym.toString)}"

	      def closureInvokeName(sym: Symbol.DefnSym): String =
	        s"flix_clo_invoke_${LlvmNamesInternal.mangle(sym.toString)}"

    def closureTypeInfoName(sym: Symbol.DefnSym): String =
      s"flix_ti_clo_${LlvmNamesInternal.mangle(sym.toString)}"

    def closurePtrOffsName(sym: Symbol.DefnSym): String =
      s"${closureTypeInfoName(sym)}_ptr_offs"

    def thunkInvokeName(sym: Symbol.DefnSym): String =
      s"flix_thunk_invoke_${LlvmNamesInternal.mangle(sym.toString)}"

    def kTypeInfoName(argTpe: SimpleType): String =
      s"flix_ti_k_${typeKey(argTpe)}"

    def kPtrOffsName(argTpe: SimpleType): String =
      s"${kTypeInfoName(argTpe)}_ptr_offs"

    private def typeKey(tpe: SimpleType): String = {
      val s = tpe.toString
      val base = LlvmNamesInternal.mangle(s)
      val md = java.security.MessageDigest.getInstance("SHA-1")
      val digest = md.digest(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      val hash = digest.take(8).map(b => f"${b & 0xff}%02x").mkString
      s"${base}_$hash"
    }

    def lazyTypeInfoName(innerTpe: SimpleType): String =
      s"flix_ti_lazy_${typeKey(innerTpe)}"

    def lazyPtrOffsName(innerTpe: SimpleType): String =
      s"${lazyTypeInfoName(innerTpe)}_ptr_offs"

	    def tupleTypeInfoName(tpe: SimpleType.Tuple): String =
	      s"flix_ti_tuple_${typeKey(tpe)}"

	    def tuplePtrOffsName(tpe: SimpleType.Tuple): String =
	      s"${tupleTypeInfoName(tpe)}_ptr_offs"

      def tagTypeInfoName(sym: Symbol.CaseSym): String =
        s"flix_ti_tag_${LlvmNamesInternal.mangle(sym.toString)}"

      def tagPtrOffsName(sym: Symbol.CaseSym): String =
        s"${tagTypeInfoName(sym)}_ptr_offs"

      def structTypeInfoName(sym: Symbol.StructSym): String =
        s"flix_ti_struct_${LlvmNamesInternal.mangle(sym.toString)}"

      def structPtrOffsName(sym: Symbol.StructSym): String =
        s"${structTypeInfoName(sym)}_ptr_offs"

      def recordTypeInfoName(tpe: SimpleType): String =
        s"flix_ti_record_${typeKey(tpe)}"

      def recordPtrOffsName(tpe: SimpleType): String =
        s"${recordTypeInfoName(tpe)}_ptr_offs"

    def thunkApplyClosureName(argTpe: SimpleType): String =
      s"flix_thunk_apply_clo_${typeKey(argTpe)}"

    def thunkTypeInfoName(sym: Symbol.DefnSym): String =
      s"flix_ti_thunk_${LlvmNamesInternal.mangle(sym.toString)}"

    def thunkPtrOffsName(sym: Symbol.DefnSym): String =
      s"${thunkTypeInfoName(sym)}_ptr_offs"

    def thunkApplyClosureTypeInfoName(argTpe: SimpleType): String =
      s"flix_ti_thunk_apply_clo_${typeKey(argTpe)}"

    def thunkApplyClosurePtrOffsName(argTpe: SimpleType): String =
      s"${thunkApplyClosureTypeInfoName(argTpe)}_ptr_offs"

    def arrayPrimTypeInfoName: String =
      "flix_ti_array_prim"

    def arrayPrimPtrOffsName: String =
      s"${arrayPrimTypeInfoName}_ptr_offs"

    def arrayPtrTypeInfoName: String =
      "flix_ti_array_ptr"

    def arrayPtrPtrOffsName: String =
      s"${arrayPtrTypeInfoName}_ptr_offs"

    def stringTypeInfoName: String =
      "flix_ti_string"

    def bigIntTypeInfoName: String =
      "flix_ti_bigint"

    def bigDecimalTypeInfoName: String =
      "flix_ti_bigdecimal"

    def handlerTypeInfoName: String =
      "flix_ti_handler"

    def suspensionTypeInfoName: String =
      "flix_ti_suspension"

    def frameTypeInfoName(sym: Symbol.DefnSym): String =
      s"flix_ti_frame_${LlvmNamesInternal.mangle(sym.toString)}"

    def framePtrOffsName(sym: Symbol.DefnSym): String =
      s"${frameTypeInfoName(sym)}_ptr_offs"

    def paramName(i: Int): String =
      s"a$i"
  }

  private object LlvmNamesInternal {
    /**
      * LLVM-safe name mangling: keep alphanumerics and `_`, replace everything else with `_`.
      */
    def mangle(s: String): String = {
      val b = new StringBuilder(s.length + 8)
      s.foreach {
        case c if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' =>
          b.append(c)
        case _ => b.append('_')
      }
      if (b.isEmpty) "_"
      else {
        val head = b.charAt(0)
        if ((head >= '0' && head <= '9')) "_" + b.toString() else b.toString()
      }
    }
  }
}
