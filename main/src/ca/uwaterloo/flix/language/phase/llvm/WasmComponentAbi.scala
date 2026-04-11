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

import ca.uwaterloo.flix.language.phase.ExportAbi
import ca.uwaterloo.flix.language.phase.ExportAbi.AbiType
import ca.uwaterloo.flix.util.InternalCompilerException

object WasmComponentAbi {

  def witTypeTag(tpe: AbiType): String = tpe match {
    case AbiType.Unit => "unit"
    case AbiType.Bool => "bool"
    case AbiType.Int8 => "int8"
    case AbiType.Int16 => "int16"
    case AbiType.Int32 => "int32"
    case AbiType.Int64 => "int64"
    case AbiType.Float32 => "float32"
    case AbiType.Float64 => "float64"
    case AbiType.String => "string"
    case AbiType.Bytes => "bytes"
    case other => other.stableId.replace('_', '-')
  }

  def witTypeOf(tpe: AbiType): String = tpe match {
    case AbiType.Unit => "tuple<>"
    case AbiType.Bool => "bool"
    case AbiType.Int8 => "s8"
    case AbiType.Int16 => "s16"
    case AbiType.Int32 => "s32"
    case AbiType.Int64 => "s64"
    case AbiType.Float32 => "f32"
    case AbiType.Float64 => "f64"
    case AbiType.String => "string"
    case AbiType.Bytes => "list<u8>"
    case other => witTypeTag(other)
  }

  def aggregateTypes(signatures: Iterable[ExportAbi.Signature]): List[AbiType] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, AbiType]

    def visit(tpe: AbiType): Unit = tpe match {
      case seq@AbiType.List(elm) =>
        visit(elm)
        out.getOrElseUpdate(seq.stableId, seq)
      case seq@AbiType.Array(elm) =>
        visit(elm)
        out.getOrElseUpdate(seq.stableId, seq)
      case tup@AbiType.Tuple(elms) =>
        elms.foreach(visit)
        out.getOrElseUpdate(tup.stableId, tup)
      case rec@AbiType.Record(fields) =>
        fields.foreach { case (_, fieldTpe) => visit(fieldTpe) }
        out.getOrElseUpdate(rec.stableId, rec)
      case opt@AbiType.Option(elm) =>
        visit(elm)
        out.getOrElseUpdate(opt.stableId, opt)
      case res@AbiType.Result(ok, err) =>
        visit(ok)
        visit(err)
        out.getOrElseUpdate(res.stableId, res)
      case _ => ()
    }

    signatures.foreach { sig =>
      sig.params.foreach(visit)
      visit(sig.result)
    }

    out.values.toList
  }

  def renderAggregateTypeDef(tpe: AbiType, indent: String = "  "): String = tpe match {
    case AbiType.List(elm) =>
      s"${indent}type ${witTypeOf(tpe)} = list<${witTypeOf(elm)}>;\n"

    case AbiType.Array(elm) =>
      s"${indent}type ${witTypeOf(tpe)} = list<${witTypeOf(elm)}>;\n"

    case AbiType.Tuple(elms) =>
      val fields = elms.zipWithIndex.map {
        case (elm, idx) => s"${indent}  f$idx: ${witTypeOf(elm)},"
      }.mkString("\n")
      s"""${indent}record ${witTypeOf(tpe)} {
         |$fields
         |${indent}}
         |""".stripMargin

    case AbiType.Record(fields) =>
      val body = fields.map {
        case (label, fieldTpe) => s"${indent}  $label: ${witTypeOf(fieldTpe)},"
      }.mkString("\n")
      s"""${indent}record ${witTypeOf(tpe)} {
         |$body
         |${indent}}
         |""".stripMargin

    case AbiType.Option(elm) =>
      s"""${indent}record ${witTypeOf(tpe)} {
         |${indent}  is-some: bool,
         |${indent}  val: ${witTypeOf(elm)},
         |${indent}}
         |""".stripMargin

    case AbiType.Result(ok, err) =>
      s"""${indent}record ${witTypeOf(tpe)} {
         |${indent}  is-ok: bool,
         |${indent}  ok: ${witTypeOf(ok)},
         |${indent}  err: ${witTypeOf(err)},
         |${indent}}
         |""".stripMargin

    case other =>
      throw InternalCompilerException(s"Unexpected non-aggregate wasm component ABI type: '$other'.", ca.uwaterloo.flix.language.ast.SourceLocation.Unknown)
  }
}
