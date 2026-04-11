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

package ca.uwaterloo.flix.language.ast

import java.nio.charset.StandardCharsets

/**
  * Computes stable "exception kind ids" from Flix types.
  *
  * The ids are used for portable exception dispatch (`try/catch`) on non-JVM targets.
  *
  * Important: v0 uses a simple hash of a stable type key. This is sufficient for deterministic
  * dispatch within a compilation unit and across backends, but we may want to align this with
  * the eventual runtime `type_id`/`typeinfo` scheme as the native runtime matures.
  */
object ExnKindId {

  /**
    * Returns the kind id for `tpe`.
    *
    * The returned value is a 32-bit hash (interpreted as signed by Flix's `Int32`).
    */
  def of(tpe: SimpleType): Int = {
    val key = stableKey(tpe)
    fnv1a32(key.getBytes(StandardCharsets.UTF_8))
  }

  private def stableKey(tpe: SimpleType): String = tpe match {
    case SimpleType.Void => "Void"
    case SimpleType.AnyType => "AnyType"
    case SimpleType.Unit => "Unit"
    case SimpleType.Bool => "Bool"
    case SimpleType.Char => "Char"
    case SimpleType.Float32 => "Float32"
    case SimpleType.Float64 => "Float64"
    case SimpleType.BigDecimal => "BigDecimal"
    case SimpleType.Int8 => "Int8"
    case SimpleType.Int16 => "Int16"
    case SimpleType.Int32 => "Int32"
    case SimpleType.Int64 => "Int64"
    case SimpleType.BigInt => "BigInt"
    case SimpleType.String => "String"
    case SimpleType.Regex => "Regex"
    case SimpleType.StringBuilderHandle => "StringBuilderHandle"
    case SimpleType.RegexMatcher => "RegexMatcher"
    case SimpleType.ChannelHandle => "ChannelHandle"
    case SimpleType.ReentrantLockHandle => "ReentrantLockHandle"
    case SimpleType.ConditionHandle => "ConditionHandle"
    case SimpleType.CyclicBarrierHandle => "CyclicBarrierHandle"
    case SimpleType.CountDownLatchHandle => "CountDownLatchHandle"
    case SimpleType.SemaphoreHandle => "SemaphoreHandle"
    case SimpleType.Region => "Region"
    case SimpleType.Null => "Null"

    case SimpleType.Array(t) => s"Array(${stableKey(t)})"
    case SimpleType.Lazy(t) => s"Lazy(${stableKey(t)})"
    case SimpleType.Tuple(ts) => s"Tuple(${ts.map(stableKey).mkString(",")})"
    case SimpleType.Enum(sym, targs) => s"Enum(${sym.toString};${targs.map(stableKey).mkString(",")})"
    case SimpleType.Struct(sym, targs) => s"Struct(${sym.toString};${targs.map(stableKey).mkString(",")})"
    case SimpleType.Arrow(args, result) => s"Arrow(${args.map(stableKey).mkString(",")})->${stableKey(result)}"

    case SimpleType.RecordEmpty => "RecordEmpty"
    case SimpleType.RecordExtend(label, value, rest) => s"RecordExtend($label,${stableKey(value)},${stableKey(rest)})"

    case SimpleType.ExtensibleEmpty => "ExtensibleEmpty"
    case SimpleType.ExtensibleExtend(cons, tpes, rest) => s"ExtensibleExtend(${cons.toString},${tpes.map(stableKey).mkString(",")},${stableKey(rest)})"

    case SimpleType.Native(clazz) => s"Native(${clazz.getName})"
  }

  private def fnv1a32(bytes: Array[Byte]): Int = {
    var hash = 0x811c9dc5
    var i = 0
    while (i < bytes.length) {
      hash ^= (bytes(i) & 0xff)
      hash *= 0x01000193
      i += 1
    }
    hash
  }

}
