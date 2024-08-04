/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.dbg.DocAst.Type

object MonoTypePrinter {

  /**
    * Returns the [[Type]] representation of `tpe`.
    */
  def print(tpe: MonoType): Type = tpe match {
    case MonoType.Void => Type.Void
    case MonoType.AnyType => Type.AnyType
    case MonoType.Unit => Type.Unit
    case MonoType.Bool => Type.Bool
    case MonoType.Char => Type.Char
    case MonoType.Float32 => Type.Float32
    case MonoType.Float64 => Type.Float64
    case MonoType.BigDecimal => Type.BigDecimal
    case MonoType.Int8 => Type.Int8
    case MonoType.Int16 => Type.Int16
    case MonoType.Int32 => Type.Int32
    case MonoType.Int64 => Type.Int64
    case MonoType.BigInt => Type.BigInt
    case MonoType.String => Type.Str
    case MonoType.Regex => Type.Regex
    case MonoType.Region => Type.Region
    case MonoType.Null => Type.Null
    case MonoType.Array(tpe) => Type.Array(print(tpe))
    case MonoType.Lazy(tpe) => Type.Lazy(print(tpe))
    case MonoType.Ref(tpe) => Type.Ref(print(tpe))
    case MonoType.Tuple(elms) => Type.Tuple(elms.map(print))
    case MonoType.Enum(sym) => Type.Enum(sym, Nil)
    case MonoType.Struct(sym) => Type.Struct(sym, Nil)
    case MonoType.Arrow(args, result) => Type.Arrow(args.map(print), print(result))
    case MonoType.RecordEmpty => Type.RecordEmpty
    case MonoType.RecordExtend(label, value, rest) => Type.RecordExtend(label, print(value), print(rest))
    case MonoType.Native(clazz) => Type.Native(clazz)
  }

}
