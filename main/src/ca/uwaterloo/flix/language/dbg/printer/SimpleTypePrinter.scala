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

import ca.uwaterloo.flix.language.ast.SimpleType
import ca.uwaterloo.flix.language.dbg.DocAst.Type

object SimpleTypePrinter {

  /** Returns the [[Type]] representation of `tpe0`. */
  def print(tpe0: SimpleType): Type = tpe0 match {
    case SimpleType.Void => Type.Void
    case SimpleType.AnyType => Type.AnyType
    case SimpleType.Unit => Type.Unit
    case SimpleType.Bool => Type.Bool
    case SimpleType.Char => Type.Char
    case SimpleType.Float32 => Type.Float32
    case SimpleType.Float64 => Type.Float64
    case SimpleType.BigDecimal => Type.BigDecimal
    case SimpleType.Int8 => Type.Int8
    case SimpleType.Int16 => Type.Int16
    case SimpleType.Int32 => Type.Int32
    case SimpleType.Int64 => Type.Int64
    case SimpleType.BigInt => Type.BigInt
    case SimpleType.String => Type.Str
    case SimpleType.Regex => Type.Regex
    case SimpleType.Region => Type.Region
    case SimpleType.Null => Type.Null
    case SimpleType.Array(tpe) => Type.Array(print(tpe))
    case SimpleType.Lazy(tpe) => Type.Lazy(print(tpe))
    case SimpleType.Tuple(elms) => Type.Tuple(elms.map(print))
    case SimpleType.Enum(sym, targs) => Type.Enum(sym, targs.map(print))
    case SimpleType.Struct(sym, targs) => Type.Struct(sym, targs.map(print))
    case SimpleType.Arrow(args, result) => Type.Arrow(args.map(print), print(result))
    case SimpleType.RecordEmpty => Type.RecordEmpty
    case SimpleType.RecordExtend(label, value, rest) => Type.RecordExtend(label, print(value), print(rest))
    case SimpleType.ExtensibleEmpty => Type.ExtensibleEmpty
    case SimpleType.ExtensibleExtend(cons, tpes, rest) => Type.ExtensibleExtend(cons.name, tpes.map(print), print(rest))
    case SimpleType.Native(clazz) => Type.Native(clazz)
  }

}
