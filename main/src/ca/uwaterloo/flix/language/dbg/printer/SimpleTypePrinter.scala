/*
 * Copyright 2023 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.SimpleType
import ca.uwaterloo.flix.language.dbg.DocAst
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
    case SimpleType.Native(clazz) => Type.AsIs(DocAst.Expr.javaClassName(clazz))
  }

}
