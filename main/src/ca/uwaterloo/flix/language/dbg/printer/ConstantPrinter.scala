/*
 * Copyright 2023 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.dbg.DocAst.Expr

object ConstantPrinter {

  /**
    * Returns the [[Expr]] representation of `cst`.
    */
  def print(cst: Constant): Expr = cst match {
    case Constant.Unit => Expr.Unit
    case Constant.Null => Expr.AsIs("null")
    case Constant.Bool(lit) => Expr.AsIs(lit.toString)
    case Constant.Char(lit) => Expr.AsIs("'''" + lit.toString + "'''")
    case Constant.Float32(lit) => Expr.AsIs(s"${lit}f32")
    case Constant.Float64(lit) => Expr.AsIs(s"${lit}f64")
    case Constant.BigDecimal(lit) => Expr.AsIs(s"${lit}ff")
    case Constant.Int8(lit) => Expr.AsIs(s"${lit}i8")
    case Constant.Int16(lit) => Expr.AsIs(s"${lit}i16")
    case Constant.Int32(lit) => Expr.AsIs(s"${lit}i32")
    case Constant.Int64(lit) => Expr.AsIs(s"${lit}i64")
    case Constant.BigInt(lit) => Expr.AsIs(s"${lit}ii")
    case Constant.Str(lit) => Expr.AsIs("\"\"\"" + lit + "\"\"\"")
    case Constant.Regex(lit) => Expr.Regex(lit)
    case Constant.RecordEmpty => Expr.RecordEmpty
    case Constant.Static => Expr.AsIs("Static")
  }

}
