/*
 * Copyright 2025 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.shared.Constant

object FormatConstant {

  /**
    * Returns a string representation of the given constant.
    */
  def formatConstant(cst: Constant): String = cst match {
    case Constant.Unit => "()"
    case Constant.Null => "null"
    case Constant.Bool(lit) => lit.toString
    case Constant.Char(lit) => "'" + lit.toString + "'"
    case Constant.Float32(lit) => s"${lit}f32"
    case Constant.Float64(lit) => s"${lit}f64"
    case Constant.BigDecimal(lit) => s"${lit}ff"
    case Constant.Int8(lit) => s"${lit}i8"
    case Constant.Int16(lit) => s"${lit}i16"
    case Constant.Int32(lit) => s"${lit}i32"
    case Constant.Int64(lit) => s"${lit}i64"
    case Constant.BigInt(lit) => s"${lit}ii"
    case Constant.Str(lit) => "\"" + lit + "\""
    case Constant.Regex(lit) => "regex\"" + lit.pattern() + "\""
    case Constant.RecordEmpty => "{}"
    case Constant.Static => "Static"
  }

}
