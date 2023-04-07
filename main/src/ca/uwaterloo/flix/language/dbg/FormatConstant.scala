/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.Ast

/**
  * Formatting of constants.
  */
object FormatConstant {

  /**
    * Returns a string representation of the given constant.
    */
  def format(cst: Ast.Constant): String = cst match {
    case Ast.Constant.Unit => "()"
    case Ast.Constant.Null => "null"
    case Ast.Constant.Bool(true) => "true"
    case Ast.Constant.Bool(false) => "false"
    case Ast.Constant.Char(lit) => "'" + lit + "'"
    case Ast.Constant.Float32(lit) => s"${lit}f32"
    case Ast.Constant.Float64(lit) => s"${lit}f64"
    case Ast.Constant.BigDecimal(lit) => s"${lit}ff"
    case Ast.Constant.Int8(lit) => s"${lit}i8"
    case Ast.Constant.Int16(lit) => s"${lit}i16"
    case Ast.Constant.Int32(lit) => s"${lit}i32"
    case Ast.Constant.Int64(lit) => s"${lit}i64"
    case Ast.Constant.BigInt(lit) => s"${lit}ii"
    case Ast.Constant.Str(lit) => "\"" + lit + "\""
    case Ast.Constant.Regex(lit) => "Regex#\"" + lit + "\""
  }
}
