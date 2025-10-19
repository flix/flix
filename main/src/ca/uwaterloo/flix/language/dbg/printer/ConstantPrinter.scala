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

import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.dbg.DocAst.Exp

object ConstantPrinter {

  /**
    * Returns the [[Exp]] representation of `cst`.
    */
  def print(cst: Constant): Exp = cst match {
    case Constant.Unit => Exp.Unit
    case Constant.Null => Exp.AsIs("null")
    case Constant.Bool(lit) => Exp.AsIs(lit.toString)
    case Constant.Char(lit) => Exp.AsIs("'''" + lit.toString + "'''")
    case Constant.Float32(lit) => Exp.AsIs(s"${lit}f32")
    case Constant.Float64(lit) => Exp.AsIs(s"${lit}f64")
    case Constant.BigDecimal(lit) => Exp.AsIs(s"${lit}ff")
    case Constant.Int8(lit) => Exp.AsIs(s"${lit}i8")
    case Constant.Int16(lit) => Exp.AsIs(s"${lit}i16")
    case Constant.Int32(lit) => Exp.AsIs(s"${lit}i32")
    case Constant.Int64(lit) => Exp.AsIs(s"${lit}i64")
    case Constant.BigInt(lit) => Exp.AsIs(s"${lit}ii")
    case Constant.Str(lit) => Exp.AsIs("\"\"\"" + lit + "\"\"\"")
    case Constant.Regex(lit) => Exp.Regex(lit)
    case Constant.RecordEmpty => Exp.RecordEmpty
    case Constant.Static => Exp.AsIs("Static")
  }

}
