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

package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst

object ConstantPrinter {

  /**
    * Returns the [[DocAst]] representation of `cst`.
    */
  def print(cst: Constant): DocAst = cst match {
    case Constant.Unit => DocAst.Unit
    case Constant.Null => DocAst.AsIs("null")
    case Constant.Bool(lit) => DocAst.AsIs(lit.toString)
    case Constant.Char(lit) => DocAst.AsIs("'''" + lit.toString + "'''")
    case Constant.Float32(lit) => DocAst.AsIs(s"${lit}f32")
    case Constant.Float64(lit) => DocAst.AsIs(s"${lit}f64")
    case Constant.BigDecimal(lit) => DocAst.AsIs(s"${lit}ff")
    case Constant.Int8(lit) => DocAst.AsIs(s"${lit}i8")
    case Constant.Int16(lit) => DocAst.AsIs(s"${lit}i16")
    case Constant.Int32(lit) => DocAst.AsIs(s"${lit}i32")
    case Constant.Int64(lit) => DocAst.AsIs(s"${lit}i64")
    case Constant.BigInt(lit) => DocAst.AsIs(s"${lit}ii")
    case Constant.Str(lit) => DocAst.AsIs("\"\"\"" + lit + "\"\"\"")
  }

}
