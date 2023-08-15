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

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatType}

object TypePrinter {

  /**
    * Returns the [[DocAst.Type]] representation of `tpe`.
    */
  def print(tpe: Type): DocAst.Type = {
    // Temporarily use existing type formatting
    // try to unpack one arrow type for the sake of signatures
    // try to unpack one tuple type for the sake of enums
    // try to unpack one unit type for the sake of enums
    tpe.baseType match {
      case Type.Cst(TypeConstructor.Arrow(_), _) =>
        val args = tpe.arrowArgTypes
        val res = tpe.arrowResultType
        DocAst.Type.Arrow(args.map(printHelper), printHelper(res))
      case Type.Cst(TypeConstructor.Tuple(_), _) =>
        val args = tpe.typeArguments
        DocAst.Type.Tuple(args.map(printHelper))
      case Type.Cst(TypeConstructor.Unit, _) =>
        DocAst.Type.Unit
      case _ => printHelper(tpe)
    }
  }

  /** Format type as a string */
  private def printHelper(tpe: Type): DocAst.Type = {
    val formattedType = FormatType.formatTypeWithOptions(tpe, FormatOptions(ignorePur = true, ignoreEff = true, FormatOptions.VarName.NameBased))
    DocAst.Type.AsIs(formattedType)
  }

}
