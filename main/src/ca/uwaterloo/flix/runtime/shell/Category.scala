/*
 * Copyright 2022 Magnus Madsen
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
package ca.uwaterloo.flix.runtime.shell

import ca.uwaterloo.flix.language.ast.Ast
import ca.uwaterloo.flix.language.phase.Parser

/**
  * A common super-type for the syntactic category of a source code fragment.
  */
sealed trait Category

object Category {
  /**
    * Represents source code that is a declaration.
    */
  case object Decl extends Category

  /**
    * Represents source code that is an expression.
    */
  case object Expr extends Category

  /**
    * Represents source code whose category cannot be determined.
    */
  case object Unknown extends Category

  /**
    * Returns the syntactic category of the given source code string `s`.
    */
  def categoryOf(s: String): Category = {
    val input = Ast.Input.Text("<shell>", s, stable = false)
    val source = Ast.Source(input, s.toCharArray, stable = false)
    val parser = new Parser(source)

    val isDecl = parser.DeclarationEOI.run().isSuccess
    val isExpr = parser.ExpressionEOI.run().isSuccess

    if (isDecl && !isExpr)
      Category.Decl
    else if (!isDecl && isExpr)
      Category.Expr
    else
      Category.Unknown
  }

}
