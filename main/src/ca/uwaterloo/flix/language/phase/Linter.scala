/*
 *  Copyright 2019 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{BinaryOperator, Symbol, TypedAst}
import ca.uwaterloo.flix.language.debug.FormatExpression
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, LinterError] = flix.phase("Linter") {
    for {
      codePatterns <- getCodePatterns(root)
    } yield {
      for (p <- codePatterns) {
        println(formatPattern(p))
      }
      println("total patterns: " + codePatterns.length)
      root
    }
  }


  /**
    * Returns all code patterns in the given AST `root`.
    */
  private def getCodePatterns(root: Root): Validation[List[CodePattern], LinterError] = {
    sequence(root.defs.values.collect {
      case defn if defn.ann.isTheorem => defn.exp match {
        case e: Expression.Universal => getCodePattern(e)
        case e => NotYetSupportedPattern.toSuccess
      }
    })
  }

  // TODO
  private def getCodePattern(exp0: Expression): Validation[CodePattern, LinterError] = exp0 match {
    case Expression.Universal(_, e, _, _) => getCodePattern(e)
    case Expression.Binary(BinaryOperator.Equal, exp1, exp2, _, _, _) =>
      mapN(getSymAndArgs(exp1)) {
        case (sym, args) => EqPattern(sym, args, exp2)
      }
    case _ => NotYetSupportedPattern.toSuccess
  }

  // TODO
  private def getSymAndArgs(exp0: Expression): Validation[(Symbol.DefnSym, List[Expression]), LinterError] = exp0 match {
    case Expression.Def(sym, _, _, _) => (sym, Nil).toSuccess
    case Expression.Apply(exp1, exp2, _, _, _) =>
      mapN(getSymAndArgs(exp1)) {
        case (sym, args) => (sym, args ::: exp2 :: Nil) // TODO: Use accumulator.
      }
    case Expression.Binary(_, _, _, _, _, _) => (null, Nil).toSuccess // TODO
    case e => (null, Nil).toSuccess // TODO
  }

  sealed trait CodePattern

  // TODO: Put in object

  // TODO: Probably need to universal variables, so that we can instantiate them...
  case class EqPattern(sym: Symbol.DefnSym, args: List[Expression], result: Expression) extends CodePattern

  case object NotYetSupportedPattern extends CodePattern

  def formatPattern(p: CodePattern): String = p match {
    case EqPattern(sym, args, result) => s"EqPattern($sym, ${args.map(FormatExpression.format)}, ${FormatExpression.format(result)}"
    case NotYetSupportedPattern => "NotYetSupportedPattern"
  }

}
