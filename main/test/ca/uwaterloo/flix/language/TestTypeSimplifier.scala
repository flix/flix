/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypedAst}
import ca.uwaterloo.flix.language.phase.TypeSimplifier
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.language.phase.unification.EqualityEnv
import ca.uwaterloo.flix.util.{InternalCompilerException, Options}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class TestTypeSimplifier extends AnyFunSuite with TestUtils {

  test("TypeCheckWithSimplifier") {
    // Check that for all types, they are equivalent to their simplified type.

    // `TypeSimplifier.simplify` does not reduce associated types, so eqEnv can be left empty without invalidating equality.
    val eqEnv: EqualityEnv = EqualityEnv.empty
    // The flix object is largely unused when calling equivalent.
    val flix: Flix = new Flix().setOptions(Options.DefaultTest)

    for (tpe <- getSampleTypes) {
      val simplifiedType = TypeSimplifier.simplify(tpe)
      assert(ConstraintSolver2.equivalent(tpe, simplifiedType)(eqEnv, flix), s"\n$tpe\ndoes not unify with\n$simplifiedType")
    }
  }

  /** Returns a list of types to use for testing. */
  private def getSampleTypes: List[Type] = {
    // To find sample types, check the standard library and pick of the types in the AST.

    implicit val flix: Flix = new Flix().setOptions(Options.DefaultTest)

    val root = flix.check() match {
      case (Some(r), Nil) => r
      case (_, _) => throw InternalCompilerException("Standard library does not compile", SourceLocation.Unknown)
    }

    // Collect all the types of the AST. Use `consumeExpr` to have both compiler generated and user written types.
    val types = mutable.ArrayBuffer.empty[Type]
    Visitor.visitRoot(
      root,
      new Consumer {
        override def consumeExpr(exp: TypedAst.Expr): Unit = types.append(exp.tpe)
      },
      (_: SourceLocation) => true
    )
    types.toList
  }

}
