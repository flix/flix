/*
 * Copyright 2021 Matthew Lutze
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

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.{EliminatedBy, IntroducedBy}
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import org.scalatest.FunSuite

class TestAstConditions extends FunSuite with TestUtils {

  private object FirstPhase extends Phase[AstNode, AstNode] {
    override def run(input: AstNode)(implicit flix: Flix): Validation[AstNode, CompilationError] = Validation.Success(input)
  }

  private object CondPhase extends Phase[AstNode, AstNode] {
    override def run(input: AstNode)(implicit flix: Flix): Validation[AstNode, CompilationError] = Validation.Success(input)
  }

  private sealed trait AstNode

  private case object SomeAstLeaf extends AstNode

  private case class SomeAstNode(node: AstNode, nodes: List[AstNode]) extends AstNode

  @IntroducedBy[CondPhase.type]
  private case class ClassIntroducedByCondPhase(x: Int, y: String) extends AstNode

  @EliminatedBy[CondPhase.type]
  private case class ClassEliminatedByCondPhase(x: Int, y: String) extends AstNode

  @IntroducedBy[CondPhase.type]
  private case object ObjectIntroducedByCondPhase extends AstNode

  @EliminatedBy[CondPhase.type]
  private case object ObjectEliminatedByCondPhase extends AstNode


  test("IntroducedBy.Err.01") {
    val ast = ClassIntroducedByCondPhase(0, "")
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
    }
  }

  test("IntroducedBy.Err.02") {
    val ast = ObjectIntroducedByCondPhase
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
    }
  }

  test("IntroducedBy.Err.03") {
    val ast = SomeAstNode(ObjectIntroducedByCondPhase, Nil)
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
    }
  }

  test("IntroducedBy.Err.04") {
    val ast = SomeAstNode(SomeAstLeaf, ObjectIntroducedByCondPhase :: Nil)
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
    }
  }

  test("IntroducedBy.Ok.01") {
    val ast = ClassIntroducedByCondPhase(0, "")
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
  }

  test("IntroducedBy.Ok.02") {
    val ast = ObjectIntroducedByCondPhase
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
  }

  test("IntroducedBy.Ok.03") {
    val ast = SomeAstNode(ObjectIntroducedByCondPhase, Nil)
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
  }

  test("IntroducedBy.Ok.04") {
    val ast = SomeAstNode(SomeAstLeaf, ObjectIntroducedByCondPhase :: Nil)
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
  }


  test("EliminatedBy.Err.01") {
    val ast = ClassEliminatedByCondPhase(0, "")
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
    }
  }

  test("EliminatedBy.Err.02") {
    val ast = ObjectEliminatedByCondPhase
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
    }
  }

  test("EliminatedBy.Err.03") {
    val ast = SomeAstNode(ObjectEliminatedByCondPhase, Nil)
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
    }
  }

  test("EliminatedBy.Err.04") {
    val ast = SomeAstNode(SomeAstLeaf, ObjectEliminatedByCondPhase :: Nil)
    assertThrows[InternalCompilerException] {
      AstConditions.checkAstAfterPhases(ast, FirstPhase :: CondPhase :: Nil)
    }
  }

  test("EliminatedBy.Ok.01") {
    val ast = ClassEliminatedByCondPhase(0, "")
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
  }

  test("EliminatedBy.Ok.02") {
    val ast = ObjectEliminatedByCondPhase
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
  }

  test("EliminatedBy.Ok.03") {
    val ast = SomeAstNode(ObjectEliminatedByCondPhase, Nil)
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
  }

  test("EliminatedBy.Ok.04") {
    val ast = SomeAstNode(SomeAstLeaf, ObjectEliminatedByCondPhase :: Nil)
    AstConditions.checkAstAfterPhases(ast, FirstPhase :: Nil)
  }
}
