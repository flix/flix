/*
 * Copyright 2026 Werner Stein
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
package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{Options, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

/**
  * Verifies the end-to-end guarantee content-addressed naming exists for: the set of
  * generated JVM class names is stable across repeated compiles of identical source, and
  * unaffected by an unrelated addition elsewhere in the source.
  *
  * `TestStableName`, `TestSpecializationKey`, and `TestErasureKey` cover the key-rendering
  * logic in isolation; nothing exercises the actual outcome through the real
  * `Specialization`, `LambdaLift`, and `Eraser` phases together, on real compiled output.
  * This does, by compiling a program that exercises specialized defs, enum/struct
  * specialization, instance members, derived defs, and lifted closures, and comparing the
  * generated class names across compiles.
  */
class TestArtifactStability extends AnyFunSuite {

  private implicit val sctx: SecurityContext = SecurityContext.Unrestricted

  /** Exercises specialized defs, enum specialization, instance members, a derived instance, and a lifted closure. */
  private val Source: String =
    """
      |enum Wrapper[a] {
      |    case Wrapper(a)
      |}
      |
      |enum Color with Eq, ToString {
      |    case Red, Green, Blue
      |}
      |
      |trait Describable[a] {
      |    pub def describe(x: a): String
      |}
      |
      |instance Describable[Int32] {
      |    pub def describe(x: Int32): String = "Int32(${x})"
      |}
      |
      |instance Describable[String] {
      |    pub def describe(x: String): String = "String(${x})"
      |}
      |
      |def unbox(b: Wrapper[a]): a = match b {
      |    case Wrapper.Wrapper(x) => x
      |}
      |
      |def liftedClosures(n: Int32): (Int32 -> Int32, Int32 -> Int32) =
      |    let f = x -> x + n;
      |    let g = y -> y * n;
      |    (f, g)
      |
      |def main(): Unit \ IO = {
      |    let bi = unbox(Wrapper.Wrapper(1));
      |    let bs = unbox(Wrapper.Wrapper("s"));
      |    let c = Color.Red;
      |    let (f, g) = liftedClosures(3);
      |    println("${Describable.describe(bi)}/${Describable.describe(bs)}/${c}/${f(1)}/${g(1)}")
      |}
      |""".stripMargin

  /** Compiles `source` and returns the binary names of every generated class. */
  private def classNames(source: String): Set[String] = {
    val flix = new Flix()
    flix.setOptions(Options.DefaultTest.copy(incremental = false))
    flix.addVirtualPath(Paths.get("Test.flix"), source)
    flix.compile().toResult match {
      case Result.Ok(result) => result.getClasses.keySet.map(_.toBinaryName)
      case Result.Err(errors) => fail(errors.map(_.summary).mkString("\n"))
    }
  }

  test("repeatedCompile.01") {
    // Two compiles of identical source must agree on every generated class name.
    val first = classNames(Source)
    val second = classNames(Source)
    // Sanity check against a vacuous pass: TreeShaker1 prunes unreachable stdlib code, but
    // this program's dependency closure (println, string interpolation, Eq/ToString
    // derivation) alone still compiles to well over a hundred classes, so a suspiciously
    // small set means classNames broke, not that stability trivially held.
    assert(first.size > 50, s"expected dozens of classes at least, got ${first.size}")
    assert(first == second)
  }

  test("unrelatedEdit.01") {
    // An unrelated addition after the tested code must not rename any of its classes --
    // this is the specific failure mode (renumbering under an unrelated edit) content-
    // addressed naming exists to fix. Asserting on the classes present before the edit,
    // rather than set equality, since the edit legitimately adds one class of its own.
    val before = classNames(Source)
    val after = classNames(Source + "\ndef unrelatedAddition(): Int32 = 42\n")
    assert(before.size > 50, s"expected dozens of classes at least, got ${before.size}")
    val renamed = before -- after
    assert(renamed.isEmpty, s"classes renamed by an unrelated edit: $renamed")
  }

}
