/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.{CompilerConstants, Flix, FlixEvent, FlixListener}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.SetHasAsScala

class TestMonomorpher extends AnyFunSuite with TestUtils {

  /**
    * Compiles `input` and returns the qualified names of the defs that are called after
    * monomorphization.
    *
    * A def that is called after monomorphization is either inlined at a call site by the optimizer
    * or emitted as a class by the backend. A def that is not called is neither.
    */
  private def calledDefs(input: String): Set[String] = {
    val names = ConcurrentHashMap.newKeySet[String]()
    val flix = new Flix().setOptions(Options.TestWithLibMin)
    flix.addSource(CompilerConstants.VirtualTestFile, input, sctx)
    flix.addListener(new FlixListener {
      override def notify(e: FlixEvent): Unit = e match {
        case FlixEvent.InlinedDef(sym) => names.add((sym.namespace :+ sym.text).mkString("."))
        case FlixEvent.EmittedClass(sym, _) => names.add((sym.namespace :+ sym.text).mkString("."))
        case _ => ()
      }
    })
    flix.compile().unsafeGet
    names.asScala.toSet
  }

  test("PrimitiveWrapper.Unary.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def neg(x: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def neg(x: Int32): Int32 = %%INT32_NEG%%(x)
        |}
        |
        |def main(): Unit \ IO = println(Wrap.neg(42))
        |""".stripMargin
    assert(!calledDefs(input).contains("neg"))
  }

  test("PrimitiveWrapper.Unary.02") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def neg(x: a): a
        |}
        |
        |instance Wrap[Bool] {
        |    pub def neg(x: Bool): Bool = not x
        |}
        |
        |def main(): Unit \ IO = println(Wrap.neg(true))
        |""".stripMargin
    assert(!calledDefs(input).contains("neg"))
  }

  test("PrimitiveWrapper.Binary.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def minus(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def minus(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
        |}
        |
        |def main(): Unit \ IO = println(Wrap.minus(10, 3))
        |""".stripMargin
    assert(!calledDefs(input).contains("minus"))
  }

  test("PrimitiveWrapper.Binary.02") {
    // The signature is used as a function.
    val input =
      """
        |trait Wrap[a] {
        |    pub def minus(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def minus(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
        |}
        |
        |def twice(f: Int32 -> Int32 -> Int32, x: Int32): Int32 = f(f(x, 1), 1)
        |
        |def main(): Unit \ IO = println(twice(Wrap.minus, 10))
        |""".stripMargin
    assert(!calledDefs(input).contains("minus"))
  }

  test("PrimitiveWrapper.Binary.03") {
    // The signature is used through a trait constraint.
    val input =
      """
        |trait Wrap[a] {
        |    pub def minus(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def minus(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
        |}
        |
        |def generic(x: a, y: a): a with Wrap[a] = Wrap.minus(x, y)
        |
        |def main(): Unit \ IO = println(generic(10, 3))
        |""".stripMargin
    assert(!calledDefs(input).contains("minus"))
  }

  test("PrimitiveWrapper.Binary.04") {
    // Only the instance that wraps a primitive operation is replaced.
    val input =
      """
        |trait Wrap[a] {
        |    pub def minus(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def minus(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
        |}
        |
        |instance Wrap[String] {
        |    pub def minus(x: String, y: String): String = "${x}-${y}"
        |}
        |
        |def main(): Unit \ IO = println(Wrap.minus("abc", "def"))
        |""".stripMargin
    assert(calledDefs(input).contains("minus"))
  }

  test("NotPrimitiveWrapper.Flipped.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def minus(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def minus(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(y, x)
        |}
        |
        |def main(): Unit \ IO = println(Wrap.minus(10, 3))
        |""".stripMargin
    assert(calledDefs(input).contains("minus"))
  }

  test("NotPrimitiveWrapper.Twice.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def plus(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def plus(x: Int32, _y: Int32): Int32 = %%INT32_ADD%%(x, x)
        |}
        |
        |def main(): Unit \ IO = println(Wrap.plus(10, 3))
        |""".stripMargin
    assert(calledDefs(input).contains("plus"))
  }

  test("NotPrimitiveWrapper.Unused.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def neg(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def neg(x: Int32, _y: Int32): Int32 = %%INT32_NEG%%(x)
        |}
        |
        |def main(): Unit \ IO = println(Wrap.neg(10, 3))
        |""".stripMargin
    assert(calledDefs(input).contains("neg"))
  }

  test("NotPrimitiveWrapper.Constant.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def inc(x: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    pub def inc(x: Int32): Int32 = %%INT32_ADD%%(x, 1)
        |}
        |
        |def main(): Unit \ IO = println(Wrap.inc(42))
        |""".stripMargin
    assert(calledDefs(input).contains("inc"))
  }

  test("NotPrimitiveWrapper.And.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def conj(x: a, y: a): a
        |}
        |
        |instance Wrap[Bool] {
        |    pub def conj(x: Bool, y: Bool): Bool = x and y
        |}
        |
        |def main(): Unit \ IO = println(Wrap.conj(true, false))
        |""".stripMargin
    assert(calledDefs(input).contains("conj"))
  }

  test("NotPrimitiveWrapper.Or.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def disj(x: a, y: a): a
        |}
        |
        |instance Wrap[Bool] {
        |    pub def disj(x: Bool, y: Bool): Bool = x or y
        |}
        |
        |def main(): Unit \ IO = println(Wrap.disj(true, false))
        |""".stripMargin
    assert(calledDefs(input).contains("disj"))
  }

  test("NotPrimitiveWrapper.DontInline.01") {
    val input =
      """
        |trait Wrap[a] {
        |    pub def minus(x: a, y: a): a
        |}
        |
        |instance Wrap[Int32] {
        |    @DontInline
        |    pub def minus(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
        |}
        |
        |def main(): Unit \ IO = println(Wrap.minus(10, 3))
        |""".stripMargin
    assert(calledDefs(input).contains("minus"))
  }

}
