/*
 * Copyright 2026 Din Jakupi
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
package ca.uwaterloo.flix.tools.fmt

import org.scalatest.Ignore

/**
  * Correctness tests for the Flix code formatter.
  *
  * The correctness properties checked here are part of the canonical set
  * required of a source code formatter. With `p`, `f` and `w` as defined in
  * [[TestFormatterCommon]], they are:
  *
  *   1. Can format:          forall s in S,  f(p(s)) is defined
  *   2. Idempotency:         forall c in C,  f(p(f(c))) = f(c)
  *   3. Non-destructiveness: forall c in C,  w(c) = w(p(f(c)))
  *
  * Each property is run on both corpora: the standard library and the `examples`.
  */
//@Ignore
class TestFormatterCorrectness extends TestFormatterCommon {

  /**
    * Property 1 -- Can format: `forall s in S, f(p(s)) is defined`.
    *
    * The formatter produces a non-empty string for every [[SyntaxTree]] tree the parser produces.
    */
  private def checkCanFormat(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val tree = sample.reparse(sample.content).tree
      PrettyPrinter.format(tree) match {
        case Some(formatted) => assert(formatted.nonEmpty, s"Formatter produced empty output for ${sample.path}")
        case None => fail(s"Formatter produced no output for ${sample.path}")
      }
    }
  }

  /**
    * Property 2 -- Idempotency: `forall c in C, f(p(f(c))) = f(c)`.
    *
    * Applying the formatter to its own output yields the same output.
    * The inner `p` accounts for the fact that the formatter produces a string.
    */
  private def checkIdempotency(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val tree1 = sample.reparse(sample.content).tree
      val once = formatOrFail(tree1)
      val tree2 = sample.reparse(once).tree
      val twice = formatOrFail(tree2)
      assert(once == twice,
        s"Formatter is not idempotent for ${sample.path}:\n${firstDivergence(once, twice)}")
    }
  }

  /**
    * Property 3 -- Non-destructiveness: `forall c in C. shape(w(c)) = shape(w(p(f(c))))`.
    *
    * The formatter must not change the shape of the [[WeededAst]].
    * This is  checked by comparing the weeded compilation unit of the original source
    * against that of the reparsed formatted output, at the kind level only.
    */
  private def checkNonDestructive(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val before = sample.reparse(sample.content)
      val formatted = formatOrFail(before.tree)
      val after = sample.reparse(formatted)
      assert(sameShape(before.weeded, after.weeded),
        s"Formatter changed the AST shape for ${sample.path}")
    }
  }

  /**
    * Checks if two [[WeededAst]] have the same shape.
    * Meaning that they have the same structure of nodes, but not necessarily the same content.
    * This is used to check the non-destructiveness property of the formatter.
    *
    * TODO: This is a simple check, therefore, find a more robust way to check for the AST integrity.
    */
  private def sameShape(a: Any, b: Any): Boolean = (a, b) match {
    case (x: Iterable[_], y: Iterable[_]) =>
      // Two collections have the same shape if they have the same length
      x.size == y.size && x.iterator.zip(y.iterator).forall { case (p, q) => sameShape(p, q) }
    case (x: Product, y: Product) =>
      // Two case classes have the same shape if they have the same string prefix.
      x.productPrefix == y.productPrefix &&
        x.productIterator.zip(y.productIterator).forall { case (p, q) => sameShape(p, q) }
    case _ => true
  }

  test("PrettyPrinter: can format (examples)") {
    checkCanFormat(ExampleSamples)
  }
  test("PrettyPrinter: can format (stdlib)") {
    checkCanFormat(StdlibSamples)
  }

  test("PrettyPrinter: idempotency (examples)") {
    checkIdempotency(ExampleSamples)
  }
  test("PrettyPrinter: idempotency (stdlib)") {
    checkIdempotency(StdlibSamples)
  }

  test("PrettyPrinter: non-destructiveness (examples)") {
    checkNonDestructive(ExampleSamples)
  }
  test("PrettyPrinter: non-destructiveness (stdlib)") {
    checkNonDestructive(StdlibSamples)
  }
}
