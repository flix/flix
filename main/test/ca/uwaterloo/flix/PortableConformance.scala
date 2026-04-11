/*
 * Copyright 2026 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst, Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt.FormatType

/**
  * Helpers for building "portable conformance" drivers that can be compiled and executed
  * on non-JVM targets (LLVM-native, LLVM-wasm, ...).
  *
  * We avoid calling `@Test` defs directly from `main` because portable tests typically keep
  * those defs non-public. Instead, we generate per-module wrappers *in the same module*.
  */
object PortableConformance {

  case class PortableTestDef(sym: Symbol.DefnSym, eff: Type, returnsBool: Boolean)

  /**
    * Collects portable `@Test` defs in deterministic order.
    */
  def collectPortableTests(root: TypedAst.Root): List[PortableTestDef] = {
    root.defs.valuesIterator
      .filter(d => d.spec.ann.isTest && !d.spec.ann.isSkip)
      .filter(d => d.sym.namespace.startsWith(List("Test", "Portable")))
      .map { d =>
        val returnsBool = d.spec.retTpe match {
          case Type.Cst(TypeConstructor.Bool, _) => true
          case _ => false
        }
        PortableTestDef(sym = d.sym, eff = d.spec.eff, returnsBool = returnsBool)
      }
      .toList
      .sortBy(d => (d.sym.namespace.mkString("."), d.sym.text))
  }

  /**
    * Generates a Flix driver program that:
    *   - defines per-module wrappers to call the discovered `@Test` defs,
    *   - defines `main` that calls those wrappers in deterministic order.
    *
    * @param banner The suite label printed at start/done.
    */
  def mkDriverSource(tests: List[PortableTestDef], banner: String)(implicit flix: Flix): String = {
    val wrappers = mkDriverWrappers(tests)
    val main = mkDriverMain(tests, banner)

    s"""
       |mod Test {}
       |
       |$wrappers
       |
       |$main
       |""".stripMargin.trim + "\n"
  }

  private def mkDriverWrappers(tests: List[PortableTestDef])(implicit flix: Flix): String = {
    val wrapperName = "runPortableTestsGenerated"
    val byNs = tests.groupBy(_.sym.namespace).toList.sortBy(_._1.mkString("."))

    val sb = new StringBuilder
    for ((ns, defs) <- byNs) {
      val moduleName = ns.mkString(".")

      // We print the current test name before invocation, so every wrapper needs `IO`.
      val wrapperEff = Type.mkUnion(Type.IO :: defs.map(_.eff).distinct, SourceLocation.Unknown)
      val wrapperEffText = FormatType.formatType(wrapperEff, minimizeEffs = true)

      sb.append(s"mod $moduleName {\n")
      sb.append(s"    pub def $wrapperName(): Unit \\ $wrapperEffText = {\n")
      for (PortableTestDef(sym, _, returnsBool) <- defs.sortBy(_.sym.text)) {
        if (sym.id.nonEmpty) {
          // If we ever hit this, we need to revisit the @Test naming conventions (since `$` names
          // are not legal in source syntax and cannot be called from Flix code).
          throw new IllegalStateException(s"Unexpected @Test def symbol with a generated ID: $sym")
        }
        val testName = sym.text
        val fqn = (sym.namespace :+ sym.text).mkString(".")
        sb.append(s"""        %%PRINTLN%%("portable: running $fqn");\n""")
        if (returnsBool) {
          sb.append(s"        if ($testName()) () else bug!(\"Test failed: $fqn\");\n")
        } else {
          sb.append(s"        let _ = $testName();\n")
        }
      }
      sb.append("        ()\n")
      sb.append("    }\n")
      sb.append("}\n\n")
    }
    sb.toString()
  }

  private def mkDriverMain(tests: List[PortableTestDef], banner: String)(implicit flix: Flix): String = {
    val wrapperName = "runPortableTestsGenerated"
    val byNs = tests.groupBy(_.sym.namespace).toList.sortBy(_._1.mkString("."))

    // Main prints module headers, so it must include `IO` plus whatever effects are exercised by tests.
    val mainEff = Type.mkUnion(Type.IO :: tests.map(_.eff).distinct, SourceLocation.Unknown)
    val mainEffText = FormatType.formatType(mainEff, minimizeEffs = true)

    val sb = new StringBuilder
    sb.append(s"def main(): Unit \\ $mainEffText = {\n")
    sb.append(s"""    %%PRINTLN%%("$banner: start");\n\n""")
    for ((ns, _) <- byNs) {
      val moduleName = ns.mkString(".")
      sb.append(s"""    %%PRINTLN%%("portable: module $moduleName");\n""")
      sb.append(s"    let _ = $moduleName.$wrapperName();\n\n")
    }
    sb.append(s"""    %%PRINTLN%%("$banner: done");\n""")
    sb.append("    ()\n")
    sb.append("}\n")
    sb.toString()
  }
}

