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

package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst, Type}
import ca.uwaterloo.flix.language.fmt.FormatType

/**
  * Builds a generated Flix-side test driver for non-JVM targets.
  *
  * The driver:
  *   - creates per-module wrappers in the same namespace as the original tests,
  *   - runs tests sequentially,
  *   - reports PASS/FAIL/SKIP in a target-independent way,
  *   - and throws at the end if any test failed, so the host process exits non-zero.
  */
object ProjectTestDriver {

  val ModuleName: String = "FlixProjectTests"
  val ResultAliasName: String = "ProjectTestResult"
  val ResultAliasFqn: String = s"$ModuleName.$ResultAliasName"
  val EntryPointFqn: String = s"$ModuleName.main"
  val EntryPointSym: Symbol.DefnSym = Symbol.mkDefnSym(EntryPointFqn)

  case class ProjectTestDef(sym: Symbol.DefnSym, eff: Type, isSkip: Boolean)

  def collectProjectTests(root: TypedAst.Root): List[ProjectTestDef] =
    root.defs.valuesIterator
      .filter(_.spec.ann.isTest)
      .map(d => ProjectTestDef(d.sym, d.spec.eff, d.spec.ann.isSkip))
      .toList
      .sortBy(_.sym.toString)

  def mkDriverSource(tests: List[ProjectTestDef], banner: String = "project-tests")(implicit flix: Flix): String = {
    val wrappers = mkDriverWrappers(tests)
    val main = mkDriverMain(tests, banner)

    s"""
       |mod $ModuleName {
       |    pub type alias $ResultAliasName = (Int32, String)
       |}
       |
       |$wrappers
       |
       |mod $ModuleName {
       |$main
       |}
       |""".stripMargin.trim + "\n"
  }

  private def mkDriverWrappers(tests: List[ProjectTestDef])(implicit flix: Flix): String = {
    val byNs = tests.groupBy(_.sym.namespace).toList.sortBy(_._1.mkString("."))
    val sb = new StringBuilder

    for ((ns, defs) <- byNs) {
      if (ns.nonEmpty) sb.append(s"mod ${ns.mkString(".")} {\n")

      for ((test, index) <- defs.zipWithIndex) {
        val wrapperEff = liftedTestEff(test.eff)
        val wrapperEffText = FormatType.formatType(wrapperEff, minimizeEffs = true)
        val wrapperName = wrapperNameFor(index)
        val localName = test.sym.text
        val fqn = (test.sym.namespace :+ test.sym.text).mkString(".")

        val body =
          if (test.isSkip) {
            """        (2, "")
              |""".stripMargin
          } else {
            s"""        try {
               |            let _ = Assert.runWithIO(() -> $localName());
               |            (0, "")
               |        } catch {
               |            case exn: String => (1, Exn.payloadAs(exn))
               |            case _: Exn => (1, "Unhandled exception in $fqn")
               |        }
               |""".stripMargin
          }

        sb.append(s"    pub def $wrapperName(): $ResultAliasFqn \\ $wrapperEffText = {\n")
        sb.append(body)
        sb.append("    }\n\n")
      }

      if (ns.nonEmpty) sb.append("}\n\n")
    }

    sb.toString()
  }

  private def mkDriverMain(tests: List[ProjectTestDef], banner: String)(implicit flix: Flix): String = {
    val mainEff = Type.mkUnion(Type.IO :: tests.map(t => liftedTestEff(t.eff)).distinct, SourceLocation.Unknown)
    val mainEffText = FormatType.formatType(mainEff, minimizeEffs = true)

    val sb = new StringBuilder
    sb.append(s"    pub def main(): Unit \\ $mainEffText = {\n")
    sb.append(s"""    %%PRINTLN%%("Running ${tests.length} tests...");\n\n""")
    sb.append("    let (passed0, failed0, skipped0) = (0i32, 0i32, 0i32);\n")

    for (((test, stateIndex), wrapperIndex) <- tests.zipWithIndex.zipWithIndex) {
      val stateName = s"passed${stateIndex + 1}, failed${stateIndex + 1}, skipped${stateIndex + 1}"
      val prevPassed = s"passed$stateIndex"
      val prevFailed = s"failed$stateIndex"
      val prevSkipped = s"skipped$stateIndex"
      val wrapper = qualifiedWrapperName(test.sym.namespace, wrapperIndexForNamespace(tests, test, wrapperIndex))
      val fqn = test.sym.toString

      sb.append(s"    let ($stateName) = match $wrapper() {\n")
      sb.append(s"""        case (0i32, _) => { %%PRINTLN%%("  PASS  $fqn"); ($prevPassed + 1i32, $prevFailed, $prevSkipped) }\n""")
      sb.append(s"""        case (1i32, msg) => { %%PRINTLN%%("  FAIL  $fqn (${"$"}{String.trim(msg)})"); ($prevPassed, $prevFailed + 1i32, $prevSkipped) }\n""")
      sb.append(s"""        case (2i32, _) => { %%PRINTLN%%("  SKIP  $fqn (SKIPPED)"); ($prevPassed, $prevFailed, $prevSkipped + 1i32) }\n""")
      sb.append(s"""        case _ => { %%PRINTLN%%("  FAIL  $fqn (invalid generated test result)"); ($prevPassed, $prevFailed + 1i32, $prevSkipped) }\n""")
      sb.append("    };\n")
    }

    val last = tests.length
    sb.append(s"""\n    %%PRINTLN%%("Passed: ${"$"}{passed$last}, Failed: ${"$"}{failed$last}. Skipped: ${"$"}{skipped$last}.");\n""")
    sb.append(s"        if (failed$last == 0i32) () else throw Exn.mk(failed$last)\n")
    sb.append("    }\n")
    sb.toString()
  }

  private def wrapperNameFor(index: Int): String =
    s"runProjectTestGenerated$index"

  private def liftedTestEff(eff: Type): Type =
    Type.mkUnion(Type.mkDifference(eff, Type.Assert, SourceLocation.Unknown), Type.IO, SourceLocation.Unknown)

  private def wrapperIndexForNamespace(all: List[ProjectTestDef], current: ProjectTestDef, absoluteIndex: Int): Int =
    all.take(absoluteIndex + 1).count(_.sym.namespace == current.sym.namespace) - 1

  private def qualifiedWrapperName(ns: List[String], localIndex: Int): String =
    if (ns.isEmpty) wrapperNameFor(localIndex)
    else s"${ns.mkString(".")}.${wrapperNameFor(localIndex)}"
}
