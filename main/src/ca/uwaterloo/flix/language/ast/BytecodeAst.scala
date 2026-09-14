/*
 * Copyright 2025 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.phase.jvm.JvmClass

import java.lang.constant.ClassDesc

object BytecodeAst {

  /**
    * An AST after JVM bytecode generation.
    *
    * @param tests all tests refer to functions of type `Unit -> t`
    * @param main  main (if present) refers to a function of type `Array[String] -> Unit`
    */
  case class Root(
                   classes: Map[ClassDesc, JvmClass],
                   tests: Map[Symbol.DefnSym, Test],
                   main: Option[Def],
                   sources: Map[Source, SourceLocation]
                 )

  case class Def(className: ClassDesc, methodName: String)

  case class Test(className: ClassDesc, methodName: String, isSkip: Boolean)

}
