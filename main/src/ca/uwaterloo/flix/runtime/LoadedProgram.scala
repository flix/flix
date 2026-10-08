/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.Symbol

/**
  * A compiled Flix program whose classes have been defined in the JVM.
  *
  * Obtained from [[JvmLoader.load]].
  *
  * @param main  the reflected main function, if present. Takes the program arguments.
  * @param tests the reflected test functions in the program.
  */
case class LoadedProgram(main: Option[Array[String] => Unit], tests: Map[Symbol.DefnSym, TestFn])
