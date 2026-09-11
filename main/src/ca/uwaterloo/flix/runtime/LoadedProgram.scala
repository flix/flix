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
