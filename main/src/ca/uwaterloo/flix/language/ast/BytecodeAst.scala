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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}

object BytecodeAst {

  /**
    * An AST after JVM bytecode generation.
    *
    * @param tests all tests refer to functions of type `Unit -> t`
    * @param main  main (if present) refers to a function of type `Array[String] -> Unit`
    */
  case class Root(
                   classes: Map[JvmName, JvmClass],
                   tests: Map[Symbol.DefnSym, Test],
                   main: Option[Def],
                   sources: Map[Source, SourceLocation]
                 )

  case class Def(className: JvmName, methodName: String)

  case class Test(className: JvmName, methodName: String, isSkip: Boolean)

}
