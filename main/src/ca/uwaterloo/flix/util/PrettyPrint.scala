/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}
import ca.uwaterloo.flix.runtime.{Value, Model}


object PrettyPrint {

  def print(fqn: String, model: Model): Unit = {
    val sym = Symbol.mkTableSym(fqn)

    var found = false

    model.getRelations.get(fqn) match {
      case None => // nop
      case Some((attributes, rows)) =>
        val ascii = new AsciiTable().withCols(attributes: _*)
        for (row <- rows) {
          ascii.mkRow(row)
        }

        Console.println(fqn)
        ascii.write(System.out)
        Console.println()
        Console.println()
        found = true
    }

    model.getLattices.get(fqn) match {
      case None => // nop
      case Some((attributes, rows)) =>
        val ascii = new AsciiTable().withCols(attributes: _*)
        for (row <- rows) {
          ascii.mkRow(row)
        }

        Console.println(fqn)
        ascii.write(System.out)
        Console.println()
        Console.println()
        found = true
    }

    if (!found)
      Console.println("No such name: " + fqn)
  }

}
