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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{Kind, Name, SourcePosition, Symbol, Type}

package object phase {

  // TODO: Cleanup, possibly unify with the Flix class?

  class GenSym() {

    private var number: Int = 0

    def freshId(): Int = {
      number = number + 1
      number
    }

    def freshDefn(ns: List[String]): Symbol.Resolved = {
      number = number + 1
      Symbol.Resolved.mk(ns.init ::: ns.last + "$" + number :: Nil)
    }

    def fresh2(): Name.Ident = fresh2("tmp")

    def fresh2(prefix: String): Name.Ident = {
      number = number + 1
      Name.Ident(SourcePosition.Unknown, prefix + "$" + number, SourcePosition.Unknown)
    }

    def freshTypeVar(): Type.Var = Type.Var(freshId().toString, Kind.Star) // TODO: Should require kind?

  }

}
