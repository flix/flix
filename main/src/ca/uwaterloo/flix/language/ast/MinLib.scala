/*
 * Copyright 2021 Matthew Lutze
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

/**
  * An interface for symbols and names of members of the minimal library.
  */
object MinLib {
  trait Class {
    val sym: Symbol.ClassSym
  }

  abstract class TopLevelClass(name: String) extends Class {
    val sym: Symbol.ClassSym = new Symbol.ClassSym(Nil, name, SourceLocation.Unknown)
  }

  abstract class Sig(clazz: Class, name: String) {
    val sym: Symbol.SigSym = new Symbol.SigSym(clazz.sym, name, SourceLocation.Unknown)
  }

  object Boxable extends TopLevelClass("Boxable")
  object Eq extends TopLevelClass("Eq") {
    object Eq extends Sig(this, "eq")
    object Neq extends Sig(this, "neq")
  }
  object Foldable extends TopLevelClass("Foldable")
  object JoinLattice extends TopLevelClass("JoinLattice")
  object LowerBound extends TopLevelClass("LowerBound")
  object MeetLattice extends TopLevelClass("MeetLattice")
  object Order extends TopLevelClass("Order")
  object PartialOrder extends TopLevelClass("PartialOrder")
  object ToString extends TopLevelClass("ToString") {
    object ToString extends Sig(this, "toString")
  }

}
