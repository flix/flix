/*
 * Copyright 2024 Holger Dal Mogensen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}

import java.util.Objects

case object TraitConstraint {
  /**
    * Represents the head (located class) of a type constraint.
    */
  case class Head(sym: Symbol.TraitSym, loc: SourceLocation)
}

/**
  * Represents that the type `arg` must belong to trait `sym`.
  */
case class TraitConstraint(head: TraitConstraint.Head, arg: Type, loc: SourceLocation) {
  override def equals(o: Any): Boolean = o match {
    case that: TraitConstraint =>
      this.head.sym == that.head.sym && this.arg == that.arg
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(head.sym, arg)
}
