/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type}

/** A type argument that flows into a `MonoVar`. */
sealed trait MonoArg

object MonoArg {
  /**
    * The i'th type parameter slot belonging to a specific MonoVar.
    */
  case class Param(v: MonoVar, index: Int) extends MonoArg

  /**
    * A type the solver does not decompose further. Usually ground, but can also be a type
    * variable the solver deliberately does not track (e.g. a local region var).
    */
  case class Const(tpe: Type) extends MonoArg

  /**
    * A type constructor applied to symbolic mono-arguments.
    * `tycon` is itself a MonoArg so higher-kinded type params can appear as the head.
    */
  case class App(tycon: MonoArg, args: List[MonoArg]) extends MonoArg

  /**
    * An associated type applied to a symbolic mono-argument, e.g. `Collection.Elm[a]` becomes
    * `Assoc(Elm, Param(v, i))`.
    * `kind` and `loc` are stored so the solver can reconstruct `Type.AssocType` for reduction.
    */
  case class Assoc(sym: Symbol.AssocTypeSym, arg: MonoArg, kind: Kind, loc: SourceLocation) extends MonoArg

  /** Returns every `(MonoVar, index)` pair referenced by a `Param` inside `arg`, however deeply wrapped. */
  def collectParams(arg: MonoArg): List[(MonoVar, Int)] = arg match {
    case MonoArg.Const(_)          => Nil
    case MonoArg.Param(v, i)       => List((v, i))
    case MonoArg.App(tycon, args)  => collectParams(tycon) ++ args.flatMap(collectParams)
    case MonoArg.Assoc(_, a, _, _) => collectParams(a)
  }
}
