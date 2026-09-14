/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.language.ast.Symbol

/**
  * A monomorphization target-variable (def/enum/sig/struct/restrictable-enum) whose concrete
  * instantiation the solver determines, then substitutes back wherever `MonoArg.Param`
  * references it.
  */
private[monomorph2] sealed trait MonoVar

private[monomorph2] object MonoVar {
  case class Def(sym: Symbol.DefnSym) extends MonoVar

  case class Enum(sym: Symbol.EnumSym) extends MonoVar

  case class Sig(sym: Symbol.SigSym) extends MonoVar

  case class RestrictableEnum(sym: Symbol.RestrictableEnumSym) extends MonoVar

  case class Struct(sym: Symbol.StructSym) extends MonoVar
}
