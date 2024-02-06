/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.language.ast.{Kind, Level, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.unification.Substitution


sealed trait TypingConstraint {

  lazy val index: (Int, Int, Int) = this match {
    case TypingConstraint.Equality(tvar1: Type.Var, Type.Pure, _, _) => (0, 0, 0)
    case TypingConstraint.Equality(Type.Pure, tvar2: Type.Var, _, _) => (0, 0, 0)
    case TypingConstraint.Equality(tvar1: Type.Var, tvar2: Type.Var, _, _) if tvar1 != tvar2 => (0, 0, 0)
    case TypingConstraint.Purification(sym, eff1, eff2, level, prov, nested, loc) => (0, 0, 0)
    case TypingConstraint.EffPurification(sym, eff1, eff2, level, prov, nested, loc) => (0, 0, 0)
    case TypingConstraint.Equality(tpe1, tpe2, prov, loc) =>
      val tvars = (tpe1.typeVars ++ tpe2.typeVars)
      val effTvars = tvars.filter(_.kind == Kind.Eff)
      (1, effTvars.size, tvars.size)
    case TypingConstraint.Class(sym, tpe, loc) => (2, 0, 0)
  }

  override def toString: String = this match {
    case TypingConstraint.Equality(tpe1, tpe2, prov, loc) => s"$tpe1 ~ $tpe2"
    case TypingConstraint.Class(sym, tpe, loc) => s"$sym[$tpe]"
    case TypingConstraint.Purification(sym, eff1, eff2, level, prov, nested, loc) => s"$eff1 ~ ($eff2)[$sym ↦ Pure] ∧ $nested"
    case TypingConstraint.EffPurification(sym, eff1, eff2, level, prov, nested, loc) => s"$eff1 ~ ($eff2)[$sym ↦ Pure] ∧ $nested"
  }

  def numVars: Int = this match {
    case TypingConstraint.Equality(tpe1, tpe2, prov, loc) => tpe1.typeVars.size + tpe2.typeVars.size
    case TypingConstraint.Class(sym, tpe, loc) => tpe.typeVars.size
    case TypingConstraint.Purification(sym, eff1, eff2, level, prov, nested, loc) => eff1.typeVars.size + eff2.typeVars.size
    case TypingConstraint.EffPurification(sym, eff1, eff2, level, prov, nested, loc) => eff1.typeVars.size + eff2.typeVars.size
  }

  private def toSubDot: String = this match {
    case TypingConstraint.Equality(tpe1, tpe2, prov, loc) => s"""$dotId [label = "$tpe1 ~ $tpe2"];"""
    case TypingConstraint.Class(sym, tpe, loc) => s"""$dotId [label = "$sym[$tpe]"];"""
    case TypingConstraint.Purification(sym, eff1, eff2, level, prov, nested, loc) =>
      val header = s"""$dotId [label = "$eff1 ~ ($eff2)[$sym ↦ Pure]"];"""
      val children = nested.map(_.toSubDot)
      val edges = nested.map { child => s"$dotId -> ${child.dotId};" }
      (header :: children ::: edges).mkString("\n")
    case TypingConstraint.EffPurification(sym, eff1, eff2, level, prov, nested, loc) =>
      val header = s"""$dotId [label = "$eff1 ~ ($eff2)[$sym ↦ Pure]"];"""
      val children = nested.map(_.toSubDot)
      val edges = nested.map { child => s"$dotId -> ${child.dotId};" }
      (header :: children ::: edges).mkString("\n")
  }

  private def dotId: Int = System.identityHashCode(this)
}

object TypingConstraint {

  // tpe1 ~ tpe2
  case class Equality(tpe1: Type, tpe2: Type, prov: Provenance, loc: SourceLocation) extends TypingConstraint

  // sym[tpe]
  case class Class(sym: Symbol.ClassSym, tpe: Type, loc: SourceLocation) extends TypingConstraint

  // eff1 ~ eff2[symˡᵉᵛᵉˡ ↦ Pure] ∧ nested
  case class Purification(sym: Symbol.KindedTypeVarSym, eff1: Type, eff2: Type, level: Level, prov: Provenance, nested: List[TypingConstraint], loc: SourceLocation) extends TypingConstraint

  // eff1 ~ eff2[symˡᵉᵛᵉˡ ↦ Pure] ∧ nested
  case class EffPurification(sym: Symbol.EffectSym, eff1: Type, eff2: Type, level: Level, prov: Provenance, nested: List[TypingConstraint], loc: SourceLocation) extends TypingConstraint

  def toDot(constrs: List[TypingConstraint]): String = {
    val contents = constrs.map(_.toSubDot)
    val edges = constrs.map { constr => s"root -> ${constr.dotId};" }
    format((
      "digraph Constraints {" ::
        "rankdir = \"LR\";" ::
        "node [shape=\"box\"];" ::
        contents :::
        edges :::
        List("}")
      ).mkString("\n"))
  }

  def toDotWithSubst(constrs: List[TypingConstraint], subst: Substitution): String = {
    val contents = constrs.map(_.toSubDot)
    val edges = constrs.map { constr => s"root -> ${constr.dotId};" }
    val substPart = subst.m.toList.sortBy(_._1).flatMap {
      case (sym, tpe) =>
        val tvar = Type.Var(sym, SourceLocation.Unknown)
        val from = System.nanoTime()
        val to = System.nanoTime()
        List(
          s"""$from [label = "$tvar"];""",
          s"""$to [label = "$tpe"];""",
          s"$from -> $to;"
        )
    }
    val substSubgraph =
      "subgraph cluster_Subst {" ::
        "label = \"Substitution\";" ::
        "style = \"filled\";" ::
        "invisL [style=\"invis\"];" :: // these invisible boxes help align the substitution
        "invisR [style=\"invis\"];" ::
        "invisL -> invisR [style=\"invis\"];" ::
        substPart :::
        List("}")

    val constrsSubgraph =
      "subgraph Constraints {" ::
        "root" ::
        contents :::
        edges :::
        List("}")

    format((
      "digraph Constraints {" ::
        "rankdir = \"LR\";" ::
        "node [shape=\"box\"];" ::
        substSubgraph :::
        constrsSubgraph :::
        "invisR -> root [style=\"invis\"];" ::
        List("}")
      ).mkString("\n"))
  }

  private def format(s: String): String = s.replace("\\", "\\\\")

  sealed trait Provenance

  object Provenance {

    /**
      * The constraint indicates that the left type is the expected type, while the right type is the actual type.
      */
    object ExpectLeft extends Provenance

    /**
      * The constraint indicates that the left type is the expected type of the `n`th argument to a function.
      */
    case class ExpectLeftArgument(sym: Symbol, num: Int) extends Provenance

    /**
      * The constraint indicates that the types must match.
      */
    object Match extends Provenance
  }
}
