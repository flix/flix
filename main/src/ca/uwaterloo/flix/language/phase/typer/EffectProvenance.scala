/*
 * Copyright 2025 Gagan Chandan
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.TypeConstructor
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.util.InternalCompilerException
import scala.collection.immutable.SortedSet

sealed trait SourceEffect {
  def equals(eff: SourceEffect): Boolean = eff match {
    case SourceEffect.Pure => this == SourceEffect.Pure
    case SourceEffect.Effect(sym) => this match {
      case SourceEffect.Pure => false
      case SourceEffect.Effect(thisSym) => thisSym == sym
    }
  }
}

object SourceEffect {
  case object Pure extends SourceEffect
  case class Effect(sym: Symbol.EffSym) extends SourceEffect
}

object EffectProvenance {
  // Whether to enable effect provenance debugging.
  private val enableEffectProvenance: Boolean = true

  // First, we find all equality constraints that have a provenance of type
  // Provenance.Match with a concrete effect on one side and an effect variable
  // on the other side.
  //
  // For each such variable and set of concrete effects , we track a path
  // through the constraints until we reach a constraint that has a
  // provenance of type Provenance.ExpectEffect and an effect in the
  // in the concrete set is not in the expected effects.
  def debug(constrs0: List[TypeConstraint]): Unit = {
    if (!enableEffectProvenance) {
      return
    }
    constrs0.foreach { constr =>
      constr match {
        // Kick off path searching from source constraints.
        case TypeConstraint.Equality(_, _, Provenance.Source(eff1, eff2, loc)) => {
          eff2 match {
            case Type.Pure => findPath(eff1, SourceEffect.Pure, constr :: Nil, constrs0.filterNot(_ == constr))
            case _ => {
              val (add, _) = findConcreteEffects(eff2)
              // For each concrete effect introduced by the source effect,
              // we begin the search for a path.
              eff2.effects.filter(add.contains(_)).foreach { eff =>
                findPath(eff1, SourceEffect.Effect(eff), constr :: Nil, constrs0.filterNot(_ == constr))
              }
            }
          }
        }
        case _ => ()
      }
    }
  }

  // Finds a path given the effect variable `eff`,
  // the set of concrete effects `concreteEff`, the current path `path`,
  // and the list of all constraints `constrs`.
  // If Provenance.ExpectEffect is found, it checks if all the concrete
  // effects are contained in the expected effects. If not, a path has
  // been found and it is printed.
  private def findPath(effVar: Type.Var, concreteEff: SourceEffect, path: List[TypeConstraint], constrs: List[TypeConstraint]): Unit = {
    // Only consider constraints that are not already in the path.
    constrs.foreach { constr =>
      constr match {
        case TypeConstraint.Equality(eff1, eff2, prov) => {
          prov match {
            case Provenance.Match(_, _, _) | Provenance.Source(_, _, _) => {
              // Effect variable found on the left.
              if (eff1.typeVars.contains(effVar)) {
                findNext(eff1, eff2, constr)
              }
              // Effect variable found on the right.
              if (eff2.typeVars.contains(effVar)) {
                findNext(eff2, eff1, constr)
              }
            }
            case Provenance.ExpectEffect(expected, actual, loc) => {
              checkPath(expected, actual, effVar, concreteEff, constr, path)
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    }

    // Helper to find the next step in the path.
    def findNext(eff1: Type, eff2: Type, constr: TypeConstraint): Unit = {
      val (add, sub) = findConcreteEffects(eff1)
      concreteEff match {
        // If the concrete effect is Pure, and new effects are not introduced.
        case SourceEffect.Pure if add.isEmpty => {
          eff2.typeVars.foreach { tpeVar =>
            findPath(tpeVar, concreteEff, constr :: path, constrs.filterNot(_ == constr))
          }
        }
        case SourceEffect.Effect(sym) if !sub.contains(sym) => {
          // If the concrete effect is not in the subtracted effects, we can traverse further.
          eff2.typeVars.foreach { tpeVar =>
            findPath(tpeVar, concreteEff, constr :: path, constrs.filterNot(_ == constr))
          }
        }
        case _ => ()
      }
    }

    def checkPath(expected: Type, actual: Type, effVar: Type.Var, concreteEff: SourceEffect, constr: TypeConstraint, path: List[TypeConstraint]): Unit = {
      if(expected.typeVars.isEmpty && actual.typeVars.contains(effVar)) {
        (expected, concreteEff) match {
          case (Type.Pure, SourceEffect.Pure) => {
            // If the expected effect is Pure and the concrete effect is Pure, we have a path.
            val newPath = EffectProvenancePath(concreteEff, path.reverse.head, constr :: path, constr)
            printPath(newPath)
          }
          case (_, SourceEffect.Effect(sym)) if !expected.effects.contains(sym) => {
            // If the expected effect contains the concrete effect, we have a path.
            val newPath = EffectProvenancePath(concreteEff, path.reverse.head, constr :: path, constr)
            printPath(newPath)
          }
          case(_, _) => ()
        }
      }
    }
  }

  // Find concrete effects to be introduced and those to be removed.
  private def findConcreteEffects(tpe: Type): (SortedSet[Symbol.EffSym], SortedSet[Symbol.EffSym]) = {
    tpe match {
      case Type.Apply(Type.Apply(Type.Difference, t1, _), t2, _) => {
        val (add1, sub1) = findConcreteEffects(t1)
        val (add2, sub2) = findConcreteEffects(t2)
        (add1.union(sub2), sub1.union(add2))
      }
      case Type.Apply(Type.Apply(Type.Union, t1, _), t2, _) => {
        val (add1, sub1) = findConcreteEffects(t1)
        val (add2, sub2) = findConcreteEffects(t2)
        (add1.union(add2), sub1.union(sub2))
      }
      case Type.Cst(TypeConstructor.Effect(sym, _), _) => (SortedSet(sym), SortedSet.empty)
      case _ => (SortedSet.empty, SortedSet.empty)
    }
  }

  // Prints the full path of constraints indicating which variables
  // were traversed and the constraints at each step.
  private def printPath(path: EffectProvenancePath): Unit = {
    path match {
      case EffectProvenancePath(effect, source, elms, sink) => {
        var msg: String = "///////////////////////////////////////////////////////////////////////////////\n"
        msg += s"Found path!!!\n"
        msg += "///////////////////////////////////////////////////////////////////////////////\n"
        msg += s"Source: ${source}\n"
        msg += "Constraints:\n"
        elms.reverse.foreach { elm =>
          msg += s"  -> ${elm} at ${elm.loc.format}\n"
        }
        msg += s"Sink: ${sink}\n"
        msg += "///////////////////////////////////////////////////////////////////////////////\n"
        println(msg)
      }
    }
  }

  // Dump all the constraints with their provenance.
  private def printConstrs(constrs: List[TypeConstraint]): Unit = {
    constrs.foreach {
      case TypeConstraint.Equality(eff1, eff2, prov) => println(s"$eff1 ~ $eff2 at ${prov.loc} with provenance $prov")
      case _ => ()
    }
  }

  // Represents a path through the constraints from a source to a sink.
  case class EffectProvenancePath(effect: SourceEffect, source: TypeConstraint, elms: List[TypeConstraint], sink: TypeConstraint)
}
