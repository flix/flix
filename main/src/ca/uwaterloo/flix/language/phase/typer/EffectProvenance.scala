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
    if (enableEffectProvenance) {
      constrs0.foreach { constr =>
        constr match {
          case TypeConstraint.Equality(_, _, prov) => {
            prov match {
              case Provenance.Source(eff1, eff2, loc) => {
                (eff1, eff2) match {
                  case _ => {
                    eff2 match {
                      case Type.Pure => findPath(eff1, Symbol.mkEffectSym("Pure"), constr :: Nil, constrs0.filterNot(_ == constr))
                      case _ => {
                        val (add, _) = findConcreteEffects(eff2)
                        eff2.effects.filter(add.contains(_)).foreach {
                          findPath(eff1, _, constr :: Nil, constrs0.filterNot(_ == constr))
                        }
                      }
                    }
                  }
                }
              }
              case _ => ()
            }
          }
          case _ => ()
        }
      }
    }
  }

  // Finds the next constraint in the path given the effect variable `eff`,
  // the set of concrete effects `concreteEff`, the current path `path`,
  // and the list of all constraints `constrs`.
  // If Provenance.ExpectEffect is found, it checks if all the concrete
  // effects are contained in the expected effects. If not, a path has
  // been found and it is printed.
  private def findPath(effVar: Type.Var, concreteEff: Symbol.EffSym, path: List[TypeConstraint], constrs: List[TypeConstraint]): Unit = {
    // Only consider constraints that are not already in the path.
    constrs.foreach { constr =>
      constr match {
        case TypeConstraint.Equality(eff1, eff2, prov) => {
          prov match {
            case Provenance.Match(_, _, _) | Provenance.Source(_, _, _) => {
              // Effect variable found on the left.
              if (eff1.typeVars.contains(effVar)) {
                val (add, sub) = findConcreteEffects(eff1)
                if (concreteEff == Symbol.mkEffectSym("Pure") && !add.isEmpty) {
                  ()
                } else if (!sub.contains(concreteEff)) {
                  eff2.typeVars.foreach { tpeVar =>
                    tpeVar match {
                      case Type.Var(_, _) =>
                        findPath(tpeVar, concreteEff, constr :: path, constrs.filterNot(_ == constr))
                      case _ => ()
                    }
                  }
                }
              }
              // Effect variable found on the right.
              if (eff2.typeVars.contains(effVar)) {
                val (add, sub) = findConcreteEffects(eff2)
                if (concreteEff == Symbol.mkEffectSym("Pure") && !add.isEmpty) {
                  ()
                } else if (!sub.contains(concreteEff)) {
                  eff1.typeVars.foreach { tpeVar =>
                    tpeVar match {
                      case Type.Var(_, _) =>
                        findPath(tpeVar, concreteEff, constr :: path, constrs.filterNot(_ == constr))
                      case _ => ()
                    }
                  }
                }
              }
            }

            case Provenance.ExpectEffect(expected, actual, loc) => {
              if (expected.typeVars.isEmpty && actual.typeVars.contains(effVar)) {
                expected match {
                  // If all actual effects are not expected, we have a path.
                  case _ => {
                    if (!expected.effects.contains(concreteEff)) {
                      val newPath = new EffectProvenancePath.Path(concreteEff, path.reverse.head, constr :: path, constr)
                      printPath(newPath)
                    } else {
                      expected match {
                        // If the expected effect is Pure, we check if the concrete effect is Pure.
                        case Type.Pure => {
                          if (concreteEff == Symbol.mkEffectSym("Pure")) {
                            println("Found expected pure effect")
                            val newPath = new EffectProvenancePath.Path(concreteEff, path.reverse.head, constr :: path, constr)
                            printPath(newPath)
                          }
                        }
                        case _ => ()
                      }
                    }
                  }
                }
              }
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    }
  }

  // Find concrete effects to be introduced and those to be removed
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
  private def printPath(path: EffectProvenancePath.Path): Unit = {
    path match {
      case EffectProvenancePath.Path(sym, source, elms, sink) => {
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
      case TypeConstraint.Equality(eff1, eff2, prov) => prov match {
        case _ => println(s"$eff1 ~ $eff2 at ${prov.loc} with provenance $prov")
      }
      case _ => ()
    }
  }

  // Represents a path through the constraints from a source to a sink.
  object EffectProvenancePath {
    case class Path(sym: Symbol.EffSym, source: TypeConstraint, elms: List[TypeConstraint], sink: TypeConstraint)
  }
}
