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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import scala.collection.immutable.SortedSet

object EffectProvenance {
  // First, we find all equality constraints that have a provenance of type
  // Provenance.Match with a concrete effect on one side and an effect variable
  // on the other side.
  //
  // For each such variable and set of concrete effects , we track a path
  // through the constraints until we reach a constraint that has a
  // provenance of type Provenance.ExpectEffect and an effect in the
  // in the concrete set is not in the expected effects.
  def debug(constrs0: List[TypeConstraint]): Unit = {
    printConstrs(constrs0)
    constrs0.foreach {
      case TypeConstraint.Equality(tpe1, tpe2, prov) => {
        prov match {
          case Provenance.Match(_, _, loc) => {
            // Variables on the left with concrete effects on the right.
            if (tpe1.typeVars.nonEmpty && tpe2.effects.nonEmpty) {
              tpe1.typeVars.foreach { tpeVar => tpeVar match { case _ =>
                println(s"Finding path for variable $tpeVar  of constraint ${TypeConstraint.Equality(tpe1, tpe2, prov)} at location $loc")
                findPath(tpeVar, tpe2.effects, (tpeVar, TypeConstraint.Equality(tpe1, tpe2, prov)) :: Nil, constrs0)
                }
              }
            }
            // Variables on the right with concrete effects on the left.
            if (tpe2.typeVars.nonEmpty && tpe1.effects.nonEmpty) {
              tpe2.typeVars.foreach { tpeVar => tpeVar match { case _ =>
                println(s"Finding path for variable $tpeVar  of constraint ${TypeConstraint.Equality(tpe1, tpe2, prov)} at location $loc")
                findPath(tpeVar, tpe1.effects, (tpeVar, TypeConstraint.Equality(tpe1, tpe2, prov)) :: Nil, constrs0)
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
  // Finds the next constraint in the path given the effect variable `eff`,
  // the set of concrete effects `concreteEff`, the current path `path`,
  // and the list of all constraints `constrs`.
  // If Provenance.ExpectEffect is found, it checks if all the concrete
  // effects are contained in the expected effects. If not, a path has
  // been found and it is printed.
  private def findPath(eff: Type.Var, concreteEff: SortedSet[Symbol.EffectSym],  path: List[(Type.Var, TypeConstraint)], constrs: List[TypeConstraint]): Unit = {
    // Only consider constraints that are not already in the path.
    constrs.filter(!path.map(p => p._2).contains(_)).foreach {
      case TypeConstraint.Equality(tpe1, tpe2, prov) => {
        prov match {
          case Provenance.Match(_, _, loc) => {
            // Effect variable found on the left.
            if (tpe1.typeVars.contains(eff)) {
              tpe2.typeVars.foreach { tpeVar => tpeVar match {
                  case Type.Var(_, _) =>
                    findPath(tpeVar, concreteEff, (tpeVar, TypeConstraint.Equality(tpe1, tpe2, prov)) :: path, constrs)
                  case _ => ()
                }
              }
            }
            // Effect variable found on the right.
            if (tpe2.typeVars.contains(eff)) {
              tpe1.typeVars.foreach { tpeVar => tpeVar match {
                  case Type.Var(_, _) =>
                    findPath(tpeVar, concreteEff, (tpeVar, TypeConstraint.Equality(tpe1, tpe2, prov)) :: path, constrs)
                  case _ => ()
                }
              }
            }
          }
          // If Provenance.ExpectEffect is found, check if the concrete effects
          // are contained in the expected effects. If not, print the path.
          case Provenance.ExpectEffect(expected, actual, loc) => {
            if(expected.typeVars.isEmpty && actual.typeVars.contains(eff)) {
              if(!concreteEff.intersect(expected.effects).equals(concreteEff)) {
                printPath((eff, TypeConstraint.Equality(tpe1, tpe2, prov)) :: path)
              }
            }
          }
          case _ => ()
        }
      }
      case _ => ()
    }
  }

  // Prints the full path of constraints indicating which variables
  // were traversed and the constraints at each step.
  private def printPath(path: List[(Type.Var, TypeConstraint)]): Unit = {
    val revPath = path.reverse
    var msg: String = "///////////////////////////////////////////////////////////////////////////////\n"
    msg += s"Found path from constraint ${revPath.head._2} through variable ${revPath.head._1} to constraint ${path.head._2}\n"
    msg += "///////////////////////////////////////////////////////////////////////////////\n"
    revPath.foreach {
      case (tpeVar, constr) => msg += s"At location ${constr.loc}:\nGot constraint: $constr and found variable: $tpeVar\n"
    }
    msg += "///////////////////////////////////////////////////////////////////////////////\n"
    println(msg)
  }

  // Dump all the constraints with their provenance.
  private def printConstrs(constrs: List[TypeConstraint]): Unit = {
    constrs.foreach {
      case TypeConstraint.Equality(tpe1, tpe2, prov) => prov match {
        case _ => println(s"$tpe1 ~ $tpe2 with Provenance $prov at ${prov.loc}")
      }
      case _ => ()
    }
  }
}
