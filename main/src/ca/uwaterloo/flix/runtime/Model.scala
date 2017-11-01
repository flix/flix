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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.util.InternalRuntimeException

/**
  * A class representing the minimal model.
  *
  * @param root        the abstract syntax tree of the program.
  * @param definitions the definitions in the program.
  * @param relations   the relational facts in the model.
  * @param lattices    the lattice facts in the model.
  */
class Model(root: Root,
            time: Time,
            definitions: Map[Symbol.DefnSym, () => AnyRef],
            relations: Map[Symbol.TableSym, Iterable[List[AnyRef]]],
            lattices: Map[Symbol.TableSym, Iterable[(List[AnyRef], AnyRef)]])
           (implicit flix: Flix) {

  /**
    * Returns the root AST.
    */
  def getRoot: Root = root

  /**
    * Returns all the benchmark functions in the program.
    */
  def getBenchmarks: Map[Symbol.DefnSym, () => AnyRef] = {
    definitions filter {
      case (sym, _) => root.defs(sym).ann.isBenchmark
    }
  }

  /**
    * Returns all the test functions in the program.
    */
  def getTests: Map[Symbol.DefnSym, () => AnyRef] = {
    definitions filter {
      case (sym, _) => root.defs(sym).ann.isTest
    }
  }

  /**
    * Returns the time taken by each compiler phase.
    */
  def getTime: Time = time

  /**
    * Immediately evaluates the given fully-qualified name `fqn`.
    *
    * Returns the raw result.
    */
  def eval(fqn: String): AnyRef = {
    // Construct the definition symbol.
    val sym = Symbol.mkDefnSym(fqn)

    // Retrieve the function and call it.
    definitions.get(sym) match {
      case None => throw new IllegalArgumentException(s"Undefined fully-qualified name: '$fqn'.")
      case Some(fn) => fn()
    }
  }

  /**
    * Immediately evaluates the given fully-qualified name `fqn`.
    *
    * Returns a string representation of the result.
    */
  def evalToString(fqn: String): String = {
    // Construct the definition symbol.
    val sym = Symbol.mkDefnSym(fqn)

    // Retrieve the definition.
    root.defs.get(sym) match {
      case None => throw new IllegalArgumentException(s"Undefined fully-qualified name: '$fqn'.")
      case Some(defn) =>
        // Retrieve the function and call it.
        val resultValue = definitions(sym)()

        // Retrieve the result type.
        val resultType = getResultType(defn.tpe)

        // Compute and return a string representation of the result.
        toString(resultValue, resultType)
    }
  }

  /**
    * Returns a map from fully-qualified relation names to a pair of an attribute list and a set of rows for that table.
    */
  def getRelations: Map[String, (List[String], Iterable[List[String]])] =
    relations.foldLeft(Map.empty[String, (List[String], Iterable[List[String]])]) {
      case (macc, (sym, rows)) =>
        root.tables(sym) match {
          case Table.Relation(_, attr, _) =>
            // Compute the attributes names.
            val attributes: List[String] = attr.map(_.name).toList

            // Compute the rows of the table.
            val rows: Iterable[List[String]] = relations(sym).map {
              case row => (row zip attr) map {
                case (obj, Attribute(_, tpe)) => toString(obj, tpe)
              }
            }

            macc + (sym.toString -> (attributes, rows))
          case Table.Lattice(_, _, _, _) => macc // Nop
        }
    }

  /**
    * Returns a map from fully-qualified lattices names to a pair of an attribute list and a set of rows for that table.
    */
  def getLattices: Map[String, (List[String], Iterable[List[String]])] = relations.foldLeft(Map.empty[String, (List[String], Iterable[List[String]])]) {
    case (macc, (sym, rows)) =>
      root.tables(sym) match {
        case Table.Relation(_, attr, _) => macc // Nop

        case Table.Lattice(_, keys, value, _) =>
          // Compute the attributes of the table.
          val attr = keys.toList ::: value :: Nil

          // Compute the attribute names.
          val attributes: List[String] = attr.map(_.name)

          // Compute the rows of the table.
          val rows: Iterable[List[String]] = relations(sym).map {
            case row => (row zip attr) map {
              case (obj, Attribute(_, tpe)) => toString(obj, tpe)
            }
          }

          macc + (sym.toString -> (attributes, rows))
      }
  }

  /**
    * Returns the result type of the given lambda type.
    */
  private def getResultType(tpe: Type): Type = tpe.typeArguments.last

  /**
    * Returns a string representation of the given reference `ref` formatted according to the given type `tpe`.
    */
  private def toString(ref: AnyRef, tpe: Type): String = {
    // Retrieve the toString special operator.
    root.specialOps(SpecialOperator.ToString).get(tpe) match {
      case None => throw InternalRuntimeException(s"Undefined 'toString' special operator for the given type: '$tpe'.")
      case Some(sym) => val target = Linker.link(sym, root)
        target.invoke(Array(ref)) match {
          case null => throw InternalRuntimeException(s"Unexpected 'null' returned by 'toString' special operator.")
          case s: String => s
          case o: Value.Str => o.lit
          case o => throw InternalRuntimeException(s"Unexpected non-string value: ${o.getClass.getCanonicalName} returned by 'toString' special operator.")
        }
    }
  }

}