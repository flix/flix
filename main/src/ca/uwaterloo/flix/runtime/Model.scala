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

import ca.uwaterloo.flix.api
import ca.uwaterloo.flix.language.ast.{ExecutableAst, SpecialOperator, Symbol, Time, Type}
import ca.uwaterloo.flix.util.InternalRuntimeException

/**
  * A class representing the minimal model.
  *
  * @param root        the abstract syntax tree of the program.
  * @param definitions the definitions in the program.
  * @param relations   the relational facts in the model.
  * @param lattices    the lattice facts in the model.
  */
class Model(root: ExecutableAst.Root,
            time: Time,
            definitions: Map[Symbol.DefnSym, () => AnyRef],
            relations: Map[Symbol.TableSym, Iterable[List[AnyRef]]],
            lattices: Map[Symbol.TableSym, Iterable[(List[AnyRef], AnyRef)]]) {

  /**
    * Returns the root AST.
    */
  def getRoot: ExecutableAst.Root = root

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
    * Returns the fully-qualified names of all relations in the program.
    */
  def getRelationNames: Set[String] = relations.keySet.map(_.toString)

  /**
    * Returns the fully-qualified names of all lattices in the program.
    */
  def getLatticeNames: Set[String] = lattices.keySet.map(_.toString)


  def getRelation(name: String): Iterable[List[AnyRef]] =
    getRelationOpt(name).get

  def getRelationOpt(name: String): Option[Iterable[List[AnyRef]]] =
    relations.get(Symbol.mkTableSym(name))

  def getLattice(name: String): Iterable[(List[AnyRef], AnyRef)] =
    getLatticeOpt(name).get

  def getLatticeOpt(name: String): Option[Iterable[(List[AnyRef], AnyRef)]] =
    lattices.get(Symbol.mkTableSym(name))

  /**
    * Returns a string representation of the given reference `ref` formatted according to the given type `tpe`.
    */
  private def toString(ref: AnyRef, tpe: Type): String = {
    // Retrieve the toString special operator.
    root.specialOps(SpecialOperator.ToString).get(tpe) match {
      case None => throw InternalRuntimeException(s"Undefined 'toString' special operator for the given type: '$tpe'.")
      case Some(sym) => val target = Linker.link(sym, root)
        target.invoke(Array(ref)) match {
          case s: String => s
          case o => throw InternalRuntimeException("Unexpected non-string value returned by 'toString' special operator.")
        }
    }
  }

  /**
    * Returns the result type of the given lambda type.
    */
  private def getResultType(tpe: Type): Type = tpe match {
    case Type.Apply(Type.Arrow(_), ts) => ts.last
    case _ => tpe
  }

}