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
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol, Time, Type}
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
    *
    * NB: Supports values from both the interpreter and the compiler.
    */
  private def toString(ref: AnyRef, tpe: Type, infix: Boolean = false): String = tpe match {
    case Type.Unit => "()"

    case Type.Bool => ref match {
      case o: java.lang.Boolean => o.toString
      case Value.True => "true"
      case Value.False => "false"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Bool`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Char => ref match {
      case o: java.lang.Character => "'" + o.toString + "'"
      case Value.Char(lit) => "'" + lit.toString + "'"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Char`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Float32 => ref match {
      case o: java.lang.Float => o.toString + "f32"
      case Value.Float32(lit) => lit.toString + "f32"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Float32`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Float64 => ref match {
      case o: java.lang.Double => o.toString + "f64"
      case Value.Float64(lit) => lit.toString + "f64"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Float64`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Int8 => ref match {
      case o: java.lang.Byte => o.toString + "i8"
      case Value.Int8(lit) => lit.toString + "i8"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Int8`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Int16 => ref match {
      case o: java.lang.Short => o.toString + "i16"
      case Value.Int16(lit) => lit.toString + "i16"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Int16`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Int32 => ref match {
      case o: java.lang.Integer => o.toString + "i32"
      case Value.Int32(lit) => lit.toString + "i32"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Int32`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Int64 => ref match {
      case o: java.lang.Long => o.toString + "i64"
      case Value.Int64(lit) => lit.toString + "i64"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Int64`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.BigInt => ref match {
      case o: java.math.BigInteger => o.toString + "ii"
      case Value.BigInt(lit) => lit.toString + "ii"
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `BigInt`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Str => ref match {
      case o: java.lang.String => "\"" + o + "\""
      case Value.Str(lit) => "\"" + lit + "\""
      case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Str`, but got: '${ref.getClass.getCanonicalName}'.")
    }

    case Type.Native => "<<native>>"

    case Type.Ref => "<<ref>>"

    case Type.Arrow(l) => "<<closure>>"

    case Type.Enum(sym, _) =>
      ref match {
        case o: api.Enum =>
          val enum = root.enums(sym)
          val caze = enum.cases(o.getTag)

          if (caze.tpe == Type.Unit) {
            o.getTag
          } else if (o.getTag == "Cons") {
            toString(o.getBoxedValue, caze.tpe, infix = true)
          } else {
            o.getTag + "(" + toString(o.getBoxedValue, caze.tpe) + ")"
          }
        case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Tag`, but got: '${ref.getClass.getCanonicalName}'.")
      }

    case Type.Apply(Type.FTuple(l), ts) =>
      ref match {
        case o: api.Tuple =>
          val elms = o.getBoxedValue.toList
          if (infix) {
            val hd = o.getBoxedValue()(0)
            val tl = o.getBoxedValue()(1)
            toString(hd, ts.head) + " :: " + toString(tl, ts.tail.head)
          } else {
            "(" + elms.zip(ts).map(p => toString(p._1, p._2)).mkString(", ") + ")"
          }
        case _ => throw InternalRuntimeException(s"Mismatched types. Expected `Tuple`, but got: '${ref.getClass.getCanonicalName}'.")
      }

    case Type.Apply(t, _) => toString(ref, t)

    case _ => ref.toString
  }

  /**
    * Returns the result type of the given lambda type.
    */
  private def getResultType(tpe: Type): Type = tpe match {
    case Type.Apply(Type.Arrow(_), ts) => ts.last
    case _ => tpe
  }

}