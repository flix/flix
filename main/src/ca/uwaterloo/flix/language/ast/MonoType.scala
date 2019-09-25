/*
 * Copyright 2019 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast

/**
  * Representation of monomorphed types.
  *
  * A mono type is a simplified representation of a type without any type variables.
  */
sealed trait MonoType

object MonoType {

  ///
  /// Primitive Types.
  ///

  case object Unit extends MonoType

  case object Bool extends MonoType

  case object Char extends MonoType

  case object Float32 extends MonoType

  case object Float64 extends MonoType

  case object Int8 extends MonoType

  case object Int16 extends MonoType

  case object Int32 extends MonoType

  case object Int64 extends MonoType

  case object BigInt extends MonoType

  case object Str extends MonoType

  ///
  /// Compound Types.
  ///

  case class Array(tpe: MonoType) extends MonoType

  case class Channel(tpe: MonoType) extends MonoType

  case class Ref(tpe: MonoType) extends MonoType

  case class Tuple(elms: List[MonoType]) extends MonoType

  case class Enum(sym: Symbol.EnumSym, args: List[MonoType]) extends MonoType

  case class Arrow(args: List[MonoType], result: MonoType) extends MonoType

  case class RecordEmpty() extends MonoType

  case class RecordExtend(label: String, value: MonoType, rest: MonoType) extends MonoType

  case class SchemaEmpty() extends MonoType

  case class SchemaExtend(sym: Symbol.PredSym, tpe: MonoType, rest: MonoType) extends MonoType

  case class Relation(sym: Symbol.RelSym, attr: List[MonoType]) extends MonoType

  case class Lattice(sym: Symbol.LatSym, attr: List[MonoType]) extends MonoType

  case class Native(clazz: Class[_]) extends MonoType

  // TODO: Should be removed.
  case class Var(id: Int) extends MonoType

}
