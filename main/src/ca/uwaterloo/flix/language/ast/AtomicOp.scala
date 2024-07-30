/*
 * Copyright 2023 Magnus Madsen
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

import java.lang.reflect.{Constructor, Field, Method}

/**
  * A common super-type for control pure expressions.
  */
sealed trait AtomicOp

object AtomicOp {

  case class Closure(sym: Symbol.DefnSym) extends AtomicOp

  case class Unary(sop: SemanticOp.UnaryOp) extends AtomicOp

  case class Binary(sop: SemanticOp.BinaryOp) extends AtomicOp

  case object Region extends AtomicOp

  case class Is(sym: Symbol.CaseSym) extends AtomicOp

  case class Tag(sym: Symbol.CaseSym) extends AtomicOp

  case class Untag(sym: Symbol.CaseSym) extends AtomicOp

  case class Index(idx: Int) extends AtomicOp

  case object Tuple extends AtomicOp

  case object RecordEmpty extends AtomicOp

  case class RecordSelect(label: Name.Label) extends AtomicOp

  case class RecordExtend(label: Name.Label) extends AtomicOp

  case class RecordRestrict(label: Name.Label) extends AtomicOp

  case object ArrayLit extends AtomicOp

  case object ArrayNew extends AtomicOp

  case object ArrayLoad extends AtomicOp

  case object ArrayStore extends AtomicOp

  case object ArrayLength extends AtomicOp

  case class StructNew(sym: Symbol.StructSym, fields: List[Symbol.StructFieldSym]) extends AtomicOp

  case class StructPut(sym: Symbol.StructSym, field: Name.Label) extends AtomicOp

  case class StructGet(sym: Symbol.StructSym, field: Name.Label) extends AtomicOp

  case object Ref extends AtomicOp

  case object Deref extends AtomicOp

  case object Assign extends AtomicOp

  case class InstanceOf(clazz: Class[_]) extends AtomicOp

  case object Cast extends AtomicOp

  case object Unbox extends AtomicOp

  case object Box extends AtomicOp

  case class InvokeConstructor(constructor: Constructor[_]) extends AtomicOp

  case class InvokeMethod(method: Method) extends AtomicOp

  case class InvokeStaticMethod(method: Method) extends AtomicOp

  case class GetField(field: Field) extends AtomicOp

  case class PutField(field: Field) extends AtomicOp

  case class GetStaticField(field: Field) extends AtomicOp

  case class PutStaticField(field: Field) extends AtomicOp

  case object Spawn extends AtomicOp

  case object Lazy extends AtomicOp

  case object Force extends AtomicOp

  case class HoleError(sym: Symbol.HoleSym) extends AtomicOp

  case object MatchError extends AtomicOp

}
