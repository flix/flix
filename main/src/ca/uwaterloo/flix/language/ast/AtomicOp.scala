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

  case class Unary(sop: SemanticOp) extends AtomicOp

  case class Binary(sop: SemanticOp) extends AtomicOp

  case object Region extends AtomicOp

  case object ScopeExit extends AtomicOp

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

  case object Ref extends AtomicOp

  case object Deref extends AtomicOp

  case object Assign extends AtomicOp

  case class InstanceOf(clazz: Class[_]) extends AtomicOp

  case object Cast extends AtomicOp

  case class InvokeConstructor(constructor: Constructor[_]) extends AtomicOp

  case class InvokeMethod(method: Method) extends AtomicOp

  case class InvokeStaticMethod(method: Method) extends AtomicOp

  case class GetField(field: Field) extends AtomicOp

  case class PutField(field: Field) extends AtomicOp

  case class GetStaticField(field: Field) extends AtomicOp

  case class PutStaticField(field: Field) extends AtomicOp

  case object Spawn extends AtomicOp // different

  case object Lazy extends AtomicOp // different

  case object Force extends AtomicOp

  case object BoxBool extends AtomicOp

  case object BoxInt8 extends AtomicOp

  case object BoxInt16 extends AtomicOp

  case object BoxInt32 extends AtomicOp

  case object BoxInt64 extends AtomicOp

  case object BoxChar extends AtomicOp

  case object BoxFloat32 extends AtomicOp

  case object BoxFloat64 extends AtomicOp

  case object UnboxBool extends AtomicOp

  case object UnboxInt8 extends AtomicOp

  case object UnboxInt16 extends AtomicOp

  case object UnboxInt32 extends AtomicOp

  case object UnboxInt64 extends AtomicOp

  case object UnboxChar extends AtomicOp

  case object UnboxFloat32 extends AtomicOp

  case object UnboxFloat64 extends AtomicOp

  case class HoleError(sym: Symbol.HoleSym) extends AtomicOp

  case object MatchError extends AtomicOp

}
