/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{JConstructor, JField, JMethod, Mutability}

import java.lang.constant.ClassDesc

/**
  * A common super-type for control pure expressions.
  */
sealed trait AtomicOp

object AtomicOp {

  sealed trait DataOp extends AtomicOp

  case class Closure(sym: Symbol.DefnSym) extends AtomicOp

  case class Unary(sop: SemanticOp.UnaryOp) extends AtomicOp

  case class Binary(sop: SemanticOp.BinaryOp) extends AtomicOp

  case class Is(sym: Symbol.CaseSym) extends DataOp

  case class Tag(sym: Symbol.CaseSym) extends DataOp

  case class Untag(sym: Symbol.CaseSym, idx: Int) extends DataOp

  case class Index(idx: Int) extends DataOp

  case object Tuple extends DataOp

  case class RecordSelect(label: Name.Label) extends DataOp

  case class RecordExtend(label: Name.Label) extends DataOp

  case class RecordRestrict(label: Name.Label) extends DataOp

  case class ExtIs(label: Name.Label) extends DataOp

  case class ExtTag(label: Name.Label) extends DataOp

  case class ExtUntag(label: Name.Label, idx: Int) extends DataOp

  case object ArrayLit extends DataOp

  case object ArrayNew extends DataOp

  case object ArrayLoad extends DataOp

  case object ArrayStore extends DataOp

  case object ArrayLength extends DataOp

  case object VectorLit extends DataOp

  case object VectorLoad extends DataOp

  case object VectorLength extends DataOp

  case class StructNew(sym: Symbol.StructSym, mutability: Mutability, fields: List[Symbol.StructFieldSym]) extends DataOp

  case class StructGet(sym: Symbol.StructFieldSym) extends DataOp

  case class StructPut(sym: Symbol.StructFieldSym) extends DataOp

  case object Lazy extends DataOp

  case object Force extends DataOp

  case class InstanceOf(clazz: ClassDesc) extends AtomicOp

  case object Cast extends AtomicOp

  case object Unbox extends AtomicOp

  case object Box extends AtomicOp

  case class InvokeConstructor(constructor: JConstructor) extends AtomicOp

  case class InvokeSuperConstructor(constructor: JConstructor) extends AtomicOp

  case class InvokeMethod(method: JMethod) extends AtomicOp

  case class InvokeSuperMethod(sym: Symbol.AnonClassSym, method: JMethod) extends AtomicOp

  case class InvokeStaticMethod(method: JMethod) extends AtomicOp

  case class GetField(field: JField) extends AtomicOp

  case class PutField(field: JField) extends AtomicOp

  case class GetStaticField(field: JField) extends AtomicOp

  case class PutStaticField(field: JField) extends AtomicOp

  case object Throw extends AtomicOp

  case object Spawn extends AtomicOp

  case class HoleError(sym: Symbol.HoleSym) extends AtomicOp

  case object MatchError extends AtomicOp

  case class CastError(from: String, to: String) extends AtomicOp

}
