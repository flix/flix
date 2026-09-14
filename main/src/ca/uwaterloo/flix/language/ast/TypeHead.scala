/*
 * Copyright 2025 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

/**
  * A type head is the first constructor of a type.
  * For example:
  * The type head of `List[String]` is `List`.
  * The type head of `a[b, c]` is `a`.
  *
  * Some types are not considered to have heads.
  * These include type aliases and associated types, among others.
  */
trait TypeHead

object TypeHead {
  case class Cst(tc: TypeConstructor) extends TypeHead
  case class Var(sym: Symbol.KindedTypeVarSym) extends TypeHead

  /**
    * Returns the head of the given type, if it exists.
    */
  def fromType(tpe: Type): Option[TypeHead] = {
    tpe.baseType match {
      case Type.Var(sym, _) => Some(Var(sym))
      case Type.Cst(tc, _) => Some(Cst(tc))
      case _ => None
    }
  }
}
