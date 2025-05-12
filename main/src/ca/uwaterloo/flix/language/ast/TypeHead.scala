/*
 * Copyright 2025 Matthew Lutze
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
