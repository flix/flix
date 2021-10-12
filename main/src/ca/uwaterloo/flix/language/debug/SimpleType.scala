/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * A well-kinded type in an easily-printable format.
  */
sealed trait SimpleType

object SimpleType {

  case class Apply(tpe: SimpleType, tpes: List[SimpleType]) extends SimpleType

  case class Var(id: Int, text: String) extends SimpleType

  case class Tuple(length: Int, fields: List[SimpleType]) extends SimpleType

  case class PureArrow(args: List[SimpleType], ret: SimpleType) extends SimpleType

  case class ImpureArrow(args: List[SimpleType], ret: SimpleType) extends SimpleType

  case class Record(fields: List[RecordFieldType], rest: Option[SimpleType.Var]) extends SimpleType

  case object RecordConstructor extends SimpleType

  case class RecordHead(field: RecordFieldType) extends SimpleType

  case class RecordFieldType(name: String, tpe: SimpleType)

  def fromWellKindedType(t: Type): SimpleType = t match {
    case Type.KindedVar(id, kind, loc, rigidity, text) => Var(id, text.get) // MATT how to handle vars? alpha-renaming, etc.
    case _: Type.UnkindedVar => throw InternalCompilerException("") // MATT
    case _: Type.Ascribe => throw InternalCompilerException("") // MATT
    case Type.Cst(tc, loc) =>
    case Type.Lambda(tvar, tpe, loc) =>
    case Type.Apply(tpe1, tpe2, loc) =>
  }
}
