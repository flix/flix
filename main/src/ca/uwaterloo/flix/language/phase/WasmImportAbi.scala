/*
 * Copyright 2026 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{SimpleType, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.ExportAbi.AbiType
import ca.uwaterloo.flix.util.Result

object WasmImportAbi {

  type Signature = ExportAbi.Signature

  def signatureOf(fparams: List[Type], result: Type): Option[Signature] =
    for {
      ps <- traverseResult(normalizeZeroArgTypeParams(fparams))(ExportAbi.portableFromType)
      r <- ExportAbi.portableFromType(result).toOption.flatten
    } yield ExportAbi.Signature(ps, r)

  def signatureOf(fparams: List[SimpleType], result: SimpleType): Option[Signature] =
    ExportAbi.portableSignature(normalizeZeroArgSimpleTypeParams(fparams), result)

  def normalizeZeroArgTypeParams(fparams: List[Type]): List[Type] = fparams match {
    case Type.Cst(TypeConstructor.Unit, _) :: Nil => Nil
    case params => params
  }

  def normalizeZeroArgSimpleTypeParams(fparams: List[SimpleType]): List[SimpleType] = fparams match {
    case SimpleType.Unit :: Nil => Nil
    case params => params
  }

  def supportsParam(tpe: Type): Boolean =
    ExportAbi.portableFromType(tpe).toOption.flatten.nonEmpty

  def supportsResult(tpe: Type): Boolean =
    ExportAbi.portableFromType(tpe).toOption.flatten.nonEmpty

  def supportsParam(tpe: SimpleType): Boolean =
    ExportAbi.portableFromSimpleType(tpe).nonEmpty

  def supportsResult(tpe: SimpleType): Boolean =
    ExportAbi.portableFromSimpleType(tpe).nonEmpty

  def isByRefBoundaryType(tpe: AbiType): Boolean = tpe match {
    case AbiType.Bool | AbiType.Int8 | AbiType.Int16 | AbiType.Int32 | AbiType.Int64 | AbiType.Float32 | AbiType.Float64 => false
    case _ => true
  }

  def aggregateTypes(sig: Signature): List[AbiType] =
    ExportAbi.aggregateTypes(sig)

  private def traverseResult(as: List[Type])(f: Type => Result[Option[AbiType], Unit]): Option[List[AbiType]] =
    as.foldRight(Option(List.empty[AbiType])) {
      case (a, acc) =>
        (f(a).toOption.flatten, acc) match {
          case (Some(x), Some(xs)) => Some(x :: xs)
          case _ => None
        }
    }
}
