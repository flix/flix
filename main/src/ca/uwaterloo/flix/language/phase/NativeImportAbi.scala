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
import ca.uwaterloo.flix.util.Result

object NativeImportAbi {

  sealed trait AbiType {
    def requiresBridgeCtx: Boolean = this match {
      case AbiType.String | AbiType.Bytes | AbiType.Portable(_) => true
      case _ => false
    }
  }

  object AbiType {
    case object Unit extends AbiType
    case object Bool extends AbiType
    case object Int8 extends AbiType
    case object Int16 extends AbiType
    case object Int32 extends AbiType
    case object Int64 extends AbiType
    case object Float32 extends AbiType
    case object Float64 extends AbiType
    case object String extends AbiType
    case object Bytes extends AbiType
    case class Portable(tpe: ExportAbi.AbiType) extends AbiType
  }

  case class Signature(params: List[AbiType], result: AbiType) {
    def requiresBridgeCtx: Boolean =
      params.exists(_.requiresBridgeCtx) || result.requiresBridgeCtx
  }

  def signatureOf(fparams: List[Type], result: Type): Option[Signature] = {
    val ps = normalizeZeroArgTypeParams(fparams).map(toParamAbiType)
    val r = toResultAbiType(result)
    if (ps.forall(_.nonEmpty) && r.nonEmpty) Some(Signature(ps.flatten, r.get)) else None
  }

  def signatureOf(fparams: List[SimpleType], result: SimpleType): Option[Signature] = {
    val ps = normalizeZeroArgSimpleTypeParams(fparams).map(toParamAbiType)
    val r = toResultAbiType(result)
    if (ps.forall(_.nonEmpty) && r.nonEmpty) Some(Signature(ps.flatten, r.get)) else None
  }

  def normalizeZeroArgTypeParams(fparams: List[Type]): List[Type] = fparams match {
    case Type.Cst(TypeConstructor.Unit, _) :: Nil => Nil
    case params => params
  }

  def normalizeZeroArgSimpleTypeParams(fparams: List[SimpleType]): List[SimpleType] = fparams match {
    case SimpleType.Unit :: Nil => Nil
    case params => params
  }

  def supportsParam(tpe: Type): Boolean = toParamAbiType(tpe).nonEmpty
  def supportsResult(tpe: Type): Boolean = toResultAbiType(tpe).nonEmpty
  def supportsParam(tpe: SimpleType): Boolean = toParamAbiType(tpe).nonEmpty
  def supportsResult(tpe: SimpleType): Boolean = toResultAbiType(tpe).nonEmpty

  private def toParamAbiType(tpe: Type): Option[AbiType] = toLeafAbiType(tpe).filter(_ != AbiType.Unit)
  private def toResultAbiType(tpe: Type): Option[AbiType] = toLeafAbiType(tpe)
  private def toParamAbiType(tpe: SimpleType): Option[AbiType] = toLeafAbiType(tpe).filter(_ != AbiType.Unit)
  private def toResultAbiType(tpe: SimpleType): Option[AbiType] = toLeafAbiType(tpe)

  private def toLeafAbiType(tpe: Type): Option[AbiType] =
    ExportAbi.portableFromType(tpe) match {
      case Result.Ok(Some(abiTpe)) => exportAbiToNativeImportAbi(abiTpe)
      case _ => None
    }

  private def toLeafAbiType(tpe: SimpleType): Option[AbiType] =
    ExportAbi.portableFromSimpleType(tpe).flatMap(exportAbiToNativeImportAbi)

  private def exportAbiToNativeImportAbi(abiTpe: ExportAbi.AbiType): Option[AbiType] = abiTpe match {
    case ExportAbi.AbiType.Unit => Some(AbiType.Unit)
    case ExportAbi.AbiType.Bool => Some(AbiType.Bool)
    case ExportAbi.AbiType.Int8 => Some(AbiType.Int8)
    case ExportAbi.AbiType.Int16 => Some(AbiType.Int16)
    case ExportAbi.AbiType.Int32 => Some(AbiType.Int32)
    case ExportAbi.AbiType.Int64 => Some(AbiType.Int64)
    case ExportAbi.AbiType.Float32 => Some(AbiType.Float32)
    case ExportAbi.AbiType.Float64 => Some(AbiType.Float64)
    case ExportAbi.AbiType.String => Some(AbiType.String)
    case ExportAbi.AbiType.Bytes => Some(AbiType.Bytes)
    case other => Some(AbiType.Portable(other))
  }
}
