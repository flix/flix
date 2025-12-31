/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.security.MessageDigest

object HashType {

  def hashType(tpe0: Type): Array[Byte] = tpe0 match {
    case Type.Var(sym, _) =>
      val h = hashKindedTypeVarSym(sym)
      hashBytes(h.appended(HType.Var.hashCode().byteValue))


    case Type.Cst(tc, _) =>
      val h = hashTypeConstructor(tc)
      hashBytes(h.appended(HType.Cst.hashCode().byteValue))

    case Type.Apply(tpe1, tpe2, _) =>
      val h1 = hashType(tpe1)
      val h2 = hashType(tpe2)
      val h3 = h1.appended(HType.Apply.hashCode().byteValue).appendedAll(h2)
      hashBytes(h3)

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, loc), arg, kind, _) => ???
    case Type.Alias(symUse, args, tpe, loc) => throw InternalCompilerException("Unexpected type alias", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
  }

  private def hashTypeConstructor(tc0: TypeConstructor): Array[Byte] = tc0 match {
    case TypeConstructor.Void => hashByte(TypeConstructor.Void.hashCode().byteValue)
    case TypeConstructor.AnyType => hashByte(TypeConstructor.AnyType.hashCode().byteValue)
    case TypeConstructor.Unit => hashByte(TypeConstructor.Unit.hashCode().byteValue)
    case TypeConstructor.Null => hashByte(TypeConstructor.Null.hashCode().byteValue)
    case TypeConstructor.Bool => hashByte(TypeConstructor.Bool.hashCode().byteValue)
    case TypeConstructor.Char => hashByte(TypeConstructor.Char.hashCode().byteValue)
    case TypeConstructor.Float32 => hashByte(TypeConstructor.Float32.hashCode().byteValue)
    case TypeConstructor.Float64 => hashByte(TypeConstructor.Float64.hashCode().byteValue)
    case TypeConstructor.BigDecimal => hashByte(TypeConstructor.BigDecimal.hashCode().byteValue)
    case TypeConstructor.Int8 => hashByte(TypeConstructor.Int8.hashCode().byteValue)
    case TypeConstructor.Int16 => hashByte(TypeConstructor.Int16.hashCode().byteValue)
    case TypeConstructor.Int32 => hashByte(TypeConstructor.Int32.hashCode().byteValue)
    case TypeConstructor.Int64 => hashByte(TypeConstructor.Int64.hashCode().byteValue)
    case TypeConstructor.BigInt => hashByte(TypeConstructor.BigInt.hashCode().byteValue)
    case TypeConstructor.Str => hashByte(TypeConstructor.Str.hashCode().byteValue)
    case TypeConstructor.Regex => hashByte(TypeConstructor.Regex.hashCode().byteValue)
    case TypeConstructor.Arrow(arity) => ???
    case TypeConstructor.ArrowWithoutEffect(arity) => ???
    case TypeConstructor.RecordRowEmpty => ???
    case TypeConstructor.RecordRowExtend(label) => ???
    case TypeConstructor.Record => ???
    case TypeConstructor.Extensible => ???
    case TypeConstructor.SchemaRowEmpty => ???
    case TypeConstructor.SchemaRowExtend(pred) => ???
    case TypeConstructor.Schema => ???
    case TypeConstructor.Sender => ???
    case TypeConstructor.Receiver => ???
    case TypeConstructor.Lazy => ???
    case TypeConstructor.Enum(sym, kind) => ???
    case TypeConstructor.Struct(sym, kind) => ???
    case TypeConstructor.RestrictableEnum(sym, kind) => ???
    case TypeConstructor.Native(clazz) => ???
    case TypeConstructor.JvmConstructor(constructor) => ???
    case TypeConstructor.JvmMethod(method) => ???
    case TypeConstructor.JvmField(field) => ???
    case TypeConstructor.Array => ???
    case TypeConstructor.ArrayWithoutRegion => ???
    case TypeConstructor.Vector => ???
    case TypeConstructor.Tuple(arity) => ???
    case TypeConstructor.Relation(arity) => ???
    case TypeConstructor.Lattice(arity) => ???
    case TypeConstructor.True => ???
    case TypeConstructor.False => ???
    case TypeConstructor.Not => ???
    case TypeConstructor.And => ???
    case TypeConstructor.Or => ???
    case TypeConstructor.Pure => ???
    case TypeConstructor.Univ => ???
    case TypeConstructor.Complement => ???
    case TypeConstructor.Union => ???
    case TypeConstructor.Intersection => ???
    case TypeConstructor.Difference => ???
    case TypeConstructor.SymmetricDiff => ???
    case TypeConstructor.Effect(sym, kind) => ???
    case TypeConstructor.CaseComplement(sym) => ???
    case TypeConstructor.CaseUnion(sym) => ???
    case TypeConstructor.CaseIntersection(sym) => ???
    case TypeConstructor.CaseSymmetricDiff(sym) => ???
    case TypeConstructor.CaseSet(syms, enumSym) => ???
    case TypeConstructor.Region(sym) => ???
    case TypeConstructor.RegionToStar => ???
    case TypeConstructor.RegionWithoutRegion => ???
    case TypeConstructor.Error(id, kind) => throw InternalCompilerException("Unexpected error type constructor", SourceLocation.Unknown)
  }

  private def hashKindedTypeVarSym(sym0: Symbol.KindedTypeVarSym): Array[Byte] = ???

  private def hashBytes(bytes: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(bytes)
    md.digest()
  }

  private def hashByte(byte: Byte): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(byte)
    md.digest()
  }

  /**
    * Common super type for hashable types.
    *
    * Case objects have hash codes that can be converted to byte values
    * and thus represents a stable and unique byte value for the constructor.
    */
  private sealed trait HType

  private object HType {

    case object Var extends HType

    case object Cst extends HType

    case object Apply extends HType

  }
}
