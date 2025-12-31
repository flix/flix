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

class HashType {
  private val md = MessageDigest.getInstance("SHA-256")

  def hash(tpe0: Type): Array[Byte] = {
    hashType(tpe0)
    md.digest()
  }

  private def hashType(tpe0: Type): Unit = tpe0 match {
    case Type.Var(sym, _) =>
      hashKindedTypeVarSym(sym)
      md.update(HType.Var.hashCode().byteValue)

    case Type.Cst(tc, _) =>
      hashTypeConstructor(tc)
      md.update(HType.Cst.hashCode().byteValue)

    case Type.Apply(tpe1, tpe2, _) => ???
    case Type.AssocType(SymUse.AssocTypeSymUse(sym, loc), arg, kind, _) => ???
    case Type.Alias(symUse, args, tpe, loc) => throw InternalCompilerException("Unexpected type alias", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
  }

  private def hashTypeConstructor(tc0: TypeConstructor): Unit = tc0 match {
    case TypeConstructor.Void => ???
    case TypeConstructor.AnyType => ???
    case TypeConstructor.Unit => ???
    case TypeConstructor.Null => ???
    case TypeConstructor.Bool => ???
    case TypeConstructor.Char => ???
    case TypeConstructor.Float32 => ???
    case TypeConstructor.Float64 => ???
    case TypeConstructor.BigDecimal => ???
    case TypeConstructor.Int8 => ???
    case TypeConstructor.Int16 => ???
    case TypeConstructor.Int32 => ???
    case TypeConstructor.Int64 => ???
    case TypeConstructor.BigInt => ???
    case TypeConstructor.Str => ???
    case TypeConstructor.Regex => ???
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

  private def hashKindedTypeVarSym(sym0: Symbol.KindedTypeVarSym): Unit = ???

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

  }
}
