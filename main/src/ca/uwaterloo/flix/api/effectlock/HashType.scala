/*
 * Copyright 2026 Jakob Schneider Villumsen
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
      // TODO: get byte representation of Type.Var
      hashBytes(???)

    case Type.Cst(tc, _) =>
      val h = hashTypeConstructor(tc)
      // TODO: get byte representation of Type.Cst
      hashBytes(h.appended(???))

    case Type.Apply(tpe1, tpe2, _) =>
      val h1 = hashType(tpe1)
      val h2 = hashType(tpe2)
      // TODO: get byte representation of Type.Apply
      val h3 = h1.appended(???).appendedAll(h2)
      hashBytes(h3)

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, loc), arg, kind, _) => ???

    case Type.Alias(_, _, _, loc) => throw InternalCompilerException("Unexpected type alias", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
  }

  private def hashTypeConstructor(tc0: TypeConstructor): Array[Byte] = hashInt(lowerTypeConstructor(tc0))

  private def lowerTypeConstructor(tc0: TypeConstructor): Int = tc0 match {
    case TypeConstructor.Void => 0
    case TypeConstructor.AnyType => 1
    case TypeConstructor.Unit => 2
    case TypeConstructor.Null => 3
    case TypeConstructor.Bool => 4
    case TypeConstructor.Char => 5
    case TypeConstructor.Float32 => 6
    case TypeConstructor.Float64 => 7
    case TypeConstructor.BigDecimal => 8
    case TypeConstructor.Int8 => 9
    case TypeConstructor.Int16 => 10
    case TypeConstructor.Int32 => 11
    case TypeConstructor.Int64 => 12
    case TypeConstructor.BigInt => 13
    case TypeConstructor.Str => 14
    case TypeConstructor.Regex => 15
    case TypeConstructor.Arrow(arity) => 16 // TODO: Consider args
    case TypeConstructor.ArrowWithoutEffect(arity) => 17 // TODO: Consider args
    case TypeConstructor.RecordRowEmpty => 18
    case TypeConstructor.RecordRowExtend(label) => 19 // TODO: Consider args
    case TypeConstructor.Record => 20
    case TypeConstructor.Extensible => 21
    case TypeConstructor.SchemaRowEmpty => 22
    case TypeConstructor.SchemaRowExtend(pred) => 23 // TODO: Consider args
    case TypeConstructor.Schema => 24
    case TypeConstructor.Sender => 25
    case TypeConstructor.Receiver => 26
    case TypeConstructor.Lazy => 27
    case TypeConstructor.Enum(sym, kind) => 28 // TODO: Consider args
    case TypeConstructor.Struct(sym, kind) => 29 // TODO: Consider args
    case TypeConstructor.RestrictableEnum(sym, kind) => 30 // TODO: Consider args
    case TypeConstructor.Native(clazz) => 31 // TODO: Consider args
    case TypeConstructor.JvmConstructor(constructor) => 32 // TODO: Consider args
    case TypeConstructor.JvmMethod(method) => 33
    case TypeConstructor.JvmField(field) => 34 // TODO: Consider args
    case TypeConstructor.Array => 35
    case TypeConstructor.ArrayWithoutRegion => 36
    case TypeConstructor.Vector => 37
    case TypeConstructor.Tuple(arity) => 38 // TODO: Consider args
    case TypeConstructor.Relation(arity) => 39 // TODO: Consider args
    case TypeConstructor.Lattice(arity) => 40 // TODO: Consider args
    case TypeConstructor.True => 41
    case TypeConstructor.False => 42
    case TypeConstructor.Not => 43
    case TypeConstructor.And => 44
    case TypeConstructor.Or => 45
    case TypeConstructor.Pure => 46
    case TypeConstructor.Univ => 47
    case TypeConstructor.Complement => 48
    case TypeConstructor.Union => 49
    case TypeConstructor.Intersection => 50
    case TypeConstructor.Difference => 51
    case TypeConstructor.SymmetricDiff => 52
    case TypeConstructor.Effect(sym, kind) => 53 // TODO: Consider args
    case TypeConstructor.CaseComplement(sym) => 54 // TODO: Consider args
    case TypeConstructor.CaseUnion(sym) => 55 // TODO: Consider args
    case TypeConstructor.CaseIntersection(sym) => 56 // TODO: Consider args
    case TypeConstructor.CaseSymmetricDiff(sym) => 57 // TODO: Consider args
    case TypeConstructor.CaseSet(syms, enumSym) => 58 // TODO: Consider args
    case TypeConstructor.Region(sym) => 59 // TODO: Consider args
    case TypeConstructor.RegionToStar => 60
    case TypeConstructor.RegionWithoutRegion => 61
    case TypeConstructor.Error(id, kind) =>
      throw InternalCompilerException("Unexpected Error type constructor", SourceLocation.Unknown)
  }

  private def hashKindedTypeVarSym(sym0: Symbol.KindedTypeVarSym): Array[Byte] = ???

  private def hashInt(n: Int): Array[Byte] = {
    // TODO: Do not use toBinaryString, use actual bytes of the int instead.
    hashBytes(n.toBinaryString.getBytes)
  }

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

}
