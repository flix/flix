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

import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.security.MessageDigest

object HashType {

  def hashType(tpe0: Type): Array[Byte] = hashErasedType(eraseAliases(tpe0))

  private def hashErasedType(tpe0: Type): Array[Byte] = tpe0 match {
    case Type.Var(sym, _) =>
      val h = hashKindedTypeVarSym(sym)
      // TODO: get byte representation of Type.Var
      hashBytes(???)

    case Type.Cst(tc, _) =>
      val h = hashTypeConstructor(tc)
      // TODO: get byte representation of Type.Cst
      hashBytes(h.appended(???))

    case Type.Apply(tpe1, tpe2, _) =>
      val h1 = hashErasedType(tpe1)
      val h2 = hashErasedType(tpe2)
      // TODO: get byte representation of Type.Apply
      val h3 = h1.appended(???).appendedAll(h2)
      hashBytes(h3)

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, loc), arg, kind, _) => ???

    case Type.Alias(_, _, _, loc) => throw InternalCompilerException("Unexpected type alias", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
  }

  private def hashTypeConstructor(tc0: TypeConstructor): Array[Byte] = tc0 match {
    case TypeConstructor.Void => hashInt(0)
    case TypeConstructor.AnyType => hashInt(1)
    case TypeConstructor.Unit => hashInt(2)
    case TypeConstructor.Null => hashInt(3)
    case TypeConstructor.Bool => hashInt(4)
    case TypeConstructor.Char => hashInt(5)
    case TypeConstructor.Float32 => hashInt(6)
    case TypeConstructor.Float64 => hashInt(7)
    case TypeConstructor.BigDecimal => hashInt(8)
    case TypeConstructor.Int8 => hashInt(9)
    case TypeConstructor.Int16 => hashInt(10)
    case TypeConstructor.Int32 => hashInt(11)
    case TypeConstructor.Int64 => hashInt(12)
    case TypeConstructor.BigInt => hashInt(13)
    case TypeConstructor.Str => hashInt(14)
    case TypeConstructor.Regex => hashInt(15)
    case TypeConstructor.Arrow(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(16)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.ArrowWithoutEffect(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(17)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.RecordRowEmpty => hashInt(18)
    case TypeConstructor.RecordRowExtend(label) =>
      val h1 = hashLabel(label)
      val h2 = hashInt(19)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Record => hashInt(20)
    case TypeConstructor.Extensible => hashInt(21)
    case TypeConstructor.SchemaRowEmpty => hashInt(22)
    case TypeConstructor.SchemaRowExtend(pred) => hashInt(23)

    case TypeConstructor.Schema => hashInt(24)
    case TypeConstructor.Sender => hashInt(25)
    case TypeConstructor.Receiver => hashInt(26)
    case TypeConstructor.Lazy => hashInt(27)
    case TypeConstructor.Enum(sym, kind) => hashInt(28)
    case TypeConstructor.Struct(sym, kind) => hashInt(29)
    case TypeConstructor.RestrictableEnum(sym, kind) => hashInt(30)
    case TypeConstructor.Native(clazz) => hashInt(31)
    case TypeConstructor.JvmConstructor(constructor) => hashInt(32)
    case TypeConstructor.JvmMethod(method) => hashInt(33)
    case TypeConstructor.JvmField(field) => hashInt(34)
    case TypeConstructor.Array => hashInt(35)
    case TypeConstructor.ArrayWithoutRegion => hashInt(36)
    case TypeConstructor.Vector => hashInt(37)
    case TypeConstructor.Tuple(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(38)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Relation(arity) => hashInt(39)
      val h1 = hashInt(arity)
      val h2 = hashInt(39)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Lattice(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(40)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.True => hashInt(41)
    case TypeConstructor.False => hashInt(42)
    case TypeConstructor.Not => hashInt(43)
    case TypeConstructor.And => hashInt(44)
    case TypeConstructor.Or => hashInt(45)
    case TypeConstructor.Pure => hashInt(46)
    case TypeConstructor.Univ => hashInt(47)
    case TypeConstructor.Complement => hashInt(48)
    case TypeConstructor.Union => hashInt(49)
    case TypeConstructor.Intersection => hashInt(50)
    case TypeConstructor.Difference => hashInt(51)
    case TypeConstructor.SymmetricDiff => hashInt(52)
    case TypeConstructor.Effect(sym, kind) => hashInt(53)
    case TypeConstructor.CaseComplement(sym) => hashInt(54)
    case TypeConstructor.CaseUnion(sym) => hashInt(55)
    case TypeConstructor.CaseIntersection(sym) => hashInt(56)
    case TypeConstructor.CaseSymmetricDiff(sym) => hashInt(57)
    case TypeConstructor.CaseSet(syms, enumSym) => hashInt(58)
    case TypeConstructor.Region(sym) => hashInt(59)
    case TypeConstructor.RegionToStar => hashInt(60)
    case TypeConstructor.RegionWithoutRegion => hashInt(61)
    case TypeConstructor.Error(_, _) =>
      throw InternalCompilerException("Unexpected Error type constructor", SourceLocation.Unknown)
  }

  private def hashLabel(label0: Name.Label): Array[Byte] = {
    hashString(label0.name)
  }

  private def hashKindedTypeVarSym(sym0: Symbol.KindedTypeVarSym): Array[Byte] = ???

  private def hashInt(n: Int): Array[Byte] = {
    // N.B.: 32-bit integer is 4 bytes, use big-endian to force consistent representation across platforms
    val bytes = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(n).array()
    hashBytes(bytes)
  }

  private def hashString(str: String): Array[Byte] = {
    // N.B.: Use UTF-8 to force consistent representation across platforms
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    hashBytes(bytes)
  }

  private def hashBytes(bytes: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(bytes)
    md.digest()
  }

}
