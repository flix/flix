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
import ca.uwaterloo.flix.language.ast.shared.{SymUse, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Name, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.security.MessageDigest

object HashType {

  def hashType(tpe0: Type): Array[Byte] = hashErasedType(eraseAliases(tpe0))

  private def hashErasedType(tpe0: Type): Array[Byte] = tpe0 match {
    case Type.Var(sym, _) =>
      val h1 = hashKindedTypeVarSym(sym)
      val h2 = hashInt(0)
      hashBytes(h1.appendedAll(h2))

    case Type.Cst(tc, _) =>
      val h1 = hashTypeConstructor(tc)
      val h2 = hashInt(1)
      hashBytes(h1.appendedAll(h2))

    case Type.Apply(tpe1, tpe2, _) =>
      val h1 = hashErasedType(tpe1)
      val h2 = hashErasedType(tpe2)
      val h3 = hashInt(2)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, _), arg, kind, _) =>
      val h1 = hashAssocTypeSym(sym)
      val h2 = hashErasedType(arg)
      val h3 = hashKind(kind)
      val h4 = hashInt(3)
      hashBytes(h1.appendedAll(h2).appendedAll(h3).appendedAll(h4))

    case Type.Alias(_, _, _, loc) => throw InternalCompilerException("Unexpected type alias", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
  }

  private def hashTypeConstructor(tc0: TypeConstructor): Array[Byte] = tc0 match {
    case TypeConstructor.Void =>
      hashInt(4)

    case TypeConstructor.AnyType =>
      hashInt(5)

    case TypeConstructor.Unit =>
      hashInt(6)

    case TypeConstructor.Null =>
      hashInt(7)

    case TypeConstructor.Bool =>
      hashInt(8)

    case TypeConstructor.Char =>
      hashInt(9)

    case TypeConstructor.Float32 =>
      hashInt(10)

    case TypeConstructor.Float64 =>
      hashInt(11)

    case TypeConstructor.BigDecimal =>
      hashInt(12)

    case TypeConstructor.Int8 =>
      hashInt(13)

    case TypeConstructor.Int16 =>
      hashInt(14)

    case TypeConstructor.Int32 =>
      hashInt(15)

    case TypeConstructor.Int64 =>
      hashInt(16)

    case TypeConstructor.BigInt =>
      hashInt(17)

    case TypeConstructor.Str =>
      hashInt(18)

    case TypeConstructor.Regex =>
      hashInt(19)

    case TypeConstructor.Arrow(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(20)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.ArrowWithoutEffect(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(21)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.RecordRowEmpty =>
      hashInt(22)

    case TypeConstructor.RecordRowExtend(label) =>
      val h1 = hashLabel(label)
      val h2 = hashInt(23)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Record =>
      hashInt(24)

    case TypeConstructor.Extensible =>
      hashInt(25)

    case TypeConstructor.SchemaRowEmpty =>
      hashInt(26)

    case TypeConstructor.SchemaRowExtend(pred) =>
      val h1 = hashPred(pred)
      val h2 = hashInt(27)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Schema =>
      hashInt(28)

    case TypeConstructor.Sender =>
      hashInt(29)

    case TypeConstructor.Receiver =>
      hashInt(30)

    case TypeConstructor.Lazy =>
      hashInt(31)

    case TypeConstructor.Enum(sym, kind) =>
      val h1 = hashEnumSym(sym)
      val h2 = hashKind(kind)
      val h3 = hashInt(32)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case TypeConstructor.Struct(sym, kind) =>
      val h1 = hashStructSym(sym)
      val h2 = hashKind(kind)
      val h3 = hashInt(33)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case TypeConstructor.RestrictableEnum(sym, kind) =>
      val h1 = hashRestrictableEnumSym(sym)
      val h2 = hashKind(kind)
      val h3 = hashInt(34)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case TypeConstructor.Native(clazz) =>
      val h1 = hashNativeClass(clazz)
      val h2 = hashInt(35)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Array =>
      hashInt(36)

    case TypeConstructor.ArrayWithoutRegion =>
      hashInt(37)

    case TypeConstructor.Vector =>
      hashInt(38)

    case TypeConstructor.Tuple(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(39)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Relation(arity) => hashInt(39)
      val h1 = hashInt(arity)
      val h2 = hashInt(40)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Lattice(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(41)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.True =>
      hashInt(42)

    case TypeConstructor.False =>
      hashInt(43)

    case TypeConstructor.Not =>
      hashInt(44)

    case TypeConstructor.And =>
      hashInt(45)

    case TypeConstructor.Or =>
      hashInt(46)

    case TypeConstructor.Pure =>
      hashInt(47)

    case TypeConstructor.Univ =>
      hashInt(48)

    case TypeConstructor.Complement =>
      hashInt(49)

    case TypeConstructor.Union =>
      hashInt(50)

    case TypeConstructor.Intersection =>
      hashInt(51)

    case TypeConstructor.Difference =>
      hashInt(52)

    case TypeConstructor.SymmetricDiff =>
      hashInt(53)

    case TypeConstructor.Effect(sym, kind) =>
      val h1 = hashEffSym(sym)
      val h2 = hashKind(kind)
      val h3 = hashInt(54)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case TypeConstructor.CaseComplement(sym) =>
      val h1 = hashRestrictableEnumSym(sym)
      val h2 = hashInt(55)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.CaseUnion(sym) =>
      val h1 = hashRestrictableEnumSym(sym)
      val h2 = hashInt(56)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.CaseIntersection(sym) =>
      val h1 = hashRestrictableEnumSym(sym)
      val h2 = hashInt(57)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.CaseSymmetricDiff(sym) =>
      val h1 = hashRestrictableEnumSym(sym)
      val h2 = hashInt(58)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.CaseSet(syms, enumSym) =>
      val h1 = hashBytes(syms.toArray.flatMap(hashRestrictableCaseSym))
      val h2 = hashRestrictableEnumSym(enumSym)
      val h3 = hashInt(59)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case TypeConstructor.Region(sym) =>
      val h1 = hashRegionSym(sym)
      val h2 = hashInt(60)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.RegionToStar =>
      hashInt(61)

    case TypeConstructor.RegionWithoutRegion =>
      hashInt(62)

    case TypeConstructor.JvmConstructor(_) =>
      throw InternalCompilerException("Unexpected type constructor: JvmConstructor", SourceLocation.Unknown)

    case TypeConstructor.JvmMethod(_) =>
      throw InternalCompilerException("Unexpected type constructor: JvmMethod", SourceLocation.Unknown)

    case TypeConstructor.JvmField(_) =>
      throw InternalCompilerException("Unexpected type constructor: JvmField", SourceLocation.Unknown)

    case TypeConstructor.Error(_, _) =>
      throw InternalCompilerException("Unexpected Error type constructor", SourceLocation.Unknown)

  }

  private def hashKind(kind0: Kind): Array[Byte] = kind0 match {
    case Kind.Wild =>
      hashInt(63)

    case Kind.WildCaseSet =>
      hashInt(64)

    case Kind.Star =>
      hashInt(65)

    case Kind.Eff =>
      hashInt(66)

    case Kind.Bool =>
      hashInt(67)

    case Kind.RecordRow =>
      hashInt(68)

    case Kind.SchemaRow =>
      hashInt(69)

    case Kind.Predicate =>
      hashInt(70)

    case Kind.Jvm =>
      hashInt(71)

    case Kind.CaseSet(sym) =>
      val h1 = hashRestrictableEnumSym(sym)
      val h2 = hashInt(72)
      hashBytes(h1.appendedAll(h2))

    case Kind.Arrow(k1, k2) =>
      val h1 = hashKind(k1)
      val h2 = hashKind(k2)
      val h3 = hashInt(73)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case Kind.Error =>
      throw InternalCompilerException("Unexpected Error kind", SourceLocation.Unknown)

  }

  private def hashLabel(label0: Name.Label): Array[Byte] = {
    // N.B.: Do not hash the source location of label
    val h1 = hashString(label0.name)
    val h2 = hashInt(74)
    hashBytes(h1.appendedAll(h2))
  }

  private def hashPred(pred0: Name.Pred): Array[Byte] = {
    // N.B.: Do not hash the source location of predicate
    val h1 = hashString(pred0.name)
    val h2 = hashInt(75)
    hashBytes(h1.appendedAll(h2))
  }

  /**
    * Hashes the fully qualified name of `clazz0` along with a
    * unique id representing the [[Class]] type.
    */
  private def hashNativeClass(clazz0: Class[?]): Array[Byte] = {
    val h1 = hashString(clazz0.getName)
    val h2 = hashInt(76)
    hashBytes(h1.appendedAll(h2))
  }

  private def hashKindedTypeVarSym(sym0: Symbol.KindedTypeVarSym): Array[Byte] = {
    val h1 = hashInt(sym0.id)
    val h2 = hashVarText(sym0.text)
    val h3 = hashKind(sym0.kind)
    val h4 = hashInt(77)
    hashBytes(h1.appendedAll(h2).appendedAll(h3).appendedAll(h4))
  }

  private def hashAssocTypeSym(sym0: Symbol.AssocTypeSym): Array[Byte] = {
    val h1 = hashTraitSym(sym0.trt)
    val h2 = hashString(sym0.name)
    val h3 = hashInt(78)
    hashBytes(h1.appendedAll(h2).appendedAll(h3))
  }

  private def hashEnumSym(sym0: Symbol.EnumSym): Array[Byte] = {
    val h1 = sym0.namespace.flatMap(hashString).toArray
    val h2 = hashString(sym0.text)
    val h3 = hashInt(79)
    hashBytes(h1.appendedAll(h2).appendedAll(h3))
  }

  private def hashStructSym(sym0: Symbol.StructSym): Array[Byte] = ???

  private def hashRestrictableEnumSym(sym0: Symbol.RestrictableEnumSym): Array[Byte] = ???

  private def hashEffSym(sym0: Symbol.EffSym): Array[Byte] = ???

  private def hashRestrictableCaseSym(sym0: Symbol.RestrictableCaseSym): Array[Byte] = ???

  private def hashRegionSym(sym0: Symbol.RegionSym): Array[Byte] = ???

  private def hashVarText(text0: VarText): Array[Byte] = text0 match {
    case VarText.Absent => hashInt(???)
    case VarText.SourceText(s) =>
      val h1 = hashString(s)
      val h2 = hashInt(???)
      hashBytes(h1.appendedAll(h2))
  }

  private def hashTraitSym(sym0: Symbol.TraitSym): Array[Byte] = {
    val h1 = sym0.namespace.flatMap(hashString).toArray
    val h2 = hashString(sym0.name)
    val h3 = hashInt(???)
    hashBytes(h1.appendedAll(h2).appendedAll(h3))
  }

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
