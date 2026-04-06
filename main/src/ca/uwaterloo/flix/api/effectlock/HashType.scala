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
      val h2 = hashInt(2)
      val h3 = hashErasedType(tpe2)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, loc), arg, kind, _) =>
      val hn = hashInt(3)
      hashBytes(???)

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
      hashInt(32)

    case TypeConstructor.Struct(sym, kind) =>
      hashInt(33)

    case TypeConstructor.RestrictableEnum(sym, kind) =>
      hashInt(34)

    case TypeConstructor.Native(clazz) =>
      // TODO: Hash fully qualified name of clazz
      hashInt(35)

    case TypeConstructor.JvmConstructor(constructor) =>
      // TODO: Hash fully qualified name of clazz, <init> and parameter types (fully qualified names of those)
      hashInt(36)

    case TypeConstructor.JvmMethod(method) =>
      // TODO: Hash fully qualified name of clazz, method name and parameter types (fully qualified names of those)
      hashInt(37)

    case TypeConstructor.JvmField(field) =>
      // TODO: Hash fully qualified name of clazz, field name and type (fully qualified name of the type)
      hashInt(38)

    case TypeConstructor.Array =>
      hashInt(39)

    case TypeConstructor.ArrayWithoutRegion =>
      hashInt(40)

    case TypeConstructor.Vector =>
      hashInt(41)

    case TypeConstructor.Tuple(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(42)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Relation(arity) => hashInt(39)
      val h1 = hashInt(arity)
      val h2 = hashInt(43)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.Lattice(arity) =>
      val h1 = hashInt(arity)
      val h2 = hashInt(44)
      hashBytes(h1.appendedAll(h2))

    case TypeConstructor.True =>
      hashInt(45)

    case TypeConstructor.False =>
      hashInt(46)

    case TypeConstructor.Not =>
      hashInt(47)

    case TypeConstructor.And =>
      hashInt(48)

    case TypeConstructor.Or =>
      hashInt(49)

    case TypeConstructor.Pure =>
      hashInt(50)

    case TypeConstructor.Univ =>
      hashInt(51)

    case TypeConstructor.Complement =>
      hashInt(52)

    case TypeConstructor.Union =>
      hashInt(53)

    case TypeConstructor.Intersection =>
      hashInt(54)

    case TypeConstructor.Difference =>
      hashInt(55)

    case TypeConstructor.SymmetricDiff =>
      hashInt(56)

    case TypeConstructor.Effect(sym, kind) =>
      hashInt(57)

    case TypeConstructor.CaseComplement(sym) =>
      hashInt(58)

    case TypeConstructor.CaseUnion(sym) =>
      hashInt(59)

    case TypeConstructor.CaseIntersection(sym) =>
      hashInt(60)

    case TypeConstructor.CaseSymmetricDiff(sym) =>
      hashInt(61)

    case TypeConstructor.CaseSet(syms, enumSym) =>
      hashInt(62)

    case TypeConstructor.Region(sym) =>
      hashInt(63)

    case TypeConstructor.RegionToStar =>
      hashInt(64)

    case TypeConstructor.RegionWithoutRegion =>
      hashInt(65)

    case TypeConstructor.Error(_, _) =>
      throw InternalCompilerException("Unexpected Error type constructor", SourceLocation.Unknown)

  }

  private def hashKind(kind0: Kind): Array[Byte] = kind0 match {
    case Kind.Wild =>
      hashInt(66)

    case Kind.WildCaseSet =>
      hashInt(67)

    case Kind.Star =>
      hashInt(68)

    case Kind.Eff =>
      hashInt(69)

    case Kind.Bool =>
      hashInt(70)

    case Kind.RecordRow =>
      hashInt(71)

    case Kind.SchemaRow =>
      hashInt(72)

    case Kind.Predicate =>
      hashInt(73)

    case Kind.Jvm =>
      hashInt(74)

    case Kind.CaseSet(sym) =>
      // TODO: Hash sym
      val h1: Array[Byte] = ???
      val h2 = hashInt(75)
      hashBytes(h1.appendedAll(h2))

    case Kind.Arrow(k1, k2) =>
      val h1 = hashKind(k1)
      val h2 = hashInt(76)
      val h3 = hashKind(k2)
      hashBytes(h1.appendedAll(h2).appendedAll(h3))

    case Kind.Error =>
      throw InternalCompilerException("Unexpected Error kind", SourceLocation.Unknown)

  }

  private def hashLabel(label0: Name.Label): Array[Byte] = {
    // N.B.: Do not hash the source location of label
    hashBytes(hashString(label0.name).appendedAll(hashInt(77)))
  }

  private def hashPred(pred0: Name.Pred): Array[Byte] = {
    // N.B.: Do not hash the source location of predicate
    hashBytes(hashString(pred0.name).appendedAll(hashInt(78)))
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
