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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Kind, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{InternalCompilerException, Sha256}

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

/**
  * Computes a cryptographic hash of a type scheme, a type, or a kind.
  *
  * The hash of a node is the digest of the hashes of its children followed by the hash of a tag
  * that names the node:
  *
  * {{{
  *   H(C(x_1, ..., x_n)) = SHA256(H(x_1) || ... || H(x_n) || H("C"))
  * }}}
  *
  * Every hash is 32 bytes, so the bytes that go into a digest can be read back apart: the last 32
  * bytes are the tag, the tag says how many children the node has, and the children fill the rest.
  * A node of variable arity - a namespace, a case set, a list of constraints - is prefixed by the
  * hash of its length, so its arity can be read back too. Two distinct nodes therefore hash
  * distinct byte sequences, and finding two that hash alike means finding a collision of SHA-256.
  *
  * Tagging each node also keeps nodes that carry the same children apart: an enum symbol and a
  * struct symbol both consist of a namespace and a name, and the tag is what separates them. The
  * tag goes last so that a length extension of the bytes of one node describes a node that does
  * not exist.
  *
  * The hash identifies a type up to the equality the compiler uses, and no finer:
  *
  *   - Type aliases are erased, because an alias names a type rather than being one.
  *   - Type variables are identified by the order in which they are first met, not by their
  *     symbol, so renaming a type parameter does not change the hash.
  *   - Source locations are never hashed.
  *
  * The hash is structural, so it does not see that two types are equal for a reason the shape of
  * the type does not show. `A + B` and `B + A` denote the same effect but hash differently, as do
  * two schemes that differ only in the order of their constraints. Equal hashes therefore mean
  * that two signatures are the same, whereas different hashes mean only that they may differ.
  */
object HashType {

  /**
    * The version of the hashing scheme itself.
    *
    * Every hash is seeded with it, so that a deliberate change to how hashes are computed can be
    * told apart from a change to the thing being hashed. Bump it whenever a rule below changes.
    */
  private val Version: Int = 1

  /**
    * Returns the hash of `sc0`.
    *
    * Every variable is identified by where it first occurs, whether the scheme quantifies it or
    * not, so a scheme that leaves a variable free hashes as though the variable were one of its
    * own. A declared scheme is closed, so this does not arise for one.
    */
  def hashScheme(sc0: Scheme): Sha256 = versioned(visitScheme(sc0)(new LocalContext))

  /**
    * Returns the hash of `tpe0`, with every type variable it contains treated as bound by it.
    */
  def hashType(tpe0: Type): Sha256 = versioned(visitType(tpe0)(new LocalContext))

  /**
    * Returns the hash of `kind0`.
    */
  def hashKind(kind0: Kind): Sha256 = versioned(visitKind(kind0))

  /**
    * Returns the hash of `sc0`.
    *
    * The base type is hashed first, so that the variables are numbered by where they occur in the
    * type rather than by where they occur in a constraint. The quantifiers are hashed last, by the
    * indices that the rest of the scheme has by then given them: the list of quantifiers is not
    * hashed as it stands, because its order comes from the identifiers of the variables and so is
    * not stable. A quantifier that the scheme does not use does not affect the hash.
    */
  private def visitScheme(sc0: Scheme)(implicit lctx: LocalContext): Array[Byte] = sc0 match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      val h1 = visitType(base)
      val h2 = seq("Scheme.TraitConstraints", tconstrs.map(visitTraitConstraint))
      val h3 = seq("Scheme.EqualityConstraints", econstrs.map(visitEqualityConstraint))
      val h4 = seq("Scheme.Quantifiers", quantifiers.flatMap(lctx.indexOpt).sorted.map(hashInt))
      node("Scheme", h1, h2, h3, h4)
  }

  private def visitTraitConstraint(tconstr0: TraitConstraint)(implicit lctx: LocalContext): Array[Byte] = tconstr0 match {
    case TraitConstraint(symUse, arg, _) =>
      node("TraitConstraint", visitTraitSym(symUse.sym), visitType(arg))
  }

  private def visitEqualityConstraint(econstr0: EqualityConstraint)(implicit lctx: LocalContext): Array[Byte] = econstr0 match {
    case EqualityConstraint(symUse, tpe1, tpe2, _) =>
      node("EqualityConstraint", visitAssocTypeSym(symUse.sym), visitType(tpe1), visitType(tpe2))
  }

  private def visitType(tpe0: Type)(implicit lctx: LocalContext): Array[Byte] = tpe0 match {
    case Type.Var(sym, _) =>
      // N.B.: Only the index and the kind of the variable are hashed. Its name, its identifier,
      // and whether it is slack are not: none of them changes which type this is.
      node("Type.Var", hashInt(lctx.indexOf(sym)), visitKind(sym.kind))

    case Type.Cst(tc, loc) =>
      node("Type.Cst", visitTypeConstructor(tc)(loc))

    case Type.Apply(tpe1, tpe2, _) =>
      node("Type.Apply", visitType(tpe1), visitType(tpe2))

    case Type.Alias(_, _, tpe, _) =>
      // An alias names a type rather than being one, so it is erased.
      visitType(tpe)

    case Type.AssocType(symUse, arg, kind, _) =>
      node("Type.AssocType", visitAssocTypeSym(symUse.sym), visitType(arg), visitKind(kind))

    case Type.JvmToType(_, loc) =>
      throw InternalCompilerException(s"Unexpected type: '$tpe0'", loc)

    case Type.JvmToEff(_, loc) =>
      throw InternalCompilerException(s"Unexpected type: '$tpe0'", loc)

    case Type.UnresolvedJvmType(_, loc) =>
      throw InternalCompilerException(s"Unexpected type: '$tpe0'", loc)
  }

  private def visitTypeConstructor(tc0: TypeConstructor)(implicit loc0: SourceLocation): Array[Byte] = tc0 match {
    case TypeConstructor.Void => tcon("Void")
    case TypeConstructor.AnyType => tcon("AnyType")
    case TypeConstructor.Unit => tcon("Unit")
    case TypeConstructor.Null => tcon("Null")
    case TypeConstructor.Bool => tcon("Bool")
    case TypeConstructor.Char => tcon("Char")
    case TypeConstructor.Float32 => tcon("Float32")
    case TypeConstructor.Float64 => tcon("Float64")
    case TypeConstructor.BigDecimal => tcon("BigDecimal")
    case TypeConstructor.Int8 => tcon("Int8")
    case TypeConstructor.Int16 => tcon("Int16")
    case TypeConstructor.Int32 => tcon("Int32")
    case TypeConstructor.Int64 => tcon("Int64")
    case TypeConstructor.BigInt => tcon("BigInt")
    case TypeConstructor.Str => tcon("Str")
    case TypeConstructor.Regex => tcon("Regex")
    case TypeConstructor.Arrow(arity) => tcon("Arrow", hashInt(arity))
    case TypeConstructor.RecordRowEmpty => tcon("RecordRowEmpty")
    case TypeConstructor.RecordRowExtend(label) => tcon("RecordRowExtend", visitLabel(label))
    case TypeConstructor.Record => tcon("Record")
    case TypeConstructor.Extensible => tcon("Extensible")
    case TypeConstructor.SchemaRowEmpty => tcon("SchemaRowEmpty")
    case TypeConstructor.SchemaRowExtend(pred) => tcon("SchemaRowExtend", visitPred(pred))
    case TypeConstructor.Schema => tcon("Schema")
    case TypeConstructor.Sender => tcon("Sender")
    case TypeConstructor.Receiver => tcon("Receiver")
    case TypeConstructor.Lazy => tcon("Lazy")
    case TypeConstructor.Enum(sym, kind) => tcon("Enum", visitEnumSym(sym), visitKind(kind))
    case TypeConstructor.Struct(sym, kind) => tcon("Struct", visitStructSym(sym), visitKind(kind))
    case TypeConstructor.RestrictableEnum(sym, kind) => tcon("RestrictableEnum", visitRestrictableEnumSym(sym), visitKind(kind))
    case TypeConstructor.Native(desc, arity) => tcon("Native", hashString(desc.descriptorString()), hashInt(arity))
    case TypeConstructor.Array => tcon("Array")
    case TypeConstructor.Vector => tcon("Vector")
    case TypeConstructor.Tuple(arity) => tcon("Tuple", hashInt(arity))
    case TypeConstructor.Relation(arity) => tcon("Relation", hashInt(arity))
    case TypeConstructor.Lattice(arity) => tcon("Lattice", hashInt(arity))
    case TypeConstructor.True => tcon("True")
    case TypeConstructor.False => tcon("False")
    case TypeConstructor.Not => tcon("Not")
    case TypeConstructor.And => tcon("And")
    case TypeConstructor.Or => tcon("Or")
    case TypeConstructor.Pure => tcon("Pure")
    case TypeConstructor.Univ => tcon("Univ")
    case TypeConstructor.Complement => tcon("Complement")
    case TypeConstructor.Union => tcon("Union")
    case TypeConstructor.Intersection => tcon("Intersection")
    case TypeConstructor.Difference => tcon("Difference")
    case TypeConstructor.SymmetricDiff => tcon("SymmetricDiff")
    case TypeConstructor.Effect(sym, kind) => tcon("Effect", visitEffSym(sym), visitKind(kind))
    case TypeConstructor.CaseComplement(sym) => tcon("CaseComplement", visitRestrictableEnumSym(sym))
    case TypeConstructor.CaseUnion(sym) => tcon("CaseUnion", visitRestrictableEnumSym(sym))
    case TypeConstructor.CaseIntersection(sym) => tcon("CaseIntersection", visitRestrictableEnumSym(sym))
    case TypeConstructor.CaseSymmetricDiff(sym) => tcon("CaseSymmetricDiff", visitRestrictableEnumSym(sym))
    // N.B.: The symbols are already in a canonical order, since they are held in a sorted set.
    case TypeConstructor.CaseSet(syms, enumSym) => tcon("CaseSet", seq("CaseSet.Cases", syms.toList.map(visitRestrictableCaseSym)), visitRestrictableEnumSym(enumSym))
    case TypeConstructor.Region(sym) => tcon("Region", visitRegionSym(sym))
    case TypeConstructor.RegionToStar => tcon("RegionToStar")
    case TypeConstructor.ArrowWithoutEffect(_) => throw InternalCompilerException(s"Unexpected type constructor: '$tc0'", loc0)
    case TypeConstructor.ArrayWithoutRegion => throw InternalCompilerException(s"Unexpected type constructor: '$tc0'", loc0)
    case TypeConstructor.RegionWithoutRegion => throw InternalCompilerException(s"Unexpected type constructor: '$tc0'", loc0)
    case TypeConstructor.JvmConstructor(_) => throw InternalCompilerException(s"Unexpected type constructor: '$tc0'", loc0)
    case TypeConstructor.JvmMethod(_, _) => throw InternalCompilerException(s"Unexpected type constructor: '$tc0'", loc0)
    case TypeConstructor.JvmField(_) => throw InternalCompilerException(s"Unexpected type constructor: '$tc0'", loc0)
    case TypeConstructor.Error(_, _) => throw InternalCompilerException(s"Unexpected type constructor: '$tc0'", loc0)
  }

  private def visitKind(kind0: Kind): Array[Byte] = kind0 match {
    case Kind.Wild => node("Kind.Wild")
    case Kind.WildCaseSet => node("Kind.WildCaseSet")
    case Kind.Star => node("Kind.Star")
    case Kind.Eff => node("Kind.Eff")
    case Kind.Bool => node("Kind.Bool")
    case Kind.RecordRow => node("Kind.RecordRow")
    case Kind.SchemaRow => node("Kind.SchemaRow")
    case Kind.Predicate => node("Kind.Predicate")
    case Kind.CaseSet(sym) => node("Kind.CaseSet", visitRestrictableEnumSym(sym))
    case Kind.Arrow(k1, k2) => node("Kind.Arrow", visitKind(k1), visitKind(k2))
    case Kind.Jvm => throw InternalCompilerException(s"Unexpected kind: '$kind0'", SourceLocation.Unknown)
    case Kind.Error => throw InternalCompilerException(s"Unexpected kind: '$kind0'", SourceLocation.Unknown)
  }

  private def visitAssocTypeSym(sym0: Symbol.AssocTypeSym): Array[Byte] = {
    node("Symbol.AssocTypeSym", visitTraitSym(sym0.trt), hashString(sym0.name))
  }

  private def visitTraitSym(sym0: Symbol.TraitSym): Array[Byte] = {
    node("Symbol.TraitSym", visitNamespace(sym0.namespace), hashString(sym0.name))
  }

  private def visitEnumSym(sym0: Symbol.EnumSym): Array[Byte] = {
    // An enum symbol carries an identifier only once it has been specialized, which happens long
    // after a declared scheme has been fixed.
    if (sym0.id.isDefined) {
      throw InternalCompilerException(s"Unexpected specialized enum symbol: '$sym0'", sym0.loc)
    }
    node("Symbol.EnumSym", visitNamespace(sym0.namespace), hashString(sym0.text))
  }

  private def visitStructSym(sym0: Symbol.StructSym): Array[Byte] = {
    // See the note in `visitEnumSym`.
    if (sym0.id.isDefined) {
      throw InternalCompilerException(s"Unexpected specialized struct symbol: '$sym0'", sym0.loc)
    }
    node("Symbol.StructSym", visitNamespace(sym0.namespace), hashString(sym0.text))
  }

  private def visitRestrictableEnumSym(sym0: Symbol.RestrictableEnumSym): Array[Byte] = {
    // N.B.: The universe of the enum is not hashed, since it is not part of the identity of the
    // symbol: two restrictable enum symbols are equal when their namespace and name are equal.
    node("Symbol.RestrictableEnumSym", visitNamespace(sym0.namespace), hashString(sym0.name))
  }

  private def visitRestrictableCaseSym(sym0: Symbol.RestrictableCaseSym): Array[Byte] = {
    node("Symbol.RestrictableCaseSym", visitRestrictableEnumSym(sym0.enumSym), hashString(sym0.name))
  }

  private def visitEffSym(sym0: Symbol.EffSym): Array[Byte] = {
    node("Symbol.EffSym", visitNamespace(sym0.namespace), hashString(sym0.name))
  }

  private def visitRegionSym(sym0: Symbol.RegionSym): Array[Byte] = {
    // N.B.: The identifier of the symbol is not hashed, since it is handed out by a counter that
    // runs over the whole program and so differs between compilations.
    node("Symbol.RegionSym", hashString(sym0.text))
  }

  private def visitNamespace(ns0: List[String]): Array[Byte] = {
    // N.B.: Each part is hashed on its own, so that the namespaces 'Foo.Bar' and 'Foob.Ar' do not
    // hash the same bytes, and the parts are counted, so that a namespace cannot be read as a
    // longer one with an empty tail.
    seq("Namespace", ns0.map(hashString))
  }

  private def visitLabel(label0: Name.Label): Array[Byte] = {
    node("Name.Label", hashString(label0.name))
  }

  private def visitPred(pred0: Name.Pred): Array[Byte] = {
    node("Name.Pred", hashString(pred0.name))
  }

  /**
    * Returns the hash of a node tagged `tag` whose children hash to `parts`.
    */
  private def node(tag: String, parts: Array[Byte]*): Array[Byte] = {
    Sha256.rawOfBytes(Array.concat((parts :+ hashString(tag)): _*))
  }

  /**
    * Returns the hash of a type constructor named `name` whose children hash to `parts`.
    *
    * The name is qualified, because the names of the type constructors are not unique on their
    * own: `Bool` and `Arrow`, among others, name both a type constructor and a kind.
    */
  private def tcon(name: String, parts: Array[Byte]*): Array[Byte] = {
    node("TypeConstructor." + name, parts: _*)
  }

  /**
    * Returns the hash of a node tagged `tag` whose children hash to `parts`, of which there may be
    * any number.
    *
    * The number of children is hashed along with them, so that the children of one node cannot be
    * read as the children of a node that holds fewer.
    */
  private def seq(tag: String, parts: Seq[Array[Byte]]): Array[Byte] = {
    node(tag, (hashInt(parts.length) +: parts): _*)
  }

  /**
    * Returns the hash of `n`.
    */
  private def hashInt(n: Int): Array[Byte] = {
    // N.B.: An Int is four bytes, written big-endian so that the bytes do not depend on the host.
    Sha256.rawOfBytes(ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(n).array())
  }

  /**
    * Returns the hash of `s`.
    */
  private def hashString(s: String): Array[Byte] = {
    // N.B.: Encoded as UTF-8 so that the bytes do not depend on the host.
    Sha256.rawOfBytes(s.getBytes(StandardCharsets.UTF_8))
  }

  /**
    * Returns `digest` sealed with the version of the hashing scheme.
    */
  private def versioned(digest: Array[Byte]): Sha256 = {
    Sha256.ofBytes(Array.concat(hashInt(Version), digest))
  }

  /**
    * The canonical index of each type variable met so far.
    *
    * A variable is named by the position at which it was first met, rather than by its symbol,
    * because the identifier of a type variable is handed out by a counter that runs over the whole
    * program: it is stable within one compilation and nowhere else. Numbering the variables as
    * they are met is what makes the hash both stable across compilations and blind to the renaming
    * of a type parameter.
    *
    * The numbering is shared by every kind, and the hash of a variable covers its kind as well as
    * its index, so two variables of different kinds cannot be confused.
    */
  private final class LocalContext {

    /**
      * The index of each variable, in the order the variables were met.
      */
    private val indices: mutable.Map[Symbol.KindedTypeVarSym, Int] = mutable.Map.empty

    /**
      * Returns the index of `sym`, giving it the next one if it has none.
      */
    def indexOf(sym: Symbol.KindedTypeVarSym): Int = indices.getOrElseUpdate(sym, indices.size)

    /**
      * Returns the index of `sym`, or `None` if `sym` has not been met.
      */
    def indexOpt(sym: Symbol.KindedTypeVarSym): Option[Int] = indices.get(sym)

  }

}
