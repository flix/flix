/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{BoundBy, VarText}
import ca.uwaterloo.flix.language.ast.Name.{Ident, NName}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.util.Objects
import scala.collection.immutable.SortedSet

sealed trait Symbol

object Symbol {

  /**
    * Returns a fresh def symbol based on the given symbol.
    */
  def freshDefnSym(sym: DefnSym)(implicit flix: Flix): DefnSym = {
    val id = Some(flix.genSym.freshId())
    new DefnSym(id, sym.namespace, sym.text, sym.loc)
  }

  /**
    * Returns a fresh hole symbol associated with the given source location `loc`.
    */
  def freshHoleSym(loc: SourceLocation)(implicit flix: Flix): HoleSym = {
    val id = flix.genSym.freshId()
    new HoleSym(Nil, "h" + id, loc)
  }

  /**
    * Returns a fresh variable symbol based on the given symbol.
    */
  def freshVarSym(sym: VarSym)(implicit flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), sym.text, sym.tvar, sym.boundBy, sym.loc)
  }

  /**
    * Returns a fresh variable symbol for the given identifier.
    */
  def freshVarSym(ident: Name.Ident, boundBy: BoundBy)(implicit flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), ident.name, Type.freshVar(Kind.Star, ident.loc), boundBy, ident.loc)
  }

  /**
    * Returns a fresh variable symbol with the given text.
    */
  def freshVarSym(text: String, boundBy: BoundBy, loc: SourceLocation)(implicit flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), text, Type.freshVar(Kind.Star, loc), boundBy, loc)
  }

  /**
    * Returns a fresh type variable symbol with the given text.
    */
  def freshKindedTypeVarSym(text: Ast.VarText, kind: Kind, isRegion: Boolean, loc: SourceLocation)(implicit flix: Flix): KindedTypeVarSym = {
    new KindedTypeVarSym(flix.genSym.freshId(), text, kind, isRegion, loc)
  }

  /**
    * Returns a fresh type variable symbol with the given text.
    */
  def freshUnkindedTypeVarSym(text: Ast.VarText, isRegion: Boolean, loc: SourceLocation)(implicit flix: Flix): UnkindedTypeVarSym = {
    new UnkindedTypeVarSym(flix.genSym.freshId(), text, isRegion: Boolean, loc)
  }

  /**
    * Returns a label symbol with the given text.
    */
  def freshLabel(text: String)(implicit flix: Flix): LabelSym = {
    new LabelSym(flix.genSym.freshId(), text)
  }

  /**
    * Returns a fresh label symbol with the same text as the given label.
    */
  def freshLabel(sym: LabelSym)(implicit flix: Flix): LabelSym = {
    new LabelSym(flix.genSym.freshId(), sym.text)
  }

  /**
    * Returns the definition symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkDefnSym(ns: NName, ident: Ident): DefnSym = {
    new DefnSym(None, ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the definition symbol for the given fully qualified name.
    */
  def mkDefnSym(fqn: String): DefnSym = split(fqn) match {
    case None => new DefnSym(None, Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new DefnSym(None, ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the enum symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkEnumSym(ns: NName, ident: Ident): EnumSym = {
    new EnumSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the restrictable enum symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkRestrictableEnumSym(ns: NName, ident: Ident, cases: List[Ident]): RestrictableEnumSym = {
    new RestrictableEnumSym(ns.parts, ident.name, cases, ident.loc)
  }

  /**
    * Returns the enum symbol for the given fully qualified name.
    */
  def mkEnumSym(fqn: String): EnumSym = split(fqn) match {
    case None => new EnumSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new EnumSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the case symbol for the given name `ident` in the given `enum`.
    */
  def mkCaseSym(sym: Symbol.EnumSym, ident: Ident): CaseSym = {
    new CaseSym(sym, ident.name, ident.loc)
  }

  /**
    * Returns the restrictable case symbol for the given name `ident` in the given `enum`.
    */
  def mkRestrictableCaseSym(sym: Symbol.RestrictableEnumSym, ident: Ident): RestrictableCaseSym = {
    new RestrictableCaseSym(sym, ident.name, ident.loc)
  }

  /**
    * Returns the class symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkClassSym(ns: NName, ident: Ident): ClassSym = {
    new ClassSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the class symbol for the given fully qualified name
    */
  def mkClassSym(fqn: String): ClassSym = split(fqn) match {
    case None => new ClassSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new ClassSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the hole symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkHoleSym(ns: NName, ident: Ident): HoleSym = {
    new HoleSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the hole symbol for the given fully qualified name.
    */
  def mkHoleSym(fqn: String): HoleSym = split(fqn) match {
    case None => new HoleSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new HoleSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the signature symbol for the given name `ident` in the class associated with the given class symbol `classSym`.
    */
  def mkSigSym(classSym: ClassSym, ident: Name.Ident): SigSym = {
    new SigSym(classSym, ident.name, ident.loc)
  }

  /**
    * Returns the type alias symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkTypeAliasSym(ns: NName, ident: Ident): TypeAliasSym = {
    new TypeAliasSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the associated type symbol for the given name `ident` in the class associated with the given class symbol `classSym`.
    */
  def mkAssocTypeSym(classSym: ClassSym, ident: Name.Ident): AssocTypeSym = {
    new AssocTypeSym(classSym, ident.name, ident.loc)
  }

  /**
    * Returns the type alias symbol for the given fully qualified name
    */
  def mkTypeAliasSym(fqn: String): TypeAliasSym = split(fqn) match {
    case None => new TypeAliasSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new TypeAliasSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the effect symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkEffectSym(ns: NName, ident: Ident): EffectSym = {
    new EffectSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the operation symbol for the given name `ident` in the effect associated with the given effect symbol `effectSym`.
    */
  def mkOpSym(effectSym: EffectSym, ident: Name.Ident): OpSym = {
    new OpSym(effectSym, ident.name, ident.loc)
  }

  /**
    * Variable Symbol.
    *
    * @param id      the globally unique name of the symbol.
    * @param text    the original name, as it appears in the source code, of the symbol
    * @param tvar    the type variable associated with the symbol. This type variable always has kind `Star`.
    * @param boundBy the way the variable is bound.
    * @param loc     the source location associated with the symbol.
    */
  final class VarSym(val id: Int, val text: String, val tvar: Type.Var, val boundBy: BoundBy, val loc: SourceLocation) extends Ordered[VarSym] with Symbol {

    /**
      * The internal stack offset. Computed during variable numbering.
      */
    private var stackOffset: Option[Int] = None

    /**
      * Returns `true` if `this` symbol is a wildcard.
      */
    def isWild: Boolean = text.startsWith("_")

    /**
      * Returns the stack offset of `this` variable symbol.
      *
      * Throws [[InternalCompilerException]] if the stack offset has not been set.
      */
    def getStackOffset: Int = stackOffset match {
      case None => throw InternalCompilerException(s"Unknown offset for variable symbol $toString.", loc)
      case Some(offset) => offset
    }

    /**
      * Sets the internal stack offset to given argument.
      */
    def setStackOffset(offset: Int): Unit = stackOffset match {
      case None => stackOffset = Some(offset)
      case Some(_) =>
        throw InternalCompilerException(s"Offset already set for variable symbol: '$toString' near ${loc.format}.", loc)
    }

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: VarSym => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = id

    /**
      * Return the comparison of `this` symbol to `that` symol.
      */
    override def compare(that: VarSym): Int = this.id.compare(that.id)

    /**
      * Human readable representation.
      */
    override def toString: String = text + Flix.Delimiter + id
  }

  /**
    * Kinded type variable symbol.
    */
  final class KindedTypeVarSym(val id: Int, val text: Ast.VarText, val kind: Kind, val isRegion: Boolean, val loc: SourceLocation) extends Symbol with Ordered[KindedTypeVarSym] with Locatable with Sourceable {

    /**
      * Returns `true` if `this` variable is non-synthetic.
      */
    def isReal: Boolean = loc.locationKind == SourceKind.Real

    /**
      * Returns the same symbol with the given kind.
      */
    def withKind(newKind: Kind): KindedTypeVarSym = new KindedTypeVarSym(id, text, newKind, isRegion, loc)

    /**
      * Returns the same symbol without a kind.
      */
    def withoutKind: UnkindedTypeVarSym = new UnkindedTypeVarSym(id, text, isRegion, loc)

    def withText(newText: Ast.VarText): KindedTypeVarSym = new KindedTypeVarSym(id, newText, kind, isRegion, loc)

    override def compare(that: KindedTypeVarSym): Int = that.id - this.id

    override def equals(that: Any): Boolean = that match {
      case tvar: KindedTypeVarSym => this.id == tvar.id
      case _ => false
    }

    override val hashCode: Int = id

    /**
      * Returns a string representation of the symbol.
      */
    override def toString: String = {
      val string = text match {
        case VarText.Absent => "tvar"
        case VarText.SourceText(s) => s
      }
      string + Flix.Delimiter + id
    }

    /**
      * Returns true if this symbol is a wildcard.
      */
    def isWild: Boolean = text match {
      case VarText.Absent => false
      case VarText.SourceText(s) => s.startsWith("_")
    }
  }

  /**
    * Unkinded type variable symbol.
    */
  final class UnkindedTypeVarSym(val id: Int, val text: Ast.VarText, val isRegion: Boolean, val loc: SourceLocation) extends Symbol with Ordered[UnkindedTypeVarSym] with Locatable with Sourceable {

    /**
      * Ascribes this UnkindedTypeVarSym with the given kind.
      */
    def withKind(k: Kind): KindedTypeVarSym = new KindedTypeVarSym(id, text, k, isRegion, loc)

    override def compare(that: UnkindedTypeVarSym): Int = that.id - this.id

    override def equals(that: Any): Boolean = that match {
      case tvar: UnkindedTypeVarSym => this.id == tvar.id
      case _ => false
    }

    override val hashCode: Int = id

    /**
      * Returns a string representation of the symbol.
      */
    override def toString: String = {
      val string = text match {
        case VarText.Absent => "tvar"
        case VarText.SourceText(s) => s
      }
      string + Flix.Delimiter + id
    }
  }

  /**
    * Definition Symbol.
    */
  final class DefnSym(val id: Option[Int], val namespace: List[String], val text: String, val loc: SourceLocation) extends Sourceable with Locatable with Symbol {

    /**
      * Returns the name of `this` symbol.
      */
    def name: String = id match {
      case None => text
      case Some(i) => text + Flix.Delimiter + i
    }

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: DefnSym => this.id == that.id && this.namespace == that.namespace && this.text == that.text
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 5 * id.hashCode() + 7 * namespace.hashCode() + 11 * text.hashCode()

    /**
      * Human readable representation.
      */
    override val toString: String = if (namespace.isEmpty) name else namespace.mkString("/") + "." + name
  }

  /**
    * Enum Symbol.
    */
  final class EnumSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: EnumSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode() + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString("/") + "." + name
  }

  /**
    * Restrictable Enum Symbol.
    */
  final class RestrictableEnumSym(val namespace: List[String], val name: String, cases: List[Name.Ident], val loc: SourceLocation) extends Symbol {

    // NB: it is critical that this be either a lazy val or a def, since otherwise `this` is not fully instantiated
    /**
      * The universe of cases associated with this restrictable enum.
      */
    def universe: SortedSet[Symbol.RestrictableCaseSym] = cases.map(Symbol.mkRestrictableCaseSym(this, _)).to(SortedSet)

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: RestrictableEnumSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode() + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString("/") + "." + name
  }

  /**
    * Case Symbol.
    */
  final class CaseSym(val enumSym: Symbol.EnumSym, val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: CaseSym => this.enumSym == that.enumSym && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(enumSym, name)

    /**
      * Human readable representation.
      */
    override def toString: String = enumSym.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = enumSym.namespace :+ enumSym.name
  }

  /**
    * Restrictable Case Symbol.
    */
  final class RestrictableCaseSym(val enumSym: Symbol.RestrictableEnumSym, val name: String, val loc: SourceLocation) extends Symbol with Ordered[RestrictableCaseSym] {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: RestrictableCaseSym => this.enumSym == that.enumSym && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(enumSym, name)

    /**
      * Human readable representation.
      */
    override def toString: String = enumSym.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = enumSym.namespace :+ enumSym.name

    /**
      * Comparison.
      */
    override def compare(that: RestrictableCaseSym): Int = this.toString.compare(that.toString)

  }

  /**
    * Class Symbol.
    */
  final class ClassSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Sourceable with Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: ClassSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString("/") + "." + name

    /**
      * Returns the source of `this`.
      */
    override def src: Ast.Source = loc.source
  }

  /**
    * Signature Symbol.
    */
  final class SigSym(val clazz: Symbol.ClassSym, val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: SigSym => this.clazz == that.clazz && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * clazz.hashCode + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = clazz.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = clazz.namespace :+ clazz.name
  }

  /**
    * Label Symbol.
    */
  final class LabelSym(val id: Int, val text: String) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: LabelSym => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * id

    /**
      * Human readable representation.
      */
    override def toString: String = text + Flix.Delimiter + id
  }

  /**
    * Hole Symbol.
    */
  final class HoleSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: HoleSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode() + 11 * name.hashCode()

    /**
      * Human readable representation.
      */
    override def toString: String = "?" + (if (namespace.isEmpty) name else namespace.mkString("/") + "." + name)
  }

  /**
    * TypeAlias Symbol.
    */
  final class TypeAliasSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: TypeAliasSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode() + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = name
  }

  /**
    * Associated Type Symbol.
    */
  final class AssocTypeSym(val clazz: Symbol.ClassSym, val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AssocTypeSym => this.clazz == that.clazz && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(clazz, name)

    /**
      * Human readable representation.
      */
    override def toString: String = clazz.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = clazz.namespace :+ clazz.name
  }

  /**
    * Effect symbol.
    */
  final class EffectSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Sourceable with Ordered[EffectSym] with Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: EffectSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(namespace, name)

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString("/") + "." + name

    /**
      * Returns the source of `this`.
      */
    override def src: Ast.Source = loc.source

    /**
      * Compares `this` and `that` effect sym.
      *
      * Fairly arbitrary comparison since the purpose is to allow for mapping the object.
      */
    override def compare(that: EffectSym): Int = this.toString.compare(that.toString)
  }

  /**
    * Effect Operation Symbol.
    */
  final class OpSym(val eff: Symbol.EffectSym, val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: OpSym => this.eff == that.eff && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(eff, name)

    /**
      * Human readable representation.
      */
    override def toString: String = eff.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = eff.namespace :+ eff.name
  }

  /**
    * Module symbol.
    */
  final class ModuleSym(val ns: List[String]) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: ModuleSym => this.ns == that.ns
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(ns)

    /**
      * Human readable representation.
      */
    override def toString: String = ns.mkString("/")
  }

  /**
    * Optionally returns the namespace part and name of the given fully qualified string `fqn`.
    *
    * Returns `None` if the `fqn` is not qualified.
    */
  private def split(fqn: String): Option[(List[String], String)] = {
    if (!fqn.contains('.'))
      return None

    val index = fqn.indexOf('.')
    val namespace = fqn.substring(0, index).split('/').toList
    val name = fqn.substring(index + 1, fqn.length)
    Some((namespace, name))
  }

}
