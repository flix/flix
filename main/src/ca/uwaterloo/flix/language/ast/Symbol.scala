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
import ca.uwaterloo.flix.language.ast.Name.{Ident, NName}
import ca.uwaterloo.flix.language.phase.sjvm.JvmName
import ca.uwaterloo.flix.language.phase.sjvm.SjvmOps.mangle
import ca.uwaterloo.flix.util.InternalCompilerException

object Symbol {

  /**
    * The main symbol.
    */
  val Main: Symbol.DefnSym = Symbol.mkDefnSym("main")

  /**
    * Returns a fresh def symbol based on the given symbol.
    */
  def freshDefnSym(sym: DefnSym)(implicit flix: Flix): DefnSym = {
    val id = Some(flix.genSym.freshId())
    new DefnSym(id, sym.namespace, sym.text, sym.loc)
  }

  /**
    * Returns a fresh def symbol with the given text.
    */
  def freshDefnSym(text: String, loc: SourceLocation)(implicit flix: Flix): DefnSym = {
    val id = Some(flix.genSym.freshId())
    new DefnSym(id, Nil, text, loc)
  }

  /**
    * Returns a fresh def symbol with the given text in the given namespace.
    */
  def freshDefnSym(ns: List[String], text: String, loc: SourceLocation)(implicit flix: Flix): DefnSym = {
    val id = Some(flix.genSym.freshId())
    new DefnSym(id, ns, text, loc)
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
    new VarSym(flix.genSym.freshId(), sym.text, sym.tvar, Scopedness.Unscoped, sym.loc)
  }

  /**
    * Returns a fresh variable symbol for the given identifier.
    */
  def freshVarSym(ident: Name.Ident)(implicit flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), ident.name, UnkindedType.freshVar(loc = ident.loc), Scopedness.Unscoped, ident.loc)
  }

  /**
    * Returns a fresh variable symbol for the given identifier and scopedness.
    */
  def freshVarSym(ident: Name.Ident, scopedness: Scopedness)(implicit flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), ident.name, UnkindedType.freshVar(loc = ident.loc), scopedness, ident.loc)
  }

  /**
    * Returns a fresh variable symbol with the given text.
    */
  def freshVarSym(text: String, loc: SourceLocation)(implicit flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), text, UnkindedType.freshVar(loc = loc), Scopedness.Unscoped, loc)
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
    * Returns the enum symbol for the given fully qualified name.
    */
  def mkEnumSym(fqn: String): EnumSym = split(fqn) match {
    case None => new EnumSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new EnumSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the class symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkClassSym(ns: NName, ident: Ident): ClassSym = {
    new ClassSym(ns.parts, ident.name, ident.loc)
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
    * Variable Symbol.
    *
    * @param id   the globally unique name of the symbol.
    * @param text the original name, as it appears in the source code, of the symbol
    * @param tvar the type variable associated with the symbol. This type variable always has kind `Star`.
    * @param loc  the source location associated with the symbol.
    */
  final class VarSym(val id: Int, val text: String, val tvar: UnkindedType.Var, val scopedness: Scopedness, val loc: SourceLocation) {

    /**
      * The internal stack offset. Computed during variable numbering.
      */
    private var stackOffset: Option[Int] = None

    /**
      * Returns `true`if `this` symbol is a wildcard.
      */
    def isWild(): Boolean = text.startsWith("_")

    /**
      * Returns the stack offset of `this` variable symbol.
      *
      * Throws [[InternalCompilerException]] if the stack offset has not been set.
      */
    def getStackOffset: Int = stackOffset match {
      case None => throw InternalCompilerException(s"Unknown offset for variable symbol $toString.")
      case Some(offset) => offset
    }

    /**
      * Sets the internal stack offset to given argument.
      */
    def setStackOffset(offset: Int): Unit = stackOffset match {
      case None => stackOffset = Some(offset)
      case Some(_) =>
        throw InternalCompilerException(s"Offset already set for variable symbol $toString.")
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
      * Human readable representation.
      */
    override def toString: String = text + JvmName.reservedDelimiter + id
  }

  /**
    * Definition Symbol.
    */
  final class DefnSym(val id: Option[Int], val namespace: List[String], val text: String, val loc: SourceLocation) {

    /**
      * Returns `true` if `this` symbol is equal to the main symbol.
      *
      * NB: Must use equality because there could be more than once instance of the main symbol.
      */
    def isMain: Boolean = this == Symbol.Main

    // TODO(JLS): Should maybe mangle here? (+ -> $add)
    lazy val defName: JvmName = JvmName(namespace, s"Def${JvmName.reservedDelimiter}${mangle(name)}")

    lazy val cloName: JvmName = JvmName(namespace, s"Clo${JvmName.reservedDelimiter}${mangle(name)}")

    lazy val nsMethodName: String = s"m${JvmName.reservedDelimiter}${mangle(name)}"

    /**
      * Returns the name of `this` symbol.
      */
    def name: String = id match {
      case None => text
      case Some(i) => text + JvmName.reservedDelimiter + i
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
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString("/") + "." + name
  }

  /**
    * Enum Symbol.
    */
  final class EnumSym(val namespace: List[String], val name: String, val loc: SourceLocation) {
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
    * Class Symbol.
    */
  final class ClassSym(val namespace: List[String], val name: String, val loc: SourceLocation) {
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
  }

  /**
    * Signature Symbol.
    */
  final class SigSym(val clazz: Symbol.ClassSym, val name: String, val loc: SourceLocation) {
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
  }

  /**
    * Label Symbol.
    */
  final class LabelSym(val id: Int, val text: String) {
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
    override def toString: String = text + JvmName.reservedDelimiter + id
  }

  /**
    * Hole Symbol.
    */
  final class HoleSym(val namespace: List[String], val name: String, val loc: SourceLocation) {
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
  final class TypeAliasSym(val namespace: List[String], val name: String, val loc: SourceLocation) {
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
