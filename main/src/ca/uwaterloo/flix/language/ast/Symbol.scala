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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.Name.{Ident, NName}
import ca.uwaterloo.flix.util.InternalCompilerException

object Symbol {

  /**
    * Returns a fresh definition symbol with the given text.
    */
  def freshDefnSym(text: String)(implicit genSym: GenSym): DefnSym = {
    val id = Some(genSym.freshId())
    new DefnSym(id, Nil, text, SourceLocation.Unknown)
  }

  /**
    * Returns a fresh definition symbol based on the given symbol.
    */
  def freshDefnSym(sym: DefnSym)(implicit genSym: GenSym): DefnSym = {
    val id = Some(genSym.freshId())
    new DefnSym(id, sym.namespace, sym.text, sym.loc)
  }

  /**
    * Returns a fresh variable symbol with no additional information.
    */
  def freshVarSym()(implicit genSym: GenSym): VarSym = {
    new VarSym(genSym.freshId(), "tmp", Type.freshTypeVar(), SourceLocation.Unknown)
  }

  /**
    * Returns a fresh variable symbol based on the given symbol.
    */
  def freshVarSym(sym: VarSym)(implicit genSym: GenSym): VarSym = {
    new VarSym(genSym.freshId(), sym.text, sym.tvar, sym.loc)
  }

  /**
    * Returns a fresh variable symbol for the given identifier.
    */
  def freshVarSym(ident: Name.Ident)(implicit genSym: GenSym): VarSym = {
    new VarSym(genSym.freshId(), ident.name, Type.freshTypeVar(), ident.loc)
  }

  /**
    * Returns a fresh variable symbol with the given text.
    */
  def freshVarSym(text: String)(implicit genSym: GenSym): VarSym = {
    new VarSym(genSym.freshId(), text, Type.freshTypeVar(), SourceLocation.Unknown)
  }

  /**
    * Returns a label symbol with the given text.
    */
  def freshLabel(text: String)(implicit genSym: GenSym): LabelSym = {
    new LabelSym(genSym.freshId(), text)
  }

  /**
    * Returns a fresh label symbol with the same text as the given label.
    */
  def freshLabel(sym: LabelSym)(implicit genSym: GenSym): LabelSym = {
    new LabelSym(genSym.freshId(), sym.text)
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
    * Returns the class symbol for the given name `ident`.
    */
  def mkClassSym(ident: Ident): ClassSym = {
    new ClassSym(ident.name, ident.loc)
  }

  /**
    * Returns the impl symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkImplSym(ident: Ident): ImplSym = {
    new ImplSym(ident.name, ident.loc)
  }

  /**
    * Returns the table symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkTableSym(ns: NName, ident: Ident): TableSym = {
    new TableSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the table symbol for the given fully qualified name.
    */
  def mkTableSym(fqn: String): TableSym = split(fqn) match {
    case None => new TableSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new TableSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Variable Symbol.
    *
    * @param id   the globally unique name of the symbol.
    * @param text the original name, as it appears in the source code, of the symbol
    * @param tvar the type variable associated with the symbol.
    * @param loc  the source location associated with the symbol.
    */
  final class VarSym(val id: Int, val text: String, val tvar: Type.Var, val loc: SourceLocation) {

    /**
      * The internal stack offset. Computed during variable numbering.
      */
    private var stackOffset: Option[Int] = None

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
    override def toString: String = text + "$" + id
  }

  /**
    * Definition Symbol.
    */
  final class DefnSym(val id: Option[Int], val namespace: List[String], val text: String, val loc: SourceLocation) {

    /**
      * Returns the name of `this` symbol.
      */
    def name: String = id match {
      case None => text
      case Some(i) => text + "$" + i
    }

    /**
      * Returns the prefix as a list of strings.
      * For example, the prefix of the symbol "A.B.C/f" is List("A", "B", "C").
      * A symbol "f" corresponds to "Root/f", so its prefix is List("Root").
      */
    // TODO: Possibly remove?
    @deprecated("DO NOT USE", "0.2.0")
    def prefix: List[String] = namespace match {
      case Nil => List("Root")
      case xs => xs
    }

    /**
      * Returns the suffix as a string.
      * For example, the suffix of the symbol "A.B.C/f" is "f".
      */
    // TODO: Possibly remove?
    @deprecated("DO NOT USE", "0.2.0")
    def suffix: String = name.
      // Mangle the name w.r.t Java conventions.
      replace("+", "$plus").
      replace("-", "$minus").
      replace("*", "$times").
      replace("/", "$divide").
      replace("%", "$modulo").
      replace("**", "$exponentiate").
      replace("<", "$lt").
      replace("<=", "$le").
      replace(">", "$gt").
      replace(">=", "$ge").
      replace("==", "$eq").
      replace("!=", "$neq").
      replace("&&", "$land").
      replace("||", "$lor").
      replace("&", "$band").
      replace("|", "$bor").
      replace("^", "$bxor").
      replace("<<", "$lshift").
      replace(">>", "$rshift")

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
    override def toString: String = name
  }

  /**
    * Class Symbol.
    */
  final class ClassSym(val name: String, val loc: SourceLocation) {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: ClassSym => this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = name
  }

  /**
    * Impl Symbol.
    */
  final class ImplSym(val name: String, val loc: SourceLocation) {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: ImplSym => this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = name
  }

  /**
    * Table Symbol.
    */
  final class TableSym(val namespace: List[String], val name: String, val loc: SourceLocation) {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: TableSym => this.namespace == that.namespace && this.name == that.name
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
    override def toString: String = text + "$" + id
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
