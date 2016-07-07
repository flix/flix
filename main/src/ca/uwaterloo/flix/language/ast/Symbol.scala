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

import ca.uwaterloo.flix.language.ast.Name.{Ident, NName}
import ca.uwaterloo.flix.language.phase.GenSym

object Symbol {

  /**
    * Returns a fresh variable symbol for the given identifier.
    */
  def mkVarSym(ident: Name.Ident)(implicit genSym: GenSym): VarSym = {
    new VarSym(genSym.freshId(), ident.name, ident.loc)
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
  def mkTableSym(fqn: String): TableSym = {
    if (!fqn.contains('/'))
      return new TableSym(List.empty, fqn, SourceLocation.Unknown)

    val index = fqn.indexOf('/')
    val namespace = fqn.substring(0, index).split('.').toList
    val name = fqn.substring(index + 1, fqn.length)
    new TableSym(namespace, name, SourceLocation.Unknown)
  }


  /**
    * Variable Symbol.
    *
    * @param id   the globally unique name of the symbol.
    * @param text the original name, as it appears in the source code, of the symbol
    * @param loc  the source location associated with the symbol.
    */
  final class VarSym(val id: Int, val text: String, val loc: SourceLocation) {
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
    override val hashCode: Int = id.hashCode()

    /**
      * Human readable representation.
      */
    override def toString: String = text + "$" + id
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
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "/" + name
  }


  /**
    * Companion object for the [[Resolved]] class.
    */
  // TODO: deprecated
  object Resolved {

    def mk(name: String): Resolved = {
      if (name.contains("/")) {
        val index = name.indexOf("/")
        val (ns, ident) = name.splitAt(index)
        mk(ns.split("\\.").toList ::: ident.substring(1) :: Nil)
      } else
        mk(List(name))
    }

    def mk(parts: List[String]): Resolved = new Resolved(parts)
  }

  /**
    * Represents a resolved name.
    *
    * @param parts the parts of the name.
    */
  // TODO: deprecated
  final class Resolved private(val parts: List[String]) {

    /**
      * Returns the fully qualified name of `this` as a string.
      */
    def fqn: String = parts match {
      case x :: Nil => x
      case xs => xs.init.mkString(".") + "/" + xs.last
    }

    /**
      * Returns the prefix as a list of strings.
      * For example, the prefix of the symbol "A.B.C/f" is List("A", "B", "C").
      * A symbol "f" corresponds to "Root/f", so its prefix is List("Root").
      */
    def prefix: List[String] = parts match {
      case x :: Nil => List("Root")
      case xs => xs.init
    }

    /**
      * Returns the suffix as a string.
      * For example, the suffix of the symbol "A.B.C/f" is "f".
      */
    def suffix: String = parts.last

    /**
      * Returns `true` if this resolved name is equal to `obj` resolved name.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Resolved => this.parts == that.parts
      case _ => false
    }

    /**
      * Returns the hash code of this resolved name.
      */
    override val hashCode: Int = parts.hashCode()

    /**
      * Human readable representation.
      */
    override val toString: String = fqn
  }

}
