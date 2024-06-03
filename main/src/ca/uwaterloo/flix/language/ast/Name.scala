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

object Name {

  /**
    * The root namespace.
    */
  val RootNS: NName = NName(Nil, SourceLocation.Unknown)

  /**
    * Returns the given string `fqn` as a qualified name.
    */
  def mkQName(fqn: String, loc: SourceLocation = SourceLocation.Unknown): QName = {
    if (!fqn.contains('.'))
      return QName(Name.RootNS, Ident(loc.sp1, fqn, loc.sp2), loc)

    val index = fqn.indexOf('.')
    val parts = fqn.substring(0, index).split('/').toList
    val name = fqn.substring(index + 1, fqn.length)
    mkQName(parts, name, loc)
  }

  /**
    * Creates a qualified name from the given namespace `ns` and name `name`.
    */
  def mkQName(ns: List[String], name: String, loc: SourceLocation): QName = {
    val nname = NName(ns.map(t => Name.Ident(loc.sp1, t, loc.sp2)), loc)
    val ident = Ident(loc.sp1, name, loc.sp2)
    QName(nname, ident, loc)
  }

  /**
    * Converts the given identifier `ident` to a label.
    */
  def mkLabel(ident: Ident): Label = Label(ident.name, SourceLocation(isReal = true, ident.sp1, ident.sp2))

  /**
    * Converts the given identifier `ident` to a predicate name.
    */
  def mkPred(ident: Ident): Pred = Pred(ident.name, SourceLocation(isReal = true, ident.sp1, ident.sp2))

  /**
    * Builds an unlocated name from the given namespace parts.
    */
  def mkUnlocatedNName(parts: List[String]): NName = {
    val idents = parts.map(Ident(SourcePosition.Unknown, _, SourcePosition.Unknown))
    NName(idents, SourceLocation.Unknown)
  }

  /**
    * Returns true if the given string represents a wildcard name.
    */
  def isWild(name: String): Boolean = name.startsWith("_")

  /**
    * Identifier.
    *
    * @param sp1  the position of the first character in the identifier.
    * @param name the identifier string.
    * @param sp2  the position of the last character in the identifier.
    */
  case class Ident(sp1: SourcePosition, name: String, sp2: SourcePosition) {
    /**
      * Returns `true`if `this` identifier is a wildcard.
      */
    def isWild: Boolean = name.startsWith("_")

    /**
      * Returns `true` if `this` identifier is uppercase.
      */
    def isUpper: Boolean = name.charAt(0).isUpper

    /**
      * Returns `true` if `this` identifier is lowercase.
      */
    def isLower: Boolean = name.charAt(0).isLower

    /**
      * The source location of the identifier.
      */
    def loc: SourceLocation = SourceLocation(isReal = true, sp1, sp2)

    /**
      * Two identifiers are equal if they have the same name.
      */
    override def hashCode(): Int = name.hashCode

    /**
      * Two identifiers are equal if they have the same name.
      */
    override def equals(o: Any): Boolean = o match {
      case that: Ident => this.name == that.name
      case _ => false
    }

    /**
      * Human readable representation.
      */
    override def toString: String = name

    /**
      * Convert this Ident to synthetic
      */
    def asSynthetic = new SyntheticIdent(sp1, name, sp2)
  }

  /**
    * Synthetic Identifier
    *
    * Behaves just like Ident, but reports its `loc` as synthetic.
    */
  class SyntheticIdent(sp1: SourcePosition, name: String, sp2: SourcePosition) extends Ident(sp1, name, sp2) {
    override def loc: SourceLocation = SourceLocation(isReal = false, sp1, sp2)
  }

  /**
    * Namespace.
    *
    * @param idents the identifiers of the namespace.
    * @param loc    the source location of the namespace.
    */
  case class NName(idents: List[Ident], loc: SourceLocation) {
    /**
      * Returns `true` if this is the root namespace.
      */
    def isRoot: Boolean = idents.isEmpty

    /**
      * Returns the string parts of the namespace.
      */
    def parts: List[String] = idents.map(_.name)

    /**
      * Returns `true` if `this` namespace equals `that`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: NName => this.parts == that.parts
      case _ => false
    }

    /**
      * Returns the hash code of `this` namespace.
      */
    override def hashCode(): Int = parts.hashCode()

    /**
      * Human readable representation.
      */
    override def toString: String = if (idents.isEmpty) "" else idents.mkString(".")
  }

  /**
    * Qualified Name.
    *
    * @param namespace the namespace
    * @param ident     the identifier.
    * @param loc       the source location of the qualified name.
    */
  case class QName(namespace: NName, ident: Ident, loc: SourceLocation) {
    /**
      * Returns `true` if this name is unqualified (i.e. has no namespace).
      */
    def isUnqualified: Boolean = namespace.isRoot

    /**
      * Human readable representation.
      */
    override def toString: String = if (isUnqualified) ident.toString else namespace.toString + "." + ident
  }

  /**
    * The name of a label.
    *
    * @param name the name of the label.
    * @param loc  the specific occurrence of the name.
    */
  case class Label(name: String, loc: SourceLocation) {

    /**
      * Two label names are equal if their names are the same.
      */
    override def equals(o: Any): Boolean = o match {
      case that: Label => this.name == that.name
      case _ => false
    }

    /**
      * Two label names are equal if their names are the same.
      */
    override def hashCode(): Int = name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = name
  }

  /**
    * The name of a predicate.
    *
    * @param name the name of the predicate.
    * @param loc  the specific occurrence of the name.
    */
  case class Pred(name: String, loc: SourceLocation) {

    /**
      * Two predicate names are equal if their names are the same.
      */
    override def equals(o: Any): Boolean = o match {
      case that: Pred => this.name == that.name
      case _ => false
    }

    /**
      * Two predicate names are equal if their names are the same.
      */
    override def hashCode(): Int = name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = name
  }

  /**
    * Java Name.
    *
    * @param fqn the fully qualified name.
    * @param loc the source location of the name.
    */
  case class JavaName(fqn: Seq[String], loc: SourceLocation) {

    /**
      * Human readable representation.
      */
    override def toString: String = fqn.mkString(".")
  }
}
