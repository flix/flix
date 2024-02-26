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
  val RootNS: NName = NName(SourcePosition.Unknown, Nil, SourcePosition.Unknown)

  /**
    * Returns the given string `fqn` as a qualified name.
    */
  def mkQName(fqn: String, sp1: SourcePosition = SourcePosition.Unknown, sp2: SourcePosition = SourcePosition.Unknown): QName = {
    if (!fqn.contains('.'))
      return QName(sp1, Name.RootNS, Ident(sp1, fqn, sp2), sp2)

    val index = fqn.indexOf('.')
    val parts = fqn.substring(0, index).split('/').toList
    val name = fqn.substring(index + 1, fqn.length)
    mkQName(parts, name, sp1, sp2)
  }

  /**
    * Creates a qualified name from the given namespace `ns` and name `name`.
    */
  def mkQName(ns: List[String], name: String, sp1: SourcePosition, sp2: SourcePosition): QName = {
    val nname = NName(sp1, ns.map(t => Name.Ident(sp1, t, sp2)), sp2)
    val ident = Ident(sp1, name, sp2)
    QName(sp1, nname, ident, sp2)
  }

  /**
    * Returns the given identifier `ident` as qualified name in the root namespace.
    */
  def mkQName(ident: Ident): QName = QName(ident.sp1, RootNS, ident, ident.sp2)

  /**
    * Converts the given identifier `ident` to a label.
    */
  def mkLabel(ident: Ident): Label = Label(ident.name, SourceLocation.mk(ident.sp1, ident.sp2))

  /**
    * Converts the given identifier `ident` to a predicate name.
    */
  def mkPred(ident: Ident): Pred = Pred(ident.name, SourceLocation.mk(ident.sp1, ident.sp2))

  /**
    * Extends the given namespace `ns` with the given identifier `ident`.
    */
  def extendNName(ns: NName, ident: Ident): NName = NName(ident.sp1, ns.idents :+ ident, ident.sp2)

  /**
    * Builds an unlocated name from the given namespace parts.
    */
  def mkUnlocatedNName(parts: List[String]): NName = {
    val idents = parts.map(Ident(SourcePosition.Unknown, _, SourcePosition.Unknown))
    NName(SourcePosition.Unknown, idents, SourcePosition.Unknown)
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
    def loc: SourceLocation = SourceLocation.mk(sp1, sp2)

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
    override def loc: SourceLocation = SourceLocation.mk(sp1, sp2, SourceKind.Synthetic)
  }

  /**
    * Namespace.
    *
    * @param sp1    the position of the first character in the namespace.
    * @param idents the identifiers of the namespace.
    * @param sp2    the position of the last character in the namespace.
    */
  case class NName(sp1: SourcePosition, idents: List[Ident], sp2: SourcePosition) {
    /**
      * Returns `true` if this is the root namespace.
      */
    def isRoot: Boolean = idents.isEmpty

    /**
      * Returns the string parts of the namespace.
      */
    def parts: List[String] = idents.map(_.name)

    /**
      * The source location of the namespace.
      */
    def loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
      * Returns the hash code of `this` namespace.
      */
    override def hashCode(): Int = parts.hashCode()

    /**
      * Returns `true` if `this` namespace equals `that`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: NName => this.parts == that.parts
      case _ => false
    }

    /**
      * Human readable representation.
      */
    override def toString: String = if (idents.isEmpty) "" else idents.mkString(".")
  }

  /**
    * Companion object for the [[QName]] class.
    */
  object QName {
    /**
      * Returns the qualified name for the given optional namespace `nsOpt` and identifier `ident`.
      */
    def mk(sp1: SourcePosition, nsOpt: Option[NName], ident: Ident, sp2: SourcePosition): QName = nsOpt match {
      case None => Name.QName(sp1, RootNS, ident, sp2)
      case Some(ns) => Name.QName(sp1, ns, ident, sp2)
    }

    /**
      * Converts the given NName into a qualified name.
      */
    def fromNName(nname0: Name.NName): QName = {
      val sp1 = nname0.idents.head.sp1
      val sp2 = nname0.idents.last.sp2
      val ns = nname0.idents.init
      val ident = nname0.idents.last
      val nname = Name.NName(sp1, ns, sp2)
      Name.QName(sp1, nname, ident, sp2)
    }
  }

  /**
    * Qualified Name.
    *
    * @param sp1       the position of the first character in the qualified name.
    * @param namespace the namespace
    * @param ident     the identifier.
    * @param sp2       the position of the last character in the qualified name.
    */
  case class QName(sp1: SourcePosition, namespace: NName, ident: Ident, sp2: SourcePosition) {

    /**
      * Returns `true` if this name is qualified by a namespace.
      */
    def isQualified: Boolean = !namespace.isRoot

    /**
      * Returns `true` if this name is unqualified (i.e. has no namespace).
      */
    def isUnqualified: Boolean = !isQualified

    /**
      * The source location of the name.
      */
    def loc: SourceLocation = SourceLocation.mk(sp1, sp2)

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
    override def hashCode(): Int = name.hashCode

    /**
      * Two label names are equal if their names are the same.
      */
    override def equals(o: Any): Boolean = o match {
      case that: Label => this.name == that.name
      case _ => false
    }

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
    override def hashCode(): Int = name.hashCode

    /**
      * Two predicate names are equal if their names are the same.
      */
    override def equals(o: Any): Boolean = o match {
      case that: Pred => this.name == that.name
      case _ => false
    }

    /**
      * Human readable representation.
      */
    override def toString: String = name
  }

  /**
    * Java Name.
    *
    * @param sp1  the position of the first character in the identifier.
    * @param fqn  the fully qualified name.
    * @param sp2  the position of the last character in the identifier.
    */
  case class JavaName(sp1: SourcePosition, fqn: Seq[String], sp2: SourcePosition) {

    /**
      * Human readable representation.
      */
    override def toString: String = fqn.mkString(".")
  }
}
