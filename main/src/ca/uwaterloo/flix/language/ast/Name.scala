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
  val RootNS = NName(SourcePosition.Unknown, Nil, SourcePosition.Unknown)

  /**
    * Returns the given string `fqn` as a qualified name.
    */
  def mkQName(fqn: String, sp1: SourcePosition = SourcePosition.Unknown, sp2: SourcePosition = SourcePosition.Unknown): QName = {
    if (!fqn.contains('.'))
      return QName(sp1, Name.RootNS, Ident(sp1, fqn, sp2), sp2)

    val index = fqn.indexOf('.')
    val parts = fqn.substring(0, index).split('/').toList
    val name = fqn.substring(index + 1, fqn.length)
    val nname = NName(sp1, parts.map(t => Name.Ident(sp1, t, sp2)), sp2)
    val ident = Ident(sp1, name, sp2)
    QName(sp1, nname, ident, sp2)
  }

  /**
    * Returns the given identifier `ident` as qualified name in the root namespace.
    */
  def mkQName(ident: Ident): QName = QName(ident.sp1, RootNS, ident, ident.sp2)

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
    override def toString: String = if (idents.isEmpty) "/" else idents.mkString("/")
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
      * Returns `true` if the name of `this` qualified name is lowercase.
      */
    def isLowerCase: Boolean = ident.name.head.isLower

    /**
      * Returns `true` if the name of `this` qualified name is uppercase.
      */
    def isUpperCase: Boolean = ident.name.head.isUpper

    /**
      * The source location of the name.
      */
    def loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
      * Human readable representation.
      */
    override def toString: String = if (isUnqualified) ident.toString else namespace.toString + "." + ident
  }

}
