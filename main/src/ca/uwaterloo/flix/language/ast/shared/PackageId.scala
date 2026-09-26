/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

object PackageId {

  /** The prefix of every canonical root. */
  private val RootPrefix: String = "$pkg$"

  /** The separator of the parts of a canonical root. */
  private val RootSeparator: Char = '$'

  /** The names a repository, an owner, and a project may be built from. */
  private val ValidName = "[A-Za-z0-9_-]+".r

  /** An identifier: a host, an owner, and a project name. */
  private val IdentifierForm = "(.+):(.+)/(.+)".r

  /** Returns `true` if `s` can name the owner or the project of a package. */
  def isValidName(s: String): Boolean = ValidName.matches(s)

  /** Returns `s` as a package identifier, if it has the form `<host>:<owner>/<name>`. */
  def mkPackageId(s: String): Option[PackageId] = s match {
    case IdentifierForm(host, owner, name) if isValidName(owner) && isValidName(name) =>
      Repository.mkRepository(host).map(PackageId(_, owner, name))
    case _ => None
  }

  /**
    * Returns the package whose canonical root is `ns`, if `ns` is one.
    *
    * The inverse of [[PackageId.canonicalRoot]]. It is total on the roots that method produces,
    * because no part of an identifier may contain the separator the root is built from, so the
    * split cannot be ambiguous.
    */
  def ofCanonicalRoot(ns: String): Option[PackageId] =
    if (!ns.startsWith(RootPrefix)) None
    else ns.stripPrefix(RootPrefix).split(RootSeparator) match {
      case Array(host, owner, name) => mkPackageId(s"$host:$owner/$name")
      case _ => None
    }

}

/**
  * A Flix package, named by where it is published and by whom.
  */
case class PackageId(host: Repository, owner: String, name: String) extends Ordered[PackageId] {

  /**
    * Returns the namespace the declarations of this package are named under.
    *
    * The root is `$pkg$` followed by the host, the owner, and the project name, joined by `$`:
    * `github:flix/museum-clerk` becomes `$pkg$github$flix$museum-clerk`. No escaping is needed and
    * no two identifiers share a root, because no part of an identifier may contain the separator,
    * so it can never be confused with content.
    *
    * A Flix name may contain `$` but may not start with one, so a root cannot be written in source.
    * The version is not part of the root: a package occurs at exactly one version in a dependency
    * graph, and keeping the root stable across versions keeps `effects.lock` comparable.
    */
  def canonicalRoot: String =
    PackageId.RootPrefix + host + PackageId.RootSeparator + owner + PackageId.RootSeparator + name

  /**
    * Returns how this package is named in a message and written in a command: its owner and its
    * name, without the host.
    *
    * `github` is the only host there is, so naming it says nothing. A manifest still keys a
    * dependency by the identifier in full, since a key is not written for a reader.
    */
  def shortName: String = s"$owner/$name"

  override def compare(that: PackageId): Int =
    Ordering[(String, String, String)].compare(
      (this.host.toString, this.owner, this.name),
      (that.host.toString, that.owner, that.name))

  override def toString: String = s"$host:$owner/$name"

}
