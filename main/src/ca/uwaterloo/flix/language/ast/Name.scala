package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.util.SmartHash

import scala.collection.mutable

object Name {

  /**
    * Identifier.
    *
    * NB: Equality on identifiers is defined by structural equality on all components.
    *
    * @param sp1  the position of the first character in the identifier.
    * @param name the identifier string.
    * @param sp2  the position of the last character in the identifier.
    */
  case class Ident(sp1: SourcePosition, name: String, sp2: SourcePosition) extends SmartHash {
    /**
      * The source location of the identifier.
      */
    val loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
      * Human readable representation.
      */
    override val toString: String = name
  }

  /**
    * Namespace.
    *
    * NB: Equality on namespaces is defined by structural equality on all components.
    *
    * @param sp1    the position of the first character in the namespace.
    * @param idents the identifiers of the namespace.
    * @param sp2    the position of the last character in the namespace.
    */
  case class NName(sp1: SourcePosition, idents: List[Ident], sp2: SourcePosition) extends SmartHash {
    /**
      * Returns `true` if this is the root namespace.
      */
    val isRoot: Boolean = idents.isEmpty

    /**
      * The source location of the namespace.
      */
    val loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
      * Human readable representation.
      */
    override val toString: String = idents.mkString(".")
  }

  /**
    * Qualified Name.
    *
    * @param sp1       the position of the first character in the qualified name.
    * @param namespace the namespace
    * @param ident     the identifier.
    * @param sp2       the position of the last character in the qualified name.
    */
  case class QName(sp1: SourcePosition, namespace: NName, ident: Ident, sp2: SourcePosition) extends SmartHash {
    /**
      * Returns `true` if this name is qualified by a namespace.
      */
    val isQualified: Boolean = !namespace.isRoot

    /**
      * The source location of the name.
      */
    val loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
      * Human readable representation.
      */
    override val toString: String = namespace.toString + "/" + ident
  }


  /**
    * Companion object for the [[Resolved]] class.
    */
  // TODO: Move to symbol.
  object Resolved {

    private val cache = mutable.HashMap.empty[List[String], Resolved]

    def mk(name: String): Resolved = {
      if (name.contains("/")) {
        val index = name.indexOf("/")
        val (ns, ident) = name.splitAt(index)
        mk(ns.split("\\.").toList ::: ident.substring(1) :: Nil)
      } else
        mk(List(name))
    }

    def mk(parts: List[String]): Resolved = {
      cache.getOrElseUpdate(parts, new Resolved(parts))
    }
  }

  /**
    * Represents a resolved name.
    *
    * @param parts the parts of the name.
    */
  final class Resolved private(val parts: List[String]) {

    /**
      * Returns the fully qualified name of `this` as a string.
      */
    def fqn: String = parts match {
      case x :: Nil => x
      case xs => xs.init.mkString(".") + "/" + xs.last
    }

    /**
      * Returns `true` if this resolved name is equal to `obj` resolved name.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Resolved => this eq that
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
