package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.phase.GenSym

import scala.collection.mutable

object Symbol {

  /**
    * Returns a fresh variable symbol for the given identifier.
    */
  def mkVarSym(ident: Name.Ident)(implicit genSym: GenSym): VarSym = {
    new VarSym(genSym.freshId(), ident.name, ident.loc)
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
    * Table Symbol.
    *
    * @param loc the source location associated with the symbol.
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
    override def toString: String = namespace.mkString(".") + "/" + name
  }


  /**
    * Companion object for the [[Resolved]] class.
    */
  // TODO: deprecated
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
