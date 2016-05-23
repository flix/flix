package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{SourcePosition, Name}

import ca.uwaterloo.flix.language.ast.Symbol

package object phase {

  // TODO: Cleanup

  class GenSym() {

    private var number: Int = 0

    def freshId(): Int = {
      number = number + 1
      number
    }

    def freshDefn(ns: List[String]): Symbol.Resolved = {
      number = number + 1
      Symbol.Resolved.mk(ns.init ::: ns.last + "$" + number :: Nil)
    }

    def fresh2(): Name.Ident = fresh2("tmp")

    def fresh2(prefix: String): Name.Ident = {
      number = number + 1
      Name.Ident(SourcePosition.Unknown, prefix + "$" + number, SourcePosition.Unknown)
    }

  }

}
