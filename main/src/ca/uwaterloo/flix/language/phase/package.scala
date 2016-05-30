package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{Name, SourcePosition, Symbol, Type}

package object phase {

  // TODO: Cleanup, possibly unify with the Flix class?

  class GenSym() {

    def freshTypeVar(): Type.Var = ???

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
