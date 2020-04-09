package ca.uwaterloo.flix.util.lsp

import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}

import scala.collection.mutable

class LspServer {

  case class Location()

  def toLoc(loc: SourceLocation): Location = ???

  class Index(root: Root) {

    private val m = mutable.Map.empty[Location, Expression]

    def visitExp(exp0: Expression): Unit = exp0 match {
      case Expression.Unit(loc) => m += toLoc(loc) -> exp0
    }

    def getSym(loc: Location): Option[Symbol.DefnSym] = ???

    def getTyp(loc: Location): Option[Type] = ???

    def getExp(loc: Location): Option[Expression] = ???


  }

}
