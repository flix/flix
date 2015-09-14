package ca.uwaterloo.flix.lang.ast

trait WeededAst

object WeededAst {

  object Declaration {

    case class Enum(ident: ParsedAst.Ident, cases: Map[String, ParsedAst.Type.Tag])

  }

}