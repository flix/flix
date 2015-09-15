package ca.uwaterloo.flix.lang.ast

trait WeededAst

object WeededAst {

  object Declaration {

    case class Enum(ident: ParsedAst.Ident, cases: Map[String, ParsedAst.Type.Tag])

  }

  sealed trait Type

  object Type {

    case object Unit extends WeededAst.Type

    case class Ambiguous(name: ParsedAst.QName) extends WeededAst.Type

    case class Function(t1: WeededAst.Type, t2: WeededAst.Type) extends WeededAst.Type

    case class Tag(ident: ParsedAst.Ident, tpe: WeededAst.Type) extends WeededAst.Type

    case class Tuple(elms: Seq[WeededAst.Type]) extends WeededAst.Type

    case class Parametric(name: ParsedAst.QName, elms: Seq[WeededAst.Type]) extends WeededAst.Type

    case class Lattice(tpe: ParsedAst.Type) extends WeededAst.Type

  }

}