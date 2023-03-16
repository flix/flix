package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._

object DocAstFormatter {

  def format(d: DocAst, indent: Int = 4): Doc =
    aux(d, paren = false)(indentationLevel(indent))

  private def aux(d: DocAst, paren: Boolean = true)(implicit i: Indent): Doc = {
    def par(d: Doc): Doc = if (paren) parens(d) else d
    d match {
      case atom: DocAst.Atom => atom match {
        case DocAst.Var(sym) =>
          text(sym.toString)
        case DocAst.AsIs(s) => ???
        case DocAst.Meta(s) =>
          text("<[") <> text(s) <> text("]>")
        case DocAst.RecordEmpty => ???
        case DocAst.HoleError(sym) => ???
        case DocAst.VarWithOffset(sym) =>
          text(sym.toString) <> text("%") <> text(sym.getStackOffset.toString)
      }
      case DocAst.Unary(op, d) => ???
      case DocAst.Binary(d1, op, d2) => ???
      case DocAst.IfThenElse(cond, thn, els) => ???
      case DocAst.Let(v, tpe, bind, body) => ???
      case DocAst.Ascription(v, tpe) => ???
    }
  }

}
