package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language.{parens, scopef}
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.{bracket, commaSep}

import scala.annotation.tailrec

object DocAstFormatter {

  def format(d: DocAst, indent: Int = 4): Doc =
    aux(d, paren = false, inBlock = true)(indentationLevel(indent))

  private def aux(d: DocAst, paren: Boolean = true, inBlock: Boolean = false)(implicit i: Indent): Doc = {
    val doc = d match {
      case DocAst.AsIs(s) =>
        text(s)
      case DocAst.Meta(s) =>
        text("<[") <> text(s) <> text("]>")
      case DocAst.RecordEmpty =>
        text("{}")
      case DocAst.Keyword(word, d) =>
        text(word) <+> aux(d)
      case DocAst.Unary(op, d) =>
        text(op) <> aux(d)
      case DocAst.Binary(d1, op, d2) =>
        aux(d1) <+> text(op) <+> aux(d2)
      case DocAst.IfThenElse(cond, thn, els) =>
        group(
          text("if") <+>
            group(bracket("(", aux(cond, paren = false), ")")) <+>
            bracket("{", aux(thn, paren = false, inBlock = true), "}") <+>
            text("else") <+>
            bracket("{", aux(els, paren = false, inBlock = true), "}")
        )
      case DocAst.Branch(d, branches) =>
        text("branching") <+> group(bracket("{",
          aux(d, paren = false, inBlock = true)
          , "}") <+> text("with") <+> bracket("{",
          commaSep(
            branches.toList.map { case (sym, dd) =>
              text("label") <+> text(sym.toString) <> text(":") <+\>> aux(dd, paren = false)
            })
          , "}"))
      case DocAst.Dot(d1, d2) =>
        aux(d1) <> text(".") <> aux(d2)
      case DocAst.DoubleDot(d1, d2) =>
        aux(d1) <> text("..") <> aux(d2)
      case l: DocAst.Let =>
        formatLetBlock(l, inBlock)
      case l: DocAst.LetRec =>
        formatLetBlock(l, inBlock)
      case DocAst.Scope(v, d) =>
        scopef(aux(v), aux(d, paren = false, inBlock = true))
      case DocAst.App(f, args) =>
        DocUtil.Language.applyf(aux(f), args.map(aux(_, paren = false)))
      case DocAst.Ascription(v, tpe) =>
        aux(v) <> text(":") <+> aux(tpe, paren = false)
      case DocAst.Cast(d, tpe) =>
        DocUtil.Language.castf(aux(d, paren = false), aux(tpe, paren = false))
      case DocAst.ArrayLit(ds) =>
        text("Array#") <> group(bracket("{", DocUtil.commaSep(ds.map(aux(_, paren = false))), "}"))
    }
    d match {
      case _: DocAst.Composite if paren => parens(doc)
      case _: DocAst.Composite | _: DocAst.Atom => doc
    }
  }

  private def formatLetBlock(d: DocAst.LetBinder, inBlock: Boolean)(implicit i: Indent): Doc = {
    val (binders, body) = collectLetBlock(d)
    val formattedBinders = binders.map {
      case DocAst.Let(v, tpe, bind, _) =>
        text("let") <+> aux(v) <> formatAscription(tpe) <+> text("=") <+> aux(bind, paren = false)
      case DocAst.LetRec(v, tpe, bind, _) =>
        text("letrec") <+> aux(v) <> formatAscription(tpe) <+> text("=") <+> aux(bind, paren = false)
    }
    val delimitedBinders = DocUtil.sep(
      text(";") <> breakWith(" "),
      formattedBinders :+ aux(body, paren = false)
    )
    if (inBlock) group(delimitedBinders)
    else group(DocUtil.bracket("{", delimitedBinders, "}"))
  }

  private def formatAscription(tpe: Option[DocAst])(implicit i: Indent): Doc =
    tpe.map(t => text(":") <+> aux(t, paren = false)).getOrElse(empty)

  private def collectLetBlock(d: DocAst): (List[DocAst.LetBinder], DocAst) = {
    @tailrec
    def chase(d0: DocAst, acc: List[DocAst.LetBinder]): (List[DocAst.LetBinder], DocAst) = {
      d0 match {
        case l@DocAst.Let(_, _, _, body) =>
          chase(body, l :: acc)
        case l@DocAst.LetRec(_, _, _, body) =>
          chase(body, l :: acc)
        case other => (acc.reverse, other)
      }
    }

    chase(d, List())
  }

}
