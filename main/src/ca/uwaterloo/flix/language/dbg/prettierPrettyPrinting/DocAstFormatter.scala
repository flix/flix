package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst.Type
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.{bracket, commaSep, fold, groupVSep}

import scala.annotation.tailrec

object DocAstFormatter {

  def format(p: DocAst.Program)(implicit i: Indent): Doc = {
    implicit val i: Indent = indentationLevel(4)
    val defs: List[Doc] = p.defs.sortBy(_.sym.toString).map {
      case DocAst.Def(_, _, sym, parameters, resType, body) =>
        defnf(
          sym.toString,
          parameters.map(format(_)),
          formatType(resType, paren = false),
          format(body)
        )
    }
    group(fold({ case (d1, d2) =>
      (d1 <> breakWith("") <> breakWith("")) <> d2
    }, defs))
  }

  def format(d: DocAst)(implicit i: Indent): Doc =
    aux(d, paren = false, inBlock = true)

  private def aux(d: DocAst, paren: Boolean = true, inBlock: Boolean = false)(implicit i: Indent): Doc = {
    val doc = d match {
      case DocAst.AsIs(s) =>
        text(s)
      case DocAst.Meta(s) =>
        text("<[") <> text(s) <> text("]>")
      case DocAst.RecordEmpty =>
        text("{}")
      case re: DocAst.RecordExtend =>
        val (exs, restOpt) = collectRecordExtends(re)
        recordExtendf(
          exs.map { case DocAst.RecordExtend(field, value, _) =>
            (text(field.name), aux(value, paren = false))
          },
          restOpt.map(aux(_, paren = false))
        )
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
          groupVSep("",
            branches.toList.map { case (sym, dd) =>
              text("label") <+> text(sym.toString) <> text(":") <+\>> aux(dd, paren = false, inBlock = true)
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
        aux(v) <> text(":") <+> formatType(tpe, paren = false)
      case DocAst.Cast(d, tpe) =>
        DocUtil.Language.castf(aux(d, paren = false), formatType(tpe))
      case DocAst.ArrayLit(ds) =>
        text("Array#") <> group(bracket("{", commaSep(ds.map(aux(_, paren = false))), "}"))
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

  private def formatAscription(tpe: Option[DocAst.Type])(implicit i: Indent): Doc =
    tpe.map(t => text(":") <+> formatType(t)).getOrElse(empty)

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

  private def collectRecordExtends(d: DocAst)(implicit i: Indent): (List[DocAst.RecordExtend], Option[DocAst]) = {
    @tailrec
    def chase(d0: DocAst, acc: List[DocAst.RecordExtend]): (List[DocAst.RecordExtend], Option[DocAst]) = {
      d0 match {
        case re@DocAst.RecordExtend(_, _, rest) =>
          chase(rest, re :: acc)
        case DocAst.RecordEmpty =>
          (acc.reverse, None)
        case other =>
          (acc.reverse, Some(other))
      }
    }

    chase(d, List())
  }

  // types

  private def formatType(tpe: DocAst.Type, paren: Boolean = true)(implicit i: Indent): Doc = {
    val d = tpe match {
      case Type.AsIs(s) =>
        text(s)
      case Type.App(obj, args) =>
        text(obj) <> bracket("[", commaSep(args.map(formatType(_, paren = false))), "]")
      case Type.Tuple(elms) =>
        tuplef(elms.map(formatType(_, paren = false)))
      case Type.Arrow(args, res) =>
        // todo: collect
        // todo: maybe not tuple formatting?
        arrowf(args.map(formatType(_)), formatType(res))
      case Type.RecordEmpty =>
        text("{}")
      case re: Type.RecordExtend =>
        val (fields, rest) = collectRecordType(re)
        DocUtil.Language.recordExtendf(
          fields.map { case Type.RecordExtend(field, value, _) => (text(field), formatType(value, paren = false)) },
          rest.map(formatType(_, paren = false))
        )
      case Type.SchemaEmpty => text("unknown")
      case Type.SchemaExtend(name, tpe, rest) => text("unknown")
    }
    tpe match {
      case _: Type.Composite if paren => parens(d)
      case _: Type.Composite | _: Type.Atom => d
    }
  }

  private def collectRecordType(tpe: Type): (List[Type.RecordExtend], Option[Type]) = {
    def chase(tpe0: Type, acc: List[Type.RecordExtend]): (List[Type.RecordExtend], Option[Type]) = {
      tpe0 match {
        case re@Type.RecordExtend(_, _, rest) =>
          chase(rest, re :: acc)
        case Type.RecordEmpty =>
          (acc.reverse, None)
        case other =>
          (acc.reverse, Some(other))
      }
    }

    chase(tpe, List())
  }

}
