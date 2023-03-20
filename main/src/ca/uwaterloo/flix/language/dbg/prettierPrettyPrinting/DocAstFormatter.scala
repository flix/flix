package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst.Type
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.{bracket, commaSep, groupVSep}

import scala.annotation.tailrec

object DocAstFormatter {

  def format(p: DocAst.Program)(implicit i: Indent): List[Doc] = {
    implicit val i: Indent = indentationLevel(4)
    val DocAst.Program(enums0, defs0) = p
    val enums = enums0.map {
      case DocAst.Enum(_, _, sym, cases) =>
        val d = group(text("enum") +: text(sym.toString) +: bracket("{",
          sep(breakWith(" "), cases.map {
            case DocAst.Case(sym) => text("case") +: text(sym.toString) :: text("(...)")
          }),
          "}"))
        ((sym.namespace :+ sym.name: Seq[String], sym.name), d)
    }
    val defs = defs0.map {
      case DocAst.Def(_, _, sym, parameters, resType, body) =>
        val d = defnf(
          sym.toString,
          parameters.map(format(_)),
          formatType(resType, paren = false),
          format(body)
        )
        ((sym.namespace: Seq[String], sym.name), d)
    }
    import scala.math.Ordering.Implicits.seqOrdering
    (enums ++ defs).sortBy(_._1).map(_._2)
  }

  def format(d: DocAst)(implicit i: Indent): Doc =
    aux(d, paren = false, inBlock = true)

  private def aux(d: DocAst, paren: Boolean = true, inBlock: Boolean = false)(implicit i: Indent): Doc = {
    val doc = d match {
      case DocAst.InRegion(d1, d2) =>
        aux(d1) +: text("@") +: aux(d2)
      case DocAst.Unit =>
        text("()")
      case DocAst.Tuple(elms) =>
        tuplef(elms.map(aux(_, paren = false)))
      case DocAst.Tag(sym, Nil) =>
        text(sym.toString)
      case DocAst.Tag(sym, List(DocAst.Unit)) =>
        text(sym.toString)
      case DocAst.Tag(sym, List(DocAst.Tuple(args))) =>
        text(sym.toString) :: tuplef(args.map(aux(_, paren = false)))
      case DocAst.Tag(sym, args) =>
        text(sym.toString) :: tuplef(args.map(aux(_, paren = false)))
      case DocAst.AsIs(s) =>
        text(s)
      case DocAst.Meta(s) =>
        text("<[") :: text(s) :: text("]>")
      case DocAst.RecordEmpty =>
        text("{}")
      case re: DocAst.RecordExtend =>
        formatRecordBlock(re)
      case rr: DocAst.RecordRestrict =>
        formatRecordBlock(rr)
      case DocAst.Keyword(word, d) =>
        text(word) +: aux(d)
      case DocAst.Unary(op, d) =>
        text(op) :: aux(d)
      case DocAst.Binary(d1, op, d2) =>
        aux(d1) +: text(op) +: aux(d2)
      case DocAst.IfThenElse(cond, thn, els) =>
        group(
          text("if") +:
            group(bracket("(", aux(cond, paren = false, inBlock = true), ")")) +:
            bracket("{", aux(thn, paren = false, inBlock = true), "}") +:
            text("else") +:
            bracket("{", aux(els, paren = false, inBlock = true), "}")
        )
      case DocAst.Branch(d, branches) =>
        text("branching") +: group(bracket("{",
          aux(d, paren = false, inBlock = true)
          , "}") +: text("with") +: bracket("{",
          groupVSep("",
            branches.toList.map { case (sym, dd) =>
              breakIndent(
                text("label") +: text(sym.toString) :: text(":"),
                aux(dd, paren = false, inBlock = true)
              )
            })
          , "}"))
      case DocAst.Dot(d1, d2) =>
        aux(d1) :: text(".") :: aux(d2)
      case DocAst.DoubleDot(d1, d2) =>
        aux(d1) :: text("..") :: aux(d2)
      case l: DocAst.Let =>
        formatLetBlock(l, inBlock)
      case l: DocAst.LetRec =>
        formatLetBlock(l, inBlock)
      case DocAst.Scope(v, d) =>
        text("region") +: aux(v) +: group(bracket("{", aux(d, paren = false, inBlock = true), "}"))
      case DocAst.App(f, args) =>
        DocUtil.Language.applyf(aux(f), args.map(aux(_, paren = false)))
      case DocAst.SquareApp(f, args) =>
        aux(f) :: group(bracket("[", commaSep(args.map(aux(_, paren = false))), "]"))
      case DocAst.Assign(d1, d2) =>
        aux(d1) +: text(":=") +: aux(d2)
      case DocAst.Ascription(v, tpe) =>
        aux(v) :: text(":") +: formatType(tpe, paren = false)
      case DocAst.DoubleKeyword(word1, d1, word2, d2E) =>
        val d2Part = d2E match {
          case Left(d2) => aux(d2, paren = false)
          case Right(tpe) => formatType(tpe, paren = false)
        }
        text(word1) +: aux(d1, paren = false) +: text(word2) +: d2Part
      case DocAst.TryCatch(d, rules) =>
        val rs = groupVSep("", rules.map {
          case (sym, clazz, ruled) =>
            text("case") +: text(sym.toString) :: text(":") +: text("##" + clazz.getName) +:
              text("=>") +\: aux(ruled, paren = false, inBlock = true)
        })
        text("try") +: group(
          bracket("{", aux(d, paren = false, inBlock = true), "}") +:
            text("catch") +:
            bracket("{", rs, "}")
        )
      case DocAst.NewObject(_, clazz, _, methods) =>
        group(text("new") +: text("##" + clazz.getName) +: bracket("{",
          sep(breakWith(" "), methods.map(formatJvmMethod)),
          "}"))
    }
    d match {
      case _: DocAst.Composite if paren => parens(doc)
      case _: DocAst.Composite | _: DocAst.Atom => doc
    }
  }

  private def formatJvmMethod(m: DocAst.JvmMethod)(implicit i: Indent): Doc = {
    val DocAst.JvmMethod(ident, fparams, clo, _) = m
    group(
      text("def") +: text(ident.name) +:
        bracket("(",
          commaSep(fparams.map(aux(_, paren = false))),
          ")"
        ) +: text("=") +\:
        aux(clo, paren = false, inBlock = true)
    )
  }

  private def formatLetBlock(d: DocAst.LetBinder, inBlock: Boolean)(implicit i: Indent): Doc = {
    val (binders, body) = collectLetBlock(d)
    val formattedBinders = binders.map {
      case DocAst.Let(v, tpe, bind, _) =>
        text("let") +: aux(v) :: formatAscription(tpe) +: text("=") +: aux(bind, paren = false)
      case DocAst.LetRec(v, tpe, bind, _) =>
        text("letrec") +: aux(v) :: formatAscription(tpe) +: text("=") +: aux(bind, paren = false)
    }
    val delimitedBinders = sep(
      text(";") :: breakWith(" "),
      formattedBinders :+ aux(body, paren = false)
    )
    if (inBlock) group(delimitedBinders)
    else group(DocUtil.bracket("{", delimitedBinders, "}"))
  }

  private def formatRecordBlock(d: DocAst.RecordOp)(implicit i: Indent): Doc = {
    val (exs, restOpt) = collectRecordOps(d)
    val exsf = exs.map {
      case DocAst.RecordExtend(field, value, _) =>
        text("+" + field.toString) +: text("=") +: aux(value, paren = false)
      case DocAst.RecordRestrict(field, _) =>
        text("-" + field.toString)
    }
    restOpt match {
      case Some(rest) =>
        group(bracket("{",
          sep(text(",") :: breakWith(" "), exsf) +: text("|") +\: aux(rest, paren = false)
          , "}"))
      case None =>
        group(bracket("{",
          sep(text(",") :: breakWith(" "), exsf) +: text("|") +\: text("{}")
          , "}"))
    }
  }

  private def formatAscription(tpe: Option[DocAst.Type])(implicit i: Indent): Doc =
    tpe.map(t => text(":") +: formatType(t)).getOrElse(empty)

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

  private def collectRecordOps(d: DocAst)(implicit i: Indent): (List[DocAst.RecordOp], Option[DocAst]) = {
    @tailrec
    def chase(d0: DocAst, acc: List[DocAst.RecordOp]): (List[DocAst.RecordOp], Option[DocAst]) = {
      d0 match {
        case re@DocAst.RecordExtend(_, _, rest) =>
          chase(rest, re :: acc)
        case re@DocAst.RecordRestrict(_, rest) =>
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
      case Type.App(obj, Nil) =>
        text(obj)
      case Type.App(obj, args) =>
        text(obj) :: group(bracket("[", commaSep(args.map(formatType(_, paren = false))), "]"))
      case Type.Tuple(elms) =>
        tuplef(elms.map(formatType(_, paren = false)))
      case arrow@Type.Arrow(_, _) =>
        val (curriedArgs, res) = collectArrowType(arrow)
        // todo: maybe not tuple formatting?
        val formattedArgs = curriedArgs.map(ts =>
          selectiveTuplef(ts.map(formatType(_, paren = false)))
        )
        group(nest(sep(text(" ->") :: breakWith(" "), formattedArgs :+ formatType(res))))
      case Type.RecordEmpty =>
        text("{}")
      case re: Type.RecordExtend =>
        val (fields, restOpt) = collectRecordType(re)
        val exsf = fields.map {
          case Type.RecordExtend(field, value, _) =>
            text(field) +: text("=") +: formatType(value, paren = false)
        }
        restOpt match {
          case Some(rest) =>
            group(bracket("{",
              sep(text(",") :: breakWith(" "), exsf) +: text("|") +\: formatType(rest, paren = false)
              , "}"))
          case None =>
            group(bracket("{",
              sep(text(",") :: breakWith(" "), exsf) +: text("|") +\: text("{}")
              , "}"))
        }
      case Type.SchemaEmpty => text("unknown")
      case Type.SchemaExtend(name, tpe, rest) => text("unknown")
    }
    tpe match {
      case _: Type.Composite if paren => parens(d)
      case _: Type.Composite | _: Type.Atom => d
    }
  }

  private def collectArrowType(tpe: Type): (List[List[Type]], Type) = {
    @tailrec
    def chase(tpe0: Type, acc: List[List[Type]]): (List[List[Type]], Type) = {
      tpe0 match {
        case Type.Arrow(args, res) => chase(res, args :: acc)
        case other => (acc.reverse, other)
      }
    }

    chase(tpe, List())
  }

  private def collectRecordType(tpe: Type): (List[Type.RecordExtend], Option[Type]) = {
    @tailrec
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
