package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._

import java.lang.reflect.Method
import scala.collection.immutable

object DocUtil {

  def fold(f: (Doc, Doc) => Doc, d: List[Doc]): Doc = d match {
    case immutable.Nil => empty
    case x :: immutable.Nil => x
    case x :: xs => f(x, fold(f, xs))
  }

  def spaces(d: List[Doc]): Doc = fold(_ +: _, d)

  def lines(d: List[Doc]): Doc = fold(_ +\: _, d)

  def bracket(l: String, x: Doc, r: String)(implicit i: Indent): Doc = {
    text(l) :: nest(breakWith("") :: x) \: text(r)
  }

  def sep(sep: Doc, d: List[Doc]): Doc =
    fold(_ :: sep :: _, d)

  /**
    * A space/line following the sep
    */
  def groupVSep(sep: String, d: List[Doc]): Doc =
    group(fold(_ :: text(sep) +\: _, d))

  def sep(s: String, d: List[Doc]): Doc =
    fold(_ :: text(s) :: _, d)

  def commaSep(d: List[Doc]): Doc = groupVSep(",", d)

  object Language {

    def tuplef(t: List[Doc])(implicit i: Indent): Doc = t match {
      case Nil => text("()") // this omits the possible line breaks
      case _ => group(bracket("(", sep(text(",") :: breakWith(" "), t), ")"))
    }

    def selectiveTuplef(t: List[Doc])(implicit i: Indent): Doc = t match {
      case Nil => text("()")
      case single :: Nil => single
      case _ => tuplef(t)
    }

    def paramf(exp: Doc, tpe: Doc)(implicit i: Indent): Doc =
      groupBreakIndent(exp :: text(":"), tpe)

    def ascf(exp: Doc, tpe: Doc)(implicit i: Indent): Doc =
      exp :: text(":") +: tpe

    def ascf(exp: Doc, tpe: Option[Doc])(implicit i: Indent): Doc = tpe match {
      case Some(value) => ascf(exp, value)
      case None => exp
    }

    def eqf(d1: Doc, d2: Doc)(implicit i: Indent): Doc =
      groupBreakIndent(d1 +: text("="), d2)

    /**
      * {a = b, c = d, e = f< | rest>}
      *
      * {
      * a = b, c = d, e = f< | rest>
      * }
      *
      * {
      * a = b,
      * c = d,
      * e = f<
      * | rest>
      * }
      *
      * {
      * a = b,
      * c =
      * d,
      * e = f<
      * | rest>
      * }
      */
    def recordExtendf(fields: List[(Doc, Doc)], rest: Option[Doc])(implicit i: Indent): Doc = {
      val prefix = rest match {
        case Some(_) => text("+")
        case None => text("")
      }
      val f = fields.map { case (x, y) => groupBreakIndent(prefix :: x +: text("="), y) }
      val restPart = rest match {
        case Some(value) => text(" |") +: value
        case None => empty
      }
      group(bracket("{", nest(group(
        sep(text(",") :: breakWith(" "), f) \: restPart
      )), "}"))
    }

    def schemaExtendf(fields: List[(Doc, Doc)], rest: Option[Doc])(implicit i: Indent): Doc =
      text("#") :: recordExtendf(fields, rest)

    /**
      * Just shows `tpe` for empty `args`.
      */
    def typeAppf(tpe: Doc, args: List[Doc])(implicit i: Indent): Doc = {
      if (args.nonEmpty) {
        tpe :: group(bracket("[", commaSep(args), "]"))
      } else {
        tpe
      }
    }

    def parens(d: Doc)(implicit i: Indent): Doc = group(bracket("(", d, ")"))

    def applyf(fun: Doc, args: List[Doc])(implicit i: Indent): Doc = {
      fun :: tuplef(args)
    }

    def applyStaticJavaf(m: Method, args: List[Doc])(implicit i: Indent): Doc = {
      val className = m.getDeclaringClass.getCanonicalName
      val methodName = text(m.getName)
      val fullMethodName = if (className == null) {
        methodName
      } else {
        text(className) :: text(".") :: methodName
      }
      applyf(fullMethodName, args)
    }

    def applyJavaf(m: Method, exp: Doc, args: List[Doc])(implicit i: Indent): Doc = {
      val className = m.getDeclaringClass.getCanonicalName
      val methodName = text(m.getName)
      val fullMethodName = if (className == null) {
        methodName
      } else {
        text(className) :: text(".") :: methodName
      }
      applyf(exp :: metaText(fullMethodName), args)
    }

    def applyf(caller: String, args: List[Doc])(implicit i: Indent): Doc = {
      text(caller) :: tuplef(args)
    }

    /**
      * def f(x: t, x: t): t = e
      *
      * def f(x: t, x: t): t =
      * e
      *
      * def f(
      * x: t,
      * x: t
      * ): t =
      * e
      */
    def defnf(name: String, args: List[Doc], resType: Doc, body: Doc)(implicit i: Indent): Doc = {
      group(
        group(
          text("def") +:
            text(name) :: bracket("(", sep(text(",") :: breakWith(" "), args), ")") ::
            text(":") +: resType +: text("=")
        ) +: bracket("{", body, "}")
      )
    }

    /**
      * let x<: t> = e
      *
      * let x<: t> =
      * e
      */
    def letf(v: Doc, tpe: Option[Doc], value: Doc, body: Doc)(implicit i: Indent): Doc = {
      //let(text("let") <+> ascf(v, tpe) <+> text("=") <+> value, body)
      ???
    }

    def letrecf(v: Doc, tpe: Option[Doc], value: Doc, body: Doc)(implicit i: Indent): Doc = {
      //      let(text("letrec") <+> ascf(v, tpe) <+> text("=") <+> value, body)
      ???
    }

    /**
      * {a; b; c}
      *
      * {
      * a;
      * b;
      * c
      * }
      */
    def seqBlockf(l: List[Doc])(implicit i: Indent): Doc = {
      group(bracket("{", sep(text(";") :: breakWith(" "), l), "}"))
    }

    def seqf(l: List[Doc])(implicit i: Indent): Doc = {
      group(sep(text(";") :: breakWith(" "), l))
    }

    /**
      * TODO: arrow chains should be formatted together
      */
    def arrowf(args: List[Doc], res: Doc)(implicit i: Indent): Doc = {
      groupBreakIndent(selectiveTuplef(args) +: text("->"), res)
    }

    /**
      * if (cond) {thn} else {els}
      *
      * if (cond) {
      * thn
      * } else {
      * els
      * }
      *
      * if (
      * cond
      * ) {
      * thn
      * } else {
      * els
      * }
      */
    def itef(cond: Doc, thn: Doc, els: Doc)(implicit i: Indent): Doc = {
      group(
        text("if") +: group(bracket("(", cond, ")")) +: bracket("{", thn, "}") +: text("else") +: bracket("{", els, "}")
      )
    }

    def castf(exp: Doc, tpe: Doc)(implicit i: Indent): Doc = {
      text("unsafe_cast") +: exp +: text("as") +: tpe
    }

    def assignf(asignee: Doc, value: Doc)(implicit i: Indent): Doc = {
      asignee +: text(":=") +: value
    }

    def emptyRecordf(): Doc = text("{}")

    def recordSelectf(exp: Doc, field: Doc): Doc =
      exp :: text(".") :: field

    def stringf(s: String): Doc =
      text("\"") :: text(s) :: text("\"")

    def arrayListf(elms: List[Doc])(implicit i: Indent): Doc =
      group(text("Array#") :: bracket("{", commaSep(elms), "}"))

    def spawnf(exp: Doc, region: Doc)(implicit i: Indent): Doc =
      text("spawn") +: group(bracket("{", exp, "}")) +: text("@") +: region

    def scopef(region: Doc, exp: Doc)(implicit i: Indent): Doc =
      text("region") +: region +: group(bracket("{", exp, "}"))

    def tupleIndexf(exp: Doc, index: Int): Doc =
      exp +: text(s"._$index")

    /**
      * `<txt>`
      */
    def metaText(txt: String): Doc = text("<") :: text(txt) :: text(">")

    /**
      * `<txt>`
      */
    def metaText(txt: Doc): Doc = text("<") :: txt :: text(">")

  }
}
