/*
 * Copyright 2023 Jonathan Lindegaard Starup
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.Ast.VarText
import ca.uwaterloo.flix.language.dbg.Doc._
import ca.uwaterloo.flix.language.dbg.DocAst.Expr._
import ca.uwaterloo.flix.language.dbg.DocAst._

import scala.annotation.tailrec

object DocAstFormatter {

  def format(p: Program)(implicit i: Indent): List[Doc] = {
    import scala.math.Ordering.Implicits.seqOrdering
    val Program(enums0, defs0) = p
    val enums = enums0.map {
      case Enum(_, _, sym, tparams, cases) =>
        val tparamsf = if (tparams.isEmpty) empty else text("[") :: sep(text(", "), tparams.map {
          case DocAst.TypeParam(sym) => text(sym.text match {
            case VarText.Absent => "?"
            case VarText.SourceText(s) => s
          })
        }) :: text("]")
        val casesf = curly(sep(breakWith(" "), cases.map {
          case Case(sym, tpe@Type.Tuple(_)) =>
            text("case") +: text(sym.name) :: formatType(tpe, paren = false)
          case Case(sym, tpe) =>
            text("case") +: text(sym.name) :: parens(formatType(tpe, paren = false))
        }))
        val d = text("enum") +: text(sym.toString) :: tparamsf +: casesf
        ((sym.namespace :+ sym.name: Seq[String], sym.name), d)
    }
    val defs = defs0.map {
      case Def(_, _, sym, parameters, resType, effect, body) =>
        val name = sym.toString
        val args = parameters.map(aux(_, paren = false))
        val resTypef = formatType(resType, paren = false)
        val effectf = formatEffect(effect, paren = false)
        val bodyf = format(body)
        val d = group(
          text("def") +: text(name) :: tuple(args) ::
            text(":") +: group(resTypef :: effectf +: text("=") :: breakWith(" ")) :: curlyOpen(bodyf)
        )
        ((sym.namespace: Seq[String], sym.name), d)
    }
    (enums ++ defs).sortBy(_._1).map(_._2)
  }

  def format(d: Expr)(implicit i: Indent): Doc =
    aux(d, paren = false, inBlock = true)

  private def aux(d: Expr, paren: Boolean = true, inBlock: Boolean = false)(implicit i: Indent): Doc = {
    val doc = d match {
      case Unit =>
        text("()")
      case Tuple(elms) =>
        tuple(elms.map(aux(_, paren = false)))
      case Tag(sym, Nil) =>
        text(sym.toString)
      case Tag(sym, List(Unit)) =>
        text(sym.toString)
      case Tag(sym, List(Tuple(args))) =>
        text(sym.toString) :: tuple(args.map(aux(_, paren = false)))
      case Tag(sym, args) =>
        text(sym.toString) :: tuple(args.map(aux(_, paren = false)))
      case AsIs(s) =>
        text(s)
      case Meta(s) =>
        text(meta(s))
      case RecordEmpty =>
        text("{}")
      case re: RecordExtend =>
        formatRecordBlock(re)
      case rr: RecordRestrict =>
        formatRecordBlock(rr)
      case Keyword(word, d) =>
        text(word) +: aux(d)
      case Unary(op, d) =>
        text(op) :: aux(d)
      case UnaryRightAfter(d, op) =>
        aux(d) :: text(op)
      case Binary(d1, op, d2) =>
        aux(d1) +: text(op) +: aux(d2)
      case IfThenElse(cond, thn, els) =>
        val condf = aux(cond, paren = false, inBlock = true)
        val thnf = aux(thn, paren = false, inBlock = true)
        val elsf = aux(els, paren = false, inBlock = true)
        group(
          text("if") +: parens(condf) +:
            curlyOpen(thnf) +: text("else") +: curlyOpen(elsf)
        )
      case Branch(d, branches) =>
        val branchHead = aux(d, paren = false, inBlock = true)
        val delimitedBranches = branches.toList.map { case (sym, dd) =>
          val labelf = aux(dd, paren = false, inBlock = true)
          text(sym.toString) :: text(":") +: breakIndent(labelf)
        }
        group(
          text("branch") +: curlyOpen(branchHead) +:
            text("labels") +: curlyOpen(semiSepOpt(delimitedBranches))
        )
      case Match(d, branches) =>
        val scrutineeF = aux(d, paren = false)
        val branchesF = branches.map { case (pat, guard, body) =>
          val patF = aux(pat, paren = false)
          val guardF = guard match {
            case None => Doc.empty
            case Some(g) => text("if") +: aux(g, paren = false, inBlock = true) +: Doc.empty
          }
          val bodyF = aux(body, paren = false, inBlock = true)
          text("case") +: patF +: guardF :: text("=>") :: breakIndent(bodyF)
        }
        group(
          text("match") +: scrutineeF +: curlyOpen(
            sep(breakWith(" "), branchesF)
          )
        )
      case TypeMatch(d, branches) =>
        val scrutineeF = aux(d, paren = false)
        val branchesF = branches.map { case (pat, tpe, body) =>
          val patF = aux(pat, paren = false)
          val tpeF = formatType(tpe, paren = false)
          val bodyF = aux(body, paren = false, inBlock = true)
          text("case") +: patF +: text(":") +: tpeF +: text("=>") :: breakIndent(bodyF)
        }
        group(
          text("typematch") +: scrutineeF +: curlyOpen(
            sep(breakWith(" "), branchesF)
          )
        )
      case Dot(d1, d2) =>
        aux(d1) :: text(".") :: aux(d2)
      case DoubleDot(d1, d2) =>
        aux(d1) :: text("..") :: aux(d2)
      case Hash(d1, d2) =>
        aux(d1) :: text("#") :: aux(d2)
      case s: Stm =>
        formatLetBlock(s, inBlock)
      case l: Let =>
        formatLetBlock(l, inBlock)
      case l: LetRec =>
        formatLetBlock(l, inBlock)
      case Scope(v, d) =>
        val bodyf = aux(d, paren = false, inBlock = true)
        val regionf = aux(v)
        text("region") +: regionf +: curly(bodyf)
      case Lambda(fparams, body) =>
        val params = fparams.map(_.v).map(aux(_, paren = false))
        tuplish(params) +: text("->") :: breakIndent(aux(body, paren = false))
      case App(f, args) =>
        aux(f) :: tuple(args.map(aux(_, paren = false)))
      case SquareApp(f, args) =>
        aux(f) :: squareTuple(args.map(aux(_, paren = false)))
      case DoubleSquareApp(f, args) =>
        aux(f) :: doubleSquareTuple(args.map(aux(_, paren = false)))
      case Assign(d1, d2) =>
        aux(d1) +: text(":=") +: aux(d2)
      case Ascription(v, tpe) =>
        aux(v) :: text(":") +: formatType(tpe, paren = false)
      case DoubleKeyword(word1, d1, word2, d2E) =>
        val d2Part = d2E match {
          case Left(d2) => aux(d2, paren = false)
          case Right(tpe) => formatType(tpe, paren = false)
        }
        group(text(word1) +: aux(d1, paren = false) +\: text(word2) +: d2Part)
      case TryCatch(d, rules) =>
        val rs = semiSepOpt(rules.map {
          case (sym, clazz, rule) =>
            val rulef = aux(rule, paren = false, inBlock = true)
            text("case") +: text(sym.toString) :: text(":") +:
              formatJavaClass(clazz) +: text("=>") :: breakIndent(rulef)
        })
        val bodyf = aux(d, paren = false, inBlock = true)
        group(
          text("try") +: curly(bodyf) +:
            text("catch") +: curly(rs)
        )
      case Throw(exp) =>
        group(
          text("throw") +: aux(exp, paren = false, inBlock = true)
        )
      case TryWith(d, eff, rules) =>
        val rs = semiSepOpt(rules.map {
          case (sym, params, rule) =>
            val rulef = aux(rule, paren = false, inBlock = true)
            text("def") +: text(sym.toString) :: tuple(params.map(aux(_, paren = false))) +:
              text("=") :: breakWith(" ") :: curlyOpen(rulef)
        })
        val bodyf = aux(d, paren = false, inBlock = true)
        group(
          text("try") +: curly(bodyf) +:
            text("with") +: text(eff.toString) +: curly(rs)
        )
      case NewObject(_, clazz, _, methods) =>
        group(text("new") +: formatJavaClass(clazz) +: curly(
          semiSepOpt(methods.map(formatJvmMethod))
        ))
      case Native(clazz) =>
        formatJavaClass(clazz)
    }
    d match {
      case _: Composite if paren => parens(doc)
      case _: Composite | _: Atom => doc
    }
  }

  private def formatJavaClass(clazz: Class[_]): Doc =
    text("##" + clazz.getName)

  private def formatJvmMethod(m: JvmMethod)(implicit i: Indent): Doc = {
    val JvmMethod(ident, fparams, clo, _) = m
    val fparamsf = fparams.map(aux(_, paren = false))
    val clof = aux(clo, paren = false, inBlock = true)
    group(
      text("def") +: text(ident.name) +:
        tuple(fparamsf) +: text("=") +\:
        clof
    )
  }

  private def formatLetBlock(d: LetBinder, inBlock: Boolean)(implicit i: Indent): Doc = {
    val (binders, body) = collectLetBinders(d)
    val bodyf = aux(body, paren = false)
    val bindersf = binders.map {
      case Stm(d1, _) =>
        aux(d1, paren = false)
      case Let(v, tpe, bind, _) =>
        val bindf = aux(bind, paren = false)
        text("let") +: aux(v) :: formatAscription(tpe) +: text("=") +: bindf
      case LetRec(v, tpe, bind, _) =>
        val bindf = aux(bind, paren = false)
        text("letrec") +: aux(v) :: formatAscription(tpe) +: text("=") +: bindf
    }
    val delimitedBinders = semiSep(bindersf :+ bodyf)
    if (inBlock) group(delimitedBinders)
    else curly(delimitedBinders)
  }

  private def formatRecordBlock(d: RecordOp)(implicit i: Indent): Doc = {
    val (exs, restOpt) = collectRecordOps(d)
    val exsf = exs.map {
      case RecordExtend(label, value, _) =>
        val valuef = aux(value, paren = false)
        text("+" + label.toString) +: text("=") +: valuef
      case RecordRestrict(label, _) =>
        text("-" + label.toString)
    }
    restOpt match {
      case Some(rest) =>
        val restf = aux(rest, paren = false)
        curly(commaSep(exsf) +: text("|") +\: restf)
      case None =>
        curly(commaSep(exsf) +: text("|") +\: text("{}"))
    }
  }

  private def formatAscription(tpe: Option[Type])(implicit i: Indent): Doc =
    tpe.map(t => text(":") +: formatType(t)).getOrElse(empty)

  private def collectLetBinders(d: Expr): (List[LetBinder], Expr) = {
    @tailrec
    def chase(d0: Expr, acc: List[LetBinder]): (List[LetBinder], Expr) = {
      d0 match {
        case s@Stm(_, d2) =>
          chase(d2, s :: acc)
        case l@Let(_, _, _, body) =>
          chase(body, l :: acc)
        case l@LetRec(_, _, _, body) =>
          chase(body, l :: acc)
        case other => (acc.reverse, other)
      }
    }

    chase(d, List())
  }

  private def collectRecordOps(d: Expr)(implicit i: Indent): (List[RecordOp], Option[Expr]) = {
    @tailrec
    def chase(d0: Expr, acc: List[RecordOp]): (List[RecordOp], Option[Expr]) = {
      d0 match {
        case re@RecordExtend(_, _, rest) =>
          chase(rest, re :: acc)
        case re@RecordRestrict(_, rest) =>
          chase(rest, re :: acc)
        case RecordEmpty =>
          (acc.reverse, None)
        case other =>
          (acc.reverse, Some(other))
      }
    }

    chase(d, List())
  }

  /**
    * Returns the [[Doc]] representation of `tpe`.
    *
    * @param paren if true, a parenthesis will be added, unless `tpe` is an
    *              atom.
    */
  private def formatType(tpe: Type, paren: Boolean = true)(implicit i: Indent): Doc = {
    val d = tpe match {
      case Type.Unit =>
        text("Unit")
      case Type.AsIs(s) =>
        text(s)
      case Type.App(obj, Nil) =>
        text(obj)
      case Type.App(obj, args) =>
        text(obj) :: squareTuple(args.map(formatType(_, paren = false)))
      case Type.Tuple(elms) =>
        tuple(elms.map(formatType(_, paren = false)))
      case arrow@Type.Arrow(_, _) =>
        val (curriedArgs, res) = collectArrowTypes(arrow)
        val formattedArgs = curriedArgs.map {
          case ts@(Type.Tuple(_) :: Nil) =>
            tuple(ts.map(formatType(_, paren = false)))
          case ts@(_ :: Nil) =>
            tuplish(ts.map(formatType(_, paren = true)))
          case ts =>
            tuplish(ts.map(formatType(_, paren = false)))
        }
        group(nest(sep(text(" ->") :: breakWith(" "), formattedArgs :+ formatType(res))))
      case Type.RecordEmpty =>
        text("{}")
      case re: Type.RecordExtend =>
        val (labels, restOpt) = collectRecordTypes(re)
        val exsf = labels.map {
          case Type.RecordExtend(label, value, _) =>
            text(label) +: text("=") +: formatType(value, paren = false)
        }
        restOpt match {
          case Some(rest) =>
            val restf = formatType(rest, paren = false)
            curly(commaSep(exsf) +: text("|") +\: restf)
          case None =>
            curlyTuple(exsf)
        }
      case Type.SchemaEmpty =>
        text("#{}")
      case se@Type.SchemaExtend(_, _, _) =>
        val (predicates, restOpt) = collectSchemaTypes(se)
        val predicatesf = predicates.map {
          case Type.SchemaExtend(name, Type.Tuple(elms), _) =>
            text(name) :: tuple(elms.map(formatType(_, paren = false)))
          case Type.SchemaExtend(name, Type.Unit, _) =>
            text(name)
          case Type.SchemaExtend(name, otherType, _) =>
            text(name) :: parens(formatType(otherType, paren = false))
        }
        restOpt match {
          case Some(rest) =>
            val restf = formatType(rest, paren = false)
            text("#") :: curly(commaSep(predicatesf) +: text("|") +\: restf)
          case None =>
            text("#") :: curlyTuple(predicatesf)
        }
      case Type.Native(clazz) =>
        formatJavaClass(clazz)
      case Type.JvmConstructor(constructor) =>
        formatJavaClass(constructor.getClass)
      case Type.JvmMethod(method) =>
        formatJavaClass(method.getClass)
      case Type.Meta(s) =>
        text(meta(s))
    }
    tpe match {
      case _: Type.Composite if paren => parens(d)
      case _: Type.Composite | _: Type.Atom => d
    }
  }

  /**
    * Collects a sequence of [[Type.Arrow]] into a shallow list of their
    * arguments and the final return type.
    */
  private def collectArrowTypes(tpe: Type): (List[List[Type]], Type) = {
    @tailrec
    def chase(tpe0: Type, acc: List[List[Type]]): (List[List[Type]], Type) = {
      tpe0 match {
        case Type.Arrow(args, res) => chase(res, args :: acc)
        case other => (acc.reverse, other)
      }
    }

    chase(tpe, List())
  }

  /**
    * Collects a sequence of [[Type.RecordExtend]] into a shallow list. The tail
    * is [[None]] if the sequence ends with a [[Type.RecordEmpty]].
    */
  private def collectRecordTypes(tpe: Type): (List[Type.RecordExtend], Option[Type]) = {
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

  /**
    * Collects a sequence of [[Type.SchemaExtend]] into a shallow list. The tail
    * is [[None]] if the sequence ends with a [[Type.SchemaEmpty]].
    */
  private def collectSchemaTypes(tpe: Type): (List[Type.SchemaExtend], Option[Type]) = {
    @tailrec
    def chase(tpe0: Type, acc: List[Type.SchemaExtend]): (List[Type.SchemaExtend], Option[Type]) = {
      tpe0 match {
        case se@Type.SchemaExtend(_, _, rest) =>
          chase(rest, se :: acc)
        case Type.SchemaEmpty =>
          (acc.reverse, None)
        case other =>
          (acc.reverse, Some(other))
      }
    }

    chase(tpe, List())
  }

  private def formatEffect(effect: Eff, paren: Boolean = true)(implicit i: Indent): Doc = effect match {
    case Eff.Pure => empty
    case Eff.Impure => text(" ") :: text("\\") +: text("Impure")
    case Eff.ControlImpure => text(" ") :: text("\\") +: text("ControlImpure")
    case Eff.AsIs(s) => text(" ") :: text("\\") +: text(s)
  }

}
