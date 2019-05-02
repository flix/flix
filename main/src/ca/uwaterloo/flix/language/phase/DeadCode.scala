package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.DeadCodeError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym

object DeadCode extends Phase[Root, Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, DeadCodeError] = flix.phase("DeadCode") {
    // TODO: Improve weird/terrible code below
    val defsRes = root.defs.map(x => visitDef(x._2)).toList.unzip
    val defsVal = defsRes._1
    val used = defsRes._2.foldLeft(Used()) (_ + _)
    val defined = Used(Set.empty, root.enums.values.map(x => visitEnum(x)).toSet.flatten, root.defs.keySet.filter(x => x.name != "main"))
    val unused = defined - used
    // TODO: Improve below code
    val usedVal =
      if (unused.defs.size == 0) {
        if (unused.tags.size == 0) {
          root.toSuccess
        } else {
          val unusedTagsHead = unused.tags.head
          DeadCodeError(root.enums(unusedTagsHead._1).cases(unusedTagsHead._2).loc, "Enum case never used").toFailure
        }
      } else {
        val unusedDefsHead = unused.defs.head
        println(unusedDefsHead.name); DeadCodeError(root.defs(unusedDefsHead).loc, "Def never used").toFailure
      }
    val newDefs = traverse(defsVal) {
      case defn => defn.map(x => x.sym -> x)
    }

    mapN(newDefs, usedVal) {
      case (defs, enums) => root.copy(defs = defs.toMap)
    }
  }

  private def visitEnum(enum: TypedAst.Enum): Set[(Symbol.EnumSym, String)] = {
    val enumKeys = enum.cases.keySet
    enumKeys.map(x => (enum.sym, x))
  }

  private def visitDef(defn: TypedAst.Def): (Validation[TypedAst.Def, DeadCodeError], Used) = { // TODO: Check
    val expU = visitExp(defn.exp)
    val expVal = expU.getTraversedVal
    val declSyms = (for (fparam <- defn.fparams if fparam.sym.text != "_unit") yield fparam.sym).toSet
    val unusedSyms = declSyms -- expU.vars
    val fparamSymVal = if (unusedSyms.size == 0) defn.toSuccess else DeadCodeError(unusedSyms.head.loc, "Formal parameter never used.").toFailure
    (mapN(expVal, fparamSymVal) {
      case _ => defn
    }, expU)
  }

  // TODO: Clean up variable names now that this functions returns things other than Validation
  private def visitExp(e: TypedAst.Expression): Used =
    e match {
      case TypedAst.Expression.Var(sym, tpe, eff, loc) =>
        Used(Set(sym), Set.empty, Set.empty, List(e.toSuccess))
      case TypedAst.Expression.Def(sym, tpe, eff, loc) =>
        Used(Set.empty, Set.empty, Set(sym), List(e.toSuccess))
      case TypedAst.Expression.Lambda(fparam, exp, tpe, eff, loc) =>
        val expU = visitExp(exp)
        if (expU.vars.contains(fparam.sym)) { // TODO: Account for case of Failure
          expU
        } else {
          Used() + DeadCodeError(fparam.sym.loc, "Lambda parameter never used.").toFailure
        }
      case TypedAst.Expression.Apply(exp1, exp2, tpe, eff, loc) => visitExp(exp1) + visitExp(exp2)
      case TypedAst.Expression.Unary(op, exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Binary(op, exp1, exp2, tpe, eff, loc) => visitExp(exp1) + visitExp(exp2)
      case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
        val letU = visitExp(exp1)
        val expU = visitExp(exp2)
        if(sym.text == "_temp" || expU.vars.contains(sym)) { // TODO: Consider alternatives
          letU + expU
        } else {
          letU + expU + DeadCodeError(sym.loc, "Variable never used.").toFailure
        }
      case TypedAst.Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) =>
        val used = visitExp(exp1) + visitExp(exp2)
        if(sym.text == "_temp" || used.vars.contains(sym)) {
          used
        } else {
          used + DeadCodeError(sym.loc, "Variable never used.").toFailure
        }
      case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) + visitExp(exp2) + visitExp(exp3)
      case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) =>
        val matchU = visitExp(exp)
        val rulesU = (for (r <- rules) yield {
          val guardU = visitExp(r.guard)
          val expU = visitExp(r.exp)
          val patRes = patternVar(r.pat)
          val unusedSyms = patRes -- expU.vars
          if(unusedSyms.size == 0) {
            guardU + expU
          } else {
            guardU + expU + DeadCodeError(unusedSyms.head.loc, "Matching variable(s) not used in expression.").toFailure
          }
        }).foldLeft(Used()) (_ + _)
        matchU + rulesU
      case TypedAst.Expression.Switch(rules, tpe, eff, loc) =>
        (for (r <- rules) yield {
          visitExp(r._1) + visitExp(r._2)
        }).foldLeft(Used()) (_ + _) // TODO: Improve
      case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        visitExp(exp) + (sym, tag)
      case TypedAst.Expression.Tuple(elms, tpe, eff, loc) =>
        elms.foldLeft(Used()) ((u, e) => u + visitExp(e))
      case TypedAst.Expression.RecordSelect(exp, label, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
        visitExp(value) + visitExp(rest)
      case TypedAst.Expression.RecordRestrict(label, rest, tpe, eff, loc) => visitExp(rest)
      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) =>
        elms.foldLeft(Used()) ((u, e) => u + visitExp(e))
      case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        visitExp(elm) + visitExp(len)
      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        visitExp(base) + visitExp(index)
      case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => visitExp(base)
      case TypedAst.Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
        visitExp(base) + visitExp(index) + visitExp(elm)
      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) =>
        visitExp(base) + visitExp(beginIndex) + visitExp(endIndex)
      case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) =>
        elms.foldLeft(Used()) ((u, e) => u + visitExp(e))
      case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => visitExp(elm)
      case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => visitExp(base)
      case TypedAst.Expression.VectorStore(base, index, elm, tpe, eff, loc) =>
        visitExp(base) + visitExp(elm)
      case TypedAst.Expression.VectorLength(base, tpe, eff, loc) => visitExp(base)
      case TypedAst.Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) =>
        visitExp(base) + visitExp(endIndex)
      case TypedAst.Expression.Ref(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Deref(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1) + visitExp(exp2)
      case TypedAst.Expression.HandleWith(exp, bindings, tpe, eff, loc) =>
        val bindingsU = bindings.foldLeft(Used()) ((u, b) => u + visitExp(b.exp))
        visitExp(exp) + bindingsU
      case TypedAst.Expression.Existential(fparam, exp, eff, loc) =>
        visitExp(exp) + fparam.sym
      case TypedAst.Expression.Universal(fparam, exp, eff, loc) =>
        visitExp(exp) + fparam.sym
      case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Cast(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
        args.foldLeft(Used()) ((u, e) => u + visitExp(e))
      case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        val rulesU = rules.foldLeft(Used()) ((u, c) => u + visitExp(c.exp) + c.sym)
        visitExp(exp) + rulesU
      case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1) + visitExp(exp2)
      case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        val defU = default match {
          case Some(ex) => visitExp(ex)
          case None => Used()
        }
        val rulesU = (for (r <- rules) yield {
          val chanU = visitExp(r.chan)
          val expU = visitExp(r.exp)
          if (expU.vars.contains(r.sym)) {
            chanU + expU
          } else {
            chanU + expU + DeadCodeError(r.sym.loc, "Select channel variable never used.").toFailure
          }
        }).foldLeft(Used()) (_ + _)
        defU + rulesU
      case TypedAst.Expression.Spawn(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Sleep(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.FixpointConstraint(c, tpe, eff, loc) =>
        val cparamsSyms = (for (cp <- c.cparams) yield {
          cp match {
            case TypedAst.ConstraintParam.HeadParam(sym, tpe, loc) => sym
            case TypedAst.ConstraintParam.RuleParam(sym, tpe, loc) => sym
          }
        }).toSet
        val headU = c.head match {
          case TypedAst.Predicate.Head.Atom(pred, terms, tpe, loc) =>
            visitExp(pred.exp) + terms.foldLeft(Used()) ((u, e) => u + visitExp(e))
        }
        val bodyU = for (b <- c.body) yield {
          b match {
            case TypedAst.Predicate.Body.Atom(pred, polarity, terms, tpe, loc) => visitExp(pred.exp)
            case TypedAst.Predicate.Body.Filter(sym, terms, loc) =>
              terms.foldLeft(Used()) ((u, e) => u + visitExp(e)) + sym
            case TypedAst.Predicate.Body.Functional(sym, term, loc) =>
              visitExp(term) + sym
          }
        }
        headU + bodyU.foldLeft(Used()) (_ + _) + cparamsSyms
      case TypedAst.Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1) + visitExp(exp2)
      case TypedAst.Expression.FixpointSolve(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
        visitExp(pred.exp) + visitExp(exp)
      case TypedAst.Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1) + visitExp(exp2)
      case _ => Used()
    }

  private def patternVar(pat: TypedAst.Pattern): Set[Symbol.VarSym] =
    pat match {
      case TypedAst.Pattern.Var(sym, tpe, loc) => Set(sym)
      case TypedAst.Pattern.Tag(sym, tag, pat, tpe, loc) => patternVar(pat) // Correct?
      case TypedAst.Pattern.Tuple(elms, tpe, loc) => (for (e <- elms) yield patternVar(e)).flatten.toSet
      case _ => Set.empty
    }
}

case class Used(vars: Set[Symbol.VarSym] = Set.empty, tags: Set[(Symbol.EnumSym, String)] = Set.empty, defs: Set[Symbol.DefnSym] = Set.empty, vals: List[Validation[TypedAst.Expression, DeadCodeError]] = List.empty) {
  def +(that: Used): Used = Used(vars ++ that.vars, tags ++ that.tags, defs ++ that.defs, vals ::: that.vals)
  def +(that: Symbol.VarSym) = Used(vars + that, tags, defs, vals)
  def +(that: Set[Symbol.VarSym]) = Used(vars ++ that, tags, defs, vals)
  def +(that: (Symbol.EnumSym, String)) = Used(vars, tags + that, defs, vals)
  def +(that: Symbol.DefnSym) = Used(vars, tags, defs + that, vals)
  def +(that: Validation[TypedAst.Expression, DeadCodeError]) = Used(vars, tags, defs, vals ::: List(that))
  def -(that: Used): Used = Used(vars -- that.vars, tags -- that.tags, defs -- that.defs, vals)
  def -(sym: Symbol.VarSym): Used = Used(vars - sym, tags, defs, vals)
  def removeVars: Used = Used(Set.empty, tags, defs, vals)
  def getTraversedVal: Validation[List[TypedAst.Expression], DeadCodeError] = traverse(this.vals) {
    case exp => exp
  }
}

