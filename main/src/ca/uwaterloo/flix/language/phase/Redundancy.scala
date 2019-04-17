/*
 *  Copyright 2019 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

// TODO: DOC
object Redundancy extends Phase[TypedAst.Root, TypedAst.Root] {

  val Empty: Validation[Used, RedundancyError] = Used.empty.toSuccess

  object Used {

    /**
      * Returns an object where no symbol is marked as used.
      */
    val empty: Used = Used(MultiMap.Empty, Set.empty, Set.empty, Set.empty)

    /**
      * Returns an object where the given enum symbol `sym` and `tag` are marked as used.
      */
    def of(sym: Symbol.EnumSym, tag: String): Used = empty.copy(enumSyms = MultiMap.Empty + (sym, tag))

    /**
      * Returns an object where the given defn symbol `sym` is marked as used.
      */
    def of(sym: Symbol.DefnSym): Used = empty.copy(defSyms = Set(sym))

    /**
      * Returns an object where the given predicate symbol `sym` is marked as used.
      */
    def of(sym: Symbol.PredSym): Used = empty.copy(predSyms = Set(sym))

    /**
      * Returns an object where the given variable symbol `sym` is marked as used.
      */
    def of(sym: Symbol.VarSym): Used = empty.copy(varSyms = Set(sym))

  }

  // TODO
  case class Used(enumSyms: MultiMap[Symbol.EnumSym, String], defSyms: Set[Symbol.DefnSym], predSyms: Set[Symbol.PredSym], varSyms: Set[Symbol.VarSym]) {
    def ++(that: Used): Used =
      if (this eq Used.empty) {
        that
      } else if (that eq Used.empty) {
        this
      } else {
        Used(
          this.enumSyms ++ that.enumSyms,
          this.defSyms ++ that.defSyms,
          this.predSyms ++ that.predSyms,
          this.varSyms ++ that.varSyms
        )
      }

    def --(syms: Set[Symbol.VarSym]): Used = copy(varSyms = varSyms -- syms)

    def -(sym: Symbol.VarSym): Used = copy(varSyms = varSyms - sym)
  }

  // TODO
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, RedundancyError] = flix.phase("Redundancy") {

    val defs = root.defs.map { case (_, v) => visitDef(v, root) }

    val usedVal = sequence(defs).map {
      case us => us.foldLeft(Used.empty)(_ ++ _)
    }

    for {
      used <- usedVal
      _ <- checkUnusedDefs(used)(root)
      _ <- checkUnusedEnumsAndTags(used)(root)
      _ <- checkUnusedRelationsAndLattices(used)(root)
    } yield root

  }

  // TODO
  private def visitDef(defn: TypedAst.Def, root: TypedAst.Root): Validation[Used, RedundancyError] = {
    for {
      u <- visitExp(defn.exp, Env.empty)
      _ <- checkUnusedFormalParameters(defn, u)
      _ <- checkUnusedTypeParameters(defn)
      //   _ <- constantFoldExp(defn.exp, Map.empty)
    } yield u
  }

  // TODO: Test cases.
  private def checkUnusedDefs(used: Used)(implicit root: Root): Validation[List[Unit], RedundancyError] = {
    // TODO: Maybe this can be cleaned up.
    traverse(root.defs) {
      case (sym, decl) if decl.mod.isPublic || sym.text == "main" =>
        // Def is public. No usage requirements.
        ().toSuccess
      case (sym, decl) =>
        // Def is non-public. Check if the def is used.
        if (unused(sym, used))
          UnusedDefSym(sym).toFailure
        else
          ().toSuccess
    }
  }

  /**
    * Checks for unused enum symbols and tags.
    */
  private def checkUnusedEnumsAndTags(used: Used)(implicit root: Root): Validation[List[Unit], RedundancyError] =
    traverse(root.enums) {
      case (sym, decl) if decl.mod.isPublic =>
        // Enum is public. No usage requirements.
        ().toSuccess
      case (sym, decl) =>
        // Enum is non-public.
        // Lookup usage information for this specific enum.
        used.enumSyms.get(sym) match {
          case None =>
            // Case 1: Enum is never used.
            UnusedEnumSym(sym).toFailure
          case Some(usedTags) =>
            // Case 2: Enum is used and here are its used tags.
            // Check if there is any unused tag.
            decl.cases.values.find(caze => !usedTags.contains(caze.tag.name)) match {
              case None => ().toSuccess
              case Some(caze) => UnusedEnumTag(sym, caze.tag).toFailure
            }
        }
    }

  /**
    * Checks for unused relation and lattice symbols.
    */
  private def checkUnusedRelationsAndLattices(used: Redundancy.Used)(implicit root: Root): Validation[List[Unit], RedundancyError] = {
    val unusedRelSyms = root.relations.keys.filter(sym => unused(sym, used))
    val unusedLatSyms = root.lattices.keys.filter(sym => unused(sym, used))
    val failures1 = unusedRelSyms.map(UnusedRelSym).toStream
    val failures2 = unusedLatSyms.map(UnusedLatSym).toStream

    if (failures1.isEmpty && failures2.isEmpty)
      Nil.toSuccess
    else
      Failure(failures1 #::: failures2)
  }

  // TODO
  private def checkUnusedFormalParameters(defn: Def, used: Used): Validation[List[Unit], RedundancyError] = {
    traverse(defn.fparams) {
      case FormalParam(sym, _, _, _) if unused(sym, used) => UnusedFormalParam(sym).toFailure
      case FormalParam(_, _, _, _) => ().toSuccess
    }
  }

  // TODO
  private def checkUnusedTypeParameters(defn: TypedAst.Def): Validation[List[Unit], RedundancyError] = {
    traverse(defn.tparams) {
      case TypeParam(name, tvar, _) if unused(tvar, defn.sc.base.typeVars) => UnusedTypeParam(name).toFailure
      case TypeParam(_, _, _) => ().toSuccess
    }
  }

  /**
    * Returns the symbols used in the given expression `e0` under the given environment `env0`.
    */
  private def visitExp(e0: TypedAst.Expression, env0: Env): Validation[Used, RedundancyError] = e0 match {
    case Expression.Unit(_) => Empty

    case Expression.True(_) => Empty

    case Expression.False(_) => Empty

    case Expression.Char(_, _) => Empty

    case Expression.Float32(_, _) => Empty

    case Expression.Float64(_, _) => Empty

    case Expression.Int8(_, _) => Empty

    case Expression.Int16(_, _) => Empty

    case Expression.Int32(_, _) => Empty

    case Expression.Int64(_, _) => Empty

    case Expression.BigInt(_, _) => Empty

    case Expression.Str(_, _) => Empty

    case Expression.Wild(_, _, _) => Empty

    case Expression.Var(sym, _, _, _) => Used.of(sym).toSuccess

    case Expression.Def(sym, _, _, _) => Used.of(sym).toSuccess

    case Expression.Eff(sym, _, _, _) => Empty

    case Expression.Hole(sym, _, _, _) => Empty

    case Expression.Lambda(fparam, exp, _, _, _) =>
      flatMapN(visitExp(exp, env0)) {
        case used if unused(fparam.sym, used) => UnusedFormalParam(fparam.sym).toFailure
        case used => used.toSuccess
      }

    case Expression.Apply(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.Unary(_, exp, _, _, _) => visitExp(exp, env0)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.Let(sym, exp1, exp2, _, _, _) =>
      // TODO: Extend env
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      flatMapN(us1, us2) {
        case (u1, u2) if unused(sym, u2) => UnusedVarSym(sym).toFailure
        case (u1, u2) => ((u1 ++ u2) - sym).toSuccess
      }

    case Expression.LetRec(sym, exp1, exp2, _, _, _) =>
      // TODO: Extend env
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      flatMapN(us1, us2) {
        // TODO: Redundancy: How do we want to check let-rec?
        case (u1, u2) if unused(sym, u1) => UnusedVarSym(sym).toFailure
        case (u1, u2) if unused(sym, u2) => UnusedVarSym(sym).toFailure
        case (u1, u2) => ((u1 ++ u2) - sym).toSuccess
      }

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      // TODO: Extend env
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      val us3 = visitExp(exp3, env0)
      mapN(us1, us2, us3)(_ ++ _ ++ _)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      flatMapN(us1, us2) {
        case (used1, used2) =>
          if (exp1.eff.isPure)
            UselessExpression(exp1.loc).toFailure
          else
            (used1 ++ used2).toSuccess
      }

    case Expression.Match(exp, rules, _, _, _) =>
      val usedMatch = visitExp(exp, env0)
      val usedRules = traverse(rules) {
        case MatchRule(pat, guard, body) =>
          val fvs = freeVars(pat)
          val stablePathOpt = toStablePath(exp)
          val extendedEnv = stablePathOpt match {
            case None => env0
            case Some(stablePath) => env0 + (stablePath -> pat)
          }
          // TODO: Need to use the subst somehow...
          flatMapN(checkImpossiblePattern(stablePathOpt, env0, pat), visitExp(guard, extendedEnv), visitExp(body, extendedEnv)) {
            case (_, usedGuard, usedBody) =>
              val unusedVarSyms = fvs.filter(sym => unused(sym, usedGuard) && unused(sym, usedBody)).toList
              if (unusedVarSyms.isEmpty)
                ((usedGuard ++ usedBody) -- fvs).toSuccess
              else
                Failure(unusedVarSyms.map(sym => UnusedVarSym(sym)).toStream)
          }
      }

      mapN(usedMatch, usedRules) {
        case (u1, xs) => xs.reduce(_ ++ _) ++ u1
      }

    case Expression.Switch(rules, _, _, _) =>
      val rulesVal = traverse(rules) {
        case (cond, body) => mapN(visitExp(cond, env0), visitExp(body, env0))(_ ++ _)
      }
      mapN(rulesVal) {
        case rs => rs.foldLeft(Used.empty)(_ ++ _)
      }

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      mapN(visitExp(exp, env0)) {
        case used => used ++ Used.of(sym, tag)
      }

    case Expression.Tuple(elms, _, _, _) => visitExps(elms, env0)

    case Expression.RecordEmpty(_, _, _) => Empty

    case Expression.RecordSelect(exp, _, _, _, _) => visitExp(exp, env0)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = visitExp(value, env0)
      val us2 = visitExp(rest, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.RecordRestrict(_, rest, _, _, _) => visitExp(rest, env0)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      visitExps(elms, env0)

    case Expression.ArrayNew(elm, len, _, _, _) =>
      val us1 = visitExp(elm, env0)
      val us2 = visitExp(len, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.ArrayLength(base, _, _, _) => visitExp(base, env0)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      val us3 = visitExp(elm, env0)
      mapN(us1, us2, us3)(_ ++ _ ++ _)

    case Expression.ArraySlice(base, begin, end, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(begin, env0)
      val us3 = visitExp(end, env0)
      mapN(us1, us2, us3)(_ ++ _ ++ _)

    case Expression.VectorLit(elms, _, _, _) => visitExps(elms, env0)

    case Expression.VectorNew(elm, len, _, _, _) => visitExp(elm, env0)

    case Expression.VectorLoad(base, _, _, _, _) => visitExp(base, env0)

    case Expression.VectorStore(base, _, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(elm, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.VectorLength(base, _, _, _) => visitExp(base, env0)

    case Expression.VectorSlice(base, _, _, _, _, _) => visitExp(base, env0)

    case Expression.Ref(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Deref(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? // TODO

    case Expression.Existential(fparam, exp, _, _) =>
      flatMapN(visitExp(exp, env0)) {
        case used if unused(fparam.sym, used) => UnusedFormalParam(fparam.sym).toFailure
        case used => used.toSuccess
      }

    case Expression.Universal(fparam, exp, _, _) =>
      flatMapN(visitExp(exp, env0)) {
        case used if unused(fparam.sym, used) => UnusedFormalParam(fparam.sym).toFailure
        case used => used.toSuccess
      }

    case Expression.Ascribe(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Cast(exp, _, _, _) => visitExp(exp, env0)

    case Expression.NativeConstructor(_, args, _, _, _) => visitExps(args, env0)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? // TODO

    case Expression.NativeField(_, _, _, _) => Used.empty.toSuccess

    case Expression.NativeMethod(_, args, _, _, _) => visitExps(args, env0)

    case Expression.NewChannel(exp, _, _, _) => visitExp(exp, env0)

    case Expression.GetChannel(exp, _, _, _) => visitExp(exp, env0)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.SelectChannel(rules, defaultOpt, _, _, _) =>
      val defaultVal = defaultOpt match {
        case None => Empty
        case Some(default) => visitExp(default, env0)
      }

      // TODO: Extend env
      val rulesVal = traverse(rules) {
        case SelectChannelRule(sym, chan, body) =>
          val chanVal = visitExp(chan, env0)
          val bodyVal = visitExp(body, env0)
          flatMapN(chanVal, bodyVal) {
            case (usedChan, usedBody) if unused(sym, usedBody) => UnusedVarSym(sym).toFailure
            case (usedChan, usedBody) => (usedChan ++ usedBody).toSuccess
          }
      }
      mapN(defaultVal, rulesVal) {
        case (defaultUsed, rulesUsed) => rulesUsed.foldLeft(defaultUsed)(_ ++ _)
      }

    case Expression.Spawn(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Sleep(exp, _, _, _) => visitExp(exp, env0)

    case Expression.FixpointConstraint(c, _, _, _) => visitConstraint(c, env0)

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.FixpointSolve(exp, _, _, _) => visitExp(exp, env0)

    case Expression.FixpointProject(_, exp, _, _, _) => visitExp(exp, env0) // TODO: Use predSym here?

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      mapN(us1, us2)(_ ++ _)

    case Expression.UserError(_, _, _) => Empty
  }

  private def visitExps(es: List[TypedAst.Expression], env0: Env): Validation[Used, RedundancyError] =
    mapN(traverse(es)(visitExp(_, env0))) {
      case xs => xs.foldLeft(Used.empty)(_ ++ _)
    }

  /**
    * Returns the free variables in the pattern `p0`.
    */
  private def freeVars(p0: Pattern): Set[Symbol.VarSym] = p0 match {
    case Pattern.Wild(_, _) => Set.empty
    case Pattern.Var(sym, _, _) => Set(sym)
    case Pattern.Unit(loc) => Set.empty
    case Pattern.True(loc) => Set.empty
    case Pattern.False(loc) => Set.empty
    case Pattern.Char(_, _) => Set.empty
    case Pattern.Float32(_, _) => Set.empty
    case Pattern.Float64(_, _) => Set.empty
    case Pattern.Int8(_, _) => Set.empty
    case Pattern.Int16(_, _) => Set.empty
    case Pattern.Int32(_, _) => Set.empty
    case Pattern.Int64(_, _) => Set.empty
    case Pattern.BigInt(_, _) => Set.empty
    case Pattern.Str(_, _) => Set.empty
    case Pattern.Tag(_, _, pat, _, _) => freeVars(pat)
    case Pattern.Tuple(pats, _, _) => pats.foldLeft(Set.empty[Symbol.VarSym]) {
      case (acc, pat) => acc ++ freeVars(pat)
    }
  }

  /**
    * Returns the symbols used in the given constraint `c0` under the given environment `env0`.
    */
  private def visitConstraint(c0: Constraint, env0: Env): Validation[Used, RedundancyError] = {
    val Constraint(_, head, body, _) = c0
    mapN(visitHeadPred(head, env0), traverse(body)(visitBodyPred(_, env0))) {
      case (h, bs) => bs.foldLeft(h)(_ ++ _)
    }
  }

  /**
    * Returns the symbols used in the given head predicate `h0` under the given environment `env0`.
    */
  private def visitHeadPred(h0: Predicate.Head, env0: Env): Validation[Used, RedundancyError] = h0 match {
    case Head.Atom(pred, terms, _, _) =>
      mapN(visitExp(pred.exp, env0), visitExps(terms, env0)) {
        case (usedParam, usedTerms) => Used.of(pred.sym) ++ usedParam ++ usedTerms
      }
  }

  /**
    * Returns the symbols used in the given body predicate `h0` under the given environment `env0`.
    */
  private def visitBodyPred(b0: Predicate.Body, env0: Env): Validation[Used, RedundancyError] = b0 match {
    case Body.Atom(pred, _, terms, _, _) =>
      mapN(visitExp(pred.exp, env0)) {
        case usedParam => Used.of(pred.sym) ++ usedParam
      }

    // TODO: Add test cases for predicates.

    case Body.Filter(sym, terms, loc) => ??? // TODO
    case Body.Functional(sym, term, loc) => ??? // TODO
  }


  // TODO: Should prbl. not take an option.
  private def checkImpossiblePattern(stablePathOpt: Option[StablePath], env: Env, pat: Pattern): Validation[Unit, RedundancyError] = {
    stablePathOpt match {
      case None => ().toSuccess
      case Some(stablePath) => env.env.get(stablePath) match {
        case Some(pat2) => mapN(unify(pat, pat2)) {
          case subst =>
            // TODO: use the subst?
            ()
        }
        case None => ().toSuccess
      }
    }
  }


  // TODO: We could store an upper, lower bound, and equal. At least for integers.
  // TODO: For other types it would just be equal and not equal.
  // TODO: Might even be for entire expressions, like xs.length > 0, xs.length == 0, and not just variables.


  // TODO: Report an error if all arguments are known to a function call that is pure. but a function could be helping by constructing some large structure.
  // TODO: Maybe we need a measure on physical code size?


  // TODO: Code like f(x), and f(x) is redundant if both are pure... This is just common sub-expression elimination.

  // TODO: Need notion of stable expression which should be used instead of variable symbol., but also need to take purity into account.
  sealed trait StablePath

  object StablePath {

    // TODO: What should be considered a stable path?

    case class Var(sym: Symbol.VarSym) extends StablePath

    case class RecordSelect(sp: StablePath, label: String) extends StablePath

    // TODO: Add additional cases.

  }

  def toStablePath(e0: Expression): Option[StablePath] = e0 match {

    case Expression.Var(sym, _, _, _) => Some(StablePath.Var(sym))

    case Expression.RecordSelect(exp, label, _, _, _) => toStablePath(exp).map(sp => StablePath.RecordSelect(sp, label))

    case _ => None
  }

  /**
    * Attempts to unify the two given patterns `p1` and `p2`.
    *
    * Returns a substitution if successful. Otherwise returns a redundancy error.
    */
  // TODO: Have to check that there is no infinite recursion here...
  private def unify(p1: Pattern, p2: Pattern): Validation[Substitution, RedundancyError] = (p1, p2) match {
    case (Pattern.Wild(_, _), _) => Substitution.empty.toSuccess

    case (_, Pattern.Wild(_, _)) => Substitution.empty.toSuccess

    case (Pattern.Var(sym1, _, _), Pattern.Var(sym2, _, _)) => Substitution(Map(sym1 -> p2)).toSuccess // TODO: Is this safe?

    case (Pattern.Var(sym, _, _), _) => Substitution(Map(sym -> p2)).toSuccess

    case (_, Pattern.Var(sym, _, _)) => Substitution(Map(sym -> p1)).toSuccess

    case (Pattern.Unit(_), Pattern.Unit(_)) => Substitution.empty.toSuccess

    case (Pattern.True(_), Pattern.True(_)) => Substitution.empty.toSuccess

    case (Pattern.False(_), Pattern.False(_)) => Substitution.empty.toSuccess

    case (Pattern.Char(lit1, _), Pattern.Char(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Float32(lit1, _), Pattern.Float32(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Float64(lit1, _), Pattern.Float64(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Int8(lit1, _), Pattern.Int8(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Int16(lit1, _), Pattern.Int16(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Int32(lit1, _), Pattern.Int32(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Int64(lit1, _), Pattern.Int64(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.BigInt(lit1, _), Pattern.BigInt(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Str(lit1, _), Pattern.Str(lit2, _)) if lit1 == lit2 => Substitution.empty.toSuccess

    case (Pattern.Tag(_, tag1, pat1, _, _), Pattern.Tag(_, tag2, pat2, _, _)) if tag1 == tag2 => unify(pat1, pat2)

    case (Pattern.Tuple(elms1, _, _), Pattern.Tuple(elms2, _, _)) => ??? // TODO

    case _ => RedundancyError.UselessPatternMatch(p1, p2).toFailure
  }


  private def unifyAll(ps1: List[Pattern], ps2: List[Pattern]): Validation[Substitution, RedundancyError] = (ps1, ps2) match {
    case (Nil, Nil) => Substitution.empty.toSuccess
    case _ => ??? // TODO
  }


  private def unused(sym: Symbol.DefnSym, used: Used): Boolean =
    !used.defSyms.contains(sym)

  /**
    * Returns `true` if `sym` is unused according to the argument `used`.
    */
  private def unused(sym: Symbol.PredSym, used: Used): Boolean =
    !used.predSyms.contains(sym)

  /**
    * Returns `true` if the type variable `tvar` is unused according to the argument `used`.
    */
  private def unused(tvar: Type.Var, used: Set[Type.Var]): Boolean = !used.contains(tvar)

  private def unused(sym: Symbol.VarSym, used: Used): Boolean =
    !used.varSyms.contains(sym) && sym.loc != SourceLocation.Unknown // TODO: Need better mechanism.


  // TODO: Carry the local environment mapping vars to patterns
  // TODO: but also carry equality relation... which should probably be a bimap (?)
  // TODO: Introduce bimap class?
  object Env {
    val empty: Env = Env(Map.empty, MultiMap.Empty, MultiMap.Empty)
  }

  // TODO: Env should probably allow stable paths.
  case class Env(env: Map[StablePath, Pattern], eq1: MultiMap[StablePath, StablePath], eq2: MultiMap[StablePath, StablePath]) {

    // TODO: Should take a stable path.
    def +(p: (StablePath, Pattern)): Env = {
      val (stablePath, pattern) = p
      copy(env = env + (stablePath -> pattern))
    }

  }

  // TODO: Maybe this is actually a relation...
  // TODO: Then, when we see a constraint x == 1, we can check if that is compatible with the values of x is mapped.

  // Note the eq maps do not contain transitive closure.

  /**
    * Companion object of the [[Substitution]] class.
    */
  object Substitution {
    /**
      * The empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)
  }

  /**
    * TODO: A substitution is a map from type variables to types.
    */
  case class Substitution(m: Map[Symbol.VarSym, Pattern]) {

    /**
      * Applies `this` substitution to the given type `tpe`.
      */
    def apply(tpe: Pattern): Pattern = ??? // TODO: Check for occurs check.

    /**
      * Applies `this` substitution to the given types `ts`.
      */
    def apply(ts: List[Pattern]): List[Pattern] = ts map apply

    /**
      * Returns the left-biased composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      Substitution(this.m ++ that.m.filter(kv => !this.m.contains(kv._1)))
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      val m = that.m.foldLeft(Map.empty[Symbol.VarSym, Pattern]) {
        case (macc, (x, t)) => macc.updated(x, this.apply(t))
      }
      Substitution(m) ++ this
    }

  }

  object MultiMap {
    // TODO: rename to empty.
    def Empty[K, V]: MultiMap[K, V] = MultiMap(Map.empty)
  }

  case class MultiMap[K, V](m: Map[K, Set[V]]) {

    def get(k: K): Option[Set[V]] = m.get(k)

    def +(k: K, v: V): MultiMap[K, V] = {
      val s = m.getOrElse(k, Set.empty)
      MultiMap(m + (k -> (s + v)))
    }

    def +(k: K, v: Set[V]): MultiMap[K, V] = {
      val s = m.getOrElse(k, Set.empty)
      MultiMap(m + (k -> (s ++ v)))
    }

    // TODO: Efficiency
    def ++(that: MultiMap[K, V]): MultiMap[K, V] = {
      val keys2 = that.m.keys
      keys2.foldLeft(this) {
        case (macc, k) => macc + (k, that.m(k))
      }
    }
  }

  // TODO: Check unused type parameters in enums

  // TODO: What counts as a use of an enum? Is it enough to (a) mention its type, (b) to use it in a pat match, or (c) to actually construct a value.
  // TODO: The pattern matching is difficult, because you could have a default match onsomething just of that type.
  // TODO: What about the void enum? How would you deal with that? What about the singleton. The above choices start to seem more false in the presence of those.
  // TODO: Add appropriate cases once a definition has been decided.
  // And if you just require it to be used in a type, should that type then be used?

  // TODO: Type Parameters: When is a type parameter considered to be used?

  // TODO: Allow wildcards everywhere.
  // TODO: Add tests for wildcards.

  // TODO: How do we want to think about shadowing? If we allow shadowing we need to augment error messages to
  // to explain that the dead variable could have been shadowed.

  // TODO: In the case of predicates, do we want to assert that there are some facts? What about in the presence of
  // first-class constraints?

  // TODO: Does new channel have a side-effect or not? Is it allowed to be discarded?
  // TODO: How do we want to deal with expressions that have side-effects and a return value that must be used.
  // E.g. like the above.

  // TODO: Add more tests for useless expressions ones the effect system is implemented.

  // TODO: We should probably disallow shadowing, because of things like this:
  // TODO: Shadowing in pattern matches:
  // match o with {
  //   case Some(x) => match o with { case x => }}
  // }

  // TODO: Also talk about linear patterns, and why disallow them.

  // TODO: No implicit promotions. No implicit coercions.

  // TODO: Rewrite tests to not use Option, but some other new type.

  // TODO: Should we also consider tricky cases such as:
  // match s with {
  // case Circle(Red) =>
  // case Circle(x) =>
  // where we know that x cannot be red, because that would have matched?
  // What about nested patterns like:
  // def main(): Int =
  //    let s = Circle(Red);
  //    match s with {
  //        case Circle(Red) => 123
  //        case Square(Blu) => match s with {
  //            case Square(Red) =>
  //        }
  //        case _ => Square(Blu)
  //    }

}
