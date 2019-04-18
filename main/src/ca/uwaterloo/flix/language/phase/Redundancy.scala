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
import ca.uwaterloo.flix.util.collection.MultiMap

// TODO: DOC
object Redundancy extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Checks the given AST `root` for redundancies.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, RedundancyError] = flix.phase("Redundancy") {
    // Return early if the redundancy phase is disabled.
    if (flix.options.xallowredundancies) {
      return root.toSuccess
    }

    // Computes all used symbols in all defs.
    val usedDefs = root.defs.foldLeft(Used.empty) {
      case (acc, (sym, decl)) => acc ++ visitDef(decl)(root)
    }

    // Computes all used symbols in all lattices.
    val usedLats = root.latticeComponents.values.foldLeft(Used.empty) {
      case (acc, LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc)) =>
        acc ++ visitExps(bot :: top :: equ :: leq :: lub :: glb :: Nil, Env.empty)
    }

    // Computes all used symbols.
    val usedAll = usedLats ++ usedDefs

    // Check for unused symbols.
    val usedRes =
      checkUnusedDefs(usedAll)(root) ++
        checkUnusedEnumsAndTags(usedAll)(root) ++
        checkUnusedRelations(usedAll)(root) ++
        checkUnusedLattices(usedAll)(root)

    // Return the root if successful, otherwise returns all redundancy errors.
    usedRes.toValidation(root) // TODO: Ensure no local var symbols are floating around
  }

  /**
    * Checks for unused symbols in the given definition and returns all used symbols.
    */
  private def visitDef(defn: TypedAst.Def)(implicit root: Root): Used = {

    /**
      * Checks for unused formal parameters.
      */
    def checkUnusedFormalParameters(used: Used): Used = {
      val unusedParams = defn.fparams.collect {
        case fparam if unused(fparam.sym, used) => UnusedFormalParam(fparam.sym)
      }
      used ++ unusedParams
    }

    /**
      * Checks for unused type parameters.
      */
    def checkUnusedTypeParameters(used: Used): Used = {
      val unusedParams = defn.tparams.collect {
        case tparam if unused(tparam.tpe, defn.sc.base.typeVars) => UnusedTypeParam(tparam.name)
      }
      used ++ unusedParams
    }

    // Compute the used symbols inside the definition.
    val used = visitExp(defn.exp, Env.empty)

    // Check for unused parameters and remove all variable symbols.
    (used ++ checkUnusedFormalParameters(used) ++ checkUnusedTypeParameters(used)).copy(varSyms = Set.empty)
  }

  /**
    * Checks for unused definition symbols.
    */
  private def checkUnusedDefs(used: Used)(implicit root: Root): Used = {
    root.defs.foldLeft(used) {
      case (acc, (sym, decl)) if unused(sym, used) =>
        if (decl.mod.isPublic || decl.ann.isTest || sym.text == "main")
          acc
        else
          acc + UnusedDefSym(sym)
      case (acc, _) => acc
    }
  }

  /**
    * Checks for unused enum symbols and tags.
    */
  private def checkUnusedEnumsAndTags(used: Used)(implicit root: Root): Used = {
    root.enums.foldLeft(used) {
      case (acc, (sym, decl)) if decl.mod.isPublic =>
        // Enum is public. No usage requirements.
        acc
      case (acc, (sym, decl)) =>
        // Enum is non-public.
        // Lookup usage information for this specific enum.
        used.enumSyms.get(sym) match {
          case None =>
            // Case 1: Enum is never used.
            acc + UnusedEnumSym(sym)
          case Some(usedTags) =>
            // Case 2: Enum is used and here are its used tags.
            // Check if there is any unused tag.
            decl.cases.values.find(caze => !usedTags.contains(caze.tag.name)) match {
              case None => acc
              case Some(caze) => acc + UnusedEnumTag(sym, caze.tag)
            }
        }
    }
  }

  /**
    * Checks for unused relation symbols.
    */
  private def checkUnusedRelations(used: Redundancy.Used)(implicit root: Root): Used = {
    val unusedRelSyms = root.relations.collect {
      case (sym, decl) if unused(sym, decl, used) => UnusedRelSym(sym)
    }
    used ++ unusedRelSyms
  }

  /**
    * Checks for unused lattice symbols.
    */
  private def checkUnusedLattices(used: Redundancy.Used)(implicit root: Root): Used = {
    val unusedLatSyms = root.lattices.collect {
      case (sym, decl) if unused(sym, decl, used) => UnusedLatSym(sym)
    }
    used ++ unusedLatSyms
  }

  /**
    * Returns the symbols used in the given expression `e0` under the given environment `env0`.
    */
  private def visitExp(e0: TypedAst.Expression, env0: Env): Used = e0 match {
    case Expression.Unit(_) => Used.empty

    case Expression.True(_) => Used.empty

    case Expression.False(_) => Used.empty

    case Expression.Char(_, _) => Used.empty

    case Expression.Float32(_, _) => Used.empty

    case Expression.Float64(_, _) => Used.empty

    case Expression.Int8(_, _) => Used.empty

    case Expression.Int16(_, _) => Used.empty

    case Expression.Int32(_, _) => Used.empty

    case Expression.Int64(_, _) => Used.empty

    case Expression.BigInt(_, _) => Used.empty

    case Expression.Str(_, _) => Used.empty

    case Expression.Wild(_, _, _) => Used.empty

    case Expression.Var(sym, _, _, _) => Used.of(sym)

    case Expression.Def(sym, _, _, _) => Used.of(sym)

    case Expression.Eff(sym, _, _, _) => Used.empty

    case Expression.Hole(sym, _, _, _) => Used.empty

    case Expression.Lambda(fparam, exp, _, _, _) =>
      val us = visitExp(exp, env0)
      if (unused(fparam.sym, us))
        us - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us - fparam.sym

    case Expression.Apply(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.Let(sym, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      if (unused(sym, us2))
        (us1 ++ us2) - sym + UnusedVarSym(sym)
      else
        (us1 ++ us2) - sym

    case Expression.LetRec(sym, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      if (unused(sym, us1) || unused(sym, us2))
        (us1 ++ us2) - sym + UnusedVarSym(sym)
      else
        (us1 ++ us2) - sym

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      val us3 = visitExp(exp3, env0)
      us1 ++ us2 ++ us3

    case Expression.Stm(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      if (exp1.eff.isPure)
        us1 ++ us2 + UselessExpression(exp1.loc)
      else
        us1 ++ us2

    case Expression.Match(exp, rules, _, _, _) =>
      val usedMatch = visitExp(exp, env0)
      val usedRules = rules map {
        case MatchRule(pat, guard, body) =>
          val fvs = freeVars(pat)
          val stablePathOpt = toStablePath(exp)
          val extendedEnv = stablePathOpt match {
            case None => env0
            case Some(stablePath) => env0 + (stablePath -> pat)
          }
          val usedGuard = visitExp(guard, extendedEnv)
          val usedBody = visitExp(body, extendedEnv)
          // TODO: Impossible pattern

          val unusedVarSyms = fvs.filter(sym => unused(sym, usedGuard) && unused(sym, usedBody)).map(UnusedVarSym)
          (usedGuard ++ usedBody) -- fvs ++ unusedVarSyms
      }

      usedRules.foldLeft(usedMatch) {
        case (acc, u) => acc ++ u
      }

    case Expression.Switch(rules, _, _, _) =>
      rules.foldLeft(Used.empty) {
        case (acc, (cond, body)) => acc ++ visitExp(cond, env0) ++ visitExp(body, env0)
      }

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      val us = visitExp(exp, env0)
      Used.of(sym, tag) ++ us

    case Expression.Tuple(elms, _, _, _) =>
      visitExps(elms, env0)

    case Expression.RecordEmpty(_, _, _) =>
      Used.empty

    case Expression.RecordSelect(exp, _, _, _, _) => visitExp(exp, env0)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = visitExp(value, env0)
      val us2 = visitExp(rest, env0)
      us1 ++ us2

    case Expression.RecordRestrict(_, rest, _, _, _) => visitExp(rest, env0)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      visitExps(elms, env0)

    case Expression.ArrayNew(elm, len, _, _, _) =>
      val us1 = visitExp(elm, env0)
      val us2 = visitExp(len, env0)
      us1 ++ us2

    case Expression.ArrayLoad(base, index, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      us1 ++ us2

    case Expression.ArrayLength(base, _, _, _) =>
      visitExp(base, env0)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(index, env0)
      val us3 = visitExp(elm, env0)
      us1 ++ us2 ++ us3

    case Expression.ArraySlice(base, begin, end, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(begin, env0)
      val us3 = visitExp(end, env0)
      us1 ++ us2 ++ us3

    case Expression.VectorLit(elms, _, _, _) =>
      visitExps(elms, env0)

    case Expression.VectorNew(elm, _, _, _, _) =>
      visitExp(elm, env0)

    case Expression.VectorLoad(base, _, _, _, _) =>
      visitExp(base, env0)

    case Expression.VectorStore(base, _, elm, _, _, _) =>
      val us1 = visitExp(base, env0)
      val us2 = visitExp(elm, env0)
      us1 ++ us2

    case Expression.VectorLength(base, _, _, _) => visitExp(base, env0)

    case Expression.VectorSlice(base, _, _, _, _, _) => visitExp(base, env0)

    case Expression.Ref(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Deref(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? // TODO

    case Expression.Existential(fparam, exp, _, _) =>
      val us = visitExp(exp, env0)
      if (unused(fparam.sym, us))
        us - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us - fparam.sym

    case Expression.Universal(fparam, exp, _, _) =>
      val us = visitExp(exp, env0)
      if (unused(fparam.sym, us))
        us - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        us - fparam.sym

    case Expression.Ascribe(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Cast(exp, _, _, _) => visitExp(exp, env0)

    case Expression.NativeConstructor(_, args, _, _, _) => visitExps(args, env0)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      ??? // TODO

    case Expression.NativeField(_, _, _, _) => Used.empty

    case Expression.NativeMethod(_, args, _, _, _) => visitExps(args, env0)

    case Expression.NewChannel(exp, _, _, _) => visitExp(exp, env0)

    case Expression.GetChannel(exp, _, _, _) => visitExp(exp, env0)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.SelectChannel(rules, defaultOpt, _, _, _) =>
      val defaultUsed = defaultOpt match {
        case None => Used.empty
        case Some(default) => visitExp(default, env0)
      }

      val rulesUsed = rules map {
        case SelectChannelRule(sym, chan, body) =>
          val chanUsed = visitExp(chan, env0)
          val bodyUsed = visitExp(body, env0)
          if (unused(sym, bodyUsed))
            (chanUsed ++ bodyUsed) - sym + UnusedVarSym(sym)
          else
            (chanUsed ++ bodyUsed) - sym
      }

      rulesUsed.foldLeft(defaultUsed) {
        case (acc, used) => acc ++ used
      }

    case Expression.Spawn(exp, _, _, _) => visitExp(exp, env0)

    case Expression.Sleep(exp, _, _, _) => visitExp(exp, env0)

    case Expression.FixpointConstraint(c, _, _, _) =>
      visitConstraint(c, env0)

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0)
      val us2 = visitExp(exp2, env0)
      us1 ++ us2

    case Expression.FixpointSolve(exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.FixpointProject(pred, exp, _, _, _) =>
      val us1 = visitExp(pred.exp, env0)
      val us2 = visitExp(exp, env0)
      Used.of(pred.sym) ++ us1 ++ us2

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      val used1 = visitExp(exp1, env0)
      val used2 = visitExp(exp2, env0)
      used1 ++ used2

    case Expression.UserError(_, _, _) =>
      Used.empty
  }

  private def visitExps(es: List[TypedAst.Expression], env0: Env): Used =
    es.foldLeft(Used.empty) {
      case (acc, exp) => acc ++ visitExp(exp, env0)
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
  private def visitConstraint(c0: Constraint, env0: Env): Used = {
    val zero = visitHeadPred(c0.head, env0)
    val used = c0.body.foldLeft(zero) {
      case (acc, b) => acc ++ visitBodyPred(b, env0)
    }
    used -- c0.cparams.map(_.sym)
  }

  /**
    * Returns the symbols used in the given head predicate `h0` under the given environment `env0`.
    */
  private def visitHeadPred(h0: Predicate.Head, env0: Env): Used = h0 match {
    case Head.Atom(pred, terms, _, _) =>
      Used.of(pred.sym) ++ visitExp(pred.exp, env0) ++ visitExps(terms, env0)
  }


  /**
    * Returns the symbols used in the given body predicate `h0` under the given environment `env0`.
    */
  private def visitBodyPred(b0: Predicate.Body, env0: Env): Used = b0 match {
    case Body.Atom(pred, _, terms, _, _) =>
      Used.of(pred.sym) ++ visitExp(pred.exp, env0)

    case Body.Filter(sym, terms, _) =>
      Used.of(sym) ++ visitExps(terms, env0)

    case Body.Functional(sym, term, _) =>
      Used.of(sym) ++ visitExp(term, env0)
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


  def toStablePath(e0: Expression): Option[StablePath] = e0 match {

    case Expression.Var(sym, _, _, _) =>
      Some(StablePath.Var(sym))

    case Expression.RecordSelect(exp, label, _, _, _) =>
      toStablePath(exp).map(sp => StablePath.RecordSelect(sp, label))

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

  // TODO: DOC
  private def unifyAll(ps1: List[Pattern], ps2: List[Pattern]): Validation[Substitution, RedundancyError] = (ps1, ps2) match {
    case (Nil, Nil) => Substitution.empty.toSuccess
    case _ => ??? // TODO
  }

  // TODO: Refactor all of these unused, maybe call them them used?

  // TODO: DOC
  private def unused(sym: Symbol.DefnSym, used: Used): Boolean =
    !used.defSyms.contains(sym)

  private def unused(sym: Symbol.RelSym, decl: Relation, used: Used): Boolean =
    !decl.mod.isPublic && !used.predSyms.contains(sym)

  private def unused(sym: Symbol.LatSym, decl: Lattice, used: Used): Boolean =
    !decl.mod.isPublic && !used.predSyms.contains(sym)

  /**
    * Returns `true` if the type variable `tvar` is unused according to the argument `used`.
    */
  private def unused(tvar: Type.Var, used: Set[Type.Var]): Boolean = !used.contains(tvar)

  // TODO: DOC
  private def unused(sym: Symbol.VarSym, used: Used): Boolean =
    !sym.text.startsWith("_") && !used.varSyms.contains(sym) && sym.loc != SourceLocation.Unknown // TODO: Need better mechanism.

  // TODO: Need notion of stable expression which should be used instead of variable symbol., but also need to take purity into account.
  sealed trait StablePath

  object StablePath {

    // TODO: What should be considered a stable path?

    case class Var(sym: Symbol.VarSym) extends StablePath

    case class RecordSelect(sp: StablePath, label: String) extends StablePath

    // TODO: Add additional cases.

  }

  // TODO: Add test cases for predicates.

  // TODO: Carry the local environment mapping vars to patterns
  // TODO: but also carry equality relation... which should probably be a bimap (?)
  // TODO: Introduce bimap class?
  object Env {
    val empty: Env = Env(Map.empty, MultiMap.empty, MultiMap.empty)
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

  object Used {

    /**
      * Returns an object where no symbol is marked as used.
      */
    val empty: Used = Used(MultiMap.empty, Set.empty, Set.empty, Set.empty, Stream.empty)

    /**
      * Returns an object where the given enum symbol `sym` and `tag` are marked as used.
      */
    def of(sym: Symbol.EnumSym, tag: String): Used = empty.copy(enumSyms = MultiMap.empty + (sym, tag))

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

  /**
    * A representation of used symbols.
    */
  case class Used(enumSyms: MultiMap[Symbol.EnumSym, String], defSyms: Set[Symbol.DefnSym], predSyms: Set[Symbol.PredSym], varSyms: Set[Symbol.VarSym], errors: Stream[RedundancyError]) {
    /**
      * Merges `this` and `that`.
      */
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
          this.varSyms ++ that.varSyms,
          this.errors #::: that.errors
        )
      }

    /**
      * Adds the given redundancy error `e` to `this` object.
      */
    def +(e: RedundancyError): Used = copy(errors = e #:: errors)

    /**
      * Adds the given traversable of redundancy errors `es` to `this` object.
      */
    def ++(es: Traversable[RedundancyError]): Used = copy(errors = es.toStream #::: errors)

    /**
      * Marks the given variable symbol `sym` as used.
      */
    def -(sym: Symbol.VarSym): Used = copy(varSyms = varSyms - sym)

    /**
      * Marks all the given variable symbols `syms` as used.
      */
    def --(syms: Traversable[Symbol.VarSym]): Used = copy(varSyms = varSyms -- syms)

    /**
      * Returns Successful(a) unless `this` contains errors.
      */
    def toValidation[A](a: A): Validation[A, RedundancyError] = if (errors.isEmpty) Success(a) else Failure(errors)
  }

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


  // TODO: Check unused type parameters in enums, relations, and lattices.

  // TODO: What counts as a use of an enum? Is it enough to (a) mention its type, (b) to use it in a pat match, or (c) to actually construct a value.
  // TODO: The pattern matching is difficult, because you could have a default match onsomething just of that type.
  // TODO: What about the void enum? How would you deal with that? What about the singleton. The above choices start to seem more false in the presence of those.
  // TODO: Add appropriate cases once a definition has been decided.
  // And if you just require it to be used in a type, should that type then be used?

  // TODO: Type Parameters: When is a type parameter considered to be used?

  // TODO: Allow wildcards everywhere.
  // TODO: Add tests for wildcards.

  // TODO: In the case of predicates, do we want to assert that there are some facts? What about in the presence of
  // first-class constraints?

  // TODO: Does new channel have a side-effect or not? Is it allowed to be discarded?
  // TODO: How do we want to deal with expressions that have side-effects and a return value that must be used.
  // E.g. like the above.

  // TODO: Add more tests for useless expressions ones the effect system is implemented.

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


  // TODO: We could store an upper, lower bound, and equal. At least for integers.
  // TODO: For other types it would just be equal and not equal.
  // TODO: Might even be for entire expressions, like xs.length > 0, xs.length == 0, and not just variables.


  // TODO: Report an error if all arguments are known to a function call that is pure. but a function could be helping by constructing some large structure.
  // TODO: Maybe we need a measure on physical code size?


  // TODO: Code like f(x), and f(x) is redundant if both are pure... This is just common sub-expression elimination.

  // TODO: Parallel execution

  // TODO: Define a notion of contradiction:
  // P(x) and Q(x) cannot be true at the same time.
  // E.g. x == 0 and x == 1, or isEmpty(xs) and nonEmpty(xs).
  // But isEmpty(r.l) and nonEmpty(r.l) cannot be true at the same time.
  // TODO: Question is how to deal with the grammar. And how to represent these things.
  // TODO: How to deal with conjunctions and disjunctions?

  // TODO: Do we want ??? to consume all arguments together with a hole perhaps??

  // TODO: Should we be allowed to *use* _score names? Probably not. How to enforce?

  // TODO: Treat ??? and ?hole as using all local variables.

  // TODO: Is validation really the right approach here?

  // Notes for the paper:
  // - We disallow shadowing (because its confusing in the presence of pattern matching).
  // - We disallow both implicit widening and narrowing of integers.
  // - We disallow all forms of implicit coercions.
  // - We disallow linear patterns.
  // - We treat holes (and ???) as using all local variables (but not anything else?)

  // Bugs found:
  // - Missing @test on def testArrayLength42(): Int = let x = [[1 :: Nil], [3 :: 4 :: 5 :: Nil]]; length[x]

  // Shadowing gone wrong:
  //     case Expression.FixpointProject(pred, exp, _, _, _) =>
  //      val PredicateWithParam(sym, exp) = pred
  //      mapN(visitExp(pred.exp, env0), visitExp(exp, env0)) {
  //        case (used1, used2) => Used.of(sym) ++ used1 ++ used2
  //      }

  // Count impacted test cases?

}
