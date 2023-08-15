/*
 * Copyright 2017 Jason Mittertreiner
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, ParYieldFragment, Pattern, Root}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.errors.NonExhaustiveMatchError
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * The Pattern Exhaustiveness phase checks pattern matches for exhaustiveness
  *
  * A pattern match is exhaustive if there exists a type correct expression which is not matched by any of the patterns
  * in the pattern match
  *
  * Implemented as described in MARANGET, L. (2007). Warnings for pattern matching. Journal of Functional Programming, 17(3), 387-421. doi:10.1017/S0956796807006223
  *
  * The algorithm works by recursively "peeling off layers" of the pattern match. It check if the outermost type
  * constructors are exhaustive, then if so, recursively calls itself on the arguments. If it finds a pattern which
  * is not exhaustive during a recursive case, it uses it to built up a pattern that is not matched by the full
  * pattern match and returns the result.
  *
  */
object PatternExhaustiveness {

  /**
    * An ADT to make matching Type Constructors easier. We need to
    * support both user created constructors and the implicit built in
    * types. This allows us to handle True, False, Tuples etc in the same
    * way we would a user defined enum.
    */
  private sealed trait TyCon

  private object TyCon {

    case object Unit extends TyCon

    case object True extends TyCon

    case object False extends TyCon

    case object Char extends TyCon

    case object BigDecimal extends TyCon

    case object BigInt extends TyCon

    case object Int8 extends TyCon

    case object Int16 extends TyCon

    case object Int32 extends TyCon

    case object Int64 extends TyCon

    case object Float32 extends TyCon

    case object Float64 extends TyCon

    case object Str extends TyCon

    case object Regex extends TyCon

    case object Wild extends TyCon

    case class Tuple(args: List[TyCon]) extends TyCon

    case object Array extends TyCon

    case object Vector extends TyCon

    case class Enum(name: String, sym: EnumSym, numArgs: Int, args: List[TyCon]) extends TyCon

  }

  /**
    * A small ADT to track if we've found a non exhaustive pattern
    *
    * Essentially a reverse Optional: we only include a parameter if there is an error
    */
  private sealed trait Exhaustiveness

  private case object Exhaustive extends Exhaustiveness

  private case class NonExhaustive(pat: List[TyCon]) extends Exhaustiveness

  /**
    * Returns an error message if a pattern match is not exhaustive
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[Root, NonExhaustiveMatchError] =
    flix.phase("PatternExhaustiveness") {
      val defErrs = root.defs.values.flatMap(defn => visitImpl(defn.impl, root))
      val instanceDefErrs = TypedAstOps.instanceDefsOf(root).flatMap(defn => visitImpl(defn.impl, root))
      // Only need to check sigs with implementations
      val sigsErrs = root.sigs.values.flatMap(_.impl).flatMap(visitImpl(_, root))

      (defErrs ++ instanceDefErrs ++ sigsErrs).toList match {
        case Nil => Validation.Success(root)
        case errs => Validation.SoftFailure(root, LazyList.from(errs))
      }
    }

  /**
    * Check that all patterns in an implementation are exhaustive
    *
    * @param impl The implementation to check
    * @param root The AST root
    */
  private def visitImpl(impl: TypedAst.Impl, root: TypedAst.Root)(implicit flix: Flix): List[NonExhaustiveMatchError] = {
    visitExp(impl.exp, root)
  }

  /**
    * Check that all patterns in an expression are exhaustive
    *
    * @param tast The expression to check
    * @param root The AST root
    */
  private def visitExp(tast: TypedAst.Expr, root: TypedAst.Root)(implicit flix: Flix): List[NonExhaustiveMatchError] = {
    tast match {
      case Expr.Var(_, _, _) => Nil
      case Expr.Def(_, _, _) => Nil
      case Expr.Sig(_, _, _) => Nil
      case Expr.Hole(_, _, _) => Nil
      case Expr.HoleWithExp(exp, _, _, _) => visitExp(exp, root)
      case Expr.OpenAs(_, exp, _, _) => visitExp(exp, root)
      case Expr.Use(_, _, exp, _) => visitExp(exp, root)
      case Expr.Cst(_, _, _) => Nil
      case Expr.Lambda(_, body, _, _) => visitExp(body, root)
      case Expr.Apply(exp, exps, _, _, _) => (exp :: exps).flatMap(visitExp(_, root))
      case Expr.Unary(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.Binary(_, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.Let(_, _, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.LetRec(_, _, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.Region(_, _) => Nil
      case Expr.Scope(_, _, exp, _, _, _) => visitExp(exp, root)
      case Expr.ScopeExit(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => List(exp1, exp2, exp3).flatMap(visitExp(_, root))
      case Expr.Stm(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.Discard(exp, _, _) => visitExp(exp, root)

      case Expr.Match(exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        val guards = rules.flatMap(_.guard)
        val expsErrs = (exp :: ruleExps ::: guards).flatMap(visitExp(_, root))
        val rulesErrs = checkRules(exp, rules, root)
        expsErrs ::: rulesErrs

      case Expr.TypeMatch(exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        val expsErrs = (exp :: ruleExps).flatMap(visitExp(_, root))
        expsErrs

      case Expr.RelationalChoose(exps, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        (exps ::: ruleExps).flatMap(visitExp(_, root))

      case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        (exp :: ruleExps).flatMap(visitExp(_, root))

      case Expr.Tag(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.RestrictableTag(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.Tuple(elms, _, _, _) => elms.flatMap(visitExp(_, root))
      case Expr.RecordEmpty(_, _) => Nil
      case Expr.RecordSelect(base, _, _, _, _) => visitExp(base, root)
      case Expr.RecordExtend(_, value, rest, _, _, _) => List(value, rest).flatMap(visitExp(_, root))
      case Expr.RecordRestrict(_, rest, _, _, _) => visitExp(rest, root)
      case Expr.ArrayLit(exps, exp, _, _, _) => (exp :: exps).flatMap(visitExp(_, root))
      case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) => List(exp1, exp2, exp3).flatMap(visitExp(_, root))
      case Expr.ArrayLoad(base, index, _, _, _) => List(base, index).flatMap(visitExp(_, root))
      case Expr.ArrayStore(base, index, elm, _, _) => List(base, index, elm).flatMap(visitExp(_, root))
      case Expr.ArrayLength(base, _, _) => visitExp(base, root)
      case Expr.VectorLit(exps, _, _, _) => exps.flatMap(visitExp(_, root))
      case Expr.VectorLoad(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.VectorLength(exp, _) => visitExp(exp, root)
      case Expr.Ref(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.Deref(exp, _, _, _) => visitExp(exp, root)
      case Expr.Assign(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.Ascribe(exp, _, _, _) => visitExp(exp, root)
      case Expr.InstanceOf(exp, _, _) => visitExp(exp, root)
      case Expr.CheckedCast(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.UncheckedCast(exp, _, _, _, _, _) => visitExp(exp, root)
      case Expr.UncheckedMaskingCast(exp, _, _, _) => visitExp(exp, root)
      case Expr.Without(exp, _, _, _, _) => visitExp(exp, root)

      case Expr.TryCatch(exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        (exp :: ruleExps).flatMap(visitExp(_, root))

      case Expr.TryWith(exp, _, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        (exp :: ruleExps).flatMap(visitExp(_, root))

      case Expr.Do(_, exps, _, _, _) => exps.flatMap(visitExp(_, root))
      case Expr.Resume(exp, _, _) => visitExp(exp, root)
      case Expr.InvokeConstructor(_, args, _, _, _) => args.flatMap(visitExp(_, root))
      case Expr.InvokeMethod(_, exp, args, _, _, _) => (exp :: args).flatMap(visitExp(_, root))
      case Expr.InvokeStaticMethod(_, args, _, _, _) => args.flatMap(visitExp(_, root))
      case Expr.GetField(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.PutField(_, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.GetStaticField(_, _, _, _) => Nil
      case Expr.PutStaticField(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.NewObject(_, _, _, _, methods, _) => methods.flatMap(m => visitExp(m.exp, root))
      case Expr.NewChannel(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.GetChannel(exp, _, _, _) => visitExp(exp, root)
      case Expr.PutChannel(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))

      case Expr.SelectChannel(rules, default, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        val chans = rules.map(_.chan)
        (ruleExps ::: chans ::: default.toList).flatMap(visitExp(_, root))

      case Expr.Spawn(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))

      case Expr.ParYield(frags, exp, _, _, loc) =>
        val fragsExps = frags.map(_.exp)
        val expsErrs = (exp :: fragsExps).flatMap(visitExp(_, root))
        val fragsErrs = checkFrags(frags, root, loc)
        expsErrs ::: fragsErrs

      case Expr.Lazy(exp, _, _) => visitExp(exp, root)
      case Expr.Force(exp, _, _, _) => visitExp(exp, root)
      case Expr.FixpointConstraintSet(cs, _, _, _) => cs.flatMap(visitConstraint(_, root))
      case Expr.FixpointLambda(_, exp, _, _, _, _) => visitExp(exp, root)
      case Expr.FixpointMerge(exp1, exp2, _, _, _, _) => List(exp1, exp2).flatMap(visitExp(_, root))
      case Expr.FixpointSolve(exp, _, _, _, _) => visitExp(exp, root)
      case Expr.FixpointFilter(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.FixpointInject(exp, _, _, _, _) => visitExp(exp, root)
      case Expr.FixpointProject(_, exp, _, _, _) => visitExp(exp, root)
      case Expr.Error(_, _, _) => Nil
    }
  }

  /**
    * Performs exhaustive checking on the given constraint `c`.
    */
  private def visitConstraint(c0: TypedAst.Constraint, root: TypedAst.Root)(implicit flix: Flix): List[NonExhaustiveMatchError] = c0 match {
    case TypedAst.Constraint(_, head0, body0, _) =>
      val headErrs = visitHeadPred(head0, root)
      val bodyErrs = body0.flatMap(visitBodyPred(_, root))
      headErrs ::: bodyErrs
  }

  private def visitHeadPred(h0: TypedAst.Predicate.Head, root: TypedAst.Root)(implicit flix: Flix): List[NonExhaustiveMatchError] = h0 match {
    case TypedAst.Predicate.Head.Atom(_, _, terms, _, _) => terms.flatMap(visitExp(_, root))
  }

  private def visitBodyPred(b0: TypedAst.Predicate.Body, root: TypedAst.Root)(implicit flix: Flix): List[NonExhaustiveMatchError] = b0 match {
    case TypedAst.Predicate.Body.Atom(_, _, _, _, _, _, _) => Nil
    case TypedAst.Predicate.Body.Guard(exp, _) => visitExp(exp, root)
    case TypedAst.Predicate.Body.Functional(_, exp, _) => visitExp(exp, root)
  }

  /**
    * Check that the given ParYield fragments are exhaustive for their corresponding expressions.
    *
    * @param frags The fragments to check
    * @param root  The root of the tree
    * @param loc   the source location of the ParYield expression.
    * @return
    */
  private def checkFrags(frags: List[ParYieldFragment], root: TypedAst.Root, loc: SourceLocation): List[NonExhaustiveMatchError] = {
    // Call findNonMatchingPat for each pattern individually
    frags.flatMap(f => findNonMatchingPat(List(List(f.pat)), 1, root) match {
      case Exhaustive => Nil
      case NonExhaustive(ctors) => NonExhaustiveMatchError(prettyPrintCtor(ctors.head), loc) :: Nil
    })
  }

  /**
    * Check that the given rules are exhaustive for the given expression
    *
    * @param root  The root of the tree
    * @param exp   The expression to check
    * @param rules The rules to check
    * @return
    */
  private def checkRules(exp: TypedAst.Expr, rules: List[TypedAst.MatchRule], root: TypedAst.Root): List[NonExhaustiveMatchError] = {
    // Filter down to the unguarded rules.
    // Guarded rules cannot contribute to exhaustiveness (the guard could be e.g. `false`)
    val unguardedRules = rules.filter(r => r.guard.isEmpty)
    findNonMatchingPat(unguardedRules.map(r => List(r.pat)), 1, root) match {
      case Exhaustive => Nil
      case NonExhaustive(ctors) => List(NonExhaustiveMatchError(prettyPrintCtor(ctors.head), exp.loc))
    }
  }

  /**
    * Given a list of patterns, computes a pattern vector of size n such
    * that p doesn't match any rule in rules
    *
    * @param rules The rules to check for exhaustion
    * @param n     The size of resulting pattern vector
    * @param root  The AST root of the expression
    * @return If no such pattern exists, returns Exhaustive, else returns NonExhaustive(a matching pattern)
    */
  private def findNonMatchingPat(rules: List[List[Pattern]], n: Int, root: TypedAst.Root): Exhaustiveness = {
    if (n == 0) {
      if (rules.isEmpty) {
        return NonExhaustive(List.empty[TyCon])
      }
      return Exhaustive
    }

    val sigma = rootCtors(rules)
    val missing = missingFromSig(sigma, root)
    if (missing.isEmpty && sigma.nonEmpty) {
      /* If the constructors are complete, then we check that the arguments to the constructors and the remaining
       * patterns are complete
       *
       * e.g. If we have
       * enum Option[a] {
       *    case Some(a),
       *    case None
       *  }
       *
       * case Some True =>
       * case Some False =>
       * case None =>
       *
       * {Some, None} is a complete signature, but the "Some" case is exhaustive only if the {True, False} case is
       *
       * exhaustive. So we create a "Specialized" matrix for "Some" with {True, False} as rows and check that.
       */
      val checkAll: List[Exhaustiveness] = sigma.map(c => {
        val res: Exhaustiveness = findNonMatchingPat(specialize(c, rules, root), countCtorArgs(c) + n - 1, root)
        res match {
          case Exhaustive => Exhaustive
          case NonExhaustive(ctors) => NonExhaustive(rebuildPattern(c, ctors))
        }
      })
      checkAll.foldRight(Exhaustive: Exhaustiveness)(mergeExhaustive)

    } else {
      /* If the constructors are not complete, then we will fall to the wild/default case. In that case, we need to
       * check for non matching patterns in the wild/default matrix.
       */
      findNonMatchingPat(defaultMatrix(rules), n - 1, root) match {
        case Exhaustive => Exhaustive
        case NonExhaustive(ctors) => sigma match {
          // If sigma is not empty, pick one of the missing constructors and return it
          case _ :: _ => NonExhaustive(rebuildPattern(missing.head, ctors))
          // Else, prepend a wildcard
          case Nil => NonExhaustive(rebuildPattern(TyCon.Wild, ctors))
        }
      }
    }
  }

  /**
    * Specialize a matrix of patterns for the Constructor ctor For a constructor of C(r1,...ra) and a matrix of width
    * n, we return a matrix of width n+a-1
    *
    * e.g. If we have
    * enum Option {
    * case Some a,
    * case Nothing
    * }
    *
    * And the pattern matrix
    *
    * case Some True =>
    * case Some False =>
    * case Nothing =>
    *
    * Specializing for Some gives use the matrix
    * True =>
    * False =>
    *
    * Where' we've taken a matrix had made of the arguments to the Some rows. Specializing for Nothing would give
    * an empty matrix as it has no arguments.
    *
    * @param ctor  The constructor to specialize for
    * @param rules The rules matrix to specialize
    * @param root  The ast root
    * @return The specialized matrix of rules
    */
  private def specialize(ctor: TyCon, rules: List[List[Pattern]], root: TypedAst.Root): List[List[TypedAst.Pattern]] = {
    // First figure out how many arguments are needed by the ctor
    val numArgs = countCtorArgs(ctor)

    def specializeRow(pat: List[TypedAst.Pattern], acc: List[List[TypedAst.Pattern]]) = pat.head match {
      // A wild constructor is the same as the constructor
      // with all its arguments as wild
      case a: TypedAst.Pattern.Wild =>
        (List.fill(numArgs)(a) ::: pat.tail) :: acc
      case a: TypedAst.Pattern.Var =>
        (List.fill(numArgs)(a) ::: pat.tail) :: acc

      // If it's a pattern with the constructor that we are
      // specializing for, we break it up into it's arguments
      // If it's not our constructor, we ignore it
      case TypedAst.Pattern.Tag(Ast.CaseSymUse(sym, _), exp, _, _) =>
        ctor match {
          case TyCon.Enum(name, _, _, _) =>
            if (sym.name == name) {
              exp match {
                // The expression varies depending on how many arguments it has, 0 arguments => unit, non zero
                // => Tuple. If there are arguments, we add them to the matrix
                case TypedAst.Pattern.Tuple(elms, _, _) =>
                  (elms ::: pat.tail) :: acc
                case TypedAst.Pattern.Cst(Ast.Constant.Unit, _, _) =>
                  pat.tail :: acc
                case _ =>
                  (exp :: pat.tail) :: acc
              }
            } else {
              acc
            }
          case _ => acc
        }
      case TypedAst.Pattern.Tuple(elms, _, _) =>
        if (ctor.isInstanceOf[TyCon.Tuple]) {
          (elms ::: pat.tail) :: acc
        } else {
          acc
        }
      // Also handle the non tag constructors
      case p =>
        if (patToCtor(p) == ctor) {
          (p :: pat.tail) :: acc
        } else acc
    }

    rules.foldRight(List.empty[List[TypedAst.Pattern]])(specializeRow)
  }

  /**
    * Extract a default matrix of width n-1
    *
    * defaultMatrix is called by findNonMatchingPat when the given constructor patterns aren't exhaustive.
    * We then want to ensure that the wild card patterns cover the remaining cases.
    *
    * DefaultMatrix constructs a matrix which only has the patterns that begin with a wildcard, then removes the
    * first wildcard since we know all the patterns start with it.
    *
    * e.g.
    * case Red =>
    * case Blue =>
    * case _ =>
    *
    * The default matrix is
    * _ =>
    *
    */
  private def defaultMatrix(rules: List[List[TypedAst.Pattern]]): List[List[TypedAst.Pattern]] = {
    val defaultRow = (pat: List[TypedAst.Pattern], acc: List[List[TypedAst.Pattern]]) => pat.head match {
      // If it's a wild card, we take the rest of the pattern
      case _: TypedAst.Pattern.Wild => pat.tail :: acc
      case _: TypedAst.Pattern.Var => pat.tail :: acc

      // If it's a constructor, we don't include a row
      case _ => acc
    }
    rules.foldRight(List.empty[List[TypedAst.Pattern]])(defaultRow)
  }


  /**
    * Computes the set of constructors that appear at the root of the
    * patterns, skipping out on wilds
    *
    * e.g.
    *
    * Foo(1, 3) => ....
    * Bar(Foo(1,3), 'a', 3) => ....
    * Baz() => ....
    * _ => ...
    *
    * returns
    * Foo
    * Bar
    * Baz
    *
    */
  private def rootCtors(rules: List[List[TypedAst.Pattern]]): List[TyCon] = {
    def rootCtor(pat: List[TypedAst.Pattern], pats: List[TypedAst.Pattern]) = pat.head match {
      case _: Pattern.Wild => pats
      case _: Pattern.Var => pats
      case tg: Pattern.Tag => tg :: pats
      case p => p :: pats
    }

    rules.foldRight(List.empty[TypedAst.Pattern])(rootCtor).map(patToCtor)
  }

  /**
    * True if ctors is a complete signature for exp. A complete signature is when all constructors of a type are
    * present. E.g. for
    *
    * enum Color {
    * case Red,
    * case Blue,
    * case Yellow
    * }
    *
    * {Red, Blue, Yellow} is a complete signature, but {Red, Blue} is not. Additionally, {Red, Blue, _} is also not
    * If the constructors are a complete signature, then they are exhaustive for the type, and we just have to
    * check that their arguments are also exhaustive
    *
    * Wildcards are exhaustive, but we need to do some additional checking in that case (@see DefaultMatrix)
    *
    * @param ctors The ctors that we match with
    * @param root  Root of the expression tree
    * @return
    */
  private def missingFromSig(ctors: List[TyCon], root: TypedAst.Root): List[TyCon] = {
    // Enumerate all the constructors that we need to cover
    def getAllCtors(x: TyCon, xs: List[TyCon]) = x match {
      // For built in constructors, we can add all the options since we know them a priori
      case TyCon.Unit => TyCon.Unit :: xs
      case TyCon.True => TyCon.True :: TyCon.False :: xs
      case TyCon.False => TyCon.True :: TyCon.False :: xs
      case a: TyCon.Tuple => a :: xs

      // For Enums, we have to figure out what base enum is, then look it up in the enum definitions to get the
      // other enums
      case TyCon.Enum(_, sym, _, _) => {
        root.enums.get(sym).get.cases.map(x => TyCon.Enum(x._1.name, sym, countTypeArgs(x._2.tpe), List.empty[TyCon]))
      }.toList ::: xs

      /* For numeric types, we consider them as "infinite" types union
       * Int = ...| -1 | 0 | 1 | 2 | 3 | ...
       * The only way we get a match is through a wild. Technically, you could, for example, cover a Char by
       * having a case for [0 255], but we'll ignore that case for now
       */
      case _ => TyCon.Wild :: xs
    }

    val expCtors = ctors.foldRight(List.empty[TyCon])(getAllCtors)
    /* We cover the needed constructors if there is a wild card in the
     * root constructor set, or if we match every constructor for the
     * expression
     */
    expCtors.foldRight(List.empty[TyCon])((x, xs) => if (ctors.exists(y => sameCtor(x, y))) xs else x :: xs)
  }

  /**
    * Gets the number of arguments for a constructor, takes into account the "fake constructors"
    *
    * @param ctor The constructor to get from
    * @return The number of arguments for the constructor
    */
  private def countCtorArgs(ctor: TyCon): Int = ctor match {
    case TyCon.Unit => 0
    case TyCon.True => 0
    case TyCon.False => 0
    case TyCon.Char => 0
    case TyCon.BigDecimal => 0
    case TyCon.BigInt => 0
    case TyCon.Int8 => 0
    case TyCon.Int16 => 0
    case TyCon.Int32 => 0
    case TyCon.Int64 => 0
    case TyCon.Float32 => 0
    case TyCon.Float64 => 0
    case TyCon.Str => 0
    case TyCon.Regex => 0
    case TyCon.Wild => 0
    case TyCon.Tuple(args) => args.size
    case TyCon.Array => 0
    case TyCon.Vector => 0
    case TyCon.Enum(_, _, numArgs, _) => numArgs
  }

  /**
    * @param tpe the type to count
    * @return the number of arguments a type constructor expects
    */
  // TODO: Maybe we can use the kind instead?
  private def countTypeArgs(tpe: Type): Int = tpe.typeConstructor match {
    case None => 0
    case Some(TypeConstructor.Unit) => 0
    case Some(TypeConstructor.Bool) => 0
    case Some(TypeConstructor.Char) => 0
    case Some(TypeConstructor.Float32) => 0
    case Some(TypeConstructor.Float64) => 0
    case Some(TypeConstructor.BigDecimal) => 0
    case Some(TypeConstructor.Int8) => 0
    case Some(TypeConstructor.Int16) => 0
    case Some(TypeConstructor.Int32) => 0
    case Some(TypeConstructor.Int64) => 0
    case Some(TypeConstructor.BigInt) => 0
    case Some(TypeConstructor.Str) => 0
    case Some(TypeConstructor.Regex) => 0
    case Some(TypeConstructor.Relation) => 0
    case Some(TypeConstructor.Lattice) => 0
    case Some(TypeConstructor.RecordRowEmpty) => 0
    case Some(TypeConstructor.SchemaRowEmpty) => 0
    case Some(TypeConstructor.Record) => 0
    case Some(TypeConstructor.Schema) => 0
    case Some(TypeConstructor.Arrow(length)) => length
    case Some(TypeConstructor.Array) => 1
    case Some(TypeConstructor.Vector) => 1
    case Some(TypeConstructor.Ref) => 0
    case Some(TypeConstructor.Lazy) => 1
    case Some(TypeConstructor.Enum(sym, kind)) => 0 // TODO: Correct?
    case Some(TypeConstructor.Native(clazz)) => 0
    case Some(TypeConstructor.Tuple(l)) => l
    case Some(TypeConstructor.RecordRowExtend(_)) => 2
    case Some(TypeConstructor.SchemaRowExtend(_)) => 2
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
  }

  /**
    * Pretty print a constructor
    *
    * @param ctor The TypeConstructor to print
    * @return A human readable string of the constructor
    */
  private def prettyPrintCtor(ctor: TyCon): String = ctor match {
    case TyCon.Unit => "Unit"
    case TyCon.True => "True"
    case TyCon.False => "False"
    case TyCon.Char => "Char"
    case TyCon.BigDecimal => "BigDecimal"
    case TyCon.BigInt => "BigInt"
    case TyCon.Int8 => "Int8"
    case TyCon.Int16 => "Int16"
    case TyCon.Int32 => "Int32"
    case TyCon.Int64 => "Int64"
    case TyCon.Float32 => "Float32"
    case TyCon.Float64 => "Float64"
    case TyCon.Str => "Str"
    case TyCon.Regex => "Regex"
    case TyCon.Wild => "_"
    case TyCon.Tuple(args) => "(" + args.foldRight("")((x, xs) => if (xs == "") prettyPrintCtor(x) + xs else prettyPrintCtor(x) + ", " + xs) + ")"
    case TyCon.Array => "Array"
    case TyCon.Vector => "Vector"
    case TyCon.Enum(name, _, num_args, args) => if (num_args == 0) name else name + prettyPrintCtor(TyCon.Tuple(args))
  }


  /**
    * Checks if two TypeConstructors refers to the same constructor.
    *
    * @param c1 First constructor to compare
    * @param c2 Second constructor to compare
    * @return True if they are the same constructor
    */
  private def sameCtor(c1: TyCon, c2: TyCon): Boolean = (c1, c2) match {
    // Two enums are the same constructor if they have the same name and enum sym
    case (TyCon.Enum(n1, s1, _, _), TyCon.Enum(n2, s2, _, _)) => n1 == n2 && s1 == s2
    // Everything else is the same constructor if they are the same type
    case (a: TyCon.Tuple, b: TyCon.Tuple) => true
    case (a, b) => a == b;
  }

  /**
    * Convert a pattern to a TypeConstructor
    *
    * @param pattern The pattern to convert
    * @return a TypeConstructor representing the given pattern
    */
  private def patToCtor(pattern: TypedAst.Pattern): TyCon = pattern match {
    case Pattern.Wild(_, _) => TyCon.Wild
    case Pattern.Var(_, _, _) => TyCon.Wild
    case Pattern.Cst(Ast.Constant.Unit, _, _) => TyCon.Unit
    case Pattern.Cst(Ast.Constant.Bool(true), _, _) => TyCon.True
    case Pattern.Cst(Ast.Constant.Bool(false), _, _) => TyCon.False
    case Pattern.Cst(Ast.Constant.Char(_), _, _) => TyCon.Char
    case Pattern.Cst(Ast.Constant.Float32(_), _, _) => TyCon.Float32
    case Pattern.Cst(Ast.Constant.Float64(_), _, _) => TyCon.Float64
    case Pattern.Cst(Ast.Constant.BigDecimal(_), _, _) => TyCon.BigDecimal
    case Pattern.Cst(Ast.Constant.Int8(_), _, _) => TyCon.Int8
    case Pattern.Cst(Ast.Constant.Int16(_), _, _) => TyCon.Int16
    case Pattern.Cst(Ast.Constant.Int32(_), _, _) => TyCon.Int32
    case Pattern.Cst(Ast.Constant.Int64(_), _, _) => TyCon.Int64
    case Pattern.Cst(Ast.Constant.BigInt(_), _, _) => TyCon.BigInt
    case Pattern.Cst(Ast.Constant.Str(_), _, _) => TyCon.Str
    case Pattern.Cst(Ast.Constant.Regex(_), _, _) => throw InternalCompilerException("unexpected regex pattern", pattern.loc)
    case Pattern.Cst(Ast.Constant.Null, _, _) => throw InternalCompilerException("unexpected null pattern", pattern.loc)
    case Pattern.Tag(Ast.CaseSymUse(sym, _), pat, _, _) => {
      val (args, numArgs) = pat match {
        case Pattern.Cst(Ast.Constant.Unit, _, _) => (List.empty[TyCon], 0)
        case Pattern.Tuple(elms, _, _) => (elms.map(patToCtor), elms.length)
        case a => (List(patToCtor(a)), 1)
      }
      TyCon.Enum(sym.name, sym.enumSym, numArgs, args)
    }
    case Pattern.Tuple(elms, _, _) => TyCon.Tuple(elms.map(patToCtor))
  }

  /**
    * Adds a TypeConstructor to a list of TypeConstructors, using up items in the list if it requires arguments
    *
    * e.g. rebuildPatter(Foo(a,b), [1,2,3]) => [Foo(1,2), 3]
    *
    * @param tc  The type constructor to add
    * @param lst The list to add to
    * @return The new list
    */
  private def rebuildPattern(tc: TyCon, lst: List[TyCon]): List[TyCon] = tc match {
    case TyCon.Tuple(args) => TyCon.Tuple(lst.take(args.size)) :: lst.drop(args.size)
    case TyCon.Enum(name, sym, numArgs, _) => TyCon.Enum(name, sym, numArgs,
      if (numArgs > lst.size) {
        lst.take(lst.size) ::: List.fill(numArgs)(TyCon.Wild)
      } else {
        lst.take(numArgs)
      }) :: lst.drop(numArgs)
    case a => a :: lst
  }

  /**
    * Flatten/join for Exhaust. Accumulates failures if they are there.
    *
    * @param x   The Exhaustiveness under consideration
    * @param acc The Exhaustiveness accumulated so far
    * @return The merged result
    */
  private def mergeExhaustive(x: Exhaustiveness, acc: Exhaustiveness): Exhaustiveness =
    (x, acc) match {
      case (Exhaustive, Exhaustive) => Exhaustive
      case (a: NonExhaustive, _) => a
      case (_, a: NonExhaustive) => a
    }

}
