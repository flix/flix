/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Level, Name, Scheme, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.DerivationError
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Validation.mapN
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
  * Constructs instances derived from enums.
  *
  * No errors are thrown in this phase: constructed instances must be well-formed.
  * Errors with overlapping instances or unfulfilled type constraints must be caught in later phases.
  */
object Deriver {

  // We don't use regions here, so we can safely put every variable in the default level
  private implicit val DefaultLevel: Level = Level.Default

  def run(root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, DerivationError] = flix.phase("Deriver") {
    val derivedInstances = ParOps.parTraverse(root.enums.values)(getDerivedInstances(_, root))

    mapN(derivedInstances) {
      instances =>
        val newInstances = instances.flatten.foldLeft(root.instances) {
          case (acc, inst) =>
            val accInsts = acc.getOrElse(inst.clazz.sym, Nil)
            acc + (inst.clazz.sym -> (inst :: accInsts))
        }
        root.copy(instances = newInstances)
    }
  }

  /**
    * Builds the instances derived from this enum.
    */
  private def getDerivedInstances(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): Validation[List[KindedAst.Instance], DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, enumSym, _, derives, cases, _, _) =>
      // lazy so that in we don't try a lookup if there are no derivations (important for Nix Lib)
      lazy val eqSym = PredefinedClasses.lookupClassSym("Eq", root)
      lazy val orderSym = PredefinedClasses.lookupClassSym("Order", root)
      lazy val toStringSym = PredefinedClasses.lookupClassSym("ToString", root)
      lazy val hashSym = PredefinedClasses.lookupClassSym("Hash", root)
      lazy val sendableSym = PredefinedClasses.lookupClassSym("Sendable", root)
      Validation.traverse(derives.classes) {
        case Ast.Derivation(classSym, loc) if cases.isEmpty => Validation.toSoftFailure(None, DerivationError.IllegalDerivationForEmptyEnum(enumSym, classSym, loc))
        case Ast.Derivation(sym, loc) if sym == eqSym => mkEqInstance(enum0, loc, root).map(Some.apply)
        case Ast.Derivation(sym, loc) if sym == orderSym => mkOrderInstance(enum0, loc, root).map(Some.apply)
        case Ast.Derivation(sym, loc) if sym == toStringSym => mkToStringInstance(enum0, loc, root).map(Some.apply)
        case Ast.Derivation(sym, loc) if sym == hashSym => mkHashInstance(enum0, loc, root).map(Some.apply)
        case Ast.Derivation(sym, loc) if sym == sendableSym => mkSendableInstance(enum0, loc, root).map(Some.apply)
        case Ast.Derivation(sym, loc) => Validation.toSoftFailure(None, DerivationError.IllegalDerivation(sym, Resolver.DerivableSyms, loc))
      }.map(_.flatten)
  }

  /**
    * Creates an Eq instance for the given enum.
    *
    * {{{
    * enum E[a] with Eq {
    *   case C1
    *   case C2(a)
    *   case C3(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Eq[E[a]] with Eq[a] {
    *   pub def eq(x: E[a], y: E[a]): Bool = (x, y) match {
    *     case (C0, C0) => true
    *     case (C1(x0), C1(y0)) => x0 == y0
    *     case (C2(x0, x1), C2(y0, y1)) => x0 == y0 and x1 == y1
    *     case _ => false
    *   }
    * }
    * }}}
    */
  private def mkEqInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val eqClassSym = PredefinedClasses.lookupClassSym("Eq", root)
      val eqDefSym = Symbol.mkDefnSym("Eq.eq", Some(flix.genSym.freshId()))

      val param1 = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val param2 = Symbol.freshVarSym("y", BoundBy.FormalParam, loc)
      val exp = mkEqImpl(enum0, param1, param2, loc, root)
      val spec = mkEqSpec(enum0, param1, param2, loc, root)

      val defn = KindedAst.Def(eqDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, eqClassSym, loc)

      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        clazz = Ast.ClassSymUse(eqClassSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        assocs = Nil,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the eq implementation for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkEqImpl(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      // create a match rule for each case
      val mainMatchRules = getCasesInStableOrder(cases).map(mkEqMatchRule(_, loc, root))

      // create a default rule
      // `case _ => false`
      val defaultRule = KindedAst.MatchRule(KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc), loc), None, KindedAst.Expr.Cst(Ast.Constant.Bool(false), loc))

      // group the match rules in an expression
      KindedAst.Expr.Match(
        KindedAst.Expr.Tuple(List(mkVarExpr(param1, loc), mkVarExpr(param2, loc)), loc),
        (mainMatchRules ++ List(defaultRule)),
        loc
      )
  }

  /**
    * Creates the eq spec for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkEqSpec(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val eqClassSym = PredefinedClasses.lookupClassSym("Eq", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(
          KindedAst.FormalParam(param1, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc),
          KindedAst.FormalParam(param2, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        ),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(eqClassSym, loc), tpe, loc)),
          Nil,
          Type.mkPureUncurriedArrow(List(tpe, tpe), Type.mkBool(loc), loc)
        ),
        tpe = Type.mkBool(loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TypeConstraint(Ast.TypeConstraint.Head(eqClassSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates an Eq match rule for the given enum case.
    */
  private def mkEqMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val eqSym = PredefinedClasses.lookupSigSym("Eq", "eq", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat1, varSyms1) = mkPattern(sym, tpe, "x", loc)
      val (pat2, varSyms2) = mkPattern(sym, tpe, "y", loc)
      val pat = KindedAst.Pattern.Tuple(List(pat1, pat2), loc)

      // call eq on each variable pair
      // `x0 == y0`, `x1 == y1`
      val eqs = varSyms1.zip(varSyms2).map {
        case (varSym1, varSym2) =>
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(eqSym, Type.freshVar(Kind.Star, loc), loc),
            List(
              mkVarExpr(varSym1, loc),
              mkVarExpr(varSym2, loc)
            ),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      // put it all together
      // `x0 == y0 and x1 == y1`
      val exp = eqs match {
        // Case 1: no arguments: return true
        case Nil => KindedAst.Expr.Cst(Ast.Constant.Bool(true), loc)
        // Case 2: at least one argument: join everything with `and`
        case head :: tail => tail.foldLeft(head: KindedAst.Expr) {
          case (acc, eq) => KindedAst.Expr.Binary(SemanticOp.BoolOp.And, acc, eq, Type.freshVar(Kind.Star, loc), loc)
        }
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates an Order instance for the given enum.
    *
    * {{{
    * enum E[a] with Order {
    *   case C1
    *   case C2(a)
    *   case C3(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Order[E[a]] with Order[a] {
    *   pub def compare(x: E[a], y: E[a]): Comparison = {
    *     let indexOf = e -> match e {
    *       case C0(_) -> 0
    *       case C1(_) -> 1
    *       case C2(_) -> 2
    *     };
    *     match (x, y) {
    *       case (C0, C0) => Comparison.EqualTo
    *       case (C1(x0), C1(y0)) => Order.compare(x0, y0)
    *       case (C2(x0, x1), C2(y0, y1)) => Order.compare(x0, y0) `Order.thenCompare` lazy Order.compare(x1, y1)
    *       case _ => Order.compare(indexOf(x), indexOf(y))
    *     }
    *   }
    * }
    * }}}
    */
  private def mkOrderInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val orderClassSym = PredefinedClasses.lookupClassSym("Order", root)
      val compareDefSym = Symbol.mkDefnSym("Order.compare", Some(flix.genSym.freshId()))

      val param1 = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val param2 = Symbol.freshVarSym("y", BoundBy.FormalParam, loc)
      val exp = mkCompareImpl(enum0, param1, param2, loc, root)
      val spec = mkCompareSpec(enum0, param1, param2, loc, root)

      val defn = KindedAst.Def(compareDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, orderClassSym, loc)
      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        clazz = Ast.ClassSymUse(orderClassSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        assocs = Nil,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the compare implementation for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkCompareImpl(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      val compareSigSym = PredefinedClasses.lookupSigSym("Order", "compare", root)

      val lambdaVarSym = Symbol.freshVarSym("indexOf", BoundBy.Let, loc)

      // Create the lambda mapping tags to indices
      val lambdaParamVarSym = Symbol.freshVarSym("e", BoundBy.FormalParam, loc)
      val indexMatchRules = getCasesInStableOrder(cases).zipWithIndex.map { case (caze, index) => mkCompareIndexMatchRule(caze, index, loc) }
      val indexMatchExp = KindedAst.Expr.Match(mkVarExpr(lambdaParamVarSym, loc), indexMatchRules, loc)
      val lambda = KindedAst.Expr.Lambda(
        KindedAst.FormalParam(lambdaParamVarSym, Ast.Modifiers.Empty, lambdaParamVarSym.tvar, Ast.TypeSource.Ascribed, loc),
        indexMatchExp,
        loc
      )

      // Create the main match expression
      val matchRules = getCasesInStableOrder(cases).map(mkComparePairMatchRule(_, loc, root))

      // Create the default rule:
      // `case _ => compare(indexOf(x), indexOf(y))`
      val defaultMatchRule = KindedAst.MatchRule(
        KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc), loc),
        None,
        KindedAst.Expr.Apply(
          KindedAst.Expr.Sig(compareSigSym, Type.freshVar(Kind.Star, loc), loc),
          List(
            KindedAst.Expr.Apply(
              mkVarExpr(lambdaVarSym, loc),
              List(mkVarExpr(param1, loc)),
              Type.freshVar(Kind.Star, loc),
              Type.freshVar(Kind.Eff, loc),
              loc
            ),
            KindedAst.Expr.Apply(
              mkVarExpr(lambdaVarSym, loc),
              List(mkVarExpr(param2, loc)),
              Type.freshVar(Kind.Star, loc),
              Type.freshVar(Kind.Eff, loc),
              loc),
          ),
          Type.freshVar(Kind.Star, loc),
          Type.freshVar(Kind.Eff, loc),
          loc
        )
      )

      // Wrap the cases in a match expression
      val matchExp = KindedAst.Expr.Match(
        KindedAst.Expr.Tuple(List(mkVarExpr(param1, loc), mkVarExpr(param2, loc)), loc),
        matchRules.toList :+ defaultMatchRule,
        loc
      )

      // Put the expressions together in a let
      KindedAst.Expr.Let(lambdaVarSym, Ast.Modifiers.Empty, lambda, matchExp, loc)
  }

  /**
    * Creates the eq spec for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkCompareSpec(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val orderClassSym = PredefinedClasses.lookupClassSym("Order", root)
      val comparisonEnumSym = PredefinedClasses.lookupEnumSym("Comparison", root)

      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(
          KindedAst.FormalParam(param1, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc),
          KindedAst.FormalParam(param2, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        ),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(orderClassSym, loc), tpe, loc)),
          Nil,
          Type.mkPureUncurriedArrow(List(tpe, tpe), Type.mkEnum(comparisonEnumSym, Kind.Star, loc), loc)
        ),
        tpe = Type.mkEnum(comparisonEnumSym, Kind.Star, loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TypeConstraint(Ast.TypeConstraint.Head(orderClassSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates an indexing match rule, mapping the given case to the given index, e.g.
    * `case C2(_) => 2`
    */
  private def mkCompareIndexMatchRule(caze: KindedAst.Case, index: Int, loc: SourceLocation)(implicit Flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, _, _, _) =>
      val pat = KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc), loc), Type.freshVar(Kind.Star, loc), loc)
      val exp = KindedAst.Expr.Cst(Ast.Constant.Int32(index), loc)
      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates a comparison match rule, comparing the elements of two tags of the same type.
    * ```case (C2(x0, x1), C2(y0, y1)) => compare(x0, y0) `thenCompare` lazy(x1, y1)```
    */
  private def mkComparePairMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val equalToSym = PredefinedClasses.lookupCaseSym("Comparison", "EqualTo", root)
      val compareSigSym = PredefinedClasses.lookupSigSym("Order", "compare", root)
      val thenCompareDefSym = PredefinedClasses.lookupDefSym(List("Order"), "thenCompare", root)

      // Match on the tuple
      // `case (C2(x0, x1), C2(y0, y1))
      val (pat1, varSyms1) = mkPattern(sym, tpe, "x", loc)
      val (pat2, varSyms2) = mkPattern(sym, tpe, "y", loc)
      val pat = KindedAst.Pattern.Tuple(List(pat1, pat2), loc)

      // Call compare on each variable pair
      // `compare(x0, y0)`, `compare(x1, y1)`
      val compares = varSyms1.zip(varSyms2).map {
        case (varSym1, varSym2) =>
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(compareSigSym, Type.freshVar(Kind.Star, loc), loc),
            List(
              mkVarExpr(varSym1, loc),
              mkVarExpr(varSym2, loc)
            ),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      /**
        * Joins the two expressions via `Compare.thenCompare`, making the second expression lazy.
        * (Cannot be inlined due to issues with Scala's type inference.
        */
      def thenCompare(exp1: KindedAst.Expr, exp2: KindedAst.Expr): KindedAst.Expr = {
        KindedAst.Expr.Apply(
          KindedAst.Expr.Def(thenCompareDefSym, Type.freshVar(Kind.Star, loc), loc),
          List(
            exp1,
            KindedAst.Expr.Lazy(exp2, loc)
          ),
          Type.freshVar(Kind.Star, loc),
          Type.freshVar(Kind.Eff, loc),
          loc
        )
      }

      // Put it all together
      // ```compare(x0, y0) `thenCompare` lazy compare(x1, y1)```
      val exp = compares match {
        // Case 1: no variables to compare; just return true
        case Nil => KindedAst.Expr.Tag(Ast.CaseSymUse(equalToSym, loc), KindedAst.Expr.Cst(Ast.Constant.Unit, loc), Type.freshVar(Kind.Star, loc), loc)
        // Case 2: multiple comparisons to be done; wrap them in Order.thenCompare
        case cmps => cmps.reduceRight(thenCompare)
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates a ToString instance for the given enum.
    *
    * {{{
    * enum E[a] with ToString {
    *   case C0
    *   case C1(a)
    *   case C2(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance ToString[E[a]] with ToString[a] {
    *   pub def toString(x: E[a]): String = match x {
    *     case C0 => "C0"
    *     case C1(x0) => "C1" + "(" + ToString.toString(x0) + ")"
    *     case C2(x0, x1) => "C2" + "(" + ToString.toString(x0) + ", " + ToString.toString(x1) + ")"
    *   }
    * }
    * }}}
    */
  private def mkToStringInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val toStringClassSym = PredefinedClasses.lookupClassSym("ToString", root)
      val toStringDefSym = Symbol.mkDefnSym("ToString.toString", Some(flix.genSym.freshId()))

      val param = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val exp = mkToStringImpl(enum0, param, loc, root)
      val spec = mkToStringSpec(enum0, param, loc, root)

      val defn = KindedAst.Def(toStringDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, toStringClassSym, loc)

      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        clazz = Ast.ClassSymUse(toStringClassSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        assocs = Nil,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the toString implementation for the given enum, where `param` is the parameter to the function.
    */
  private def mkToStringImpl(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      // create a match rule for each case
      val matchRules = getCasesInStableOrder(cases).map(mkToStringMatchRule(_, loc, root))

      // group the match rules in an expression
      KindedAst.Expr.Match(
        mkVarExpr(param, loc),
        matchRules.toList,
        loc
      )
  }

  /**
    * Creates the toString spec for the given enum, where `param` is the parameter to the function.
    */
  private def mkToStringSpec(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val toStringClassSym = PredefinedClasses.lookupClassSym("ToString", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(toStringClassSym, loc), tpe, loc)),
          Nil,
          Type.mkPureArrow(tpe, Type.mkString(loc), loc)
        ),
        tpe = Type.mkString(loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TypeConstraint(Ast.TypeConstraint.Head(toStringClassSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates a ToString match rule for the given enum case.
    */
  private def mkToStringMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val toStringSym = PredefinedClasses.lookupSigSym("ToString", "toString", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat, varSyms) = mkPattern(sym, tpe, "x", loc)

      // "C2"
      val tagPart = KindedAst.Expr.Cst(Ast.Constant.Str(sym.name), loc)

      // call toString on each variable,
      // `toString(x0)`, `toString(x1)`
      val toStrings = varSyms.map {
        varSym =>
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(toStringSym, Type.freshVar(Kind.Star, loc), loc),
            List(mkVarExpr(varSym, loc)),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      // put commas between the arguments
      // `toString(x0)`, `", "`, `toString(x1)`
      val sep = mkStrExpr(", ", loc)
      val valuePart = intersperse(toStrings, sep)

      // put it all together
      // `"C2" + "(" + toString(x0) + ", " + toString(x1) + ")"`
      val exp = valuePart match {
        // Case 1: no arguments: just show the tag
        case Nil => tagPart
        // Case 2: at least one argument: concatenate the tag with the values wrapped in parens
        case exps => concatAll(tagPart :: mkStrExpr("(", loc) :: (exps :+ mkStrExpr(")", loc)), loc)
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates a Hash instance for the given enum.
    *
    * {{{
    * enum E[a] with Hash {
    *   case C0
    *   case C1(a)
    *   case C2(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Hash[E[a]] with Hash[a] {
    *   pub def hash(x: E[a]): Int = match x {
    *     case C0 => 1
    *     case C1(x0) => 2 `combine` hash(x0)
    *     case C2(x0, x1) => 3 `combine` hash(x0) `combine` hash(x1)
    *   }
    * }
    * }}}
    */
  private def mkHashInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val hashClassSym = PredefinedClasses.lookupClassSym("Hash", root)
      val hashDefSym = Symbol.mkDefnSym("Hash.hash", Some(flix.genSym.freshId()))

      val param = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val exp = mkHashImpl(enum0, param, loc, root)
      val spec = mkHashSpec(enum0, param, loc, root)

      val defn = KindedAst.Def(hashDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, hashClassSym, loc)
      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        clazz = Ast.ClassSymUse(hashClassSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        defs = List(defn),
        assocs = Nil,
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the hash implementation for the given enum, where `param` is the parameter to the function.
    */
  private def mkHashImpl(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      // create a match rule for each case
      val matchRules = getCasesInStableOrder(cases).zipWithIndex.map {
        case (caze, index) => mkHashMatchRule(caze, index, loc, root)
      }

      // group the match rules in an expression
      KindedAst.Expr.Match(
        mkVarExpr(param, loc),
        matchRules,
        loc
      )
  }

  /**
    * Creates the hash spec for the given enum, where `param` is the parameter to the function.
    */
  private def mkHashSpec(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val hashClassSym = PredefinedClasses.lookupClassSym("Hash", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(hashClassSym, loc), tpe, loc)),
          Nil,
          Type.mkPureArrow(tpe, Type.mkInt32(loc), loc)
        ),
        tpe = Type.mkInt32(loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TypeConstraint(Ast.TypeConstraint.Head(hashClassSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates a Hash match rule for the given enum case.
    */
  private def mkHashMatchRule(caze: KindedAst.Case, index: Int, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val hashSigSym = PredefinedClasses.lookupSigSym("Hash", "hash", root)
      val combineDefSym = PredefinedClasses.lookupDefSym(List("Hash"), "combine", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat, varSyms) = mkPattern(sym, tpe, "x", loc)

      // build a hash code by repeatedly adding elements via the combine function
      // the first hash is the index + 1
      // `3 `combine` hash(x0) `combine` hash(y0)`
      val exp = varSyms.foldLeft(KindedAst.Expr.Cst(Ast.Constant.Int32(index + 1), loc): KindedAst.Expr) {
        case (acc, varSym) =>
          // `acc `combine` hash(varSym)
          KindedAst.Expr.Apply(
            KindedAst.Expr.Def(combineDefSym, Type.freshVar(Kind.Star, loc), loc),
            List(
              acc,
              KindedAst.Expr.Apply(
                KindedAst.Expr.Sig(hashSigSym, Type.freshVar(Kind.Star, loc), loc),
                List(mkVarExpr(varSym, loc)),
                Type.freshVar(Kind.Star, loc),
                Type.freshVar(Kind.Eff, loc),
                loc
              ),
            ),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates an Sendable instance for the given enum.
    *
    * {{{
    * enum E[a] with Sendable {
    *   case C1
    *   case C2(a)
    *   case C3(a, Int32)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Sendable[E[a]] with Sendable[a]
    * }}}
    *
    * The instance is empty: we check for immutability by checking for the absence of region kinded type parameters.
    */
  private def mkSendableInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val sendableClassSym = PredefinedClasses.lookupClassSym("Sendable", root)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, sendableClassSym, loc)

      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        clazz = Ast.ClassSymUse(sendableClassSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        defs = Nil,
        assocs = Nil,
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Returns the cases in `m` in a *stable order* that relies on the order of their source locations.
    */
  private def getCasesInStableOrder(m: Map[Symbol.CaseSym, KindedAst.Case]): List[KindedAst.Case] = {
    m.values.toList.sortBy(_.loc)
  }

  /**
    * Creates type constraints for the given type parameters.
    * Filters out non-star type parameters and wild type parameters.
    */
  private def getTypeConstraintsForTypeParams(tparams: List[KindedAst.TypeParam], clazz: Symbol.ClassSym, loc: SourceLocation): List[Ast.TypeConstraint] = tparams.collect {
    case tparam if tparam.sym.kind == Kind.Star && !tparam.name.isWild =>
      Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), Type.Var(tparam.sym, loc), loc)
  }

  /**
    * Builds a string expression from the given string.
    */
  private def mkStrExpr(str: String, loc: SourceLocation): KindedAst.Expr = KindedAst.Expr.Cst(Ast.Constant.Str(str), loc)

  /**
    * Builds a var expression from the given var sym.
    */
  private def mkVarExpr(varSym: Symbol.VarSym, loc: SourceLocation): KindedAst.Expr.Var = KindedAst.Expr.Var(varSym, loc)

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  private def concat(exp1: KindedAst.Expr, exp2: KindedAst.Expr, loc: SourceLocation)(implicit flix: Flix): KindedAst.Expr = {
    KindedAst.Expr.Binary(SemanticOp.StringOp.Concat, exp1, exp2, Type.freshVar(Kind.Star, loc), loc)
  }

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  private def concatAll(exps: List[KindedAst.Expr], loc: SourceLocation)(implicit flix: Flix): KindedAst.Expr = {
    exps match {
      case Nil => KindedAst.Expr.Cst(Ast.Constant.Str(""), loc)
      case head :: tail => tail.foldLeft(head)(concat(_, _, loc))
    }
  }

  /**
    * Extracts the types from the given aggregate type.
    * A Unit unpacks to an empty list.
    * A Tuple unpacks to its member types.
    * Anything else unpacks to the singleton list of itself.
    */
  private def unpack(tpe: Type): List[Type] = tpe.typeConstructor match {
    case Some(TypeConstructor.Unit) => Nil
    case Some(TypeConstructor.Tuple(_)) => tpe.typeArguments
    case _ => List(tpe)
  }

  /**
    * Creates a pattern corresponding to the given tag type.
    */
  private def mkPattern(sym: Symbol.CaseSym, tpe: Type, varPrefix: String, loc: SourceLocation)(implicit flix: Flix): (KindedAst.Pattern, List[Symbol.VarSym]) = {
    unpack(tpe) match {
      case Nil => (KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), KindedAst.Pattern.Cst(Ast.Constant.Unit, loc), Type.freshVar(Kind.Star, loc), loc), Nil)
      case _ :: Nil =>
        val varSym = Symbol.freshVarSym(s"${varPrefix}0", BoundBy.Pattern, loc)
        (KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), mkVarPattern(varSym, loc), Type.freshVar(Kind.Star, loc), loc), List(varSym))
      case tpes =>
        val varSyms = tpes.zipWithIndex.map { case (_, index) => Symbol.freshVarSym(s"$varPrefix$index", BoundBy.Pattern, loc) }
        val subPats = varSyms.map(varSym => mkVarPattern(varSym, loc))
        (KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), KindedAst.Pattern.Tuple(subPats, loc), Type.freshVar(Kind.Star, loc), loc), varSyms)
    }
  }

  /**
    * Creates a variable pattern using the given variable symbol.
    */
  private def mkVarPattern(varSym: Symbol.VarSym, loc: SourceLocation): KindedAst.Pattern = KindedAst.Pattern.Var(varSym, varSym.tvar, loc)

  /**
    * Inserts `sep` between every two elements of `list`.
    */
  private def intersperse[A](list: List[A], sep: A): List[A] = list match {
    case Nil => Nil
    case last :: Nil => last :: Nil
    case head :: neck :: tail => head :: sep :: intersperse(neck :: tail, sep)
  }
}
