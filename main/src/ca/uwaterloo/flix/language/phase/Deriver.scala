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
import ca.uwaterloo.flix.language.ast.Ast.VarText.FallbackText
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Name, Scheme, SemanticOperator, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * Constructs instances derived from enums.
  *
  * No errors are thrown in this phase: constructed instances must be well-formed.
  * Errors with overlapping instances or unfulfilled type constraints must be caught in later phases.
  */
object Deriver {

  def run(root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, Nothing] = flix.phase("Deriver") {
    val derivedInstances = root.enums.values.flatMap {
      enum => getDerivedInstances(enum, root)
    }

    val newInstances = derivedInstances.foldLeft(root.instances) {
      case (acc, inst) =>
        val accInsts = acc.getOrElse(inst.sym.clazz, Nil)
        acc + (inst.sym.clazz -> (inst :: accInsts))
    }

    root.copy(instances = newInstances).toSuccess
  }

  /**
    * Builds the instances derived from this enum.
    */
  private def getDerivedInstances(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): List[KindedAst.Instance] = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, derives, _, _, _, _) =>
      // lazy so that in we don't try a lookup if there are no derivations (important for Nix Lib)
      lazy val eqSym = PredefinedClasses.lookupClassSym("Eq", root)
      lazy val orderSym = PredefinedClasses.lookupClassSym("Order", root)
      lazy val toStringSym = PredefinedClasses.lookupClassSym("ToString", root)
      lazy val boxableSym = PredefinedClasses.lookupClassSym("Boxable", root)
      lazy val hashSym = PredefinedClasses.lookupClassSym("Hash", root)
      derives.map {
        case Ast.Derivation(sym, loc) if sym == eqSym => mkEqInstance(enum0, loc, root)
        case Ast.Derivation(sym, loc) if sym == orderSym => mkOrderInstance(enum0, loc, root)
        case Ast.Derivation(sym, loc) if sym == toStringSym => mkToStringInstance(enum0, loc, root)
        case Ast.Derivation(sym, loc) if sym == boxableSym => mkBoxableInstance(enum0, loc, root)
        case Ast.Derivation(sym, loc) if sym == hashSym => mkHashInstance(enum0, loc, root)
        case unknownSym => throw InternalCompilerException(s"Unexpected derivation: $unknownSym")
      }
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
  private def mkEqInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val eqClassSym = PredefinedClasses.lookupClassSym("Eq", root)
      val eqInstanceSym = Symbol.freshInstanceSym(eqClassSym, loc)
      val eqDefSym = Symbol.mkDefnSym("Eq.eq")

      val param1 = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val param2 = Symbol.freshVarSym("y", BoundBy.FormalParam, loc)
      val exp = mkEqImpl(enum0, param1, param2, loc, root)
      val spec = mkEqSpec(enum0, param1, param2, loc, root)

      val defn = KindedAst.Def(eqDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, eqClassSym, loc)

      KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        sym = eqInstanceSym,
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      )
  }

  /**
    * Creates the eq implementation for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkEqImpl(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expression = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _, _) =>
      // create a match rule for each case
      val mainMatchRules = cases.values.map(mkEqMatchRule(_, loc, root))

      // create a default rule
      // `case _ => false`
      val defaultRule = KindedAst.MatchRule(KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc, text = FallbackText("wild")), loc), KindedAst.Expression.True(loc), KindedAst.Expression.False(loc))

      // group the match rules in an expression
      KindedAst.Expression.Match(
        KindedAst.Expression.Tuple(List(mkVarExpr(param1, loc), mkVarExpr(param2, loc)), loc),
        (mainMatchRules ++ List(defaultRule)).toList,
        loc
      )
  }

  /**
    * Creates the eq spec for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkEqSpec(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val eqClassSym = PredefinedClasses.lookupClassSym("Eq", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param1, Ast.Modifiers.Empty, sc.base, loc), KindedAst.FormalParam(param2, Ast.Modifiers.Empty, sc.base, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(eqClassSym, loc), sc.base, loc)),
          Type.mkPureUncurriedArrow(List(sc.base, sc.base), Type.mkBool(loc), loc)
        ),
        tpe = Type.mkBool(loc),
        eff = Type.Cst(TypeConstructor.True, loc),
        loc = loc
      )
  }

  /**
    * Creates an Eq match rule for the given enum case.
    */
  private def mkEqMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(_, _, _, sc) =>
      val eqSym = PredefinedClasses.lookupSigSym("Eq", "eq", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat1, varSyms1) = mkPattern(sc.base, "x", loc)
      val (pat2, varSyms2) = mkPattern(sc.base, "y", loc)
      val pat = KindedAst.Pattern.Tuple(List(pat1, pat2), loc)

      val guard = KindedAst.Expression.True(loc)

      // call eq on each variable pair
      // `x0 == y0`, `x1 == y1`
      val eqs = varSyms1.zip(varSyms2).map {
        case (varSym1, varSym2) =>
          KindedAst.Expression.Apply(
            KindedAst.Expression.Sig(eqSym, Type.freshVar(Kind.Star, loc, text = FallbackText("eq")), loc),
            List(
              mkVarExpr(varSym1, loc),
              mkVarExpr(varSym2, loc)
            ),
            Type.freshVar(Kind.Star, loc, text = FallbackText("eqType")),
            Type.freshVar(Kind.Bool, loc, text = FallbackText("eqEff")),
            loc
          )
      }

      // put it all together
      // `x0 == y0 and x1 == y1`
      val exp = eqs match {
        // Case 1: no arguments: return true
        case Nil => KindedAst.Expression.True(loc)
        // Case 2: at least one argument: join everything with `and`
        case head :: tail => tail.foldLeft(head: KindedAst.Expression) {
          case (acc, eq) => KindedAst.Expression.Binary(SemanticOperator.BoolOp.And, acc, eq, Type.freshVar(Kind.Star, loc, text = FallbackText("and")), loc)
        }
      }

      KindedAst.MatchRule(pat, guard, exp)
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
  private def mkOrderInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val orderClassSym = PredefinedClasses.lookupClassSym("Order", root)
      val orderInstanceSym = Symbol.freshInstanceSym(orderClassSym, loc)
      val compareDefSym = Symbol.mkDefnSym("Order.compare")

      val param1 = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val param2 = Symbol.freshVarSym("y", BoundBy.FormalParam, loc)
      val exp = mkCompareImpl(enum0, param1, param2, loc, root)
      val spec = mkCompareSpec(enum0, param1, param2, loc, root)

      val defn = KindedAst.Def(compareDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, orderClassSym, loc)

      KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        sym = orderInstanceSym,
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      )
  }

  /**
    * Creates the compare implementation for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkCompareImpl(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expression = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _, _) =>
      val compareSigSym = PredefinedClasses.lookupSigSym("Order", "compare", root)

      val lambdaVarSym = Symbol.freshVarSym("indexOf", BoundBy.Let, loc)

      // Create the lambda mapping tags to indices
      val lambdaParamVarSym = Symbol.freshVarSym("e", BoundBy.FormalParam, loc)
      val indexMatchRules = cases.values.zipWithIndex.map { case (caze, index) => mkCompareIndexMatchRule(caze, index, loc) }
      val indexMatchExp = KindedAst.Expression.Match(mkVarExpr(lambdaParamVarSym, loc), indexMatchRules.toList, loc)
      val lambda = KindedAst.Expression.Lambda(
        KindedAst.FormalParam(lambdaParamVarSym, Ast.Modifiers.Empty, lambdaParamVarSym.tvar.ascribedWith(Kind.Star), loc),
        indexMatchExp,
        Type.freshVar(Kind.Star, loc, text = FallbackText("indexOfType")),
        loc
      )

      // Create the main match expression
      val matchRules = cases.values.map(mkComparePairMatchRule(_, loc, root))

      // Create the default rule:
      // `case _ => compare(indexOf(x), indexOf(y))`
      val defaultMatchRule = KindedAst.MatchRule(
        KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc, text = FallbackText("wild")), loc),
        KindedAst.Expression.True(loc),
        KindedAst.Expression.Apply(
          KindedAst.Expression.Sig(compareSigSym, Type.freshVar(Kind.Star, loc, text = FallbackText("compare")), loc),
          List(
            KindedAst.Expression.Apply(
              mkVarExpr(lambdaVarSym, loc),
              List(mkVarExpr(param1, loc)),
              Type.freshVar(Kind.Star, loc, text = FallbackText("xType")),
              Type.freshVar(Kind.Bool, loc, text = FallbackText("xEff")),
              loc
            ),
            KindedAst.Expression.Apply(
              mkVarExpr(lambdaVarSym, loc),
              List(mkVarExpr(param2, loc)),
              Type.freshVar(Kind.Star, loc, text = FallbackText("yType")),
              Type.freshVar(Kind.Bool, loc, text = FallbackText("yEff")),
              loc),
          ),
          Type.freshVar(Kind.Star, loc, text = FallbackText("compareType")),
          Type.freshVar(Kind.Bool, loc, text = FallbackText("compareEff")),
          loc
        )
      )

      // Wrap the cases in a match expression
      val matchExp = KindedAst.Expression.Match(
        KindedAst.Expression.Tuple(List(mkVarExpr(param1, loc), mkVarExpr(param2, loc)), loc),
        matchRules.toList :+ defaultMatchRule,
        loc
      )

      // Put the expressions together in a let
      KindedAst.Expression.Let(lambdaVarSym, Ast.Modifiers.Empty, lambda, matchExp, loc)
  }

  /**
    * Creates the eq spec for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkCompareSpec(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val orderClassSym = PredefinedClasses.lookupClassSym("Order", root)
      val comparisonEnumSym = PredefinedClasses.lookupEnum("Comparison", root)

      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param1, Ast.Modifiers.Empty, sc.base, loc), KindedAst.FormalParam(param2, Ast.Modifiers.Empty, sc.base, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(orderClassSym, loc), sc.base, loc)),
          Type.mkPureUncurriedArrow(List(sc.base, sc.base), Type.mkEnum(comparisonEnumSym, Kind.Star, loc), loc)
        ),
        tpe = Type.mkEnum(comparisonEnumSym, Kind.Star, loc),
        eff = Type.Cst(TypeConstructor.True, loc),
        loc = loc
      )
  }

  /**
    * Creates an indexing match rule, mapping the given case to the given index, e.g.
    * `case C2(_) => 2`
    */
  private def mkCompareIndexMatchRule(caze: KindedAst.Case, index: Int, loc: SourceLocation)(implicit Flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(_, _, _, sc) =>
      val TypeConstructor.Tag(sym, tag) = getTagConstructor(sc.base)
      val pat = KindedAst.Pattern.Tag(sym, tag, KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc, text = FallbackText("wild")), loc), Type.freshVar(Kind.Star, loc, text = FallbackText("tag")), loc)
      val guard = KindedAst.Expression.True(loc)
      val exp = KindedAst.Expression.Int32(index, loc)
      KindedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Creates a comparison match rule, comparing the elements of two tags of the same type.
    * ```case (C2(x0, x1), C2(y0, y1)) => compare(x0, y0) `thenCompare` lazy(x1, y1)```
    */
  private def mkComparePairMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(_, _, _, sc) =>
      val comparisonEnumSym = PredefinedClasses.lookupEnum("Comparison", root)
      val equalToTag = Name.Tag("EqualTo", loc)
      val compareSigSym = PredefinedClasses.lookupSigSym("Order", "compare", root)
      val thenCompareDefSym = PredefinedClasses.lookupDefSym(List("Order"), "thenCompare", root)

      // Match on the tuple
      // `case (C2(x0, x1), C2(y0, y1))
      val (pat1, varSyms1) = mkPattern(sc.base, "x", loc)
      val (pat2, varSyms2) = mkPattern(sc.base, "y", loc)
      val pat = KindedAst.Pattern.Tuple(List(pat1, pat2), loc)

      val guard = KindedAst.Expression.True(loc)

      // Call compare on each variable pair
      // `compare(x0, y0)`, `compare(x1, y1)`
      val compares = varSyms1.zip(varSyms2).map {
        case (varSym1, varSym2) =>
          KindedAst.Expression.Apply(
            KindedAst.Expression.Sig(compareSigSym, Type.freshVar(Kind.Star, loc, text = FallbackText("compare")), loc),
            List(
              mkVarExpr(varSym1, loc),
              mkVarExpr(varSym2, loc)
            ),
            Type.freshVar(Kind.Star, loc, text = FallbackText("compareType")),
            Type.freshVar(Kind.Bool, loc, text = FallbackText("compareEff")),
            loc
          )
      }

      /**
        * Joins the two expressions via `Compare.thenCompare`, making the second expression lazy.
        * (Cannot be inlined due to issues with Scala's type inference.
        */
      def thenCompare(exp1: KindedAst.Expression, exp2: KindedAst.Expression): KindedAst.Expression = {
        KindedAst.Expression.Apply(
          KindedAst.Expression.Def(thenCompareDefSym, Type.freshVar(Kind.Star, loc, text = FallbackText("thenCompare")), loc),
          List(
            exp1,
            KindedAst.Expression.Lazy(exp2, loc)
          ),
          Type.freshVar(Kind.Star, loc, text = FallbackText("thenCompareType")),
          Type.freshVar(Kind.Bool, loc, text = FallbackText("thenCompareEff")),
          loc
        )
      }

      // Put it all together
      // ```compare(x0, y0) `thenCompare` lazy compare(x1, y1)```
      val exp = compares match {
        // Case 1: no variables to compare; just return true
        case Nil => KindedAst.Expression.Tag(comparisonEnumSym, equalToTag, KindedAst.Expression.Unit(loc), Type.freshVar(Kind.Star, loc, text = FallbackText("tag")), loc)
        // Case 2: multiple comparisons to be done; wrap them in Order.thenCompare
        case cmps => cmps.reduceRight(thenCompare)
      }

      KindedAst.MatchRule(pat, guard, exp)
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
  private def mkToStringInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val toStringClassSym = PredefinedClasses.lookupClassSym("ToString", root)
      val toStringInstanceSym = Symbol.freshInstanceSym(toStringClassSym, loc)
      val toStringDefSym = Symbol.mkDefnSym("ToString.toString")

      val param = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val exp = mkToStringImpl(enum0, param, loc, root)
      val spec = mkToStringSpec(enum0, param, loc, root)

      val defn = KindedAst.Def(toStringDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, toStringClassSym, loc)

      KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        sym = toStringInstanceSym,
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      )
  }

  /**
    * Creates the toString implementation for the given enum, where `param` is the parameter to the function.
    */
  private def mkToStringImpl(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expression = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _, _) =>
      // create a match rule for each case
      val matchRules = cases.values.map(mkToStringMatchRule(_, loc, root))

      // group the match rules in an expression
      KindedAst.Expression.Match(
        mkVarExpr(param, loc),
        matchRules.toList,
        loc
      )
  }

  /**
    * Creates the toString spec for the given enum, where `param` is the parameter to the function.
    */
  private def mkToStringSpec(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val toStringClassSym = PredefinedClasses.lookupClassSym("ToString", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param, Ast.Modifiers.Empty, sc.base, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(toStringClassSym, loc), sc.base, loc)),
          Type.mkPureArrow(sc.base, Type.mkString(loc), loc)
        ),
        tpe = Type.mkString(loc),
        eff = Type.Cst(TypeConstructor.True, loc),
        loc = loc
      )
  }

  /**
    * Creates a ToString match rule for the given enum case.
    */
  private def mkToStringMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(_, tag, _, sc) =>
      val toStringSym = PredefinedClasses.lookupSigSym("ToString", "toString", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat, varSyms) = mkPattern(sc.base, "x", loc)

      val guard = KindedAst.Expression.True(loc)

      // "C2"
      val tagPart = KindedAst.Expression.Str(tag.name, loc)

      // call toString on each variable,
      // `toString(x0)`, `toString(x1)`
      val toStrings = varSyms.map {
        varSym =>
          KindedAst.Expression.Apply(
            KindedAst.Expression.Sig(toStringSym, Type.freshVar(Kind.Star, loc, text = FallbackText("toString")), loc),
            List(mkVarExpr(varSym, loc)),
            Type.freshVar(Kind.Star, loc, text = FallbackText("toStringType")),
            Type.freshVar(Kind.Bool, loc, text = FallbackText("toStringEff")),
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

      KindedAst.MatchRule(pat, guard, exp)
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
    *     case C0 => 0
    *     case C1(x0) => 1 * 31 + hash(x0)
    *     case C2(x0, x1) => (2 * 31 + hash(x0)) * 31 + hash(x1)
    *   }
    * }
    * }}}
    */
  private def mkHashInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val hashClassSym = PredefinedClasses.lookupClassSym("Hash", root)
      val hashInstanceSym = Symbol.freshInstanceSym(hashClassSym, loc)
      val hashDefSym = Symbol.mkDefnSym("Hash.hash")

      val param = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val exp = mkHashImpl(enum0, param, loc, root)
      val spec = mkHashSpec(enum0, param, loc, root)

      val defn = KindedAst.Def(hashDefSym, spec, exp)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, hashClassSym, loc)

      KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        sym = hashInstanceSym,
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      )
  }

  /**
    * Creates the hash implementation for the given enum, where `param` is the parameter to the function.
    */
  private def mkHashImpl(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expression = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _, _) =>
      // create a match rule for each case
      val matchRules = cases.values.zipWithIndex.map {
        case (caze, index) => mkHashMatchRule(caze, index, loc, root)
      }

      // group the match rules in an expression
      KindedAst.Expression.Match(
        mkVarExpr(param, loc),
        matchRules.toList,
        loc
      )
  }

  /**
    * Creates the hash spec for the given enum, where `param` is the parameter to the function.
    */
  private def mkHashSpec(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val hashClassSym = PredefinedClasses.lookupClassSym("Hash", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param, Ast.Modifiers.Empty, sc.base, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TypeConstraint(Ast.TypeConstraint.Head(hashClassSym, loc), sc.base, loc)),
          Type.mkPureArrow(sc.base, Type.mkInt32(loc), loc)
        ),
        tpe = Type.mkInt32(loc),
        eff = Type.Cst(TypeConstructor.True, loc),
        loc = loc
      )
  }

  /**
    * Creates a ToString match rule for the given enum case.
    */
  private def mkHashMatchRule(caze: KindedAst.Case, index: Int, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(_, _, _, sc) =>
      val hashSigSym = PredefinedClasses.lookupSigSym("Hash", "hash", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat, varSyms) = mkPattern(sc.base, "x", loc)

      val guard = KindedAst.Expression.True(loc)

      // build a hash code by repeatedly adding the next hash and multiplying by 31
      // the first hash is the index
      // `((2 * 31) + hash(x0)) * 31 + hash(y0)`
      val exp = varSyms.foldLeft(KindedAst.Expression.Int32(index, loc): KindedAst.Expression) {
        case (acc, varSym) =>
          // `(acc * 31) + hash(varSym)
          KindedAst.Expression.Binary(
            SemanticOperator.Int32Op.Add,
            KindedAst.Expression.Binary(
              SemanticOperator.Int32Op.Mul,
              acc,
              KindedAst.Expression.Int32(31, loc),
              Type.freshVar(Kind.Star, loc, text = FallbackText("product")),
              loc
            ),
            KindedAst.Expression.Apply(
              KindedAst.Expression.Sig(hashSigSym, Type.freshVar(Kind.Star, loc, text = FallbackText("hash")), loc),
              List(mkVarExpr(varSym, loc)),
              Type.freshVar(Kind.Star, loc, text = FallbackText("hashType")),
              Type.freshVar(Kind.Bool, loc, text = FallbackText("hashEff")),
              loc
            ),
            Type.freshVar(Kind.Star, loc, text = FallbackText("sum")),
            loc
          )
      }

      KindedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Creates a Boxable instance for the given enum.
    *
    * {{{
    * enum E[a] with Boxable {
    *   case C0
    *   case C1(a)
    *   case C2(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Boxable[E[a]]
    * }}}
    *
    * The instance is empty because the class has default definitions.
    */
  private def mkBoxableInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, _, sc, _) =>
      val boxableClassSym = PredefinedClasses.lookupClassSym("Boxable", root)
      val boxableInstanceSym = Symbol.freshInstanceSym(boxableClassSym, loc)

      val tconstrs = getTypeConstraintsForTypeParams(tparams, boxableClassSym, loc)

      KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        sym = boxableInstanceSym,
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = Nil,
        ns = Name.RootNS,
        loc = loc
      )
  }

  /**
    * Creates type constraints for the given type parameters.
    * Filters out non-star type parameters and wild type parameters.
    */
  private def getTypeConstraintsForTypeParams(tparams: List[KindedAst.TypeParam], clazz: Symbol.ClassSym, loc: SourceLocation): List[Ast.TypeConstraint] = tparams.collect {
    case tparam if tparam.sym.kind == Kind.Star && !tparam.name.isWild =>
      Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), Type.KindedVar(tparam.sym, loc), loc)
  }

  /**
    * Builds a string expression from the given string.
    */
  private def mkStrExpr(str: String, loc: SourceLocation): KindedAst.Expression.Str = KindedAst.Expression.Str(str, loc)

  /**
    * Builds a var expression from the given var sym.
    */
  private def mkVarExpr(varSym: Symbol.VarSym, loc: SourceLocation): KindedAst.Expression.Var = KindedAst.Expression.Var(varSym, varSym.tvar.ascribedWith(Kind.Star), loc)

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  private def concat(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation)(implicit flix: Flix): KindedAst.Expression = {
    KindedAst.Expression.Binary(SemanticOperator.StringOp.Concat, exp1, exp2, Type.freshVar(Kind.Star, loc, text = FallbackText("concat")), loc)
  }

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  private def concatAll(exps: List[KindedAst.Expression], loc: SourceLocation)(implicit flix: Flix): KindedAst.Expression = {
    exps match {
      case Nil => KindedAst.Expression.Str("", loc)
      case head :: tail => tail.foldLeft(head)(concat(_, _, loc))
    }
  }

  /**
    * Extracts the types from the given tag type.
    */
  private def getTagArguments(tpe: Type): List[Type] = {
    tpe.typeArguments.headOption match {
      case None => throw InternalCompilerException("Unexpected empty type arguments.")
      case Some(packedArgs) => unpack(packedArgs)
    }
  }

  /**
    * Extracts the enum sym from the given tag type.
    */
  private def getTagConstructor(tpe: Type): TypeConstructor.Tag = tpe.typeConstructor match {
    case Some(cst: TypeConstructor.Tag) => cst
    case _ => throw InternalCompilerException("Unexpected non-tag type.")
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
  private def mkPattern(tpe: Type, varPrefix: String, loc: SourceLocation)(implicit flix: Flix): (KindedAst.Pattern, List[Symbol.VarSym]) = tpe.typeConstructor match {
    case Some(TypeConstructor.Tag(sym, tag)) =>
      getTagArguments(tpe) match {
        case Nil => (KindedAst.Pattern.Tag(sym, tag, KindedAst.Pattern.Unit(loc), Type.freshVar(Kind.Star, loc, text = FallbackText("tag")), loc), Nil)
        case _ :: Nil =>
          val varSym = Symbol.freshVarSym(s"${varPrefix}0", BoundBy.Pattern, loc)
          (KindedAst.Pattern.Tag(sym, tag, mkVarPattern(varSym, loc), Type.freshVar(Kind.Star, loc, text = FallbackText("elm")), loc), List(varSym))
        case tpes =>
          val varSyms = tpes.zipWithIndex.map { case (_, index) => Symbol.freshVarSym(s"$varPrefix$index", BoundBy.Pattern, loc) }
          val subPats = varSyms.map(varSym => mkVarPattern(varSym, loc))
          (KindedAst.Pattern.Tag(sym, tag, KindedAst.Pattern.Tuple(subPats, loc), Type.freshVar(Kind.Star, loc, text = FallbackText("tuple")), loc), varSyms)
      }
    case _ => throw InternalCompilerException("Unexpected non-tag type.")
  }

  /**
    * Creates a variable pattern using the given variable symbol.
    */
  private def mkVarPattern(varSym: Symbol.VarSym, loc: SourceLocation): KindedAst.Pattern = KindedAst.Pattern.Var(varSym, varSym.tvar.ascribedWith(Kind.Star), loc)

  /**
    * Inserts `sep` between every two elements of `list`.
    */
  private def intersperse[A](list: List[A], sep: A): List[A] = list match {
    case Nil => Nil
    case last :: Nil => last :: Nil
    case head :: neck :: tail => head :: sep :: intersperse(neck :: tail, sep)
  }
}
