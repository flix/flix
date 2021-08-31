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
object Deriver extends Phase[KindedAst.Root, KindedAst.Root] {

  override def run(root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, Nothing] = {
    val derivedInstances = root.enums.values.flatMap {
      enum => getDerivations(enum, root)
    }

    val newInstances = derivedInstances.foldLeft(root.instances) {
      case (acc, inst) =>
        val accInsts = acc.getOrElse(inst.sym, Nil)
        acc + (inst.sym -> (inst :: accInsts))
    }

    root.copy(instances = newInstances).toSuccess
  }

  /**
    * Builds the instances derived from this enum.
    */
  private def getDerivations(enum: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): List[KindedAst.Instance] = enum match {
    case KindedAst.Enum(_, _, _, _, derives, _, _, _, _) =>
      // lazy so that in we don't try a lookup if there are no derivations (important for Nix Lib)
      lazy val toStringSym = PredefinedClasses.lookupClassSym("ToString", root)
      derives.map {
        case Ast.Derivation(sym, loc) if sym == toStringSym => createToString(enum, loc, root)
        case unknownSym => throw InternalCompilerException(s"Unexpected derivation: $unknownSym")
      }
  }

  /**
    * Creates a toString instance for the given enum.
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
    *     case C1(y) => "C1" + "(" + ToString.toString(y) + ")"
    *     case C2(y0, y1) => "C2" + "(" + ToString.toString(y0) + ", " + ToString.toString(y1) + ")"
    *   }
    * }}}
    */
  private def createToString(enum: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = enum match {
    case KindedAst.Enum(_, _, _, tparams, _, cases, _, sc, _) =>
      val toStringClassSym = PredefinedClasses.lookupClassSym("ToString", root)
      val toStringDefSym = Symbol.mkDefnSym("ToString.toString")

      // create a match rule for each case and put them in a match expression
      val matchRules = cases.values.map(createToStringMatchRule(_, loc, root))
      val varSym = Symbol.freshVarSym("x", loc)
      val exp = KindedAst.Expression.Match(
        mkVarExpr(varSym, loc),
        matchRules.toList,
        loc
      )

      val spec = KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(varSym, Ast.Modifiers.Empty, sc.base, loc)),
        sc = Scheme(
          tparams.map(_.tpe),
          List(Ast.TypeConstraint(toStringClassSym, sc.base, loc)),
          Type.mkPureArrow(sc.base, Type.mkString(loc), loc)
        ),
        tpe = Type.mkString(loc),
        eff = Type.Cst(TypeConstructor.True, loc),
        loc = loc
      )
      val defn = KindedAst.Def(toStringDefSym, spec, exp)

      // Add a type constraint to the instance for any non-wild param with kind Star
      val tconstrs = tparams.collect {
        case tparam if tparam.tpe.kind <:: Kind.Star && !tparam.name.isWild => Ast.TypeConstraint(toStringClassSym, tparam.tpe, loc)
      }

      KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        mod = Ast.Modifiers.Empty,
        sym = toStringClassSym,
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      )
  }

  /**
    * Creates a ToString match rule for the given enum case.
    */
  private def createToStringMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(enum, tag, tpeDeprecated, sc) =>

      // get a pattern corresponding to this case
      val (pat, varSyms) = mkPattern(sc.base, loc)

      val guard = KindedAst.Expression.True(loc)

      val tagPart = KindedAst.Expression.Str(tag.name, loc)

      val toStringSym = PredefinedClasses.lookupSigSym("ToString", "toString", root)
      // call toString on each variable
      val toStrings = varSyms.map {
        varSym =>
          KindedAst.Expression.Apply(
            KindedAst.Expression.Sig(toStringSym, Type.freshVar(Kind.Star, loc), loc),
            List(mkVarExpr(varSym, loc)),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Bool, loc),
            loc
          )
      }

      // put commas between the arguments
      val sep = mkStrExpr(", ", loc)
      val valuePart = intersperse(toStrings, sep)

      // put it all together
      val exp = valuePart match {
        // Case 1: no arguments: just show the tag
        case Nil => tagPart
        // Case 2: at least one argument: concatenate the tag with the values wrapped in parens
        case exps => concatAll(tagPart :: mkStrExpr("(", loc) :: (exps :+ mkStrExpr(")", loc)), loc)
      }

      KindedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Builds a string expression from the given string.
    */
  private def mkStrExpr(str: String, loc: SourceLocation): KindedAst.Expression.Str = KindedAst.Expression.Str(str, loc)

  /**
    * Builds a string expression from the given string.
    */
  private def mkVarExpr(varSym: Symbol.VarSym, loc: SourceLocation): KindedAst.Expression.Var = KindedAst.Expression.Var(varSym, varSym.tvar.ascribedWith(Kind.Star), loc)

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  private def concat(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation)(implicit flix: Flix): KindedAst.Expression = {
    KindedAst.Expression.Binary(SemanticOperator.StringOp.Concat, exp1, exp2, Type.freshVar(Kind.Star, loc), loc)
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
  private def mkPattern(tpe: Type, loc: SourceLocation)(implicit flix: Flix): (KindedAst.Pattern, List[Symbol.VarSym]) = tpe.typeConstructor match {
    case Some(TypeConstructor.Tag(sym, tag)) =>
      getTagArguments(tpe) match {
        case Nil => (KindedAst.Pattern.Tag(sym, tag, KindedAst.Pattern.Unit(loc), Type.freshVar(Kind.Star, loc), loc), Nil)
        case _ :: Nil =>
          val varSym = Symbol.freshVarSym("y", loc)
          (KindedAst.Pattern.Tag(sym, tag, mkVarPattern(varSym, loc), Type.freshVar(Kind.Star, loc), loc), List(varSym))
        case tpes =>
          val varSyms = tpes.zipWithIndex.map { case (_, index) => Symbol.freshVarSym(s"y$index", loc) }
          val subPats = varSyms.map(varSym => mkVarPattern(varSym, loc))
          (KindedAst.Pattern.Tag(sym, tag, KindedAst.Pattern.Tuple(subPats, loc), Type.freshVar(Kind.Star, loc), loc), varSyms)
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
