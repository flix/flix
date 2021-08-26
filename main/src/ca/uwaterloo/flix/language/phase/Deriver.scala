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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Ast, MinLib, Name, ResolvedAst, SemanticOperator, SourceLocation, Symbol, UnkindedType}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

// MATT docs
object Deriver extends Phase[ResolvedAst.Root, ResolvedAst.Root] {

  // MATT maybe change to Nothing if no errors can occur here
  // MATT or maybe put duplicate derivation and stuff here?
  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Root, CompilationError] = {
    val derivedInstances = root.enums.values.flatMap {
      enum => getDerivations(enum)
    }

    val newInstances = derivedInstances.foldLeft(root.instances) {
      case (acc, inst) =>
        val accInsts = acc.getOrElse(inst.sym, Nil)
        acc + (inst.sym -> (inst :: accInsts))
    }

    root.copy(instances = newInstances).toSuccess
  }

  def getDerivations(enum: ResolvedAst.Enum)(implicit flix: Flix): List[ResolvedAst.Instance] = enum match {
    // MATT associate location with derives and use that location everywhere for checking duplicate syms and such
    case ResolvedAst.Enum(doc, mod, sym, tparams, derives, cases, tpeDeprecated, sc, loc) =>
      derives.map {
        case MinLib.ToString.sym => createToString(enum)
        case unknownSym => throw InternalCompilerException(s"Unexpected derivation: $unknownSym")
      }
  }

  def createToString(enum: ResolvedAst.Enum)(implicit flix: Flix): ResolvedAst.Instance = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams, derives, cases, tpeDeprecated, sc, loc) =>
      val matchRules = cases.values.map(createToStringMatchRule(_, sym))
      val varSym = Symbol.freshVarSym()
      val exp = ResolvedAst.Expression.Match(
        ResolvedAst.Expression.Var(varSym, varSym.tvar, SourceLocation.Unknown),
        matchRules.toList,
        SourceLocation.Unknown
      )
      val spec = ResolvedAst.Spec(
        doc = Ast.Doc(Nil, SourceLocation.Unknown),
        ann = Nil,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(ResolvedAst.FormalParam(varSym, Ast.Modifiers.Empty, sc.base, SourceLocation.Unknown)),
        sc = ResolvedAst.Scheme(
          tparams.tparams.map(_.tpe),
          List(ResolvedAst.TypeConstraint(MinLib.ToString.sym, sc.base, SourceLocation.Unknown)),
          UnkindedType.mkPureArrow(sc.base, UnkindedType.mkString(SourceLocation.Unknown))
        ),
        tpe = UnkindedType.mkString(SourceLocation.Unknown),
        eff = UnkindedType.Cst(UnkindedType.Constructor.Pure, SourceLocation.Unknown),
        loc = SourceLocation.Unknown
      )
      val defn = ResolvedAst.Def(Symbol.mkDefnSym("ToString.toString"), spec, exp)

      val caseTvars = for {
        caze <- cases.values
        tpe <- unpack(caze.sc.base.typeArguments.head) // MATT put into helper because it looks weird here
        if tpe.isInstanceOf[UnkindedType.Var]
      } yield tpe

      val tconstrs = caseTvars.toList.distinct.map(ResolvedAst.TypeConstraint(MinLib.ToString.sym, _, SourceLocation.Unknown))

      ResolvedAst.Instance(
        doc = Ast.Doc(Nil, SourceLocation.Unknown),
        mod = Ast.Modifiers.Empty,
        sym = MinLib.ToString.sym,
        tpe = sc.base,
        tconstrs = tconstrs,
        defs = List(defn),
        ns = Name.RootNS,
        loc = SourceLocation.Unknown
      )
  }

  def createToStringMatchRule(caze: ResolvedAst.Case, enumSym: Symbol.EnumSym)(implicit flix: Flix): ResolvedAst.MatchRule = caze match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated, sc) =>
      val (pat, varSyms) = mkPattern(sc.base)

      val guard = ResolvedAst.Expression.True(SourceLocation.Unknown)

      val tagPart = ResolvedAst.Expression.Str(tag.name, SourceLocation.Unknown)

      // MATT lots of inline docs pls
      val toStrings = varSyms.map {
        varSym =>
          ResolvedAst.Expression.Apply(
            ResolvedAst.Expression.Sig(MinLib.ToString.ToString.sym, SourceLocation.Unknown),
            List(ResolvedAst.Expression.Var(varSym, varSym.tvar, SourceLocation.Unknown)),
            SourceLocation.Unknown
          )
      }
      val sep = mkExpr(", ")
      val valuePart = intersperse(toStrings, sep)

      val exp = valuePart match {
        case Nil => tagPart
        case exps => concatAll(tagPart :: mkExpr("(") :: (exps :+ mkExpr(")")))
      }

      ResolvedAst.MatchRule(pat, guard, exp)
  }

  def mkExpr(str: String): ResolvedAst.Expression.Str = ResolvedAst.Expression.Str(str, SourceLocation.Unknown)

  def concat(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression): ResolvedAst.Expression = {
    ResolvedAst.Expression.Binary(SemanticOperator.StringOp.Concat, exp1, exp2, SourceLocation.Unknown)
  }

  def concatAll(exps: List[ResolvedAst.Expression]): ResolvedAst.Expression = {
    exps match {
      case Nil => ResolvedAst.Expression.Str("", SourceLocation.Unknown)
      case head :: tail => tail.foldLeft(head)(concat)
    }
  }

  def unpack(tpe: UnkindedType): List[UnkindedType] = tpe.typeConstructor match {
    case Some(UnkindedType.Constructor.Unit) => Nil
    case Some(UnkindedType.Constructor.Tuple(_)) => tpe.typeArguments
    case _ => List(tpe)
  }

  def mkPattern(tpe: UnkindedType)(implicit flix: Flix): (ResolvedAst.Pattern, List[Symbol.VarSym]) = tpe.typeConstructor match {
    case Some(UnkindedType.Constructor.Tag(sym, tag)) =>
      unpack(tpe.typeArguments.head) match {
        case Nil => (ResolvedAst.Pattern.Tag(sym, tag, ResolvedAst.Pattern.Unit(SourceLocation.Unknown), SourceLocation.Unknown), Nil)
        case _ :: Nil =>
          val varSym = Symbol.freshVarSym()
          (ResolvedAst.Pattern.Tag(sym, tag, ResolvedAst.Pattern.Var(varSym, SourceLocation.Unknown), SourceLocation.Unknown), List(varSym))
        case tpes =>
          val varSyms = tpes.map(_ => Symbol.freshVarSym())
          val subPats = varSyms.map(varSym => ResolvedAst.Pattern.Var(varSym, SourceLocation.Unknown))
          (ResolvedAst.Pattern.Tag(sym, tag, ResolvedAst.Pattern.Tuple(subPats, SourceLocation.Unknown), SourceLocation.Unknown), varSyms)
      }

  }

  def intersperse[A](list: List[A], sep: A): List[A] = list match {
    case Nil => Nil
    case last :: Nil => last :: Nil
    case head :: neck :: tail => head :: sep :: intersperse(neck :: tail, sep)
  }

}
