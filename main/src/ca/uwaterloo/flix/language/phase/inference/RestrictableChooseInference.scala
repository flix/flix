/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.inference

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.KindedAst.RestrictableChoicePattern
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.Typer
import ca.uwaterloo.flix.language.phase.Typer.inferExp
import ca.uwaterloo.flix.language.phase.unification.InferMonad
import ca.uwaterloo.flix.language.phase.unification.InferMonad.traverseM
import ca.uwaterloo.flix.language.phase.unification.Unification.{expectTypeM, liftM, unifyTypeM}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.ListOps.unzip4

import scala.collection.immutable.SortedSet
import scala.collection.mutable

object RestrictableChooseInference {

  /**
    * Returns the domain of the list of rules,
    * i.e., the set of tags in the patterns.
    *
    * For example, if the rules are
    *
    * {{{
    *     match x {
    *         case Expr.Var(y) => Expr.Cst(22)
    *         case Expr.Xor(y, z) => Expr.Xor(y, z)
    *         case Expr.Cst(y)    => Expr.Cst(y)
    *         case Expr.And(y, z) => Expr.Or(x, y)
    *     }
    * }}}
    *
    * then it returns { Expr.Var, Expr.Xor, Expr.Cst, Expr.And }.
    */
  private def dom(rules: List[KindedAst.RestrictableChoiceRule]): Set[Symbol.RestrictableCaseSym] = {
    rules.map {
      case KindedAst.RestrictableChoiceRule(KindedAst.RestrictableChoicePattern.Tag(sym, _, _, _), _) => sym.sym
    }.toSet
  }

  /**
    * Converts the list of restrictable case symbols to a closed set type.
    */
  private def toType(syms: Set[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym, loc: SourceLocation): Type = {
    Type.Cst(TypeConstructor.CaseSet(syms.to(SortedSet), enumSym), loc)
  }

  /**
    * Unifies t1 and t2 where t1 is a subset of t2.
    */
  private def unifySubset(t1: Type, t2: Type, sym: Symbol.RestrictableEnumSym, root: KindedAst.Root, loc: SourceLocation)(implicit flix: Flix): InferMonad[Unit] = {
    val diff = Type.mkCaseDifference(t1, t2, sym, loc)
    // t1 <: t2 <=> t1 - t2 ≡ ∅
    for {
      _ <- unifyTypeM(diff, Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, sym), loc), loc)
    } yield ()
  }

  /**
    * Performs type inference on the given restrictable choose expression.
    */
  def infer(exp: KindedAst.Expression.RestrictableChoose, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = {

    exp match {
      case KindedAst.Expression.RestrictableChoose(false, exp0, rules0, tpe0, loc) =>

        // Get the enum symbols for the matched type
        val enumSym = rules0.headOption.getOrElse(throw InternalCompilerException("unexpected empty choose", loc)).pat match {
          case KindedAst.RestrictableChoicePattern.Tag(sym, _, _, _) => sym.sym.enumSym
        }

        val enum = root.restrictableEnums(enumSym)
        val universe = enum.cases.keys.toSet
        val (enumType, indexVar, _) = instantiatedEnumType(enumSym, enum, loc.asSynthetic)
        val domSet = dom(rules0)
        val domM = toType(domSet, enumSym, loc.asSynthetic)

        for {
          // Γ ⊢ e: τ_in
          (constrs, tpe, pur, eff) <- Typer.inferExp(exp0, root)
          patTpes <- inferRestrictableChoicePatterns(rules0.map(_.pat), root)
          _ <- unifyTypeM(tpe :: patTpes, loc)

          // τ_in = (... + l_i(τ_i) + ...)[φ_in]
          _ <- unifyTypeM(enumType, tpe, loc)

          // φ_in <: dom(M)
          _ <- if (domSet != universe) unifySubset(indexVar, domM, enumSym, root, loc.asSynthetic) else InferMonad.point(())

          // Γ, x_i: τ_i ⊢ e_i: τ_out
          (constrss, tpes, purs, effs) <- traverseM(rules0)(rule => inferExp(rule.exp, root)).map(unzip4)
          resultTconstrs = constrs ::: constrss.flatten

          // τ_out
          resultTpe <- unifyTypeM(tpe0 :: tpes, loc)
          resultPur = Type.mkAnd(pur :: purs, loc)
          resultEff = Type.mkUnion(eff :: effs, loc)
        } yield (resultTconstrs, resultTpe, resultPur, resultEff)

      case KindedAst.Expression.RestrictableChoose(true, exp0, rules0, tpe0, loc) =>

        // Get the enum symbols for the matched type
        val enumSym = rules0.headOption.getOrElse(throw InternalCompilerException("unexpected empty choose", loc)).pat match {
          case KindedAst.RestrictableChoicePattern.Tag(sym, _, _, _) => sym.sym.enumSym
        }

        val enum = root.restrictableEnums(enumSym)

        // The expected enum types and the index variables.
        val (enumTypeIn, indexInVar, _) = instantiatedEnumType(enumSym, enum, loc.asSynthetic)
        val (enumTypeOut, indexOutVar, targsOut) = instantiatedEnumType(enumSym, enum, loc.asSynthetic)
        val (bodyTypes, bodyIndexVars, bodyTargs) = rules0.map(_ => instantiatedEnumType(enumSym, enum, loc.asSynthetic)).unzip3
        val patternTagTypes = rules0.map(_.pat match {
          case RestrictableChoicePattern.Tag(sym, _, _, loc) => Type.Cst(TypeConstructor.CaseSet(SortedSet(sym.sym), enumSym), loc.asSynthetic)
        })

        val domSet = dom(rules0)
        val domM = toType(domSet, enumSym, loc.asSynthetic)

        def mkUnion(l: List[Type]): Type =
          l.reduceOption(Type.mkCaseUnion(_, _, enumSym, loc.asSynthetic)).
            getOrElse(throw InternalCompilerException("unexpected empty choose", loc))

        for {
          // Γ ⊢ e: τ_in
          (constrs, tpe, pur, eff) <- Typer.inferExp(exp0, root)
          patTpes <- inferRestrictableChoicePatterns(rules0.map(_.pat), root)
          _ <- unifyTypeM(tpe :: patTpes, loc)

          // τ_in = (... + l^in_i(τ^in_i) + ...)[φ_in]
          _ <- unifyTypeM(enumTypeIn, tpe, loc)

          // φ_in <: dom(M)
          _ <- unifySubset(indexInVar, domM, enumSym, root, loc.asSynthetic)

          // Γ, x_i: τ^in_i ⊢ e_i: τ^out_i
          (constrss, tpes, purs, effs) <- traverseM(rules0)(rule => inferExp(rule.exp, root)).map(unzip4)
          _ <- traverseM(tpes.zip(bodyTypes)){case (t1, t2) => unifyTypeM(t1, t2, loc)}

          // τ_out = (... + l^out_i(τ^out_i) + ...)[_]
          _ <- traverseM((targsOut :: bodyTargs).transpose)(unifyTypeM(_, loc))

          indicesAndTags = bodyIndexVars.zip(patternTagTypes)
          intros = mkUnion(indicesAndTags.map { case (i, tag) => Type.mkCaseDifference(i, tag, enumSym, loc.asSynthetic) })
          potentiallyStable = mkUnion(indicesAndTags.map { case (i, tag) => Type.mkCaseIntersection(i, tag, enumSym, loc.asSynthetic) })

          // φ_out :> (φ_in ∩ potentiallyStable) ∪ intros
          set = Type.mkCaseUnion(
            Type.mkCaseIntersection(indexInVar, potentiallyStable, enumSym, loc),
            intros,
            enumSym,
            loc
          )
          _ <- unifySubset(set, indexOutVar, enumSym, root, loc)

          resultTconstrs = constrs ::: constrss.flatten

          // τ_out
          resultTpe <- unifyTypeM(enumTypeOut, tpe0, loc)
          resultPur = Type.mkAnd(pur :: purs, loc)
          resultEff = Type.mkUnion(eff :: effs, loc)
        } yield (resultTconstrs, resultTpe, resultPur, resultEff)

    }
  }

  /**
    * Performs type inference on the given restrictable tag expression.
    */
  def inferRestrictableTag(exp: KindedAst.Expression.RestrictableTag, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = exp match {
    case KindedAst.Expression.RestrictableTag(symUse, exp, isOpen, tvar, loc) =>

      // Lookup the enum declaration.
      val enumSym = symUse.sym.enumSym
      val decl = root.restrictableEnums(enumSym)

      // Lookup the case declaration.
      val caze = decl.cases(symUse.sym)

      val (enumType, indexVar, _) = instantiatedEnumType(enumSym, decl, loc.asSynthetic)

      // Instantiate the type scheme of the case.
      val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

      // Add a free variable only if the tag is open
      val index = if (isOpen) {
        // φ ∪ {l_i}
        Type.mkCaseUnion(
          Type.freshVar(Kind.CaseSet(enumSym), loc.asSynthetic),
          Type.Cst(TypeConstructor.CaseSet(SortedSet(symUse.sym), enumSym), loc.asSynthetic),
          enumSym,
          loc.asSynthetic
        )
      } else {
        // {l_i}
        Type.Cst(TypeConstructor.CaseSet(SortedSet(symUse.sym), enumSym), loc.asSynthetic)
      }

      //
      // The tag type is a function from the type of variant to the type of the enum.
      //
      for {
        // Γ ⊢ e: τ
        (constrs, tpe, pur, eff) <- Typer.inferExp(exp, root)
        _ <- unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
        // τ = (... + l_i(τ_i) + ...)[φ ∪ {l_i}]
        _ <- unifyTypeM(enumType, tvar, loc)
        _ <- unifyTypeM(indexVar, index, loc)
        resultTyp = tvar
        resultPur = pur
        resultEff = eff
      } yield (constrs, resultTyp, resultPur, resultEff)
  }

  /**
    * Performs type inference on the given `of` expression.
    */
  def inferOf(exp0: KindedAst.Expression.Of, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = exp0 match {
    case KindedAst.Expression.Of(symUse, exp, tvar, loc) =>

      // Must check that the type of the expression cannot return anything but our symbol

      val enumSym = symUse.sym.enumSym
      val enum = root.restrictableEnums(enumSym)

      val caseType = Type.Cst(TypeConstructor.CaseSet(SortedSet(symUse.sym), enumSym), symUse.loc)

      val (enumType, indexVar, _) = instantiatedEnumType(enumSym, enum, loc.asSynthetic)

      for {
        // infer the inner expression type
        (constrs, tpe, pur, eff) <- Typer.inferExp(exp, root)

        // set the expected type to (...)[TheCase]
        _ <- unifyTypeM(indexVar, caseType, loc)

        // check that the expression type matches the expected type
        _ <- expectTypeM(expected = enumType, actual = tpe, loc)

        // unify with the tvar
        _ <- unifyTypeM(tvar, tpe, loc)
      } yield (constrs, tpe, pur, eff)
  }

  /**
    * Performs type inference on the given OpenAs expression.
    *
    * `OpenAs X e` requires that `e` have the type X[s] for some s
    * The result type of the expression is X[s + φ] for some free φ
    *
    *         Γ ⊢ e : X[s][α1 ... αn]
    * -------------------------------------
    * Γ ⊢ open_as X e : X[s + φ][α1 ... αn]
    */
  def inferOpenAs(exp0: KindedAst.Expression.OpenAs, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = exp0 match {
    case KindedAst.Expression.OpenAs(sym, exp, tvar, loc) =>
      val enum = root.restrictableEnums(sym)

      val (enumType, indexVar, targs) = instantiatedEnumType(sym, enum, loc.asSynthetic)
      val kargs = enum.index.sym.kind :: enum.tparams.map(_.sym.kind)
      val kind = Kind.mkArrow(kargs)

      for {
        // infer the inner expression type τ
        (constrs, tpe, pur, eff) <- Typer.inferExp(exp, root)

        // make sure the expression has type EnumType[s][α1 ... αn]
        _ <- expectTypeM(expected = enumType, actual = tpe, loc)

        // the new index is s ∪ φ for some free φ
        openIndex = Type.mkCaseUnion(indexVar, Type.freshVar(Kind.CaseSet(enum.sym), loc.asSynthetic), sym, loc)

        // the result type is EnumType[s ∪ φ][α1 ... αn]
        resultType = Type.mkApply(
          Type.Cst(TypeConstructor.RestrictableEnum(sym, kind), loc.asSynthetic),
          openIndex :: targs,
          loc
        )

        // unify the tvar
        _ <- unifyTypeM(tvar, resultType, loc)

      } yield (constrs, resultType, pur, eff)

  }

  /**
    * Returns the instantiated conceptual schema of the enum along with the type
    * variable that is the index type argument.
    *
    * The first and the second instantiation share all variables except the index.
    */
  private def instantiatedEnumType(enumSym: Symbol.RestrictableEnumSym, decl: KindedAst.RestrictableEnum, loc: SourceLocation)(implicit flix: Flix): (Type, Type.Var, List[Type]) = {
    // TODO RESTR-VARS can get rid of enumSym since it's in the decl
    // Make fresh vars for all the type parameters
    // This will unify with the enum type to extract the index

    def instantiate(tp: KindedAst.TypeParam): Type.Var = tp match {
      case KindedAst.TypeParam(_, sym, loc) => Type.freshVar(sym.kind, loc, sym.isRegion)
    }

    val indexVar = instantiate(decl.index)
    val tparamArgs = decl.tparams.map(instantiate)
    val targs = indexVar :: tparamArgs

    val enumConstructorKind = Kind.mkArrow(targs.map(_.kind))
    val enumConstructor = Type.Cst(TypeConstructor.RestrictableEnum(enumSym, enumConstructorKind), loc)

    // The expected enum type.
    val enumType = Type.mkApply(enumConstructor, targs, loc)

    (enumType, indexVar, tparamArgs)
  }

  /**
    * Infers the type of the given restrictable choice pattern `pat0`.
    */
  private def inferRestrictableChoicePattern(pat0: KindedAst.RestrictableChoicePattern, root: KindedAst.Root)(implicit flix: Flix): InferMonad[Type] = {

    /**
      * Local pattern visitor.
      */
    def visit(p: KindedAst.RestrictableChoicePattern): InferMonad[Type] = p match {
      case KindedAst.RestrictableChoicePattern.Tag(symUse, pat, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = root.restrictableEnums(symUse.sym.enumSym)

        // Lookup the case declaration.
        val caze = decl.cases(symUse.sym)

        // Instantiate the type scheme of the case.
        val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        //
        // The tag type is a function from the type of variant to the type of the enum.
        //
        for {
          tpes <- traverseM(pat)(inferVarOrWild(_, root))
          tpe = Type.mkTuplish(tpes, loc)
          _ <- unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
          resultTyp = tvar
        } yield resultTyp
    }

    visit(pat0)
  }

  /**
    * Infers the type of the given restrictable choice pattern `pat0`.
    */
  private def inferVarOrWild(pat: KindedAst.RestrictableChoicePattern.VarOrWild, root: KindedAst.Root)(implicit flix: Flix): InferMonad[Type] = pat match {
    case KindedAst.RestrictableChoicePattern.Wild(tvar, _) => liftM(tvar)
    case KindedAst.RestrictableChoicePattern.Var(sym, tvar, loc) => unifyTypeM(sym.tvar, tvar, loc)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferRestrictableChoicePatterns(pats0: List[KindedAst.RestrictableChoicePattern], root: KindedAst.Root)(implicit flix: Flix): InferMonad[List[Type]] = {
    traverseM(pats0)(inferRestrictableChoicePattern(_, root))
  }

}
