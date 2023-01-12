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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.Typer
import ca.uwaterloo.flix.language.phase.Typer.inferExp
import ca.uwaterloo.flix.language.phase.unification.InferMonad
import ca.uwaterloo.flix.language.phase.unification.InferMonad.traverseM
import ca.uwaterloo.flix.language.phase.unification.Unification.{liftM, unifyTypeM}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.ListOps.unzip4

object RestrictableChooseInference {

  /**
    * Returns the domain of the list of rules,
    * i.e., the set of tags in the patterns.
    */
  private def dom(rules: List[KindedAst.RestrictableChoiceRule]): List[Symbol.RestrictableCaseSym] = {
    rules.map {
      case KindedAst.RestrictableChoiceRule(KindedAst.RestrictableChoicePattern.Tag(sym, _, _, _), _, _) => sym.sym
    }
  }

  /**
    * Converts the list of restrictable case symbols to a set type.
    */
  private def toType(syms: List[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym, loc: SourceLocation): Type = {
    syms.map {
        case sym => Type.Cst(TypeConstructor.CaseConstant(sym), loc.asSynthetic)
      }.reduceLeft[Type] {
        case (acc, tpe) => Type.mkApply(
          Type.Cst(TypeConstructor.CaseUnion(enumSym), loc.asSynthetic),
          List(acc, tpe),
          loc.asSynthetic
        )
      }
  }

  /**
    * Unifies t1 and t2 where t1 is a subset of t2.
    */
  private def unifySubset(t1: Type, t2: Type, sym: Symbol.RestrictableEnumSym, loc: SourceLocation)(implicit flix: Flix): InferMonad[Unit] = {
    val diff = Type.mkCaseDifference(t1, t2, sym, loc)
    // t1 <: t2 <=> t1 - t2 ≡ ∅
    for {
      _ <- unifyTypeM(diff, Type.Cst(TypeConstructor.CaseEmpty(sym), loc), loc)
    } yield ()
  }

  /**
    * Performs type inference on the given restrictable choose expression.
    */
  def infer(exp: KindedAst.Expression.RestrictableChoose, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type, Type)] = exp match {
    case KindedAst.Expression.RestrictableChoose(false, exp0, rules0, tpe0, loc) =>

      // Get the enum symbols for the matched type
      val enumSym = rules0.headOption.getOrElse(throw InternalCompilerException("unexpected empty choose", loc)).pat match {
        case KindedAst.RestrictableChoicePattern.Tag(sym, _, _, _) => sym.sym.enumSym
      }

      val enum = root.restrictableEnums(enumSym)

      // Make fresh vars for all the type parameters
      // This will unify with the enum type to extract the index
      val targs = (enum.index :: enum.tparams).map {
        case KindedAst.TypeParam(_, sym, loc) => Type.freshVar(sym.kind, loc.asSynthetic, sym.isRegion)
      }
      val enumConstructorKind = Kind.mkArrow(targs.map(_.kind))
      val enumConstructor = Type.Cst(TypeConstructor.RestrictableEnum(enumSym, enumConstructorKind), loc.asSynthetic)

      // The expected enum type.
      val enumType = Type.mkApply(enumConstructor, targs, loc.asSynthetic)

      val domM = toType(dom(rules0), enumSym, loc.asSynthetic)

      for {
        // Γ ⊢ e: τ_in
        (constrs, tpe, pur, eff) <- Typer.inferExp(exp0, root)
        patTpes <- inferRestrictableChoicePatterns(rules0.map(_.pat), root)
        _ <- unifyTypeM(tpe :: patTpes, loc)

        // τ_in = (... + l_i(τ_i) + ...)[φ_in]
        _ <- unifyTypeM(enumType, tpe, loc)

        // φ_in <: dom(M)
        _ <- unifySubset(targs.head, domM, enumSym, loc.asSynthetic)

        // Γ, x_i: τ_i ⊢ e_i: τ_out
        (constrss, tpes, purs, effs) <- traverseM(rules0)(rule => inferExp(rule.exp, root)).map(unzip4)
        resultTconstrs = constrs ::: constrss.flatten

        // τ_out
        resultTpe <- unifyTypeM(tpe0 :: tpes, loc)
        resultPur = Type.mkAnd(pur :: purs, loc)
        resultEff = Type.mkUnion(eff:: effs, loc)
      } yield (resultTconstrs, resultTpe, resultPur, resultEff)

    case KindedAst.Expression.RestrictableChoose(true, exp, rules, tpe, loc) => ???
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
          tpes <- traverseM(pat)(inferVarOrWild)
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
  private def inferVarOrWild(pat: KindedAst.RestrictableChoicePattern.VarOrWild)(implicit flix: Flix): InferMonad[Type] = pat match {
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
