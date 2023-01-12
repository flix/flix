package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.Typer
import ca.uwaterloo.flix.language.phase.Typer.inferExp
import ca.uwaterloo.flix.language.phase.unification.InferMonad.traverseM
import ca.uwaterloo.flix.language.phase.unification.Unification.unifyTypeM
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
        patTpes <- Typer.inferRestrictableChoicePatterns(rules0.map(_.pat), root)
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

}
