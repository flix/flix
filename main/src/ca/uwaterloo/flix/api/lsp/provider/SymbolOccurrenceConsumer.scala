package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.Visitor.Consumer
import ca.uwaterloo.flix.language.ast.Ast.{AssocTypeConstructor, Derivation, Derivations, EqualityConstraint}
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record.RecordLabelPattern
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{AssocTypeDef, AssocTypeSig, Binder, Case, CatchRule, Constraint, ConstraintParam, Def, Effect, Enum, Expr, FormalParam, HandlerRule, Instance, JvmMethod, MatchRule, Op, ParYieldFragment, Pattern, Predicate, PredicateParam, SelectChannelRule, Sig, Struct, StructField, Trait, TypeAlias, TypeMatchRule, TypeParam}
import ca.uwaterloo.flix.language.ast.shared.{Annotation, TraitConstraint}
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, CaseSymUse, DefSymUse, EffectSymUse, LocalDefSymUse, OpSymUse, SigSymUse, StructFieldSymUse, TraitSymUse}

case class SymbolOccurrenceConsumer(target: Symbol) extends Consumer {
  var occurances: List[Symbol] = Nil

  private def check(sym: Symbol): Unit = {
    if (sym == target) { add(sym) }
  }

  private def add(x: Symbol): Unit = {
    occurances = x :: occurances
  }

  override def consumeExpr(exp: Expr): Unit = exp match {
    case Expr.Var(sym, _, _) => if (sym == target) { add(sym) }
    case _ => ()
  }

  override def consumeBinder(bnd: Binder): Unit = {
    val Binder(sym, _) = bnd
    check(sym)
  }

  override def consumeStructFieldSymUse(symUse: StructFieldSymUse): Unit = {
    val StructFieldSymUse(sym, _) = symUse
    check(sym)
  }

  override def consumeSigSymUse(symUse: SigSymUse): Unit = {
    val SigSymUse(sym, _) = symUse
    check(sym)
  }

  override def consumeDefSymUse(symUse: DefSymUse): Unit = {
    val DefSymUse(sym, _) = symUse
    check(sym)
  }

  override def consumeCaseSymUse(symUse: CaseSymUse): Unit = {
    val CaseSymUse(sym, _) = symUse
    check(sym)
  }

  override def consumeOpSymUse(symUse: OpSymUse): Unit = {
    val OpSymUse(sym, _) = symUse
    check(sym)
  }

  override def consumeEffectSymUse(effUse: EffectSymUse): Unit = {
    val EffectSymUse(sym, _) = effUse
    check(sym)
  }

  override def consumeTraitSymUse(symUse: TraitSymUse): Unit = {
    val TraitSymUse(sym, _) = symUse
    check(sym)
  }

  override def consumeLocalDefSym(symUse: LocalDefSymUse): Unit = {
    val LocalDefSymUse(sym, _) = symUse
    check(sym)
  }

  override def consumeAssocTypeSymUse(symUse: AssocTypeSymUse): Unit = {
    val AssocTypeSymUse(sym, _) = symUse
    check(sym)
  }
}
