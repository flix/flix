package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Ast, Name, Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Predicate, Root}
import ca.uwaterloo.flix.util.Validation

object Lowering extends Phase[Root, Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Lowering") {
    ???
  }

  private def visitHeadPred(p: Predicate.Head): Expression = p match {
    case Head.Atom(pred, den, terms, tpe, loc) =>
      ???

    case Head.Union(exp, tpe, loc) => ???
  }

  private def visitBodyPred(p: Predicate.Body)(implicit root: Root, flix: Flix): Expression = p match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val polarity1 = visitPolarity(polarity, loc)
      ???
    case Body.Guard(exp, loc) => ???
  }

  private def visitHeadTerm(e: Expression): Expression = ???

  private def visitPolarity(p: Ast.Polarity, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = p match {
    case Polarity.Positive =>
      val sym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
      val tag = Name.Tag("Positive", loc)
      val innerExp = Expression.Unit(loc)
      val (_, tpe) = Scheme.instantiate(root.enums(sym).sc, InstantiateMode.Flexible)
      val eff = Type.Pure
      Expression.Tag(sym, tag, innerExp, tpe, eff, loc)

    case Polarity.Negative => ???
  }

  private def visitSourceLocation(loc: SourceLocation): Expression = ???

}
