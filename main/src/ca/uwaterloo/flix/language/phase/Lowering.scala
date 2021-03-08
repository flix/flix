package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Predicate, Root}
import ca.uwaterloo.flix.language.ast.{Ast, Name, Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.Validation

object Lowering extends Phase[Root, Root] {

  val PolaritySym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.Polarity")
  val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint/Ast.BodyTerm")

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Lowering") {
    // TODO: Recursively visit every expression and replace every constraint by its corresponding Flix edition.
    ???
  }

  private def visitHeadPred(p: Predicate.Head): Expression = p match {
    case Head.Atom(pred, den, terms, tpe, loc) =>
      ???

    case Head.Union(exp, tpe, loc) => ???
  }

  private def visitBodyPred(p: Predicate.Body)(implicit root: Root, flix: Flix): Expression = p match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      val p = visitPolarity(polarity, loc)
      val ts = terms.map(visitBodyTerm)

      ???
    case Body.Guard(exp, loc) => ???
  }

  private def visitHeadTerm(e: Expression): Expression = ???

  private def visitBodyTerm(p: Pattern): Expression = p match {
    case Pattern.Wild(tpe, loc) => ??? // TODO: Support ?
    case Pattern.Var(sym, tpe, loc) => ??? // TODO: Translate to BodyTerm.Var
    case Pattern.Unit(loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.True(loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.False(loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Char(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Float32(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Float64(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int8(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int16(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int32(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Int64(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.BigInt(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Str(lit, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Tag(sym, tag, pat, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Tuple(elms, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.Array(elms, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.ArrayTailSpread(elms, sym, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
    case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) => ??? // TODO: Translate to BodyTerm.Lit
  }

  /**
    * Returns an expression that evaluates the given polarity `p` to a Flix expression.
    */
  private def visitPolarity(p: Ast.Polarity, loc: SourceLocation)(implicit root: Root, flix: Flix): Expression = p match {
    case Polarity.Positive =>
      val (_, tpe) = Scheme.instantiate(root.enums(PolaritySym).sc, InstantiateMode.Flexible)
      mkUnitTag(PolaritySym, "Positive", tpe, loc)

    case Polarity.Negative =>
      val (_, tpe) = Scheme.instantiate(root.enums(PolaritySym).sc, InstantiateMode.Flexible)
      mkUnitTag(PolaritySym, "Negative", tpe, loc)

  }

  private def visitSourceLocation(loc: SourceLocation): Expression = ???

  private def mkUnitTag(sym: Symbol.EnumSym, tag: String, tpe: Type, loc: SourceLocation): Expression = {
    val innerExp = Expression.Unit(loc)
    Expression.Tag(sym, Name.Tag(tag, loc), innerExp, tpe, Type.Pure, loc)
  }

}
