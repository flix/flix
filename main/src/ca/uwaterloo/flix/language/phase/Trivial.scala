/*
 *  Copyright 2019 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, Def, Expression, FormalParam, HandlerBinding, MatchRule, Root, SelectChannelRule}
import ca.uwaterloo.flix.language.ast.{Ast, BinaryOperator, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.TrivialError
import ca.uwaterloo.flix.language.errors.TrivialError.TrivialExpression
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

// TODO: Come up with better name.
object Trivial extends Phase[TypedAst.Root, TypedAst.Root] {

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, TrivialError] = flix.phase("Trivial") {

    // TODO: Introduce flag to disable

    // Check for trivial expressions.
    val trivial: List[TrivialError] = root.defs.par.aggregate(Nil: List[TrivialError])({
      case (acc, (sym, decl)) => acc ::: visitDef(decl)(root, flix)
    }, _ ++ _)

    if (trivial.isEmpty)
      root.toSuccess
    else
      Validation.Failure(trivial.toStream)
  }

  private object Catalog {

    val TInt32: Type = Type.Cst(TypeConstructor.Int32)

    import Expression._
    import SourceLocation.{Unknown => SL}
    import ast.Eff.Pure

    val Unit: Expression = Expression.Unit(SL)
    val Wild: Expression = Expression.Wild(TInt32, Pure, SL)
    val Zer: Expression = Expression.Int32(0, SL)
    val One: Expression = Expression.Int32(1, SL)
    val EmptyString: Expression = Expression.Str("", SL)
    val ListNil: Expression = Expression.Tag(Symbol.mkEnumSym("List"), "Nil", Unit, TInt32, Pure, SL)

    def mkVar()(implicit flix: Flix): Expression = Expression.Var(Symbol.freshVarSym()(flix.genSym), TInt32, Pure, SL)

    def add(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Plus, e1, e2, TInt32, Pure, SL)

    def sub(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Minus, e1, e2, TInt32, Pure, SL)

    def mul(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Times, e1, e2, TInt32, Pure, SL)

    def div(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Divide, e1, e2, TInt32, Pure, SL)

    def app(e1: Expression, e2: Expression): Expression = Apply(e1, e2, TInt32, Pure, SL)

    def tag(s: String, t: String, e: Expression): Expression = {
      val sym = Symbol.mkEnumSym(s)
      Tag(sym, t, e, TInt32, Pure, SL)
    }

    def defn(s: String): Expression = {
      val sym = Symbol.mkDefnSym(s)
      Expression.Def(sym, TInt32, Pure, SL)
    }

    def identity()(implicit flix: Flix): Expression = {
      val sym = Symbol.freshVarSym()(flix.genSym)
      val fparam = FormalParam(sym, Ast.Modifiers.Empty, TInt32, SL)
      val body = Var(sym, TInt32, Pure, SL)
      Lambda(fparam, body, TInt32, Pure, SL)
    }

    /**
      * Trivial Expression: _ + 0
      */
    def leftAdditionByZero(): Expression = add(Zer, Wild)

    /**
      * Trivial Expression: 0 + _
      */
    def rightAdditionByZero(): Expression = add(Wild, Zer)

    /**
      * Trivial Expression: _ - 0
      */
    def subtractionByZero(): Expression = sub(Wild, Zer)

    /**
      * Trivial Expression: x - x
      */
    def subtractionBySelf()(implicit flix: Flix): Expression = {
      val varX = mkVar()
      sub(varX, varX)
    }

    /**
      * Trivial Expression: 0 * _
      */
    def leftMultiplicationByZero(): Expression = mul(Zer, Wild)

    /**
      * Trivial Expression: _ * 0
      */
    def rightMultiplicationByZero(): Expression = mul(Wild, Zer)

    /**
      * Trivial Expression: 1 * _
      */
    def leftMultiplicationByOne(): Expression = mul(One, Wild)

    /**
      * Trivial Expression: _ * 1
      */
    def rightMultiplicationByOne(): Expression = mul(Wild, One)

    /**
      * Trivial Expression: _ / 1
      */
    def divisionByOne(): Expression = div(Wild, One)

    /**
      * Trivial Expression: x / x
      */
    def divisionBySelf()(implicit flix: Flix): Expression = {
      val varX = mkVar()
      div(varX, varX)
    }

    /**
      * Trivial Expression: "" + _
      */
    def leftConcatenateEmptyString()(implicit flix: Flix): Expression = add(EmptyString, Wild)

    /**
      * Trivial Expression: _ + ""
      */
    def rightConcatenateEmptyString()(implicit flix: Flix): Expression = add(Wild, EmptyString)

    /**
      * Trivial Expression: Nil ::: _
      */
    def leftAppendNil()(implicit flix: Flix): Expression = app(app(defn("List.append"), ListNil), Wild)

    /**
      * Trivial Expression: _ ::: Nil
      */
    def rightAppendNil()(implicit flix: Flix): Expression = app(app(defn("List.append"), Wild), ListNil)

    /**
      * Trivial Expression: List.isEmpty(_ :: _)
      */
    def listIsEmptyCons()(implicit flix: Flix): Expression = app(defn("List.isEmpty"), tag("List", "Cons", Wild))

    /**
      * Trivial Expression: List.map(x -> x, _)
      */
    def listMapIdentity()(implicit flix: Flix): Expression = app(app(defn("List.map"), identity()), Wild)

    /**
      * A list of trivial expression patterns.
      */
    def allPatterns(implicit root: Root, flix: Flix): List[Expression] = List(
      rightAdditionByZero(),
      leftAdditionByZero(),
      subtractionByZero(),
      //subtractionBySelf(), // TODO
      leftMultiplicationByZero(),
      rightMultiplicationByZero(),
      leftMultiplicationByOne(),
      rightMultiplicationByOne(),
      //divisionByOne(), // TODO
      // divisionBySelf(), // TODO
      leftConcatenateEmptyString(),
      rightConcatenateEmptyString(),
      // leftAppendNil(), // TODO
      // rightAppendNil(), // TODO
      //      listIsEmptyCons(), // TODO
      listMapIdentity()
    )

  }

  // TODO: Use set instead of list of trivial errors?

  /**
    * Finds trivial computations in the given definition `defn0`.
    */
  private def visitDef(defn0: Def)(implicit root: Root, flix: Flix): List[TrivialError] = {
    visitExp(defn0.exp)
  }

  /**
    * Finds trivial computations in the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit root: Root, flix: Flix): List[TrivialError] = matchesTrivialTemplate(exp0) ++ (exp0 match {

    case Expression.Unit(_) => Nil

    case Expression.True(_) => Nil

    case Expression.False(_) => Nil

    case Expression.Char(_, _) => Nil

    case Expression.Float32(_, _) => Nil

    case Expression.Float64(_, _) => Nil

    case Expression.Int8(_, _) => Nil

    case Expression.Int16(_, _) => Nil

    case Expression.Int32(_, _) => Nil

    case Expression.Int64(_, _) => Nil

    case Expression.BigInt(_, _) => Nil

    case Expression.Str(_, _) => Nil

    case Expression.Wild(_, _, _) => Nil

    case Expression.Var(_, _, _, _) => Nil

    case Expression.Def(_, _, _, _) => Nil

    case Expression.Eff(_, _, _, _) => Nil

    case Expression.Hole(_, _, _, _) => Nil

    case Expression.Lambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Apply(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRec(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(exp, rules, _, _, _) =>
      // Visit the match value.
      val d = visitExp(exp)

      // Visit the match rules.
      rules.foldLeft(d) {
        case (acc, MatchRule(_, guard, body)) => acc ++ visitExp(guard) ++ visitExp(body)
      }

    case Expression.Switch(rules, _, _, _) =>
      rules.foldLeft(Nil: List[TrivialError]) {
        case (acc, (cond, body)) => acc ++ visitExp(cond) ++ visitExp(body)
      }

    case Expression.Tag(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(elms, _, _, _) =>
      elms.foldLeft(Nil: List[TrivialError]) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.RecordEmpty(_, _, _) => Nil

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.ArrayLit(elms, _, _, _) =>
      elms.foldLeft(Nil: List[TrivialError]) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expression.ArrayNew(elm, len, _, _, _) =>
      visitExp(elm) ++ visitExp(len)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      visitExp(base) ++ visitExp(index)

    case Expression.ArrayLength(base, _, _, _) =>
      visitExp(base)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expression.ArraySlice(base, begin, end, _, _, _) =>
      visitExp(base) ++ visitExp(begin) ++ visitExp(end)

    case Expression.VectorLit(elms, _, _, _) =>
      elms.foldLeft(Nil: List[TrivialError]) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expression.VectorNew(elm, _, _, _, _) =>
      visitExp(elm)

    case Expression.VectorLoad(base, _, _, _, _) =>
      visitExp(base)

    case Expression.VectorStore(base, _, elm, _, _, _) =>
      visitExp(base) ++ visitExp(elm)

    case Expression.VectorLength(base, _, _, _) =>
      visitExp(base)

    case Expression.VectorSlice(base, _, _, _, _, _) =>
      visitExp(base)

    case Expression.Ref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.HandleWith(exp, bindings, _, _, _) =>
      bindings.foldLeft(visitExp(exp)) {
        case (acc, HandlerBinding(_, e)) => acc ++ visitExp(e)
      }

    case Expression.Existential(_, exp, _, _) =>
      visitExp(exp)

    case Expression.Universal(_, exp, _, _) =>
      visitExp(exp)

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _) =>
      visitExp(exp)

    case Expression.NativeConstructor(_, args, _, _, _) =>
      args.foldLeft(Nil: List[TrivialError]) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expression.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, CatchRule(_, _, body)) => acc ++ visitExp(body)
      }

    case Expression.NativeField(_, _, _, _) => Nil

    case Expression.NativeMethod(_, args, _, _, _) =>
      args.foldLeft(Nil: List[TrivialError]) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      // Visit the default expression.
      val d = default.map(visitExp).getOrElse(Nil)

      // Visit each select rule.
      rules.foldLeft(d) {
        case (acc, SelectChannelRule(_, chan, body)) => acc ++ visitExp(chan) ++ visitExp(body)
      }

    case Expression.ProcessSpawn(exp, _, _, _) =>
      visitExp(exp)

    case Expression.ProcessSleep(exp, _, _, _) =>
      visitExp(exp)

    case Expression.ProcessPanic(_, _, _, _) => Nil

    case Expression.FixpointConstraint(c, _, _, _) =>
      visitConstraint(c)

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProject(pred, exp, _, _, _) =>
      visitExp(pred.exp) ++ visitExp(exp)

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

  })

  /**
    * Finds trivial computations in the given constraint `con0`.
    */
  private def visitConstraint(con0: TypedAst.Constraint)(implicit root: Root, flix: Flix): List[TrivialError] =
    con0.body.foldLeft(visitHeadPred(con0.head)) {
      case (acc, body) => acc ++ visitBodyPred(body)
    }

  /**
    * Finds trivial computations in the given head predicate `head0`.
    */
  private def visitHeadPred(head0: TypedAst.Predicate.Head)(implicit root: Root, flix: Flix): List[TrivialError] = head0 match {
    case Head.Atom(pred, terms, _, _) =>
      terms.foldLeft(visitExp(pred.exp)) {
        case (acc, term) => acc ++ visitExp(term)
      }
  }

  /**
    * Finds trivial computations in the given body predicate `body0`.
    */
  private def visitBodyPred(body0: TypedAst.Predicate.Body)(implicit root: Root, flix: Flix): List[TrivialError] = body0 match {
    case Body.Atom(pred, _, terms, _, _) => visitExp(pred.exp)

    case Body.Filter(_, terms, _) =>
      terms.foldLeft(Nil: List[TrivialError]) {
        case (acc, term) => acc ++ visitExp(term)
      }

    case Body.Functional(_, term, _) => visitExp(term)
  }

  /**
    * Determines if given expression `exp0` is trivial.
    */
  private def matchesTrivialTemplate(exp0: TypedAst.Expression)(implicit root: Root, flix: Flix): List[TrivialError] = {
    // Check if the expression unifies with any of the trivial patterns.
    Catalog.allPatterns.foldLeft(Nil: List[TrivialError]) {
      case (acc, template) if unify(exp0, template).nonEmpty =>
        TrivialExpression(exp0.loc) :: acc
      case (acc, _) => acc
    }
  }

  // TODO: DOC
  private def unify(exp1: Expression, exp2: Expression): Option[Substitution] = (exp1, exp2) match {

    case (Expression.Unit(_), Expression.Unit(_)) => Substitution.emptyOpt

    case (Expression.True(_), Expression.True(_)) => Substitution.emptyOpt

    case (Expression.False(_), Expression.False(_)) => Substitution.emptyOpt

    case (Expression.Char(lit1, _), Expression.Char(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Float32(lit1, _), Expression.Float32(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Float64(lit1, _), Expression.Float64(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Int8(lit1, _), Expression.Int8(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Int16(lit1, _), Expression.Int16(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Int32(lit1, _), Expression.Int32(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Int64(lit1, _), Expression.Int64(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.BigInt(lit1, _), Expression.BigInt(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Str(lit1, _), Expression.Str(lit2, _)) =>
      if (lit1 != lit2) None else Substitution.emptyOpt

    case (Expression.Wild(_, _, _), _) => Substitution.emptyOpt

    case (_, Expression.Wild(_, _, _)) => Substitution.emptyOpt

    // TODO: How to deal with variables? We dont have meta variables?
    case (Expression.Var(sym1, _, _, _), Expression.Var(sym2, _, _, _)) if sym1 == sym2 => Substitution.emptyOpt

    case (Expression.Def(sym1, _, _, _), Expression.Def(sym2, _, _, _)) if sym1 == sym2 => Substitution.emptyOpt

    case (Expression.Eff(sym1, _, _, _), Expression.Eff(sym2, _, _, _)) if sym1 == sym2 => Substitution.emptyOpt

    case (Expression.Hole(sym1, _, _, _), Expression.Hole(sym2, _, _, _)) if sym1 == sym2 => Substitution.emptyOpt

    case (Expression.Lambda(fparam1, exp1, _, _, _), Expression.Lambda(fparam2, exp2, _, _, _)) =>
      // TODO: How to handle params?
      unify(exp1, exp2)

    case (Expression.Apply(exp11, exp12, _, _, _), Expression.Apply(exp21, exp22, _, _, _)) =>
      for {
        subst1 <- unify(exp11, exp21)
        subst2 <- unify(subst1(exp12), subst1(exp22))
      } yield subst2 @@ subst1



    // TODO: All cases to consider:
    //
    //    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Stm(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Switch(rules: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Tag(sym: Symbol.EnumSym, tag: String, exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordEmpty(tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordSelect(exp: TypedAst.Expression, label: String, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordExtend(label: String, value: TypedAst.Expression, rest: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordRestrict(label: String, rest: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayLit(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayNew(elm: TypedAst.Expression, len: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayLoad(base: TypedAst.Expression, index: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayLength(base: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayStore(base: TypedAst.Expression, index: TypedAst.Expression, elm: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArraySlice(base: TypedAst.Expression, beginIndex: TypedAst.Expression, endIndex: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLit(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorNew(elm: TypedAst.Expression, len: Int, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLoad(base: TypedAst.Expression, index: Int, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorStore(base: TypedAst.Expression, index: Int, elm: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLength(base: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorSlice(base: TypedAst.Expression, startIndex: Int, endIndex: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Ref(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Deref(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class HandleWith(exp: TypedAst.Expression, bindings: List[TypedAst.HandlerBinding], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Cst(TypeConstructor.Bool)
    //  }
    //
    //    case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Cst(TypeConstructor.Bool)
    //  }
    //
    //    case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Cast(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class NativeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class NativeField(field: Field, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class NativeMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class NewChannel(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class GetChannel(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class PutChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ProcessSpawn(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ProcessSleep(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ProcessPanic(msg: String, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointConstraint(c: TypedAst.Constraint, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointCompose(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointSolve(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointProject(pred: TypedAst.PredicateWithParam, exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointEntails(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression

    // TODO: Order

    case (Expression.Binary(op1, exp11, exp12, _, _, _), Expression.Binary(op2, exp21, exp22, _, _, _)) =>
      if (op1 != op2)
        None
      else
        for {
          subst1 <- unify(exp11, exp21)
          subst2 <- unify(exp12, exp22)
        } yield Substitution.empty // TODO


    case (Expression.Tag(sym1, tag1, exp1, _, _, _), Expression.Tag(sym2, tag2, exp2, _, _, _)) =>
      if (sym1 != sym2 || tag1 != tag2)
        None
      else
        for {
          subst <- unify(exp1, exp2)
        } yield Substitution.empty // TODO

    case _ => None
  }

  /**
    * Companion object of the [[Substitution]] class.
    */
  private object Substitution {
    /**
      * The empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)

    /**
      * The empty substitution wrapped in [[Some]].
      */
    val emptyOpt: Option[Substitution] = Some(Substitution.empty)

    // TODO: DOC
    def of(sym: Symbol.VarSym, exp: Expression): Substitution = Substitution(Map(sym -> exp))
  }

  /**
    * A substitution is a map from variable symbols to expressions.
    */
  private case class Substitution(m: Map[Symbol.VarSym, Expression]) {
    // TODO: Optimize for empty subst?

    /**
      * Applies `this` substitution to the given expression `exp0`.
      */
    def apply(exp0: Expression): Expression = exp0 // TODO: Incorrect

    /**
      * Applies `this` substitution to the given expressions `es`.
      */
    def apply(es: List[Expression]): List[Expression] = es map apply

    /**
      * Returns the left-biased composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      Substitution(this.m ++ that.m.filter(kv => !this.m.contains(kv._1)))
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      val m = that.m.foldLeft(Map.empty[Symbol.VarSym, Expression]) {
        case (macc, (x, t)) => macc.updated(x, this.apply(t))
      }
      Substitution(m) ++ this
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // TODOs
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Ensure consistent parameter names.

  // TODO: Introduce appropriate theorem type, probably something that holds a rewrite rule like:
  // TODO:
  // TODO: thm listIsEmptyCons[a](): Bool = \forall (x: a, xs: List[a]). List.isEmpty(x :: xs) ~~> false
  // TODO: thm leftConcatenateEmptyString(): Bool = \forall (s: Str). "" + s ~~> s
  // TODO: thm listFilterEmpty[a, b](): Bool = \forall (f: a -> b, xs: List[a]). List.isEmpty(xs) => (List.filter(f, xs) ~~> Nil)
  // TODO: law reflexive[e](⊑: (e, e) -> Bool): Bool = ∀(x: e). x ⊑ x

  // TODO: Asger had some interesting patterns, e.g. x != 'a' || x != 'b'

  // TODO: Use cases to find:

  // TODO: - List.getWithDefault(List.map(_, o), false)         --> List.exists(_)
  // TODO: - List.isEmpty(xs) && List.exists(_, xs)             --> false
  // TODO: - Option.flatMap(x => if (f(x)) Some(x) else None))  --> Option.filter(f)
  // TODO - x == x or equalities that are always or never true.

  // TODO: Compile to automaton or similar?

  // TODO: !! Could this phase not run concurrently with other phases!? We should just run all such checker phase concurrent with the rest of the entire compiler.
  // TODO: We could even start codegen while this is running.

  // TODO: Introduce annotated expression, e.g. @trivial 0 + 0
  // TODO: Introduce annotated expression: @unreachable 2 + 1, or 2 + 3 @ dead.
  // TODO: Where should these annotations go?

  // TODO: Should we also consider tricky cases such as:
  // match s with {
  // case Circle(Red) =>
  // case Circle(x) =>
  // where we know that x cannot be red, because that would have matched?
  // What about nested patterns like:
  // def main(): Int =
  //    let s = Circle(Red);
  //    match s with {
  //        case Circle(Red) => 123
  //        case Square(Blu) => match s with {
  //            case Square(Red) =>
  //        }
  //        case _ => Square(Blu)
  //    }

  // TODO: Write argument about dynamic checks/assertions and dead code.

  // TODO: Add while(true) java case to paper?

  // TODO: Ensure everything is private.

  // TODO: What about overlapping select cases?

  // TODO: A good story to tell is that in a large codebase, if you discover a bug, you can add it to the
  // TODO: repository of bug patterns and recompile everything.

  // TODO: To what extend should these patterns be imported? It seems a bit insafe if they are not available by default
  // TODO: even so, do we want some notion of scoping?

  // TODO: Maybe invite someone onboard the project to write these theorems?

  /////////////////////////////////////////////////////////////////////////////
  // Paper Notes
  /////////////////////////////////////////////////////////////////////////////

  // Papers:
  // - Finding Application Errors and Security Flaws Using PQL: a Program Query Language
  // - Using SCL to Specify and Check Design Intent in Source Code
  // - A Framework for Source Code Search using Program Patterns

  // Notes for the paper:
  // - We disallow shadowing (because its confusing in the presence of pattern matching).
  // - We disallow both implicit widening and narrowing of integers.
  // - We disallow all forms of implicit coercions.
  // - We disallow linear patterns.
  // - We treat holes (and ???) as using all local variables (but not anything else?)
  // - We implement the checker using a fork-join style monoid thingy.
  // - If we allow shadowing then that might lead to "mysterious" unused variable warnings.

  // Questions:
  // - When is an enum used? Is it enough to (a) mention its type, (b) to use it in a pat match, or (c) to actually construct a value.
  //     (What if you match on a value of that type, but use a wildcard?)
  //     (What is consistent with the Void enum and the singleton enum?)
  // - When is a predicate used? Is it enough to use it in a rule, or must it also appear in a head predicate?
  // - How do we appropriately distinguish between the effect of NewChannel and e.g. PutChannel?
  //     (How do we deal with return values that must be used, e.g. deleteFile?)

  // Bugs found:
  // - Missing @test on def testArrayLength42(): Int = let x = [[1 :: Nil], [3 :: 4 :: 5 :: Nil]]; length[x]

  // Shadowing gone wrong:
  //     case Expression.FixpointProject(pred, exp, _, _, _) =>
  //      val PredicateWithParam(sym, exp) = pred
  //      mapN(visitExp(pred.exp, env0), visitExp(exp, env0)) {
  //        case (used1, used2) => Used.of(sym) ++ used1 ++ used2
  //      }

  // Shadowing in action:
  // let childList : List[Path] = Nil;
  // let childList = childrenHelper(dirIterator, childList);

  // Count impacted test cases?

  // thm \forall f: List.filter(f, Nil) = Nil
  // false <= List.isEmpty(xs), List.nonEmpty(xs).

  // Ideas from: Using Redundancies to Find Errors

  // [Idempotent operations]: (1) Assign to self, (2) divide by itself, (3) bitwise xord, (4) bitwise and,
  // (Assignment to self could account for record update), there are also refs.

  // [Redundant Assignments]

  // [Dead Code] (early returns, so not really relevant).

  // [Redundant Conditionals]: Detects branches that are always dead.
  // Implemented as a combination of (1) integer propagation, (2) set of known predicates, and (3) bounds on integers.

  // Compile the theorems/bugpatterns to an automaton. Union could be fast.

}
