package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SemanticOperator, SemanticOperatorOps, Type, TypeConstructor, TypedAst, UnaryOperator}

/**
  * Pretty printing of expressions.
  */
object PrettyExpression {

  /**
    * TypedAst.
    */
  def pretty(e0: TypedAst.Expression): String = e0 match {

    case Expression.Unit(_) => "()"

    case Expression.True(_) => "true"

    case Expression.False(_) => "false"

    case Expression.Char(lit, _) => s"'$lit'"

    case Expression.Float32(lit, _) => s"${lit}f32"

    case Expression.Float64(lit, _) => s"$lit"

    case Expression.Int8(lit, _) => s"${lit}i8"

    case Expression.Int16(lit, _) => s"${lit}i16"

    case Expression.Int32(lit, _) => s"$lit"

    case Expression.Int64(lit, _) => s"${lit}i64"

    case Expression.BigInt(lit, _) => s"${lit}ii"

    case Expression.Str(lit, _) => "\"" + lit + "\""

    case Expression.Wild(_, _) => "_"

    case Expression.Var(sym, _, _) => sym.text

    case Expression.Def(sym, _, _) => sym.toString

    case Expression.Hole(sym, _, _) => s"?${sym.name}"

    case Expression.Lambda(fparam, exp, _, _) =>
      s"${fparam.sym.text} -> ${pretty(exp)}"

    case Expression.Apply(exp, exps, _, _, _, _) =>
      s"${pretty(exp)}(${exps.map(pretty).mkString(", ")})"

    case Expression.Unary(sop, exp, _, _, _, _) =>
      val op = SemanticOperatorOps.toUnaryOp(sop)
      op match {
        case UnaryOperator.LogicalNot => s"not ${pretty(exp)}"
        case UnaryOperator.Plus => s"+${pretty(exp)}"
        case UnaryOperator.Minus => s"-${pretty(exp)}"
        case UnaryOperator.BitwiseNegate => s"~~~${pretty(exp)}"
    }

    case Expression.Binary(sop, exp1, exp2, _, _, _, _) =>
      val op = SemanticOperatorOps.toBinaryOp(sop)
      op match {
        case BinaryOperator.Plus => s"${pretty(exp1)} + ${pretty(exp2)}"
        case BinaryOperator.Minus => s"${pretty(exp1)} - ${pretty(exp2)}"
        case BinaryOperator.Times => s"${pretty(exp1)} * ${pretty(exp2)}"
        case BinaryOperator.Divide => s"${pretty(exp1)} / ${pretty(exp2)}"
        case BinaryOperator.Remainder => s"${pretty(exp1)} % ${pretty(exp2)}"
        case BinaryOperator.Exponentiate => s"${pretty(exp1)} ** ${pretty(exp2)}"
        case BinaryOperator.Less => s"${pretty(exp1)} < ${pretty(exp2)}"
        case BinaryOperator.LessEqual => s"${pretty(exp1)} <= ${pretty(exp2)}"
        case BinaryOperator.Greater => s"${pretty(exp1)} > ${pretty(exp2)}"
        case BinaryOperator.GreaterEqual => s"${pretty(exp1)} >= ${pretty(exp2)}"
        case BinaryOperator.Equal => s"${pretty(exp1)} == ${pretty(exp2)}"
        case BinaryOperator.NotEqual => s"${pretty(exp1)} != ${pretty(exp2)}"
        case BinaryOperator.Spaceship => s"${pretty(exp1)} <=> ${pretty(exp2)}"
        case BinaryOperator.LogicalAnd => s"${pretty(exp1)} and ${pretty(exp2)}"
        case BinaryOperator.LogicalOr => s"${pretty(exp1)} or ${pretty(exp2)}"
        case BinaryOperator.BitwiseAnd => s"${pretty(exp1)} &&& ${pretty(exp2)}"
        case BinaryOperator.BitwiseOr => s"${pretty(exp1)} ||| ${pretty(exp2)}"
        case BinaryOperator.BitwiseXor => s"${pretty(exp1)} ^^^ ${pretty(exp2)}"
        case BinaryOperator.BitwiseLeftShift => s"${pretty(exp1)} <<< ${pretty(exp2)}"
        case BinaryOperator.BitwiseRightShift => s"${pretty(exp1)} >>> ${pretty(exp2)}"
      case _ => e0.toString
    }

    //
    //    case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      s"${pretty(exp1); pretty(exp2)}"

    case Expression.Discard(exp, _, _, _) =>
      s"discard ${pretty(exp)}"

    //
    //    case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //

    case Expression.Tag(_, tag, exp, _, _, _, _) => exp.tpe.typeConstructor match {
      case Some(TypeConstructor.Unit) => tag.name
      case _ => s"${tag.name}${pretty(exp)}"
    }

    case Expression.Tuple(elms, _, _, _, _) =>
      s"(${elms.map(pretty).mkString(", ")})"

    //    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordSelect(exp: TypedAst.Expression, label: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordExtend(label: String, value: TypedAst.Expression, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordRestrict(label: String, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayNew(elm: TypedAst.Expression, len: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayLoad(base: TypedAst.Expression, index: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayStore(base: TypedAst.Expression, index: TypedAst.Expression, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArraySlice(base: TypedAst.Expression, beginIndex: TypedAst.Expression, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Ref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Deref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Cast(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class InvokeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class InvokeMethod(method: Method, exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class InvokeStaticMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class GetField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class PutField(field: Field, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class PutStaticField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class NewChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class GetChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class PutChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //

    case Expression.Spawn(exp, _, _, _, _) =>
      s"spawn ${pretty(exp)}"

    case _ => e0.toString
  }

}
