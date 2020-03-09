package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.{BinaryOperator, TypedAst}

/**
  * Pretty printing of expressions.
  */
object PrettyExpression {

  /**
    * TypedAst.
    */
  def pretty(e0: TypedAst.Expression): String = e0 match {

    //
    //    case class Unit(loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Unit
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class True(loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Bool
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class False(loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Bool
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Char
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Float32
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Float64
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Int8
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Int16
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    case Expression.Int32(lit, _) => lit.toString

    //    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Int32
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Int64
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.BigInt
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Str
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Wild(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //  def eff: Type = Type.Pure
    //  }
    //

    case Expression.Var(sym, _, _) => sym.text

    case Expression.Def(sym, _, _) => sym.toString

    //
    //    case class Hole(sym: Symbol.HoleSym, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Lambda(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //  def eff: Type = Type.Pure
    //  }
    //
    case Expression.Lambda(fparam, exp, _, _) =>
      s"${fparam.sym.text} -> ${pretty(exp)}"

    case Expression.Apply(exp1, exp2, _, _, _) =>
      s"${pretty(exp1)}(${pretty(exp2)})"

    //    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    case Expression.Binary(op, exp1, exp2, _, _, _) => op match {
      case BinaryOperator.Plus => s"${pretty(exp1)} + ${pretty(exp2)}"
      case _ => e0.toString
    }


    //
    //    case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Stm(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case Expression.Stm(exp1, exp2, _, _, _) =>
      s"${pretty(exp1); pretty(exp2)}"

    //
    //    case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Tag(sym: Symbol.EnumSym, tag: String, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

    case Expression.Tag(_, tag, exp, _, _, _) =>
      s"$tag${pretty(exp)}"

    //
    //    case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //  def eff: Type = Type.Pure
    //  }
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
    //    case class VectorLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorNew(elm: TypedAst.Expression, len: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLoad(base: TypedAst.Expression, index: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorStore(base: TypedAst.Expression, index: Int, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorSlice(base: TypedAst.Expression, startIndex: Int, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Ref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Deref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Bool
    //
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
    //  def tpe: Type = Type.Bool
    //
    //  def eff: Type = Type.Pure
    //  }
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
    //    case class ProcessSpawn(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ProcessPanic(msg: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //  def eff: Type = Type.Pure
    //  }
    //
    //    case class FixpointCompose(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointSolve(exp: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointProject(sym: Symbol.PredSym, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointEntails(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class FixpointFold(sym: Symbol.PredSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression


    case _ => e0.toString
  }

}
