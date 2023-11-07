package ca.uwaterloo.flix.language.phase.mutator.arithmetic

import ca.uwaterloo.flix.language.ast.SemanticOp.{Float32Op, Float64Op, Int16Op, Int32Op, Int64Op, Int8Op}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object ArithBinaryMutator extends ExprMutator {
  private val binaryFloat32OpMutants: Map[Float32Op, Float32Op] = Map(
    Float32Op.Add -> Float32Op.Sub,
    Float32Op.Sub -> Float32Op.Add,
    Float32Op.Mul -> Float32Op.Div,
    Float32Op.Div -> Float32Op.Mul,
  )

  private val binaryFloat64OpMutants: Map[Float64Op, Float64Op] = Map(
    Float64Op.Add -> Float64Op.Sub,
    Float64Op.Sub -> Float64Op.Add,
    Float64Op.Mul -> Float64Op.Div,
    Float64Op.Div -> Float64Op.Mul,
  )

  private val binaryInt8OpMutants: Map[Int8Op, Int8Op] = Map(
    Int8Op.Add -> Int8Op.Sub,
    Int8Op.Sub -> Int8Op.Add,
    Int8Op.Mul -> Int8Op.Div,
    Int8Op.Div -> Int8Op.Mul,
    Int8Op.Rem -> Int8Op.Mul,
  )

  private val binaryInt16OpMutants: Map[Int16Op, Int16Op] = Map(
    Int16Op.Add -> Int16Op.Sub,
    Int16Op.Sub -> Int16Op.Add,
    Int16Op.Mul -> Int16Op.Div,
    Int16Op.Div -> Int16Op.Mul,
    Int16Op.Rem -> Int16Op.Mul,
  )

  private val binaryInt32OpMutants: Map[Int32Op, Int32Op] = Map(
    Int32Op.Add -> Int32Op.Sub,
    Int32Op.Sub -> Int32Op.Add,
    Int32Op.Mul -> Int32Op.Div,
    Int32Op.Div -> Int32Op.Mul,
    Int32Op.Rem -> Int32Op.Mul,
  )

  private val binaryInt64OpMutants: Map[Int64Op, Int64Op] = Map(
    Int64Op.Add -> Int64Op.Sub,
    Int64Op.Sub -> Int64Op.Add,
    Int64Op.Mul -> Int64Op.Div,
    Int64Op.Div -> Int64Op.Mul,
    Int64Op.Rem -> Int64Op.Mul,
  )

  override def mutateExpr(exp: TypedAst.Expr): Option[TypedAst.Expr] = exp match {
    case Expr.Binary(sop: Float32Op, exp1, exp2, tpe, eff, loc) => sop match {
      case Float32Op.Add | Float32Op.Sub | Float32Op.Mul | Float32Op.Div =>
        Some(Expr.Binary(binaryFloat32OpMutants(sop), exp1, exp2, tpe, eff, loc))
      case _ => None
    }
    case Expr.Binary(sop: Float64Op, exp1, exp2, tpe, eff, loc) => sop match {
      case Float64Op.Add | Float64Op.Sub | Float64Op.Mul | Float64Op.Div =>
        Some(Expr.Binary(binaryFloat64OpMutants(sop), exp1, exp2, tpe, eff, loc))
      case _ => None
    }
    case Expr.Binary(sop: Int8Op, exp1, exp2, tpe, eff, loc) => sop match {
      case Int8Op.Add | Int8Op.Sub | Int8Op.Mul | Int8Op.Div | Int8Op.Rem =>
        Some(Expr.Binary(binaryInt8OpMutants(sop), exp1, exp2, tpe, eff, loc))
      case _ => None
    }
    case Expr.Binary(sop: Int16Op, exp1, exp2, tpe, eff, loc) => sop match {
      case Int16Op.Add | Int16Op.Sub | Int16Op.Mul | Int16Op.Div | Int16Op.Rem =>
        Some(Expr.Binary(binaryInt16OpMutants(sop), exp1, exp2, tpe, eff, loc))
      case _ => None
    }
    case Expr.Binary(sop: Int32Op, exp1, exp2, tpe, eff, loc) => sop match {
      case Int32Op.Add | Int32Op.Sub | Int32Op.Mul | Int32Op.Div | Int32Op.Rem =>
        Some(Expr.Binary(binaryInt32OpMutants(sop), exp1, exp2, tpe, eff, loc))
      case _ => None
    }
    case Expr.Binary(sop: Int64Op, exp1, exp2, tpe, eff, loc) => sop match {
      case Int64Op.Add | Int64Op.Sub | Int64Op.Mul | Int64Op.Div | Int64Op.Rem =>
        Some(Expr.Binary(binaryInt64OpMutants(sop), exp1, exp2, tpe, eff, loc))
      case _ => None
    }
    case _ => None
  }
}
