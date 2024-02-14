package ca.uwaterloo.flix.language.phase.mutator.binary

import ca.uwaterloo.flix.language.ast.SemanticOp.{BoolOp, CharOp, Float32Op, Float64Op, Int16Op, Int32Op, Int64Op, Int8Op, StringOp}
import ca.uwaterloo.flix.language.ast.{SemanticOp, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.phase.mutator.ExprMutator

object BinaryMutator extends ExprMutator {
  private val binaryBoolOpMutators: Map[BoolOp, BoolOp] = Map(
    BoolOp.And -> BoolOp.Or,
    BoolOp.Or -> BoolOp.And,
    BoolOp.Eq -> BoolOp.Neq,
    BoolOp.Neq -> BoolOp.Eq,
  )

  private val binaryFloat32OpMutators: Map[Float32Op, Float32Op] = Map(
    Float32Op.Add -> Float32Op.Sub,
    Float32Op.Sub -> Float32Op.Add,
    Float32Op.Mul -> Float32Op.Div,
    Float32Op.Div -> Float32Op.Mul,
    Float32Op.Eq -> Float32Op.Neq,
    Float32Op.Neq -> Float32Op.Eq,
    Float32Op.Lt -> Float32Op.Ge,
    Float32Op.Le -> Float32Op.Gt,
    Float32Op.Gt -> Float32Op.Le,
    Float32Op.Ge -> Float32Op.Lt,
  )

  private val binaryFloat64OpMutators: Map[Float64Op, Float64Op] = Map(
    Float64Op.Add -> Float64Op.Sub,
    Float64Op.Sub -> Float64Op.Add,
    Float64Op.Mul -> Float64Op.Div,
    Float64Op.Div -> Float64Op.Mul,
    Float64Op.Eq -> Float64Op.Neq,
    Float64Op.Neq -> Float64Op.Eq,
    Float64Op.Lt -> Float64Op.Ge,
    Float64Op.Le -> Float64Op.Gt,
    Float64Op.Gt -> Float64Op.Le,
    Float64Op.Ge -> Float64Op.Lt,
  )

  private val binaryInt8OpMutators: Map[Int8Op, Int8Op] = Map(
    Int8Op.Add -> Int8Op.Sub,
    Int8Op.Sub -> Int8Op.Add,
    Int8Op.Mul -> Int8Op.Div,
    Int8Op.Div -> Int8Op.Mul,
    Int8Op.Rem -> Int8Op.Mul,
    Int8Op.And -> Int8Op.Or,
    Int8Op.Or -> Int8Op.And,
    Int8Op.Xor -> Int8Op.And,
    Int8Op.Shr -> Int8Op.Shl,
    Int8Op.Shl -> Int8Op.Shr,
    Int8Op.Eq -> Int8Op.Neq,
    Int8Op.Neq -> Int8Op.Eq,
    Int8Op.Lt -> Int8Op.Ge,
    Int8Op.Le -> Int8Op.Gt,
    Int8Op.Gt -> Int8Op.Le,
    Int8Op.Ge -> Int8Op.Lt,
  )

  private val binaryInt16OpMutators: Map[Int16Op, Int16Op] = Map(
    Int16Op.Add -> Int16Op.Sub,
    Int16Op.Sub -> Int16Op.Add,
    Int16Op.Mul -> Int16Op.Div,
    Int16Op.Div -> Int16Op.Mul,
    Int16Op.Rem -> Int16Op.Mul,
    Int16Op.And -> Int16Op.Or,
    Int16Op.Or -> Int16Op.And,
    Int16Op.Xor -> Int16Op.And,
    Int16Op.Shr -> Int16Op.Shl,
    Int16Op.Shl -> Int16Op.Shr,
    Int16Op.Eq -> Int16Op.Neq,
    Int16Op.Neq -> Int16Op.Eq,
    Int16Op.Lt -> Int16Op.Ge,
    Int16Op.Le -> Int16Op.Gt,
    Int16Op.Gt -> Int16Op.Le,
    Int16Op.Ge -> Int16Op.Lt,
  )

  private val binaryInt32OpMutators: Map[Int32Op, Int32Op] = Map(
    Int32Op.Add -> Int32Op.Sub,
    Int32Op.Sub -> Int32Op.Add,
    Int32Op.Mul -> Int32Op.Div,
    Int32Op.Div -> Int32Op.Mul,
    Int32Op.Rem -> Int32Op.Mul,
    Int32Op.And -> Int32Op.Or,
    Int32Op.Or -> Int32Op.And,
    Int32Op.Xor -> Int32Op.And,
    Int32Op.Shr -> Int32Op.Shl,
    Int32Op.Shl -> Int32Op.Shr,
    Int32Op.Eq -> Int32Op.Neq,
    Int32Op.Neq -> Int32Op.Eq,
    Int32Op.Lt -> Int32Op.Ge,
    Int32Op.Le -> Int32Op.Gt,
    Int32Op.Gt -> Int32Op.Le,
    Int32Op.Ge -> Int32Op.Lt,
  )

  private val binaryInt64OpMutators: Map[Int64Op, Int64Op] = Map(
    Int64Op.Add -> Int64Op.Sub,
    Int64Op.Sub -> Int64Op.Add,
    Int64Op.Mul -> Int64Op.Div,
    Int64Op.Div -> Int64Op.Mul,
    Int64Op.Rem -> Int64Op.Mul,
    Int64Op.And -> Int64Op.Or,
    Int64Op.Or -> Int64Op.And,
    Int64Op.Xor -> Int64Op.And,
    Int64Op.Shr -> Int64Op.Shl,
    Int64Op.Shl -> Int64Op.Shr,
    Int64Op.Eq -> Int64Op.Neq,
    Int64Op.Neq -> Int64Op.Eq,
    Int64Op.Lt -> Int64Op.Ge,
    Int64Op.Le -> Int64Op.Gt,
    Int64Op.Gt -> Int64Op.Le,
    Int64Op.Ge -> Int64Op.Lt,
  )

  override def mutateExpr(exp: TypedAst.Expr): Option[TypedAst.Expr] = exp match {
    case Expr.Binary(sop: SemanticOp, exp1, exp2, tpe, eff, loc) =>
      sop match {
        // TODO: does it make sense to mutate CharOp, StringOp ?
        case op: BoolOp => binaryBoolOpMutators.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
        case op: CharOp => ???
        case op: Float32Op => binaryFloat32OpMutators.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
        case op: Float64Op => binaryFloat64OpMutators.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
        case op: Int8Op => binaryInt8OpMutators.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
        case op: Int16Op => binaryInt16OpMutators.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
        case op: Int32Op => binaryInt32OpMutators.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
        case op: Int64Op => binaryInt64OpMutators.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
        case op: StringOp => ???
      }
  }
}
