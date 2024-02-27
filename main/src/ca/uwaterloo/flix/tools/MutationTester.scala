package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.{SemanticOp, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expr, Root}
import ca.uwaterloo.flix.util.{Result, Validation}

/* TODO: think also:
 * Do we need a flix.phase anywhere ?
 * Check mutant compilation. Create 'MutantStatus' enum
 * Add statistics
 * Add reporter
 * Recursive operators mutation
*/

object MutationTester {

  private val mutators: List[ExprMutator] = List(
    ExprMutator.Incrementer,
    ExprMutator.Decrementer,
    ExprMutator.BooleanMutator,
    ExprMutator.ArithmeticUnaryMutator,
    ExprMutator.ArithmeticBinaryMutator,
    ExprMutator.ConditionalNegateMutator,
    ExprMutator.ConditionalBoundaryMutator,
  )

  def run()(implicit flix: Flix): Result[(Int, Int, Int), Int] = {
    val root = flix.getTyperAst

    var all, killed, compilationFailed = 0 // TODO: refactor

    // TODO: also need to filter library defs
    val defs = root.defs.values.filter(defn => !defn.spec.ann.isTest)

    defs.foreach {
      defn =>
        mutators.foreach {
          mutator =>
            mutator.mutate(root, defn) match {
              case Some(mutant) =>
                all += 1
                testMutant(mutant).toHardResult match {
                  case Result.Ok(testResult) => testResult match {
                    case Result.Ok(_) => // mutant was not killed
                    case Result.Err(_) => killed += 1
                  }
                  case Result.Err(_) => compilationFailed += 1
                }
              case None => // do nothing
            }
        }
    }

    Result.Ok(all, killed, compilationFailed)
  }

  private def testMutant(mutant: Root)(implicit flix: Flix) = {
    Validation.flatMapN(flix.codeGen(mutant).toHardFailure) {
      compilationResult =>
        Validation.success(Tester.run(Nil, compilationResult))
    }
  }

  private sealed trait ExprMutator {
    def mutateExpr(exp: Expr): Option[Expr]

    def mutate(root: Root, defn: Def): Option[Root] = {
      mutateExpr(defn.exp).map { mutatedExp =>
        val mutatedDef = defn.copy(exp = mutatedExp)
        root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef))
      }
    }
  }

  private object ExprMutator {
    case object Incrementer extends ExprMutator {

      override def mutateExpr(exp: Expr): Option[Expr] = exp match {
        case Expr.Cst(cst, tpe, loc) => cst match {
          case Constant.Float32(lit) => Some(Expr.Cst(Constant.Float32(lit + 1), tpe, loc))
          case Constant.Float64(lit) => Some(Expr.Cst(Constant.Float64(lit + 1), tpe, loc))
          case Constant.BigDecimal(lit) => Some(Expr.Cst(Constant.BigDecimal(lit.add(java.math.BigDecimal.ONE)), tpe, loc))
          case Constant.Int8(lit) => Some(Expr.Cst(Constant.Int8((lit + 1).toByte), tpe, loc))
          case Constant.Int16(lit) => Some(Expr.Cst(Constant.Int16((lit + 1).toShort), tpe, loc))
          case Constant.Int32(lit) => Some(Expr.Cst(Constant.Int32(lit + 1), tpe, loc))
          case Constant.Int64(lit) => Some(Expr.Cst(Constant.Int64(lit + 1), tpe, loc))
          case Constant.BigInt(lit) => Some(Expr.Cst(Constant.BigInt(lit.add(java.math.BigInteger.ONE)), tpe, loc))
          case _ => None
        }
        case _ => None
      }
    }

    case object Decrementer extends ExprMutator {

      override def mutateExpr(exp: Expr): Option[Expr] = exp match {
        case Expr.Cst(cst, tpe, loc) => cst match {
          case Constant.Float32(lit) => Some(Expr.Cst(Constant.Float32(lit - 1), tpe, loc))
          case Constant.Float64(lit) => Some(Expr.Cst(Constant.Float64(lit - 1), tpe, loc))
          case Constant.BigDecimal(lit) => Some(Expr.Cst(Constant.BigDecimal(lit.subtract(java.math.BigDecimal.ONE)), tpe, loc))
          case Constant.Int8(lit) => Some(Expr.Cst(Constant.Int8((lit - 1).toByte), tpe, loc))
          case Constant.Int16(lit) => Some(Expr.Cst(Constant.Int16((lit - 1).toShort), tpe, loc))
          case Constant.Int32(lit) => Some(Expr.Cst(Constant.Int32(lit - 1), tpe, loc))
          case Constant.Int64(lit) => Some(Expr.Cst(Constant.Int64(lit - 1), tpe, loc))
          case Constant.BigInt(lit) => Some(Expr.Cst(Constant.BigInt(lit.subtract(java.math.BigInteger.ONE)), tpe, loc))
          case _ => None
        }
        case _ => None
      }
    }

    case object BooleanMutator extends ExprMutator {

      override def mutateExpr(exp: Expr): Option[Expr] = exp match {
        case Expr.Cst(Constant.Bool(lit), tpe, loc) => Some(Expr.Cst(Constant.Bool(!lit), tpe, loc))
        case Expr.Unary(BoolOp.Not, exp, _, _, _) => Some(exp)
        case Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc) => Some(Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc))
        case Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc) => Some(Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc))
        case _ => None
      }
    }

    case object ArithmeticUnaryMutator extends ExprMutator {

      private val ops: Set[SemanticOp] = Set(
        Float32Op.Neq,
        Float64Op.Neq,
        Int8Op.Neq,
        Int16Op.Neq,
        Int32Op.Neq,
        Int64Op.Neq,
        Int8Op.Not,
        Int16Op.Not,
        Int32Op.Not,
        Int64Op.Not,
      )

      override def mutateExpr(exp: Expr): Option[Expr] = exp match {
        case Expr.Unary(sop: SemanticOp, exp, _, _, _) => if (ops.contains(sop)) Some(exp) else None
        case _ => None
      }
    }

    case object ArithmeticBinaryMutator extends ExprMutator {

      private val float32Ops: Map[Float32Op, Float32Op] = Map(
        Float32Op.Add -> Float32Op.Sub,
        Float32Op.Sub -> Float32Op.Add,
        Float32Op.Mul -> Float32Op.Div,
        Float32Op.Div -> Float32Op.Mul,
      )

      private val float64Ops: Map[Float64Op, Float64Op] = Map(
        Float64Op.Add -> Float64Op.Sub,
        Float64Op.Sub -> Float64Op.Add,
        Float64Op.Mul -> Float64Op.Div,
        Float64Op.Div -> Float64Op.Mul,
      )

      private val int8Ops: Map[Int8Op, Int8Op] = Map(
        // base
        Int8Op.Add -> Int8Op.Sub,
        Int8Op.Sub -> Int8Op.Add,
        Int8Op.Mul -> Int8Op.Div,
        Int8Op.Div -> Int8Op.Mul,
        Int8Op.Rem -> Int8Op.Mul,
        // bitwise
        Int8Op.And -> Int8Op.Or,
        Int8Op.Or -> Int8Op.And,
        Int8Op.Xor -> Int8Op.And,
        Int8Op.Shr -> Int8Op.Shl,
        Int8Op.Shl -> Int8Op.Shr,
      )

      private val int16Ops: Map[Int16Op, Int16Op] = Map(
        // base
        Int16Op.Add -> Int16Op.Sub,
        Int16Op.Sub -> Int16Op.Add,
        Int16Op.Mul -> Int16Op.Div,
        Int16Op.Div -> Int16Op.Mul,
        Int16Op.Rem -> Int16Op.Mul,
        // bitwise
        Int16Op.And -> Int16Op.Or,
        Int16Op.Or -> Int16Op.And,
        Int16Op.Xor -> Int16Op.And,
        Int16Op.Shr -> Int16Op.Shl,
        Int16Op.Shl -> Int16Op.Shr,
      )

      private val int32Ops: Map[Int32Op, Int32Op] = Map(
        // base
        Int32Op.Add -> Int32Op.Sub,
        Int32Op.Sub -> Int32Op.Add,
        Int32Op.Mul -> Int32Op.Div,
        Int32Op.Div -> Int32Op.Mul,
        Int32Op.Rem -> Int32Op.Mul,
        // bitwise
        Int32Op.And -> Int32Op.Or,
        Int32Op.Or -> Int32Op.And,
        Int32Op.Xor -> Int32Op.And,
        Int32Op.Shr -> Int32Op.Shl,
        Int32Op.Shl -> Int32Op.Shr,
      )

      private val int64Ops: Map[Int64Op, Int64Op] = Map(
        // base
        Int64Op.Add -> Int64Op.Sub,
        Int64Op.Sub -> Int64Op.Add,
        Int64Op.Mul -> Int64Op.Div,
        Int64Op.Div -> Int64Op.Mul,
        Int64Op.Rem -> Int64Op.Mul,
        // bitwise
        Int64Op.And -> Int64Op.Or,
        Int64Op.Or -> Int64Op.And,
        Int64Op.Xor -> Int64Op.And,
        Int64Op.Shr -> Int64Op.Shl,
        Int64Op.Shl -> Int64Op.Shr,
      )

      override def mutateExpr(exp: TypedAst.Expr): Option[TypedAst.Expr] = exp match {
        case Expr.Binary(sop: SemanticOp, exp1, exp2, tpe, eff, loc) =>
          sop match {
            case op: Float32Op => float32Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Float64Op => float64Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int8Op => int8Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int16Op => int16Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int32Op => int32Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int64Op => int64Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case _ => None
          }
        case _ => None
      }
    }

    case object ConditionalNegateMutator extends ExprMutator {

      private val boolOps: Map[BoolOp, BoolOp] = Map(
        BoolOp.Eq -> BoolOp.Neq,
        BoolOp.Neq -> BoolOp.Eq,
      )

      private val float32Ops: Map[Float32Op, Float32Op] = Map(
        Float32Op.Eq -> Float32Op.Neq,
        Float32Op.Neq -> Float32Op.Eq,
        Float32Op.Lt -> Float32Op.Ge,
        Float32Op.Le -> Float32Op.Gt,
        Float32Op.Gt -> Float32Op.Le,
        Float32Op.Ge -> Float32Op.Lt,
      )

      private val float64Ops: Map[Float64Op, Float64Op] = Map(
        Float64Op.Eq -> Float64Op.Neq,
        Float64Op.Neq -> Float64Op.Eq,
        Float64Op.Lt -> Float64Op.Ge,
        Float64Op.Le -> Float64Op.Gt,
        Float64Op.Gt -> Float64Op.Le,
        Float64Op.Ge -> Float64Op.Lt,
      )

      private val int8Ops: Map[Int8Op, Int8Op] = Map(
        Int8Op.Eq -> Int8Op.Neq,
        Int8Op.Neq -> Int8Op.Eq,
        Int8Op.Lt -> Int8Op.Ge,
        Int8Op.Le -> Int8Op.Gt,
        Int8Op.Gt -> Int8Op.Le,
        Int8Op.Ge -> Int8Op.Lt,
      )

      private val int16Ops: Map[Int16Op, Int16Op] = Map(
        Int16Op.Eq -> Int16Op.Neq,
        Int16Op.Neq -> Int16Op.Eq,
        Int16Op.Lt -> Int16Op.Ge,
        Int16Op.Le -> Int16Op.Gt,
        Int16Op.Gt -> Int16Op.Le,
        Int16Op.Ge -> Int16Op.Lt,
      )

      private val int32Ops: Map[Int32Op, Int32Op] = Map(
        Int32Op.Eq -> Int32Op.Neq,
        Int32Op.Neq -> Int32Op.Eq,
        Int32Op.Lt -> Int32Op.Ge,
        Int32Op.Le -> Int32Op.Gt,
        Int32Op.Gt -> Int32Op.Le,
        Int32Op.Ge -> Int32Op.Lt,
      )

      private val int64Ops: Map[Int64Op, Int64Op] = Map(
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
            case op: BoolOp => boolOps.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Float32Op => float32Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Float64Op => float64Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int8Op => int8Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int16Op => int16Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int32Op => int32Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int64Op => int64Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case _ => None
          }
        case _ => None
      }
    }

    case object ConditionalBoundaryMutator extends ExprMutator {

      private val float32Ops: Map[Float32Op, Float32Op] = Map(
        Float32Op.Lt -> Float32Op.Le,
        Float32Op.Le -> Float32Op.Lt,
        Float32Op.Gt -> Float32Op.Ge,
        Float32Op.Ge -> Float32Op.Gt,
      )

      private val float64Ops: Map[Float64Op, Float64Op] = Map(
        Float64Op.Lt -> Float64Op.Le,
        Float64Op.Le -> Float64Op.Lt,
        Float64Op.Gt -> Float64Op.Ge,
        Float64Op.Ge -> Float64Op.Gt,
      )

      private val int8Ops: Map[Int8Op, Int8Op] = Map(
        Int8Op.Lt -> Int8Op.Le,
        Int8Op.Le -> Int8Op.Lt,
        Int8Op.Gt -> Int8Op.Ge,
        Int8Op.Ge -> Int8Op.Gt,
      )

      private val int16Ops: Map[Int16Op, Int16Op] = Map(
        Int16Op.Lt -> Int16Op.Le,
        Int16Op.Le -> Int16Op.Lt,
        Int16Op.Gt -> Int16Op.Ge,
        Int16Op.Ge -> Int16Op.Gt,
      )

      private val int32Ops: Map[Int32Op, Int32Op] = Map(
        Int32Op.Lt -> Int32Op.Le,
        Int32Op.Le -> Int32Op.Lt,
        Int32Op.Gt -> Int32Op.Ge,
        Int32Op.Ge -> Int32Op.Gt,
      )

      private val int64Ops: Map[Int64Op, Int64Op] = Map(
        Int64Op.Lt -> Int64Op.Le,
        Int64Op.Le -> Int64Op.Lt,
        Int64Op.Gt -> Int64Op.Ge,
        Int64Op.Ge -> Int64Op.Gt,
      )

      override def mutateExpr(exp: TypedAst.Expr): Option[TypedAst.Expr] = exp match {
        case Expr.Binary(sop: SemanticOp, exp1, exp2, tpe, eff, loc) =>
          sop match {
            case op: Float32Op => float32Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Float64Op => float64Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int8Op => int8Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int16Op => int16Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int32Op => int32Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case op: Int64Op => int64Ops.get(op).map(mut => Expr.Binary(mut, exp1, exp2, tpe, eff, loc))
            case _ => None
          }
        case _ => None
      }
    }
  }
}
