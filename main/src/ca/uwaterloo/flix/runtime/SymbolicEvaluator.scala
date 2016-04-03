package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression.Ref
import ca.uwaterloo.flix.language.phase.GenSym
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object SymbolicEvaluator {

  sealed trait Expr

  object Expr {

    case class Var(ident: Name.Ident) extends Expr

    case class Not(e: Expr) extends Expr

    case class UnaryMinus(e: Expr) extends Expr

    case class Plus(e1: Expr, e2: Expr) extends Expr

    case class Eq(e1: Expr, e2: Expr) extends Expr

    case class Neq(e1: Expr, e2: Expr) extends Expr

  }

  sealed trait SymVal

  object SymVal {

    /**
      * An atomic symbolic variable.
      */
    case class AtomicVar(ident: Name.Ident) extends SymVal

    /**
      * The `Unit` value.
      */
    case object Unit extends SymVal

    /**
      * The `True` value.
      */
    case object True extends SymVal

    /**
      * The `False` value.
      */
    case object False extends SymVal

    /**
      * An Int32 value.
      */
    case class Int32(lit: Int) extends SymVal


    case class Tag(tag: String, value: SymVal) extends SymVal

    case class Tuple(elms: List[SymVal]) extends SymVal

    case class Environment(m: Map[String, SymVal]) extends SymVal

    case class Closure(exp: Expression.Ref, cloVar: String, env: Environment) extends SymVal


  }

  /**
    * The type of path constraints.
    */
  type PathConstraint = List[Expr]

  /**
    * The type of environments.
    */
  type Environment = mutable.Map[String, SymVal]

  def eval(exp0: Expression, env0: Map[String, Expression], root: ExecutableAst.Root)(implicit genSym: GenSym): List[(PathConstraint, SymVal)] = {

    def eval(pc0: PathConstraint, exp0: Expression, env0: Environment)(implicit genSym: GenSym): List[(PathConstraint, SymVal)] = exp0 match {
      case Expression.Unit => lift(pc0, SymVal.Unit)
      case Expression.True => lift(pc0, SymVal.True)
      case Expression.False => lift(pc0, SymVal.False)

      case Expression.Int32(i) => lift(pc0, SymVal.Int32(i))

      case Expression.Var(ident, offset, tpe, loc) => lift(pc0, env0(ident.name))

      case Expression.ClosureVar(env, name, _, _) =>
        val SymVal.Environment(m) = env0(env.name)
        lift(pc0, m(name.name))

      case Expression.Let(ident, _, exp1, exp2, _, _) =>
        eval(pc0, exp1, env0) flatMap {
          case (pc, v1) =>
            val newEnv = env0.clone()
            newEnv += (ident.name -> v1)
            eval(pc, exp2, newEnv)
        }

      case Expression.Ref(name, tpe, loc) =>
        val defn = root.constants(name)
        eval(pc0, defn.exp, env0)

      case Expression.ApplyClosure(exp, args, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Closure(cloExp, cloVar, cloEnv)) =>
            val newEnv = mutable.Map.empty[String, SymVal]
            newEnv += (cloVar -> cloEnv)
            eval(pc, cloExp, newEnv)
          case (_, v) => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      case Expression.ApplyRef(name, args, _, _) =>
        val defn = root.constants(name)
        evaln(pc0, args, env0) flatMap {
          case (pc, as) =>
            val newEnv = mutable.Map.empty[String, SymVal]
            for ((formal, actual) <- defn.formals zip as) {
              newEnv += (formal.ident.name -> actual)
            }
            eval(pc, defn.exp, newEnv)
        }

      case Expression.MkClosure(lambda, cloVar, freeVars, _, _) =>
        val closureEnv = mutable.Map.empty[String, SymVal]
        for (freeVar <- freeVars) {
          closureEnv += (freeVar.name -> env0(freeVar.name))
        }
        val cloVal = SymVal.Closure(lambda.asInstanceOf[Ref], cloVar.name, SymVal.Environment(closureEnv.toMap))
        lift(pc0, cloVal)

      /**
        * Unary Expressions.
        */
      case Expression.Unary(op, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, v) => op match {
            /**
              * Unary Not.
              */
            case UnaryOperator.LogicalNot => v match {
              case SymVal.True => lift(pc, SymVal.False)
              case SymVal.False => lift(pc, SymVal.True)
              case SymVal.AtomicVar(id) => List(
                (Expr.Var(id) :: pc, SymVal.False),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.True)
              )
              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

            /**
              * Unary Plus.
              */
            case UnaryOperator.Plus => lift(pc, v)

            /**
              * Unary Minus.
              */
            case UnaryOperator.Minus => v match {
              case SymVal.Int32(i) => lift(pc, SymVal.Int32(-i))
              case SymVal.AtomicVar(id) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.UnaryMinus(Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

            /**
              * Unary Bitwise Negate.
              */
            case UnaryOperator.BitwiseNegate => ??? // TODO

          }
        }

      /**
        * Binary Expressions.
        */
      case Expression.Binary(op, exp1, exp2, _, _) =>
        eval2(pc0, exp1, exp2, env0) flatMap {
          case (pc, (v1, v2)) => op match {

            /**
              * Binary Plus.
              */
            case BinaryOperator.Plus => (v1, v2) match {
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 + i2))
              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Plus(Expr.Var(id1), Expr.Var(id2))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
                // TODO: Could one of these be a var and the other not? I guess so
            }

            case BinaryOperator.LogicalAnd => (v1, v2) match {
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.False)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.False)
            }

            case BinaryOperator.LogicalOr => (v1, v2) match {
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.False)
            }

            case BinaryOperator.Equal => eq(pc, v1, v2)
          }
        }

      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        eval(pc0, exp1, env0) flatMap {
          case (pc, c) => c match {
            case SymVal.True => eval(pc, exp2, env0)
            case SymVal.False => eval(pc, exp3, env0)
            case _ =>
              println(c)
              ???
          }
        }

      case Expression.Tag(enum, tag, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, v) => lift(pc, SymVal.Tag(tag.name, v))
        }

      case Expression.Tuple(elms, _, _) =>
        evaln(pc0, elms, env0) flatMap {
          case (pc, es) => lift(pc, SymVal.Tuple(es))
        }

      case Expression.CheckTag(tag, exp, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Tag(tag2, _)) =>
            if (tag.name == tag2)
              lift(pc, SymVal.True)
            else
              lift(pc, SymVal.False)
        }

      case Expression.GetTagValue(tag, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Tag(_, v)) => lift(pc, v)
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      case Expression.GetTupleIndex(base, offset, _, _) =>
        eval(pc0, base, env0) flatMap {
          case (pc, SymVal.Tuple(elms)) => lift(pc, elms(offset))
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

    }

    def lift(pc: PathConstraint, v: SymVal): List[(PathConstraint, SymVal)] = List(pc -> v)

    def eq(pc0: PathConstraint, x: SymVal, y: SymVal): List[(PathConstraint, SymVal)] = (x, y) match {
      case (SymVal.AtomicVar(ident1), SymVal.AtomicVar(ident2)) => List(
        (Expr.Eq(Expr.Var(ident1), Expr.Var(ident2)) :: pc0, SymVal.True),
        (Expr.Neq(Expr.Var(ident1), Expr.Var(ident2)) :: pc0, SymVal.False)
      )

      case (SymVal.Unit, SymVal.Unit) => lift(pc0, SymVal.True)
      case (SymVal.Tag(tag1, v1), SymVal.Tag(tag2, v2)) => if (tag1 == tag2) eq(pc0, v1, v2) else lift(pc0, SymVal.False)
    }

    def eval2(pc0: PathConstraint, x: Expression, y: Expression, env0: Environment): List[(PathConstraint, (SymVal, SymVal))] =
      eval(pc0, x, env0) flatMap {
        case (pcx, vx) => eval(pcx, y, env0) map {
          case (pcy, vy) => pcy -> ((vx, vy))
        }
      }

    def evaln(pc0: PathConstraint, xs: Traversable[Expression], env0: Environment): List[(PathConstraint, List[SymVal])] = {
      /*
       * Local visitor.
       */
      def visit(pc: PathConstraint, xs: List[Expression], env: Environment): List[(PathConstraint, List[SymVal])] = xs match {
        case Nil => List((pc, Nil))
        case r :: rs => eval(pc, r, env) flatMap {
          case (pc1, v) => visit(pc1, rs, env) map {
            case (pc2, vs) => (pc2, v :: vs)
          }
        }
      }

      visit(pc0, xs.toList, env0)
    }

    def toSymVal(exp0: Expression): SymVal = exp0 match {
      case Expression.Unit => SymVal.Unit
      case Expression.Var(ident, _, _, _) => SymVal.AtomicVar(ident)
      case Expression.Tag(enum, tag, exp, tpe, loc) =>
        SymVal.Tag(tag.name, toSymVal(exp))
    }


    val initEnv = mutable.Map.empty[String, SymVal]
    for ((name, exp) <- env0) {
      initEnv += (name -> toSymVal(exp))
    }

    eval(Nil, exp0, initEnv)
  }


}
