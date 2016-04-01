package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression.Ref
import ca.uwaterloo.flix.language.phase.GenSym
import ca.uwaterloo.flix.runtime.SymbolicEvaluator.SymVal.Closure

import scala.collection.mutable

object SymbolicEvaluator {

  sealed trait Constraint

  object Constraint {

    case class Plus(id1: Name.Ident, id2: Name.Ident) extends Constraint

    case class Eq(id1: Name.Ident, id2: Name.Ident) extends Constraint

    case class Neq(id1: Name.Ident, id2: Name.Ident) extends Constraint

  }

  sealed trait SymVal

  object SymVal {

    case object Unit extends SymVal

    case object True extends SymVal

    case object False extends SymVal

    case class Int32(lit: Int) extends SymVal

    case class AtomicVar(ident: Name.Ident) extends SymVal

    case class Environment(m: Map[String, SymVal]) extends SymVal

    case class Closure(exp: Expression.Ref, cloVar: String, env: Environment) extends SymVal

    case class Tag(tag: String, value: SymVal) extends SymVal

    case class Tuple(elms: List[SymVal]) extends SymVal

  }

  /**
    * The type of path constraints.
    */
  type PathConstraint = List[Constraint]

  /**
    * The type of environments.
    */
  type Environment = mutable.Map[String, SymVal]

  def eval(exp0: Expression, env0: Map[String, Expression], root: ExecutableAst.Root)(implicit genSym: GenSym): SymVal = {

    // TODO: make pc first arg
    def eval(exp0: Expression, env0: Environment, pc0: PathConstraint)(implicit genSym: GenSym): List[(PathConstraint, SymVal)] = exp0 match {
      case Expression.Unit => lift(pc0, SymVal.Unit)
      case Expression.True => lift(pc0, SymVal.True)
      case Expression.False => lift(pc0, SymVal.False)

      case Expression.Int32(i) => lift(pc0, SymVal.Int32(i))

      case Expression.Var(ident, offset, tpe, loc) => lift(pc0, env0(ident.name))

      case Expression.ClosureVar(env, name, _, _) =>
        val SymVal.Environment(m) = env0(env.name)
        lift(pc0, m(name.name))

      case Expression.Let(ident, _, exp1, exp2, _, _) =>
        eval(exp1, env0, pc0) flatMap {
          case (pc, v1) =>
            val newEnv = env0.clone()
            newEnv += (ident.name -> v1)
            eval(exp2, newEnv, pc)
        }

      case Expression.Ref(name, tpe, loc) =>
        val defn = root.constants(name)
        eval(defn.exp, env0, pc0)

      case Expression.ApplyClosure(exp, args, _, _) =>
        eval(exp, env0, pc0) flatMap {
          case (pc, SymVal.Closure(cloExp, cloVar, cloEnv)) =>
            val newEnv = mutable.Map.empty[String, SymVal]
            newEnv += (cloVar -> cloEnv)
            eval(cloExp, newEnv, pc)
        }

      case Expression.ApplyRef(name, args, _, _) =>
        val defn = root.constants(name)
        evaln(pc0, args, env0) flatMap {
          case (pc, as) =>
            val newEnv = mutable.Map.empty[String, SymVal]
            for ((formal, actual) <- defn.formals zip as) {
              newEnv += (formal.ident.name -> actual)
            }
            eval(defn.exp, newEnv, pc)
        }

      case Expression.MkClosure(lambda, cloVar, freeVars, _, _) =>
        val closureEnv = mutable.Map.empty[String, SymVal]
        for (freeVar <- freeVars) {
          closureEnv += (freeVar.name -> env0(freeVar.name))
        }
        val cloVal = SymVal.Closure(lambda.asInstanceOf[Ref], cloVar.name, SymVal.Environment(closureEnv.toMap))
        lift(pc0, cloVal)

      case Expression.Unary(op, exp, _, _) =>
        eval(exp, env0, pc0) flatMap {
          case (pc, v) => op match {
            case UnaryOperator.LogicalNot => v match {
              case SymVal.True => lift(pc, SymVal.False)
              case SymVal.False => lift(pc, SymVal.True)
              case _ => ??? // TODO
            }
          }
        }

      case Expression.Binary(op, exp1, exp2, _, _) =>
        eval2(pc0, exp1, exp2, env0) flatMap {
          case (pc, (v1, v2)) => op match {

//            case BinaryOperator.Plus => (v1, v2) match {
//              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) =>
//                val newVar = genSym.fresh2()
//                val newPC = Constraint.Eq(newVar, Constraint.Plus(id1, id2)) :: pc
//                lift(newPC, SymVal.AtomicVar(newVar))
//
//              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 + i2))
//            }

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
        eval(exp1, env0, pc0) flatMap {
          case (pc, c) => c match {
            case SymVal.True => eval(exp2, env0, pc)
            case SymVal.False => eval(exp3, env0, pc)
            case _ =>
              println(c)
              ???
          }
        }

      case Expression.Tag(enum, tag, exp, _, _) =>
        eval(exp, env0, pc0) flatMap {
          case (pc, v) => lift(pc, SymVal.Tag(tag.name, v))
        }

      case Expression.Tuple(elms, _, _) =>
        evaln(pc0, elms, env0) flatMap {
          case (pc, es) => lift(pc, SymVal.Tuple(es))
        }

      case Expression.CheckTag(tag, exp, _) =>
        eval(exp, env0, pc0) flatMap {
          case (pc, SymVal.Tag(tag2, _)) =>
            if (tag.name == tag2)
              lift(pc, SymVal.True)
            else
              lift(pc, SymVal.False)
        }

      case Expression.GetTagValue(tag, exp, _, _) =>
        eval(exp, env0, pc0) flatMap {
          case (pc, SymVal.Tag(_, v)) => lift(pc, v)
        }

      case Expression.GetTupleIndex(base, offset, _, _) =>
        eval(base, env0, pc0) flatMap {
          case (pc, SymVal.Tuple(elms)) => lift(pc, elms(offset))
        }

    }

    def lift(pc: PathConstraint, v: SymVal): List[(PathConstraint, SymVal)] = List(pc -> v)

    def eq(pc0: PathConstraint, x: SymVal, y: SymVal): List[(PathConstraint, SymVal)] = (x, y) match {
      case (SymVal.AtomicVar(ident1), SymVal.AtomicVar(ident2)) => List(
        (Constraint.Eq(ident1, ident2) :: pc0, SymVal.True),
        (Constraint.Neq(ident1, ident2) :: pc0, SymVal.False)
      )

      case (SymVal.Unit, SymVal.Unit) => lift(pc0, SymVal.True)
      case (SymVal.Tag(tag1, v1), SymVal.Tag(tag2, v2)) => if (tag1 == tag2) eq(pc0, v1, v2) else lift(pc0, SymVal.False)
    }

    def eval2(pc0: PathConstraint, x: Expression, y: Expression, env0: Environment): List[(PathConstraint, (SymVal, SymVal))] =
      eval(x, env0, pc0) flatMap {
        case (pcx, vx) => eval(y, env0, pcx) map {
          case (pcy, vy) => pcy -> ((vx, vy))
        }
      }

    def evaln(pc0: PathConstraint, xs: Traversable[Expression], env0: Environment): List[(PathConstraint, List[SymVal])] = {
      /*
       * Local visitor.
       */
      def visit(pc: PathConstraint, xs: List[Expression], env: Environment): List[(PathConstraint, List[SymVal])] = xs match {
        case Nil => List((pc, Nil))
        case r :: rs => eval(r, env, pc) flatMap {
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

    val res = eval(exp0, initEnv, Nil)
    res.head._2 // TODO:
  }


}
