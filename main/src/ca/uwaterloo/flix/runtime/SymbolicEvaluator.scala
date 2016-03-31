package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{BinaryOperator, SimplifiedAst, UnaryOperator}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.phase.GenSym

import scala.collection.mutable

object SymbolicEvaluator {

  sealed trait Constraint

  sealed trait SymVal

  object SymVal {

    case object Unit extends SymVal

    case object True extends SymVal

    case object False extends SymVal

    case class Lambda(args: List[String], exp: Expression) extends SymVal

    case class Environment(m: Map[String, SymVal]) extends SymVal

    case class Closure(exp: Expression.Ref, cloVar: String, env: Environment) extends SymVal

    case class Tag(tag: String, value: SymVal) extends SymVal

    case class Tuple(elms: List[SymVal]) extends SymVal


  }

  case class Context(value: SymVal, env: Map[String, SymVal])


  def eval(exp0: Expression, env0: Map[String, Expression], root: SimplifiedAst.Root)(implicit genSym: GenSym): SymVal = {

    def eval(exp0: Expression, env0: mutable.Map[String, SymVal], pc: List[Constraint])(implicit genSym: GenSym): SymVal = exp0 match {
      case Expression.Unit => SymVal.Unit
      case Expression.True => SymVal.True
      case Expression.False => SymVal.False

      case Expression.Var(ident, offset, tpe, loc) => env0(ident.name)

      case Expression.ClosureVar(env, name, _, _) =>
        val SymVal.Environment(m) = env0(env.name)
        m(name.name)

      case Expression.Let(ident, _, exp1, exp2, _, _) =>
        val v1 = eval(exp1, env0, pc)
        val newEnv = env0.clone()
        newEnv += (ident.name -> v1)
        eval(exp2, newEnv, pc)

      case Expression.Ref(name, tpe, loc) =>
        val defn = root.constants(name)
        eval(defn.exp, env0, pc)

      case Expression.Apply(exp, args, _, _) =>
        val SymVal.Closure(cloExp, cloVar, cloEnv) = eval(exp, env0, pc)
        val newEnv = mutable.Map.empty[String, SymVal]
        newEnv += (cloVar -> cloEnv)
        eval(cloExp, newEnv, pc)

      case Expression.ApplyRef(name, args, _, _) =>
        val defn = root.constants(name)
        val as = args.map(a => eval(a, env0, pc))
        val newEnv = mutable.Map.empty[String, SymVal]

        for ((formal, actual) <- defn.formals zip as) {
          newEnv += (formal.ident.name -> actual)
        }
        eval(defn.exp, newEnv, pc)

      case Expression.MkClosure(lambda, cloVar, freeVars, _, _) =>
        val closureEnv = mutable.Map.empty[String, SymVal]
        for (freeVar <- freeVars) {
          closureEnv += (freeVar.name -> env0(freeVar.name))
        }
        SymVal.Closure(lambda.asInstanceOf[Expression.Ref], cloVar.name, SymVal.Environment(closureEnv.toMap))

      case Expression.MkClosureRef(ref, cloVar, freeVars, tpe, loc) =>
        // TODO: Why are there two?
        val closureEnv = mutable.Map.empty[String, SymVal]
        for (freeVar <- freeVars) {
          closureEnv += (freeVar.name -> env0(freeVar.name))
        }
        SymVal.Closure(ref, cloVar.name, SymVal.Environment(closureEnv.toMap))

      case Expression.Unary(op, exp, _, _) =>
        val v = eval(exp, env0, pc)
        op match {
          case UnaryOperator.LogicalNot => v match {
            case SymVal.True => SymVal.False
            case SymVal.False => SymVal.True
            case _ => ???
          }
        }

      case Expression.Binary(op, exp1, exp2, _, _) =>
        val v1 = eval(exp1, env0, pc)
        val v2 = eval(exp2, env0, pc)
        op match {
          case BinaryOperator.LogicalAnd => (v1, v2) match {
            case (SymVal.True, SymVal.True) => SymVal.True
            case (SymVal.False, SymVal.True) => SymVal.False
            case (SymVal.True, SymVal.False) => SymVal.False
            case (SymVal.False, SymVal.False) => SymVal.False
          }

          case BinaryOperator.LogicalOr => (v1, v2) match {
            case (SymVal.True, SymVal.True) => SymVal.True
            case (SymVal.False, SymVal.True) => SymVal.True
            case (SymVal.True, SymVal.False) => SymVal.True
            case (SymVal.False, SymVal.False) => SymVal.False
          }

          case BinaryOperator.Equal =>
            if (v1 == v2) SymVal.True else SymVal.False
        }

      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        val cond = eval(exp1, env0, pc)

        cond match {
          case SymVal.True => eval(exp2, env0, pc)
          case SymVal.False => eval(exp3, env0, pc)
          case _ =>
            println(cond)
            ???
        }

      case Expression.Tuple(elms, _, _) =>
        val es = elms map (e => eval(e, env0, pc))
        SymVal.Tuple(es)

      case Expression.CheckTag(tag, exp, _) =>
        val SymVal.Tag(tag2, _) = eval(exp, env0, pc)
        if (tag.name == tag2)
          SymVal.True
        else
          SymVal.False

      case Expression.GetTagValue(tag, exp, _, _) =>
        val SymVal.Tag(_, v) = eval(exp, env0, pc)
        v

      case Expression.GetTupleIndex(base, offset, _, _) =>
        val SymVal.Tuple(elms) = eval(base, env0, pc)
        elms(offset)

    }

    val initEnv = mutable.Map.empty[String, SymVal]
    for ((name, exp) <- env0) {
      initEnv += (name -> toSymVal(exp))
    }

    val r = eval(exp0, initEnv, Nil)
    r
  }

  def toSymVal(exp0: Expression): SymVal = exp0 match {
    case Expression.Unit => SymVal.Unit
    case Expression.Tag(enum, tag, exp, tpe, loc) =>
      SymVal.Tag(tag.name, toSymVal(exp))
  }

}
