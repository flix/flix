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

    case class Int8(lit: Byte) extends Expr

    case class Int16(lit: Short) extends Expr

    case class Int32(lit: Int) extends Expr

    case class Int64(lit: Long) extends Expr

    case class Var(ident: Name.Ident) extends Expr

    case class Not(e: Expr) extends Expr

    case class Conj(e1: Expr, e2: Expr) extends Expr

    case class Disj(e1: Expr, e2: Expr) extends Expr

    case class Implication(e1: Expr, e2: Expr) extends Expr

    case class Bicondition(e1: Expr, e2: Expr) extends Expr

    case class UnaryMinus(e: Expr) extends Expr

    case class Plus(e1: Expr, e2: Expr) extends Expr

    case class Minus(e1: Expr, e2: Expr) extends Expr

    case class Times(e1: Expr, e2: Expr) extends Expr

    case class Divide(e1: Expr, e2: Expr) extends Expr

    case class Modulo(e1: Expr, e2: Expr) extends Expr

    case class BitwiseAnd(e1: Expr, e2: Expr) extends Expr

    case class BitwiseOr(e1: Expr, e2: Expr) extends Expr

    case class BitwiseXor(e1: Expr, e2: Expr) extends Expr

    case class BitwiseLeftShift(e1: Expr, e2: Expr) extends Expr

    case class BitwiseRightShift(e1: Expr, e2: Expr) extends Expr

    case class Eq(e1: Expr, e2: Expr) extends Expr

    case class Neq(e1: Expr, e2: Expr) extends Expr


  }

  /**
    * Symbolic Values.
    *
    * A symbolic value is like a regular value but allows atomic variables.
    */
  sealed trait SymVal

  object SymVal {

    /**
      * An atomic symbolic variable.
      *
      * @param ident the identifier.
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
      * A Char value.
      */
    case class Char(lit: Int) extends SymVal

    /**
      * A Float32 value.
      */
    case class Float32(lit: Float) extends SymVal

    /**
      * A Float64 value.
      */
    case class Float64(lit: Double) extends SymVal

    /**
      * An Int8 value.
      *
      * @param lit the int literal.
      */
    case class Int8(lit: Byte) extends SymVal

    /**
      * An Int16 value.
      *
      * @param lit the int literal.
      */
    case class Int16(lit: Short) extends SymVal

    /**
      * An Int32 value.
      *
      * @param lit the int literal.
      */
    case class Int32(lit: Int) extends SymVal

    /**
      * An Int64 value.
      *
      * @param lit the int literal.
      */
    case class Int64(lit: Long) extends SymVal

    /**
      * A String value.
      *
      * @param lit the int literal.
      */
    case class Str(lit: String) extends SymVal

    /**
      * A tag value.
      *
      * @param tag   the tag name.
      * @param value the tagged value.
      */
    case class Tag(tag: String, value: SymVal) extends SymVal

    /**
      * A tuple value.
      *
      * @param elms the elements of the tuple.
      */
    case class Tuple(elms: List[SymVal]) extends SymVal

    /**
      * A closure value.
      *
      * @param exp    the nexpression of the closure.
      * @param cloVar the name of the closure variable.
      * @param env    the closure environment.
      */
    case class Closure(exp: Expression, cloVar: String, env: Environment) extends SymVal

    /**
      * An environment for closure variables.
      *
      * @param m the map from closure variables to symbolic values.
      */
    case class Environment(m: Map[String, SymVal]) extends SymVal

    /**
      * A user error value.
      */
    case class UserError(loc: SourceLocation) extends SymVal

    /**
      * A match error value.
      */
    case class MatchError(loc: SourceLocation) extends SymVal

    /**
      * A switch error value.
      */
    case class SwitchError(loc: SourceLocation) extends SymVal

  }

  /**
    * The type of path constraints.
    */
  type PathConstraint = List[Expr]

  /**
    * The type of environments.
    */
  type Environment = mutable.Map[String, SymVal] // TODO: Consider immutable map

  // TODO: Consider ContextType

  /**
    * Evaluates the given expression `exp0` under the given environment `env0`.
    */
  def eval(exp0: Expression, env0: Map[String, Expression], root: ExecutableAst.Root)(implicit genSym: GenSym): List[(PathConstraint, SymVal)] = {
    /*
      * Local visitor.
      */
    def eval(pc0: PathConstraint, exp0: Expression, env0: Environment)(implicit genSym: GenSym): List[(PathConstraint, SymVal)] = exp0 match {
      /**
        * Unit.
        */
      case Expression.Unit => lift(pc0, SymVal.Unit)

      /**
        * True.
        */
      case Expression.True => lift(pc0, SymVal.True)

      /**
        * False.
        */
      case Expression.False => lift(pc0, SymVal.False)

      /**
        * Char.
        */
      case Expression.Char(lit) => lift(pc0, SymVal.Char(lit))

      /**
        * Float32.
        */
      case Expression.Float32(lit) => lift(pc0, SymVal.Float32(lit))

      /**
        * Float64.
        */
      case Expression.Float64(lit) => lift(pc0, SymVal.Float64(lit))

      /**
        * Int8.
        */
      case Expression.Int8(lit) => lift(pc0, SymVal.Int8(lit))

      /**
        * Int16.
        */
      case Expression.Int16(lit) => lift(pc0, SymVal.Int16(lit))

      /**
        * Int32.
        */
      case Expression.Int32(lit) => lift(pc0, SymVal.Int32(lit))

      /**
        * Int64.
        */
      case Expression.Int64(lit) => lift(pc0, SymVal.Int64(lit))

      /**
        * Str.
        */
      case Expression.Str(lit) => lift(pc0, SymVal.Str(lit))

      /**
        * Local Variable.
        */
      case Expression.Var(ident, _, tpe, loc) => lift(pc0, env0(ident.name))

      /**
        * Closure Variable.
        */
      case Expression.ClosureVar(env, name, _, _) =>
        // Lookup the closure environment.
        val SymVal.Environment(m) = env0(env.name)
        // Lookup the variable in the closure environment.
        lift(pc0, m(name.name))

      /**
        * Reference.
        */
      case Expression.Ref(name, tpe, loc) =>
        // Lookup and evaluate the definition.
        root.constants.get(name) match {
          case None => throw InternalCompilerException(s"Type Error: Unresolved reference '$name'.")
          case Some(defn) => eval(pc0, defn.exp, env0)
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

      case Expression.ApplyClosure(exp, args, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Closure(cloExp, cloVar, cloEnv)) =>
            val newEnv = mutable.Map.empty[String, SymVal]
            newEnv += (cloVar -> cloEnv)
            eval(pc, cloExp, newEnv)
          case (_, v) => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }


      case Expression.MkClosure(lambda, cloVar, freeVars, _, _) =>
        val closureEnv = mutable.Map.empty[String, SymVal]
        for (freeVar <- freeVars) {
          closureEnv += (freeVar.name -> env0(freeVar.name))
        }
        val cloVal = SymVal.Closure(lambda.asInstanceOf[Ref], cloVar.name, SymVal.Environment(closureEnv.toMap))
        lift(pc0, cloVal)

      /**
        * Unary.
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

            //            /**
            //              * Unary Bitwise Negate.
            //              */
            //            case UnaryOperator.BitwiseNegate => ??? // TODO

          }
        }

      /**
        * Binary.
        */
      case Expression.Binary(op, exp1, exp2, _, _) =>
        eval2(pc0, exp1, exp2, env0) flatMap {
          case (pc, (v1, v2)) => op match {

            /**
              * Binary Plus.
              */
            case BinaryOperator.Plus => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 + i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 + i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 + i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 + i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Plus(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Plus(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 + $v2'.")
            }

            /**
              * Binary Minus.
              */
            case BinaryOperator.Minus => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 - i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 - i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 - i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 - i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Minus(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Minus(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 - $v2'.")
            }

            /**
              * Binary Times.
              */
            case BinaryOperator.Times => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 * i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 * i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 * i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 * i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Times(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Times(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 * $v2'.")
            }

            /**
              * Binary Divide.
              */
            case BinaryOperator.Divide => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 / i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 / i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 / i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 / i2))

              // Symbolic semantics.
                // TODO: So we could simplify this down to just one case? Just call toExpr on each v1 and v2.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Divide(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Divide(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 * $v2'.")
            }

            /**
              * Binary Modulo.
              */
            case BinaryOperator.Modulo => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 % i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 % i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 % i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 % i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Modulo(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.Modulo(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 * $v2'.")
            }

            /**
              * Logical And.
              */
            case BinaryOperator.LogicalAnd => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.False)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => List(
                (Expr.Var(id) :: pc, SymVal.True),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.True) => List(
                (Expr.Var(id) :: pc, SymVal.True),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.False)
              )

              case (SymVal.False, SymVal.AtomicVar(id)) => lift(pc, SymVal.False)
              case (SymVal.AtomicVar(id), SymVal.False) => lift(pc, SymVal.False)

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (Expr.Conj(Expr.Var(id1), Expr.Var(id2)) :: pc, SymVal.True),
                (Expr.Not(Expr.Conj(Expr.Var(id1), Expr.Var(id2))) :: pc, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 && $v2'.")
            }

            /**
              * Logical Or.
              */
            case BinaryOperator.LogicalOr => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.False)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => lift(pc, SymVal.True)
              case (SymVal.AtomicVar(id), SymVal.True) => lift(pc, SymVal.True)

              case (SymVal.False, SymVal.AtomicVar(id)) => List(
                (Expr.Var(id) :: pc, SymVal.True),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.False) => List(
                (Expr.Var(id) :: pc, SymVal.True),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.False)
              )

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (Expr.Disj(Expr.Var(id1), Expr.Var(id2)) :: pc, SymVal.True),
                (Expr.Not(Expr.Disj(Expr.Var(id1), Expr.Var(id2))) :: pc, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 || $v2'.")
            }

            /**
              * Logical Implication.
              */
            case BinaryOperator.Implication => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.True)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => List(
                (Expr.Var(id) :: pc, SymVal.True),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.False)
              )
              case (SymVal.False, SymVal.AtomicVar(id)) => lift(pc, SymVal.True)

              case (SymVal.AtomicVar(id), SymVal.False) => List(
                (Expr.Var(id) :: pc, SymVal.False),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.True)
              )
              case (SymVal.AtomicVar(id), SymVal.True) => lift(pc, SymVal.True)

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (Expr.Implication(Expr.Var(id1), Expr.Var(id2)) :: pc, SymVal.True),
                (Expr.Not(Expr.Implication(Expr.Var(id1), Expr.Var(id2))) :: pc, SymVal.True)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 => $v2'.")
            }

            /**
              * Logical Bicondition.
              */
            case BinaryOperator.Biconditional => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.True)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => List(
                (Expr.Var(id) :: pc, SymVal.True),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.True) => List(
                (Expr.Var(id) :: pc, SymVal.True),
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.False)
              )

              case (SymVal.False, SymVal.AtomicVar(id)) => List(
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.True),
                (Expr.Var(id) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.False) => List(
                (Expr.Not(Expr.Var(id)) :: pc, SymVal.True),
                (Expr.Var(id) :: pc, SymVal.False)
              )

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (Expr.Bicondition(Expr.Var(id1), Expr.Var(id2)) :: pc, SymVal.True),
                (Expr.Not(Expr.Bicondition(Expr.Var(id1), Expr.Var(id2))) :: pc, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 <==> $v2'.")
            }

            /**
              * Bitwise And.
              */
            case BinaryOperator.BitwiseAnd => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 & i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 & i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 & i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 & i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseAnd(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseAnd(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 & $v2'.")
            }

            /**
              * Bitwise Or.
              */
            case BinaryOperator.BitwiseOr => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 | i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 | i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 | i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 | i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseOr(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseOr(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 | $v2'.")
            }

            /**
              * Bitwise Xor.
              */
            case BinaryOperator.BitwiseXor => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 ^ i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 ^ i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 ^ i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 ^ i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseXor(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseXor(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 ^ $v2'.")
            }

            /**
              * Bitwise Left Shift.
              */
            case BinaryOperator.BitwiseLeftShift => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 << i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 << i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 << i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 << i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseLeftShift(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseLeftShift(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 ^ $v2'.")
            }

            /**
              * Bitwise Right Shift.
              */
            case BinaryOperator.BitwiseRightShift => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 >> i2).asInstanceOf[Byte]))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 >> i2).asInstanceOf[Short]))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 >> i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 >> i2))

              // Symbolic semantics.
              case (SymVal.AtomicVar(id), v) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseRightShift(Expr.Var(id), toExpr(v))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case (v, SymVal.AtomicVar(id)) if isVarOrInt(v) =>
                val newVar = genSym.fresh2()
                val newPC = Expr.Eq(Expr.Var(newVar), Expr.BitwiseRightShift(toExpr(v), Expr.Var(id))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 ^ $v2'.")
            }

            /**
              * Equal.
              */
            case BinaryOperator.Equal => eq(pc, v1, v2)

            /**
              * Not Equal.
              */

          }
        }

      /**
        * If-then-else.
        */
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        eval(pc0, exp1, env0) flatMap {
          case (pc, c) => c match {
            case SymVal.True => eval(pc, exp2, env0)
            case SymVal.False => eval(pc, exp3, env0)
            case SymVal.AtomicVar(id) =>
              // Evaluate both branches under different path constraints.
              val consequent = eval(Expr.Var(id) :: pc, exp2, env0.clone())
              val alternative = eval(Expr.Not(Expr.Var(id)) :: pc, exp3, env0.clone())
              consequent ++ alternative
            case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
          }
        }

      /**
        * Let-binding.
        */
      case Expression.Let(ident, _, exp1, exp2, _, _) =>
        eval(pc0, exp1, env0) flatMap {
          case (pc, v1) =>
            // Bind the variable to the value of `exp1` which is `v1`.
            val newEnv = env0.clone()
            newEnv += (ident.name -> v1)
            eval(pc, exp2, newEnv)
        }

      /**
        * Tags.
        */
      case Expression.Tag(enum, tag, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, v) => lift(pc, SymVal.Tag(tag.name, v))
        }

      /**
        * Tuples.
        */
      case Expression.Tuple(elms, _, _) =>
        evaln(pc0, elms, env0) flatMap {
          case (pc, es) => lift(pc, SymVal.Tuple(es))
        }

      /**
        * Check Tag Value.
        */
      case Expression.CheckTag(tag, exp, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Tag(tag2, _)) =>
            if (tag.name == tag2)
              lift(pc, SymVal.True)
            else
              lift(pc, SymVal.False)
          case (_, v) => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * Get Tag Value.
        */
      case Expression.GetTagValue(tag, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Tag(_, v)) => lift(pc, v)
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * Get Tuple Index.
        */
      case Expression.GetTupleIndex(base, offset, _, _) =>
        eval(pc0, base, env0) flatMap {
          case (pc, SymVal.Tuple(elms)) => lift(pc, elms(offset))
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * User Error.
        */
      case Expression.UserError(tpe, loc) => lift(pc0, SymVal.UserError(loc))

      /**
        * Match Error.
        */
      case Expression.MatchError(tpe, loc) => lift(pc0, SymVal.MatchError(loc))

      /**
        * Switch Error
        */
      case Expression.SwitchError(tpe, loc) => lift(pc0, SymVal.SwitchError(loc))

      // TODO: These need to be harmonized.
      case e: Expression.FSet => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.CheckNil => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.CheckCons => throw InternalCompilerException(s"Unsupported expression: '$e'.")


      /**
        * Unsupported expressions.
        */
      case e: Expression.Hook => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.Universal => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.Existential => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadBool => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadInt8 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadInt16 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadInt32 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreBool => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreInt8 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreInt16 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreInt32 => throw InternalCompilerException(s"Unsupported expression: '$e'.")

    }

    /**
      * Returns a context with the value `v` guarded by the path constraint `pc`.
      */
    def lift(pc: PathConstraint, v: SymVal): List[(PathConstraint, SymVal)] = List(pc -> v)


    def eq(pc0: PathConstraint, x: SymVal, y: SymVal): List[(PathConstraint, SymVal)] = (x, y) match {
      case (SymVal.AtomicVar(ident1), SymVal.AtomicVar(ident2)) =>
        // Two identifiers are either equal to each other or they are not.
        // We can encode this using two path constraints.
        List(
          (Expr.Eq(Expr.Var(ident1), Expr.Var(ident2)) :: pc0, SymVal.True),
          (Expr.Neq(Expr.Var(ident1), Expr.Var(ident2)) :: pc0, SymVal.False)
        )
      case (SymVal.Unit, SymVal.Unit) => lift(pc0, SymVal.True)
      case (SymVal.Tag(tag1, v1), SymVal.Tag(tag2, v2)) => if (tag1 == tag2) eq(pc0, v1, v2) else lift(pc0, SymVal.False)
      // TODO: Rest
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

    /**
      * Returns `true` if `v` is a int value.
      */
    def isVarOrInt(v: SymVal): Boolean = v match {
      case SymVal.AtomicVar(id) => true
      case SymVal.Int8(i) => true
      case SymVal.Int16(i) => true
      case SymVal.Int32(i) => true
      case SymVal.Int64(i) => true
      case _ => false
    }

    /**
      * Converts the given value `v` to an expression.
      */
    def toExpr(v: SymVal): Expr = v match {
      case SymVal.AtomicVar(id) => Expr.Var(id)
      case SymVal.Int8(i) => Expr.Int8(i)
      case SymVal.Int16(i) => Expr.Int16(i)
      case SymVal.Int32(i) => Expr.Int32(i)
      case SymVal.Int64(i) => Expr.Int64(i)
      case _ => throw InternalCompilerException(s"Unexpected value: '$v'.")
    }

    //  TODO: Replace this by a different enumeration.
    def toSymVal(exp0: Expression): SymVal = exp0 match {
      case Expression.Unit => SymVal.Unit
      case Expression.Var(ident, _, _, _) => SymVal.AtomicVar(ident)
      case Expression.Tag(enum, tag, exp, tpe, loc) =>
        SymVal.Tag(tag.name, toSymVal(exp))
      case _ => ???
    }

    /**
      * Construct the initial environment.
      */
    val initEnv = mutable.Map.empty[String, SymVal]
    for ((name, exp) <- env0) {
      initEnv += (name -> toSymVal(exp))
    }

    eval(Nil, exp0, initEnv)
  }


  // TODO: toByte, toShort?

}
