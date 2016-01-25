package ca.uwaterloo.flix

import ca.uwaterloo.flix.language.ast.TypedAst.{Expression => FExp}
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SourceLocation, Type => FType}

// TODO use builder interface for Flix:

object Context {

  private final class Impl extends Context {

    import ca.uwaterloo.flix.language.ast.TypedAst.Expression._

    override type Exp = FExp

    override type BoolExp = FExp

    override type Type = FType

    /**
      *
      */
    override def mkAnd(e1: FExp, e2: FExp): FExp =
      Binary(BinaryOperator.LogicalAnd, e1, e2, FType.Bool, SourceLocation.Unknown)

    /////////////////////////////////////////////////////////////////////////////
    // Expressions                                                             //
    /////////////////////////////////////////////////////////////////////////////
    override def mkBool(b: Boolean): BoolExp = ???

    override def mkBoolType: Type = FType.Bool

    override def mkIntType: Type = FType.Int

    override def close(): Unit = {
      // release resources, if any.
    }

  }

}

/**
  * Programmatic interface for the Flix language and run-time.
  *
  * The abstract type members `Exp` and `Type` ensures that Flix can change its
  * underlying implementation/representation without affecting any client code.
  *
  * Casting a flix expression/type to its actual implementation/representation
  * is strongly discouraged.
  */
trait Context extends AutoCloseable {

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The type of expressions.
    */
  type Exp

  type BoolExp

  /**
    * Returns the given boolean `b` as a Flix expression.
    */
  def mkBool(b: Boolean): BoolExp

  /**
    *
    */
  def mkAnd(e1: BoolExp, e2: BoolExp): BoolExp

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The type of types.
    */
  type Type

  /**
    * Returns the boolean type.
    */
  def mkBoolType: Type

  /**
    * Returns the int type.
    */
  def mkIntType: Type

}

