package ca.uwaterloo.flix.runtime.verifier

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}

/**
  * Symbolic Values.
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
    * A BigInt value.
    *
    * @param lit the int literal.
    */
  case class BigInt(lit: java.math.BigInteger) extends SymVal

  /**
    * A String value.
    *
    * @param lit the int literal.
    */
  case class Str(lit: String) extends SymVal

  /**
    * A tag value.
    *
    * @param tag the tag name.
    * @param elm the tagged value.
    */
  case class Tag(tag: String, elm: SymVal) extends SymVal

  /**
    * A tuple value.
    *
    * @param elms the elements of the tuple.
    */
  case class Tuple(elms: List[SymVal]) extends SymVal

  /**
    * A closure value.
    *
    * @param ref the name of the top-level definition.
    * @param clo the name of the closure variable.
    * @param env the closure environment.
    */
  case class Closure(ref: Symbol.Resolved, clo: String, env: Environment) extends SymVal

  /**
    * An environment for closure variables.
    *
    * @param m the map from closure variables to symbolic values.
    */
  case class Environment(m: Map[String, SymVal]) extends SymVal


  /**
    * A user error value.
    */
  // TODO: Should throw exception instead!
  case class UserError(loc: SourceLocation) extends SymVal

  /**
    * A match error value.
    */
  // TODO: Should throw exception instead!
  case class MatchError(loc: SourceLocation) extends SymVal

  /**
    * A switch error value.
    */
  // TODO: Should throw exception instead!
  case class SwitchError(loc: SourceLocation) extends SymVal

}