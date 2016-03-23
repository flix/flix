package ca.uwaterloo.flix.language.ast

import scala.collection.immutable

/**
  * A common super-type for types.
  */
sealed trait Type {
  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = this match {
    case Type.Var(x) => "Var(" + x + ")"
    case Type.Unit => "Unit"
    case Type.Bool => "Bool"
    case Type.Char => "Char"
    case Type.Float32 => "Float32"
    case Type.Float64 => "Float64"
    case Type.Int8 => "Int8"
    case Type.Int16 => "Int16"
    case Type.Int32 => "Int32"
    case Type.Int64 => "Int64"
    case Type.Str => "Str"
    case Type.Native => "Native"
    case Type.Prop => "Prop"
    case Type.Tag(enum, tag, tpe) => tag.name + "(" + tpe + ")"
    case Type.UnresolvedTag(enum, tag, tpe) => "?" + tag.name + "(" + tpe + ")"
    case Type.Enum(enum, cases) => enum.fqn
    case Type.Tuple(elms) => "(" + elms.mkString(". ") + ")"
    case Type.Lambda(args, r) => "λ(" + args.mkString(", ") + ") -> " + r
    case Type.Parametric(name, elms) => "Parametric(" + name + ", " + elms.mkString(", ") + ")"
    case Type.FOpt(tpe) => "Opt[" + tpe + "]"
    case Type.FList(tpe) => "Lst[" + tpe + "]"
    case Type.FSet(tpe) => "Set[" + tpe + "]"
    case Type.FMap(key, value) => "Map[" + key + ", " + value + "]"
    case Type.Unresolved(name) => "?" + name
    case Type.Abs(name, tpe) => ??? // TODO
    case Type.Any => "Any"
    case Type.Predicate(terms) => "Predicate(" + terms.mkString(", ") + ")"

  }
}

object Type {

  /**
    * A type variable.
    */
  case class Var(x: String) extends Type

  /**
    * An AST node that represents the Unit type.
    */
  case object Unit extends Type

  /**
    * An AST node that represents the Bool type.
    */
  case object Bool extends Type

  /**
    * An AST node that represents the Char type.
    */
  case object Char extends Type

  /**
    * An AST node that represents the Float32 type.
    */
  case object Float32 extends Type

  /**
    * An AST node that represents the Float64 type.
    */
  case object Float64 extends Type

  /**
    * An AST node that represents the 8-bit signed integer type.
    */
  case object Int8 extends Type

  /**
    * An AST node that represents the 16-bit signed integer type.
    */
  case object Int16 extends Type

  /**
    * An AST node that represents the 32-bit signed integer type.
    */
  case object Int32 extends Type

  /**
    * An AST node that represents the 64-bit signed integer type.
    */
  case object Int64 extends Type

  /**
    * An AST node that represents the Str type.
    */
  case object Str extends Type

  /**
    * An AST node that represents a native type.
    */
  case object Native extends Type

  /**
    * An AST node that represents the proposition type.
    */
  case object Prop extends Type

  /**
    * An AST node that represents an enum type.
    *
    * @param name  the fully qualified name of the enum.
    * @param cases a map from tag names to tag types.
    */
  case class Enum(name: Symbol.Resolved, cases: immutable.Map[String, Type.Tag]) extends Type

  /**
    * An AST node that represents a tuple type.
    *
    * @param elms the types of the elements.
    */
  case class Tuple(elms: List[Type]) extends Type

  /**
    * An AST node that represents a lambda type.
    *
    * @param args   the type of the arguments.
    * @param retTpe the type of the return type.
    */
  case class Lambda(args: List[Type], retTpe: Type) extends Type

  /**
    * An AST node that represent a parametric type.
    *
    * @param name the ambiguous name.
    * @param elms the type of the type parameters.
    */
  // TODO: check with pierce book and see how this should be represented.
  case class Parametric(name: Name.QName, elms: Seq[Type]) extends Type

  /**
    * An AST node that represents an Opt type.
    *
    * @param tpe the type of the wrapped value.
    */
  case class FOpt(tpe: Type) extends Type

  /**
    * An AST node that represents a List type.
    *
    * @param tpe the type of the list elements.
    */
  case class FList(tpe: Type) extends Type

  /**
    * An AST node that represents a Set type.
    *
    * @param tpe the type of the set elements.
    */
  case class FSet(tpe: Type) extends Type

  /**
    * An AST node that represents a Map type.
    *
    * @param key   the type of the keys.
    * @param value the type of the values.
    */
  case class FMap(key: Type, value: Type) extends Type

  /**
    * An AST node that represents a predicate type.
    *
    * @param terms the type of predicate terms.
    */
  case class Predicate(terms: List[Type]) extends Type

  /**
    * An AST node that represent a reference to an unresolved type.
    *
    * @param name the name of the unresolved type.
    */
  @deprecated("to be removed", "0.1.0")
  case class Unresolved(name: Name.QName) extends Type

  // TODO: check with pierce book and see how this should be represented.
  @deprecated("to be removed", "0.1.0")
  case class Abs(name: Var, tpe: Type) extends Type

  @deprecated("to be removed", "0.1.0")
  case object Any extends Type

  /**
    * An AST node that represents the type of a tag.
    *
    * @param enum the fully qualified name of the enum.
    * @param tag  the name of the tag.
    * @param tpe  the type of the nested value.
    */
  @deprecated("to be removed", "0.1.0")
  case class Tag(enum: Symbol.Resolved, tag: Name.Ident, tpe: Type) extends Type

  /**
    * An AST node that represents the unresolved type of a tag.
    *
    * @param enum the unresolved enum name.
    * @param tag  the name of the tag.
    * @param tpe  the type of the nested value.
    */
  @deprecated("to be removed", "0.1.0")
  case class UnresolvedTag(enum: Name.Ident, tag: Name.Ident, tpe: Type) extends Type

}