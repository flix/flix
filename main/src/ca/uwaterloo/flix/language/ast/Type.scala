package ca.uwaterloo.flix.language.ast

// TODO: Cleanup this class.

/**
  * A common super-type for types.
  */
sealed trait Type

object Type {

  /**
    * A type variable.
    */
  case class Var(x: String) extends Type

  /**
    * An AST node representing the Unit type.
    */
  case object Unit extends Type

  /**
    * An AST node representing the Boolean type.
    */
  case object Bool extends Type

  // TODO: Eventually we'll want to use the specialized ints below, and this will alias to Int32
  case object Int extends Type

  /**
    * An AST node representing the 8-bit signed integer type, i.e. a byte.
    */
  case object Int8 extends Type

  /**
    * An AST node representing the 16-bit signed integer type, i.e. a short.
    */
  case object Int16 extends Type

  /**
    * An AST node representing the 32-bit signed integer type, i.e. an int.
    */
  case object Int32 extends Type

  /**
    * An AST node representing the 64-bit signed integer type, i.e. a long.
    */
  case object Int64 extends Type

  /**
    * An AST node representing the String type.
    */
  case object Str extends Type

  /**
    * An AST node representing the type of a tag.
    *
    * @param enum the namespace of the tag.
    * @param tag  the name of the tag.
    * @param tpe  the type of the nested value.
    */
  case class Tag(enum: Name.Resolved, tag: Name.Ident, tpe: Type) extends Type

  /**
    * An AST node representing an enum type (a set of tags).
    *
    * @param cases a map from tag names to tag types.
    */
  case class Enum(name: Name.Resolved, cases: scala.collection.immutable.Map[String, Type.Tag]) extends Type


  // TDOO: need this one?
  case class UnresolvedEnum(name: Name.Ident, cases: scala.collection.immutable.Map[String, Type.Tag]) extends Type

  case class UnresolvedTag(enum: Name.Ident, tag: Name.Ident, tpe: Type) extends Type


  /**
    * An AST node representing a tuple type.
    *
    * @param elms the types of the elements.
    */
  case class Tuple(elms: List[Type]) extends Type {
    @deprecated("removed", "0.1")
    val asArray: Array[Type] = elms.toArray
  }



  /**
    * An AST node representing a function type.
    *
    * @param args   the type of the arguments.
    * @param retTpe the type of the return type.
    */
  case class Lambda(args: List[Type], retTpe: Type) extends Type

  /**
    * An AST node representing a predicate type.
    *
    * @param terms the terms of the predicate.
    */
  case class Predicate(terms: List[Type]) extends Type

  /**
    * An AST node that represents a native type.
    */
  case class Native(name: String) extends Type // TODO: remove name




  /**
    * An AST node that represent a parametric type.
    *
    * @param name the ambiguous name.
    * @param elms the type of the type parameters.
    */
  case class Parametric(name: Name.Unresolved, elms: Seq[Type]) extends Type



  // TODO: Document
  case class Opt(elmType: Type) extends Type

  // TODO: Document
  case class Lst(elmType: Type) extends Type

  /**
    * An AST node representing a set type.
    *
    * @param elmType the types of the elements.
    */
  case class Set(elmType: Type) extends Type

  // TODO: Document
  case class Map(key: Type, value: Type) extends Type

  // TODO
  case object Char extends Type

  // TODO
  case class Abs(name: Var, tpe: Type) extends Type


  // TODO: Remove or rename to error???
  case object Any extends Type


  /**
    * An AST node that represent a reference to a named type.
    *
    * @param name the name of the type.
    */
  case class Named(name: Name.Unresolved) extends Type


}