/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.jvm

import java.nio.file.{Files, LinkOption, Path}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.phase.Unification
import ca.uwaterloo.flix.util.{InternalCompilerException, Optimization}

object JvmOps {

  /**
    * The root package name.
    */
  val RootPackage: List[String] = Nil

  /**
    * Returns the given Flix type `tpe` as JVM type.
    *
    * For example, if the type is:
    *
    * Bool                  =>      Boolean
    * Char                  =>      Char
    * Option[Int]           =>      Option$Int
    * Result[Bool, Int]     =>      Result$Bool$Int
    * Int -> Bool           =>      Fn1$Int$Bool
    * (Int, Int) -> Bool    =>      Fn2$Int$Int$Bool
    */
  def getJvmType(tpe: Type)(implicit root: Root, flix: Flix): JvmType = {
    // Retrieve the type constructor.
    val base = tpe.typeConstructor

    // Retrieve the type arguments.
    val args = tpe.typeArguments

    // Match on the type constructor.
    base match {
      case Type.Unit => JvmType.Unit
      case Type.Bool => JvmType.PrimBool
      case Type.Char => JvmType.PrimChar
      case Type.Float32 => JvmType.PrimFloat
      case Type.Float64 => JvmType.PrimDouble
      case Type.Int8 => JvmType.PrimByte
      case Type.Int16 => JvmType.PrimShort
      case Type.Int32 => JvmType.PrimInt
      case Type.Int64 => JvmType.PrimLong
      case Type.BigInt => JvmType.BigInteger
      case Type.Str => JvmType.String
      case Type.Native => JvmType.Object
      case Type.Ref => getCellClassType(tpe)
      case Type.Arrow(l) => getFunctionInterfaceType(tpe)
      case Type.Tuple(l) => getTupleInterfaceType(tpe)
      case Type.Array => JvmType.Object
      case Type.Vector => JvmType.Object
      case Type.Enum(sym, kind) =>
        getNullability(tpe) match {
          case Nullability.Nullable(t) =>
            // If the enum is nullable it means that it is the Option[a] type.
            // Hence we extract the inner type, the 'a'.
            getJvmType(args.head)
          case Nullability.NonNullable(t) => getEnumInterfaceType(tpe)
          case Nullability.Primitive(t) => throw InternalCompilerException(s"Unexpected primitive type: '$tpe'.")
          case Nullability.Reference(t) => throw InternalCompilerException(s"Unexpected reference type: '$tpe'.")
        }
      case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
    }
  }

  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    */
  def getErasedJvmType(tpe: Type)(implicit root: Root, flix: Flix): JvmType = {
    /**
      * Returns the erased JvmType of the given JvmType `tpe`.
      *
      * Every primitive type is mapped to itself and every other type is mapped to Object.
      */
    def erase(tpe: JvmType): JvmType = tpe match {
      case JvmType.Void => JvmType.Void
      case JvmType.PrimBool => JvmType.PrimBool
      case JvmType.PrimChar => JvmType.PrimChar
      case JvmType.PrimByte => JvmType.PrimByte
      case JvmType.PrimShort => JvmType.PrimShort
      case JvmType.PrimInt => JvmType.PrimInt
      case JvmType.PrimLong => JvmType.PrimLong
      case JvmType.PrimFloat => JvmType.PrimFloat
      case JvmType.PrimDouble => JvmType.PrimDouble
      case JvmType.Reference(jvmName) => JvmType.Object
    }

    erase(getJvmType(tpe))
  }

  /**
    * Returns the erased result type of the given type `tpe`.
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getErasedResultJvmType(tpe: Type)(implicit root: Root, flix: Flix): JvmType = {
    // Check that the given type is an arrow type.
    if (!tpe.typeConstructor.isArrow)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    getErasedJvmType(tpe.typeArguments.last)
  }

  /**
    * Returns the continuation interface type `Cont$X` for the given type `tpe`.
    *
    * Int -> Int          =>  Cont$Int
    * (Int, Int) -> Int   =>  Cont$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getContinuationInterfaceType(tpe: Type)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Check that the given type is an arrow type.
    if (!tpe.typeConstructor.isArrow)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // The return type is the last type argument.
    val returnType = getErasedResultJvmType(tpe)

    // The JVM name is of the form Cont$ErasedType
    val name = "Cont$" + stringify(returnType)

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the function interface type `FnX$Y$Z` for the given type `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Fn2$Int$Int
    * (Int, Int) -> Int   =>  Fn3$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getFunctionInterfaceType(tpe: Type)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Check that the given type is an arrow type.
    if (!tpe.typeConstructor.isArrow)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Compute the arity of the function interface.
    // We subtract one since the last argument is the return type.
    val arity = tpe.typeArguments.length - 1

    // Compute the stringified erased type of each type argument.
    val args = tpe.typeArguments.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form FnArity$Arg0$Arg1$Arg2
    val name = "Fn" + arity + "$" + args.mkString("$")

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the closure class `Clo$Name` for the given closure.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.length       =>    List/Clo$length
    * List.map          =>    List/Clo$map
    */
  def getClosureClassType(closure: ClosureInfo)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Retrieve the arrow type of the closure.
    val tpe = closure.tpe

    // Check that the given type is an arrow type.
    if (!tpe.typeConstructor.isArrow)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Compute the arity of the function interface.
    // We subtract one since the last argument is the return type.
    val arity = tpe.typeArguments.length - 1

    // Compute the stringified erased type of each type argument.
    val args = tpe.typeArguments.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form Clo$sym.name
    val name = "Clo" + "$" + mangle(closure.sym.name)

    // The JVM package is the namespace of the symbol.
    val pkg = closure.sym.namespace

    // The result type.
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the enum interface type `Enum$X$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * Color                 =>      IColor
    * Option[Int]           =>      IOption$Int
    * Result[Char, Int]     =>      IResult$Char$Int
    *
    * NB: The given type `tpe` must be an enum type.
    */
  def getEnumInterfaceType(tpe: Type)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Check that the given type is an enum type.
    if (!tpe.typeConstructor.isEnum)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Retrieve the enum symbol.
    val sym = tpe.typeConstructor.asInstanceOf[Type.Enum].sym

    // Compute the stringified erased type of each type argument.
    val args = tpe.typeArguments.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form Option$ or Option$Int
    val name = if (args.isEmpty) "I" + sym.name else "I" + sym.name + "$" + args.mkString("$")

    // The enum resides in its namespace package.
    JvmType.Reference(JvmName(sym.namespace, name))
  }

  /**
    * Returns the tag class `Tag$X$Y$Z` for the given tag.
    *
    * For example,
    *
    * None: Option[Int]         =>    None
    * Some: Option[Char]        =>    Some$Char
    * Some: Option[Int]         =>    Some$Int
    * Some: Option[String]      =>    Some$Obj
    * Ok: Result[Bool, Char]    =>    Ok$Bool$Char
    * Err: Result[Bool, Char]   =>    Err$Bool$Char
    */
  // TODO: Magnus: Can we improve the representation w.r.t. unused type variables?
  def getTagClassType(tag: TagInfo)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Retrieve the tag name.
    val tagName = tag.tag

    // Retrieve the type arguments.
    val args = tag.tparams.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form Tag$Arg0$Arg1$Arg2
    val name = if (args.isEmpty) tagName else tagName + "$" + args.mkString("$")

    // The tag class resides in its namespace package.
    JvmType.Reference(JvmName(tag.sym.namespace, name))
  }

  /**
    * Returns the tuple interface type `TX$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * (Int, Int)              =>    T2$Int$Int
    * (Int, Int, Int)         =>    T3$Int$Int$Int
    * (Bool, Char, Int)       =>    T3$Bool$Char$Int
    *
    * NB: The given type `tpe` must be a tuple type.
    */
  def getTupleInterfaceType(tpe: Type)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Check that the given type is an tuple type.
    if (!tpe.typeConstructor.isTuple)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Compute the arity of the tuple.
    val arity = tpe.typeArguments.length

    // Compute the stringified erased type of each type argument.
    val args = tpe.typeArguments.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form TArity$Arg0$Arg1$Arg2
    val name = "ITuple" + arity + "$" + args.mkString("$")

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the tuple class type `TupleX$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * (Int, Int)              =>    Tuple2$Int$Int
    * (Int, Int, Int)         =>    Tuple3$Int$Int$Int
    * (Bool, Char, Int)       =>    Tuple3$Bool$Char$Int
    * (Bool, List[Int])       =>    Tuple2$Bool$Obj
    * (Bool, (Int, Int))      =>    Tuple2$Bool$Obj
    *
    * NB: The given type `tpe` must be a tuple type.
    */
  def getTupleClassType(tpe: Type)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Check that the given type is an tuple type.
    if (!tpe.typeConstructor.isTuple)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Compute the arity of the tuple.
    val arity = tpe.typeArguments.length

    // Compute the stringified erased type of each type argument.
    val args = tpe.typeArguments.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form TupleArity$Arg0$Arg1$Arg2
    val name = "Tuple" + arity + "$" + args.mkString("$")

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns cell class type for the given type `tpe`.
    *
    * Ref[Bool]              =>    Ref$Bool
    * Ref[List[Int]          =>    Ref$Obj
    *
    * NB: The type must be a reference type.
    */
  def getCellClassType(tpe: Type)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Check that the given type is an tuple type.
    if (!tpe.typeConstructor.isRef)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Compute the stringified erased type of the argument.
    val arg = stringify(getErasedJvmType(tpe.typeArguments.head))

    // The JVM name is of the form TArity$Arg0$Arg1$Arg2
    val name = "Cell" + "$" + arg

    // The type resides in the ca.uwaterloo.flix.api.cell package.
    JvmType.Reference(JvmName(List("ca", "uwaterloo", "flix"), name))
  }

  /**
    * Returns the function definition class for the given symbol.
    *
    * For example:
    *
    * print         =>  Def$print
    * List.length   =>  List.Def$length
    */
  def getFunctionDefinitionClassType(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): JvmType.Reference = {
    val pkg = sym.namespace
    val name = "Def$" + mangle(sym.name)
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the namespace type for the given namespace `ns`.
    *
    * For example:
    *
    * <root>      =>  Ns
    * Foo         =>  Foo.Ns
    * Foo.Bar     =>  Foo.Bar.Ns
    * Foo.Bar.Baz =>  Foo.Bar.Baz.Ns
    */
  def getNamespaceClassType(ns: NamespaceInfo)(implicit root: Root, flix: Flix): JvmType.Reference = {
    val pkg = ns.ns
    val name = "Ns"
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the field name of a namespace as used in the Context class.
    *
    * For example:
    *
    * <root>      =>  Ns$Root$
    * Foo         =>  Foo$Ns
    * Foo.Bar     =>  Foo$Bar$Ns
    * Foo.Bar.Baz =>  Foo$Bar$Baz$Ns
    */
  def getNamespaceFieldNameInContextClass(ns: NamespaceInfo): String =
    if (ns.isRoot)
      "ns$Root$"
    else
      "ns$" + ns.ns.mkString("$")

  /**
    * Returns the field name of a defn as used in a namespace class.
    *
    * For example:
    *
    * find      =>  f_find
    * length    =>  f_length
    */
  def getDefFieldNameInNamespaceClass(sym: Symbol.DefnSym): String = "f_" + mangle(sym.name)

  /**
    * Returns the method name of a defn as used in a namespace class.
    *
    * For example:
    *
    * find      =>  m_find
    * length    =>  m_length
    */
  def getDefMethodNameInNamespaceClass(sym: Symbol.DefnSym): String = "m_" + mangle(sym.name)

  /**
    * Optionally returns the given `tag` as a fusion `tag`.
    */
  def getFusionTag(tag: TagInfo): Option[FusionTagInfo] = {
    // Retrieve the tag type.
    val innerType = tag.tagType

    // Return none if the tag type is a non-tuple.
    if (!innerType.isTuple)
      return None

    // Retrieve the element types of the tuple.
    val elementTypes = innerType.typeArguments

    // Construct the fusion tag.
    Some(FusionTagInfo(tag.sym, tag.tag, tag.enumType, tag.tagType, elementTypes))
  }

  /**
    * Returns the fusion class type `Cons$X$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * Some((Int, Int))      =>    Some$2$Int$Int
    */
  def getFusionClassType(tag: FusionTagInfo)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Retrieve the tag name.
    val tagName = tag.tag

    // Retrieve the type arguments.
    val args = tag.elms.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form Tag$Arg0$Arg1$Arg2
    val name = if (args.isEmpty) tagName else tagName + "$" + args.length + "$" + args.mkString("$")

    // The tag class resides in its namespace package.
    JvmType.Reference(JvmName(tag.sym.namespace, name))
  }

  /**
    * Returns `true` if the given `tag` associated with the given enum symbol `sym` can be represented by null.
    */
  def isNullTag(sym: Symbol.EnumSym, tag: String)(implicit root: Root, flix: Flix): Boolean = {
    // Retrieve the enum.
    val enum = root.enums(sym)

    // Retrieve the case.
    val caze = enum.cases(tag)

    // Check if the case is unit (i.e. can be represented by null).
    caze.tpe == Type.Unit
  }

  /**
    * Returns `true` if the given type `tpe` is nullable.
    */
  def isNullable(tpe: Type)(implicit root: Root, flix: Flix): Boolean = {
    if (!tpe.isEnum)
      throw InternalCompilerException(s"Unexpected type: $tpe")

    getNullability(tpe) match {
      case Nullability.Nullable(t) => true
      case Nullability.Primitive(t) => false
      case Nullability.Reference(t) => false
      case Nullability.NonNullable(t) => false
    }
  }

  /**
    * Returns the nullability of the given type `tpe`.
    */
  def getNullability(tpe: Type)(implicit root: Root, flix: Flix): Nullability = {
    // Check if the optimization is enabled.
    if (!(flix.options.optimizations contains Optimization.NullableEnums))
      return Nullability.NonNullable(tpe)

    // Retrieve the type constructor.
    val base = tpe.typeConstructor

    // Retrieve the type arguments.
    val args = tpe.typeArguments

    // Match on the type constructor.
    base match {
      // Primitive types.
      case Type.Unit => Nullability.Primitive(tpe)
      case Type.Bool => Nullability.Primitive(tpe)
      case Type.Char => Nullability.Primitive(tpe)
      case Type.Float32 => Nullability.Primitive(tpe)
      case Type.Float64 => Nullability.Primitive(tpe)
      case Type.Int8 => Nullability.Primitive(tpe)
      case Type.Int16 => Nullability.Primitive(tpe)
      case Type.Int32 => Nullability.Primitive(tpe)
      case Type.Int64 => Nullability.Primitive(tpe)

      // Nullable types.
      case Type.BigInt => Nullability.Reference(tpe)
      case Type.Str => Nullability.Reference(tpe)
      case Type.Native => Nullability.Reference(tpe)
      case Type.Ref => Nullability.Reference(tpe)
      case Type.Arrow(l) => Nullability.Reference(tpe)
      case Type.Tuple(l) => Nullability.Reference(tpe)

      // Enum types.
      case Type.Enum(sym, kind) =>
        // Check if the enum is the Option type.
        if (sym.name != "Option") {
          return Nullability.NonNullable(tpe)
        }

        // The Option type has exactly one type argument.
        val elementType = args.head

        // Determine if the element type is nullable.
        getNullability(elementType) match {
          case Nullability.Nullable(t) => Nullability.NonNullable(tpe)
          case Nullability.Primitive(t) => Nullability.NonNullable(tpe)
          case Nullability.Reference(t) => Nullability.Nullable(tpe)
          case Nullability.NonNullable(t) => Nullability.Nullable(elementType)
        }
      case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
    }
  }

  /**
    * Returns `true` if the given enum symbol `sym` is a single-case enum.
    */
  def isSingleCaseEnum(sym: Symbol.EnumSym)(implicit root: Root, flix: Flix): Boolean = {
    // Check if the optimization is enabled.
    if (!(flix.options.optimizations contains Optimization.SingleCaseEnums)) {
      return false
    }

    // Retrieve the enum declaration.
    val enum = root.enums(sym)

    // Check whether the enum is single-cased.
    enum.cases.size == 1
  }

  /**
    * Returns the given Flix type `tpe` where every single-case enum has been eliminated.
    *
    * For example,
    *
    * Assume we have the declarations:
    *
    * type Celsius = Celsius(Int)
    * type Cold = Cold(Celsius)
    * type Hot = Hot(Celsius)
    *
    * Then we return the following:
    *
    * Celsius             =>    Int
    * Cold                =>    Int
    * Hot                 =>    Int
    * Option[Celsius]     =>    Option[Int]
    * Result[Hot, Cold]   =>    Result[Int, Int]
    *
    * NB: This function assumes that a single-case enum is non-recursive.
    */
  def getSingleCaseEnumType(tpe: Type)(implicit root: Root, flix: Flix): Type = {
    // Check if the optimization is enabled.
    if (!(flix.options.optimizations contains Optimization.SingleCaseEnums)) {
      return tpe
    }

    // Retrieve the type constructor.
    val base = tpe.typeConstructor

    // Retrieve the type arguments.
    val args = tpe.typeArguments

    // Match on the base type.
    val result = base match {
      case Type.Unit => Type.Unit
      case Type.Bool => Type.Bool
      case Type.Char => Type.Char
      case Type.Float32 => Type.Float32
      case Type.Float64 => Type.Float64
      case Type.Int8 => Type.Int8
      case Type.Int16 => Type.Int16
      case Type.Int32 => Type.Int32
      case Type.Int64 => Type.Int64
      case Type.BigInt => Type.BigInt
      case Type.Str => Type.Str
      case Type.Native => Type.Native
      case Type.Ref => Type.Ref
      case Type.Arrow(l) => Type.Arrow(l)
      case Type.Tuple(l) => Type.Tuple(l)
      case Type.Enum(sym, kind) =>
        // Retrieve the tags of the enum.
        val tags = getTagsOf(tpe).toList

        // Check whether the enum is single-cased.
        tags match {
          case tag :: Nil =>
            // Case 1: The enum has exactly one case. Retrieve the inner type, recursively.
            getSingleCaseEnumType(tag.tagType)
          case _ =>
            // Case 2: The enum has zero, one, or more than two tags.
            tpe
        }
      case _ => throw InternalCompilerException(s"Unexpected type: '$base'.")
    }

    // Reconstruct the type.
    args.foldLeft(base)(Type.Apply)
  }

  /**
    * Performs name mangling on the given string `s` to avoid issues with special characters.
    */
  // TODO: Magnus: Use this in appropriate places.
  def mangle(s: String): String = s.
    replace("+", "$plus").
    replace("-", "$minus").
    replace("*", "$times").
    replace("/", "$divide").
    replace("%", "$modulo").
    replace("**", "$exponentiate").
    replace("<", "$lt").
    replace("<=", "$le").
    replace(">", "$gt").
    replace(">=", "$ge").
    replace("==", "$eq").
    replace("!=", "$neq").
    replace("&&", "$land").
    replace("||", "$lor").
    replace("&", "$band").
    replace("|", "$bor").
    replace("^", "$bxor").
    replace("<<", "$lshift").
    replace(">>", "$rshift")

  /**
    * Returns stringified name of the given JvmType `tpe`.
    *
    * The stringified name is short hand used for generation of interface and class names.
    */
  def stringify(tpe: JvmType): String = tpe match {
    case JvmType.Void => "Void"
    case JvmType.PrimBool => "Bool"
    case JvmType.PrimChar => "Char"
    case JvmType.PrimFloat => "Float32"
    case JvmType.PrimDouble => "Float64"
    case JvmType.PrimByte => "Int8"
    case JvmType.PrimShort => "Int16"
    case JvmType.PrimInt => "Int32"
    case JvmType.PrimLong => "Int64"
    case JvmType.Reference(jvmName) => "Obj"
  }

  /**
    * Returns the set of closures in the given AST `root`.
    */
  def closuresOf(root: Root): Set[ClosureInfo] = {
    /**
      * Returns the set of closures in the given expression `exp0`.
      */
    def visitExp(exp0: Expression): Set[ClosureInfo] = exp0 match {
      case Expression.Unit => Set.empty
      case Expression.True => Set.empty
      case Expression.False => Set.empty
      case Expression.Char(lit) => Set.empty
      case Expression.Float32(lit) => Set.empty
      case Expression.Float64(lit) => Set.empty
      case Expression.Int8(lit) => Set.empty
      case Expression.Int16(lit) => Set.empty
      case Expression.Int32(lit) => Set.empty
      case Expression.Int64(lit) => Set.empty
      case Expression.BigInt(lit) => Set.empty
      case Expression.Str(lit) => Set.empty
      case Expression.Var(sym, tpe, loc) => Set.empty

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
        Set(ClosureInfo(sym, freeVars, tpe))

      case Expression.ApplyClo(exp, args, tpe, loc) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyEff(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, tpe, loc) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyEffTail(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(sym, fparams, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        visitExp(exp)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Branch(exp, branches, tpe, loc) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }
      case Expression.JumpTo(sym, tpe, loc) => Set.empty

      case Expression.Let(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp)
      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp)
      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp)

      case Expression.Index(base, offset, tpe, loc) => visitExp(base)
      case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayNew(elm, len, tpe, loc) => visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.ArrayLength(exp, tpe, loc) => visitExp(exp)

      case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Ref(exp, tpe, loc) => visitExp(exp)
      case Expression.Deref(exp, tpe, loc) => visitExp(exp)
      case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.HandleWith(exp, bindings, tpe, loc) => ??? // TODO

      case Expression.Existential(fparam, exp, loc) => visitExp(exp)
      case Expression.Universal(fparam, exp, loc) => visitExp(exp)

      case Expression.NativeConstructor(constructor, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }
      case Expression.NativeField(field, tpe, loc) => Set.empty
      case Expression.NativeMethod(method, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.UserError(tpe, loc) => Set.empty
      case Expression.HoleError(sym, tpe, loc) => Set.empty
      case Expression.MatchError(tpe, loc) => Set.empty
      case Expression.SwitchError(tpe, loc) => Set.empty
    }


    // TODO: Magnus: Look for closures in other places.

    // Visit every definition.
    root.defs.foldLeft(Set.empty[ClosureInfo]) {
      case (sacc, (sym, defn)) => sacc ++ visitExp(defn.exp)
    }
  }

  /**
    * Returns the namespace info of the given definition symbol `sym`.
    */
  def getNamespace(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): NamespaceInfo = {
    NamespaceInfo(sym.namespace, Map.empty) // TODO: Magnus: Empty map.
  }

  /**
    * Returns the set of namespaces in the given AST `root`.
    */
  def namespacesOf(root: Root): Set[NamespaceInfo] = {
    // Group every symbol by namespace.
    root.defs.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        // Collect all non-law definitions.
        val nonLaws = defs filter {
          case (sym, defn) => nonLaw(defn)
        }
        NamespaceInfo(ns, nonLaws)
    }.toSet
  }

  /**
    * Returns the set of tags associated with the given type.
    */
  def getTagsOf(tpe: Type)(implicit root: Root, flix: Flix): Set[TagInfo] = {
    // Return the empty set if the type is not an enum.
    if (!tpe.isEnum) {
      return Set.empty
    }

    // Retrieve the enum symbol and type arguments.
    val enumType = tpe.typeConstructor.asInstanceOf[Type.Enum]
    val args = tpe.typeArguments

    // Retrieve the enum.
    val enum = root.enums(enumType.sym)

    // Compute the tag info.
    enum.cases.foldLeft(Set.empty[TagInfo]) {
      case (sacc, (_, Case(enumSym, tagName, uninstantiatedTagType, loc))) =>
        // TODO: Magnus: It would be nice if this information could be stored somewhere...
        val subst = Unification.unify(enum.tpe, tpe).get
        val tagType = subst(uninstantiatedTagType)

        sacc + TagInfo(enumSym, tagName.name, args, tpe, tagType)
    }
  }


  /**
    * Returns the tag info for the given `tpe` and `tag`
    */
  // TODO: Magnus: Should use getTags and then just find the correct tag.
  def getTagInfo(tpe: Type, tag: String)(implicit root: Root, flix: Flix): TagInfo = {
    // Throw an exception if `tpe` is not an enum type
    if (!tpe.isEnum)
      throw InternalCompilerException(s"Unexpected type: $tpe")

    val tags = getTagsOf(tpe)
    tags.find(_.tag == tag).get
  }

  /**
    * Returns true if the value of the given `tag` is the unit value.
    */
  def isUnitTag(tag: TagInfo): Boolean = {
    tag.tagType == Type.Unit
  }

  /**
    * Returns the set of tags in the given AST `root`.
    */
  def tagsOf(root: Root)(implicit flix: Flix): Set[TagInfo] = {
    typesOf(root).flatMap(tpe => getTagsOf(tpe)(root, flix))
  }

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  def typesOf(root: Root)(implicit flix: Flix): Set[Type] = {
    /**
      * Returns the set of types which occur in the given definition `defn0`.
      */
    def visitDefn(defn: Def): Set[Type] = {
      // Compute the types in the formal parameters.
      val formalParamTypes = defn.formals.foldLeft(Set.empty[Type]) {
        case (sacc, FormalParam(sym, tpe)) => sacc + tpe
      }

      // Compute the types in the expression.
      val expressionTypes = visitExp(defn.exp)

      // Return the types in the defn.
      formalParamTypes ++ expressionTypes + defn.tpe
    }

    /**
      * Returns the set of types which occur in the given expression `exp0`.
      */
    def visitExp(exp0: Expression): Set[Type] = exp0 match {
      case Expression.Unit => Set(Type.Unit)
      case Expression.True => Set(Type.Bool)
      case Expression.False => Set(Type.Bool)
      case Expression.Char(lit) => Set(Type.Char)
      case Expression.Float32(lit) => Set(Type.Float32)
      case Expression.Float64(lit) => Set(Type.Float64)
      case Expression.Int8(lit) => Set(Type.Int8)
      case Expression.Int16(lit) => Set(Type.Int16)
      case Expression.Int32(lit) => Set(Type.Int32)
      case Expression.Int64(lit) => Set(Type.Int64)
      case Expression.BigInt(lit) => Set(Type.BigInt)
      case Expression.Str(lit) => Set(Type.Str)
      case Expression.Var(sym, tpe, loc) => Set(tpe)

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) => Set(tpe)

      case Expression.ApplyClo(exp, args, tpe, loc) => args.foldLeft(visitExp(exp) + tpe) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyEff(sym, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, tpe, loc) => args.foldLeft(visitExp(exp) + tpe) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyEffTail(sym, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(sym, fparams, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        visitExp(exp) + tpe

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Branch(exp, branches, tpe, loc) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }
      case Expression.JumpTo(sym, tpe, loc) => Set(tpe)

      case Expression.Let(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp)
      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp) + tpe
      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Index(base, offset, tpe, loc) => visitExp(base) + tpe
      case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayNew(elm, len, tpe, loc) => visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.ArrayLength(exp, tpe, loc) => visitExp(exp)

      case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Ref(exp, tpe, loc) => visitExp(exp) + tpe
      case Expression.Deref(exp, tpe, loc) => visitExp(exp) + tpe
      case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.HandleWith(exp, bindings, tpe, loc) => bindings.foldLeft(visitExp(exp)) {
        case (sacc, HandlerBinding(sym, handler)) => sacc ++ visitExp(handler)
      }

      case Expression.Existential(fparam, exp, loc) => visitExp(exp) + fparam.tpe
      case Expression.Universal(fparam, exp, loc) => visitExp(exp) + fparam.tpe

      case Expression.NativeConstructor(constructor, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }
      case Expression.NativeField(field, tpe, loc) => Set(tpe)
      case Expression.NativeMethod(method, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.UserError(tpe, loc) => Set(tpe)
      case Expression.HoleError(sym, tpe, loc) => Set(tpe)
      case Expression.MatchError(tpe, loc) => Set(tpe)
      case Expression.SwitchError(tpe, loc) => Set(tpe)
    }

    // TODO: Magnus: Look for types in other places.

    // Visit every definition.
    val result = root.defs.foldLeft(Set.empty[Type]) {
      case (sacc, (_, defn)) => sacc ++ visitDefn(defn)
    }

    result.flatMap(t => nestedTypesOf(t)(root, flix))
  }

  /**
    * Returns all the type components of the given type `tpe`.
    *
    * For example, if the given type is `Option[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Option[(Bool, Char, Int)]`.
    */
  def nestedTypesOf(tpe: Type)(implicit root: Root, flix: Flix): Set[Type] = {
    // Retrieve the type constructor.
    val base = tpe.typeConstructor

    // Retrieve the type arguments.
    val args = tpe.typeArguments

    //
    // Check if the tag is an enum and if so, extract the types of its tags.
    // Usually this is not "necessary", but an enum might occur as a type,
    // but not have all its tags constructed as expressions.
    //
    base match {
      case Type.Enum(sym, _) =>
        // Case 1: The nested types are the type itself, its type arguments, and the types of the tags.
        val tagTypes = getTagsOf(tpe).map(_.tagType)

        args.foldLeft(Set(tpe) ++ tagTypes) {
          case (sacc, arg) => sacc ++ nestedTypesOf(arg)
        }
      case _ =>
        // Case 2: The nested types are the type itself and its type arguments.
        args.foldLeft(Set(tpe)) {
          case (sacc, arg) => sacc ++ nestedTypesOf(arg)
        }
    }
  }

  /**
    * Returns `true` if the given `path` exists and is a Java Virtual Machine class file.
    */
  def isClassFile(path: Path): Boolean = {
    if (Files.exists(path) && Files.isReadable(path) && Files.isRegularFile(path)) {
      // Read the first four bytes of the file.
      val is = Files.newInputStream(path)
      val b1 = is.read()
      val b2 = is.read()
      val b3 = is.read()
      val b4 = is.read()
      is.close()

      // Check if the four first bytes match CAFE BABE.
      return b1 == 0xCA && b2 == 0xFE && b3 == 0xBA && b4 == 0xBE
    }
    false
  }

  /**
    * Writes the given JVM class `clazz` to a sub path under the given `prefixPath`.
    *
    * For example, if the prefix path is `/tmp/` and the class name is Foo.Bar.Baz
    * then the bytecode is written to the path `/tmp/Foo/Bar/Baz.class` provided
    * that this path either does not exist or is already a JVM class file.
    */
  def writeClass(prefixPath: Path, clazz: JvmClass): Unit = {
    // Compute the absolute path of the class file to write.
    val path = prefixPath.resolve(clazz.name.toPath).toAbsolutePath

    // Create all parent directories (in case they don't exist).
    Files.createDirectories(path.getParent)

    // Check if the file already exists.
    if (Files.exists(path)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.")
      }

      // Check if the file is writable.
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.")
      }

      // Check that the file is a class file.
      if (!isClassFile(path)) {
        throw InternalCompilerException(s"Refusing to overwrite non-class file: '$path'.")
      }
    }

    // Write the bytecode.
    Files.write(path, clazz.bytecode)
  }

  /**
    * Returns `true` if the given definition `defn` is a law.
    */
  def nonLaw(defn: Def): Boolean = !defn.ann.isLaw

}
