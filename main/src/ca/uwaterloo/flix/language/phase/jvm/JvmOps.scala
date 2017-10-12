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
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Case, Expression, Root}
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.phase.Unification
import ca.uwaterloo.flix.util.InternalCompilerException

object JvmOps {

  // TODO: Sort functions.

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
      case Type.Unit => JvmType.Reference(JvmName(List("ca", "uwaterloo", "flix", "api"), "Unit"))
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
      case Type.Enum(sym, kind) => getEnumInterfaceType(tpe)
      case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
    }
  }

  /**
    * Returns the result type of the given type `tpe`.
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getResultType(tpe: Type)(implicit root: Root, flix: Flix): Type = {
    // Check that the given type is an arrow type.
    if (!tpe.typeConstructor.isArrow)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    // Check that the given type has at least one type argument.
    if (tpe.typeArguments.isEmpty)
      throw InternalCompilerException(s"Unexpected type: '$tpe'.")

    tpe.typeArguments.last
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
    val returnType = getResultType(tpe)

    // The JVM name is of the form Cont$ErasedType
    val name = "Cont$" + stringify(getErasedType(returnType))

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
    val args = tpe.typeArguments.map(tpe => stringify(getErasedType(tpe)))

    // The JVM name is of the form FnArity$Arg0$Arg1$Arg2
    val name = "Fn" + arity + "$" + args.mkString("$")

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the enum interface type `Enum$X$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * Color                 =>      Color
    * Option[Int]           =>      Option$Int
    * Result[Char, Int]     =>      Result$Char$Int
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
    val args = tpe.typeArguments.map(tpe => stringify(getErasedType(tpe)))

    // The JVM name is of the form Enum$Arg0$Arg1$Arg2
    val name = if (args.isEmpty) sym.name else sym.name + "$" + args.mkString("$")

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
    * Ok: Result[Bool, Char]    =>    Ok$Bool$Char
    * Err: Result[Bool, Char]   =>    Err$Bool$Char
    */
  // TODO: Can we improve the representation w.r.t. unused type variables?
  def getTagClassType(tag: TagInfo)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // Retrieve the tag name.
    val tagName = tag.tag

    // Retrieve the type arguments.
    val args = tag.tparams

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
    val args = tpe.typeArguments.map(tpe => stringify(getErasedType(tpe)))

    // The JVM name is of the form TArity$Arg0$Arg1$Arg2
    val name = "T" + arity + "$" + args.mkString("$")

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
    val args = tpe.typeArguments.map(tpe => stringify(getErasedType(tpe)))

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
    val arg = stringify(getErasedType(tpe.typeArguments.head))

    // The JVM name is of the form TArity$Arg0$Arg1$Arg2
    val name = "Cell" + "$" + arg

    // The type resides in the ca.uwaterloo.flix.api.cell package.
    JvmType.Reference(JvmName(List("ca.uwaterloo.flix.api.cell"), name))
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
    val name = "Def$" + sym.name
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
    * <root>.X()      =>  $X
    * Foo.X()         =>  Foo$Ns$X
    * Foo.Bar.X()     =>  Foo$Bar$Ns$X
    * Foo.Bar.Baz.Y() =>  Foo$Bar$Baz$Ns$X
    */
  // TODO: We should move "suffix" and "prefix" into helpers inside JvmOps.
  def getDefFieldNameInNamespaceClass(sym: Symbol.DefnSym): String = sym.suffix + '$' + sym.prefix.mkString("$")

  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    *
    * Every primitive type is mapped to itself and every other type is mapped to Object.
    */
  def getErasedType(tpe: Type)(implicit root: Root, flix: Flix): JvmType = tpe match {
    case Type.Bool => JvmType.PrimBool
    case Type.Char => JvmType.PrimChar
    case Type.Float32 => JvmType.PrimFloat
    case Type.Float64 => JvmType.PrimDouble
    case Type.Int8 => JvmType.PrimByte
    case Type.Int16 => JvmType.PrimShort
    case Type.Int32 => JvmType.PrimInt
    case Type.Int64 => JvmType.PrimLong
    case _ => JvmType.Object
  }

  /**
    * Performs name mangling on the given string `s` to avoid issues with special characters.
    */
  // TODO: Use this in appropriate places.
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
    * Returns the set of namespaces in the given AST `root`.
    */
  def namespacesOf(root: Root): Set[NamespaceInfo] = {
    // Group every symbol by namespace.
    root.defs.groupBy(_._1.namespace).map {
      case (ns, defs) => NamespaceInfo(ns, defs)
    }.toSet
  }

  /**
    * Returns the set of tags in the given AST `root`.
    */
  def tagsOf(root: Root): Set[TagInfo] = {
    /**
      * Returns the set of tags associated with the given type.
      */
    def visitType(tpe: Type): Set[TagInfo] = {
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
          // TODO: It would be nice if this information could be stored somewhere...
          val subst = Unification.unify(enum.tpe, tpe).get
          val tagType = subst(uninstantiatedTagType)

          sacc + TagInfo(enumSym, tagName.name, args, tpe, tagType)
      }
    }

    typesOf(root).flatMap(visitType)
  }

  /**
    * Returns the tag info for the given `tpe` and `tag`
    */
  def getTagInfo(tpe: Type, tag: String)(implicit root: Root, flix: Flix): TagInfo = {
    // Throw an exception if `tpe` is not an enum type
    if(!tpe.isEnum) {
      throw new InternalCompilerException(s"Unexpected type: $tpe")
    }

    // Retrieve the enum symbol and type arguments.
    val enumType = tpe.typeConstructor.asInstanceOf[Type.Enum]
    val args = tpe.typeArguments

    // Retrieve the enum.
    val enum = root.enums(enumType.sym)

    // Retrive the case
    val enumCase = enum.cases(tag)

    val subst = Unification.unify(enum.tpe, tpe).get
    val tagType = subst(enumCase.tpe)

    TagInfo(enumCase.sym, tag, args, tpe, tagType)
  }

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  def typesOf(root: Root): Set[Type] = {
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

      case Expression.ApplyCloTail(exp, args, tpe, loc) => args.foldLeft(visitExp(exp) + tpe) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(sym, fparams, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyHook(hook, args, tpe, loc) => args.foldLeft(Set(tpe)) {
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

      case Expression.Ref(exp, tpe, loc) => visitExp(exp) + tpe
      case Expression.Deref(exp, tpe, loc) => visitExp(exp) + tpe
      case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

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
      case Expression.MatchError(tpe, loc) => Set(tpe)
      case Expression.SwitchError(tpe, loc) => Set(tpe)
    }

    // TODO: Look for types in other places.

    // Visit every definition.
    root.defs.foldLeft(Set.empty[Type]) {
      case (sacc, (_, defn)) => sacc ++ visitExp(defn.exp) + defn.tpe
    }
  }

  /**
    * Returns all the type components of the given type `tpe`.
    *
    * For example, if the given type is `Option[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Option[(Bool, Char, Int)]`.
    */
  def typesOf(tpe: Type): Set[Type] = ??? // TODO

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
    * Returns true if the case has a unit field, which means the case can be a singleton. It returns false otherwise.
    *
    * @param tag Enum Case
    */
  def isSingletonEnum(tag: TagInfo): Boolean = {
    tag.tagType == Type.Unit
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

}
