/*
 * Copyright 2017 Magnus Madsen
 * Copyright 2021 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.{MonoType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.mangle
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.nio.file.{Files, LinkOption, Path}

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
    * Option$42             =>      Option$42
    * Result$123            =>      Result$123
    * Int -> Bool           =>      Fn1$Int$Bool
    * (Int, Int) -> Bool    =>      Fn2$Int$Int$Bool
    */
  def getJvmType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType = tpe match {
    // Primitives
    case MonoType.Unit => JvmType.Unit
    case MonoType.Bool => JvmType.PrimBool
    case MonoType.Char => JvmType.PrimChar
    case MonoType.Float32 => JvmType.PrimFloat
    case MonoType.Float64 => JvmType.PrimDouble
    case MonoType.BigDecimal => JvmType.BigDecimal
    case MonoType.Int8 => JvmType.PrimByte
    case MonoType.Int16 => JvmType.PrimShort
    case MonoType.Int32 => JvmType.PrimInt
    case MonoType.Int64 => JvmType.PrimLong
    case MonoType.BigInt => JvmType.BigInteger
    case MonoType.String => JvmType.String
    case MonoType.Regex => JvmType.Regex
    case MonoType.Region => JvmType.Object

    // Compound
    case MonoType.Array(_) => JvmType.Object
    case MonoType.Lazy(_) => JvmType.Object
    case MonoType.Ref(_) => getRefClassType(tpe)
    case MonoType.Tuple(_) => getTupleClassType(tpe.asInstanceOf[MonoType.Tuple])
    case MonoType.RecordEmpty => getRecordInterfaceType()
    case MonoType.RecordExtend(_, _, _) => getRecordInterfaceType()
    case MonoType.Enum(sym) => getEnumInterfaceType(sym)
    case MonoType.Arrow(_, _) => getFunctionInterfaceType(tpe)
    case MonoType.Native(clazz) =>
      // TODO: Ugly hack.
      val fqn = clazz.getName.replace('.', '/')
      JvmType.Reference(JvmName.mk(fqn))

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }


  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    *
    * Every primitive type is mapped to itself and every other type is mapped to Object.
    */
  def getErasedJvmType(tpe: MonoType): JvmType = {
    import MonoType._
    tpe match {
      case Bool => JvmType.PrimBool
      case Char => JvmType.PrimChar
      case Float32 => JvmType.PrimFloat
      case Float64 => JvmType.PrimDouble
      case Int8 => JvmType.PrimByte
      case Int16 => JvmType.PrimShort
      case Int32 => JvmType.PrimInt
      case Int64 => JvmType.PrimLong
      case Unit | BigDecimal | BigInt | String | Regex | Region | Array(_) |
           Lazy(_) | Ref(_) | Tuple(_) | Enum(_) | Arrow(_, _) | RecordEmpty |
           RecordExtend(_, _, _) | SchemaEmpty | SchemaExtend(_, _, _) |
           Native(_) => JvmType.Object
    }
  }

  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    *
    * Every primitive type is mapped to itself and every other type is mapped to Object.
    */
  def asErasedJvmType(tpe: MonoType): JvmType = {
    import MonoType._
    tpe match {
      case Bool => JvmType.PrimBool
      case Char => JvmType.PrimChar
      case Float32 => JvmType.PrimFloat
      case Float64 => JvmType.PrimDouble
      case Int8 => JvmType.PrimByte
      case Int16 => JvmType.PrimShort
      case Int32 => JvmType.PrimInt
      case Int64 => JvmType.PrimLong
      case Native(clazz) if clazz == classOf[Object] => JvmType.Object
      case Unit | BigDecimal | BigInt | String | Regex | Region | Array(_) |
           Lazy(_) | Ref(_) | Tuple(_) | Enum(_) | Arrow(_, _) | RecordEmpty |
           RecordExtend(_, _, _) | SchemaEmpty | SchemaExtend(_, _, _) |
           Native(_) => throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    }
  }

  /**
    * Returns the function abstract class type `FnX$Y$Z` for the given type `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Fn2$Int$Int
    * (Int, String) -> Int   =>  Fn3$Int$Obj$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getFunctionInterfaceType(tpe: MonoType): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      getFunctionInterfaceType(targs.map(getErasedJvmType), getErasedJvmType(tresult))
    case _ =>
      throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  /**
    * Returns the function abstract class type `FnX$Y$Z` for the given types.
    *
    * For example:
    *
    * (Int)(Int)          =>  Fn2$Int$Int
    * (Int, String)(Int)   =>  Fn3$Int$Obj$Int
    */
  def getFunctionInterfaceType(args: List[JvmType], res: JvmType): JvmType.Reference = {
    getFunctionInterfaceType(args.map(_.toErased).map(stringify), stringify(res.toErased))
  }
  /**
    * The JVM name is of the form `FnArity$argTypes0$argTypes1$..$resType`
    */
  private def getFunctionInterfaceType(argTypes: List[String], resType: String): JvmType.Reference = {
    val arity = argTypes.length
    val typeStrings = argTypes :+ resType
    val name = JvmName.mkClassName(s"Fn$arity", typeStrings)
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the closure abstract class type `CloX$Y$Z` for the given [[MonoType]].
    *
    * For example:
    *
    * Int -> Int          =>  Clo1$Int$Int
    * (Int, Int) -> Int   =>  Clo2$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getClosureAbstractClassType(tpe: MonoType): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      getClosureAbstractClassType(targs.map(getErasedJvmType), getErasedJvmType(tresult))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }


  /**
    * Returns the closure abstract class type `CloX$Y$Z` for the given signature.
    *
    * For example:
    *
    * Int -> Int          =>  Clo1$Int$Int
    * (Int, Int) -> Int   =>  Clo2$Int$Int$Int
    */
  def getClosureAbstractClassType(argTypes: List[JvmType], resType: JvmType): JvmType.Reference = {
    val arity = argTypes.length
    val args = (argTypes ::: resType :: Nil).map(_.toErased).map(stringify)
    val name = JvmName.mkClassName(s"Clo$arity", args)
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the closure class `Clo$Name` for the given closure.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.length       =>    List/Clo$length
    * List.map          =>    List/Clo$map
    */
  def getClosureClassType(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // The JVM name is of the form Clo$sym.name
    val name = JvmName.mkClassName(s"Clo", sym.name)

    // The JVM package is the namespace of the symbol.
    val pkg = sym.namespace

    // The result type.
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the enum interface type `IEnum$` for the given type `tpe`.
    *
    * For example,
    *
    * Color                 =>      IColor$
    *
    * NB: The given type `tpe` must be an enum type.
    */
  def getEnumInterfaceType(sym: Symbol.EnumSym)(implicit root: Root, flix: Flix): JvmType.Reference = {
      // The enum resides in its namespace package.
      val name = JvmName.mkClassName("I", sym.name)
      JvmType.Reference(JvmName(sym.namespace, name))
  }

  /**
    * Returns the tag class `Option$None` for the given tag.
    *
    * For example,
    *
    * None: Option$42   =>  Option$42$None
    * Some: Option$52   =>  Option$52$Some
    * Ok: Result$123    =>  Result$123$Ok
    */
  def getTagClassType(sym: Symbol.CaseSym)(implicit root: Root, flix: Flix): JvmType.Reference = {
    // TODO: Magnus: Can we improve the representation w.r.t. unused type variables?
    val name = JvmName.mkClassName(sym.enumSym.name, sym.name)
    // The tag class resides in its namespace package.
    JvmType.Reference(JvmName(sym.namespace, name))
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
  def getTupleClassType(tpe: MonoType.Tuple)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Tuple(elms) =>
      // Compute the arity of the tuple.
      val arity = elms.length

      // Compute the stringified erased type of each type argument.
      val args = elms.map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form TupleArity$Arg0$Arg1$Arg2
      val name = JvmName.mkClassName(s"Tuple$arity", args)

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))
  }

  def getLazyClassType(tpe: MonoType.Lazy)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Lazy(tpe) =>
      val arg = stringify(getErasedJvmType(tpe))
      val name = JvmName.mkClassName("Lazy", arg)
      JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the record interface type `Record`.
    *
    * For example,
    *
    * {}                    =>  Record
    * {x :: Int}            =>  Record
    * {x :: Str, y :: Int}  =>  Record
    */
  def getRecordInterfaceType()(implicit root: Root, flix: Flix): JvmType.Reference = {

    // The JVM name is of the form Record
    val name = JvmName.mkClassName("Record")

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the empty record class type `RecordEmtpy`
    *
    * For example,
    *
    * {}         =>    RecordEmpty
    *
    */
  def getRecordEmptyClassType()(implicit root: Root, flix: Flix): JvmType.Reference = {

    // The JVM name is of the form RecordEmpty
    val name = JvmName.mkClassName("RecordEmpty")

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }


  /**
    * Returns the extended record class type `RecordExtend$X` for the given type 'tpe'
    *
    * For example,
    *
    * {+z :: Int  | {}}                   =>    RecordExtend$Int
    * {+y :: Char | {z :: Int}            =>    RecordExtend$Char
    * {+x :: Str | {y :: Char, z :: Int}  =>    RecordExtend$Obj
    *
    * NB: The given type `tpe` must be a Record type
    */
  def getRecordExtendClassType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {

    case MonoType.RecordExtend(_, value, _) =>
      // Compute the stringified erased type of value.
      val valueType = stringify(getErasedJvmType(value))

      // The JVM name is of the form RecordExtend
      val name = JvmName.mkClassName("RecordExtend", valueType)

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }


  /**
    * Returns the extended record class type `RecordExtend$X` which contains the given type 'tpe'
    *
    * For example,
    *
    * Int                   =>  RecordExtend$Int
    * Char                  =>  RecordExtend$Char
    * {x :: Char, y :: Int} =>  RecordExtend$Obj
    *
    */
  def getRecordType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = {

    // Compute the stringified erased type of 'tpe'.
    val valueType = JvmOps.stringify(JvmOps.getErasedJvmType(tpe))

    // The JVM name is of the form RecordExtend
    val name = JvmName.mkClassName("RecordExtend", valueType)

    // The type resides in the root package.
    JvmType.Reference(JvmName(JvmOps.RootPackage, name))
  }

  /**
    * Returns the Main  `Main`
    */
  def getMainClassType()(implicit root: Root, flix: Flix): JvmType.Reference = {

    // The JVM name is of the form Main
    val name = "Main"

    // The type resides in the root package.
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns reference class type for the given type `tpe`.
    *
    * Ref[Bool]              =>    Ref$Bool
    * Ref[List[Int]          =>    Ref$Obj
    *
    * NB: The type must be a reference type.
    */
  def getRefClassType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Ref(elmType) =>
      // Compute the stringified erased type of the argument.
      val arg = stringify(getErasedJvmType(elmType))

      // The JVM name is of the form TArity$Arg0$Arg1$Arg2
      val name = JvmName.mkClassName("Ref", arg)

      // The type resides in the ca.uwaterloo.flix.api.cell package.
      JvmType.Reference(JvmName(Nil, name))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
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
    val name = JvmName.mkClassName("Def", sym.name)
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the effect definition class for the given symbol.
    *
    * For example:
    *
    * Print       =>  Eff$Print
    * List.Crash  =>  List.Eff$Crash
    */
  def getEffectDefinitionClassType(sym: Symbol.EffectSym)(implicit root: Root, flix: Flix): JvmType.Reference = {
    val pkg = sym.namespace
    val name = JvmName.mkClassName("Eff", sym.name)
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the op name of the given symbol.
    */
  def getEffectOpName(op: Symbol.OpSym): String = {
    mangle(op.name)
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
    val name = JvmName.mkClassName("Ns")
    JvmType.Reference(JvmName(pkg, name))
  }

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
    case JvmType.Reference(_) => "Obj"
  }

  /**
    * Returns the namespace info of the given definition symbol `sym`.
    */
  def getNamespace(sym: Symbol.DefnSym): NamespaceInfo = {
    NamespaceInfo(sym.namespace, Map.empty) // TODO: Magnus: Empty map.
  }

  /**
    * Returns the set of namespaces in the given AST `root`.
    */
  def namespacesOf(root: Root): Set[NamespaceInfo] = {
    // Group every symbol by namespace.
    root.defs.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        NamespaceInfo(ns, defs)
    }.toSet
  }

  /**
    * Returns true if the value of the given `tag` is the unit value.
    */
  def isUnitTag(tag: Case): Boolean = {
    tag.tpe == MonoType.Unit
  }

  /**
    * Returns the set of erased ref types in `types` without searching recursively.
    */
  def getErasedRefsOf(types: Iterable[MonoType]): Set[BackendObjType.Ref] =
    types.foldLeft(Set.empty[BackendObjType.Ref]) {
      case (acc, MonoType.Ref(tpe)) => acc + BackendObjType.Ref(BackendType.toErasedBackendType(tpe))
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased record extend types in `types` without searching recursively.
    */
  def getErasedRecordExtendsOf(types: Iterable[MonoType]): Set[BackendObjType.RecordExtend] =
    types.foldLeft(Set.empty[BackendObjType.RecordExtend]) {
      case (acc, MonoType.RecordExtend(field, value, _)) =>
        // TODO: should use mono -> backend transformation on `rest`
        acc + BackendObjType.RecordExtend(field, BackendType.toErasedBackendType(value), BackendObjType.RecordEmpty.toTpe)
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased function types in `types` without searching recursively.
    */
  def getErasedArrowsOf(types: Iterable[MonoType]): Set[BackendObjType.Arrow] =
    types.foldLeft(Set.empty[BackendObjType.Arrow]) {
      case (acc, MonoType.Arrow(args, result)) =>
        acc + BackendObjType.Arrow(args.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(result))
      case (acc, _) => acc
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
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.", SourceLocation.Unknown)
      }

      // Check that the file is empty or a class file.
      if (!(isEmpty(path) || isClassFile(path))) {
        throw InternalCompilerException(s"Refusing to overwrite non-empty, non-class file: '$path'.", SourceLocation.Unknown)
      }
    }

    // Write the bytecode.
    Files.write(path, clazz.bytecode)
  }

  /**
    * Returns `true` if the given `path` is non-empty (i.e. contains data).
    */
  private def isEmpty(path: Path): Boolean = Files.size(path) == 0L

  /**
    * Returns `true` if the given `path` exists and is a Java Virtual Machine class file.
    */
  private def isClassFile(path: Path): Boolean = {
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

}
