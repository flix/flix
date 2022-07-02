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
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.{Ast, Kind, MonoType, Name, Rigidity, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.Finalize
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.InternalCompilerException

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
    * Option[Int]           =>      Option$Int
    * Result[Bool, Int]     =>      Result$Bool$Int
    * Int -> Bool           =>      Fn1$Int$Bool
    * (Int, Int) -> Bool    =>      Fn2$Int$Int$Bool
    */
  def getJvmType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType = tpe match {
    // Polymorphic
    case MonoType.Var(_) => JvmType.Unit

    // Primitives
    case MonoType.Unit => JvmType.Unit
    case MonoType.Bool => JvmType.PrimBool
    case MonoType.Char => JvmType.PrimChar
    case MonoType.Float32 => JvmType.PrimFloat
    case MonoType.Float64 => JvmType.PrimDouble
    case MonoType.Int8 => JvmType.PrimByte
    case MonoType.Int16 => JvmType.PrimShort
    case MonoType.Int32 => JvmType.PrimInt
    case MonoType.Int64 => JvmType.PrimLong
    case MonoType.BigInt => JvmType.BigInteger
    case MonoType.Str => JvmType.String

    // Compound
    case MonoType.Array(_) => JvmType.Object
    case MonoType.Channel(_) => JvmType.Object
    case MonoType.Lazy(_) => JvmType.Object
    case MonoType.Ref(_) => getRefClassType(tpe)
    case MonoType.Tuple(_) => getTupleClassType(tpe.asInstanceOf[MonoType.Tuple])
    case MonoType.RecordEmpty() => getRecordInterfaceType()
    case MonoType.RecordExtend(_, _, _) => getRecordInterfaceType()
    case MonoType.Enum(_, _) => getEnumInterfaceType(tpe)
    case MonoType.Arrow(_, _) => getFunctionInterfaceType(tpe)
    case MonoType.Native(clazz) =>
      // TODO: Ugly hack.
      val fqn = clazz.getName.replace('.', '/')
      JvmType.Reference(JvmName.mk(fqn))

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }


  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    */
  def getErasedJvmType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType = {
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
      case JvmType.Reference(_) => JvmType.Object
    }

    erase(getJvmType(tpe))
  }

  /**
    * Returns the continuation class type `Cont$X` for the given type `tpe`.
    *
    * Int -> Int          =>  Cont$Int
    * (Int, Int) -> Int   =>  Cont$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getContinuationInterfaceType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Arrow(_, tresult) =>
      // The return type is the last type argument.
      val returnType = JvmOps.getErasedJvmType(tresult)

      // The JVM name is of the form Cont$ErasedType
      val name = "Cont" + Flix.Delimiter + stringify(returnType)

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Returns the function abstract class type `FnX$Y$Z` for the given type `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Fn2$Int$Int
    * (Int, Int) -> Int   =>  Fn3$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getFunctionInterfaceType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      // Compute the arity of the function abstract class.
      // We subtract one since the last argument is the return type.
      val arity = targs.length

      // Compute the stringified erased type of each type argument.
      val args = (targs ::: tresult :: Nil).map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form FnArity$Arg0$Arg1$Arg2
      val name = "Fn" + arity + Flix.Delimiter + args.mkString(Flix.Delimiter)

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Returns the closure abstract class type `CloX$Y$Z` for the given type `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Clo2$Int$Int
    * (Int, Int) -> Int   =>  Clo3$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getClosureAbstractClassType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      // Compute the arity of the function abstract class.
      // We subtract one since the last argument is the return type.
      val arity = targs.length

      // Compute the stringified erased type of each type argument.
      val args = (targs ::: tresult :: Nil).map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form FnArity$Arg0$Arg1$Arg2
      val name = "Clo" + arity + Flix.Delimiter + args.mkString(Flix.Delimiter)

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Returns the closure class `Clo$Name` for the given closure.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.length       =>    List/Clo$length
    * List.map          =>    List/Clo$map
    */
  def getClosureClassType(sym: Symbol.DefnSym, tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Arrow(_, _) =>
      // The JVM name is of the form Clo$sym.name
      val name = s"Clo${Flix.Delimiter}${mangle(sym.name)}"

      // The JVM package is the namespace of the symbol.
      val pkg = sym.namespace

      // The result type.
      JvmType.Reference(JvmName(pkg, name))

    case tpe => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
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
  def getEnumInterfaceType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Enum(sym, elms) =>
      // Compute the stringified erased type of each type argument.
      val args = elms.map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form Option$ or Option$Int
      val name = if (args.isEmpty) "I" + sym.name else "I" + sym.name + Flix.Delimiter + args.mkString(Flix.Delimiter)

      // The enum resides in its namespace package.
      JvmType.Reference(JvmName(sym.namespace, name))

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
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
    val name = tag.sym.name + Flix.Delimiter + (if (args.isEmpty) tagName else tagName + Flix.Delimiter + args.mkString(Flix.Delimiter))

    // The tag class resides in its namespace package.
    JvmType.Reference(JvmName(tag.sym.namespace, name))
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
      val name = "Tuple" + arity + Flix.Delimiter + args.mkString(Flix.Delimiter)

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))
  }

  def getLazyClassType(tpe: MonoType.Lazy)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Lazy(tpe) =>
      val arg = stringify(getErasedJvmType(tpe))
      val name = "Lazy" + Flix.Delimiter + arg
      JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the record interface type `IRecord`.
    *
    * For example,
    *
    * {}                    =>  IRecord
    * {x :: Int}            =>  IRecord
    * {x :: Str, y :: Int}  =>  IRecord
    */
  def getRecordInterfaceType()(implicit root: Root, flix: Flix): JvmType.Reference = {

    // The JVM name is of the form IRecord
    val name = s"IRecord${Flix.Delimiter}"

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
    val name = "RecordEmpty"

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
      val name = "RecordExtend" + Flix.Delimiter + valueType

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
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
    val name = "RecordExtend" + Flix.Delimiter + valueType

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
      val name = "Ref" + Flix.Delimiter + arg

      // The type resides in the ca.uwaterloo.flix.api.cell package.
      JvmType.Reference(JvmName(Nil, name))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
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
    val name = "Def" + Flix.Delimiter + mangle(sym.name)
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
    * Returns the method name of a defn as used in a namespace class.
    *
    * For example:
    *
    * find      =>  m_find
    * length    =>  m_length
    */
  def getDefMethodNameInNamespaceClass(sym: Symbol.DefnSym): String = "m_" + mangle(sym.name)

  /**
    * Performs name mangling on the given string `s` to avoid issues with special characters.
    */
  // TODO: Magnus: Use this in appropriate places.
  def mangle(s: String): String = s.
    replace("+", Flix.Delimiter + "plus").
    replace("-", Flix.Delimiter + "minus").
    replace("*", Flix.Delimiter + "asterisk").
    replace("/", Flix.Delimiter + "fslash").
    replace("\\", Flix.Delimiter + "bslash").
    replace("<", Flix.Delimiter + "less").
    replace(">", Flix.Delimiter + "greater").
    replace("=", Flix.Delimiter + "eq").
    replace("&", Flix.Delimiter + "ampersand").
    replace("|", Flix.Delimiter + "bar").
    replace("^", Flix.Delimiter + "caret").
    replace("~", Flix.Delimiter + "tilde").
    replace("!", Flix.Delimiter + "exclamation").
    replace("#", Flix.Delimiter + "hashtag").
    replace(":", Flix.Delimiter + "colon").
    replace("?", Flix.Delimiter + "question").
    replace("@", Flix.Delimiter + "at")

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
    * Returns the set of closures in the given AST `root`.
    */
  def closuresOf(root: Root): Set[ClosureInfo] = {
    /**
      * Returns the set of closures in the given expression `exp0`.
      */
    def visitExp(exp0: Expression): Set[ClosureInfo] = exp0 match {
      case Expression.Unit(_) => Set.empty

      case Expression.Null(_, _) => Set.empty

      case Expression.True(_) => Set.empty

      case Expression.False(_) => Set.empty

      case Expression.Char(_, _) => Set.empty

      case Expression.Float32(_, _) => Set.empty

      case Expression.Float64(_, _) => Set.empty

      case Expression.Int8(_, _) => Set.empty

      case Expression.Int16(_, _) => Set.empty

      case Expression.Int32(_, _) => Set.empty

      case Expression.Int64(_, _) => Set.empty

      case Expression.BigInt(_, _) => Set.empty

      case Expression.Str(_, _) => Set.empty

      case Expression.Var(_, _, _) => Set.empty

      case Expression.Closure(sym, closureArgs, _, tpe, _) =>
        val closureInfo = closureArgs.foldLeft(Set.empty[ClosureInfo]) {
          case (sacc, e) => sacc ++ visitExp(e)
        }
        Set(ClosureInfo(sym, closureArgs.map(_.tpe), tpe)) ++ closureInfo

      case Expression.ApplyClo(exp, args, _, _) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(_, args, _, _) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, _, _) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(_, args, _, _) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(_, _, args, _, _) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.Unary(_, _, exp, _, _) =>
        visitExp(exp)

      case Expression.Binary(_, _, exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Branch(exp, branches, _, _) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }

      case Expression.JumpTo(_, _, _) => Set.empty

      case Expression.Let(_, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.LetRec(_, _, _, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Is(_, _, exp, _) => visitExp(exp)

      case Expression.Tag(_, _, exp, _, _) => visitExp(exp)

      case Expression.Untag(_, _, exp, _, _) => visitExp(exp)

      case Expression.Index(base, _, _, _) => visitExp(base)

      case Expression.Tuple(elms, _, _) => elms.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.RecordEmpty(_, _) => Set.empty

      case Expression.RecordExtend(_, value, rest, _, _) => visitExp(value) ++ visitExp(rest)

      case Expression.RecordSelect(exp, _, _, _) => visitExp(exp)

      case Expression.RecordRestrict(_, rest, _, _) => visitExp(rest)

      case Expression.ArrayLit(elms, _, _) => elms.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayNew(elm, len, _, _) => visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.ArrayStore(exp1, exp2, exp3, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.ArrayLength(exp, _, _) => visitExp(exp)

      case Expression.ArraySlice(exp1, exp2, exp3, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Ref(exp, _, _) => visitExp(exp)

      case Expression.Deref(exp, _, _) => visitExp(exp)

      case Expression.Assign(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Cast(exp, _, _) => visitExp(exp)

      case Expression.TryCatch(exp, rules, _, _) =>
        rules.foldLeft(visitExp(exp)) {
          case (sacc, CatchRule(_, _, body)) => sacc ++ visitExp(body)
        }

      case Expression.InvokeConstructor(_, args, _, _) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.InvokeMethod(_, exp, args, _, _) =>
        args.foldLeft(visitExp(exp)) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.InvokeStaticMethod(_, args, _, _) =>
        args.foldLeft(Set.empty[ClosureInfo]) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.GetField(_, exp, _, _) =>
        visitExp(exp)

      case Expression.PutField(_, exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.GetStaticField(_, _, _) =>
        Set.empty

      case Expression.PutStaticField(_, exp, _, _) =>
        visitExp(exp)

      case Expression.NewObject(_, _, _) =>
        Set.empty

      case Expression.NewChannel(exp, _, _) => visitExp(exp)

      case Expression.GetChannel(exp, _, _) => visitExp(exp)

      case Expression.PutChannel(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.SelectChannel(rules, default, _, _) =>
        val rs = rules.foldLeft(Set.empty[ClosureInfo])((old, rule) =>
          old ++ visitExp(rule.chan) ++ visitExp(rule.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.Spawn(exp, _, _) => visitExp(exp)

      case Expression.Lazy(exp, _, _) => visitExp(exp)

      case Expression.Force(exp, _, _) => visitExp(exp)

      case Expression.HoleError(_, _, _) => Set.empty

      case Expression.MatchError(_, _) => Set.empty

      case Expression.BoxBool(exp, _) => visitExp(exp)

      case Expression.BoxInt8(exp, _) => visitExp(exp)

      case Expression.BoxInt16(exp, _) => visitExp(exp)

      case Expression.BoxInt32(exp, _) => visitExp(exp)

      case Expression.BoxInt64(exp, _) => visitExp(exp)

      case Expression.BoxChar(exp, _) => visitExp(exp)

      case Expression.BoxFloat32(exp, _) => visitExp(exp)

      case Expression.BoxFloat64(exp, _) => visitExp(exp)

      case Expression.UnboxBool(exp, _) => visitExp(exp)

      case Expression.UnboxInt8(exp, _) => visitExp(exp)

      case Expression.UnboxInt16(exp, _) => visitExp(exp)

      case Expression.UnboxInt32(exp, _) => visitExp(exp)

      case Expression.UnboxInt64(exp, _) => visitExp(exp)

      case Expression.UnboxChar(exp, _) => visitExp(exp)

      case Expression.UnboxFloat32(exp, _) => visitExp(exp)

      case Expression.UnboxFloat64(exp, _) => visitExp(exp)
    }

    // TODO: Look for closures in other places.

    // Visit every definition.
    root.defs.foldLeft(Set.empty[ClosureInfo]) {
      case (sacc, (_, defn)) => sacc ++ visitExp(defn.exp)
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
        NamespaceInfo(ns, defs)
    }.toSet
  }

  /**
    * Returns the set of tags associated with the given type.
    */
  def getTagsOf(tpe: MonoType)(implicit root: Root, flix: Flix): Set[TagInfo] = tpe match {
    case enumType@MonoType.Enum(_, args) =>
      // Retrieve the enum.
      val enum0 = root.enums(enumType.sym)

      // Compute the tag info.
      enum0.cases.foldLeft(Set.empty[TagInfo]) {
        case (sacc, (_, Case(enumSym, tagName, uninstantiatedTagType, _))) =>
          // TODO: Magnus: It would be nice if this information could be stored somewhere...
          val subst = Unification.unifyTypes(hackMonoType2Type(enum0.tpeDeprecated), hackMonoType2Type(tpe), RigidityEnv.empty).get
          val tagType = subst(hackMonoType2Type(uninstantiatedTagType))

          sacc + TagInfo(enumSym, tagName.name, args, tpe, hackType2MonoType(tagType))
      }
    case _ => Set.empty
  }

  // TODO: Should be removed.
  private def hackMonoType2Type(tpe: MonoType): Type = tpe match {
    case MonoType.Var(id) => Type.KindedVar(hackId2TypeVarSym(id), SourceLocation.Unknown)
    case MonoType.Unit => Type.Unit
    case MonoType.Bool => Type.Bool
    case MonoType.Char => Type.Char
    case MonoType.Float32 => Type.Float32
    case MonoType.Float64 => Type.Float64
    case MonoType.Int8 => Type.Int8
    case MonoType.Int16 => Type.Int16
    case MonoType.Int32 => Type.Int32
    case MonoType.Int64 => Type.Int64
    case MonoType.BigInt => Type.BigInt
    case MonoType.Str => Type.Str
    case MonoType.Array(elm) => Type.mkArray(hackMonoType2Type(elm), Type.Impure, SourceLocation.Unknown)
    case MonoType.Lazy(tpe) => Type.mkLazy(hackMonoType2Type(tpe), SourceLocation.Unknown)
    case MonoType.Channel(elm) => Type.mkChannel(hackMonoType2Type(elm), SourceLocation.Unknown)
    case MonoType.Native(clazz) => Type.mkNative(clazz, SourceLocation.Unknown)
    case MonoType.Ref(elm) => Type.mkRef(hackMonoType2Type(elm), Type.False, SourceLocation.Unknown)
    case MonoType.Arrow(targs, tresult) => Type.mkPureCurriedArrow(targs map hackMonoType2Type, hackMonoType2Type(tresult), SourceLocation.Unknown)
    case MonoType.Enum(sym, args) => Type.mkEnum(sym, args.map(hackMonoType2Type), SourceLocation.Unknown)
    case MonoType.Relation(attr) => Type.mkRelation(attr.map(hackMonoType2Type), SourceLocation.Unknown)
    case MonoType.Lattice(attr) => Type.mkLattice(attr.map(hackMonoType2Type), SourceLocation.Unknown)
    case MonoType.Tuple(_) => Type.mkTuple(Nil, SourceLocation.Unknown) // hack
    case MonoType.RecordEmpty() => Type.mkRecord(Type.RecordRowEmpty, SourceLocation.Unknown)
    case MonoType.RecordExtend(_, _, _) => Type.mkRecord(hackMonoType2RecordRowType(tpe), SourceLocation.Unknown)
    case MonoType.SchemaEmpty() => Type.mkSchema(Type.RecordRowEmpty, SourceLocation.Unknown)
    case MonoType.SchemaExtend(_, _, _) => Type.mkSchema(hackMonoType2SchemaRowType(tpe), SourceLocation.Unknown)
  }

  // TODO: Remove
  private def hackMonoType2RecordRowType(tpe: MonoType): Type = tpe match {
    case MonoType.RecordExtend(field, value, rest) => Type.mkRecordRowExtend(Name.Field(field, SourceLocation.Unknown), hackMonoType2Type(value), hackMonoType2RecordRowType(rest), SourceLocation.Unknown)
    case MonoType.RecordEmpty() => Type.RecordRowEmpty
    case MonoType.Var(id) => Type.KindedVar(hackId2TypeVarSym(id), SourceLocation.Unknown)
    case _ => throw InternalCompilerException("Unexpected non-row type.")
  }

  // TODO: Remove
  private def hackMonoType2SchemaRowType(tpe: MonoType): Type = tpe match {
    case MonoType.SchemaExtend(sym, t, rest) => Type.mkSchemaRowExtend(Name.Pred(sym, SourceLocation.Unknown), hackMonoType2Type(t), hackMonoType2SchemaRowType(rest), SourceLocation.Unknown)
    case MonoType.SchemaEmpty() => Type.SchemaRowEmpty
    case MonoType.Var(id) => Type.KindedVar(hackId2TypeVarSym(id), SourceLocation.Unknown)
    case _ => throw InternalCompilerException("Unexpected non-row type.")
  }

  // TODO: Remove
  private def hackType2MonoType(tpe: Type): MonoType = Finalize.visitType(tpe)

  // TODO: Remove
  private def hackId2TypeVarSym(id: Int): Symbol.KindedTypeVarSym = new Symbol.KindedTypeVarSym(id, Ast.VarText.Absent, Kind.Wild, isRegion = false, SourceLocation.Unknown)

  /**
    * Returns the tag info for the given `tpe` and `tag`
    */
  // TODO: Magnus: Should use getTags and then just find the correct tag.
  def getTagInfo(tpe: MonoType, tag: String)(implicit root: Root, flix: Flix): TagInfo = tpe match {
    case MonoType.Enum(_, _) =>
      val tags = getTagsOf(tpe)
      tags.find(_.tag == tag).get
    case _ => throw InternalCompilerException(s"Unexpected type: $tpe")
  }

  /**
    * Returns true if the value of the given `tag` is the unit value.
    */
  def isUnitTag(tag: TagInfo): Boolean = {
    tag.tagType == MonoType.Unit
  }

  /**
    * Returns the set of tags in the given AST `root`.
    */
  def tagsOf(root: Root)(implicit flix: Flix): Set[TagInfo] = {
    typesOf(root).flatMap(tpe => getTagsOf(tpe)(root, flix))
  }

  /**
    * Returns the set of ref types in `types` without searching recursively.
    */
  def getRefsOf(types: Iterable[MonoType])(implicit flix: Flix, root: Root): Set[BackendObjType.Ref] =
    types.foldLeft(Set.empty[BackendObjType.Ref]) {
      case (acc, MonoType.Ref(tpe)) => acc + BackendObjType.Ref(BackendType.toErasedBackendType(tpe))
      case (acc, _) => acc
    }

  /**
    * Returns the set of record extend types in `types` without searching recursively.
    */
  def getRecordExtendsOf(types: Iterable[MonoType])(implicit flix: Flix, root: Root): Set[BackendObjType.RecordExtend] =
    types.foldLeft(Set.empty[BackendObjType.RecordExtend]) {
      case (acc, MonoType.RecordExtend(field, value, _)) =>
        // TODO: should use mono -> backend transformation on `rest`
        acc + BackendObjType.RecordExtend(field, BackendType.toErasedBackendType(value), BackendObjType.RecordEmpty.toTpe)
      case (acc, _) => acc
    }

  /**
    * Returns the set of function types in `types` without searching recursively.
    */
  def getArrowsOf(types: Iterable[MonoType])(implicit flix: Flix, root: Root): Set[BackendObjType.Arrow] =
    types.foldLeft(Set.empty[BackendObjType.Arrow]) {
      case (acc, MonoType.Arrow(args, result)) =>
        acc + BackendObjType.Arrow(args.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(result))
      case (acc, _) => acc
    }

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  def typesOf(root: Root)(implicit flix: Flix): Set[MonoType] = {
    /**
      * Returns the set of types which occur in the given definition `defn0`.
      */
    def visitDefn(defn: Def): Set[MonoType] = {
      // Compute the types in the formal parameters.
      val formalParamTypes = defn.formals.foldLeft(Set.empty[MonoType]) {
        case (sacc, FormalParam(_, tpe)) => sacc + tpe
      }

      // Compute the types in the expression.
      val expressionTypes = visitExp(defn.exp)

      // Return the types in the defn.
      formalParamTypes ++ expressionTypes + defn.tpe
    }

    /**
      * Returns the set of types which occur in the given expression `exp0`.
      */
    def visitExp(exp0: Expression): Set[MonoType] = (exp0 match {
      case Expression.Unit(_) => Set.empty

      case Expression.Null(_, _) => Set.empty

      case Expression.True(_) => Set.empty

      case Expression.False(_) => Set.empty

      case Expression.Char(_, _) => Set.empty

      case Expression.Float32(_, _) => Set.empty

      case Expression.Float64(_, _) => Set.empty

      case Expression.Int8(_, _) => Set.empty

      case Expression.Int16(_, _) => Set.empty

      case Expression.Int32(_, _) => Set.empty

      case Expression.Int64(_, _) => Set.empty

      case Expression.BigInt(_, _) => Set.empty

      case Expression.Str(_, _) => Set.empty

      case Expression.Var(_, _, _) => Set.empty

      case Expression.Closure(_, closureArgs, _, _, _) => closureArgs.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyClo(exp, args, _, _) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(_, args, _, _) => args.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, _, _) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(_, args, _, _) => args.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(_, _, args, _, _) => args.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.Unary(_, _, exp, _, _) =>
        visitExp(exp)

      case Expression.Binary(_, _, exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Branch(exp, branches, _, _) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }

      case Expression.JumpTo(_, _, _) => Set.empty

      case Expression.Let(_, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.LetRec(_, _, _, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Is(_, _, exp, _) => visitExp(exp)

      case Expression.Tag(_, _, exp, _, _) => visitExp(exp)

      case Expression.Untag(_, _, exp, _, _) => visitExp(exp)

      case Expression.Index(base, _, _, _) => visitExp(base)

      case Expression.Tuple(elms, _, _) => elms.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.RecordEmpty(_, _) => Set.empty

      case Expression.RecordSelect(exp, _, _, _) => visitExp(exp)

      case Expression.RecordExtend(_, value, rest, _, _) => visitExp(value) ++ visitExp(rest)

      case Expression.RecordRestrict(_, rest, _, _) => visitExp(rest)

      case Expression.ArrayLit(elms, _, _) => elms.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayNew(elm, len, _, _) => visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.ArrayStore(exp1, exp2, exp3, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.ArrayLength(exp, _, _) => visitExp(exp)

      case Expression.ArraySlice(exp1, exp2, exp3, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Ref(exp, _, _) => visitExp(exp)

      case Expression.Deref(exp, _, _) => visitExp(exp)

      case Expression.Assign(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Cast(exp, _, _) => visitExp(exp)

      case Expression.TryCatch(exp, rules, _, _) => rules.foldLeft(visitExp(exp)) {
        case (sacc, CatchRule(_, _, body)) => sacc ++ visitExp(body)
      }

      case Expression.InvokeConstructor(_, args, _, _) => args.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.InvokeMethod(_, exp, args, _, _) =>
        args.foldLeft(visitExp(exp)) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.InvokeStaticMethod(_, args, _, _) => args.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.GetField(_, exp, _, _) => visitExp(exp)

      case Expression.PutField(_, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.GetStaticField(_, _, _) => Set.empty

      case Expression.PutStaticField(_, exp, _, _) => visitExp(exp)

      case Expression.NewObject(_, _, _) => Set.empty

      case Expression.NewChannel(exp, _, _) => visitExp(exp)

      case Expression.GetChannel(exp, _, _) => visitExp(exp)

      case Expression.PutChannel(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.SelectChannel(rules, default, _, _) =>
        val rs = rules.foldLeft(Set.empty[MonoType])((old, rule) => old ++ visitExp(rule.chan) ++ visitExp(rule.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.Spawn(exp, _, _) => visitExp(exp)

      case Expression.Lazy(exp, _, _) => visitExp(exp)

      case Expression.Force(exp, _, _) => visitExp(exp)

      case Expression.HoleError(_, _, _) => Set.empty

      case Expression.MatchError(_, _) => Set.empty

      case Expression.BoxBool(exp, _) => visitExp(exp)

      case Expression.BoxInt8(exp, _) => visitExp(exp)

      case Expression.BoxInt16(exp, _) => visitExp(exp)

      case Expression.BoxInt32(exp, _) => visitExp(exp)

      case Expression.BoxInt64(exp, _) => visitExp(exp)

      case Expression.BoxChar(exp, _) => visitExp(exp)

      case Expression.BoxFloat32(exp, _) => visitExp(exp)

      case Expression.BoxFloat64(exp, _) => visitExp(exp)

      case Expression.UnboxBool(exp, _) => visitExp(exp)

      case Expression.UnboxInt8(exp, _) => visitExp(exp)

      case Expression.UnboxInt16(exp, _) => visitExp(exp)

      case Expression.UnboxInt32(exp, _) => visitExp(exp)

      case Expression.UnboxInt64(exp, _) => visitExp(exp)

      case Expression.UnboxChar(exp, _) => visitExp(exp)

      case Expression.UnboxFloat32(exp, _) => visitExp(exp)

      case Expression.UnboxFloat64(exp, _) => visitExp(exp)
    }) ++ Set(exp0.tpe)

    // TODO: Magnus: Look for types in other places.

    // Visit every definition.
    val result = root.defs.foldLeft(Set.empty[MonoType]) {
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
  def nestedTypesOf(tpe: MonoType)(implicit root: Root, flix: Flix): Set[MonoType] = {
    //
    // Check if the tag is an enum and if so, extract the types of its tags.
    // Usually this is not "necessary", but an enum might occur as a type,
    // but not have all its tags constructed as expressions.
    //
    tpe match {
      case MonoType.Unit => Set(tpe)
      case MonoType.Bool => Set(tpe)
      case MonoType.Char => Set(tpe)
      case MonoType.Float32 => Set(tpe)
      case MonoType.Float64 => Set(tpe)
      case MonoType.Int8 => Set(tpe)
      case MonoType.Int16 => Set(tpe)
      case MonoType.Int32 => Set(tpe)
      case MonoType.Int64 => Set(tpe)
      case MonoType.BigInt => Set(tpe)
      case MonoType.Str => Set(tpe)

      case MonoType.Array(elm) => nestedTypesOf(elm) + tpe
      case MonoType.Channel(elm) => nestedTypesOf(elm) + tpe
      case MonoType.Lazy(elm) => nestedTypesOf(elm) + tpe
      case MonoType.Ref(elm) => nestedTypesOf(elm) + tpe
      case MonoType.Tuple(elms) => elms.flatMap(nestedTypesOf).toSet + tpe
      case MonoType.Enum(_, args) =>
        // Case 1: The nested types are the type itself, its type arguments, and the types of the tags.
        val tagTypes = getTagsOf(tpe).map(_.tagType)

        args.foldLeft(Set(tpe) ++ tagTypes) {
          case (sacc, arg) => sacc ++ nestedTypesOf(arg)
        }
      case MonoType.Arrow(targs, tresult) => targs.flatMap(nestedTypesOf).toSet ++ nestedTypesOf(tresult) + tpe

      case MonoType.RecordEmpty() => Set(tpe)
      case MonoType.RecordExtend(_, value, rest) => Set(tpe) ++ nestedTypesOf(value) ++ nestedTypesOf(rest)

      case MonoType.SchemaEmpty() => Set(tpe)
      case MonoType.SchemaExtend(_, t, rest) => nestedTypesOf(t) ++ nestedTypesOf(rest) + t + rest

      case MonoType.Relation(attr) => attr.flatMap(nestedTypesOf).toSet + tpe
      case MonoType.Lattice(attr) => attr.flatMap(nestedTypesOf).toSet + tpe
      case MonoType.Native(_) => Set(tpe)
      case MonoType.Var(_) => Set.empty
    }
  }

  /**
    * Returns the set of all anonymous classes (NewObjects) in the given AST `root`.
    */
  def anonClassesOf(root: Root)(implicit flix: Flix): Set[Expression.NewObject] = {
    /**
      * Returns the set of anonymous classes which occur in the given definition `defn0`.
      */
    def visitDefn(defn: Def): Set[Expression.NewObject] = {
      visitExp(defn.exp)
    }

    /**
      * Returns the set of anonymouse classes which occur in the given expression `exp0`.
      */
    def visitExp(exp0: Expression): Set[Expression.NewObject] = (exp0 match {
      case Expression.Unit(_) => Set.empty

      case Expression.Null(_, _) => Set.empty

      case Expression.True(_) => Set.empty

      case Expression.False(_) => Set.empty

      case Expression.Char(_, _) => Set.empty

      case Expression.Float32(_, _) => Set.empty

      case Expression.Float64(_, _) => Set.empty

      case Expression.Int8(_, _) => Set.empty

      case Expression.Int16(_, _) => Set.empty

      case Expression.Int32(_, _) => Set.empty

      case Expression.Int64(_, _) => Set.empty

      case Expression.BigInt(_, _) => Set.empty

      case Expression.Str(_, _) => Set.empty

      case Expression.Var(_, _, _) => Set.empty

      case Expression.Closure(_, closureArgs, _, _, _) => closureArgs.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyClo(exp, args, _, _) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(_, args, _, _) => args.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, _, _) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(_, args, _, _) => args.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(_, _, args, _, _) => args.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.Unary(_, _, exp, _, _) =>
        visitExp(exp)

      case Expression.Binary(_, _, exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Branch(exp, branches, _, _) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }

      case Expression.JumpTo(_, _, _) => Set.empty

      case Expression.Let(_, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.LetRec(_, _, _, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Is(_, _, exp, _) => visitExp(exp)

      case Expression.Tag(_, _, exp, _, _) => visitExp(exp)

      case Expression.Untag(_, _, exp, _, _) => visitExp(exp)

      case Expression.Index(base, _, _, _) => visitExp(base)

      case Expression.Tuple(elms, _, _) => elms.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.RecordEmpty(_, _) => Set.empty

      case Expression.RecordSelect(exp, _, _, _) => visitExp(exp)

      case Expression.RecordExtend(_, value, rest, _, _) => visitExp(value) ++ visitExp(rest)

      case Expression.RecordRestrict(_, rest, _, _) => visitExp(rest)

      case Expression.ArrayLit(elms, _, _) => elms.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayNew(elm, len, _, _) => visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.ArrayStore(exp1, exp2, exp3, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.ArrayLength(exp, _, _) => visitExp(exp)

      case Expression.ArraySlice(exp1, exp2, exp3, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Ref(exp, _, _) => visitExp(exp)

      case Expression.Deref(exp, _, _) => visitExp(exp)

      case Expression.Assign(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Cast(exp, _, _) => visitExp(exp)

      case Expression.TryCatch(exp, rules, _, _) => rules.foldLeft(visitExp(exp)) {
        case (sacc, CatchRule(_, _, body)) => sacc ++ visitExp(body)
      }

      case Expression.InvokeConstructor(_, args, _, _) => args.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.InvokeMethod(_, exp, args, _, _) =>
        args.foldLeft(visitExp(exp)) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.InvokeStaticMethod(_, args, _, _) => args.foldLeft(Set.empty[Expression.NewObject]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.GetField(_, exp, _, _) => visitExp(exp)

      case Expression.PutField(_, exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.GetStaticField(_, _, _) => Set.empty

      case Expression.PutStaticField(_, exp, _, _) => visitExp(exp)

      case obj: Expression.NewObject => Set(obj)

      case Expression.NewChannel(exp, _, _) => visitExp(exp)

      case Expression.GetChannel(exp, _, _) => visitExp(exp)

      case Expression.PutChannel(exp1, exp2, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.SelectChannel(rules, default, _, _) =>
        val rs = rules.foldLeft(Set.empty[Expression.NewObject])((old, rule) => old ++ visitExp(rule.chan) ++ visitExp(rule.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.Spawn(exp, _, _) => visitExp(exp)

      case Expression.Lazy(exp, _, _) => visitExp(exp)

      case Expression.Force(exp, _, _) => visitExp(exp)

      case Expression.HoleError(_, _, _) => Set.empty

      case Expression.MatchError(_, _) => Set.empty

      case Expression.BoxBool(exp, _) => visitExp(exp)

      case Expression.BoxInt8(exp, _) => visitExp(exp)

      case Expression.BoxInt16(exp, _) => visitExp(exp)

      case Expression.BoxInt32(exp, _) => visitExp(exp)

      case Expression.BoxInt64(exp, _) => visitExp(exp)

      case Expression.BoxChar(exp, _) => visitExp(exp)

      case Expression.BoxFloat32(exp, _) => visitExp(exp)

      case Expression.BoxFloat64(exp, _) => visitExp(exp)

      case Expression.UnboxBool(exp, _) => visitExp(exp)

      case Expression.UnboxInt8(exp, _) => visitExp(exp)

      case Expression.UnboxInt16(exp, _) => visitExp(exp)

      case Expression.UnboxInt32(exp, _) => visitExp(exp)

      case Expression.UnboxInt64(exp, _) => visitExp(exp)

      case Expression.UnboxChar(exp, _) => visitExp(exp)

      case Expression.UnboxFloat32(exp, _) => visitExp(exp)

      case Expression.UnboxFloat64(exp, _) => visitExp(exp)
    })

    // Visit every definition.
    root.defs.foldLeft(Set.empty[Expression.NewObject]) {
      case (sacc, (_, defn)) => sacc ++ visitDefn(defn)
    }
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

      // Check that the file is empty or a class file.
      if (!(isEmpty(path) || isClassFile(path))) {
        throw InternalCompilerException(s"Refusing to overwrite non-empty, non-class file: '$path'.")
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
