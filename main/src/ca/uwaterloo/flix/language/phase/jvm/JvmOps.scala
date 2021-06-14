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
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.ast.{Kind, MonoType, Name, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.Finalize
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.InternalCompilerException

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
    case MonoType.Tuple(elms) => getTupleInterfaceType(tpe.asInstanceOf[MonoType.Tuple])
    case MonoType.RecordEmpty() => getRecordInterfaceType()
    case MonoType.RecordExtend(_, value, rest) => getRecordInterfaceType()
    case MonoType.Enum(sym, kind) => getEnumInterfaceType(tpe)
    case MonoType.Arrow(_, _) => getFunctionInterfaceType(tpe)
    case MonoType.Relation(attr) => JvmType.Reference(JvmName.PredSym)
    case MonoType.Native(clazz) =>
      // TODO: Ugly hack.
      val fqn = clazz.getCanonicalName.replace('.', '/')
      JvmType.Reference(JvmName.mk(fqn))
    case MonoType.SchemaEmpty() => JvmType.Reference(JvmName.Runtime.Fixpoint.ConstraintSystem)
    case MonoType.SchemaExtend(_, _, _) => JvmType.Reference(JvmName.Runtime.Fixpoint.ConstraintSystem)

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
      case JvmType.Reference(jvmName) => JvmType.Object
    }

    erase(getJvmType(tpe))
  }

  /**
    * Returns the continuation interface type `Cont$X` for the given type `tpe`.
    *
    * Int -> Int          =>  Cont$Int
    * (Int, Int) -> Int   =>  Cont$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getContinuationInterfaceType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      // The return type is the last type argument.
      val returnType = JvmOps.getErasedJvmType(tresult)

      // The JVM name is of the form Cont$ErasedType
      val name = "Cont$" + stringify(returnType)

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
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
  def getFunctionInterfaceType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      // Compute the arity of the function interface.
      // We subtract one since the last argument is the return type.
      val arity = targs.length

      // Compute the stringified erased type of each type argument.
      val args = (targs ::: tresult :: Nil).map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form FnArity$Arg0$Arg1$Arg2
      val name = "Fn" + arity + "$" + args.mkString("$")

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
  def getClosureClassType(closure: ClosureInfo)(implicit root: Root, flix: Flix): JvmType.Reference = closure.tpe match {
    case MonoType.Arrow(targs, tresult) =>
      // Compute the arity of the function interface.
      // We subtract one since the last argument is the return type.
      val arity = targs.length

      // Compute the stringified erased type of each type argument.
      val args = (targs ::: tresult :: Nil).map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form Clo$sym.name
      val name = "Clo" + "$" + mangle(closure.sym.name)

      // The JVM package is the namespace of the symbol.
      val pkg = closure.sym.namespace

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
      val name = if (args.isEmpty) "I" + sym.name else "I" + sym.name + "$" + args.mkString("$")

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
    val name = tag.sym.name + "$" + (if (args.isEmpty) tagName else tagName + "$" + args.mkString("$"))

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
  def getTupleInterfaceType(tpe: MonoType.Tuple)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Tuple(elms) =>
      // Compute the arity of the tuple.
      val arity = elms.length

      // Compute the stringified erased type of each type argument.
      val args = elms.map(tpe => stringify(getErasedJvmType(tpe)))

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
  def getTupleClassType(tpe: MonoType.Tuple)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Tuple(elms) =>
      // Compute the arity of the tuple.
      val arity = elms.length

      // Compute the stringified erased type of each type argument.
      val args = elms.map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form TupleArity$Arg0$Arg1$Arg2
      val name = "Tuple" + arity + "$" + args.mkString("$")

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))
  }

  def getLazyClassType(tpe: MonoType.Lazy)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {
    case MonoType.Lazy(tpe) =>
      val arg = stringify(getErasedJvmType(tpe))
      val name = "Lazy$" + arg
      JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the record interface type `IRecord`.
    *
    * For example,
    *
    * {}                  =>    IRecord
    * {x : Int}           =>    IRecord
    * {x : Str, y : Int}  =>    IRecord
    */
  def getRecordInterfaceType()(implicit root: Root, flix: Flix): JvmType.Reference = {

    // The JVM name is of the form IRecord
    val name = "IRecord"

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
    * {+z : Int  | {}}                =>    RecordExtend$Int
    * {+y : Char | {z : Int}          =>    RecordExtend$Char
    * {+x : Str |{y : Char, z : Int}  =>    RecordExtend$Obj
    *
    * NB: The given type `tpe` must be a Record type
    */
  def getRecordExtendClassType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = tpe match {

    case MonoType.RecordExtend(_, value, _) =>
      // Compute the stringified erased type of value.
      val valueType = stringify(getErasedJvmType(value))

      // The JVM name is of the form RecordExtend
      val name = "RecordExtend$" + valueType

      // The type resides in the root package.
      JvmType.Reference(JvmName(RootPackage, name))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }


  /**
    * Returns the extended record class type `RecordExtend$X` which contains the given type 'tpe'
    *
    * For example,
    *
    * Int                  =>    RecordExtend$Int
    * Char                 =>    RecordExtend$Char
    * {x : Char, y : Int}  =>    RecordExtend$Obj
    *
    */
  def getRecordType(tpe: MonoType)(implicit root: Root, flix: Flix): JvmType.Reference = {

    // Compute the stringified erased type of 'tpe'.
    val valueType = JvmOps.stringify(JvmOps.getErasedJvmType(tpe))

    // The JVM name is of the form RecordExtend
    val name = "RecordExtend$" + valueType

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
      val name = "Ref" + "$" + arg

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
      case Expression.Unit(_) => Set.empty

      case Expression.Null(tpe, _) => Set.empty

      case Expression.True(_) => Set.empty

      case Expression.False(_) => Set.empty

      case Expression.Char(lit, _) => Set.empty

      case Expression.Float32(lit, _) => Set.empty

      case Expression.Float64(lit, _) => Set.empty

      case Expression.Int8(lit, _) => Set.empty

      case Expression.Int16(lit, _) => Set.empty

      case Expression.Int32(lit, _) => Set.empty

      case Expression.Int64(lit, _) => Set.empty

      case Expression.BigInt(lit, _) => Set.empty

      case Expression.Str(lit, _) => Set.empty

      case Expression.Var(sym, tpe, loc) => Set.empty

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
        Set(ClosureInfo(sym, freeVars, tpe))

      case Expression.ApplyClo(exp, args, tpe, loc) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, tpe, loc) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
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

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp)

      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp)

      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp)

      case Expression.Index(base, offset, tpe, loc) => visitExp(base)

      case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.RecordEmpty(tpe, loc) => Set.empty

      case Expression.RecordExtend(_, value, rest, tpe, loc) => visitExp(value) ++ visitExp(rest)

      case Expression.RecordSelect(exp, _, tpe, loc) => visitExp(exp)

      case Expression.RecordRestrict(_, rest, tpe, loc) => visitExp(rest)

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

      case Expression.Existential(fparam, exp, loc) => visitExp(exp)

      case Expression.Universal(fparam, exp, loc) => visitExp(exp)

      case Expression.Cast(exp, tpe, loc) => visitExp(exp)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        rules.foldLeft(visitExp(exp)) {
          case (sacc, CatchRule(sym, clazz, body)) => sacc ++ visitExp(body)
        }

      case Expression.InvokeConstructor(constructor, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        args.foldLeft(visitExp(exp)) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        args.foldLeft(Set.empty[ClosureInfo]) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.GetField(field, exp, tpe, loc) =>
        visitExp(exp)

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.GetStaticField(field, tpe, loc) =>
        Set.empty

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        visitExp(exp)

      case Expression.NewChannel(exp, tpe, loc) => visitExp(exp)

      case Expression.GetChannel(exp, tpe, loc) => visitExp(exp)

      case Expression.PutChannel(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules.foldLeft(Set.empty[ClosureInfo])((old, rule) =>
          old ++ visitExp(rule.chan) ++ visitExp(rule.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.Spawn(exp, tpe, loc) => visitExp(exp)

      case Expression.Lazy(exp, tpe, loc) => visitExp(exp)

      case Expression.Force(exp, tpe, loc) => visitExp(exp)

      case Expression.HoleError(sym, tpe, loc) => Set.empty

      case Expression.MatchError(tpe, loc) => Set.empty
    }

    // TODO: Look for closures in other places.

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
  def getTagsOf(tpe: MonoType)(implicit root: Root, flix: Flix): Set[TagInfo] = tpe match {
    case enumType@MonoType.Enum(sym, args) =>
      // Retrieve the enum.
      val enum = root.enums(enumType.sym)

      // Compute the tag info.
      enum.cases.foldLeft(Set.empty[TagInfo]) {
        case (sacc, (_, Case(enumSym, tagName, uninstantiatedTagType, loc))) =>
          // TODO: Magnus: It would be nice if this information could be stored somewhere...
          val subst = Unification.unifyTypes(hackMonoType2Type(enum.tpeDeprecated), hackMonoType2Type(tpe)).get
          val tagType = subst(hackMonoType2Type(uninstantiatedTagType))

          sacc + TagInfo(enumSym, tagName.name, args, tpe, hackType2MonoType(tagType))
      }
    case _ => Set.empty
  }

  // TODO: Should be removed.
  private def hackMonoType2Type(tpe: MonoType): Type = tpe match {
    case MonoType.Var(id) => Type.Var(id, Kind.Star)
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
    case MonoType.Array(elm) => Type.mkArray(hackMonoType2Type(elm))
    case MonoType.Lazy(tpe) => Type.mkLazy(hackMonoType2Type(tpe))
    case MonoType.Channel(elm) => Type.mkChannel(hackMonoType2Type(elm))
    case MonoType.Native(clazz) => Type.mkNative(clazz)
    case MonoType.Ref(elm) => Type.mkScopedRef(hackMonoType2Type(elm), Type.False)
    case MonoType.Arrow(targs, tresult) => Type.mkPureCurriedArrow(targs map hackMonoType2Type, hackMonoType2Type(tresult))
    case MonoType.Enum(sym, args) => Type.mkEnum(sym, args.map(hackMonoType2Type))
    case MonoType.Relation(attr) => Type.mkRelation(attr.map(hackMonoType2Type))
    case MonoType.Lattice(attr) => Type.mkLattice(attr.map(hackMonoType2Type))
    case MonoType.Tuple(length) => Type.mkTuple(Nil) // hack
    case MonoType.RecordEmpty() => Type.RecordEmpty
    case MonoType.RecordExtend(field, value, rest) => Type.mkRecordExtend(Name.Field(field, SourceLocation.Unknown), hackMonoType2Type(value), hackMonoType2Type(rest))
    case MonoType.SchemaEmpty() => Type.SchemaEmpty
    case MonoType.SchemaExtend(sym, t, rest) => Type.mkSchemaExtend(Name.Pred(sym, SourceLocation.Unknown), hackMonoType2Type(t), hackMonoType2Type(rest))
  }

  // TODO: Remove
  private def hackType2MonoType(tpe: Type): MonoType = Finalize.visitType(tpe)

  /**
    * Returns the tag info for the given `tpe` and `tag`
    */
  // TODO: Magnus: Should use getTags and then just find the correct tag.
  def getTagInfo(tpe: MonoType, tag: String)(implicit root: Root, flix: Flix): TagInfo = tpe match {
    case enumType@MonoType.Enum(sym, _) =>
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
    def visitExp(exp0: Expression): Set[MonoType] = exp0 match {
      case Expression.Unit(_) => Set(MonoType.Unit)

      case Expression.Null(tpe, _) => Set(tpe)

      case Expression.True(_) => Set(MonoType.Bool)

      case Expression.False(_) => Set(MonoType.Bool)

      case Expression.Char(lit, _) => Set(MonoType.Char)

      case Expression.Float32(lit, _) => Set(MonoType.Float32)

      case Expression.Float64(lit, _) => Set(MonoType.Float64)

      case Expression.Int8(lit, _) => Set(MonoType.Int8)

      case Expression.Int16(lit, _) => Set(MonoType.Int16)

      case Expression.Int32(lit, _) => Set(MonoType.Int32)

      case Expression.Int64(lit, _) => Set(MonoType.Int64)

      case Expression.BigInt(lit, _) => Set(MonoType.BigInt)

      case Expression.Str(lit, _) => Set(MonoType.Str)

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

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        visitExp(exp) + tpe

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + tpe

      case Expression.Branch(exp, branches, tpe, loc) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }

      case Expression.JumpTo(sym, tpe, loc) => Set(tpe)

      case Expression.Let(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp)

      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Index(base, offset, tpe, loc) => visitExp(base) + tpe

      case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.RecordEmpty(tpe, loc) => Set(tpe)

      case Expression.RecordSelect(exp, _, tpe, loc) => Set(tpe) ++ visitExp(exp)

      case Expression.RecordExtend(_, value, rest, tpe, loc) => Set(tpe) ++ visitExp(value) ++ visitExp(rest)

      case Expression.RecordRestrict(_, rest, tpe, loc) => Set(tpe) ++ visitExp(rest)

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

      case Expression.Existential(fparam, exp, loc) => visitExp(exp) + fparam.tpe

      case Expression.Universal(fparam, exp, loc) => visitExp(exp) + fparam.tpe

      case Expression.Cast(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        rules.foldLeft(visitExp(exp)) {
          case (sacc, CatchRule(sym, clazz, body)) => sacc ++ visitExp(body)
        }

      case Expression.InvokeConstructor(constructor, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        args.foldLeft(visitExp(exp) + tpe) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        args.foldLeft(Set(tpe)) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.GetField(field, exp, tpe, loc) =>
        visitExp(exp) + tpe

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.GetStaticField(field, tpe, loc) =>
        Set(tpe)

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        visitExp(exp) + tpe

      case Expression.NewChannel(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.GetChannel(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.PutChannel(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules.foldLeft(Set(tpe))((old, rule) => old ++ visitExp(rule.chan) ++ visitExp(rule.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.Spawn(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Lazy(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Force(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.HoleError(sym, tpe, loc) => Set(tpe)

      case Expression.MatchError(tpe, loc) => Set(tpe)
    }

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
      case MonoType.Enum(sym, args) =>
        // Case 1: The nested types are the type itself, its type arguments, and the types of the tags.
        val tagTypes = getTagsOf(tpe).map(_.tagType)

        args.foldLeft(Set(tpe) ++ tagTypes) {
          case (sacc, arg) => sacc ++ nestedTypesOf(arg)
        }
      case MonoType.Arrow(targs, tresult) => targs.flatMap(nestedTypesOf).toSet ++ nestedTypesOf(tresult) + tpe

      case MonoType.RecordEmpty() => Set(tpe)
      case MonoType.RecordExtend(_, value, rest) => Set(tpe) ++ nestedTypesOf(value) ++ nestedTypesOf(rest)

      case MonoType.SchemaEmpty() => Set(tpe)
      case MonoType.SchemaExtend(sym, t, rest) => nestedTypesOf(t) ++ nestedTypesOf(rest) + t + rest

      case MonoType.Relation(attr) => attr.flatMap(nestedTypesOf).toSet + tpe
      case MonoType.Lattice(attr) => attr.flatMap(nestedTypesOf).toSet + tpe
      case MonoType.Native(_) => Set(tpe)
      case MonoType.Var(_) => Set.empty
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

  /**
    * Returns `true` if the given definition `defn` is a law.
    */
  def nonLaw(defn: Def): Boolean = !defn.ann.isLaw

}
