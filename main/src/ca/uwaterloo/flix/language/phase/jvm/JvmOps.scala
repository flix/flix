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

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
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

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
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
    val name = s"Clo${Flix.Delimiter}${mangle(sym.name)}"

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
      JvmType.Reference(JvmName(sym.namespace, "I" + sym.name + Flix.Delimiter))
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
    val name = sym.enumSym.name + Flix.Delimiter + sym.name
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
    * Returns true if the value of the given `tag` is the unit value.
    */
  def isUnitTag(tag: Case): Boolean = {
    tag.tpe == MonoType.Unit
  }

  /**
    * Returns the set of erased ref types in `types` without searching recursively.
    */
  def getErasedRefsOf(types: Iterable[MonoType])(implicit flix: Flix, root: Root): Set[BackendObjType.Ref] =
    types.foldLeft(Set.empty[BackendObjType.Ref]) {
      case (acc, MonoType.Ref(tpe)) => acc + BackendObjType.Ref(BackendType.toErasedBackendType(tpe))
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased record extend types in `types` without searching recursively.
    */
  def getErasedRecordExtendsOf(types: Iterable[MonoType])(implicit flix: Flix, root: Root): Set[BackendObjType.RecordExtend] =
    types.foldLeft(Set.empty[BackendObjType.RecordExtend]) {
      case (acc, MonoType.RecordExtend(field, value, _)) =>
        // TODO: should use mono -> backend transformation on `rest`
        acc + BackendObjType.RecordExtend(field, BackendType.toErasedBackendType(value), BackendObjType.RecordEmpty.toTpe)
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased function types in `types` without searching recursively.
    */
  def getErasedArrowsOf(types: Iterable[MonoType])(implicit flix: Flix, root: Root): Set[BackendObjType.Arrow] =
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
      // Compute the types in the captured formal parameters.
      val cParamTypes = defn.cparams.foldLeft(Set.empty[MonoType]) {
        case (sacc, FormalParam(_, _, tpe, _)) => sacc + tpe
      }

      // Compute the types in the expression.
      val expressionTypes = visitStmt(defn.stmt)

      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`

      // Return the types in the defn.
      cParamTypes ++ expressionTypes + defn.arrowType
    }

    def visitExps(exps: Iterable[Expr]): Set[MonoType] = {
      exps.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }
    }

    /**
      * Returns the set of types which occur in the given expression `exp0`.
      */
    def visitExp(exp0: Expr): Set[MonoType] = (exp0 match {
      case Expr.Cst(_, tpe, _) => Set(tpe)

      case Expr.Var(_, tpe, _) => Set(tpe)

      case Expr.ApplyClo(exp, exps, _, tpe, _, _) => visitExp(exp) ++ visitExps(exps) ++ Set(tpe)

      case Expr.ApplyDef(_, exps, _, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case Expr.ApplySelfTail(_, _, exps, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expr.Branch(exp, branches, _, _, _) =>
        val exps = branches.map {
          case (_, e) => e
        }
        visitExp(exp) ++ visitExps(exps)

      case Expr.JumpTo(_, _, _, _) => Set.empty

      case Expr.Let(_, exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expr.LetRec(_, _, _, exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case Expr.Scope(_, exp, _, _, _) => visitExp(exp)

      case Expr.TryCatch(exp, rules, _, _, _) => visitExp(exp) ++ visitExps(rules.map(_.exp))

      case Expr.TryWith(exp, _, rules, _, _, _) => visitExp(exp) ++ visitExps(rules.map(_.exp))

      case Expr.Do(_, exps, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case Expr.Resume(exp, tpe, _) => visitExp(exp) ++ Set(tpe)

      case Expr.NewObject(_, _, _, _, _, exps, _) =>
        visitExps(exps)

      case Expr.ApplyAtomic(_, exps, tpe, _, _) => visitExps(exps) + tpe

    }) ++ Set(exp0.tpe)

    /**
      * Returns the set of types which occur in the given expression `exp0`.
      */
    def visitStmt(s: Stmt): Set[MonoType] = s match {
      case Stmt.Ret(e, tpe, loc) => visitExp(e)
    }

    // TODO: Magnus: Look for types in other places.

    // Visit every definition.
    val defTypes = root.defs.foldLeft(Set.empty[MonoType]) {
      case (sacc, (_, defn)) => sacc ++ visitDefn(defn)
    }

    val enumTypes = root.enums.foldLeft(Set.empty[MonoType]) {
      case (sacc, (_, e)) =>
        // the enum type itself
        val eType = e.tpe
        // the types inside the cases
        val caseTypes = e.cases.values.flatMap(c => nestedTypesOf(c.tpe)(root, flix))
        sacc + eType ++ caseTypes
    }

    val result = defTypes ++ enumTypes

    result.flatMap(t => nestedTypesOf(t)(root, flix))
  }

  /**
    * Returns all the type components of the given type `tpe`.
    *
    * For example, if the given type is `Array[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Array[(Bool, Char, Int)]`.
    */
  def nestedTypesOf(tpe: MonoType)(implicit root: Root, flix: Flix): Set[MonoType] = {
    tpe match {
      case MonoType.Unit => Set(tpe)
      case MonoType.Bool => Set(tpe)
      case MonoType.Char => Set(tpe)
      case MonoType.Float32 => Set(tpe)
      case MonoType.Float64 => Set(tpe)
      case MonoType.BigDecimal => Set(tpe)
      case MonoType.Int8 => Set(tpe)
      case MonoType.Int16 => Set(tpe)
      case MonoType.Int32 => Set(tpe)
      case MonoType.Int64 => Set(tpe)
      case MonoType.BigInt => Set(tpe)
      case MonoType.String => Set(tpe)
      case MonoType.Regex => Set(tpe)
      case MonoType.Region => Set(tpe)

      case MonoType.Array(elm) => nestedTypesOf(elm) + tpe
      case MonoType.Lazy(elm) => nestedTypesOf(elm) + tpe
      case MonoType.Ref(elm) => nestedTypesOf(elm) + tpe
      case MonoType.Tuple(elms) => elms.flatMap(nestedTypesOf).toSet + tpe
      case MonoType.Enum(_) => Set(tpe)
      case MonoType.Arrow(targs, tresult) => targs.flatMap(nestedTypesOf).toSet ++ nestedTypesOf(tresult) + tpe

      case MonoType.RecordEmpty => Set(tpe)
      case MonoType.RecordExtend(_, value, rest) => Set(tpe) ++ nestedTypesOf(value) ++ nestedTypesOf(rest)

      case MonoType.SchemaEmpty => Set(tpe)
      case MonoType.SchemaExtend(_, t, rest) => nestedTypesOf(t) ++ nestedTypesOf(rest) + t + rest

      case MonoType.Native(_) => Set(tpe)
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
