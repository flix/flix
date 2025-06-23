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

import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.ast.{MonoType, ReducedAst, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.mangle
import ca.uwaterloo.flix.util.InternalCompilerException

object JvmOps {

  /**
    * Returns the erased arrow type of `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Fn2$Int$Int
    * (Int, String) -> Int   =>  Fn3$Int$Obj$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getErasedFunctionInterfaceType(tpe: MonoType)(implicit root: Root): BackendObjType.Arrow = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      BackendObjType.Arrow(targs.map(BackendType.toErasedBackendType), BackendType.toBackendType(tresult))
    case _ =>
      throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  /**
    * Returns the erased closure abstract class type `CloX$Y$Z` for the given [[MonoType]].
    *
    * For example:
    *
    * Int -> Int          =>  Clo1$Int$Int
    * (Int, Int) -> Int   =>  Clo2$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getErasedClosureAbstractClassType(tpe: MonoType): BackendObjType.AbstractArrow = tpe match {
    case MonoType.Arrow(targs, tresult) =>
     BackendObjType.AbstractArrow(targs.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(tresult))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  /**
    * Returns the closure class `Clo$Name` for the given closure.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.length       =>    List/Clo$length
    * List.map          =>    List/Clo$map
    */
  def getClosureClassName(sym: Symbol.DefnSym): JvmName = {
    // The JVM name is of the form Clo$sym.name
    val name = JvmName.mkClassName(s"Clo", sym.name)

    // The JVM package is the namespace of the symbol.
    val pkg = sym.namespace

    // The result type.
    JvmName(pkg, name)
  }

  /**
    * Returns the effect definition class for the given symbol.
    *
    * For example:
    *
    * Print       =>  Eff$Print
    * List.Crash  =>  List.Eff$Crash
    */
  def getEffectDefinitionClassName(sym: Symbol.EffSym): JvmName = {
    val pkg = sym.namespace
    val name = JvmName.mkClassName("Eff", sym.name)
    JvmName(pkg, name)
  }

  /**
    * Returns the op name of the given symbol.
    */
  def getEffectOpName(op: Symbol.OpSym): String =
    mangle(op.name)

  def getTagName(name: String): String =
    mangle(name)

  /** Returns the set of namespaces in the given AST `root`. */
  def namespacesOf(root: Root): Set[NamespaceInfo] = {
    // Group every symbol by namespace.
    root.defs.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        NamespaceInfo(ns, defs)
    }.toSet
  }

  /** Returns the set of lazy types in `types` without searching recursively. */
  def getLazyTypesOf(types: Iterable[MonoType])(implicit root: Root): Set[BackendObjType.Lazy] =
    types.foldLeft(Set.empty[BackendObjType.Lazy]) {
      case (acc, MonoType.Lazy(tpe)) => acc + BackendObjType.Lazy(BackendType.toBackendType(tpe))
      case (acc, _) => acc
    }

  /** Returns the set of record extend types in `types` without searching recursively. */
  def getRecordExtendsOf(types: Iterable[MonoType])(implicit root: Root): Set[BackendObjType.RecordExtend] =
    types.foldLeft(Set.empty[BackendObjType.RecordExtend]) {
      case (acc, MonoType.RecordExtend(_, value, _)) =>
        acc + BackendObjType.RecordExtend(BackendType.toBackendType(value))
      case (acc, _) => acc
    }

  /** Returns the set of erased function types in `types` without searching recursively. */
  def getErasedArrowsOf(types: Iterable[MonoType]): Set[BackendObjType.Arrow] =
    types.foldLeft(Set.empty[BackendObjType.Arrow]) {
      case (acc, MonoType.Arrow(args, result)) =>
        acc + BackendObjType.Arrow(args.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(result))
      case (acc, _) => acc
    }

  /** Returns the set of tuple types in `types` without searching recursively. */
  def getTupleTypesOf(types: Iterable[MonoType])(implicit root: Root): Set[BackendObjType.Tuple] =
    types.foldLeft(Set.empty[BackendObjType.Tuple]) {
      case (acc, MonoType.Tuple(elms)) =>
        acc + BackendObjType.Tuple(elms.map(BackendType.toBackendType))
      case (acc, _) => acc
    }

  /** Returns the set of erased struct types in `types` without searching recursively. */
  def getErasedStructTypesOf(root: Root, types: Iterable[MonoType]): Set[BackendObjType.Struct] =
    types.foldLeft(Set.empty[BackendObjType.Struct]) {
      case (acc, MonoType.Struct(sym, targs)) =>
        acc + BackendObjType.Struct(instantiateStruct(sym, targs)(root))
      case (acc, _) => acc
    }

  /**
    * Returns the ordered list of struct fields based on the given type `sym[targs..]`. It is
    * assumed that both `targs` and the structs in `root` use erased types.
    *
    * Example:
    *   - `instantiateStruct(S, List(Int32, IO)) = List(Int32, Int32, Object)`
    *     for `struct S[t, r] { mut x: t, y: t, z: Object }`
    */
  def instantiateStruct(sym: Symbol.StructSym, targs: List[MonoType])(implicit root: ReducedAst.Root): List[BackendType] = {
    val struct = root.structs(sym)
    assert(struct.tparams.length == targs.length)
    val map = struct.tparams.map(_.sym).zip(targs).toMap
    struct.fields.map(field => instantiateType(map, field.tpe))
  }

  /**
    * Returns the set of erased tag types in `types` without searching recursively.
    */
  def getErasedTagTypesOf(types: Iterable[MonoType])(implicit root: ReducedAst.Root): Set[BackendObjType.TagType] =
    types.foldLeft(Set.empty[BackendObjType.TagType]) {
      case (acc0, MonoType.Enum(sym, targs)) =>
        val tags = instantiateEnum(root.enums(sym), targs)
        tags.foldLeft(acc0) {
          case (acc, (tagSym, Nil)) => acc + BackendObjType.NullaryTag(tagSym.name)
          case (acc, (_, tagElms)) => acc + BackendObjType.Tag(tagElms)
        }
      case (acc, _) => acc
    }

  /**
    * Returns the ordered list of enums terms based on the given type `sym[targs..]`. It is assumed
    * that both `targs` and the enums in `root` use erased types.
    *
    * Example:
    *   - `instantiateEnum(E, List(Char)) = Map(A -> List(Char, Object), B -> List(Int32))`
    *     for `enum E[t] { case A(t, Object) case B(Int32) }`
    */
  def instantiateEnum(enm: ReducedAst.Enum, targs: List[MonoType])(implicit root: Root): Map[Symbol.CaseSym, List[BackendType]] = {
    assert(enm.tparams.length == targs.length)
    val map = enm.tparams.map(_.sym).zip(targs).toMap
    enm.cases.map {
      case (_, caze) => (caze.sym, caze.tpes.map(instantiateType(map, _)))
    }
  }

  /**
    * Instantiates `tpe` given the variable map `m`.
    *
    * Examples:
    *   - `instantiateType([x -> Int32], x) = Int32`
    *   - `instantiateType(_, Int32) = Int32`
    *   - `instantiateType(_, Object) = Object`
    *   - `instantiateType([x -> String], x) = throw InternalCompilerException`
    *   - `instantiateType([x -> Int32], y) = throw InternalCompilerException`
    *   - `instantiateType(_, Option[Int32]) =  throw InternalCompilerException`
    *
    * @param m Decides types for variables, must only contain erased types.
    * @param tpe the type to instantiate, must be a polymorphic erased type
    *            (either [[Type.Var]], a primitive type, or `java.lang.Object`)
    */
  private def instantiateType(m: Map[Symbol.KindedTypeVarSym, MonoType], tpe: Type)(implicit root: Root): BackendType = tpe match {
    case Type.Var(sym, _) => BackendType.toBackendType(m(sym))
    case Type.Cst(tc, _) => tc match {
      case TypeConstructor.Bool => BackendType.Bool
      case TypeConstructor.Char => BackendType.Char
      case TypeConstructor.Float32 => BackendType.Float32
      case TypeConstructor.Float64 => BackendType.Float64
      case TypeConstructor.Int8 => BackendType.Int8
      case TypeConstructor.Int16 => BackendType.Int16
      case TypeConstructor.Int32 => BackendType.Int32
      case TypeConstructor.Int64 => BackendType.Int64
      case TypeConstructor.Native(clazz) if clazz == classOf[Object] => BackendType.Object
      case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    }
    case Type.Apply(_, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
  }

  /** Returns the set of extensible tag types in `types` without searching recursively. */
  def getExtensibleTagTypesOf(types: Iterable[MonoType])(implicit root: Root): Set[BackendObjType.TagType] =
    types.foldLeft(Set.empty[BackendObjType.TagType]) {
      case (acc, MonoType.ExtensibleExtend(cons, targs, _)) =>
        targs match {
          case Nil => acc + BackendObjType.NullaryTag(cons.name)
          case nary => acc + BackendObjType.Tag(nary.map(BackendType.toBackendType))
        }
      case (acc, _) => acc
    }

}
