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

import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.ast.{JvmAst, SimpleType, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.mangle
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.ListOps

object JvmOps {

  /** Returns the index of `varOffset` combined with the context offset. */
  def getIndex(varOffset: Int, contextOffset: Int): Int =
    varOffset + contextOffset

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
  def getErasedFunctionInterfaceType(tpe: SimpleType)(implicit root: Root): BackendObjType.Arrow = tpe match {
    case SimpleType.Arrow(targs, tresult) =>
      BackendObjType.Arrow(targs.map(BackendType.toErasedBackendType), BackendType.toBackendType(tresult))
    case _ =>
      throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  /**
    * Returns the erased closure abstract class type `CloX$Y$Z` for the given [[SimpleType]].
    *
    * For example:
    *
    * Int -> Int          =>  Clo1$Int$Int
    * (Int, Int) -> Int   =>  Clo2$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getErasedClosureAbstractClassType(tpe: SimpleType): BackendObjType.AbstractArrow = tpe match {
    case SimpleType.Arrow(targs, tresult) =>
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
  def getLazyTypesOf(types: Iterable[SimpleType])(implicit root: Root): Set[BackendObjType.Lazy] =
    types.foldLeft(Set.empty[BackendObjType.Lazy]) {
      case (acc, SimpleType.Lazy(tpe)) => acc + BackendObjType.Lazy(BackendType.toBackendType(tpe))
      case (acc, _) => acc
    }

  /** Returns the set of record extend types in `types` without searching recursively. */
  def getRecordExtendsOf(types: Iterable[SimpleType])(implicit root: Root): Set[BackendObjType.RecordExtend] =
    types.foldLeft(Set.empty[BackendObjType.RecordExtend]) {
      case (acc, SimpleType.RecordExtend(_, value, _)) =>
        acc + BackendObjType.RecordExtend(BackendType.toBackendType(value))
      case (acc, _) => acc
    }

  /** Returns the set of erased function types in `types` without searching recursively. */
  def getErasedArrowsOf(types: Iterable[SimpleType]): Set[BackendObjType.Arrow] =
    types.foldLeft(Set.empty[BackendObjType.Arrow]) {
      case (acc, SimpleType.Arrow(args, result)) =>
        acc + BackendObjType.Arrow(args.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(result))
      case (acc, _) => acc
    }

  /** Returns the set of tuple types in `types` without searching recursively. */
  def getTupleTypesOf(types: Iterable[SimpleType])(implicit root: Root): Set[BackendObjType.Tuple] =
    types.foldLeft(Set.empty[BackendObjType.Tuple]) {
      case (acc, SimpleType.Tuple(elms)) =>
        acc + BackendObjType.Tuple(elms.map(BackendType.toBackendType))
      case (acc, _) => acc
    }

  /** Returns the struct type of `struct`. */
  def getStructType(struct: JvmAst.Struct)(implicit root: Root): BackendObjType.Struct =
    BackendObjType.Struct(struct.fields.map(field => BackendType.toBackendType(field.tpe)))


  /** Returns the tag type of each case in `enm`. */
  def getTagsOf(enm: JvmAst.Enum)(implicit root: Root): List[BackendObjType.TagType] = {
    enm.cases.values.map {
      case caze => caze.tpes match {
        case Nil =>
          BackendObjType.NullaryTag(caze.sym.name)
        case elms =>
          BackendObjType.Tag(elms.map(BackendType.toBackendType))
      }
    }.toList
  }

  /** Returns the set of extensible tag types in `types` without searching recursively. */
  def getExtensibleTagTypesOf(types: Iterable[SimpleType])(implicit root: Root): Set[BackendObjType.TagType] =
    types.foldLeft(Set.empty[BackendObjType.TagType]) {
      case (acc, SimpleType.ExtensibleExtend(cons, targs, _)) =>
        targs match {
          case Nil => acc + BackendObjType.NullaryTag(cons.name)
          case nary => acc + BackendObjType.Tag(nary.map(BackendType.toBackendType))
        }
      case (acc, _) => acc
    }

}
