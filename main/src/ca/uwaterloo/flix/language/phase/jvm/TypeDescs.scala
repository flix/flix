/*
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

import ca.uwaterloo.flix.language.phase.jvm.classes.{GenExtTagged, GenRecord, GenRegion, GenTagged, GenUnit}

import ca.uwaterloo.flix.language.ast.{JvmAst, SimpleType, SourceLocation}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_Object, CD_String, CD_boolean, CD_byte, CD_char, CD_double, CD_float, CD_int, CD_long, CD_short}

/**
  * Converts [[SimpleType]]s into the [[ClassDesc]]s of their JVM representations.
  */
object TypeDescs {

  /** Converts the given [[SimpleType]] into the [[ClassDesc]] of its JVM representation. */
  def toClassDesc(tpe0: SimpleType)(implicit root: JvmAst.Root): ClassDesc = tpe0 match {
    case SimpleType.Void => CD_Object
    case SimpleType.AnyType => CD_Object
    case SimpleType.Unit => GenUnit.desc
    case SimpleType.Bool => CD_boolean
    case SimpleType.Char => CD_char
    case SimpleType.Float32 => CD_float
    case SimpleType.Float64 => CD_double
    case SimpleType.BigDecimal => JavaClasses.BigDecimal
    case SimpleType.Int8 => CD_byte
    case SimpleType.Int16 => CD_short
    case SimpleType.Int32 => CD_int
    case SimpleType.Int64 => CD_long
    case SimpleType.BigInt => JavaClasses.BigInteger
    case SimpleType.String => CD_String
    case SimpleType.Regex => JavaClasses.Regex
    case SimpleType.Region => GenRegion.desc
    case SimpleType.Null => CD_Object
    case SimpleType.Array(tpe) => toClassDesc(tpe).arrayType()
    case SimpleType.Lazy(tpe) => BackendObjType.Lazy(toErasedClassDesc(tpe)).desc
    case SimpleType.Tuple(elms) => BackendObjType.Tuple(elms.map(toErasedClassDesc)).desc
    case SimpleType.Enum(_, Nil) => GenTagged.desc
    case SimpleType.Struct(sym, Nil) => BackendObjType.Struct.fromStruct(root.structs(sym)).desc
    case SimpleType.Arrow(args, result) => BackendObjType.Arrow(args.map(toErasedClassDesc), toErasedClassDesc(result)).desc
    case SimpleType.RecordEmpty => GenRecord.desc
    case SimpleType.RecordExtend(_, _, _) => GenRecord.desc
    case SimpleType.ExtensibleEmpty => GenExtTagged.desc
    case SimpleType.ExtensibleExtend(_, _, _) => GenExtTagged.desc
    case SimpleType.Native(clazz) => clazz
    case SimpleType.Enum(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe0'", SourceLocation.Unknown)
    case SimpleType.Struct(_, _) => throw InternalCompilerException(s"Unexpected type '$tpe0'", SourceLocation.Unknown)
  }

  /**
    * Computes the [[ClassDesc]] of the erased JVM representation of the given [[SimpleType]].
    *
    * Unlike [[toClassDesc]] this needs no [[JvmAst.Root]], since every reference type erases
    * to `java.lang.Object` without inspecting it.
    */
  def toErasedClassDesc(tpe: SimpleType): ClassDesc = tpe match {
    case SimpleType.Bool => CD_boolean
    case SimpleType.Char => CD_char
    case SimpleType.Int8 => CD_byte
    case SimpleType.Int16 => CD_short
    case SimpleType.Int32 => CD_int
    case SimpleType.Int64 => CD_long
    case SimpleType.Float32 => CD_float
    case SimpleType.Float64 => CD_double
    case SimpleType.Void | SimpleType.AnyType | SimpleType.Unit | SimpleType.BigDecimal | SimpleType.BigInt |
         SimpleType.String | SimpleType.Regex | SimpleType.Array(_) | SimpleType.Lazy(_) |
         SimpleType.Tuple(_) | SimpleType.Enum(_, _) | SimpleType.Struct(_, _) | SimpleType.Arrow(_, _) |
         SimpleType.RecordEmpty | SimpleType.RecordExtend(_, _, _) |
         SimpleType.ExtensibleExtend(_, _, _) | SimpleType.ExtensibleEmpty | SimpleType.Native(_) |
         SimpleType.Region | SimpleType.Null =>
      CD_Object
  }

  /** The [[ClassDesc]]s every type erases to: the primitives and `java.lang.Object`. */
  val erasedTypes: List[ClassDesc] = List(
    CD_boolean,
    CD_char,
    CD_float,
    CD_double,
    CD_byte,
    CD_short,
    CD_int,
    CD_long,
    CD_Object,
  )

}
