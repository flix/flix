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

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{Delimiter, DevFlixRuntime, JavaLang, RootPackage}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Represents all Flix types that are objects on the JVM (array is an exception).
  */
sealed trait BackendObjType {
  /**
    * The `JvmName` that represents the type `Ref(Int)` refers to `Ref$Int`.
    */
  val jvmName: JvmName = this match {
    case BackendObjType.Unit => JvmName(DevFlixRuntime, "Unit")
    case BackendObjType.BigInt => JvmName(List("java", "math"), "BigInteger")
    case BackendObjType.String => JvmName(JavaLang, "String")
    case BackendObjType.Channel(_) => JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "Channel")
    case BackendObjType.Lazy(tpe) => JvmName(RootPackage, s"Lazy$Delimiter${tpe.toErased}")
    case BackendObjType.Ref(tpe) => JvmName(RootPackage, s"Ref$Delimiter${tpe.toErased}")
    case BackendObjType.Tuple(elms) => JvmName(RootPackage, s"Tuple${elms.length}$Delimiter${erasedListOfTypes(elms)}")
    case BackendObjType.Enum(_, _) => throw InternalCompilerException("Enum JVM NAME") // TODO
    case BackendObjType.Arrow(args, result) => JvmName(RootPackage, s"Fn${args.length}$Delimiter${erasedListOfTypes(args)}$Delimiter${result.toErased}")
    case BackendObjType.RecordEmpty => JvmName(RootPackage, s"RecordEmpty$Delimiter")
    case BackendObjType.RecordExtend(_, value, _) => JvmName(RootPackage, s"RecordExtend$Delimiter${value.toErased}")
    case BackendObjType.SchemaEmpty() => throw InternalCompilerException("SchemaEmpty JVM NAME") // TODO
    case BackendObjType.SchemaExtend(_, _, _) => throw InternalCompilerException("SchemaExtend JVM NAME") // TODO
    case BackendObjType.Relation(_) => throw InternalCompilerException("Relation JVM NAME") // TODO
    case BackendObjType.Lattice(_) => throw InternalCompilerException("Lattice JVM NAME") // TODO
    case BackendObjType.Native(className) => className
  }

  /**
    * The JVM type descriptor of the form `L<jvmName.toInternalName>;`.
    */
  def toDescriptor: String = jvmName.toDescriptor

  /**
    * Returns `this` wrapped in `BackendType.Reference`.
    */
  def toTpe: BackendType.Reference = BackendType.Reference(this)

  /**
    * Constructs a concatenated list of erased strings delimited with `JvmName.Delimiter`.
    */
  private def erasedListOfTypes(ts: List[BackendType]): String =
    ts.map(e => e.toErased.toString).mkString(Delimiter)
}

object BackendObjType {
  case object Unit extends BackendObjType

  case object BigInt extends BackendObjType

  case object String extends BackendObjType

  case class Channel(tpe: BackendType) extends BackendObjType

  case class Lazy(tpe: BackendType) extends BackendObjType

  case class Ref(tpe: BackendType) extends BackendObjType

  case class Tuple(elms: List[BackendType]) extends BackendObjType

  case class Enum(sym: Symbol.EnumSym, args: List[BackendType]) extends BackendObjType

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType

  case object RecordEmpty extends BackendObjType {
    val interface: JvmName = JvmName(RootPackage, s"IRecord$Delimiter")
  }

  case class RecordExtend(field: String, value: BackendType, rest: BackendType) extends BackendObjType {
    val interface: JvmName = JvmName(RootPackage, s"IRecord$Delimiter")
  }

  // TODO: Should be an object
  case class SchemaEmpty() extends BackendObjType

  case class SchemaExtend(name: String, tpe: BackendType, rest: BackendType) extends BackendObjType

  case class Relation(tpes: List[BackendType]) extends BackendObjType

  case class Lattice(tpes: List[BackendType]) extends BackendObjType

  /**
    * Represents a JVM type not represented in Flix like `java.lang.Object` or `dev.flix.runtime.ReifiedSourceLocation`.
    * This should not be used for `java.lang.String` for example since `BackendObjType.String`
    * represents this type.
    */
  case class Native(className: JvmName) extends BackendObjType
}
