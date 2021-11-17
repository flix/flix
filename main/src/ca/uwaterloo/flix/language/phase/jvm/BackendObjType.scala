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
import ca.uwaterloo.flix.language.phase.jvm.JvmName.Delimiter

sealed trait BackendObjType {
  def jvmName: JvmName

  def toDescriptor: String = jvmName.toDescriptor

  def toTpe: BackendType.Reference = BackendType.Reference(this)
}

object BackendObjType {

  private val rootPackage: List[String] = Nil

  private val devFlixRuntime = List("dev", "flix", "runtime")

  private val javaLang = List("java", "lang")

  private def erasedListOfTypes(ts: List[BackendType]): String =
    ts.map(e => e.toErased.toString).mkString(Delimiter)

  case object Unit extends BackendObjType {
    override val jvmName: JvmName = JvmName(devFlixRuntime, "Unit")
  }

  case object BigInt extends BackendObjType {
    override val jvmName: JvmName = JvmName(List("java", "math"), "BigInteger")
  }

  case object String extends BackendObjType {
    override val jvmName: JvmName = JvmName(javaLang, "String")
  }

  case class Channel(tpe: BackendType) extends BackendObjType {
    override val jvmName: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "Channel")
  }

  case class Lazy(tpe: BackendType) extends BackendObjType {
    override val jvmName: JvmName = JvmName(rootPackage, s"Lazy$Delimiter${tpe.toErased}")
  }

  case class Ref(tpe: BackendType) extends BackendObjType {
    override val jvmName: JvmName = JvmName(rootPackage, s"Ref$Delimiter${tpe.toErased}")
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType {
    override val jvmName: JvmName = JvmName(rootPackage, s"Tuple${elms.length}$Delimiter${erasedListOfTypes(elms)}")
  }

  case class Enum(sym: Symbol.EnumSym, args: List[BackendType]) extends BackendObjType {
    override val jvmName: JvmName = ???
  }

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType {
    override val jvmName: JvmName = JvmName(rootPackage, s"Fn${args.length}$Delimiter${erasedListOfTypes(args)}$Delimiter${result.toErased}")
  }

  case object RecordEmpty extends BackendObjType {
    // TODO: These should include delimiter
    override val jvmName: JvmName = JvmName(rootPackage, s"RecordEmpty")
    val interface: JvmName = JvmName(rootPackage, "IRecord")
  }

  case class RecordExtend(field: String, value: BackendType, rest: BackendType) extends BackendObjType {
    override val jvmName: JvmName = JvmName(rootPackage, s"RecordExtend$Delimiter${value.toErased}")
  }

  case object SchemaEmpty extends BackendObjType {
    override val jvmName: JvmName = ???
  }

  case class SchemaExtend(name: String, tpe: BackendType, rest: BackendType) extends BackendObjType {
    override val jvmName: JvmName = ???
  }

  case class Relation(tpes: List[BackendType]) extends BackendObjType {
    override val jvmName: JvmName = ???
  }

  case class Lattice(tpes: List[BackendType]) extends BackendObjType {
    override val jvmName: JvmName = ???
  }

  case class Native(jvmName: JvmName) extends BackendObjType
}
