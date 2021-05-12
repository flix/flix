/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.PRefType.PRef
import ca.uwaterloo.flix.language.ast.{Describable, PRefType, PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmTarget}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes

class ClassMaker(visitor: ClassWriter) {
  private def makeField[T <: PType](fieldName: String, fieldType: RType[T], mod: Mod): Unit = {
    val field = visitor.visitField(mod.getInt, fieldName, fieldType.toDescriptor, null, null)
    field.visitEnd()
  }

  def mkField[T <: PType](fieldName: String, fieldType: RType[T], mod: Mod = Mod.nothing): Unit = {
    makeField(fieldName, fieldType, mod)
  }

  def mkStaticField[T <: PType](fieldName: String, fieldType: RType[T]): Unit = {
    makeField(fieldName, fieldType, Mod.isStatic)
  }

  def mkConstructor(f: F[StackNil] => F[StackEnd], descriptor: String): Unit =
    mkMethod(f, JvmName.constructorMethod, descriptor, Mod.isPublic)

  def mkObjectConstructor[T <: PRefType](): Unit = {
    val f: F[StackNil] => F[StackEnd] = {
      START[StackNil] ~
        THISLOAD(tag[T]) ~
        INVOKEOBJECTCONSTRUCTOR ~
        RETURN
    }
    mkConstructor(f, JvmName.nothingToVoid)
  }

  def mkMethod(f: F[StackNil] => F[StackEnd], methodName: String, descriptor: String, mod: Mod): Unit = {
    val methodVisitor = visitor.visitMethod(mod.getInt, methodName, descriptor, null, null)
    methodVisitor.visitCode()
    f(F(methodVisitor))
    methodVisitor.visitMaxs(1, 1)
    methodVisitor.visitEnd()
  }

  // TODO(JLS): make a better interface. Mod doesn't work as an API
  def mkAbstractMethod(methodName: String, descriptor: String, mod: Mod): Unit = {
    visitor.visitMethod(mod.getInt, methodName, descriptor, null, null).visitEnd()
  }

  def closeClassMaker: Array[Byte] = {
    visitor.visitEnd()
    visitor.toByteArray
  }
}

object ClassMaker {

  /**
   * Returns the target JVM version.
   */
  private def JavaVersion(implicit flix: Flix): Int = flix.options.target match {
    case JvmTarget.Version16 => Opcodes.V1_6
    case JvmTarget.Version17 => Opcodes.V1_7
    case JvmTarget.Version18 => Opcodes.V1_8
    case JvmTarget.Version19 => throw InternalCompilerException(s"Unsupported Java version: '1.9'.")
  }

  /**
   * Returns a freshly created class writer object.
   *
   * The object is constructed to compute stack map frames automatically.
   */
  private def makeClassWriter(): ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
    override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
      JvmName.Java.Lang.Object.name
    }
  }

  def mkClassMaker[T <: PRefType](className: JvmName, addSource: Boolean, superClass: Option[JvmName], mod: Mod, interfaces: JvmName*)(implicit flix: Flix): ClassMaker = {
    val visitor = makeClassWriter()
    visitor.visit(JavaVersion, mod.getInt, className.toInternalName, null, superClass.getOrElse(JvmName.Java.Lang.Object).toInternalName, interfaces.map(_.toInternalName).toArray)
    if (addSource) visitor.visitSource(className.toInternalName, null)
    new ClassMaker(visitor)
  }

  // TODO(JLS): maybe individual classes, since interface fields are always abstract etc
  def mkClass(className: JvmName, addSource: Boolean, superClass: Option[JvmName], interfaces: JvmName*)(implicit flix: Flix): ClassMaker = {
    mkClassMaker(className, addSource = addSource, superClass, Mod.isPublic.isFinal, interfaces:_*)
  }

  def mkAbstractClass(className: JvmName, addSource: Boolean, superClass: Option[JvmName], interfaces: JvmName*)(implicit flix: Flix): ClassMaker = {
    mkClassMaker(className, addSource = addSource, superClass, Mod.isPublic.isAbstract, interfaces:_*)
  }

  def mkInterface(className: JvmName, addSource: Boolean, interfaces: JvmName*)(implicit flix: Flix): ClassMaker = {
    mkClassMaker(className, addSource = addSource, None, Mod.isPublic.isAbstract.isInterface, interfaces:_*)
  }

  class Mod private {
    private var fin = 0
    private var stat = 0
    private var pub = 0
    private var priv = 0
    private var abs = 0
    private var inter = 0

    def getInt: Int = fin + stat + pub + priv + abs + inter

    def isFinal: Mod = {
      fin = Opcodes.ACC_FINAL
      this
    }

    def isStatic: Mod = {
      stat = Opcodes.ACC_STATIC
      this
    }

    def isPublic: Mod = {
      if (priv != 0) throw InternalCompilerException("mod cannot both be private and public")
      pub = Opcodes.ACC_PUBLIC
      this
    }

    def isPrivate: Mod = {
      if (pub != 0) throw InternalCompilerException("mod cannot both be private and public")
      priv = Opcodes.ACC_PRIVATE
      this
    }

    def isAbstract: Mod = {
      abs = Opcodes.ACC_ABSTRACT
      this
    }

    def isInterface: Mod = {
      inter = Opcodes.ACC_INTERFACE
      this
    }
  }

  object Mod {
    def isFinal: Mod = new Mod().isFinal

    def isStatic: Mod = new Mod().isStatic

    def isPublic: Mod = new Mod().isPublic

    def isPrivate: Mod = new Mod().isPrivate

    def isAbstract: Mod = new Mod().isAbstract

    def isInterface: Mod = new Mod().isInterface

    def nothing: Mod = new Mod()
  }

}
