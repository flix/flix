/*
 * Copyright 2022 Paul Butcher
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
import ca.uwaterloo.flix.language.phase.jvm.JvmName.RootPackage
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for anonymous classes (created through NewObject)
  */

object GenAnonymousClasses {

  /**
    * Returns the set of anonymous classes for the given set of objects
    */
  def gen(objs: Set[Expression.NewObject])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate an anonymous class for each object and collect the results in a map.
    //
    ParOps.parAgg(objs, Map.empty[JvmName, JvmClass])({
      case (macc, obj) =>
        val className = JvmName(RootPackage, "HardcodedAnon")
        flix.subtask(className.toInternalName, sample = true)

        macc + (className -> JvmClass(className, genByteCode(className, obj)))
    }, _ ++ _)
  }

  private def genByteCode(className: JvmName,
                          obj: Expression.NewObject)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, className.toInternalName, null,
      null, Array(obj.clazz.getName))

    visitor.visitEnd()
    visitor.toByteArray
  }
}
