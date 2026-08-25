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

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{SimpleType, SourceLocation}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{AbstractMethod, ConstructorMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, Mangle, TypeDescs}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ClassDesc

/**
  * The closure abstract class of an erased arrow type, sitting between [[GenArrow]] and the
  * generated closure classes.
  *
  * (Int, String) -> Bool example:
  * public abstract class Clo2$Int$Obj$Bool extends Fn2$Int$Obj$Bool {
  * public Clo2$Int$Obj$Bool() { ... }
  * public abstract Clo2$Int$Obj$Bool getUniqueThreadClosure();
  * }
  */
object GenAbstractArrow {

  def desc(args: List[ClassDesc], result: ClassDesc): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName(s"Clo${args.length}", (args :+ result).map(Mangle.erasedName)))

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
  def descOfArrowType(tpe: SimpleType): ClassDesc = tpe match {
    case SimpleType.Arrow(targs, tresult) =>
      desc(targs.map(TypeDescs.toErasedClassDesc), TypeDescs.toErasedClassDesc(tresult))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  def genByteCode(args: List[ClassDesc], result: ClassDesc)(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkAbstractClass(desc(args, result), GenArrow.desc(args, result))
    cm.mkConstructor(Constructor(args, result), IsPublic, nullarySuperConstructor(GenArrow.Constructor(args, result))(_))

    cm.mkAbstractMethod(GetUniqueThreadClosureMethod(args, result))

    cm.closeClassMaker()
  }

  def Constructor(args: List[ClassDesc], result: ClassDesc): ConstructorMethod =
    ConstructorMethod(desc(args, result), Nil)

  def GetUniqueThreadClosureMethod(args: List[ClassDesc], result: ClassDesc): AbstractMethod =
    AbstractMethod(desc(args, result), "getUniqueThreadClosure", mkDescriptor()(desc(args, result)))

}
