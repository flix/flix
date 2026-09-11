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
import ca.uwaterloo.flix.language.jvm.{ClassDescs, JavaClasses}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.NotFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, InstanceMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.{mkDescriptor, mkVoidDescriptor}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle, TypeDescs}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_Object, CD_boolean, CD_double, CD_int, CD_long}

/**
  * The function interface `FnN$...` of an erased arrow type, which every generated Flix
  * function extends. For argument types with a matching `java.util.function` interface it
  * also implements those, so Flix functions can be passed to Java APIs.
  */
object GenArrow {

  def desc(args: List[ClassDesc], result: ClassDesc): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName(s"Fn${args.length}", (args :+ result).map(Mangle.erasedName)))

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
  def descOfArrowType(tpe: SimpleType): ClassDesc = {
    val (args, result) = erasedArgsAndResult(tpe)
    desc(args, result)
  }

  def erasedArgsAndResult(tpe: SimpleType): (List[ClassDesc], ClassDesc) = tpe match {
    case SimpleType.Arrow(targs, tresult) =>
      (targs.map(TypeDescs.toErasedClassDesc), TypeDescs.toErasedClassDesc(tresult))
    case _ =>
      throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }


  /**
    * Represents a function interface from `java.util.function`.
    */
  private sealed trait FunctionInterface {
    /**
      * The [[ClassDesc]] of the interface.
      */
    def desc: ClassDesc = this match {
      case ObjFunction => JavaClasses.ObjFunction
      case ObjConsumer => JavaClasses.ObjConsumer
      case ObjPredicate => JavaClasses.ObjPredicate
      case IntFunction => JavaClasses.IntFunction
      case IntConsumer => JavaClasses.IntConsumer
      case IntPredicate => JavaClasses.IntPredicate
      case IntUnaryOperator => JavaClasses.IntUnaryOperator
      case LongFunction => JavaClasses.LongFunction
      case LongConsumer => JavaClasses.LongConsumer
      case LongPredicate => JavaClasses.LongPredicate
      case LongUnaryOperator => JavaClasses.LongUnaryOperator
      case DoubleFunction => JavaClasses.DoubleFunction
      case DoubleConsumer => JavaClasses.DoubleConsumer
      case DoublePredicate => JavaClasses.DoublePredicate
      case DoubleUnaryOperator => JavaClasses.DoubleUnaryOperator
    }

    /**
      * The required method of the interface.
      * These methods should do the same as a non-tail call in genExpression.
      */
    def functionMethod: InstanceMethod = this match {
      case ObjFunction => InstanceMethod(this.desc, "apply",
        mkDescriptor(JavaClasses.Object)(JavaClasses.Object))
      case ObjConsumer => InstanceMethod(this.desc, "accept",
        mkVoidDescriptor(JavaClasses.Object))
      case ObjPredicate => InstanceMethod(this.desc, "test",
        mkDescriptor(JavaClasses.Object)(CD_boolean))
      case IntFunction => InstanceMethod(this.desc, "apply",
        mkDescriptor(CD_int)(JavaClasses.Object))
      case IntConsumer => InstanceMethod(this.desc, "accept",
        mkVoidDescriptor(CD_int))
      case IntPredicate => InstanceMethod(this.desc, "test",
        mkDescriptor(CD_int)(CD_boolean))
      case IntUnaryOperator => InstanceMethod(this.desc, "applyAsInt",
        mkDescriptor(CD_int)(CD_int))
      case LongFunction => InstanceMethod(this.desc, "apply",
        mkDescriptor(CD_long)(JavaClasses.Object))
      case LongConsumer => InstanceMethod(this.desc, "accept",
        mkVoidDescriptor(CD_long))
      case LongPredicate => InstanceMethod(this.desc, "test",
        mkDescriptor(CD_long)(CD_boolean))
      case LongUnaryOperator => InstanceMethod(this.desc, "applyAsLong",
        mkDescriptor(CD_long)(CD_long))
      case DoubleFunction => InstanceMethod(this.desc, "apply",
        mkDescriptor(CD_double)(JavaClasses.Object))
      case DoubleConsumer => InstanceMethod(this.desc, "accept",
        mkVoidDescriptor(CD_double))
      case DoublePredicate => InstanceMethod(this.desc, "test",
        mkDescriptor(CD_double)(CD_boolean))
      case DoubleUnaryOperator => InstanceMethod(this.desc, "applyAsDouble",
        mkDescriptor(CD_double)(CD_double))
    }

    /**
      * The required method of the interface.
      * These methods should do the same as a non-tail call in genExpression.
      */
    def functionIns(argField: InstanceField)(implicit mv: MethodVisitor): Unit = this match {
      case ObjFunction =>
        thisLoad()
        DUP()
        ALOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        ARETURN()
      case ObjConsumer =>
        thisLoad()
        DUP()
        ALOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        RETURN()
      case ObjPredicate =>
        thisLoad()
        DUP()
        ALOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        IRETURN()
      case IntFunction =>
        thisLoad()
        DUP()
        ILOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        ARETURN()
      case IntConsumer =>
        thisLoad()
        DUP()
        ILOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        RETURN()
      case IntPredicate =>
        thisLoad()
        DUP()
        ILOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        IRETURN()
      case IntUnaryOperator =>
        thisLoad()
        DUP()
        ILOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_int, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        IRETURN()
      case LongFunction =>
        thisLoad()
        DUP()
        LLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        ARETURN()
      case LongConsumer =>
        thisLoad()
        DUP()
        LLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        RETURN()
      case LongPredicate =>
        thisLoad()
        DUP()
        LLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        IRETURN()
      case LongUnaryOperator =>
        thisLoad()
        DUP()
        LLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_long, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        LRETURN()
      case DoubleFunction =>
        thisLoad()
        DUP()
        DLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        ARETURN()
      case DoubleConsumer =>
        thisLoad()
        DUP()
        DLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        RETURN()
      case DoublePredicate =>
        thisLoad()
        DUP()
        DLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        IRETURN()
      case DoubleUnaryOperator =>
        thisLoad()
        DUP()
        DLOAD(1)
        PUTFIELD(argField)
        GenResult.unwindSuspensionFreeThunkToType(CD_double, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
        DRETURN()
    }
  }

  // ClassMaker.Object -> ClassMaker.Object
  private case object ObjFunction extends FunctionInterface

  // ClassMaker.Object -> Unit
  private case object ObjConsumer extends FunctionInterface

  // ClassMaker.Object -> Bool
  private case object ObjPredicate extends FunctionInterface

  // Int32 -> ClassMaker.Object
  private case object IntFunction extends FunctionInterface

  // Int32 -> Unit
  private case object IntConsumer extends FunctionInterface

  // Int32 -> Bool
  private case object IntPredicate extends FunctionInterface

  // Int32 -> Int32
  private case object IntUnaryOperator extends FunctionInterface

  // Int64 -> ClassMaker.Object
  private case object LongFunction extends FunctionInterface

  // Int64 -> Unit
  private case object LongConsumer extends FunctionInterface

  // Int64 -> Bool
  private case object LongPredicate extends FunctionInterface

  // Int64 -> Int64
  private case object LongUnaryOperator extends FunctionInterface

  // Float64 -> ClassMaker.Object
  private case object DoubleFunction extends FunctionInterface

  // Float64 -> Unit
  private case object DoubleConsumer extends FunctionInterface

  // Float64 -> Bool
  private case object DoublePredicate extends FunctionInterface

  // Float64 -> Float64
  private case object DoubleUnaryOperator extends FunctionInterface

  /**
    * Returns the specialized java function interfaces of the function type.
    */
  private def specialization(args: List[ClassDesc], result: ClassDesc): List[FunctionInterface] = {
    (args, result) match {
      case (CD_Object :: Nil, _) =>
        ObjFunction :: ObjConsumer :: ObjPredicate :: Nil
      case (CD_int :: Nil, _) =>
        IntFunction :: IntConsumer :: IntPredicate :: IntUnaryOperator :: Nil
      case (CD_long :: Nil, _) =>
        LongFunction :: LongConsumer :: LongPredicate :: LongUnaryOperator :: Nil
      case (CD_double :: Nil, _) =>
        DoubleFunction :: DoubleConsumer :: DoublePredicate :: DoubleUnaryOperator :: Nil
      case _ => Nil
    }
  }

  def genByteCode(args: List[ClassDesc], result: ClassDesc)(implicit flix: Flix): Array[Byte] = {
    val specializedInterface = specialization(args, result)
    val interfaces = GenThunk.Desc :: specializedInterface.map(_.desc)

    val cm = ClassMaker.mkAbstractClass(desc(args, result), superClass = CD_Object, interfaces)

    cm.mkConstructor(Constructor(args, result), IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    args.indices.foreach(argIndex => cm.mkField(ArgField(args, result, argIndex), IsPublic, NotFinal, NotVolatile))
    specializedInterface.foreach(i => cm.mkMethod(i.functionMethod, IsPublic, NotFinal, i.functionIns(ArgField(args, result, 0))(_)))

    cm.closeClassMaker()
  }

  def Constructor(args: List[ClassDesc], result: ClassDesc): ConstructorMethod = ConstructorMethod(desc(args, result), Nil)

  def ArgField(args: List[ClassDesc], result: ClassDesc, index: Int): InstanceField =
    InstanceField(desc(args, result), s"arg$index", args(index))

}
