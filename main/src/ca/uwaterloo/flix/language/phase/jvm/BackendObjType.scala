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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{JvmAst, SimpleType, SourceLocation}
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.mkClassName
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.{IsVolatile, NotVolatile}
import ca.uwaterloo.flix.language.phase.jvm.classes.{GenReifiedSourceLocation, GenUncaughtExceptionHandler, GenUnhandledEffectError}
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.{mkDescriptor, mkVoidDescriptor}
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_Object, CD_boolean, CD_byte, CD_char, CD_double, CD_float, CD_int, CD_long, CD_short}

/**
  * Represents all Flix types that are objects on the JVM (array is an exception).
  */
sealed trait BackendObjType {
  /**
    * The [[ClassDesc]] of this type, e.g. the type `Ref(Int)` refers to `"Ref$Int"`.
    */
  val desc: ClassDesc = this match {
    case BackendObjType.Unit => mkDesc(DevFlixRuntime, mkClassName("Unit"))
    case BackendObjType.Lazy(tpe) => mkDesc(RootPackage, mkClassName("Lazy", tpe))
    case BackendObjType.Tuple(elms) => mkDesc(RootPackage, mkClassName("Tuple", elms))
    case BackendObjType.Struct(elms) => mkDesc(RootPackage, mkClassName("Struct", elms))
    case BackendObjType.Tagged => mkDesc(RootPackage, mkClassName("Tagged"))
    case BackendObjType.ExtTagged => mkDesc(RootPackage, mkClassName("ExtTagged"))
    case BackendObjType.AbstractArrow(args, result) => mkDesc(RootPackage, mkClassName(s"Clo${args.length}", args :+ result))
    case BackendObjType.Arrow(args, result) => mkDesc(RootPackage, mkClassName(s"Fn${args.length}", args :+ result))
    case BackendObjType.RecordEmpty => mkDesc(RootPackage, mkClassName(s"RecordEmpty"))
    case BackendObjType.Record => mkDesc(RootPackage, mkClassName("Record"))
    case BackendObjType.Region => mkDesc(DevFlixRuntime, mkClassName("Region"))
    // Java classes
    case BackendObjType.Native(clazz) => clazz
    // Effects Runtime
    case BackendObjType.Result => mkDesc(DevFlixRuntime, mkClassName("Result"))
    case BackendObjType.Value => mkDesc(DevFlixRuntime, mkClassName("Value"))
    case BackendObjType.Frame => mkDesc(DevFlixRuntime, mkClassName("Frame"))
    case BackendObjType.Thunk => mkDesc(DevFlixRuntime, mkClassName("Thunk"))
    case BackendObjType.Suspension => mkDesc(DevFlixRuntime, mkClassName("Suspension"))
    case BackendObjType.Frames => mkDesc(DevFlixRuntime, mkClassName("Frames"))
    case BackendObjType.FramesCons => mkDesc(DevFlixRuntime, mkClassName("FramesCons"))
    case BackendObjType.FramesNil => mkDesc(DevFlixRuntime, mkClassName("FramesNil"))
    case BackendObjType.Resumption => mkDesc(DevFlixRuntime, mkClassName("Resumption"))
    case BackendObjType.ResumptionCons => mkDesc(DevFlixRuntime, mkClassName("ResumptionCons"))
    case BackendObjType.ResumptionNil => mkDesc(DevFlixRuntime, mkClassName("ResumptionNil"))
    case BackendObjType.Handler => mkDesc(DevFlixRuntime, mkClassName("Handler"))
    case BackendObjType.EffectCall => mkDesc(DevFlixRuntime, mkClassName("EffectCall"))
    case BackendObjType.ResumptionWrapper(t) => mkDesc(DevFlixRuntime, mkClassName("ResumptionWrapper", t))
  }

  /**
    * The JVM type descriptor of the form `"L<internal name>;"`.
    */
  def toDescriptor: String = desc.descriptorString()

  /**
    * Returns `this` wrapped in `BackendType.Reference`.
    */
  def toTpe: BackendType.Reference = BackendType.Reference(this)
}

object BackendObjType {

  private def mkClassName(prefix: String, tpe: BackendType): String = {
    Mangle.mkClassName(prefix, tpe.toErasedString)
  }

  private def mkClassName(prefix: String, tpes: List[BackendType]): String = {
    Mangle.mkClassName(prefix, tpes.map(_.toErasedString))
  }

  private def mkClassName(prefix: String): String = {
    Mangle.mkClassName(prefix)
  }

  case object Unit extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal)

      cm.mkStaticConstructor(StaticConstructorMethod(this.desc), singletonStaticConstructor(Constructor, SingletonField)(_))
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def SingletonField: StaticField = StaticField(this.desc, "INSTANCE", this.desc)

  }

  case class Lazy(tpe: BackendType) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.desc, IsFinal)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkField(ExpField, IsPublic, NotFinal, IsVolatile)
      cm.mkField(ValueField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(LockField, IsPrivate, NotFinal, NotVolatile)
      cm.mkMethod(Nil, ForceMethod, IsPublic, IsFinal, forceIns(_))

      cm.closeClassMaker()
    }

    def ExpField: InstanceField = InstanceField(this.desc, "expression", CD_Object)

    def ValueField: InstanceField = InstanceField(this.desc, "value", tpe.toClassDesc)

    private def LockField: InstanceField = InstanceField(this.desc, "lock", JavaClasses.ReentrantLock)

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, List(CD_Object))

    /** `[] --> return` */
    private def constructorIns(implicit mv: MethodVisitor): Unit =
      withName(1, CD_Object)(exp => {
        // super()
        thisLoad()
        INVOKESPECIAL(ClassConstants.Object.Constructor)
        // this.exp = exp
        thisLoad()
        exp.load()
        PUTFIELD(ExpField)
        // this.lock = new ReentrantLock()
        thisLoad()
        NEW(JavaClasses.ReentrantLock)
        DUP()
        INVOKESPECIAL(ClassConstants.ReentrantLock.Constructor)
        PUTFIELD(LockField)
        // return
        RETURN()
      })

    def ForceMethod: InstanceMethod = InstanceMethod(this.desc, "force", mkDescriptor()(tpe.toClassDesc))

    /** `[] --> return tpe` */
    private def forceIns(implicit mv: MethodVisitor): Unit = {
      def unlockLock(): Unit = {
        thisLoad()
        GETFIELD(LockField)
        INVOKEVIRTUAL(ClassConstants.ReentrantLock.UnlockMethod)
      }

      thisLoad()
      GETFIELD(LockField)
      INVOKEVIRTUAL(ClassConstants.ReentrantLock.LockInterruptiblyMethod)
      tryCatch {
        thisLoad()
        GETFIELD(ExpField)
        // if the expression is not null, compute the value and erase the expression
        ifCondition(Condition.NONNULL) {
          thisLoad()
          // get expression as thunk
          DUP()
          GETFIELD(ExpField)
          CHECKCAST(Thunk.desc)
          // this.value = thunk.unwind()
          Result.unwindSuspensionFreeThunkToType(tpe, "during call to Lazy.force", SourceLocation.Unknown)
          PUTFIELD(ValueField)
          // this.exp = null
          thisLoad()
          pushNull()
          PUTFIELD(ExpField)
        }
        thisLoad()
        GETFIELD(ValueField)
      } {
        // catch
        unlockLock()
        ATHROW()
      }
      unlockLock()
      xReturn(tpe.toClassDesc)
    }
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.desc, IsFinal)

      elms.indices.foreach(i => cm.mkField(IndexField(i), IsPublic, NotFinal, NotVolatile))
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

      cm.closeClassMaker()
    }

    def IndexField(i: Int): InstanceField = InstanceField(this.desc, s"field$i", elms(i).toClassDesc)

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, elms.map(_.toClassDesc))

    /** `[] --> return` */
    private def constructorIns(implicit mv: MethodVisitor): Unit =
      withNames(1, elms.map(_.toClassDesc)) { case (_, variables) =>
        thisLoad()
        // super()
        DUP()
        INVOKESPECIAL(ClassConstants.Object.Constructor)
        // this.field$i = var$j
        for ((elm, i) <- variables.zipWithIndex) {
          DUP()
          elm.load()
          PUTFIELD(IndexField(i))
        }
        RETURN()
      }

  }

  object Struct {
    /** Returns the struct type of `struct`. */
    def fromStruct(struct: JvmAst.Struct)(implicit root: JvmAst.Root): Struct =
      Struct(struct.fields.map(field => BackendType.toBackendType(field.tpe)))
  }

  case class Struct(elms: List[BackendType]) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.desc, IsFinal)

      elms.indices.foreach(i => cm.mkField(IndexField(i), IsPublic, NotFinal, NotVolatile))
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

      cm.closeClassMaker()
    }

    def IndexField(i: Int): InstanceField = InstanceField(this.desc, s"field$i", elms(i).toClassDesc)

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, elms.map(_.toClassDesc))

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      withNames(1, elms.map(_.toClassDesc)) { case (_, variables) =>
        thisLoad()
        // super()
        DUP()
        INVOKESPECIAL(ClassConstants.Object.Constructor)
        // this.field$i = var$j
        // fields are numbered consecutively while variables skip indices based
        // on their stack size
        for ((elm, i) <- variables.zipWithIndex) {
          DUP()
          elm.load()
          PUTFIELD(IndexField(i))
        }
        RETURN()
      }
    }

  }

  case object Tagged extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.desc)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

      cm.mkField(OrdinalField, IsPublic, NotFinal, NotVolatile)

      cm.closeClassMaker()
    }

    def OrdinalField: InstanceField = InstanceField(this.desc, "ordinal", CD_int)

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)
  }

  case object ExtTagged extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.desc)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

      cm.mkField(NameField, IsPublic, NotFinal, NotVolatile)

      cm.closeClassMaker()
    }

    def NameField: InstanceField = InstanceField(this.desc, "tag", JavaClasses.String)

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    /** [...] -> [..., tagName] */
    def mkTagName(name: String)(implicit mv: MethodVisitor): Unit = pushString(Mangle.mangle(name))

    /** [..., tagName1, tagName2] --> [..., tagName1 == tagName2] */
    def eqTagName()(implicit mv: MethodVisitor): Unit = {
      // ACMP is okay since tag strings are loaded through ldc instructions
      ifConditionElse(Condition.ACMPEQ)(pushBool(true))(pushBool(false))
    }
  }

  object AbstractArrow {
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
    def fromArrowType(tpe: SimpleType): AbstractArrow = tpe match {
      case SimpleType.Arrow(targs, tresult) =>
        AbstractArrow(targs.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(tresult))
      case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
    }
  }

  /**
    * (Int, String) -> Bool example:
    * public abstract class Clo2$Int$Obj$Bool extends Fn2$Int$Obj$Bool {
    * public Clo2$Int$Obj$Bool() { ... }
    * public abstract Clo2$Int$Obj$Bool getUniqueThreadClosure();
    * }
    */
  case class AbstractArrow(args: List[BackendType], result: BackendType) extends BackendObjType {

    def superClass: BackendObjType.Arrow = Arrow(args, result)

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.desc, superClass.desc)
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(superClass.Constructor)(_))

      cm.mkAbstractMethod(GetUniqueThreadClosureMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def GetUniqueThreadClosureMethod: AbstractMethod = AbstractMethod(this.desc, "getUniqueThreadClosure", mkDescriptor()(this.desc))

  }

  object Arrow {
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
    def fromArrowType(tpe: SimpleType)(implicit root: JvmAst.Root): Arrow = tpe match {
      case SimpleType.Arrow(targs, tresult) =>
        Arrow(targs.map(BackendType.toErasedBackendType), BackendType.toBackendType(tresult))
      case _ =>
        throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
    }
  }

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType {

    /**
      * Represents a function interface from `java.util.function`.
      */
    sealed trait FunctionInterface {
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
      def functionIns(implicit mv: MethodVisitor): Unit = this match {
        case ObjFunction =>
          thisLoad()
          DUP()
          ALOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case ObjConsumer =>
          thisLoad()
          DUP()
          ALOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case ObjPredicate =>
          thisLoad()
          DUP()
          ALOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case IntFunction =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case IntConsumer =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case IntPredicate =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case IntUnaryOperator =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Int32, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case LongFunction =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case LongConsumer =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case LongPredicate =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case LongUnaryOperator =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Int64, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          LRETURN()
        case DoubleFunction =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case DoubleConsumer =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case DoublePredicate =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case DoubleUnaryOperator =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Float64, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          DRETURN()
      }
    }

    // ClassMaker.Object -> ClassMaker.Object
    case object ObjFunction extends FunctionInterface

    // ClassMaker.Object -> Unit
    case object ObjConsumer extends FunctionInterface

    // ClassMaker.Object -> Bool
    case object ObjPredicate extends FunctionInterface

    // Int32 -> ClassMaker.Object
    case object IntFunction extends FunctionInterface

    // Int32 -> Unit
    case object IntConsumer extends FunctionInterface

    // Int32 -> Bool
    case object IntPredicate extends FunctionInterface

    // Int32 -> Int32
    case object IntUnaryOperator extends FunctionInterface

    // Int64 -> ClassMaker.Object
    case object LongFunction extends FunctionInterface

    // Int64 -> Unit
    case object LongConsumer extends FunctionInterface

    // Int64 -> Bool
    case object LongPredicate extends FunctionInterface

    // Int64 -> Int64
    case object LongUnaryOperator extends FunctionInterface

    // Float64 -> ClassMaker.Object
    case object DoubleFunction extends FunctionInterface

    // Float64 -> Unit
    case object DoubleConsumer extends FunctionInterface

    // Float64 -> Bool
    case object DoublePredicate extends FunctionInterface

    // Float64 -> Float64
    case object DoubleUnaryOperator extends FunctionInterface

    /**
      * Returns the specialized java function interfaces of the function type.
      */
    private def specialization(): List[FunctionInterface] = {
      (args, result) match {
        case (BackendType.Reference(BackendObjType.Native(CD_Object)) :: Nil, _) =>
          ObjFunction :: ObjConsumer :: ObjPredicate :: Nil
        case (BackendType.Int32 :: Nil, _) =>
          IntFunction :: IntConsumer :: IntPredicate :: IntUnaryOperator :: Nil
        case (BackendType.Int64 :: Nil, _) =>
          LongFunction :: LongConsumer :: LongPredicate :: LongUnaryOperator :: Nil
        case (BackendType.Float64 :: Nil, _) =>
          DoubleFunction :: DoubleConsumer :: DoublePredicate :: DoubleUnaryOperator :: Nil
        case _ => Nil
      }
    }

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val specializedInterface = specialization()
      val interfaces = Thunk.desc :: specializedInterface.map(_.desc)

      val cm = ClassMaker.mkAbstractClass(this.desc, superClass = CD_Object, interfaces)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      args.indices.foreach(argIndex => cm.mkField(ArgField(argIndex), IsPublic, NotFinal, NotVolatile))
      specializedInterface.foreach(i => cm.mkMethod(i.functionMethod, IsPublic, NotFinal, i.functionIns(_)))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def ArgField(index: Int): InstanceField = InstanceField(this.desc, s"arg$index", args(index).toClassDesc)
  }

  case object RecordEmpty extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.desc, IsFinal, interfaces = List(this.interface.desc))

      cm.mkStaticConstructor(StaticConstructorMethod(this.desc), singletonStaticConstructor(Constructor, SingletonField)(_))
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)
      cm.mkMethod(Nil, LookupFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))
      cm.mkMethod(Nil, RestrictFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def interface: Record.type = Record

    def SingletonField: StaticField = StaticField(this.desc, "INSTANCE", this.desc)

    private def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.desc)

    private def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.desc)

    private def throwUnsupportedExc(implicit mv: MethodVisitor): Unit = {
      throwUnsupportedOperationException(
        s"${Record.LookupFieldMethod.name} method shouldn't be called")
    }
  }

  case object Record extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkInterface(this.desc)

      cm.mkInterfaceMethod(LookupFieldMethod)
      cm.mkInterfaceMethod(RestrictFieldMethod)

      cm.closeClassMaker()
    }

    def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.desc, "lookupField",
      mkDescriptor(JavaClasses.String)(this.desc))

    def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.desc, "restrictField",
      mkDescriptor(JavaClasses.String)(this.desc))
  }

  /**
    * Represents a JVM type not represented in BackendObjType.
    * This should not be used for `java.lang.String` for example since `BackendObjType.String`
    * represents this type.
    */
  case class Native(clazz: ClassDesc) extends BackendObjType


  case object Region extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal)

      cm.mkField(ThreadsField, IsPrivate, IsFinal, NotVolatile)
      cm.mkField(RegionThreadField, IsPrivate, IsFinal, NotVolatile)
      cm.mkField(ChildExceptionField, IsPrivate, NotFinal, IsVolatile)
      cm.mkField(OnExitField, IsPrivate, IsFinal, NotVolatile)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

      cm.mkMethod(Nil, SpawnMethod, IsPublic, IsFinal, spawnIns(_))
      cm.mkMethod(Nil, ExitMethod, IsPublic, IsFinal, exitIns(_))
      cm.mkMethod(Nil, ReportChildExceptionMethod, IsPublic, IsFinal, reportChildExceptionIns(_))
      cm.mkMethod(Nil, ReThrowChildExceptionMethod, IsPublic, IsFinal, reThrowChildExceptionIns(_))
      cm.mkMethod(Nil, RunOnExitMethod, IsPublic, IsFinal, runOnExitIns(_))

      cm.closeClassMaker()
    }

    // private final ConcurrentLinkedQueue<Thread> threads = new ConcurrentLinkedQueue<Thread>();
    private def ThreadsField: InstanceField = InstanceField(this.desc, "threads", JavaClasses.ConcurrentLinkedQueue)

    // private final LinkedList<Runnable> onExit = new LinkedList<Runnable>();
    private def OnExitField: InstanceField = InstanceField(this.desc, "onExit", JavaClasses.LinkedList)

    // private final Thread regionThread = Thread.currentThread();
    private def RegionThreadField: InstanceField = InstanceField(this.desc, "regionThread", JavaClasses.Thread)

    // private volatile Throwable childException = null;
    private def ChildExceptionField: InstanceField = InstanceField(this.desc, "childException", JavaClasses.Throwable)

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      INVOKESPECIAL(ClassConstants.Object.Constructor)
      thisLoad()
      NEW(JavaClasses.ConcurrentLinkedQueue)
      DUP()
      invokeConstructor(JavaClasses.ConcurrentLinkedQueue, MethodTypeDescs.NothingToVoid)
      PUTFIELD(ThreadsField)
      thisLoad()
      INVOKESTATIC(ClassConstants.Thread.CurrentThreadMethod)
      PUTFIELD(RegionThreadField)
      thisLoad()
      ACONST_NULL()
      PUTFIELD(ChildExceptionField)
      thisLoad()
      NEW(JavaClasses.LinkedList)
      DUP()
      invokeConstructor(JavaClasses.LinkedList, MethodTypeDescs.NothingToVoid)
      PUTFIELD(OnExitField)
      RETURN()
    }

    // final public void spawn(Runnable r) {
    //   Thread t = new Thread(r);
    //   t.setUncaughtExceptionHandler(new UncaughtExceptionHandler(this));
    //   t.start();
    //   threads.add(t);
    // }
    def SpawnMethod: InstanceMethod = InstanceMethod(this.desc, "spawn", mkVoidDescriptor(JavaClasses.Runnable))

    private def spawnIns(implicit mv: MethodVisitor): Unit = {
      INVOKESTATIC(ClassConstants.Thread.OfVirtualMethod)
      ALOAD(1)
      INVOKEINTERFACE(ClassConstants.ThreadBuilderOfVirtual.UnstartedMethod)
      storeWithName(2, JavaClasses.Thread) { thread =>
        thread.load()
        NEW(GenUncaughtExceptionHandler.desc)
        DUP()
        thisLoad()
        invokeConstructor(GenUncaughtExceptionHandler.desc, mkVoidDescriptor(BackendObjType.Region.desc))
        INVOKEVIRTUAL(ClassConstants.Thread.SetUncaughtExceptionHandlerMethod)
        thread.load()
        INVOKEVIRTUAL(ClassConstants.Thread.StartMethod)
        thisLoad()
        GETFIELD(ThreadsField)
        thread.load()
        INVOKEVIRTUAL(ClassConstants.ConcurrentLinkedQueue.AddMethod)
        POP()
        RETURN()
      }
    }

    // final public void exit() throws InterruptedException {
    //   Thread t;
    //   while ((t = threads.poll()) != null)
    //     t.join();
    //   for (Runnable r: onExit)
    //     r.run();
    // }
    def ExitMethod: InstanceMethod = InstanceMethod(this.desc, "exit", MethodTypeDescs.NothingToVoid)

    private def exitIns(implicit mv: MethodVisitor): Unit = {
      withName(1, JavaClasses.Thread) { t =>
        whileLoop(Condition.NONNULL) {
          thisLoad()
          GETFIELD(ThreadsField)
          INVOKEVIRTUAL(ClassConstants.ConcurrentLinkedQueue.PollMethod)
          CHECKCAST(JavaClasses.Thread)
          DUP()
          t.store()
        } {
          t.load()
          INVOKEVIRTUAL(ClassConstants.Thread.JoinMethod)
        }
        withName(2, JavaClasses.Iterator) { i =>
          thisLoad()
          GETFIELD(OnExitField)
          INVOKEVIRTUAL(ClassConstants.LinkedList.IteratorMethod)
          i.store()
          whileLoop(Condition.NE) {
            i.load()
            INVOKEINTERFACE(ClassConstants.Iterator.HasNextMethod)
          } {
            i.load()
            INVOKEINTERFACE(ClassConstants.Iterator.NextMethod)
            CHECKCAST(JavaClasses.Runnable)
            INVOKEINTERFACE(ClassConstants.Runnable.RunMethod)
          }
        }
        RETURN()
      }
    }

    // final public void reportChildException(Throwable e) {
    //   childException = e;
    //   regionThread.interrupt();
    // }
    def ReportChildExceptionMethod: InstanceMethod = InstanceMethod(this.desc, "reportChildException", mkVoidDescriptor(JavaClasses.Throwable))

    private def reportChildExceptionIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      ALOAD(1)
      PUTFIELD(ChildExceptionField)
      thisLoad()
      GETFIELD(RegionThreadField)
      INVOKEVIRTUAL(ClassConstants.Thread.InterruptMethod)
      RETURN()
    }

    // final public void reThrowChildException() throws Throwable {
    //   if (childException != null)
    //     throw childException;
    // }
    def ReThrowChildExceptionMethod: InstanceMethod = InstanceMethod(this.desc, "reThrowChildException", MethodTypeDescs.NothingToVoid)

    private def reThrowChildExceptionIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      GETFIELD(ChildExceptionField)
      ifCondition(Condition.NONNULL) {
        thisLoad()
        GETFIELD(ChildExceptionField)
        ATHROW()
      }
      RETURN()
    }

    // final public void runOnExit(Runnable r) {
    //   onExit.addFirst(r);
    // }
    private def RunOnExitMethod: InstanceMethod = InstanceMethod(this.desc, "runOnExit", mkVoidDescriptor(JavaClasses.Runnable))

    private def runOnExitIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      GETFIELD(OnExitField)
      ALOAD(1)
      INVOKEVIRTUAL(ClassConstants.LinkedList.AddFirstMethod)
      RETURN()
    }
  }

  case object Result extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.desc)
      cm.closeClassMaker()
    }

    /**
      * Expects a Result on the stack and leaves a non-Thunk Result.
      * [..., Result] --> [..., Suspension|Value]
      */
    def unwindThunk()(implicit mv: MethodVisitor): Unit = {
      whileLoop(Condition.NE) {
        DUP()
        INSTANCEOF(Thunk.desc)
      } {
        CHECKCAST(Thunk.desc)
        INVOKEINTERFACE(Thunk.InvokeMethod)
      }
    }

    /**
      * Expects a Result on the stack.
      * If the result is a Suspension, this will return a modified Suspension.
      * If the result in NOT a Suspension, this will leave it on the stack.
      * [..., Result] --> [..., Thunk|Value]
      * side effect: Will return a modified suspension if a suspension occurs
      */
    private def handleSuspension(pc: Int, newFrame: MethodVisitor => Unit, setPc: MethodVisitor => Unit)(implicit mv: MethodVisitor): Unit = {
      DUP()
      INSTANCEOF(Suspension.desc)
      ifCondition(Condition.NE) {
        DUP()
        CHECKCAST(Suspension.desc) // [..., s]
        // Add our new frame
        NEW(Suspension.desc)
        DUP()
        INVOKESPECIAL(Suspension.Constructor) // [..., s, s']
        SWAP() // [..., s', s]
        DUP2() // [..., s', s, s', s]
        GETFIELD(Suspension.EffSymField)
        PUTFIELD(Suspension.EffSymField) // [..., s', s]
        DUP2()
        GETFIELD(Suspension.EffOpField)
        PUTFIELD(Suspension.EffOpField) // [..., s', s]
        DUP2()
        GETFIELD(Suspension.ResumptionField)
        PUTFIELD(Suspension.ResumptionField) // [..., s', s]
        DUP2()
        GETFIELD(Suspension.PrefixField) // [..., s', s, s', s.prefix]
        // Make the new frame and push it
        newFrame(mv)
        DUP()
        pushInt(pc)
        setPc(mv)
        INVOKEINTERFACE(Frames.PushMethod) // [..., s', s, s', prefix']
        PUTFIELD(Suspension.PrefixField) // [..., s', s]
        POP() // [..., s']
        // Return the suspension up the stack
        xReturn(Suspension.desc)
      }
    }

    /**
      * Expects a Result on the stack and leaves a Value.
      * This might return if a Suspension is encountered.
      * [..., Result] --> [..., Value.value: tpe]
      * side effect: Will return any Suspension found
      */
    def unwindThunkToValue(pc: Int, newFrame: MethodVisitor => Unit, setPc: MethodVisitor => Unit)(implicit mv: MethodVisitor): Unit = {
      unwindThunk()
      handleSuspension(pc, newFrame, setPc)
      CHECKCAST(Value.desc) // Cannot fail
    }

    /**
      * Expects a Result on the stack and leaves something of the given tpe but erased.
      * Assumes that the result is control-pure, i.e. it is not a suspension and will never return a suspension through a thunk.
      * [..., Result] --> [..., Value.value: tpe]
      * side effect: crashes on suspensions
      */
    def unwindSuspensionFreeThunkToType(tpe: BackendType, errorHint: String, loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
      unwindThunk()
      crashIfSuspension(errorHint, loc)
      CHECKCAST(Value.desc) // Cannot fail
      GETFIELD(Value.fieldFromType(tpe))
      castIfNotPrim(tpe.toClassDesc)
    }

    /**
      * Expects a Result on the stack and leaves a Value.
      * Assumes that the result is control-pure, i.e. it is not a suspension and will never return a suspension through a thunk.
      * [..., Result] --> [..., Value]
      * side effect: crashes on suspensions
      */
    def unwindSuspensionFreeThunk(errorHint: String, loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
      unwindThunk()
      crashIfSuspension(errorHint, loc)
      CHECKCAST(Value.desc)
    }

    /**
      * [..., Result] -> [..., Value|Thunk]
      * side effect: if the result is a suspension, a [[GenUnhandledEffectError]] is thrown.
      */
    def crashIfSuspension(errorHint: String, loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
      DUP()
      INSTANCEOF(Suspension.desc)
      ifCondition(Condition.NE) {
        CHECKCAST(Suspension.desc)
        NEW(GenUnhandledEffectError.desc)
        // [.., suspension, UEE] -> [.., suspension, UEE, UEE, suspension]
        DUP2()
        SWAP()
        pushString(errorHint)
        pushLoc(loc)
        // [.., suspension, UEE, UEE, suspension, info, rsl] -> [.., suspension, UEE]
        INVOKESPECIAL(GenUnhandledEffectError.Constructor)
        ATHROW()
      }
    }
  }

  case object Value extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal, interfaces = List(Result.desc))

      // The fields of all erased types, only one will be relevant
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(BoolField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(CharField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(Int8Field, IsPublic, NotFinal, NotVolatile)
      cm.mkField(Int16Field, IsPublic, NotFinal, NotVolatile)
      cm.mkField(Int32Field, IsPublic, NotFinal, NotVolatile)
      cm.mkField(Int64Field, IsPublic, NotFinal, NotVolatile)
      cm.mkField(Float32Field, IsPublic, NotFinal, NotVolatile)
      cm.mkField(Float64Field, IsPublic, NotFinal, NotVolatile)
      cm.mkField(ObjectField, IsPublic, NotFinal, NotVolatile)

      // Cached singleton Value instances for Unit, true, and false
      cm.mkField(UnitField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(TrueField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(FalseField, IsPublic, IsFinal, NotVolatile)
      cm.mkStaticConstructor(StaticConstructorMethod(this.desc), staticConstructorIns(_))

      cm.closeClassMaker()
    }

    private def staticConstructorIns(implicit mv: MethodVisitor): Unit = {
      // Value.UNIT = new Value(); Value.UNIT.o = Unit.INSTANCE
      NEW(this.desc)
      DUP()
      INVOKESPECIAL(Constructor)
      DUP()
      GETSTATIC(BackendObjType.Unit.SingletonField)
      PUTFIELD(ObjectField)
      PUTSTATIC(UnitField)
      // Value.TRUE = new Value(); Value.TRUE.b = true
      NEW(this.desc)
      DUP()
      INVOKESPECIAL(Constructor)
      DUP()
      ICONST_1()
      PUTFIELD(BoolField)
      PUTSTATIC(TrueField)
      // Value.FALSE = new Value(); Value.FALSE.b = false (default, but explicit)
      NEW(this.desc)
      DUP()
      INVOKESPECIAL(Constructor)
      PUTSTATIC(FalseField)
      RETURN()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    private def BoolField: InstanceField = InstanceField(this.desc, "b", CD_boolean)

    private def CharField: InstanceField = InstanceField(this.desc, "c", CD_char)

    private def Int8Field: InstanceField = InstanceField(this.desc, "i8", CD_byte)

    private def Int16Field: InstanceField = InstanceField(this.desc, "i16", CD_short)

    private def Int32Field: InstanceField = InstanceField(this.desc, "i32", CD_int)

    private def Int64Field: InstanceField = InstanceField(this.desc, "i64", CD_long)

    private def Float32Field: InstanceField = InstanceField(this.desc, "f32", CD_float)

    private def Float64Field: InstanceField = InstanceField(this.desc, "f64", CD_double)

    private def ObjectField: InstanceField = InstanceField(this.desc, "o", CD_Object)

    def UnitField: StaticField = StaticField(this.desc, "UNIT", this.desc)

    def TrueField: StaticField = StaticField(this.desc, "TRUE", this.desc)

    def FalseField: StaticField = StaticField(this.desc, "FALSE", this.desc)

    /**
      * Returns the field of Value corresponding to the given type
      */
    def fieldFromType(tpe: BackendType): InstanceField = {
      import BackendType.*
      tpe match {
        case Bool => BoolField
        case Char => CharField
        case Int8 => Int8Field
        case Int16 => Int16Field
        case Int32 => Int32Field
        case Int64 => Int64Field
        case Float32 => Float32Field
        case Float64 => Float64Field
        case Array(_) | BackendType.Reference(_) => ObjectField
      }
    }
  }

  /** Frame is really just java.util.Function<Value, Result> * */
  case object Frame extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.desc)

      cm.mkInterfaceMethod(ApplyMethod)
      cm.mkStaticInterfaceMethod(StaticApplyMethod, IsPublic, NotFinal, staticApplyIns(_))

      cm.closeClassMaker()
    }

    def ApplyMethod: InterfaceMethod = InterfaceMethod(this.desc, "applyFrame", mkDescriptor(Value.desc)(Result.desc))

    def StaticApplyMethod: StaticInterfaceMethod = StaticInterfaceMethod(
      this.desc,
      "applyFrameStatic",
      mkDescriptor(Frame.desc, Value.desc)(Result.desc)
    )

    private def staticApplyIns(implicit mv: MethodVisitor): Unit = {
      withName(0, Frame.desc) { fun =>
        withName(1, Value.desc) { resumeArg =>
          fun.load()
          resumeArg.load()
          INVOKEINTERFACE(Frame.ApplyMethod)
          ARETURN()
        }
      }
    }
  }

  case object Thunk extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.desc, interfaces = List(Result.desc, JavaClasses.Runnable))

      cm.mkInterfaceMethod(InvokeMethod)
      cm.mkDefaultMethod(RunMethod, IsPublic, NotFinal, runIns(_))

      cm.closeClassMaker()
    }

    def InvokeMethod: InterfaceMethod = InterfaceMethod(this.desc, "invoke", mkDescriptor()(Result.desc))

    private def RunMethod: DefaultMethod = DefaultMethod(this.desc, "run", mkVoidDescriptor())

    private def runIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      Result.unwindSuspensionFreeThunk(s"in ${ClassDescs.binaryNameOf(JavaClasses.Runnable)}", SourceLocation.Unknown)
      POP()
      RETURN()
    }
  }

  case object Suspension extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal, interfaces = List(Result.desc))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(EffSymField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(EffOpField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(PrefixField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(ResumptionField, IsPublic, NotFinal, NotVolatile)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def EffSymField: InstanceField = InstanceField(this.desc, "effSym", JavaClasses.String)

    def EffOpField: InstanceField = InstanceField(this.desc, "effOp", EffectCall.desc)

    def PrefixField: InstanceField = InstanceField(this.desc, "prefix", Frames.desc)

    def ResumptionField: InstanceField = InstanceField(this.desc, "resumption", Resumption.desc)

  }

  case object Frames extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.desc)

      cm.mkInterfaceMethod(PushMethod)
      cm.mkInterfaceMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def PushMethod: InterfaceMethod = InterfaceMethod(this.desc, "push", mkDescriptor(Frame.desc)(Frames.desc))

    def ReverseOntoMethod: InterfaceMethod = InterfaceMethod(this.desc, "reverseOnto", mkDescriptor(Frames.desc)(Frames.desc))

    def pushImplementation(implicit mv: MethodVisitor): Unit = {
      withName(1, Frame.desc) { frame =>
        NEW(FramesCons.desc)
        DUP()
        INVOKESPECIAL(FramesCons.Constructor)
        DUP()
        frame.load()
        PUTFIELD(FramesCons.HeadField)
        DUP()
        thisLoad()
        PUTFIELD(FramesCons.TailField)
        xReturn(FramesCons.desc)
      }
    }
  }

  case object FramesCons extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal, interfaces = List(Frames.desc))

      cm.mkField(HeadField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(TailField, IsPublic, NotFinal, NotVolatile)
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkMethod(Nil, PushMethod, IsPublic, IsFinal, Frames.pushImplementation(_))
      cm.mkMethod(Nil, Frames.ReverseOntoMethod.implementation(this.desc), IsPublic, IsFinal, reverseOntoIns(_))

      cm.closeClassMaker()
    }

    def HeadField: InstanceField = InstanceField(this.desc, "head", Frame.desc)

    def TailField: InstanceField = InstanceField(this.desc, "tail", Frames.desc)

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.desc)

    private def reverseOntoIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Frames.desc) { rest =>
        thisLoad()
        GETFIELD(TailField)
        NEW(FramesCons.desc)
        DUP()
        INVOKESPECIAL(FramesCons.Constructor)
        DUP()
        thisLoad()
        GETFIELD(HeadField)
        PUTFIELD(HeadField)
        DUP()
        rest.load()
        PUTFIELD(TailField)
        INVOKEINTERFACE(Frames.ReverseOntoMethod)
        xReturn(Frames.desc)
      }
    }
  }

  case object FramesNil extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal, interfaces = List(Frames.desc))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkMethod(Nil, PushMethod, IsPublic, IsFinal, Frames.pushImplementation(_))
      cm.mkMethod(Nil, Frames.ReverseOntoMethod.implementation(this.desc), IsPublic, IsFinal, reverseOntoIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.desc)

    private def reverseOntoIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Frames.desc) { rest =>
        rest.load()
        xReturn(rest.tpe)
      }
    }
  }

  case object Resumption extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.desc)
      cm.mkInterfaceMethod(RewindMethod)
      cm.mkStaticInterfaceMethod(StaticRewindMethod, IsPublic, NotFinal, staticRewindIns(_))
      cm.closeClassMaker()
    }

    def RewindMethod: InterfaceMethod = InterfaceMethod(this.desc, "rewind", mkDescriptor(Value.desc)(Result.desc))

    def StaticRewindMethod: StaticInterfaceMethod = StaticInterfaceMethod(this.desc, "staticRewind", mkDescriptor(Resumption.desc, Value.desc)(Result.desc))

    private def staticRewindIns(implicit mv: MethodVisitor): Unit = {
      withName(0, Resumption.desc) { resumption =>
        withName(1, Value.desc) { v =>
          resumption.load()
          v.load()
          INVOKEINTERFACE(Resumption.RewindMethod)
          ARETURN()
        }
      }
    }
  }

  case object ResumptionCons extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal, interfaces = List(Resumption.desc))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

      cm.mkField(SymField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(HandlerField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(FramesField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(TailField, IsPublic, NotFinal, NotVolatile)

      cm.mkMethod(Nil, Resumption.RewindMethod.implementation(this.desc), IsPublic, IsFinal, rewindIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def SymField: InstanceField = InstanceField(this.desc, "sym", JavaClasses.String)

    def HandlerField: InstanceField = InstanceField(this.desc, "handler", Handler.desc)

    def FramesField: InstanceField = InstanceField(this.desc, "frames", Frames.desc)

    def TailField: InstanceField = InstanceField(this.desc, "tail", Resumption.desc)

    private def rewindIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Value.desc) { v =>
        thisLoad()
        GETFIELD(SymField)
        thisLoad()
        GETFIELD(HandlerField)
        thisLoad()
        GETFIELD(FramesField)
        // () -> tail.rewind(v)
        thisLoad()
        GETFIELD(TailField)
        v.load()
        mkStaticLambda(Thunk.InvokeMethod, Resumption.StaticRewindMethod, drop = 0)
        mkStaticLambda(Thunk.InvokeMethod, Handler.InstallHandlerMethod, drop = 0)
        xReturn(Thunk.desc)
      }
    }
  }

  case object ResumptionNil extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal, interfaces = List(Resumption.desc))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkMethod(Nil, Resumption.RewindMethod.implementation(this.desc), IsPublic, IsFinal, rewindIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    private def rewindIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Value.desc) { v =>
        v.load()
        xReturn(v.tpe)
      }
    }
  }

  case object Handler extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.desc)
      cm.mkStaticInterfaceMethod(InstallHandlerMethod, IsPublic, NotFinal, installHandlerIns(_))
      cm.closeClassMaker()
    }

    def InstallHandlerMethod: StaticInterfaceMethod = StaticInterfaceMethod(
      this.desc,
      "installHandler",
      mkDescriptor(JavaClasses.String, Handler.desc, Frames.desc, Thunk.desc)(Result.desc)
    )

    private def installHandlerIns(implicit mv: MethodVisitor): Unit = {
      withName(0, JavaClasses.String) { effSym =>
        withName(1, Handler.desc) { handler =>
          withName(2, Frames.desc) { frames =>
            withName(3, Thunk.desc) { thunk =>
              thunk.load()
              // Thunk|Value|Suspension
              Result.unwindThunk()
              // Value|Suspension
              // handle suspension
              DUP()
              INSTANCEOF(Suspension.desc)
              ifCondition(Condition.NE) {
                DUP()
                CHECKCAST(Suspension.desc)
                storeWithName(4, Suspension.desc) { s =>
                  NEW(ResumptionCons.desc)
                  DUP()
                  INVOKESPECIAL(ResumptionCons.Constructor)
                  DUP()
                  effSym.load()
                  PUTFIELD(ResumptionCons.SymField)
                  DUP()
                  handler.load()
                  PUTFIELD(ResumptionCons.HandlerField)
                  DUP()
                  s.load()
                  GETFIELD(Suspension.PrefixField)
                  frames.load()
                  INVOKEINTERFACE(Frames.ReverseOntoMethod)
                  PUTFIELD(ResumptionCons.FramesField)
                  DUP()
                  s.load()
                  GETFIELD(Suspension.ResumptionField)
                  PUTFIELD(ResumptionCons.TailField)
                  storeWithName(5, ResumptionCons.desc) { r =>
                    s.load()
                    GETFIELD(Suspension.EffSymField)
                    effSym.load()
                    INVOKEVIRTUAL(ClassConstants.Object.EqualsMethod)
                    ifCondition(Condition.NE) {
                      s.load()
                      GETFIELD(Suspension.EffOpField)
                      handler.load()
                      r.load()
                      INVOKEINTERFACE(EffectCall.ApplyMethod)
                      xReturn(Result.desc)
                    }
                    NEW(Suspension.desc)
                    DUP()
                    INVOKESPECIAL(Suspension.Constructor)
                    DUP()
                    s.load()
                    GETFIELD(Suspension.EffSymField)
                    PUTFIELD(Suspension.EffSymField)
                    DUP()
                    s.load()
                    GETFIELD(Suspension.EffOpField)
                    PUTFIELD(Suspension.EffOpField)
                    DUP()
                    NEW(FramesNil.desc)
                    DUP()
                    INVOKESPECIAL(FramesNil.Constructor)
                    PUTFIELD(Suspension.PrefixField)
                    DUP()
                    r.load()
                    PUTFIELD(Suspension.ResumptionField)
                    xReturn(Suspension.desc)
                  }
                }
              }

              // Value
              CHECKCAST(Value.desc)
              storeWithName(6, Value.desc) { res =>
                //
                // Case on frames
                // FramesNil
                frames.load()
                INSTANCEOF(FramesNil.desc)
                ifCondition(Condition.NE) {
                  res.load()
                  xReturn(Value.desc)
                }
                // FramesCons
                frames.load()
                CHECKCAST(FramesCons.desc)
                storeWithName(7, FramesCons.desc) { cons => {
                  effSym.load()
                  handler.load()
                  cons.load()
                  GETFIELD(FramesCons.TailField)
                  // thunk
                  cons.load()
                  GETFIELD(FramesCons.HeadField)
                  res.load()
                  mkStaticLambda(Thunk.InvokeMethod, Frame.StaticApplyMethod, drop = 0)
                  INVOKESTATIC(InstallHandlerMethod)
                  xReturn(Result.desc)
                }
                }
              }
            }
          }
        }
      }
    }
  }

  case object EffectCall extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.desc)
      cm.mkInterfaceMethod(ApplyMethod)
      cm.closeClassMaker()
    }

    def ApplyMethod: InterfaceMethod = InterfaceMethod(this.desc, "apply", mkDescriptor(Handler.desc, Resumption.desc)(Result.desc))

  }

  case class ResumptionWrapper(tpe: BackendType) extends BackendObjType {

    // tpe -> Result
    private val superClass: AbstractArrow = AbstractArrow(List(tpe.toErased), BackendType.Object)

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.desc, IsFinal, superClass.desc)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkField(ResumptionField, IsPrivate, IsFinal, NotVolatile)
      cm.mkMethod(Nil, InvokeMethod, IsPublic, NotFinal, invokeIns(_))
      cm.mkMethod(Nil, UniqueMethod, IsPublic, NotFinal, uniqueIns(_))
      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, List(Resumption.desc))

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Resumption.desc) { resumption =>
        thisLoad()
        INVOKESPECIAL(superClass.desc, ConstructorMethodName, MethodTypeDescs.NothingToVoid)
        thisLoad()
        resumption.load()
        PUTFIELD(ResumptionField)
        RETURN()
      }
    }

    def ResumptionField: InstanceField = InstanceField(this.desc, "resumption", Resumption.desc)

    def InvokeMethod: InstanceMethod = Thunk.InvokeMethod.implementation(this.desc)

    private def invokeIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      GETFIELD(ResumptionField)
      tpe.toErased match {
        case BackendType.Bool =>
          // Use cached Value.TRUE / Value.FALSE singletons
          thisLoad()
          mv.visitFieldInsn(Opcodes.GETFIELD, ClassDescs.internalNameOf(desc), "arg0", tpe.toErased.toDescriptor)
          val falseLabel = new Label()
          val doneLabel = new Label()
          mv.visitJumpInsn(Opcodes.IFEQ, falseLabel)
          GETSTATIC(Value.TrueField)
          mv.visitJumpInsn(Opcodes.GOTO, doneLabel)
          mv.visitLabel(falseLabel)
          GETSTATIC(Value.FalseField)
          mv.visitLabel(doneLabel)
        case _ =>
          NEW(Value.desc)
          DUP()
          INVOKESPECIAL(Value.Constructor)
          DUP()
          thisLoad()
          mv.visitFieldInsn(Opcodes.GETFIELD, ClassDescs.internalNameOf(desc), "arg0", tpe.toErased.toDescriptor)
          PUTFIELD(Value.fieldFromType(tpe.toErased))
      }
      INVOKEINTERFACE(Resumption.RewindMethod)
      xReturn(Result.desc)
    }

    private def UniqueMethod: InstanceMethod = InstanceMethod(this.desc, "getUniqueThreadClosure", mkDescriptor()(this.superClass.desc))

    private def uniqueIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      ARETURN()
    }
  }
}
