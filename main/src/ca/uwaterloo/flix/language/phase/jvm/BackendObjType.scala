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
import ca.uwaterloo.flix.language.phase.jvm.classes.{GenReifiedSourceLocation, GenResult, GenThunk, GenUncaughtExceptionHandler, GenValue}
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
    case BackendObjType.Lazy(tpe) => mkDesc(RootPackage, Mangle.mkClassName("Lazy", Mangle.erasedName(tpe)))
    case BackendObjType.Tuple(elms) => mkDesc(RootPackage, Mangle.mkClassName("Tuple", elms.map(Mangle.erasedName)))
    case BackendObjType.Struct(elms) => mkDesc(RootPackage, Mangle.mkClassName("Struct", elms.map(Mangle.erasedName)))
    case BackendObjType.Tagged => mkDesc(RootPackage, mkClassName("Tagged"))
    case BackendObjType.ExtTagged => mkDesc(RootPackage, mkClassName("ExtTagged"))
    case BackendObjType.AbstractArrow(args, result) => mkDesc(RootPackage, Mangle.mkClassName(s"Clo${args.length}", (args :+ result).map(Mangle.erasedName)))
    case BackendObjType.Arrow(args, result) => mkDesc(RootPackage, Mangle.mkClassName(s"Fn${args.length}", (args :+ result).map(Mangle.erasedName)))
    case BackendObjType.RecordEmpty => mkDesc(RootPackage, mkClassName(s"RecordEmpty"))
    case BackendObjType.Record => mkDesc(RootPackage, mkClassName("Record"))
    case BackendObjType.Region => mkDesc(DevFlixRuntime, mkClassName("Region"))
    // Java classes
    case BackendObjType.Native(clazz) => clazz
    // Effects Runtime
  }

  /**
    * The JVM type descriptor of the form `"L<internal name>;"`.
    */
  def toDescriptor: String = desc.descriptorString()
}

object BackendObjType {

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

  case class Lazy(tpe: ClassDesc) extends BackendObjType {

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

    def ValueField: InstanceField = InstanceField(this.desc, "value", tpe)

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

    def ForceMethod: InstanceMethod = InstanceMethod(this.desc, "force", mkDescriptor()(tpe))

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
          CHECKCAST(GenThunk.desc)
          // this.value = thunk.unwind()
          GenResult.unwindSuspensionFreeThunkToType(tpe, "during call to Lazy.force", SourceLocation.Unknown)
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
      xReturn(tpe)
    }
  }

  case class Tuple(elms: List[ClassDesc]) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.desc, IsFinal)

      elms.indices.foreach(i => cm.mkField(IndexField(i), IsPublic, NotFinal, NotVolatile))
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

      cm.closeClassMaker()
    }

    def IndexField(i: Int): InstanceField = InstanceField(this.desc, s"field$i", elms(i))

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, elms)

    /** `[] --> return` */
    private def constructorIns(implicit mv: MethodVisitor): Unit =
      withNames(1, elms) { case (_, variables) =>
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
      Struct(struct.fields.map(field => TypeDescs.toErasedClassDesc(field.tpe)))
  }

  case class Struct(elms: List[ClassDesc]) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.desc, IsFinal)

      elms.indices.foreach(i => cm.mkField(IndexField(i), IsPublic, NotFinal, NotVolatile))
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

      cm.closeClassMaker()
    }

    def IndexField(i: Int): InstanceField = InstanceField(this.desc, s"field$i", elms(i))

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, elms)

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      withNames(1, elms) { case (_, variables) =>
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
        AbstractArrow(targs.map(TypeDescs.toErasedClassDesc), TypeDescs.toErasedClassDesc(tresult))
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
  case class AbstractArrow(args: List[ClassDesc], result: ClassDesc) extends BackendObjType {

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
    def fromArrowType(tpe: SimpleType): Arrow = tpe match {
      case SimpleType.Arrow(targs, tresult) =>
        Arrow(targs.map(TypeDescs.toErasedClassDesc), TypeDescs.toErasedClassDesc(tresult))
      case _ =>
        throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
    }
  }

  case class Arrow(args: List[ClassDesc], result: ClassDesc) extends BackendObjType {

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
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case ObjConsumer =>
          thisLoad()
          DUP()
          ALOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case ObjPredicate =>
          thisLoad()
          DUP()
          ALOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case IntFunction =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case IntConsumer =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case IntPredicate =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case IntUnaryOperator =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_int, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case LongFunction =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case LongConsumer =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case LongPredicate =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case LongUnaryOperator =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_long, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          LRETURN()
        case DoubleFunction =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          ARETURN()
        case DoubleConsumer =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_Object, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          RETURN()
        case DoublePredicate =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_boolean, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
          IRETURN()
        case DoubleUnaryOperator =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          GenResult.unwindSuspensionFreeThunkToType(CD_double, s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
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

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val specializedInterface = specialization()
      val interfaces = GenThunk.desc :: specializedInterface.map(_.desc)

      val cm = ClassMaker.mkAbstractClass(this.desc, superClass = CD_Object, interfaces)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      args.indices.foreach(argIndex => cm.mkField(ArgField(argIndex), IsPublic, NotFinal, NotVolatile))
      specializedInterface.foreach(i => cm.mkMethod(i.functionMethod, IsPublic, NotFinal, i.functionIns(_)))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

    def ArgField(index: Int): InstanceField = InstanceField(this.desc, s"arg$index", args(index))
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
}
