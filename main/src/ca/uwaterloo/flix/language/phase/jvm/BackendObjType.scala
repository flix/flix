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
import ca.uwaterloo.flix.language.ast.{JvmAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.mkClassName
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.*
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.{IsVolatile, NotVolatile}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{DevFlixRuntime, MethodDescriptor, RootPackage}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.{MethodVisitor, Opcodes}

/**
  * Represents all Flix types that are objects on the JVM (array is an exception).
  */
sealed trait BackendObjType {
  /**
    * The `JvmName` that represents the type `Ref(Int)` refers to `"Ref$Int"`.
    */
  val jvmName: JvmName = this match {
    case BackendObjType.Unit => JvmName(DevFlixRuntime, mkClassName("Unit"))
    case BackendObjType.Lazy(tpe) => JvmName(RootPackage, mkClassName("Lazy", tpe))
    case BackendObjType.Tuple(elms) => JvmName(RootPackage, mkClassName("Tuple", elms))
    case BackendObjType.Struct(elms) => JvmName(RootPackage, mkClassName("Struct", elms))
    case BackendObjType.NullaryTag(sym) => JvmName(RootPackage, mkClassName(sym.toString))
    case BackendObjType.Tagged => JvmName(RootPackage, mkClassName("Tagged"))
    case BackendObjType.Tag(tpes) => JvmName(RootPackage, mkClassName("Tag", tpes))
    case BackendObjType.AbstractArrow(args, result) => JvmName(RootPackage, mkClassName(s"Clo${args.length}", args :+ result))
    case BackendObjType.Arrow(args, result) => JvmName(RootPackage, mkClassName(s"Fn${args.length}", args :+ result))
    case BackendObjType.Defn(sym) => JvmName(sym.namespace, JvmName.mkClassName("Def", sym.name))
    case BackendObjType.RecordEmpty => JvmName(RootPackage, mkClassName(s"RecordEmpty"))
    case BackendObjType.RecordExtend(value) => JvmName(RootPackage, mkClassName("RecordExtend", value))
    case BackendObjType.Record => JvmName(RootPackage, mkClassName("Record"))
    case BackendObjType.ReifiedSourceLocation => JvmName(DevFlixRuntime, mkClassName("ReifiedSourceLocation"))
    case BackendObjType.Global => JvmName(DevFlixRuntime, "Global") // "Global" is fixed in source code, so should not be mangled and $ suffixed
    case BackendObjType.HoleError => JvmName(DevFlixRuntime, mkClassName("HoleError"))
    case BackendObjType.MatchError => JvmName(DevFlixRuntime, mkClassName("MatchError"))
    case BackendObjType.CastError => JvmName(DevFlixRuntime, mkClassName("CastError"))
    case BackendObjType.UnhandledEffectError => JvmName(DevFlixRuntime, mkClassName("UnhandledEffectError"))
    case BackendObjType.Region => JvmName(DevFlixRuntime, mkClassName("Region"))
    case BackendObjType.UncaughtExceptionHandler => JvmName(DevFlixRuntime, mkClassName("UncaughtExceptionHandler"))
    case BackendObjType.Main => JvmName(RootPackage, "Main")
    case BackendObjType.Namespace(ns) => JvmName(ns.dropRight(1), ns.lastOption.getOrElse(s"Root${Flix.Delimiter}"))
    // Java classes
    case BackendObjType.Native(className) => className
    // Effects Runtime
    case BackendObjType.Result => JvmName(DevFlixRuntime, mkClassName("Result"))
    case BackendObjType.Value => JvmName(DevFlixRuntime, mkClassName("Value"))
    case BackendObjType.Frame => JvmName(DevFlixRuntime, mkClassName("Frame"))
    case BackendObjType.Thunk => JvmName(DevFlixRuntime, mkClassName("Thunk"))
    case BackendObjType.Suspension => JvmName(DevFlixRuntime, mkClassName("Suspension"))
    case BackendObjType.Frames => JvmName(DevFlixRuntime, mkClassName("Frames"))
    case BackendObjType.FramesCons => JvmName(DevFlixRuntime, mkClassName("FramesCons"))
    case BackendObjType.FramesNil => JvmName(DevFlixRuntime, mkClassName("FramesNil"))
    case BackendObjType.Resumption => JvmName(DevFlixRuntime, mkClassName("Resumption"))
    case BackendObjType.ResumptionCons => JvmName(DevFlixRuntime, mkClassName("ResumptionCons"))
    case BackendObjType.ResumptionNil => JvmName(DevFlixRuntime, mkClassName("ResumptionNil"))
    case BackendObjType.Handler => JvmName(DevFlixRuntime, mkClassName("Handler"))
    case BackendObjType.EffectCall => JvmName(DevFlixRuntime, mkClassName("EffectCall"))
    case BackendObjType.ResumptionWrapper(t) => JvmName(DevFlixRuntime, mkClassName("ResumptionWrapper", t))
  }

  /**
    * The JVM type descriptor of the form `"L<jvmName.toInternalName>;"`.
    */
  def toDescriptor: String = jvmName.toDescriptor

  /**
    * Returns `this` wrapped in `BackendType.Reference`.
    */
  def toTpe: BackendType.Reference = BackendType.Reference(this)

  /** `[] --> return` */
  protected def nullarySuperConstructor(superClass: ConstructorMethod)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(superClass)
    RETURN()
  }

  /** `[] --> return` */
  protected def singletonStaticConstructor(thisConstructor: ConstructorMethod, singleton: StaticField)(implicit mv: MethodVisitor): Unit = {
    NEW(this.jvmName)
    DUP()
    INVOKESPECIAL(thisConstructor)
    PUTSTATIC(singleton)
    RETURN()
  }
}

object BackendObjType {

  private def mkClassName(prefix: String, tpe: BackendType): String = {
    JvmName.mkClassName(prefix, tpe.toErasedString)
  }

  private def mkClassName(prefix: String, tpes: List[BackendType]): String = {
    JvmName.mkClassName(prefix, tpes.map(_.toErasedString))
  }

  private def mkClassName(prefix: String): String = {
    JvmName.mkClassName(prefix)
  }

  case object Unit extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkStaticConstructor(StaticConstructorMethod(this.jvmName), singletonStaticConstructor(Constructor, SingletonField)(_))
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)
      cm.mkMethod(Nil, ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def SingletonField: StaticField = StaticField(this.jvmName, "INSTANCE", this.toTpe)

    /** `[] --> return String` */
    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      pushString("()")
      ARETURN()
    }

  }

  case class Lazy(tpe: BackendType) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkField(ExpField, IsPublic, NotFinal, IsVolatile)
      cm.mkField(ValueField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(LockField, IsPrivate, NotFinal, NotVolatile)
      cm.mkMethod(Nil, ForceMethod, IsPublic, IsFinal, forceIns(_))

      cm.closeClassMaker()
    }

    def ExpField: InstanceField = InstanceField(this.jvmName, "expression", BackendType.Object)

    def ValueField: InstanceField = InstanceField(this.jvmName, "value", tpe)

    private def LockField: InstanceField = InstanceField(this.jvmName, "lock", JvmName.ReentrantLock.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, List(BackendType.Object))

    /** `[] --> return` */
    private def constructorIns(implicit mv: MethodVisitor): Unit =
      withName(1, BackendType.Object)(exp => {
        // super()
        thisLoad()
        INVOKESPECIAL(ClassConstants.Object.Constructor)
        // this.exp = exp
        thisLoad()
        exp.load()
        PUTFIELD(ExpField)
        // this.lock = new ReentrantLock()
        thisLoad()
        NEW(JvmName.ReentrantLock)
        DUP()
        INVOKESPECIAL(ClassConstants.ReentrantLock.Constructor)
        PUTFIELD(LockField)
        // return
        RETURN()
      })

    def ForceMethod: InstanceMethod = InstanceMethod(this.jvmName, "force", mkDescriptor()(tpe))

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
          CHECKCAST(Thunk.jvmName)
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
      xReturn(tpe)
    }
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      elms.indices.foreach(i => cm.mkField(IndexField(i), IsPublic, NotFinal, NotVolatile))
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkMethod(Nil, ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))

      cm.closeClassMaker()
    }

    def IndexField(i: Int): InstanceField = InstanceField(this.jvmName, s"field$i", elms(i))

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, elms)

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

    /** `[] --> return String` */
    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      Util.mkString(Some(_ => pushString("(")), Some(_ => pushString(")")), elms.length, getIndexField)
      xReturn(BackendType.String)
    }

    /** `[] --> [this.index(i).xString()]` */
    private def getIndexField(i: Int)(implicit mv: MethodVisitor): Unit = {
      val field = IndexField(i)
      thisLoad()
      GETFIELD(field)
      xToString(field.tpe)
    }

  }

  case class Struct(elms: List[BackendType]) extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      elms.indices.foreach(i => cm.mkField(IndexField(i), IsPublic, NotFinal, NotVolatile))
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkMethod(Nil, ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))

      cm.closeClassMaker()
    }

    def IndexField(i: Int): InstanceField = InstanceField(this.jvmName, s"field$i", elms(i))

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, elms)

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

    /** `[] --> return String` */
    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      Util.mkString(Some(_ => pushString("Struct(")), Some(_ => pushString(")")), elms.length, getIndexString)
      xReturn(BackendType.String)
    }

    /** `[] --> [this.index(i).xString()]` */
    private def getIndexString(i: Int)(implicit mv: MethodVisitor): Unit = {
      val field = IndexField(i)
      thisLoad()
      GETFIELD(field)
      xToString(field.tpe)
    }

  }

  case object Tagged extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.jvmName)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

      cm.mkField(NameField, IsPublic, NotFinal, NotVolatile)

      cm.closeClassMaker()
    }

    def NameField: InstanceField = InstanceField(this.jvmName, "tag", BackendType.String)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    /** [...] -> [..., tagName] */
    def mkTagName(name: String)(implicit mv: MethodVisitor): Unit = pushString(JvmOps.getTagName(name))

    /** [..., tagName1, tagName2] --> [..., tagName1 == tagName2] */
    def eqTagName()(implicit mv: MethodVisitor): Unit = {
      // ACMP is okay since tag strings are loaded through ldc instructions
      ifConditionElse(Condition.ACMPEQ)(pushBool(true))(pushBool(false))
    }
  }

  sealed trait TagType extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte]
  }

  case class NullaryTag(name: String) extends TagType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, superClass = Tagged.jvmName)

      cm.mkStaticConstructor(StaticConstructorMethod(this.jvmName), singletonStaticConstructor(Constructor, SingletonField)(_))
      cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkMethod(Nil, ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))

      cm.closeClassMaker()
    }

    def SingletonField: StaticField = StaticField(this.jvmName, "singleton", this.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    /** `[] --> return` */
    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      INVOKESPECIAL(Tagged.Constructor)
      thisLoad()
      Tagged.mkTagName(name)
      PUTFIELD(Tagged.NameField)
      RETURN()
    }

    /** `[] --> return String` */
    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      Tagged.mkTagName(name)
      xReturn(BackendType.String)
    }
  }

  case class Tag(elms: List[BackendType]) extends TagType {
    if (elms.isEmpty) throw InternalCompilerException(s"Unexpected nullary Tag type", SourceLocation.Unknown)

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, superClass = Tagged.jvmName)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(Tagged.Constructor)(_))
      elms.indices.foreach(i => cm.mkField(IndexField(i), IsPublic, NotFinal, NotVolatile))
      cm.mkMethod(Nil, ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))

      cm.closeClassMaker()
    }

    def NameField: InstanceField = Tagged.NameField

    def IndexField(i: Int): InstanceField = InstanceField(this.jvmName, s"v$i", elms(i))

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    /** `[] --> return String` */
    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      Util.mkString(Some({ _ =>
        thisLoad()
        GETFIELD(NameField)
        pushString("(")
        INVOKEVIRTUAL(ClassConstants.String.Concat)
      }), Some(_ => pushString(")")), elms.length, getIndexString)
      xReturn(BackendType.String)
    }

    /** `[] --> [this.index(i).xString()]` */
    private def getIndexString(i: Int)(implicit mv: MethodVisitor): Unit = {
      val field = IndexField(i)
      thisLoad()
      GETFIELD(field)
      xToString(field.tpe)
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
      val cm = ClassMaker.mkAbstractClass(this.jvmName, superClass.jvmName)
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(superClass.Constructor)(_))

      cm.mkAbstractMethod(GetUniqueThreadClosureMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def GetUniqueThreadClosureMethod: AbstractMethod = AbstractMethod(this.jvmName, "getUniqueThreadClosure", mkDescriptor()(this.toTpe))

  }

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType {

    /**
      * Represents a function interface from `java.util.function`.
      */
    sealed trait FunctionInterface {
      /**
        * The JvmName of the interface.
        */
      def jvmName: JvmName = this match {
        case ObjFunction => JvmName.ObjFunction
        case ObjConsumer => JvmName.ObjConsumer
        case ObjPredicate => JvmName.ObjPredicate
        case IntFunction => JvmName.IntFunction
        case IntConsumer => JvmName.IntConsumer
        case IntPredicate => JvmName.IntPredicate
        case IntUnaryOperator => JvmName.IntUnaryOperator
        case LongFunction => JvmName.LongFunction
        case LongConsumer => JvmName.LongConsumer
        case LongPredicate => JvmName.LongPredicate
        case LongUnaryOperator => JvmName.LongUnaryOperator
        case DoubleFunction => JvmName.DoubleFunction
        case DoubleConsumer => JvmName.DoubleConsumer
        case DoublePredicate => JvmName.DoublePredicate
        case DoubleUnaryOperator => JvmName.DoubleUnaryOperator
      }

      /**
        * The required method of the interface.
        * These methods should do the same as a non-tail call in genExpression.
        */
      def functionMethod: InstanceMethod = this match {
        case ObjFunction => InstanceMethod(this.jvmName, "apply",
          mkDescriptor(BackendType.Object)(BackendType.Object))
        case ObjConsumer => InstanceMethod(this.jvmName, "accept",
          mkDescriptor(BackendType.Object)(VoidableType.Void))
        case ObjPredicate => InstanceMethod(this.jvmName, "test",
          mkDescriptor(BackendType.Object)(BackendType.Bool))
        case IntFunction => InstanceMethod(this.jvmName, "apply",
          mkDescriptor(BackendType.Int32)(BackendType.Object))
        case IntConsumer => InstanceMethod(this.jvmName, "accept",
          mkDescriptor(BackendType.Int32)(VoidableType.Void))
        case IntPredicate => InstanceMethod(this.jvmName, "test",
          mkDescriptor(BackendType.Int32)(BackendType.Bool))
        case IntUnaryOperator => InstanceMethod(this.jvmName, "applyAsInt",
          mkDescriptor(BackendType.Int32)(BackendType.Int32))
        case LongFunction => InstanceMethod(this.jvmName, "apply",
          mkDescriptor(BackendType.Int64)(BackendType.Object))
        case LongConsumer => InstanceMethod(this.jvmName, "accept",
          mkDescriptor(BackendType.Int64)(VoidableType.Void))
        case LongPredicate => InstanceMethod(this.jvmName, "test",
          mkDescriptor(BackendType.Int64)(BackendType.Bool))
        case LongUnaryOperator => InstanceMethod(this.jvmName, "applyAsLong",
          mkDescriptor(BackendType.Int64)(BackendType.Int64))
        case DoubleFunction => InstanceMethod(this.jvmName, "apply",
          mkDescriptor(BackendType.Float64)(BackendType.Object))
        case DoubleConsumer => InstanceMethod(this.jvmName, "accept",
          mkDescriptor(BackendType.Float64)(VoidableType.Void))
        case DoublePredicate => InstanceMethod(this.jvmName, "test",
          mkDescriptor(BackendType.Float64)(BackendType.Bool))
        case DoubleUnaryOperator => InstanceMethod(this.jvmName, "applyAsDouble",
          mkDescriptor(BackendType.Float64)(BackendType.Float64))
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
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          ARETURN()
        case ObjConsumer =>
          thisLoad()
          DUP()
          ALOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          RETURN()
        case ObjPredicate =>
          thisLoad()
          DUP()
          ALOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          IRETURN()
        case IntFunction =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          ARETURN()
        case IntConsumer =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          RETURN()
        case IntPredicate =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          IRETURN()
        case IntUnaryOperator =>
          thisLoad()
          DUP()
          ILOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Int32, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          IRETURN()
        case LongFunction =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          ARETURN()
        case LongConsumer =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          RETURN()
        case LongPredicate =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          IRETURN()
        case LongUnaryOperator =>
          thisLoad()
          DUP()
          LLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Int64, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          LRETURN()
        case DoubleFunction =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          ARETURN()
        case DoubleConsumer =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Object, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          RETURN()
        case DoublePredicate =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
          IRETURN()
        case DoubleUnaryOperator =>
          thisLoad()
          DUP()
          DLOAD(1)
          PUTFIELD(ArgField(0))
          Result.unwindSuspensionFreeThunkToType(BackendType.Float64, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown)
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
        case (BackendType.Reference(BackendObjType.Native(JvmName.Object)) :: Nil, _) =>
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
      val interfaces = Thunk.jvmName :: specializedInterface.map(_.jvmName)

      val cm = ClassMaker.mkAbstractClass(this.jvmName, superClass = JvmName.Object, interfaces)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      args.indices.foreach(argIndex => cm.mkField(ArgField(argIndex), IsPublic, NotFinal, NotVolatile))
      specializedInterface.foreach(i => cm.mkMethod(i.functionMethod, IsPublic, NotFinal, i.functionIns(_)))
      cm.mkMethod(ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def ArgField(index: Int): InstanceField = InstanceField(this.jvmName, s"arg$index", args(index))

    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      val argString = args match {
        case Nil => "()"
        case arg :: Nil => arg.toErasedString
        case _ => args.map(_.toErasedString).mkString("(", ", ", ")")
      }
      pushString(s"$argString -> ${result.toErasedString}")
      ARETURN()
    }
  }

  case class Defn(sym: Symbol.DefnSym) extends BackendObjType

  case object RecordEmpty extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(this.interface.jvmName))

      cm.mkStaticConstructor(StaticConstructorMethod(this.jvmName), singletonStaticConstructor(Constructor, SingletonField)(_))
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)
      cm.mkMethod(Nil, LookupFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))
      cm.mkMethod(Nil, RestrictFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))
      cm.mkMethod(Nil, ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))
      cm.mkMethod(Nil, ToTailStringMethod, IsPublic, IsFinal, toTailStringIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def interface: Record.type = Record

    def SingletonField: StaticField = StaticField(this.jvmName, "INSTANCE", this.toTpe)

    private def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName)

    private def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName)

    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      pushString("{}")
      ARETURN()
    }

    private def ToTailStringMethod: InstanceMethod = interface.ToTailStringMethod.implementation(this.jvmName)

    private def toTailStringIns(implicit mv: MethodVisitor): Unit = {
      withName(1, JvmName.StringBuilder.toTpe) { sb =>
        sb.load()
        pushString("}")
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
        ARETURN()
      }
    }

    private def throwUnsupportedExc(implicit mv: MethodVisitor): Unit = {
      throwUnsupportedOperationException(
        s"${Record.LookupFieldMethod.name} method shouldn't be called")
    }
  }

  case class RecordExtend(value: BackendType) extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(Record.jvmName))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(LabelField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(ValueField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(RestField, IsPublic, NotFinal, NotVolatile)
      cm.mkMethod(Nil, Record.LookupFieldMethod.implementation(this.jvmName), IsPublic, IsFinal, lookupFieldIns(_))
      cm.mkMethod(Nil, RestrictFieldMethod, IsPublic, IsFinal, restrictFieldIns(_))
      cm.mkMethod(Nil, ClassConstants.Object.ToStringMethod.implementation(this.jvmName), IsPublic, NotFinal, toStringIns(_))
      cm.mkMethod(Nil, Record.ToTailStringMethod.implementation(this.jvmName), IsPublic, IsFinal, toTailStringIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def LabelField: InstanceField = InstanceField(this.jvmName, "label", BackendType.String)

    def ValueField: InstanceField = InstanceField(this.jvmName, "value", value)

    def RestField: InstanceField = InstanceField(this.jvmName, "rest", Record.toTpe)

    private def lookupFieldIns(implicit mv: MethodVisitor): Unit = {
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad()
          ARETURN()
        case FalseBranch =>
          thisLoad()
          GETFIELD(RestField)
          ALOAD(1)
          INVOKEINTERFACE(Record.LookupFieldMethod)
          ARETURN()
      }
    }

    def RestrictFieldMethod: InstanceMethod = Record.RestrictFieldMethod.implementation(this.jvmName)

    private def restrictFieldIns(implicit mv: MethodVisitor): Unit = {
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad()
          GETFIELD(RestField)
          ARETURN()
        case FalseBranch =>
          NEW(this.jvmName)
          DUP()
          INVOKESPECIAL(this.Constructor)
          DUP()
          thisLoad()
          GETFIELD(LabelField)
          PUTFIELD(LabelField)
          DUP()
          thisLoad()
          GETFIELD(ValueField)
          PUTFIELD(ValueField)
          DUP() // get the new restricted rest to put
          thisLoad()
          GETFIELD(RestField)
          ALOAD(1)
          INVOKEINTERFACE(Record.RestrictFieldMethod)
          PUTFIELD(RestField) // put the rest field and return
          ARETURN()
      }
    }

    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      // save the `rest` for the last recursive call
      thisLoad()
      GETFIELD(this.RestField)
      // build this segment of the string
      NEW(JvmName.StringBuilder)
      DUP()
      INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
      pushString("{")
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      thisLoad()
      GETFIELD(this.LabelField)
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      pushString(" = ")
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      thisLoad()
      GETFIELD(this.ValueField)
      xToString(this.ValueField.tpe)
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      INVOKEINTERFACE(Record.ToTailStringMethod)
      ARETURN()
    }

    private def toTailStringIns(implicit mv: MethodVisitor): Unit = {
      withName(1, JvmName.StringBuilder.toTpe) { sb =>
        // save the `rest` for the last recursive call
        thisLoad()
        GETFIELD(this.RestField)
        // build this segment of the string
        sb.load()
        pushString(", ")
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        thisLoad()
        GETFIELD(this.LabelField)
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        pushString(" = ")
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        thisLoad()
        GETFIELD(this.ValueField)
        xToString(this.ValueField.tpe)
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        // call the tailString of `rest`
        INVOKEINTERFACE(Record.ToTailStringMethod)
        ARETURN()

      }
    }

    /**
      * Compares the label of `this`and `ALOAD(1)` and executes the designated branch.
      */
    private def caseOnLabelEquality(cases: Branch => Unit)(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      GETFIELD(LabelField)
      ALOAD(1)
      INVOKEVIRTUAL(ClassConstants.Object.EqualsMethod)
      branch(Condition.Bool)(cases)
    }
  }

  case object Record extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkInterface(this.jvmName)

      cm.mkInterfaceMethod(LookupFieldMethod)
      cm.mkInterfaceMethod(RestrictFieldMethod)
      cm.mkInterfaceMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "lookupField",
      mkDescriptor(BackendType.String)(this.toTpe))

    def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "restrictField",
      mkDescriptor(BackendType.String)(this.toTpe))

    def ToTailStringMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "toTailString",
      mkDescriptor(JvmName.StringBuilder.toTpe)(BackendType.String))
  }

  /**
    * Represents a JVM type not represented in BackendObjType.
    * This should not be used for `java.lang.String` for example since `BackendObjType.String`
    * represents this type.
    */
  case class Native(className: JvmName) extends BackendObjType

  case object ReifiedSourceLocation extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

      cm.mkField(SourceField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(BeginLineField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(BeginColField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(EndLineField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(EndColField, IsPublic, IsFinal, NotVolatile)

      cm.mkMethod(Nil, ToStringMethod, IsPublic, NotFinal, toStringIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(
      this.jvmName, List(BackendType.String, BackendType.Int32, BackendType.Int32, BackendType.Int32, BackendType.Int32)
    )

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      INVOKESPECIAL(ClassConstants.Object.Constructor)
      thisLoad()
      ALOAD(1)
      PUTFIELD(SourceField)
      thisLoad()
      ILOAD(2)
      PUTFIELD(BeginLineField)
      thisLoad()
      ILOAD(3)
      PUTFIELD(BeginColField)
      thisLoad()
      ILOAD(4)
      PUTFIELD(EndLineField)
      thisLoad()
      ILOAD(5)
      PUTFIELD(EndColField)
      RETURN()
    }

    private def SourceField: InstanceField =
      InstanceField(this.jvmName, "source", BackendType.String)

    private def BeginLineField: InstanceField =
      InstanceField(this.jvmName, "beginLine", BackendType.Int32)

    private def BeginColField: InstanceField =
      InstanceField(this.jvmName, "beginCol", BackendType.Int32)

    private def EndLineField: InstanceField =
      InstanceField(this.jvmName, "endLine", BackendType.Int32)

    private def EndColField: InstanceField =
      InstanceField(this.jvmName, "endCol", BackendType.Int32)

    private def ToStringMethod: InstanceMethod = ClassConstants.Object.ToStringMethod.implementation(this.jvmName)

    private def toStringIns(implicit mv: MethodVisitor): Unit = {
      // create string builder
      NEW(JvmName.StringBuilder)
      DUP()
      INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
      // build string
      thisLoad()
      GETFIELD(SourceField)
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      pushString(":")
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      thisLoad()
      GETFIELD(BeginLineField)
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendInt32Method)
      pushString(":")
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      thisLoad()
      GETFIELD(BeginColField)
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendInt32Method)
      // create the string
      INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
      ARETURN()
    }
  }

  case object Global extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkStaticConstructor(StaticConstructorMethod(this.jvmName), staticConstructorIns(_))

      cm.mkField(CounterField, IsPrivate, IsFinal, NotVolatile)
      cm.mkStaticMethod(NewIdMethod, IsPublic, IsFinal, newIdIns(_))

      cm.mkField(ArgsField, IsPrivate, NotFinal, NotVolatile)
      cm.mkStaticMethod(GetArgsMethod, IsPublic, IsFinal, getArgsIns(_))
      cm.mkStaticMethod(SetArgsMethod, IsPublic, IsFinal, setArgsIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    private def staticConstructorIns(implicit mv: MethodVisitor): Unit = {
      NEW(JvmName.AtomicLong)
      DUP()
      invokeConstructor(JvmName.AtomicLong, MethodDescriptor.NothingToVoid)
      PUTSTATIC(CounterField)
      ICONST_0()
      ANEWARRAY(JvmName.String)
      PUTSTATIC(ArgsField)
      RETURN()
    }

    private def NewIdMethod: StaticMethod = StaticMethod(this.jvmName, "newId", mkDescriptor()(BackendType.Int64))

    private def newIdIns(implicit mv: MethodVisitor): Unit = {
      GETSTATIC(CounterField)
      INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement",
        MethodDescriptor(Nil, BackendType.Int64))
      LRETURN()
    }

    private def GetArgsMethod: StaticMethod = StaticMethod(this.jvmName, "getArgs", mkDescriptor()(BackendType.Array(BackendType.String)))

    private def getArgsIns(implicit mv: MethodVisitor): Unit = {
      GETSTATIC(ArgsField)
      ARRAYLENGTH()
      ANEWARRAY(JvmName.String)
      ASTORE(0)
      // the new array is now created, now to copy the args
      GETSTATIC(ArgsField)
      ICONST_0()
      ALOAD(0)
      ICONST_0()
      GETSTATIC(ArgsField)
      ARRAYLENGTH()
      arrayCopy()
      ALOAD(0)
      ARETURN()
    }

    def SetArgsMethod: StaticMethod =
      StaticMethod(this.jvmName, "setArgs", mkDescriptor(BackendType.Array(BackendType.String))(VoidableType.Void))

    private def setArgsIns(implicit mv: MethodVisitor): Unit = {
      ALOAD(0)
      ARRAYLENGTH()
      ANEWARRAY(JvmName.String)
      ASTORE(1)
      ALOAD(0)
      ICONST_0()
      ALOAD(1)
      ICONST_0()
      ALOAD(0)
      ARRAYLENGTH()
      arrayCopy()
      ALOAD(1)
      PUTSTATIC(ArgsField)
      RETURN()
    }

    private def CounterField: StaticField = StaticField(this.jvmName, "counter", JvmName.AtomicLong.toTpe)

    private def ArgsField: StaticField = StaticField(this.jvmName, "args", BackendType.Array(BackendType.String))

    private def arrayCopy()(implicit mv: MethodVisitor): Unit = {
      mv.visitMethodInstruction(Opcodes.INVOKESTATIC, JvmName.System, "arraycopy",
        MethodDescriptor(List(BackendType.Object, BackendType.Int32, BackendType.Object, BackendType.Int32,
          BackendType.Int32), VoidableType.Void), isInterface = false)
    }
  }

  case object HoleError extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, JvmName.FlixError)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      // These fields allow external equality checking.
      cm.mkField(HoleField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(LocationField, IsPublic, IsFinal, NotVolatile)

      cm.closeClassMaker()
    }

    private def HoleField: InstanceField = InstanceField(this.jvmName, "hole", BackendType.String)

    private def LocationField: InstanceField = InstanceField(this.jvmName, "location", ReifiedSourceLocation.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, List(BackendType.String, ReifiedSourceLocation.toTpe))

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      withName(1, BackendType.String) { hole =>
        withName(2, ReifiedSourceLocation.toTpe) { loc =>
          thisLoad()
          // create an error msg
          NEW(JvmName.StringBuilder)
          DUP()
          INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
          pushString("Hole '")
          INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
          hole.load()
          INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
          pushString("' at ")
          INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
          loc.load()
          INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
          INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
          INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
          INVOKESPECIAL(ClassConstants.FlixError.Constructor)
          // save the arguments locally
          thisLoad()
          hole.load()
          PUTFIELD(HoleField)
          thisLoad()
          loc.load()
          PUTFIELD(LocationField)
          RETURN()
        }
      }
    }
  }

  case object MatchError extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(MatchError.jvmName, IsFinal, superClass = JvmName.FlixError)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      // This field allows external equality checking.
      cm.mkField(LocationField, IsPublic, IsFinal, NotVolatile)

      cm.closeClassMaker()
    }

    private def LocationField: InstanceField = InstanceField(this.jvmName, "location", ReifiedSourceLocation.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(MatchError.jvmName, List(ReifiedSourceLocation.toTpe))

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      NEW(JvmName.StringBuilder)
      DUP()
      INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
      pushString("Non-exhaustive match at ")
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      ALOAD(1)
      INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
      INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
      INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
      INVOKESPECIAL(ClassConstants.FlixError.Constructor)
      // save argument locally
      thisLoad()
      ALOAD(1)
      PUTFIELD(this.LocationField)
      RETURN()
    }
  }

  case object CastError extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, superClass = JvmName.FlixError)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, List(ReifiedSourceLocation.toTpe, BackendType.String))

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      withName(1, ReifiedSourceLocation.toTpe)(loc => withName(2, BackendType.String)(msg => {
        thisLoad()
        NEW(JvmName.StringBuilder)
        DUP()
        INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
        msg.load()
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        pushString(" at ")
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        loc.load()
        INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
        INVOKESPECIAL(ClassConstants.FlixError.Constructor)
        RETURN()
      }))
    }
  }

  case object UnhandledEffectError extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, superClass = JvmName.FlixError)

      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      // This field allows external equality checking.
      cm.mkField(EffectNameField, IsPublic, IsFinal, NotVolatile)
      cm.mkField(LocationField, IsPublic, IsFinal, NotVolatile)

      cm.closeClassMaker()
    }

    private def EffectNameField: InstanceField = InstanceField(this.jvmName, "effectName", BackendType.String)

    private def LocationField: InstanceField = InstanceField(this.jvmName, "location", ReifiedSourceLocation.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, List(Suspension.toTpe, BackendType.String, ReifiedSourceLocation.toTpe))

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Suspension.toTpe)(suspension => withName(2, BackendType.String)(info => withName(3, ReifiedSourceLocation.toTpe)(loc => {
        def appendString(): Unit = INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)

        thisLoad()
        NEW(JvmName.StringBuilder)
        DUP()
        INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
        pushString("Unhandled effect '")
        appendString()
        suspension.load()
        GETFIELD(Suspension.EffSymField)
        appendString()
        pushString("' (")
        appendString()
        info.load()
        appendString()
        pushString(") at ")
        appendString()
        loc.load()
        INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
        appendString()
        INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
        INVOKESPECIAL(ClassConstants.FlixError.Constructor)
        // save arguments locally
        thisLoad()
        suspension.load()
        GETFIELD(Suspension.EffSymField)
        PUTFIELD(EffectNameField)
        thisLoad()
        loc.load()
        PUTFIELD(LocationField)
        RETURN()
      })))
    }
  }


  case object Region extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

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
    private def ThreadsField: InstanceField = InstanceField(this.jvmName, "threads", JvmName.ConcurrentLinkedQueue.toTpe)

    // private final LinkedList<Runnable> onExit = new LinkedList<Runnable>();
    private def OnExitField: InstanceField = InstanceField(this.jvmName, "onExit", JvmName.LinkedList.toTpe)

    // private final Thread regionThread = Thread.currentThread();
    private def RegionThreadField: InstanceField = InstanceField(this.jvmName, "regionThread", JvmName.Thread.toTpe)

    // private volatile Throwable childException = null;
    private def ChildExceptionField: InstanceField = InstanceField(this.jvmName, "childException", JvmName.Throwable.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      INVOKESPECIAL(ClassConstants.Object.Constructor)
      thisLoad()
      NEW(JvmName.ConcurrentLinkedQueue)
      DUP()
      invokeConstructor(JvmName.ConcurrentLinkedQueue, MethodDescriptor.NothingToVoid)
      PUTFIELD(ThreadsField)
      thisLoad()
      INVOKESTATIC(ClassConstants.Thread.CurrentThreadMethod)
      PUTFIELD(RegionThreadField)
      thisLoad()
      ACONST_NULL()
      PUTFIELD(ChildExceptionField)
      thisLoad()
      NEW(JvmName.LinkedList)
      DUP()
      invokeConstructor(JvmName.LinkedList, MethodDescriptor.NothingToVoid)
      PUTFIELD(OnExitField)
      RETURN()
    }

    // final public void spawn(Runnable r) {
    //   Thread t = new Thread(r);
    //   t.setUncaughtExceptionHandler(new UncaughtExceptionHandler(this));
    //   t.start();
    //   threads.add(t);
    // }
    def SpawnMethod: InstanceMethod = InstanceMethod(this.jvmName, "spawn", mkDescriptor(JvmName.Runnable.toTpe)(VoidableType.Void))

    private def spawnIns(implicit mv: MethodVisitor): Unit = {
      INVOKESTATIC(ClassConstants.Thread.OfVirtualMethod)
      ALOAD(1)
      INVOKEINTERFACE(ClassConstants.ThreadBuilderOfVirtual.UnstartedMethod)
      storeWithName(2, JvmName.Thread.toTpe) { thread =>
        thread.load()
        NEW(BackendObjType.UncaughtExceptionHandler.jvmName)
        DUP()
        thisLoad()
        invokeConstructor(BackendObjType.UncaughtExceptionHandler.jvmName, mkDescriptor(BackendObjType.Region.toTpe)(VoidableType.Void))
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
    def ExitMethod: InstanceMethod = InstanceMethod(this.jvmName, "exit", MethodDescriptor.NothingToVoid)

    private def exitIns(implicit mv: MethodVisitor): Unit = {
      withName(1, JvmName.Thread.toTpe) { t =>
        whileLoop(Condition.NONNULL) {
          thisLoad()
          GETFIELD(ThreadsField)
          INVOKEVIRTUAL(ClassConstants.ConcurrentLinkedQueue.PollMethod)
          CHECKCAST(JvmName.Thread)
          DUP()
          t.store()
        } {
          t.load()
          INVOKEVIRTUAL(ClassConstants.Thread.JoinMethod)
        }
        withName(2, JvmName.Iterator.toTpe) { i =>
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
            CHECKCAST(JvmName.Runnable)
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
    def ReportChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, "reportChildException", mkDescriptor(JvmName.Throwable.toTpe)(VoidableType.Void))

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
    def ReThrowChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, "reThrowChildException", MethodDescriptor.NothingToVoid)

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
    private def RunOnExitMethod: InstanceMethod = InstanceMethod(this.jvmName, "runOnExit", mkDescriptor(JvmName.Runnable.toTpe)(VoidableType.Void))

    private def runOnExitIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      GETFIELD(OnExitField)
      ALOAD(1)
      INVOKEVIRTUAL(ClassConstants.LinkedList.AddFirstMethod)
      RETURN()
    }
  }

  case object UncaughtExceptionHandler extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(JvmName.Thread$UncaughtExceptionHandler))

      cm.mkField(RegionField, IsPrivate, IsFinal, NotVolatile)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkMethod(Nil, UncaughtExceptionMethod, IsPublic, IsFinal, uncaughtExceptionsIns(_))

      cm.closeClassMaker()
    }

    // private final Region r;
    private def RegionField: InstanceField = InstanceField(this.jvmName, "r", BackendObjType.Region.toTpe)

    // UncaughtExceptionHandler(Region r) { this.r = r; }
    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, BackendObjType.Region.toTpe :: Nil)

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      INVOKESPECIAL(ClassConstants.Object.Constructor)
      thisLoad()
      ALOAD(1)
      PUTFIELD(RegionField)
      RETURN()
    }

    // public void uncaughtException(Thread t, Throwable e) { r.reportChildException(e); }
    private def UncaughtExceptionMethod: InstanceMethod =
      InstanceMethod(this.jvmName, "uncaughtException", ClassConstants.ThreadUncaughtExceptionHandler.UncaughtExceptionMethod.d)

    private def uncaughtExceptionsIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      GETFIELD(RegionField)
      ALOAD(2)
      INVOKEVIRTUAL(Region.ReportChildExceptionMethod)
      RETURN()
    }
  }

  case object Main extends BackendObjType {

    def genByteCode(sym: Symbol.DefnSym)(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkStaticMethod(MainMethod, IsPublic, NotFinal, mainIns(sym)(_))

      cm.closeClassMaker()
    }

    def MainMethod: StaticMethod = StaticMethod(this.jvmName, "main", mkDescriptor(BackendType.Array(BackendType.String))(VoidableType.Void))

    private def mainIns(sym: Symbol.DefnSym)(implicit mv: MethodVisitor): Unit = {
      val defName = BackendObjType.Defn(sym).jvmName
      withName(0, BackendType.Array(BackendType.String))(args => {
        args.load()
        INVOKESTATIC(Global.SetArgsMethod)
        NEW(defName)
        DUP()
        INVOKESPECIAL(defName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid)
        DUP()
        GETSTATIC(Unit.SingletonField)
        PUTFIELD(InstanceField(defName, "arg0", BackendType.Object))
        Result.unwindSuspensionFreeThunk(s"in ${this.jvmName.toBinaryName}", SourceLocation.Unknown)
        POP()
        RETURN()
      })
    }
  }

  case class Namespace(ns: List[String]) extends BackendObjType {

    def genByteCode(defs: List[JvmAst.Def])(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

      for (defn <- defs) {
        cm.mkStaticMethod(ShimMethod(defn), IsPublic, IsFinal, shimIns(defn)(_))
      }

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def ShimMethod(defn: JvmAst.Def): StaticMethod = {
      val erasedArgs = defn.fparams.map(_.tpe).map(BackendType.toErasedBackendType)
      val erasedResult = BackendType.toErasedBackendType(defn.unboxedType.tpe)
      // Exported names are checked in Safety, so no mangling is needed.
      val name = if (defn.ann.isExport) defn.sym.name else "m_" + JvmName.mangle(defn.sym.name)
      StaticMethod(this.jvmName, name, MethodDescriptor(erasedArgs, erasedResult))
    }

    private def shimIns(defn: JvmAst.Def)(implicit mv: MethodVisitor): Unit = {
      val defnT = Defn(defn.sym)
      val paramTypes = defn.fparams.map(fp => BackendType.toErasedBackendType(fp.tpe))
      withNames(0, paramTypes) {
        case (_, args) =>
          val erasedResult = BackendType.toErasedBackendType(defn.unboxedType.tpe)
          NEW(defnT.jvmName)
          DUP()
          INVOKESPECIAL(ConstructorMethod(defnT.jvmName, Nil))
          for ((arg, index) <- args.zipWithIndex) {
            DUP()
            arg.load()
            PUTFIELD(InstanceField(defnT.jvmName, s"arg$index", arg.tpe))
          }
          Result.unwindSuspensionFreeThunkToType(erasedResult, s"in shim method of ${defn.sym}", defn.loc)
          xReturn(erasedResult)
      }
    }
  }

  //
  // Java Types
  //

  case object Result extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.closeClassMaker()
    }

    /**
      * Expects a Result on the stack and leaves a non-Thunk Result.
      * [..., Result] --> [..., Suspension|Value]
      */
    def unwindThunk()(implicit mv: MethodVisitor): Unit = {
      whileLoop(Condition.NE) {
        DUP()
        INSTANCEOF(Thunk.jvmName)
      } {
        CHECKCAST(Thunk.jvmName)
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
      INSTANCEOF(Suspension.jvmName)
      ifCondition(Condition.NE) {
        DUP()
        CHECKCAST(Suspension.jvmName) // [..., s]
        // Add our new frame
        NEW(Suspension.jvmName)
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
        xReturn(Suspension.toTpe)
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
      CHECKCAST(Value.jvmName) // Cannot fail
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
      CHECKCAST(Value.jvmName) // Cannot fail
      GETFIELD(Value.fieldFromType(tpe))
      castIfNotPrim(tpe)
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
      CHECKCAST(Value.jvmName)
    }

    /**
      * [..., Result] -> [..., Value|Thunk]
      * side effect: if the result is a suspension, a [[UnhandledEffectError]] is thrown.
      */
    def crashIfSuspension(errorHint: String, loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
      DUP()
      INSTANCEOF(Suspension.jvmName)
      ifCondition(Condition.NE) {
        CHECKCAST(Suspension.jvmName)
        NEW(UnhandledEffectError.jvmName)
        // [.., suspension, UEE] -> [.., suspension, UEE, UEE, suspension]
        DUP2()
        SWAP()
        pushString(errorHint)
        pushLoc(loc)
        // [.., suspension, UEE, UEE, suspension, info, rsl] -> [.., suspension, UEE]
        INVOKESPECIAL(UnhandledEffectError.Constructor)
        ATHROW()
      }
    }
  }

  case object Value extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Result.jvmName))

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

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    private def BoolField: InstanceField = InstanceField(this.jvmName, "b", BackendType.Bool)

    private def CharField: InstanceField = InstanceField(this.jvmName, "c", BackendType.Char)

    private def Int8Field: InstanceField = InstanceField(this.jvmName, "i8", BackendType.Int8)

    private def Int16Field: InstanceField = InstanceField(this.jvmName, "i16", BackendType.Int16)

    private def Int32Field: InstanceField = InstanceField(this.jvmName, "i32", BackendType.Int32)

    private def Int64Field: InstanceField = InstanceField(this.jvmName, "i64", BackendType.Int64)

    private def Float32Field: InstanceField = InstanceField(this.jvmName, "f32", BackendType.Float32)

    private def Float64Field: InstanceField = InstanceField(this.jvmName, "f64", BackendType.Float64)

    private def ObjectField: InstanceField = InstanceField(this.jvmName, "o", BackendType.Object)

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
      val cm = mkInterface(this.jvmName)

      cm.mkInterfaceMethod(ApplyMethod)
      cm.mkStaticInterfaceMethod(StaticApplyMethod, IsPublic, NotFinal, staticApplyIns(_))

      cm.closeClassMaker()
    }

    def ApplyMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "applyFrame", mkDescriptor(Value.toTpe)(Result.toTpe))

    def StaticApplyMethod: StaticInterfaceMethod = StaticInterfaceMethod(
      this.jvmName,
      "applyFrameStatic",
      mkDescriptor(Frame.toTpe, Value.toTpe)(Result.toTpe)
    )

    private def staticApplyIns(implicit mv: MethodVisitor): Unit = {
      withName(0, Frame.toTpe) { fun =>
        withName(1, Value.toTpe) { resumeArg =>
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
      val cm = mkInterface(this.jvmName, interfaces = List(Result.jvmName, JvmName.Runnable))

      cm.mkInterfaceMethod(InvokeMethod)
      cm.mkDefaultMethod(RunMethod, IsPublic, NotFinal, runIns(_))

      cm.closeClassMaker()
    }

    def InvokeMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "invoke", mkDescriptor()(Result.toTpe))

    private def RunMethod: DefaultMethod = DefaultMethod(this.jvmName, "run", mkDescriptor()(VoidableType.Void))

    private def runIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      Result.unwindSuspensionFreeThunk(s"in ${JvmName.Runnable.toBinaryName}", SourceLocation.Unknown)
      POP()
      RETURN()
    }
  }

  case object Suspension extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Result.jvmName))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkField(EffSymField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(EffOpField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(PrefixField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(ResumptionField, IsPublic, NotFinal, NotVolatile)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def EffSymField: InstanceField = InstanceField(this.jvmName, "effSym", BackendType.String)

    def EffOpField: InstanceField = InstanceField(this.jvmName, "effOp", EffectCall.toTpe)

    def PrefixField: InstanceField = InstanceField(this.jvmName, "prefix", Frames.toTpe)

    def ResumptionField: InstanceField = InstanceField(this.jvmName, "resumption", Resumption.toTpe)

  }

  case object Frames extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)

      cm.mkInterfaceMethod(PushMethod)
      cm.mkInterfaceMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def PushMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "push", mkDescriptor(Frame.toTpe)(Frames.toTpe))

    def ReverseOntoMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "reverseOnto", mkDescriptor(Frames.toTpe)(Frames.toTpe))

    def pushImplementation(implicit mv: MethodVisitor): Unit = {
      withName(1, Frame.toTpe) { frame =>
        NEW(FramesCons.jvmName)
        DUP()
        INVOKESPECIAL(FramesCons.Constructor)
        DUP()
        frame.load()
        PUTFIELD(FramesCons.HeadField)
        DUP()
        thisLoad()
        PUTFIELD(FramesCons.TailField)
        xReturn(FramesCons.toTpe)
      }
    }
  }

  case object FramesCons extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Frames.jvmName))

      cm.mkField(HeadField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(TailField, IsPublic, NotFinal, NotVolatile)
      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkMethod(Nil, PushMethod, IsPublic, IsFinal, Frames.pushImplementation(_))
      cm.mkMethod(Nil, Frames.ReverseOntoMethod.implementation(this.jvmName), IsPublic, IsFinal, reverseOntoIns(_))

      cm.closeClassMaker()
    }

    def HeadField: InstanceField = InstanceField(this.jvmName, "head", Frame.toTpe)

    def TailField: InstanceField = InstanceField(this.jvmName, "tail", Frames.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.jvmName)

    private def reverseOntoIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Frames.toTpe) { rest =>
        thisLoad()
        GETFIELD(TailField)
        NEW(FramesCons.jvmName)
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
        xReturn(Frames.toTpe)
      }
    }
  }

  case object FramesNil extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Frames.jvmName))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkMethod(Nil, PushMethod, IsPublic, IsFinal, Frames.pushImplementation(_))
      cm.mkMethod(Nil, Frames.ReverseOntoMethod.implementation(this.jvmName), IsPublic, IsFinal, reverseOntoIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.jvmName)

    private def reverseOntoIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Frames.toTpe) { rest =>
        rest.load()
        xReturn(rest.tpe)
      }
    }
  }

  case object Resumption extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkInterfaceMethod(RewindMethod)
      cm.mkStaticInterfaceMethod(StaticRewindMethod, IsPublic, NotFinal, staticRewindIns(_))
      cm.closeClassMaker()
    }

    def RewindMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "rewind", mkDescriptor(Value.toTpe)(Result.toTpe))

    def StaticRewindMethod: StaticInterfaceMethod = StaticInterfaceMethod(this.jvmName, "staticRewind", mkDescriptor(Resumption.toTpe, Value.toTpe)(Result.toTpe))

    private def staticRewindIns(implicit mv: MethodVisitor): Unit = {
      withName(0, Resumption.toTpe) { resumption =>
        withName(1, Value.toTpe) { v =>
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
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Resumption.jvmName))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

      cm.mkField(SymField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(HandlerField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(FramesField, IsPublic, NotFinal, NotVolatile)
      cm.mkField(TailField, IsPublic, NotFinal, NotVolatile)

      cm.mkMethod(Nil, Resumption.RewindMethod.implementation(this.jvmName), IsPublic, IsFinal, rewindIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    def SymField: InstanceField = InstanceField(this.jvmName, "sym", BackendType.String)

    def HandlerField: InstanceField = InstanceField(this.jvmName, "handler", Handler.toTpe)

    def FramesField: InstanceField = InstanceField(this.jvmName, "frames", Frames.toTpe)

    def TailField: InstanceField = InstanceField(this.jvmName, "tail", Resumption.toTpe)

    private def rewindIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Value.toTpe) { v =>
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
        xReturn(Thunk.toTpe)
      }
    }
  }

  case object ResumptionNil extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Resumption.jvmName))

      cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
      cm.mkMethod(Nil, Resumption.RewindMethod.implementation(this.jvmName), IsPublic, IsFinal, rewindIns(_))

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, Nil)

    private def rewindIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Value.toTpe) { v =>
        v.load()
        xReturn(v.tpe)
      }
    }
  }

  case object Handler extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkStaticInterfaceMethod(InstallHandlerMethod, IsPublic, NotFinal, installHandlerIns(_))
      cm.closeClassMaker()
    }

    def InstallHandlerMethod: StaticInterfaceMethod = StaticInterfaceMethod(
      this.jvmName,
      "installHandler",
      mkDescriptor(BackendType.String, Handler.toTpe, Frames.toTpe, Thunk.toTpe)(Result.toTpe)
    )

    private def installHandlerIns(implicit mv: MethodVisitor): Unit = {
      withName(0, BackendType.String) { effSym =>
        withName(1, Handler.toTpe) { handler =>
          withName(2, Frames.toTpe) { frames =>
            withName(3, Thunk.toTpe) { thunk =>
              thunk.load()
              // Thunk|Value|Suspension
              Result.unwindThunk()
              // Value|Suspension
              // handle suspension
              DUP()
              INSTANCEOF(Suspension.jvmName)
              ifCondition(Condition.NE) {
                DUP()
                CHECKCAST(Suspension.jvmName)
                storeWithName(4, Suspension.toTpe) { s =>
                  NEW(ResumptionCons.jvmName)
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
                  storeWithName(5, ResumptionCons.toTpe) { r =>
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
                      xReturn(Result.toTpe)
                    }
                    NEW(Suspension.jvmName)
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
                    NEW(FramesNil.jvmName)
                    DUP()
                    INVOKESPECIAL(FramesNil.Constructor)
                    PUTFIELD(Suspension.PrefixField)
                    DUP()
                    r.load()
                    PUTFIELD(Suspension.ResumptionField)
                    xReturn(Suspension.toTpe)
                  }
                }
              }

              // Value
              CHECKCAST(Value.jvmName)
              storeWithName(6, Value.toTpe) { res =>
                //
                // Case on frames
                // FramesNil
                frames.load()
                INSTANCEOF(FramesNil.jvmName)
                ifCondition(Condition.NE) {
                  res.load()
                  xReturn(Value.toTpe)
                }
                // FramesCons
                frames.load()
                CHECKCAST(FramesCons.jvmName)
                storeWithName(7, FramesCons.toTpe) { cons => {
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
                  xReturn(Result.toTpe)
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
      val cm = mkInterface(this.jvmName)
      cm.mkInterfaceMethod(ApplyMethod)
      cm.closeClassMaker()
    }

    def ApplyMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "apply", mkDescriptor(Handler.toTpe, Resumption.toTpe)(Result.toTpe))

  }

  case class ResumptionWrapper(tpe: BackendType) extends BackendObjType {

    // tpe -> Result
    private val superClass: AbstractArrow = AbstractArrow(List(tpe.toErased), BackendType.Object)

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, superClass.jvmName)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.mkField(ResumptionField, IsPrivate, IsFinal, NotVolatile)
      cm.mkMethod(Nil, InvokeMethod, IsPublic, NotFinal, invokeIns(_))
      cm.mkMethod(Nil, UniqueMethod, IsPublic, NotFinal, uniqueIns(_))
      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, List(Resumption.toTpe))

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      withName(1, Resumption.toTpe) { resumption =>
        thisLoad()
        INVOKESPECIAL(superClass.jvmName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid)
        thisLoad()
        resumption.load()
        PUTFIELD(ResumptionField)
        RETURN()
      }
    }

    def ResumptionField: InstanceField = InstanceField(this.jvmName, "resumption", Resumption.toTpe)

    def InvokeMethod: InstanceMethod = Thunk.InvokeMethod.implementation(this.jvmName)

    private def invokeIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      GETFIELD(ResumptionField)
      NEW(Value.jvmName)
      DUP()
      INVOKESPECIAL(Value.Constructor)
      DUP()
      thisLoad()
      mv.visitFieldInsn(Opcodes.GETFIELD, this.jvmName.toInternalName, "arg0", tpe.toErased.toDescriptor)
      PUTFIELD(Value.fieldFromType(tpe.toErased))
      INVOKEINTERFACE(Resumption.RewindMethod)
      xReturn(Result.toTpe)
    }

    private def UniqueMethod: InstanceMethod = InstanceMethod(this.jvmName, "getUniqueThreadClosure", mkDescriptor()(this.superClass.toTpe))

    private def uniqueIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      ARETURN()
    }
  }
}
