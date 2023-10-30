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
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.mkName
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch._
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.{IsVolatile, NotVolatile}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{DevFlixRuntime, JavaLang, JavaLangInvoke, JavaUtil, JavaUtilConcurrent, MethodDescriptor, RootPackage}
import org.objectweb.asm.Opcodes

/**
  * Represents all Flix types that are objects on the JVM (array is an exception).
  */
sealed trait BackendObjType {
  /**
    * The `JvmName` that represents the type `Ref(Int)` refers to `"Ref$Int"`.
    */
  val jvmName: JvmName = this match {
    case BackendObjType.Unit => JvmName(DevFlixRuntime, "Unit")
    case BackendObjType.BigDecimal => JvmName(List("java", "math"), "BigDecimal")
    case BackendObjType.BigInt => JvmName(List("java", "math"), "BigInteger")
    case BackendObjType.Lazy(tpe) => JvmName(RootPackage, mkName("Lazy", tpe))
    case BackendObjType.Ref(tpe) => JvmName(RootPackage, mkName("Ref", tpe))
    case BackendObjType.Tuple(elms) => JvmName(RootPackage, mkName("Tuple", elms))
    case BackendObjType.Arrow(args, result) => JvmName(RootPackage, mkName(s"Fn${args.length}", args :+ result))
    case BackendObjType.RecordEmpty => JvmName(RootPackage, mkName(s"RecordEmpty"))
    case BackendObjType.RecordExtend(_, value, _) => JvmName(RootPackage, mkName("RecordExtend", value))
    case BackendObjType.Record => JvmName(RootPackage, s"IRecord${Flix.Delimiter}")
    case BackendObjType.Native(className) => className
    case BackendObjType.ReifiedSourceLocation => JvmName(DevFlixRuntime, "ReifiedSourceLocation")
    case BackendObjType.Global => JvmName(DevFlixRuntime, "Global")
    case BackendObjType.Regex => JvmName(List("java", "util", "regex"), "Pattern")
    case BackendObjType.FlixError => JvmName(DevFlixRuntime, "FlixError")
    case BackendObjType.HoleError => JvmName(DevFlixRuntime, "HoleError")
    case BackendObjType.MatchError => JvmName(DevFlixRuntime, "MatchError")
    case BackendObjType.Region => JvmName(DevFlixRuntime, "Region")
    case BackendObjType.UncaughtExceptionHandler => JvmName(DevFlixRuntime, "UncaughtExceptionHandler")
    // Java classes
    case BackendObjType.JavaObject => JvmName(JavaLang, "Object")
    case BackendObjType.String => JvmName(JavaLang, "String")
    case BackendObjType.Arrays => JvmName(JavaUtil, "Arrays")
    case BackendObjType.StringBuilder => JvmName(JavaLang, "StringBuilder")
    case BackendObjType.Objects => JvmName(JavaLang, "Objects")
    case BackendObjType.LambdaMetaFactory => JvmName(JavaLangInvoke, "LambdaMetaFactory")
    case BackendObjType.LinkedList => JvmName(JavaUtil, "LinkedList")
    case BackendObjType.Iterator => JvmName(JavaUtil, "Iterator")
    case BackendObjType.Runnable => JvmName(JavaLang, "Runnable")
    case BackendObjType.ConcurrentLinkedQueue => JvmName(JavaUtilConcurrent, "ConcurrentLinkedQueue")
    case BackendObjType.Thread => JvmName(JavaLang, "Thread")
    case BackendObjType.ThreadBuilderOfVirtual => JvmName(JavaLang, "Thread$Builder$OfVirtual")
    case BackendObjType.ThreadUncaughtExceptionHandler => JvmName(JavaLang, "Thread$UncaughtExceptionHandler")
    case BackendObjType.Result => JvmName(DevFlixRuntime, "Result")
    // Effects Runtime
    case BackendObjType.Value => JvmName(DevFlixRuntime, "Value")
    case BackendObjType.Frame => JvmName(DevFlixRuntime, "Frame")
    case BackendObjType.Thunk => JvmName(DevFlixRuntime, "Thunk")
    case BackendObjType.Suspension => JvmName(DevFlixRuntime, "Suspension")
    case BackendObjType.Frames => JvmName(DevFlixRuntime, "Frames")
    case BackendObjType.FramesCons => JvmName(DevFlixRuntime, "FramesCons")
    case BackendObjType.FramesNil => JvmName(DevFlixRuntime, "FramesNil")
    case BackendObjType.Resumption => JvmName(DevFlixRuntime, "Resumption")
    case BackendObjType.ResumptionCons => JvmName(DevFlixRuntime, "ResumptionCons")
    case BackendObjType.ResumptionNil => JvmName(DevFlixRuntime, "ResumptionNil")
    case BackendObjType.Handler => JvmName(DevFlixRuntime, "Handler")
    case BackendObjType.EffectCall => JvmName(DevFlixRuntime, "EffectCall")
  }

  /**
    * The JVM type descriptor of the form `"L<jvmName.toInternalName>;"`.
    */
  def toDescriptor: String = jvmName.toDescriptor

  /**
    * Returns `this` wrapped in `BackendType.Reference`.
    */
  def toTpe: BackendType.Reference = BackendType.Reference(this)

  protected def nullarySuperConstructor(superClass: ConstructorMethod): ConstructorMethod = ConstructorMethod(
    this.jvmName,
    IsPublic,
    Nil,
    Some(_ => thisLoad() ~ INVOKESPECIAL(superClass) ~ RETURN())
  )

  protected def singletonStaticConstructor(thisConstructor: ConstructorMethod, singleton: StaticField): StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(_ =>
    NEW(this.jvmName) ~
      DUP() ~ INVOKESPECIAL(thisConstructor) ~
      PUTSTATIC(singleton) ~
      RETURN()
  ))
}

object BackendObjType {
  /**
    * Constructs a concatenated string using `JvmName.Delimiter`. The call
    * `mkName("Tuple2", List(Object, Int, String))` would
    * result in the string `"Tuple2$Obj$Int32$Obj"`.
    */
  private def mkName(prefix: String, args: List[BackendType]): String = {
    // TODO: Should delimiter always be included?
    if (args.isEmpty) prefix
    else s"$prefix${Flix.Delimiter}${args.map(e => e.toErased.toErasedString).mkString(Flix.Delimiter)}"
  }

  private def mkName(prefix: String, arg: BackendType): String =
    mkName(prefix, List(arg))

  private def mkName(prefix: String): String =
    mkName(prefix, Nil)

  case object Unit extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkStaticConstructor(StaticConstructor)
      cm.mkConstructor(Constructor)
      cm.mkField(SingletonField)
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def StaticConstructor: StaticConstructorMethod = singletonStaticConstructor(Constructor, SingletonField)

    def SingletonField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, NotVolatile, "INSTANCE", this.toTpe)

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      pushString("()") ~ ARETURN()
    ))
  }

  case object BigDecimal extends BackendObjType

  case object BigInt extends BackendObjType

  case class Lazy(tpe: BackendType) extends BackendObjType

  case class Ref(tpe: BackendType) extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkField(ValueField)
      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", tpe)
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType

  //case class Enum(sym: Symbol.EnumSym, args: List[BackendType]) extends BackendObjType

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType with Generatable {


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
        case ObjFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(JavaObject.toTpe)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ ARETURN()
          ))
        case ObjConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(JavaObject.toTpe)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ RETURN()
          ))
        case ObjPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(JavaObject.toTpe)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool) ~ IRETURN()
          ))
        case IntFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Int32)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ ARETURN()
          ))
        case IntConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Int32)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ RETURN()
          ))
        case IntPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Int32)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool) ~ IRETURN()
          ))
        case IntUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsInt",
          mkDescriptor(BackendType.Int32)(BackendType.Int32),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Int32) ~ IRETURN()
          ))
        case LongFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Int64)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ ARETURN()
          ))
        case LongConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Int64)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ RETURN()
          ))
        case LongPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Int64)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool) ~ IRETURN()
          ))
        case LongUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsLong",
          mkDescriptor(BackendType.Int64)(BackendType.Int64),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Int64) ~ LRETURN()
          ))
        case DoubleFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Float64)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ ARETURN()
          ))
        case DoubleConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Float64)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe) ~ RETURN()
          ))
        case DoublePredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Float64)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool) ~ IRETURN()
          ))
        case DoubleUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsDouble",
          mkDescriptor(BackendType.Float64)(BackendType.Float64),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Float64) ~ DRETURN()
          ))

      }
    }

    // JavaObject -> JavaObject
    case object ObjFunction extends FunctionInterface

    // JavaObject -> Unit
    case object ObjConsumer extends FunctionInterface

    // JavaObject -> Bool
    case object ObjPredicate extends FunctionInterface

    // Int32 -> JavaObject
    case object IntFunction extends FunctionInterface

    // Int32 -> Unit
    case object IntConsumer extends FunctionInterface

    // Int32 -> Bool
    case object IntPredicate extends FunctionInterface

    // Int32 -> Int32
    case object IntUnaryOperator extends FunctionInterface

    // Int64 -> JavaObject
    case object LongFunction extends FunctionInterface

    // Int64 -> Unit
    case object LongConsumer extends FunctionInterface

    // Int64 -> Bool
    case object LongPredicate extends FunctionInterface

    // Int64 -> Int64
    case object LongUnaryOperator extends FunctionInterface

    // Float64 -> JavaObject
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
    def specialization(): List[FunctionInterface] = {
      (args, result) match {
        case (BackendType.Reference(BackendObjType.JavaObject) :: Nil, BackendType.Reference(BackendObjType.JavaObject)) =>
          ObjFunction :: ObjConsumer :: Nil
        case (BackendType.Reference(BackendObjType.JavaObject) :: Nil, BackendType.Bool) =>
          ObjPredicate :: Nil
        case (BackendType.Int32 :: Nil, BackendType.Reference(BackendObjType.JavaObject)) =>
          IntFunction :: IntConsumer :: Nil
        case (BackendType.Int32 :: Nil, BackendType.Bool) =>
          IntPredicate :: Nil
        case (BackendType.Int32 :: Nil, BackendType.Int32) =>
          IntUnaryOperator :: Nil
        case (BackendType.Int64 :: Nil, BackendType.Reference(BackendObjType.JavaObject)) =>
          LongFunction :: LongConsumer :: Nil
        case (BackendType.Int64 :: Nil, BackendType.Bool) =>
          LongPredicate :: Nil
        case (BackendType.Int64 :: Nil, BackendType.Int64) =>
          LongUnaryOperator :: Nil
        case (BackendType.Float64 :: Nil, BackendType.Reference(BackendObjType.JavaObject)) =>
          DoubleFunction :: DoubleConsumer :: Nil
        case (BackendType.Float64 :: Nil, BackendType.Bool) =>
          DoublePredicate :: Nil
        case (BackendType.Float64 :: Nil, BackendType.Float64) =>
          DoubleUnaryOperator :: Nil
        case _ => Nil
      }
    }

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val specializedInterface = specialization()
      val interfaces = specializedInterface.map(_.jvmName)

      val cm = ClassMaker.mkAbstractClass(this.jvmName, superClass = Thunk.jvmName, interfaces)

      cm.mkConstructor(Constructor)
      args.indices.foreach(argIndex => cm.mkField(ArgField(argIndex)))
      specializedInterface.foreach(i => cm.mkMethod(i.functionMethod))
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(Thunk.Constructor)

    def ArgField(index: Int): InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, s"arg$index", args(index))

    def ToStringMethod: InstanceMethod = {
      val argString = args match {
        case Nil => "()"
        case arg :: Nil => arg.toErasedString
        case _ => args.map(_.toErasedString).mkString("(", ", ", ")")
      }
      JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
        pushString(s"$argString -> ${result.toErasedString}") ~
          ARETURN()
      ))
    }
  }

  case object RecordEmpty extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(this.interface.jvmName))

      cm.mkStaticConstructor(StaticConstructor)
      cm.mkConstructor(Constructor)
      cm.mkField(SingletonField)
      cm.mkMethod(LookupFieldMethod)
      cm.mkMethod(RestrictFieldMethod)
      cm.mkMethod(ToStringMethod)
      cm.mkMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def StaticConstructor: StaticConstructorMethod = singletonStaticConstructor(Constructor, SingletonField)

    def interface: Record.type = Record

    def SingletonField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, NotVolatile, "INSTANCE", this.toTpe)

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      throwUnsupportedOperationException(
        s"${Record.LookupFieldMethod.name} method shouldn't be called")
    ))

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      throwUnsupportedOperationException(
        s"${Record.RestrictFieldMethod.name} method shouldn't be called")
    ))

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      pushString("{}") ~ ARETURN()
    ))

    private def ToTailStringMethod: InstanceMethod = Record.ToTailStringMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, StringBuilder.toTpe) { sb =>
        sb.load() ~ pushString("}") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ ARETURN()
      }
    ))
  }

  case class RecordExtend(field: String, value: BackendType, rest: BackendType) extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(Record.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkField(LabelField)
      cm.mkField(ValueField)
      cm.mkField(RestField)
      cm.mkMethod(LookupFieldMethod)
      cm.mkMethod(RestrictFieldMethod)
      cm.mkMethod(ToStringMethod)
      cm.mkMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def LabelField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "label", String.toTpe)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", value)

    def RestField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "rest", Record.toTpe)

    def LookupFieldMethod: InstanceMethod = Record.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad() ~ ARETURN()
        case FalseBranch =>
          thisLoad() ~ GETFIELD(RestField) ~
            ALOAD(1) ~
            INVOKEINTERFACE(Record.LookupFieldMethod) ~
            ARETURN()
      }
    ))

    def RestrictFieldMethod: InstanceMethod = Record.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad() ~ GETFIELD(RestField) ~ ARETURN()
        case FalseBranch =>
          NEW(this.jvmName) ~ DUP() ~ INVOKESPECIAL(this.Constructor) ~
            DUP() ~ thisLoad() ~ GETFIELD(LabelField) ~ PUTFIELD(LabelField) ~
            DUP() ~ thisLoad() ~ GETFIELD(ValueField) ~ PUTFIELD(ValueField) ~
            DUP() ~ // get the new restricted rest to put
            thisLoad() ~ GETFIELD(RestField) ~
            ALOAD(1) ~
            INVOKEINTERFACE(Record.RestrictFieldMethod) ~
            PUTFIELD(RestField) ~ // put the rest field and return
            ARETURN()
      }
    ))

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      // save the `rest` for the last recursive call
      thisLoad() ~ GETFIELD(this.RestField) ~
        // build this segment of the string
        NEW(StringBuilder.jvmName) ~ DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        pushString("{") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(this.LabelField) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        pushString(" = ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(this.ValueField) ~ xToString(this.ValueField.tpe) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        INVOKEINTERFACE(Record.ToTailStringMethod) ~ ARETURN()
    ))

    private def ToTailStringMethod: InstanceMethod = Record.ToTailStringMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, StringBuilder.toTpe) { sb =>
        // save the `rest` for the last recursive call
        thisLoad() ~ GETFIELD(this.RestField) ~
          // build this segment of the string
          sb.load() ~ pushString(", ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          thisLoad() ~ GETFIELD(this.LabelField) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          pushString(" = ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          thisLoad() ~ GETFIELD(this.ValueField) ~ xToString(this.ValueField.tpe) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          // call the tailString of `rest`
          INVOKEINTERFACE(Record.ToTailStringMethod) ~ ARETURN()

      }
    ))

    /**
      * Compares the label of `this`and `ALOAD(1)` and executes the designated branch.
      */
    private def caseOnLabelEquality(cases: Branch => InstructionSet): InstructionSet =
      thisLoad() ~ GETFIELD(LabelField) ~
        ALOAD(1) ~
        INVOKEVIRTUAL(JavaObject.EqualsMethod) ~
        branch(Condition.Bool)(cases)
  }

  case object Record extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkInterface(this.jvmName)

      cm.mkInterfaceMethod(LookupFieldMethod)
      cm.mkInterfaceMethod(RestrictFieldMethod)
      cm.mkInterfaceMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "lookupField",
      mkDescriptor(String.toTpe)(this.toTpe))

    def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "restrictField",
      mkDescriptor(String.toTpe)(this.toTpe))

    def ToTailStringMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "toTailString",
      mkDescriptor(StringBuilder.toTpe)(String.toTpe))
  }

  // case object SchemaEmpty extends BackendObjType

  //  case class SchemaExtend(name: String, tpe: BackendType, rest: BackendType) extends BackendObjType

  //  case class Relation(tpes: List[BackendType]) extends BackendObjType

  //  case class Lattice(tpes: List[BackendType]) extends BackendObjType

  /**
    * Represents a JVM type not represented in BackendObjType.
    * This should not be used for `java.lang.String` for example since `BackendObjType.String`
    * represents this type.
    */
  case class Native(className: JvmName) extends BackendObjType


  case object ReifiedSourceLocation extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor)

      cm.mkField(SourceField)
      cm.mkField(BeginLineField)
      cm.mkField(BeginColField)
      cm.mkField(EndLineField)
      cm.mkField(EndColField)

      cm.mkMethod(EqualsMethod)
      cm.mkMethod(HashCodeMethod)
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic,
      List(String.toTpe, BackendType.Int32, BackendType.Int32, BackendType.Int32, BackendType.Int32), Some(_ =>
        thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~
          thisLoad() ~ ALOAD(1) ~ PUTFIELD(SourceField) ~
          thisLoad() ~ ILOAD(2) ~ PUTFIELD(BeginLineField) ~
          thisLoad() ~ ILOAD(3) ~ PUTFIELD(BeginColField) ~
          thisLoad() ~ ILOAD(4) ~ PUTFIELD(EndLineField) ~
          thisLoad() ~ ILOAD(5) ~ PUTFIELD(EndColField) ~
          RETURN()
      ))

    def SourceField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "source", String.toTpe)

    def BeginLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "beginLine", BackendType.Int32)

    def BeginColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "beginCol", BackendType.Int32)

    def EndLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "endLine", BackendType.Int32)

    def EndColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "endCol", BackendType.Int32)

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      // create string builder
      NEW(StringBuilder.jvmName) ~ DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        // build string
        thisLoad() ~ GETFIELD(SourceField) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        pushString(":") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(BeginLineField) ~ INVOKEVIRTUAL(StringBuilder.AppendInt32Method) ~
        pushString(":") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(BeginColField) ~ INVOKEVIRTUAL(StringBuilder.AppendInt32Method) ~
        // create the string
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ ARETURN()
    ))

    private def EqualsMethod: InstanceMethod = JavaObject.EqualsMethod.implementation(this.jvmName, Some(_ =>
      withName(1, JavaObject.toTpe) { otherObj =>
        // check exact equality
        thisLoad() ~
          otherObj.load() ~
          ifCondition(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
          // check `other == null`
          otherObj.load() ~
          ifCondition(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
          // the class equality
          thisLoad() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          otherObj.load() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          ifCondition(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
          // check individual fields
          otherObj.load() ~
          CHECKCAST(this.jvmName) ~
          storeWithName(2, this.toTpe) { otherLoc =>
            thisLoad() ~ GETFIELD(BeginLineField) ~
              otherLoc.load() ~ GETFIELD(BeginLineField) ~
              ifCondition(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(BeginColField) ~
              otherLoc.load() ~ GETFIELD(BeginColField) ~
              ifCondition(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(EndLineField) ~
              otherLoc.load() ~ GETFIELD(EndLineField) ~
              ifCondition(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(EndColField) ~
              otherLoc.load() ~ GETFIELD(EndColField) ~
              ifCondition(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(SourceField) ~
              otherLoc.load() ~ GETFIELD(SourceField) ~
              INVOKESTATIC(Objects.EqualsMethod) ~
              IRETURN()
          }
      }
    ))

    private def HashCodeMethod: InstanceMethod = JavaObject.HashcodeMethod.implementation(this.jvmName, Some(_ =>
      ICONST_5() ~ ANEWARRAY(JavaObject.jvmName) ~
        DUP() ~ ICONST_0() ~ thisLoad() ~ GETFIELD(SourceField) ~ AASTORE() ~
        DUP() ~ ICONST_1() ~ thisLoad() ~ GETFIELD(BeginLineField) ~ boxInt() ~ AASTORE() ~
        DUP() ~ ICONST_2() ~ thisLoad() ~ GETFIELD(BeginColField) ~ boxInt() ~ AASTORE() ~
        DUP() ~ ICONST_3() ~ thisLoad() ~ GETFIELD(EndLineField) ~ boxInt() ~ AASTORE() ~
        DUP() ~ ICONST_4() ~ thisLoad() ~ GETFIELD(EndColField) ~ boxInt() ~ AASTORE() ~
        INVOKESTATIC(Objects.HashMethod) ~
        IRETURN()
    ))

    private def boxInt(): InstructionSet = INVOKESTATIC(JvmName.Integer, "valueOf",
      mkDescriptor(BackendType.Int32)(JvmName.Integer.toTpe))
  }

  case object Global extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor)
      cm.mkStaticConstructor(StaticConstructor)

      cm.mkField(CounterField)
      cm.mkStaticMethod(NewIdMethod)

      cm.mkField(ArgsField)
      cm.mkStaticMethod(GetArgsMethod)
      cm.mkStaticMethod(SetArgsMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def StaticConstructor: StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(_ =>
      NEW(JvmName.AtomicLong) ~
        DUP() ~ invokeConstructor(JvmName.AtomicLong, MethodDescriptor.NothingToVoid) ~
        PUTSTATIC(CounterField) ~
        ICONST_0() ~
        ANEWARRAY(String.jvmName) ~
        PUTSTATIC(ArgsField) ~
        RETURN()
    ))

    def NewIdMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "newId",
      mkDescriptor()(BackendType.Int64), Some(_ =>
        GETSTATIC(CounterField) ~
          INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement",
            MethodDescriptor(Nil, BackendType.Int64)) ~
          LRETURN()
      ))

    def GetArgsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "getArgs",
      mkDescriptor()(BackendType.Array(String.toTpe)), Some(_ =>
        GETSTATIC(ArgsField) ~ ARRAYLENGTH() ~ ANEWARRAY(String.jvmName) ~ ASTORE(0) ~
          // the new array is now created, now to copy the args
          GETSTATIC(ArgsField) ~
          ICONST_0() ~
          ALOAD(0) ~
          ICONST_0() ~
          GETSTATIC(ArgsField) ~ ARRAYLENGTH() ~
          arrayCopy() ~
          ALOAD(0) ~ ARETURN()
      ))

    def SetArgsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "setArgs",
      mkDescriptor(BackendType.Array(String.toTpe))(VoidableType.Void), Some(_ =>
        ALOAD(0) ~ ARRAYLENGTH() ~ ANEWARRAY(String.jvmName) ~ ASTORE(1) ~
          ALOAD(0) ~
          ICONST_0() ~
          ALOAD(1) ~
          ICONST_0() ~
          ALOAD(0) ~ ARRAYLENGTH() ~
          arrayCopy() ~
          ALOAD(1) ~ PUTSTATIC(ArgsField) ~ RETURN()
      ))

    def CounterField: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "counter", JvmName.AtomicLong.toTpe)

    def ArgsField: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "args", BackendType.Array(String.toTpe))

    private def arrayCopy(): InstructionSet = (f: F) => {
      f.visitMethodInstruction(Opcodes.INVOKESTATIC, JvmName.System, "arraycopy",
        MethodDescriptor(List(JavaObject.toTpe, BackendType.Int32, JavaObject.toTpe, BackendType.Int32,
          BackendType.Int32), VoidableType.Void))
      f
    }
  }

  case object Regex extends BackendObjType

  case object FlixError extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.jvmName, JvmName.Error)

      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, List(String.toTpe), Some(_ =>
      thisLoad() ~
        ALOAD(1) ~
        invokeConstructor(JvmName.Error, mkDescriptor(String.toTpe)(VoidableType.Void)) ~
        RETURN()
    ))
  }

  case object HoleError extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, FlixError.jvmName)

      cm.mkConstructor(Constructor)
      cm.mkField(HoleField)
      cm.mkField(LocationField)
      cm.mkMethod(EqualsMethod)
      cm.mkMethod(HashCodeMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic,
      List(String.toTpe, ReifiedSourceLocation.toTpe), Some(_ =>
        withName(1, String.toTpe) { hole =>
          withName(2, ReifiedSourceLocation.toTpe) { loc =>
            thisLoad() ~
              // create an error msg
              NEW(StringBuilder.jvmName) ~
              DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
              pushString("Hole '") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              hole.load() ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              pushString("' at ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              loc.load() ~ INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
              INVOKESPECIAL(FlixError.Constructor) ~
              // save the arguments locally
              thisLoad() ~ hole.load() ~ PUTFIELD(HoleField) ~
              thisLoad() ~ loc.load() ~ PUTFIELD(LocationField) ~
              RETURN()
          }
        }
      ))

    private def HoleField: InstanceField =
      InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "hole", String.toTpe)

    private def LocationField: InstanceField =
      InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "location", ReifiedSourceLocation.toTpe)

    private def EqualsMethod: InstanceMethod = JavaObject.EqualsMethod.implementation(this.jvmName, Some(_ =>
      withName(1, JavaObject.toTpe) { other =>
        // check exact equality
        thisLoad() ~ other.load() ~
          ifCondition(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
          // check for null
          other.load() ~
          ifCondition(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
          // check for class equality
          thisLoad() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          other.load() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          ifCondition(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
          // cast the other obj
          other.load() ~ CHECKCAST(this.jvmName) ~
          storeWithName(2, HoleError.toTpe) { otherHoleError =>
            // compare the hole field
            thisLoad() ~ GETFIELD(HoleField) ~
              otherHoleError.load() ~ GETFIELD(HoleField) ~
              INVOKESTATIC(Objects.EqualsMethod) ~
              ifCondition(Condition.EQ)(pushBool(false) ~ IRETURN()) ~
              // compare the location field
              thisLoad() ~ GETFIELD(LocationField) ~
              otherHoleError.load() ~ GETFIELD(LocationField) ~
              INVOKESTATIC(Objects.EqualsMethod) ~
              IRETURN()
          }
      }
    ))

    private def HashCodeMethod: InstanceMethod = JavaObject.HashcodeMethod.implementation(this.jvmName, Some(_ =>
      ICONST_2() ~
        ANEWARRAY(JavaObject.jvmName) ~
        // store hole
        DUP() ~ ICONST_0() ~ thisLoad() ~ GETFIELD(HoleField) ~ AASTORE() ~
        // store location
        DUP() ~ ICONST_1() ~ thisLoad() ~ GETFIELD(LocationField) ~ AASTORE() ~
        // hash the array
        INVOKESTATIC(Objects.HashMethod) ~
        IRETURN()
    ))
  }

  case object MatchError extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(MatchError.jvmName, IsFinal, superClass = FlixError.jvmName)

      cm.mkConstructor(Constructor)

      cm.mkField(LocationField)

      cm.mkMethod(EqualsMethod)
      cm.mkMethod(HashCodeMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(MatchError.jvmName, IsPublic, List(ReifiedSourceLocation.toTpe), Some(_ =>
      thisLoad() ~
        NEW(StringBuilder.jvmName) ~
        DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        pushString("Non-exhaustive match at ") ~
        INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        ALOAD(1) ~ INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKESPECIAL(FlixError.Constructor) ~
        thisLoad() ~
        ALOAD(1) ~
        PUTFIELD(MatchError.LocationField) ~
        RETURN()
    ))

    def LocationField: InstanceField = InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "location", ReifiedSourceLocation.toTpe)

    private def EqualsMethod: InstanceMethod = JavaObject.EqualsMethod.implementation(this.jvmName, Some(_ =>
      withName(1, JavaObject.toTpe) { otherObj =>
        // check exact equality
        thisLoad() ~
          otherObj.load() ~
          ifCondition(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
          // check `other == null`
          otherObj.load() ~
          ifCondition(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
          // the class equality
          thisLoad() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          otherObj.load() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          ifCondition(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
          // check individual fields
          ALOAD(1) ~ CHECKCAST(this.jvmName) ~
          storeWithName(2, this.toTpe) { otherErr =>
            thisLoad() ~ GETFIELD(LocationField) ~
              otherErr.load() ~ GETFIELD(MatchError.LocationField) ~
              INVOKESTATIC(Objects.EqualsMethod) ~
              IRETURN()
          }
      }
    ))

    private def HashCodeMethod: InstanceMethod = JavaObject.HashcodeMethod.implementation(this.jvmName, Some(_ =>
      ICONST_1() ~ ANEWARRAY(JavaObject.jvmName) ~
        DUP() ~ ICONST_0() ~ thisLoad() ~ GETFIELD(LocationField) ~ AASTORE() ~
        INVOKESTATIC(Objects.HashMethod) ~
        IRETURN()
    ))
  }

  case object Region extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkField(ThreadsField)
      cm.mkField(RegionThreadField)
      cm.mkField(ChildExceptionField)
      cm.mkField(OnExitField)

      cm.mkConstructor(Constructor)

      cm.mkMethod(SpawnMethod)
      cm.mkMethod(ExitMethod)
      cm.mkMethod(ReportChildExceptionMethod)
      cm.mkMethod(ReThrowChildExceptionMethod)
      cm.mkMethod(RunOnExitMethod)

      cm.closeClassMaker()
    }

    // private final ConcurrentLinkedQueue<Thread> threads = new ConcurrentLinkedQueue<Thread>();
    def ThreadsField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "threads", BackendObjType.ConcurrentLinkedQueue.toTpe)

    // private final LinkedList<Runnable> onExit = new LinkedList<Runnable>();
    def OnExitField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "onExit", BackendObjType.LinkedList.toTpe)

    // private final Thread regionThread = Thread.currentThread();
    def RegionThreadField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "regionThread", JvmName.Thread.toTpe)

    // private volatile Throwable childException = null;
    def ChildExceptionField: InstanceField = InstanceField(this.jvmName, IsPrivate, NotFinal, IsVolatile, "childException", JvmName.Throwable.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(_ =>
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~
      thisLoad() ~ NEW(BackendObjType.ConcurrentLinkedQueue.jvmName) ~
      DUP() ~ invokeConstructor(BackendObjType.ConcurrentLinkedQueue.jvmName, MethodDescriptor.NothingToVoid) ~
      PUTFIELD(ThreadsField) ~
      thisLoad() ~ INVOKESTATIC(Thread.CurrentThreadMethod) ~
      PUTFIELD(RegionThreadField) ~
      thisLoad() ~ ACONST_NULL() ~
      PUTFIELD(ChildExceptionField) ~
      thisLoad() ~ NEW(BackendObjType.LinkedList.jvmName) ~
      DUP() ~ invokeConstructor(BackendObjType.LinkedList.jvmName, MethodDescriptor.NothingToVoid) ~
      PUTFIELD(OnExitField) ~
      RETURN()
    ))

    // final public void spawn(Runnable r) {
    //   Thread t = new Thread(r);
    //   t.setUncaughtExceptionHandler(new UncaughtExceptionHandler(this));
    //   t.start();
    //   threads.add(t);
    // }
    def SpawnMethod(implicit flix: Flix): InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "spawn", mkDescriptor(JvmName.Runnable.toTpe)(VoidableType.Void), Some(_ =>
      (
        // TODO: VirtualThreads: Enable by default once JDK 21+ becomes a requirement.
        if (false) {
          INVOKESTATIC(Thread.OfVirtualMethod) ~ ALOAD(1) ~ INVOKEINTERFACE(ThreadBuilderOfVirtual.UnstartedMethod)
        } else {
          NEW(BackendObjType.Thread.jvmName) ~ DUP() ~ ALOAD(1) ~
          invokeConstructor(BackendObjType.Thread.jvmName, mkDescriptor(JvmName.Runnable.toTpe)(VoidableType.Void))
        }
      ) ~
      storeWithName(2, BackendObjType.Thread.toTpe) { thread =>
        thread.load() ~ NEW(BackendObjType.UncaughtExceptionHandler.jvmName) ~
        DUP() ~ thisLoad() ~
        invokeConstructor(BackendObjType.UncaughtExceptionHandler.jvmName, mkDescriptor(BackendObjType.Region.toTpe)(VoidableType.Void)) ~
        INVOKEVIRTUAL(Thread.SetUncaughtExceptionHandlerMethod) ~
        thread.load() ~ INVOKEVIRTUAL(Thread.StartMethod) ~
        thisLoad() ~ GETFIELD(ThreadsField) ~ thread.load() ~
        INVOKEVIRTUAL(ConcurrentLinkedQueue.AddMethod) ~ POP() ~
        RETURN()
      }
    ))

    // final public void exit() throws InterruptedException {
    //   Thread t;
    //   while ((t = threads.poll()) != null)
    //     t.join();
    //   for (Runnable r: onExit)
    //     r.run();
    // }
    def ExitMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "exit", MethodDescriptor.NothingToVoid, Some(_ =>
      withName(1, BackendObjType.Thread.toTpe) { t =>
        whileLoop(Condition.NONNULL) {
          thisLoad() ~ GETFIELD(ThreadsField) ~
          INVOKEVIRTUAL(ConcurrentLinkedQueue.PollMethod) ~
          CHECKCAST(BackendObjType.Thread.jvmName) ~ DUP() ~ t.store()
        } {
          t.load() ~ INVOKEVIRTUAL(Thread.JoinMethod)
        } ~
        withName(2, BackendObjType.Iterator.toTpe) { i =>
          thisLoad() ~ GETFIELD(OnExitField) ~
          INVOKEVIRTUAL(LinkedList.IteratorMethod) ~
          i.store() ~
          whileLoop(Condition.NE) {
            i.load() ~ INVOKEINTERFACE(Iterator.HasNextMethod)
          } {
            i.load() ~ INVOKEINTERFACE(Iterator.NextMethod) ~
            CHECKCAST(Runnable.jvmName) ~
            INVOKEINTERFACE(Runnable.RunMethod)
          }
        } ~
        RETURN()
      }
    ))

    // final public void reportChildException(Throwable e) {
    //   childException = e;
    //   regionThread.interrupt();
    // }
    def ReportChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "reportChildException", mkDescriptor(JvmName.Throwable.toTpe)(VoidableType.Void), Some(_ =>
      thisLoad() ~ ALOAD(1) ~
      PUTFIELD(ChildExceptionField) ~
      thisLoad() ~ GETFIELD(RegionThreadField) ~
      INVOKEVIRTUAL(Thread.InterruptMethod) ~
      RETURN()
    ))

    // final public void reThrowChildException() throws Throwable {
    //   if (childException != null)
    //     throw childException;
    // }
    def ReThrowChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "reThrowChildException", MethodDescriptor.NothingToVoid, Some(_ =>
      thisLoad() ~ GETFIELD(ChildExceptionField) ~
      ifCondition(Condition.NONNULL) {
        thisLoad() ~ GETFIELD(ChildExceptionField) ~
        ATHROW()
      } ~
      RETURN()
    ))

    // final public void runOnExit(Runnable r) {
    //   onExit.addFirst(r);
    // }
    def RunOnExitMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "runOnExit", mkDescriptor(BackendObjType.Runnable.toTpe)(VoidableType.Void), Some(_ =>
      thisLoad() ~ GETFIELD(OnExitField) ~ ALOAD(1) ~
      INVOKEVIRTUAL(LinkedList.AddFirstMethod) ~
      RETURN()
    ))
  }

  case object UncaughtExceptionHandler extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(ThreadUncaughtExceptionHandler.jvmName))

      cm.mkField(RegionField)
      cm.mkConstructor(Constructor)
      cm.mkMethod(UncaughtExceptionMethod)

      cm.closeClassMaker()
    }

    // private final Region r;
    def RegionField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "r", BackendObjType.Region.toTpe)

    // UncaughtExceptionHandler(Region r) { this.r = r; }
    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, BackendObjType.Region.toTpe :: Nil, Some(_ =>
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~
      thisLoad() ~ ALOAD(1) ~ PUTFIELD(RegionField) ~
      RETURN()
    ))

    // public void uncaughtException(Thread t, Throwable e) { r.reportChildException(e); }
    def UncaughtExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "uncaughtException", ThreadUncaughtExceptionHandler.UncaughtExceptionMethod.d, Some(_ =>
      thisLoad() ~ GETFIELD(RegionField) ~
      ALOAD(2) ~ INVOKEVIRTUAL(Region.ReportChildExceptionMethod) ~
      RETURN()
    ))
  }

  //
  // Java Types
  //

  case object String extends BackendObjType {
    def BoolValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Bool)(this.jvmName.toTpe), None)

    def CharValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Char)(this.jvmName.toTpe), None)

    // implicit use of Int8 as Int32
    def Int8ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int32)(this.jvmName.toTpe), None)

    // implicit use of Int16 as Int32
    def Int16ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int32)(this.jvmName.toTpe), None)

    def Int32ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int32)(this.jvmName.toTpe), None)

    def Int64ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int64)(this.jvmName.toTpe), None)

    def Float32ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Float32)(this.jvmName.toTpe), None)

    def Float64ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Float64)(this.jvmName.toTpe), None)

    def ObjectValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendObjType.JavaObject.toTpe)(this.jvmName.toTpe), None)
  }

  case object Arrays extends BackendObjType {
    def BoolArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Bool))(BackendObjType.String.toTpe), None)

    def CharArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Char))(BackendObjType.String.toTpe), None)

    def Int8ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int8))(BackendObjType.String.toTpe), None)

    def Int16ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int16))(BackendObjType.String.toTpe), None)

    def Int32ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int32))(BackendObjType.String.toTpe), None)

    def Int64ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int64))(BackendObjType.String.toTpe), None)

    def Float32ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Float32))(BackendObjType.String.toTpe), None)

    def Float64ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Float64))(BackendObjType.String.toTpe), None)

    def ObjArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendObjType.JavaObject.toTpe))(BackendObjType.String.toTpe), None)

    def DeepToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "deepToString", mkDescriptor(BackendType.Array(BackendObjType.JavaObject.toTpe))(BackendObjType.String.toTpe), None)
  }

  case object JavaObject extends BackendObjType {

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, None)

    def EqualsMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "equals",
      mkDescriptor(JavaObject.toTpe)(BackendType.Bool), None)

    def HashcodeMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "hashCode",
      mkDescriptor()(BackendType.Int32), None)

    def ToStringMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "toString",
      mkDescriptor()(String.toTpe), None)

    def GetClassMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "getClass",
      mkDescriptor()(JvmName.Class.toTpe), None)
  }

  case object StringBuilder extends BackendObjType {

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, None)

    def AppendStringMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "append",
      mkDescriptor(String.toTpe)(StringBuilder.toTpe), None)

    def AppendInt32Method: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "append",
      mkDescriptor(BackendType.Int32)(StringBuilder.toTpe), None)

  }

  case object Objects extends BackendObjType {

    def EqualsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "equals",
      mkDescriptor(JavaObject.toTpe, JavaObject.toTpe)(BackendType.Bool), None)

    def HashMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "hash",
      mkDescriptor(BackendType.Array(JavaObject.toTpe))(BackendType.Int32), None)

  }

  case object LambdaMetaFactory extends BackendObjType {
    private def methodHandlesLookup: BackendType = BackendObjType.Native(JvmName(List("java", "lang", "invoke"), "MethodHandles$Lookup")).toTpe

    private def methodType: BackendType = BackendObjType.Native(JvmName(List("java", "lang", "invoke"), "MethodType")).toTpe

    private def methodHandle: BackendType = BackendObjType.Native(JvmName(List("java", "lang", "invoke"), "MethodHandle")).toTpe

    private def callSite: BackendType = BackendObjType.Native(JvmName(List("java", "lang", "invoke"), "CallSite")).toTpe

    def MetaFactoryMethod: StaticMethod = StaticMethod(
      this.jvmName, IsPublic, IsFinal, "metaFactory",
      mkDescriptor(methodHandlesLookup, String.toTpe, methodType, methodType, methodHandle, methodType)(callSite),
      None
    )
  }

  case object LinkedList extends BackendObjType {

    def AddFirstMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "addFirst",
      mkDescriptor(JavaObject.toTpe)(VoidableType.Void), None)

    def IteratorMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "iterator",
      mkDescriptor()(BackendObjType.Iterator.toTpe), None)
  }

  case object Iterator extends BackendObjType {

    def HasNextMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "hasNext",
      mkDescriptor()(BackendType.Bool))

    def NextMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "next",
      mkDescriptor()(JavaObject.toTpe))
  }

  case object Runnable extends BackendObjType {

    def RunMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "run",
      MethodDescriptor.NothingToVoid)
  }

  case object ConcurrentLinkedQueue extends BackendObjType {

    def AddMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "add",
      mkDescriptor(JavaObject.toTpe)(BackendType.Bool), None)

    def PollMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "poll",
      mkDescriptor()(JavaObject.toTpe), None)
  }

  case object Thread extends BackendObjType {

    def StartMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "start",
      MethodDescriptor.NothingToVoid, None)

    def JoinMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "join",
      MethodDescriptor.NothingToVoid, None)

    def CurrentThreadMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "currentThread",
      mkDescriptor()(this.toTpe), None)

    def InterruptMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "interrupt",
      MethodDescriptor.NothingToVoid, None)

    def SetUncaughtExceptionHandlerMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "setUncaughtExceptionHandler",
      mkDescriptor(ThreadUncaughtExceptionHandler.toTpe)(VoidableType.Void), None)

    def OfVirtualMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "ofVirtual",
      mkDescriptor()(ThreadBuilderOfVirtual.toTpe), None)
  }

  case object ThreadBuilderOfVirtual extends BackendObjType {

    def UnstartedMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "unstarted",
      mkDescriptor(JvmName.Runnable.toTpe)(BackendObjType.Thread.toTpe))
  }

  case object ThreadUncaughtExceptionHandler extends BackendObjType {

    def UncaughtExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "uncaughtException",
      mkDescriptor(Thread.toTpe, JvmName.Throwable.toTpe)(VoidableType.Void), None)
  }

  case object Result extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.closeClassMaker()
    }

    /**
      * Expects a Thunk on the stack and leaves a non-Thunk Result.
      * [..., Result] --> [..., Suspension|Value]
      */
    def unwindThunk(): InstructionSet = {
      INVOKEVIRTUAL(Thunk.InvokeMethod) ~
        whileLoop(Condition.NE)(DUP() ~ INSTANCEOF(Thunk.jvmName)) {
          CHECKCAST(Thunk.jvmName) ~
            INVOKEVIRTUAL(Thunk.InvokeMethod)
        }
    }

    /**
      * Expects a Result on the stack.
      * If the result is a Suspension, this will return a modified Suspension.
      * If the result in NOT a Suspension, this will leave it on the stack.
      * [..., Result] --> [..., Thunk|Value]
      * side effect: might return
      */
    def handleSuspension(): InstructionSet = {
      DUP() ~ INSTANCEOF(Suspension.jvmName) ~
      ifCondition(Condition.NE) {
        DUP() ~ CHECKCAST(Suspension.jvmName) ~ // [..., s]
        // Add our new frame
        NEW(Suspension.jvmName) ~ DUP() ~ INVOKESPECIAL(Suspension.Constructor) ~ // [..., s, s']
        SWAP() ~ // [..., s', s]
        DUP2() ~ // [..., s', s, s', s]
        GETFIELD(Suspension.EffSymField) ~ PUTFIELD(Suspension.EffSymField) ~ // [..., s', s]
        DUP2() ~ GETFIELD(Suspension.EffOpField) ~ PUTFIELD(Suspension.EffOpField) ~ // [..., s', s]
        DUP2() ~ GETFIELD(Suspension.ResumptionField) ~ PUTFIELD(Suspension.ResumptionField) ~ // [..., s', s]
        DUP2() ~ GETFIELD(Suspension.PrefixField) ~ // [..., s', s, s', s.prefix]
        // Make the new frame and push it
        pushNull() ~ // TODO
        INVOKEINTERFACE(Frames.PushMethod) ~ // [..., s', s, s', prefix']
        PUTFIELD(Suspension.PrefixField) ~ // [..., s', s]
        POP() ~ // [..., s']
        // Return the suspension up the stack
        xReturn(Suspension.toTpe)
      }
    }

    /**
      * Expects a Result on the stack and crashes if it is a Suspension, otherwise leaves it on the stack.
      * [..., Result] --> [..., Thunk|Value]
      * side effect: might crash
      */
    def crashIfSuspension(): InstructionSet = {
      val errorMessage = "function was assumed control-pure for java interop but suspension was returned"
      DUP() ~ INSTANCEOF(Suspension.jvmName) ~
      ifCondition(Condition.NE) {
        // If Suspension is found, fail hard.
        NEW(FlixError.jvmName) ~
        DUP() ~ pushString(errorMessage) ~ INVOKESPECIAL(FlixError.Constructor) ~
        ATHROW()
      }
    }

    /**
      * Expects a Result on the stack and leaves something of the given tpe but erased.
      * This might return if a Suspension is encountered.
      * [..., Result] --> [..., Value.value: tpe]
      * side effect: Might return
      */
    def unwindThunkToType(tpe: BackendType): InstructionSet = {
      unwindThunk() ~
      handleSuspension() ~
      CHECKCAST(Value.jvmName) ~ GETFIELD(Value.fieldFromType(tpe))
    }

    /**
      * Expects a Thunk on the stack and leaves something of the given tpe but erased.
      * Assumes that the thunk is control-pure, i.e. never returns a suspension.
      * [..., Result] --> [..., Value.value: tpe]
      * side effect: might crash
      */
    def unwindSuspensionFreeThunkToType(tpe: BackendType): InstructionSet = {
      unwindThunk() ~
      crashIfSuspension() ~
      CHECKCAST(Value.jvmName) ~ GETFIELD(Value.fieldFromType(tpe))
    }
  }

  case object Value extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Result.jvmName))

      // The fields of all erased types, only one will be relevant
      cm.mkConstructor(Constructor)
      cm.mkField(BoolField)
      cm.mkField(CharField)
      cm.mkField(Int8Field)
      cm.mkField(Int16Field)
      cm.mkField(Int32Field)
      cm.mkField(Int64Field)
      cm.mkField(Float32Field)
      cm.mkField(Float64Field)
      cm.mkField(ObjectField)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def BoolField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "b", BackendType.Bool)

    def CharField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "c", BackendType.Char)

    def Int8Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i8", BackendType.Int8)

    def Int16Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i16", BackendType.Int16)

    def Int32Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i32", BackendType.Int32)

    def Int64Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i64", BackendType.Int64)

    def Float32Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "f32", BackendType.Float32)

    def Float64Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "f64", BackendType.Float64)

    def ObjectField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "o", BackendObjType.JavaObject.toTpe)

    /**
      * Returns the field of Value corresponding to the given type
      */
    def fieldFromType(tpe: BackendType): InstanceField = {
      import BackendType._
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

  /** Frame is really just java.util.Function<Value, Result> **/
  case object Frame extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkAbstractClass(this.jvmName)

      cm.mkConstructor(Constructor)
      cm.mkAbstractMethod(ApplyMethod)
      cm.mkStaticMethod(StaticApplyMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def ApplyMethod: AbstractMethod = AbstractMethod(this.jvmName, IsPublic, "apply", mkDescriptor(Value.toTpe)(Result.toTpe))

    def StaticApplyMethod: StaticMethod = StaticMethod(
      this.jvmName,
      IsPublic,
      IsFinal,
      "apply",
      mkDescriptor(Frame.toTpe, Value.toTpe)(Result.toTpe),
      Some(_ => withName(0, Frame.toTpe){f => withName(1, Value.toTpe){resumeArg => {
        f.load() ~ resumeArg.load() ~ INVOKEVIRTUAL(Frame.ApplyMethod) ~ ARETURN()
      }}}))
  }

  case object Thunk extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkAbstractClass(this.jvmName, interfaces = List(Result.jvmName, Runnable.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkAbstractMethod(InvokeMethod)
      cm.mkMethod(RunMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def InvokeMethod: AbstractMethod = AbstractMethod(this.jvmName, IsPublic, "invoke", mkDescriptor()(Result.toTpe))

    def RunMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "run", mkDescriptor()(VoidableType.Void), Some(_ =>
      thisLoad() ~ Result.unwindThunk() ~ Result.crashIfSuspension() ~ POP() ~ RETURN()
    ))
  }

  case object Suspension extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Result.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkField(EffSymField)
      cm.mkField(EffOpField)
      cm.mkField(PrefixField)
      cm.mkField(ResumptionField)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def EffSymField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "effSym", String.toTpe)
    def EffOpField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "effOp", EffectCall.toTpe)
    def PrefixField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "prefix", Frames.toTpe)
    def ResumptionField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "resumption", Resumption.toTpe)

  }

  case object Frames extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)

      cm.mkInterfaceMethod(PushMethod)
      cm.mkInterfaceMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def PushMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "push", mkDescriptor(Frame.toTpe)(Frames.toTpe))

    def ReverseOntoMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "reverseOnto", mkDescriptor(Frames.toTpe)(Frames.toTpe))

    val pushImplementation: Unit => InstructionSet = _ => {
      withName(1, Frame.toTpe)(frame =>
        NEW(FramesCons.jvmName) ~ DUP() ~ INVOKESPECIAL(FramesCons.Constructor) ~
          DUP() ~ frame.load() ~ PUTFIELD(FramesCons.HeadField) ~
          DUP() ~ thisLoad() ~ PUTFIELD(FramesCons.TailField) ~
          xReturn(FramesCons.toTpe)
      )
    }
  }

  case object FramesCons extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Frames.jvmName))

      cm.mkField(HeadField)
      cm.mkField(TailField)
      cm.mkConstructor(Constructor)
      cm.mkMethod(PushMethod)
      cm.mkMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def HeadField: InstanceField = InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "head", Frame.toTpe)

    def TailField: InstanceField = InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "tail", Frames.toTpe)

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.jvmName, IsFinal, Some(Frames.pushImplementation))

    def ReverseOntoMethod: InstanceMethod = Frames.ReverseOntoMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Frames.toTpe)(rest =>
        thisLoad() ~ GETFIELD(TailField) ~
        NEW(FramesCons.jvmName) ~ DUP() ~ INVOKESPECIAL(FramesCons.Constructor) ~
        DUP() ~ thisLoad() ~ GETFIELD(HeadField) ~ PUTFIELD(HeadField) ~
        DUP() ~ rest.load() ~ PUTFIELD(TailField) ~
        INVOKEINTERFACE(Frames.ReverseOntoMethod) ~
        xReturn(Frames.toTpe)
      )
    ))
  }

  case object FramesNil extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Frames.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkMethod(PushMethod)
      cm.mkMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.jvmName, IsFinal, Some(Frames.pushImplementation))

    def ReverseOntoMethod: InstanceMethod = Frames.ReverseOntoMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Frames.toTpe)(rest =>
        rest.load() ~ xReturn(rest.tpe)
      )
    ))
  }

  case object Resumption extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkInterfaceMethod(RewindMethod)
      cm.mkStaticMethod(StaticRewindMethod)
      cm.closeClassMaker()
    }

    def RewindMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "rewind", mkDescriptor(Value.toTpe)(Result.toTpe))

    def StaticRewindMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, NotFinal, "staticRewind", mkDescriptor(Resumption.toTpe, Value.toTpe)(Result.toTpe), Some(_ =>
      withName(0, Resumption.toTpe) { resumption =>
        withName(1, Value.toTpe) { v => {
          resumption.load() ~ v.load() ~ INVOKEINTERFACE(Resumption.RewindMethod) ~ ARETURN()
        }
        }
      }
    ))
  }

  case object ResumptionCons extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Resumption.jvmName))

      cm.mkConstructor(Constructor)

      cm.mkField(SymField)
      cm.mkField(HandlerField)
      cm.mkField(FramesField)
      cm.mkField(TailField)

      cm.mkMethod(RewindMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def SymField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "sym", String.toTpe)
    def HandlerField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "handler", Handler.toTpe)
    def FramesField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "frames", Frames.toTpe)
    def TailField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "tail", Resumption.toTpe)

    def RewindMethod: InstanceMethod = Resumption.RewindMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Value.toTpe) { v =>
        thisLoad() ~ GETFIELD(SymField) ~
          thisLoad() ~ GETFIELD(HandlerField) ~
          thisLoad() ~ GETFIELD(FramesField) ~
          // () -> tail.rewind(v)
          thisLoad() ~ GETFIELD(TailField) ~
          v.load() ~
          mkStaticLambda(Thunk.InvokeMethod, Resumption.StaticRewindMethod) ~
          mkStaticLambda(Thunk.InvokeMethod, Handler.InstallHandlerMethod) ~
          xReturn(Thunk.toTpe)
      }))
  }

  case object ResumptionNil extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Resumption.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkMethod(RewindMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def RewindMethod: InstanceMethod = Resumption.RewindMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Value.toTpe) { v =>
        v.load() ~ xReturn(v.tpe)
      }
    ))
  }

  case object Handler extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkStaticMethod(InstallHandlerMethod)
      cm.closeClassMaker()
    }

    def InstallHandlerMethod: StaticMethod = StaticMethod(
      this.jvmName,
      IsPublic,
      NotFinal,
      "installHandler",
      mkDescriptor(String.toTpe, Handler.toTpe, Frames.toTpe, Thunk.toTpe)(Result.toTpe),
      Some(_ => withName(0, String.toTpe){effSym => withName(1, Handler.toTpe){handler =>
        withName(2, Frames.toTpe){frames => withName(3, Thunk.toTpe){thunk =>
          thunk.load() ~
          // Thunk|Value|Suspension
          Result.unwindThunk() ~
          // Value|Suspension
          { // handle suspension
            DUP() ~ INSTANCEOF(Suspension.jvmName) ~ ifCondition(Condition.NE) {
              DUP() ~ CHECKCAST(Suspension.jvmName) ~ storeWithName(4, Suspension.toTpe) {s =>
                NEW(ResumptionCons.jvmName) ~ DUP() ~ INVOKESPECIAL(ResumptionCons.Constructor) ~
                  DUP() ~ effSym.load() ~ PUTFIELD(ResumptionCons.SymField) ~
                  DUP() ~ handler.load() ~ PUTFIELD(ResumptionCons.HandlerField) ~
                  DUP() ~
                  s.load() ~ GETFIELD(Suspension.PrefixField) ~ frames.load() ~ INVOKEINTERFACE(Frames.ReverseOntoMethod) ~
                  PUTFIELD(ResumptionCons.FramesField) ~
                  DUP() ~ s.load() ~ GETFIELD(Suspension.ResumptionField) ~ PUTFIELD(ResumptionCons.TailField) ~
                  storeWithName(5, ResumptionCons.toTpe){r =>
                    s.load() ~ GETFIELD(Suspension.EffSymField) ~ effSym.load() ~ INVOKEVIRTUAL(JavaObject.EqualsMethod) ~
                    ifCondition(Condition.NE){
                      s.load() ~ GETFIELD(Suspension.EffOpField) ~ handler.load() ~ r.load() ~
                      INVOKEINTERFACE(EffectCall.ApplyMethod) ~ xReturn(Result.toTpe)
                    } ~
                    NEW(Suspension.jvmName) ~ DUP() ~ INVOKESPECIAL(Suspension.Constructor) ~
                    DUP() ~ s.load() ~ GETFIELD(Suspension.EffSymField) ~ PUTFIELD(Suspension.EffSymField) ~
                    DUP() ~ s.load() ~ GETFIELD(Suspension.EffOpField) ~ PUTFIELD(Suspension.EffOpField) ~
                    DUP() ~ NEW(FramesNil.jvmName) ~ DUP() ~ INVOKESPECIAL(FramesNil.Constructor) ~ PUTFIELD(Suspension.PrefixField) ~
                    DUP() ~ r.load() ~ PUTFIELD(Suspension.ResumptionField) ~
                    xReturn(Suspension.toTpe)
                  }
              }
            }
          } ~
          // Value
          CHECKCAST(Value.jvmName) ~ storeWithName(6, Value.toTpe) {res =>
            //
            // Case on frames
            // FramesNil
            frames.load() ~ INSTANCEOF(FramesNil.jvmName) ~ ifCondition(Condition.NE) {
              res.load() ~ xReturn(Value.toTpe)
            } ~
              // FramesCons
              frames.load() ~ CHECKCAST(FramesCons.jvmName) ~ storeWithName(7, FramesCons.toTpe) { cons => {
              effSym.load() ~
              handler.load() ~
              cons.load() ~ GETFIELD(FramesCons.TailField) ~
              // thunk
              cons.load() ~ GETFIELD(FramesCons.HeadField) ~
              res.load() ~
              mkStaticLambda(Thunk.InvokeMethod, Frame.StaticApplyMethod) ~
              INVOKESTATIC(InstallHandlerMethod) ~
              xReturn(Result.toTpe)
            }
          }}
      }}}}
      )
    )
  }

  case object EffectCall extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkInterfaceMethod(ApplyMethod)
      cm.closeClassMaker()
    }

    def ApplyMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "apply", mkDescriptor(Handler.toTpe, Resumption.toTpe)(Result.toTpe))

  }
}

sealed trait Generatable extends BackendObjType {
  def genByteCode()(implicit flix: Flix): Array[Byte]
}
