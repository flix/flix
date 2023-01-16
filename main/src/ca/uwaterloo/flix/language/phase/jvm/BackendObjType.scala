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
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{DevFlixRuntime, JavaLang, JavaUtil, JavaUtilConcurrent, MethodDescriptor, RootPackage}
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
    case BackendObjType.Continuation(result) => JvmName(RootPackage, mkName("Cont", result))
    case BackendObjType.RecordEmpty => JvmName(RootPackage, mkName(s"RecordEmpty"))
    case BackendObjType.RecordExtend(_, value, _) => JvmName(RootPackage, mkName("RecordExtend", value))
    case BackendObjType.Record => JvmName(RootPackage, s"IRecord${Flix.Delimiter}")
    case BackendObjType.Native(className) => className
    case BackendObjType.ReifiedSourceLocation => JvmName(DevFlixRuntime, "ReifiedSourceLocation")
    case BackendObjType.Global => JvmName(DevFlixRuntime, "Global")
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
    case BackendObjType.ConcurrentLinkedQueue => JvmName(JavaUtilConcurrent, "ConcurrentLinkedQueue")
    case BackendObjType.Thread => JvmName(JavaLang, "Thread")
    case BackendObjType.ThreadBuilderOfVirtual => JvmName(JavaLang, "Thread$Builder$OfVirtual")
    case BackendObjType.ThreadUncaughtExceptionHandler => JvmName(JavaLang, "Thread$UncaughtExceptionHandler")
  }

  /**
    * The JVM type descriptor of the form `"L<jvmName.toInternalName>;"`.
    */
  def toDescriptor: String = jvmName.toDescriptor

  /**
    * Returns `this` wrapped in `BackendType.Reference`.
    */
  def toTpe: BackendType.Reference = BackendType.Reference(this)
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


  case object Unit extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkStaticConstructor(StaticConstructor)
      cm.mkConstructor(Constructor)
      cm.mkField(InstanceField)
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ RETURN()
    ))

    def StaticConstructor: StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(
      NEW(this.jvmName) ~
        DUP() ~ INVOKESPECIAL(Constructor) ~
        PUTSTATIC(InstanceField) ~
        RETURN()
    ))

    def InstanceField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, NotVolatile, "INSTANCE", this.toTpe)

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(
      pushString("()") ~ ARETURN()
    ))
  }

  case object BigDecimal extends BackendObjType

  case object BigInt extends BackendObjType

  case class Lazy(tpe: BackendType) extends BackendObjType

  case class Ref(tpe: BackendType) extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkField(ValueField)
      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ RETURN()
    ))

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", tpe)
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType

  //case class Enum(sym: Symbol.EnumSym, args: List[BackendType]) extends BackendObjType

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
        case ObjFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(JavaObject.toTpe)(JavaObject.toTpe),
          Some(
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ ARETURN()
          ))
        case ObjConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(JavaObject.toTpe)(VoidableType.Void),
          Some(
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ RETURN()
          ))
        case ObjPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(JavaObject.toTpe)(BackendType.Bool),
          Some(
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ IRETURN()
          ))
        case IntFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Int32)(JavaObject.toTpe),
          Some(
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ ARETURN()
          ))
        case IntConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Int32)(VoidableType.Void),
          Some(
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ RETURN()
          ))
        case IntPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Int32)(BackendType.Bool),
          Some(
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ IRETURN()
          ))
        case IntUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsInt",
          mkDescriptor(BackendType.Int32)(BackendType.Int32),
          Some(
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ IRETURN()
          ))
        case LongFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Int64)(JavaObject.toTpe),
          Some(
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ ARETURN()
          ))
        case LongConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Int64)(VoidableType.Void),
          Some(
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ RETURN()
          ))
        case LongPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Int64)(BackendType.Bool),
          Some(
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ IRETURN()
          ))
        case LongUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsLong",
          mkDescriptor(BackendType.Int64)(BackendType.Int64),
          Some(
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ LRETURN()
          ))
        case DoubleFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Float64)(JavaObject.toTpe),
          Some(
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ ARETURN()
          ))
        case DoubleConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Float64)(VoidableType.Void),
          Some(
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ RETURN()
          ))
        case DoublePredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Float64)(BackendType.Bool),
          Some(
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ IRETURN()
          ))
        case DoubleUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsDouble",
          mkDescriptor(BackendType.Float64)(BackendType.Float64),
          Some(
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              INVOKEVIRTUAL(continuation.UnwindMethod) ~ DRETURN()
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

      val cm = ClassMaker.mkAbstractClass(this.jvmName, superClass = continuation.jvmName, interfaces)

      cm.mkConstructor(Constructor)
      args.indices.foreach(argIndex => cm.mkField(ArgField(argIndex)))
      specializedInterface.foreach(i => cm.mkMethod(i.functionMethod))
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def continuation: Continuation = Continuation(result.toErased)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(continuation.Constructor) ~ RETURN()
    ))

    def ArgField(index: Int): InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, s"arg$index", args(index))

    def ToStringMethod: InstanceMethod = {
      val argString = args match {
        case Nil => "()"
        case arg :: Nil => arg.toErasedString
        case _ => args.map(_.toErasedString).mkString("(", ", ", ")")
      }
      JavaObject.ToStringMethod.implementation(this.jvmName, Some(
        pushString(s"$argString -> ${result.toErasedString}") ~
          ARETURN()
      ))
    }
  }

  case class Continuation(result: BackendType) extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.jvmName, interfaces = List(JvmName.Runnable))

      cm.mkConstructor(Constructor)
      cm.mkField(ResultField)
      cm.mkAbstractMethod(InvokeMethod)
      cm.mkMethod(UnwindMethod)
      cm.mkMethod(RunMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ RETURN()
    ))

    def ResultField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "result", result)

    def InvokeMethod: AbstractMethod = AbstractMethod(this.jvmName, IsPublic, "invoke", mkDescriptor()(this.toTpe))

    def UnwindMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "unwind", mkDescriptor()(result), Some(
      thisLoad() ~ storeWithName(1, this.toTpe) { currentCont =>
        pushNull() ~ storeWithName(2, this.toTpe) { previousCont =>
          doWhile(Condition.NONNULL) {
            currentCont.load() ~ previousCont.store() ~
              currentCont.load() ~ INVOKEVIRTUAL(InvokeMethod) ~
              DUP() ~ currentCont.store()
          } ~
            previousCont.load() ~ GETFIELD(ResultField) ~ xReturn(this.result)
        }
      }
    ))

    /**
      * Called when spawned, should only be used by functions returning void.
      */
    def RunMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "run", MethodDescriptor.NothingToVoid, Some(
      thisLoad() ~ INVOKEVIRTUAL(UnwindMethod) ~ xPop(this.result) ~
        RETURN()
    ))
  }

  case object RecordEmpty extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(this.interface.jvmName))

      cm.mkStaticConstructor(StaticConstructor)
      cm.mkConstructor(Constructor)
      cm.mkField(InstanceField)
      cm.mkMethod(LookupFieldMethod)
      cm.mkMethod(RestrictFieldMethod)
      cm.mkMethod(ToStringMethod)
      cm.mkMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ RETURN()
    ))

    def StaticConstructor: StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(
      NEW(this.jvmName) ~
        DUP() ~ INVOKESPECIAL(Constructor) ~
        PUTSTATIC(InstanceField) ~
        RETURN()
    ))

    def interface: Record.type = Record

    def InstanceField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, NotVolatile, "INSTANCE", this.toTpe)

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(
      throwUnsupportedOperationException(
        s"${Record.LookupFieldMethod.name} method shouldn't be called")
    ))

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(
      throwUnsupportedOperationException(
        s"${Record.RestrictFieldMethod.name} method shouldn't be called")
    ))

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(
      pushString("{}") ~ ARETURN()
    ))

    private def ToTailStringMethod: InstanceMethod = Record.ToTailStringMethod.implementation(this.jvmName, IsFinal, Some(
      withName(1, StringBuilder.toTpe) { sb =>
        sb.load() ~ pushString("}") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ ARETURN()
      }
    ))
  }

  case class RecordExtend(field: String, value: BackendType, rest: BackendType) extends BackendObjType {
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

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ RETURN()
    ))

    def LabelField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "label", String.toTpe)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", value)

    def RestField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "rest", Record.toTpe)

    def LookupFieldMethod: InstanceMethod = Record.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(
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

    def RestrictFieldMethod: InstanceMethod = Record.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(
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

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(
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

    private def ToTailStringMethod: InstanceMethod = Record.ToTailStringMethod.implementation(this.jvmName, IsFinal, Some(
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

  case object Record extends BackendObjType {
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


  case object ReifiedSourceLocation extends BackendObjType {
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
      List(String.toTpe, BackendType.Int32, BackendType.Int32, BackendType.Int32, BackendType.Int32), Some(
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

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(
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

    private def EqualsMethod: InstanceMethod = JavaObject.EqualsMethod.implementation(this.jvmName, Some(
      withName(1, JavaObject.toTpe) { otherObj =>
        // check exact equality
        thisLoad() ~
          otherObj.load() ~
          ifTrue(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
          // check `other == null`
          otherObj.load() ~
          ifTrue(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
          // the class equality
          thisLoad() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          otherObj.load() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          ifTrue(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
          // check individual fields
          otherObj.load() ~
          CHECKCAST(this.jvmName) ~
          storeWithName(2, this.toTpe) { otherLoc =>
            thisLoad() ~ GETFIELD(BeginLineField) ~
              otherLoc.load() ~ GETFIELD(BeginLineField) ~
              ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(BeginColField) ~
              otherLoc.load() ~ GETFIELD(BeginColField) ~
              ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(EndLineField) ~
              otherLoc.load() ~ GETFIELD(EndLineField) ~
              ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(EndColField) ~
              otherLoc.load() ~ GETFIELD(EndColField) ~
              ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
              thisLoad() ~ GETFIELD(SourceField) ~
              otherLoc.load() ~ GETFIELD(SourceField) ~
              INVOKESTATIC(Objects.EqualsMethod) ~
              IRETURN()
          }
      }
    ))

    private def HashCodeMethod: InstanceMethod = JavaObject.HashcodeMethod.implementation(this.jvmName, Some(
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

  case object Global extends BackendObjType {
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

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ RETURN()
    ))

    def StaticConstructor: StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(
      NEW(JvmName.AtomicLong) ~
        DUP() ~ invokeConstructor(JvmName.AtomicLong, MethodDescriptor.NothingToVoid) ~
        PUTSTATIC(CounterField) ~
        ICONST_0() ~
        ANEWARRAY(String.jvmName) ~
        PUTSTATIC(ArgsField) ~
        RETURN()
    ))

    def NewIdMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "newId",
      mkDescriptor()(BackendType.Int64), Some(
        GETSTATIC(CounterField) ~
          INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement",
            MethodDescriptor(Nil, BackendType.Int64)) ~
          LRETURN()
      ))

    def GetArgsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "getArgs",
      mkDescriptor()(BackendType.Array(String.toTpe)), Some(
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
      mkDescriptor(BackendType.Array(String.toTpe))(VoidableType.Void), Some(
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

  case object FlixError extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.jvmName, JvmName.Error)

      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, List(String.toTpe), Some(
      thisLoad() ~
        ALOAD(1) ~
        invokeConstructor(JvmName.Error, mkDescriptor(String.toTpe)(VoidableType.Void)) ~
        RETURN()
    ))
  }

  case object HoleError extends BackendObjType {
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
      List(String.toTpe, ReifiedSourceLocation.toTpe), Some(
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

    private def EqualsMethod: InstanceMethod = JavaObject.EqualsMethod.implementation(this.jvmName, Some(
      withName(1, JavaObject.toTpe) { other =>
        // check exact equality
        thisLoad() ~ other.load() ~
          ifTrue(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
          // check for null
          other.load() ~
          ifTrue(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
          // check for class equality
          thisLoad() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          other.load() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          ifTrue(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
          // cast the other obj
          other.load() ~ CHECKCAST(this.jvmName) ~
          storeWithName(2, HoleError.toTpe) { otherHoleError =>
            // compare the hole field
            thisLoad() ~ GETFIELD(HoleField) ~
              otherHoleError.load() ~ GETFIELD(HoleField) ~
              INVOKESTATIC(Objects.EqualsMethod) ~
              ifTrue(Condition.EQ)(pushBool(false) ~ IRETURN()) ~
              // compare the location field
              thisLoad() ~ GETFIELD(LocationField) ~
              otherHoleError.load() ~ GETFIELD(LocationField) ~
              INVOKESTATIC(Objects.EqualsMethod) ~
              IRETURN()
          }
      }
    ))

    private def HashCodeMethod: InstanceMethod = JavaObject.HashcodeMethod.implementation(this.jvmName, Some(
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

  case object MatchError extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(MatchError.jvmName, IsFinal, superClass = FlixError.jvmName)

      cm.mkConstructor(Constructor)

      cm.mkField(LocationField)

      cm.mkMethod(EqualsMethod)
      cm.mkMethod(HashCodeMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(MatchError.jvmName, IsPublic, List(ReifiedSourceLocation.toTpe), Some(
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

    private def EqualsMethod: InstanceMethod = JavaObject.EqualsMethod.implementation(this.jvmName, Some(
      withName(1, JavaObject.toTpe) { otherObj =>
        // check exact equality
        thisLoad() ~
          otherObj.load() ~
          ifTrue(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
          // check `other == null`
          otherObj.load() ~
          ifTrue(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
          // the class equality
          thisLoad() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          otherObj.load() ~
          INVOKEVIRTUAL(JavaObject.GetClassMethod) ~
          ifTrue(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
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

    private def HashCodeMethod: InstanceMethod = JavaObject.HashcodeMethod.implementation(this.jvmName, Some(
      ICONST_1() ~ ANEWARRAY(JavaObject.jvmName) ~
        DUP() ~ ICONST_0() ~ thisLoad() ~ GETFIELD(LocationField) ~ AASTORE() ~
        INVOKESTATIC(Objects.HashMethod) ~
        IRETURN()
    ))
  }

  case object Region extends BackendObjType {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkField(ThreadsField)
      cm.mkField(RegionThreadField)
      cm.mkField(ChildExceptionField)

      cm.mkConstructor(Constructor)

      cm.mkMethod(SpawnMethod)
      cm.mkMethod(ExitMethod)
      cm.mkMethod(ReportChildExceptionMethod)
      cm.mkMethod(ReThrowChildExceptionMethod)

      cm.closeClassMaker()
    }

    // private final ConcurrentLinkedQueue<Thread> threads = new ConcurrentLinkedQueue<Thread>();
    def ThreadsField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "threads", BackendObjType.ConcurrentLinkedQueue.toTpe)

    // private final Thread regionThread = Thread.currentThread();
    def RegionThreadField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "regionThread", JvmName.Thread.toTpe)

    // private volatile Throwable childException = null;
    def ChildExceptionField: InstanceField = InstanceField(this.jvmName, IsPrivate, NotFinal, IsVolatile, "childException", JvmName.Throwable.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ 
      thisLoad() ~ NEW(BackendObjType.ConcurrentLinkedQueue.jvmName) ~
      DUP() ~ invokeConstructor(BackendObjType.ConcurrentLinkedQueue.jvmName, MethodDescriptor.NothingToVoid) ~
      PUTFIELD(ThreadsField) ~
      thisLoad() ~ INVOKESTATIC(Thread.CurrentThreadMethod) ~
      PUTFIELD(RegionThreadField) ~
      thisLoad() ~ ACONST_NULL() ~
      PUTFIELD(ChildExceptionField) ~
      RETURN()
    ))

    // final public void spawn(Runnable r) {
    //   Thread t = new Thread(r);
    //   t.setUncaughtExceptionHandler(new UncaughtExceptionHandler(this));
    //   t.start();
    //   threads.add(t);
    // }
    def SpawnMethod(implicit flix: Flix): InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "spawn", mkDescriptor(JvmName.Runnable.toTpe)(VoidableType.Void), Some(
      (
        if (flix.options.xvirtualthreads) {
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
    // }
    def ExitMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "exit", MethodDescriptor.NothingToVoid, Some(
      withName(1, BackendObjType.Thread.toTpe) { t =>
        whileLoop(Condition.NONNULL) {
          thisLoad() ~ GETFIELD(ThreadsField) ~ 
          INVOKEVIRTUAL(ConcurrentLinkedQueue.PollMethod) ~
          CHECKCAST(BackendObjType.Thread.jvmName) ~ DUP() ~ t.store() 
        } {
          t.load() ~ INVOKEVIRTUAL(Thread.JoinMethod)
        } ~
        RETURN()
      }
    ))

    // final public void reportChildException(Throwable e) {
    //   childException = e;
    //   regionThread.interrupt();
    // }
    def ReportChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "reportChildException", mkDescriptor(JvmName.Throwable.toTpe)(VoidableType.Void), Some(
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
    def ReThrowChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "reThrowChildException", MethodDescriptor.NothingToVoid, Some(
      thisLoad() ~ GETFIELD(ChildExceptionField) ~
      ifTrue(Condition.NONNULL) {
        thisLoad() ~ GETFIELD(ChildExceptionField) ~
        ATHROW()
      } ~
      RETURN()
    ))
  }

  case object UncaughtExceptionHandler extends BackendObjType {

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
    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, BackendObjType.Region.toTpe :: Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ 
      thisLoad() ~ ALOAD(1) ~ PUTFIELD(RegionField) ~
      RETURN()
    ))

    // public void uncaughtException(Thread t, Throwable e) { r.reportChildException(e); }
    def UncaughtExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "uncaughtException", ThreadUncaughtExceptionHandler.UncaughtExceptionMethod.d, Some(
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
}
