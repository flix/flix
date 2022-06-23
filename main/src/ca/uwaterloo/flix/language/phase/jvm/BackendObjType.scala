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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{DevFlixRuntime, JavaLang, MethodDescriptor, RootPackage}
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
    case BackendObjType.BigInt => JvmName(List("java", "math"), "BigInteger")
    case BackendObjType.String => JvmName(JavaLang, "String")
    case BackendObjType.Channel(_) => JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), mkName("Channel"))
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
    def InstanceField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, "INSTANCE", this.toTpe)
  }

  case object BigInt extends BackendObjType

  case object String extends BackendObjType

  case class Channel(tpe: BackendType) extends BackendObjType

  case class Lazy(tpe: BackendType) extends BackendObjType

  case class Ref(tpe: BackendType) extends BackendObjType {
    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "value", tpe)
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType

  //case class Enum(sym: Symbol.EnumSym, args: List[BackendType]) extends BackendObjType

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType {
    def continuation: BackendObjType.Continuation = Continuation(result.toErased)

    def ArgField(index: Int): InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, s"arg$index", args(index))

    def ResultField: InstanceField = continuation.ResultField

    def InvokeMethod: AbstractMethod = continuation.InvokeMethod

    def UnwindMethod: InstanceMethod = continuation.UnwindMethod
  }

  case class Continuation(result: BackendType) extends BackendObjType {
    def ResultField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "result", result)

    def InvokeMethod: AbstractMethod = AbstractMethod(this.jvmName, IsPublic, "invoke", mkDescriptor()(this.toTpe))

    def UnwindMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "unwind", mkDescriptor()(result), Some(
      thisLoad() ~ storeWithName(1, this.toTpe) { currentCont =>
        pushNull() ~ storeWithName(2, this.toTpe) { previousCont =>
          doWhile(Condition.NONNULL) {
            currentCont.load() ~
              previousCont.store() ~
              currentCont.load() ~
              INVOKEVIRTUAL(this.InvokeMethod) ~
              DUP() ~
              currentCont.store()
          } ~
            previousCont.load() ~
            GETFIELD(this.ResultField) ~
            xReturn(this.result)
        }
      }
    ))

    /**
      * Called when spawned, should only be used by functions returning void.
      */
    def RunMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "run", MethodDescriptor.NothingToVoid, Some(
      thisLoad() ~
        INVOKEVIRTUAL(this.UnwindMethod) ~
        xPop(this.result) ~
        RETURN()
    ))
  }

  case object RecordEmpty extends BackendObjType {
    def interface: BackendObjType.Record.type = Record

    def InstanceField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, "INSTANCE", this.toTpe)

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, {
      Some(throwUnsupportedOperationException(
        s"${BackendObjType.Record.LookupFieldMethod.name} method shouldn't be called"))
    })

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, {
      Some(throwUnsupportedOperationException(
        s"${BackendObjType.Record.RestrictFieldMethod.name} method shouldn't be called"))
    })
  }

  case class RecordExtend(field: String, value: BackendType, rest: BackendType) extends BackendObjType {
    def interface: BackendObjType.Record.type = Record

    def LabelField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "label", BackendObjType.String.toTpe)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "value", value)

    def RestField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "rest", interface.toTpe)

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad() ~ ARETURN()
        case FalseBranch =>
          thisLoad() ~ GETFIELD(this.RestField) ~
            ALOAD(1) ~
            INVOKEINTERFACE(BackendObjType.Record.LookupFieldMethod) ~
            ARETURN()
      }
    ))

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad() ~ GETFIELD(this.RestField) ~
            ARETURN()
        case FalseBranch =>
          thisLoad() ~
            DUP() ~ GETFIELD(this.RestField) ~
            ALOAD(1) ~
            INVOKEINTERFACE(BackendObjType.Record.RestrictFieldMethod) ~
            PUTFIELD(this.RestField) ~
            thisLoad() ~ ARETURN()
      }
    ))

    /**
      * Compares the label of `this`and `ALOAD(1)` and executes the designated branch.
      */
    private def caseOnLabelEquality(cases: Branch => InstructionSet): InstructionSet =
      thisLoad() ~ GETFIELD(this.LabelField) ~
        ALOAD(1) ~
        INVOKEVIRTUAL(BackendObjType.String.jvmName, "equals", mkDescriptor(JvmName.Object.toTpe)(BackendType.Bool)) ~
        branch(Condition.Bool)(cases)
  }

  case object Record extends BackendObjType {
    def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "lookupField",
      mkDescriptor(BackendObjType.String.toTpe)(BackendObjType.Record.toTpe))

    def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "restrictField",
      mkDescriptor(BackendObjType.String.toTpe)(BackendObjType.Record.toTpe))
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
    def ConstructorDescriptor: MethodDescriptor =
      mkDescriptor(BackendObjType.String.toTpe, BackendType.Int32, BackendType.Int32, BackendType.Int32, BackendType.Int32)(VoidableType.Void)

    def SourceField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "source", BackendObjType.String.toTpe)

    def BeginLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "beginLine", BackendType.Int32)

    def BeginColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "beginCol", BackendType.Int32)

    def EndLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "endLine", BackendType.Int32)

    def EndColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "endCol", BackendType.Int32)
  }

  case object Global extends BackendObjType {
    def NewIdMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "newId",
      mkDescriptor()(BackendType.Int64), Some(
        GETSTATIC(Global.CounterField) ~
          INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement",
            MethodDescriptor(Nil, BackendType.Int64)) ~
          LRETURN()
      ))

    def GetArgsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "getArgs",
      mkDescriptor()(BackendType.Array(BackendObjType.String.toTpe)), Some(
        GETSTATIC(Global.ArgsField) ~
          ARRAYLENGTH() ~
          ANEWARRAY(BackendObjType.String.jvmName) ~
          ASTORE(0) ~
          // the new array is now created, now to copy the args
          GETSTATIC(Global.ArgsField) ~
          ICONST_0() ~
          ALOAD(0) ~
          ICONST_0() ~
          GETSTATIC(Global.ArgsField) ~ ARRAYLENGTH() ~
          arrayCopy() ~
          ALOAD(0) ~
          ARETURN()
      ))

    def SetArgsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "setArgs",
      mkDescriptor(BackendType.Array(BackendObjType.String.toTpe))(VoidableType.Void), Some(
        ALOAD(0) ~
          ARRAYLENGTH() ~
          ANEWARRAY(BackendObjType.String.jvmName) ~
          ASTORE(1) ~
          ALOAD(0) ~
          ICONST_0() ~
          ALOAD(1) ~
          ICONST_0() ~
          ALOAD(0) ~ ARRAYLENGTH() ~
          arrayCopy() ~
          ALOAD(1) ~ PUTSTATIC(Global.ArgsField) ~ RETURN()
      ))

    def CounterField: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, "counter", JvmName.AtomicLong.toTpe)

    def ArgsField: StaticField = StaticField(this.jvmName, IsPrivate, IsFinal, "args",
      BackendType.Array(BackendObjType.String.toTpe))

    private def arrayCopy(): InstructionSet = (f: F) => {
      f.visitMethodInstruction(Opcodes.INVOKESTATIC, JvmName.System, "arraycopy",
        MethodDescriptor(List(
          JvmName.Object.toTpe,
          BackendType.Int32,
          JvmName.Object.toTpe,
          BackendType.Int32,
          BackendType.Int32
        ), VoidableType.Void))
      f
    }
  }

  case object FlixError extends BackendObjType {

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, List(BackendObjType.String.toTpe), Some(
      thisLoad() ~
        ALOAD(1) ~
        invokeConstructor(JvmName.Error, mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
        RETURN()
    ))
  }
}
