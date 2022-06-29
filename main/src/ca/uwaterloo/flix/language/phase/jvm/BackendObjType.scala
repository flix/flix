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
    case BackendObjType.HoleError => JvmName(DevFlixRuntime, "HoleError")
    case BackendObjType.MatchError => JvmName(DevFlixRuntime, "MatchError")
    // Java classes
    case BackendObjType.JavaObject => JvmName(JavaLang, "Object")
    case BackendObjType.String => JvmName(JavaLang, "String")
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

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~
        invokeConstructor(continuation.jvmName, MethodDescriptor.NothingToVoid) ~
        RETURN()
    ))

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

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(
      throwUnsupportedOperationException(
        s"${BackendObjType.Record.LookupFieldMethod.name} method shouldn't be called")
    ))

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(
      throwUnsupportedOperationException(
        s"${BackendObjType.Record.RestrictFieldMethod.name} method shouldn't be called")
    ))
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
          thisLoad() ~ GETFIELD(this.RestField) ~ ARETURN()
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
        INVOKEVIRTUAL(BackendObjType.JavaObject.EqualsMethod) ~
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
    def StaticConstructor: StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(
      NEW(JvmName.AtomicLong) ~
        DUP() ~ invokeConstructor(JvmName.AtomicLong) ~
        PUTSTATIC(Global.CounterField) ~
        ICONST_0() ~
        ANEWARRAY(BackendObjType.String.jvmName) ~
        PUTSTATIC(Global.ArgsField) ~
        RETURN()
    ))

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
          BackendObjType.JavaObject.toTpe,
          BackendType.Int32,
          BackendObjType.JavaObject.toTpe,
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

  case object HoleError extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(BackendObjType.HoleError.jvmName, IsFinal, BackendObjType.FlixError.jvmName)

      cm.mkConstructor(Constructor)
      cm.mkField(HoleField)
      cm.mkField(LocationField)
      cm.mkMethod(EqualsMethod)
      cm.mkMethod(HashCodeMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic,
      List(BackendObjType.String.toTpe, BackendObjType.ReifiedSourceLocation.toTpe), Some(
        withName(1, BackendObjType.String.toTpe) { hole =>
          withName(2, BackendObjType.ReifiedSourceLocation.toTpe) { loc =>
            thisLoad() ~
              // create an error msg
              NEW(JvmName.StringBuilder) ~
              DUP() ~
              invokeConstructor(JvmName.StringBuilder) ~
              pushString("Hole '") ~ stringBuilderAppend() ~
              hole.load() ~ stringBuilderAppend() ~
              pushString("' at ") ~ stringBuilderAppend() ~
              loc.load() ~ INVOKEVIRTUAL(BackendObjType.JavaObject.ToStringMethod) ~ stringBuilderAppend() ~
              INVOKEVIRTUAL(BackendObjType.JavaObject.ToStringMethod) ~
              INVOKESPECIAL(BackendObjType.FlixError.Constructor) ~
              // save the arguments locally
              thisLoad() ~ hole.load() ~ PUTFIELD(BackendObjType.HoleError.HoleField) ~
              thisLoad() ~ loc.load() ~ PUTFIELD(BackendObjType.HoleError.LocationField) ~
              RETURN()
          }
        }
      ))

    private def HoleField: InstanceField =
      InstanceField(this.jvmName, IsPrivate, IsFinal, "hole", BackendObjType.String.toTpe)

    private def LocationField: InstanceField =
      InstanceField(this.jvmName, IsPrivate, IsFinal, "location", BackendObjType.ReifiedSourceLocation.toTpe)

    private def EqualsMethod: InstanceMethod = BackendObjType.JavaObject.EqualsMethod.implementation(BackendObjType.HoleError.jvmName, Some(
      withName(1, BackendObjType.JavaObject.toTpe) { other =>
        // check exact equality
        thisLoad() ~ other.load() ~
          ifTrue(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
          // check for null
          other.load() ~
          ifTrue(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
          // check for class equality
          thisLoad() ~
          INVOKEVIRTUAL(BackendObjType.JavaObject.GetClassMethod) ~
          other.load() ~
          INVOKEVIRTUAL(BackendObjType.JavaObject.GetClassMethod) ~
          ifTrue(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
          // cast the other obj
          other.load() ~ CHECKCAST(BackendObjType.HoleError.jvmName) ~
          storeWithName(2, BackendObjType.HoleError.toTpe) { otherHoleError =>
            // compare the hole field
            thisLoad() ~ GETFIELD(BackendObjType.HoleError.HoleField) ~
              otherHoleError.load() ~ GETFIELD(BackendObjType.HoleError.HoleField) ~
              objectsEquals() ~
              ifTrue(Condition.EQ)(pushBool(false) ~ IRETURN()) ~
              // compare the location field
              thisLoad() ~ GETFIELD(BackendObjType.HoleError.LocationField) ~
              otherHoleError.load() ~ GETFIELD(BackendObjType.HoleError.LocationField) ~
              objectsEquals() ~
              IRETURN()
          }
      }
    ))

    private def HashCodeMethod: InstanceMethod = BackendObjType.JavaObject.HashcodeMethod.implementation(BackendObjType.HoleError.jvmName, Some(
      ICONST_2() ~
        ANEWARRAY(BackendObjType.JavaObject.jvmName) ~
        // store hole
        DUP() ~
        ICONST_0() ~
        thisLoad() ~ GETFIELD(BackendObjType.HoleError.HoleField) ~
        AASTORE() ~
        // store location
        DUP() ~
        ICONST_1() ~
        thisLoad() ~ GETFIELD(BackendObjType.HoleError.LocationField) ~
        AASTORE() ~
        // hash the array
        INVOKESTATIC(JvmName.Objects, "hash", mkDescriptor(BackendType.Array(BackendObjType.JavaObject.toTpe))(BackendType.Int32)) ~
        IRETURN()
    ))

    private def stringBuilderAppend(): InstructionSet = INVOKEVIRTUAL(JvmName.StringBuilder, "append",
      mkDescriptor(BackendObjType.String.toTpe)(JvmName.StringBuilder.toTpe))

    private def objectsEquals(): InstructionSet = INVOKESTATIC(JvmName.Objects, "equals",
      mkDescriptor(BackendObjType.JavaObject.toTpe, BackendObjType.JavaObject.toTpe)(BackendType.Bool))
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
        NEW(JvmName.StringBuilder) ~
        DUP() ~ invokeConstructor(JvmName.StringBuilder) ~
        pushString("Non-exhaustive match at ") ~
        INVOKEVIRTUAL(JvmName.StringBuilder, "append", mkDescriptor(String.toTpe)(JvmName.StringBuilder.toTpe)) ~
        ALOAD(1) ~ INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKEVIRTUAL(JvmName.StringBuilder, "append", mkDescriptor(String.toTpe)(JvmName.StringBuilder.toTpe)) ~
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKESPECIAL(FlixError.Constructor) ~
        thisLoad() ~
        ALOAD(1) ~
        PUTFIELD(MatchError.LocationField) ~
        RETURN()
    ))

    def LocationField: InstanceField = InstanceField(this.jvmName, IsPublic, IsFinal, "location", ReifiedSourceLocation.toTpe)

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
              INVOKESTATIC(JvmName.Objects, "equals", mkDescriptor(JavaObject.toTpe, JavaObject.toTpe)(BackendType.Bool)) ~
              IRETURN()
          }
      }
    ))

    private def HashCodeMethod: InstanceMethod = JavaObject.HashcodeMethod.implementation(this.jvmName, Some(
      ICONST_1() ~ ANEWARRAY(JavaObject.jvmName) ~
        DUP() ~ ICONST_0() ~ thisLoad() ~ GETFIELD(LocationField) ~ AASTORE() ~
        INVOKESTATIC(JvmName.Objects, "hash", mkDescriptor(BackendType.Array(JavaObject.toTpe))(BackendType.Int32)) ~
        IRETURN()
    ))
  }

  //
  // Java Types
  //

  case object JavaObject extends BackendObjType {

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, None)

    def EqualsMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "equals",
      mkDescriptor(BackendObjType.JavaObject.toTpe)(BackendType.Bool), None)

    def HashcodeMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "hashCode",
      mkDescriptor()(BackendType.Int32), None)

    def ToStringMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "toString",
      mkDescriptor()(BackendObjType.String.toTpe), None)

    def GetClassMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "getClass",
      mkDescriptor()(JvmName.Class.toTpe), None)
  }
}
