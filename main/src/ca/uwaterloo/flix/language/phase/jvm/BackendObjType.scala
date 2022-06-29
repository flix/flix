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
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkStaticConstructor(StaticConstructor)
      cm.mkConstructor(Constructor)
      cm.mkField(InstanceField)

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

    def InstanceField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, "INSTANCE", this.toTpe)
  }

  case object BigInt extends BackendObjType

  case object String extends BackendObjType

  case class Channel(tpe: BackendType) extends BackendObjType

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

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "value", tpe)
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType

  //case class Enum(sym: Symbol.EnumSym, args: List[BackendType]) extends BackendObjType

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.jvmName, superClass = continuation.jvmName)

      cm.mkConstructor(Constructor)
      args.indices.foreach(argIndex => cm.mkField(ArgField(argIndex)))

      cm.closeClassMaker()
    }

    def continuation: Continuation = Continuation(result.toErased)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(continuation.Constructor) ~ RETURN()
    ))

    def ArgField(index: Int): InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, s"arg$index", args(index))
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

    def ResultField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "result", result)

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

    def InstanceField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, "INSTANCE", this.toTpe)

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(
      throwUnsupportedOperationException(
        s"${Record.LookupFieldMethod.name} method shouldn't be called")
    ))

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(
      throwUnsupportedOperationException(
        s"${Record.RestrictFieldMethod.name} method shouldn't be called")
    ))
  }

  case class RecordExtend(field: String, value: BackendType, rest: BackendType) extends BackendObjType {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(this.interface.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkField(LabelField)
      cm.mkField(ValueField)
      cm.mkField(RestField)
      cm.mkMethod(LookupFieldMethod)
      cm.mkMethod(RestrictFieldMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~ RETURN()
    ))

    def interface: Record.type = Record

    def LabelField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "label", String.toTpe)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "value", value)

    def RestField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, "rest", interface.toTpe)

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(
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

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad() ~ GETFIELD(RestField) ~ ARETURN()
        case FalseBranch =>
          thisLoad() ~
            DUP() ~ GETFIELD(RestField) ~
            ALOAD(1) ~
            INVOKEINTERFACE(Record.RestrictFieldMethod) ~
            PUTFIELD(RestField) ~
            thisLoad() ~ ARETURN()
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

      cm.closeClassMaker()
    }

    def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "lookupField",
      mkDescriptor(String.toTpe)(this.toTpe))

    def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "restrictField",
      mkDescriptor(String.toTpe)(this.toTpe))
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
        thisLoad() ~ invokeConstructor(JavaObject.jvmName) ~
          thisLoad() ~ ALOAD(1) ~ PUTFIELD(SourceField) ~
          thisLoad() ~ ILOAD(2) ~ PUTFIELD(BeginLineField) ~
          thisLoad() ~ ILOAD(3) ~ PUTFIELD(BeginColField) ~
          thisLoad() ~ ILOAD(4) ~ PUTFIELD(EndLineField) ~
          thisLoad() ~ ILOAD(5) ~ PUTFIELD(EndColField) ~
          RETURN()
      ))

    def SourceField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "source", String.toTpe)

    def BeginLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "beginLine", BackendType.Int32)

    def BeginColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "beginCol", BackendType.Int32)

    def EndLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "endLine", BackendType.Int32)

    def EndColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, "endCol", BackendType.Int32)

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(
      // create string builder
      NEW(JvmName.StringBuilder) ~ DUP() ~ invokeConstructor(JvmName.StringBuilder) ~
        // build string
        thisLoad() ~ GETFIELD(SourceField) ~ appendString() ~
        pushString(":") ~ appendString() ~
        thisLoad() ~ GETFIELD(BeginLineField) ~ appendInt() ~
        pushString(":") ~ appendString() ~
        thisLoad() ~ GETFIELD(BeginColField) ~ appendInt() ~
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
              INVOKESTATIC(JvmName.Objects, "equals", mkDescriptor(JavaObject.toTpe, JavaObject.toTpe)(BackendType.Bool)) ~
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
        INVOKESTATIC(JvmName.Objects, "hash", mkDescriptor(BackendType.Array(JavaObject.toTpe))(BackendType.Int32)) ~
        IRETURN()
    ))

    private def boxInt(): InstructionSet = INVOKESTATIC(JvmName.Integer, "valueOf",
      mkDescriptor(BackendType.Int32)(JvmName.Integer.toTpe))

    private def appendString(): InstructionSet = INVOKEVIRTUAL(JvmName.StringBuilder, "append",
      mkDescriptor(String.toTpe)(JvmName.StringBuilder.toTpe))

    private def appendInt(): InstructionSet = INVOKEVIRTUAL(JvmName.StringBuilder, "append",
      mkDescriptor(BackendType.Int32)(JvmName.StringBuilder.toTpe))
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
        DUP() ~ invokeConstructor(JvmName.AtomicLong) ~
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
      StaticField(this.jvmName, IsPrivate, IsFinal, "counter", JvmName.AtomicLong.toTpe)

    def ArgsField: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, "args", BackendType.Array(String.toTpe))

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
              NEW(JvmName.StringBuilder) ~
              DUP() ~ invokeConstructor(JvmName.StringBuilder) ~
              pushString("Hole '") ~ stringBuilderAppend() ~
              hole.load() ~ stringBuilderAppend() ~
              pushString("' at ") ~ stringBuilderAppend() ~
              loc.load() ~ INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ stringBuilderAppend() ~
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
      InstanceField(this.jvmName, IsPrivate, IsFinal, "hole", String.toTpe)

    private def LocationField: InstanceField =
      InstanceField(this.jvmName, IsPrivate, IsFinal, "location", ReifiedSourceLocation.toTpe)

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
          storeWithName(2, this.toTpe) { otherHoleError =>
            // compare the hole field
            thisLoad() ~ GETFIELD(HoleField) ~
              otherHoleError.load() ~ GETFIELD(HoleField) ~
              objectsEquals() ~
              ifTrue(Condition.EQ)(pushBool(false) ~ IRETURN()) ~
              // compare the location field
              thisLoad() ~ GETFIELD(LocationField) ~
              otherHoleError.load() ~ GETFIELD(LocationField) ~
              objectsEquals() ~
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
        INVOKESTATIC(JvmName.Objects, "hash", mkDescriptor(BackendType.Array(JavaObject.toTpe))(BackendType.Int32)) ~
        IRETURN()
    ))

    private def stringBuilderAppend(): InstructionSet = INVOKEVIRTUAL(JvmName.StringBuilder, "append",
      mkDescriptor(String.toTpe)(JvmName.StringBuilder.toTpe))

    private def objectsEquals(): InstructionSet = INVOKESTATIC(JvmName.Objects, "equals",
      mkDescriptor(JavaObject.toTpe, JavaObject.toTpe)(BackendType.Bool))
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
      mkDescriptor(JavaObject.toTpe)(BackendType.Bool), None)

    def HashcodeMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "hashCode",
      mkDescriptor()(BackendType.Int32), None)

    def ToStringMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "toString",
      mkDescriptor()(String.toTpe), None)

    def GetClassMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "getClass",
      mkDescriptor()(JvmName.Class.toTpe), None)
  }
}
