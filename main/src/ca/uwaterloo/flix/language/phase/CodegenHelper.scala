/*
 * Copyright 2015-2017 Ramin Zarifi
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Definition, Expression}
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.ast.{Type, _}
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{Type => _, _}

object CodegenHelper {

  val fixedPrefix = List("ca", "waterloo", "flix", "enums")

  case class QualName(ref: List[String])

  def getEnumCaseName(sym: EnumSym, tag: String, tpe: Option[Type]) : QualName = tpe match {
    case Some(t) if t.isPrimitive => QualName(CodegenHelper.fixedPrefix ++ sym.namespace ++ List(sym.name, t.toString, tag))
    case _ => QualName(CodegenHelper.fixedPrefix ++ sym.namespace ++ List(sym.name, "object", tag))
  }

  def getEnumInterfaceName(sym: EnumSym): QualName = {
    QualName(CodegenHelper.fixedPrefix ++ sym.namespace ++ List(sym.name, "EnumInterface"))
  }

  /*
   * Decorate (mangle) a prefix (list of strings) to get the internal JVM name.
   */
  def decorate(prefix: List[String]): String = prefix.mkString("/")

  def decorate(qualName: QualName): String = decorate(qualName.ref)

  /*
   * Returns the internal name of the JVM type that `tpe` maps to.
   *
   * The descriptor of a type must be associated with a Context, because the descriptor of a closure object (i.e. a
   * lambda function and not a JVM method) is the descriptor of a generated interface.
   *
   * The inner function may seem weird at first, but we need it because Type.Lambda has two different descriptors
   * depending on how it's used. If we're compiling a method that takes two ints and returns an int,
   * (i.e. its type is Type.Lambda(List(Int, Int), Int)) we need to generate the JVM descriptor `(II)I`.
   * On the other hand, if that same method is being passed as a closure, we need to use the interface that was
   * generated for that closure, and not its JVM type descriptor. We don't want a type descriptor to look like
   * `((II)I)I`.
   */
  //TODO: We should get rid of `check` when we fully implement enum interfaces
  def descriptor(tpe: Type, interfaces: Map[Type, QualName]): String = {
    def inner(tpe: Type): String = tpe match {
      case Type.Var(id, kind) => throw InternalCompilerException(s"Non-monomorphed type variable '$id in type '$tpe'.")
      case Type.Unit => asm.Type.getDescriptor(Constants.unitClass)
      case Type.Bool => asm.Type.BOOLEAN_TYPE.getDescriptor
      case Type.Char => asm.Type.CHAR_TYPE.getDescriptor
      case Type.Float32 => asm.Type.FLOAT_TYPE.getDescriptor
      case Type.Float64 => asm.Type.DOUBLE_TYPE.getDescriptor
      case Type.Int8 => asm.Type.BYTE_TYPE.getDescriptor
      case Type.Int16 => asm.Type.SHORT_TYPE.getDescriptor
      case Type.Int32 => asm.Type.INT_TYPE.getDescriptor
      case Type.Int64 => asm.Type.LONG_TYPE.getDescriptor
      case Type.BigInt => asm.Type.getDescriptor(Constants.bigIntegerClass)
      case Type.Str => asm.Type.getDescriptor(Constants.stringClass)
      case Type.Native => asm.Type.getDescriptor(Constants.objectClass)
      case Type.Apply(Type.Arrow(l), _) => s"L${decorate(interfaces(tpe))};"
      case Type.Apply(Type.FTuple(l), _) => asm.Type.getDescriptor(Constants.arrayObjectClass)
      case _ if tpe.isEnum =>
        val sym = tpe match {
          case Type.Apply(Type.Enum(s, _), _) => s
          case Type.Enum(s, _) => s
          case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
        }
        val fullName = fixedPrefix ++ sym.namespace ++ List(sym.name, "EnumInterface")
        s"L${decorate(fullName)};"
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
    }

    tpe match {
      case Type.Apply(Type.Arrow(l), ts) => s"(${ts.take(l - 1).map(inner).mkString})${inner(ts.last)}"
      case _ => inner(tpe)
    }
  }

  /*
  * Constants passed to the asm library to generate bytecode.
  *
  * Originally, we simply used string literals (to encode internal class names, descriptors, etc.) and passed them to
  * the library. This was very brittle for two reasons: 1) changing a class or method would break things and we
  * wouldn't know until run time; and 2) multiple string literals would have to be updated.
  *
  * The solution is to use compile-time reflection whenever possible, and move commonly-used constants into this
  * object. Constants that are used only once are left inline.
  *
  * Compile-time reflection gives us compile-time errors if we change a class and don't update here. However, we are
  * stuck with run-time reflection for certain things, e.g. getting a method. So if we change a method and don't
  * update here, we won't know until we run the tests.
  *
  * Note that we can't easily use reflection to get singleton or package objects. (It is possible with run-time
  * reflection, but it is awkward and we don't get the benefit of compile-time errors.)
  *
  * TODO: Refactor the Value object to be Java-like style so reflection is easier. Or use run-time reflection?
  */
  object Constants {
    val objectClass : Class[_] = classOf[Object]
    val stringClass : Class[_] = classOf[java.lang.String]
    val bigIntegerClass : Class[_]= classOf[java.math.BigInteger]
    val arrayObjectClass : Class[_] = classOf[Array[Object]]
    val setClass : Class[_] = classOf[scala.collection.immutable.Set[Object]]
    val flixClass : Class[_] = classOf[Flix]

    val unitClass : Class[_] = Value.Unit.getClass
    val tagClass : Class[_] = classOf[TagInterface]
    val valueObject = "ca/uwaterloo/flix/runtime/Value$"
    val scalaPredef = "scala/Predef$"
    val scalaMathPkg = "scala/math/package$"
    val tagInterface : Class[_] = classOf[TagInterface]

    def loadValueObject(visitor: MethodVisitor): Unit =
      visitor.visitFieldInsn(GETSTATIC, valueObject, "MODULE$", s"L$valueObject;")
  }

  // This constant is used in LoadBytecode, so we can't put it in the private Constants object.
  val flixObject = "flixObject"

  /**
    * Generates all the names of the functional interfaces used in the Flix program.
    */
  def generateInterfaceNames(consts: List[Definition.Constant])(implicit genSym: GenSym): Map[Type, QualName] = {
    def visit(e: Expression): Set[Type] = e match {
      case Expression.Unit => Set.empty
      case Expression.True => Set.empty
      case Expression.False => Set.empty
      case Expression.Char(lit) => Set.empty
      case Expression.Float32(lit) => Set.empty
      case Expression.Float64(lit) => Set.empty
      case Expression.Int8(lit) => Set.empty
      case Expression.Int16(lit) => Set.empty
      case Expression.Int32(lit) => Set.empty
      case Expression.Int64(lit) => Set.empty
      case Expression.BigInt(lit) => Set.empty
      case Expression.Str(lit) => Set.empty
      case Expression.LoadBool(n, o) => Set.empty
      case Expression.LoadInt8(b, o) => Set.empty
      case Expression.LoadInt16(b, o) => Set.empty
      case Expression.LoadInt32(b, o) => Set.empty
      case Expression.StoreBool(b, o, v) => Set.empty
      case Expression.StoreInt8(b, o, v) => Set.empty
      case Expression.StoreInt16(b, o, v) => Set.empty
      case Expression.StoreInt32(b, o, v) => Set.empty
      case Expression.Var(sym, tpe, loc) => Set.empty
      case Expression.Ref(name, tpe, loc) => Set.empty
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => Set(tpe)
      case Expression.ApplyRef(name, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplyTail(name, formals, actuals, tpe, loc) => actuals.flatMap(visit).toSet
      case Expression.ApplyHook(hook, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplyClosure(exp, args, tpe, loc) => visit(exp) ++ args.flatMap(visit)
      case Expression.Unary(op, exp, tpe, loc) => visit(exp)
      case Expression.Binary(op, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => visit(exp1) ++ visit(exp2) ++ visit(exp3)
      case Expression.Let(sym, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.Is(sym, tag, exp, loc) => visit(exp)
      case Expression.Tag(enum, tag, exp, tpe, loc) => visit(exp)
      case Expression.Untag(sym, tag, exp, tpe, loc) => visit(exp)
      case Expression.Index(base, offset, tpe, loc) => visit(base)
      case Expression.Tuple(elms, tpe, loc) => elms.flatMap(visit).toSet
      case Expression.Reference(exp, tpe, loc) => ??? // TODO
      case Expression.Dereference(exp, tpe, loc) => ??? // TODO
      case Expression.Assignment(exp1, exp2, tpe, loc) => ??? // TODO
      case Expression.Existential(params, exp, loc) =>
        ???
      case Expression.Universal(params, exp, loc) =>
        ???
      case Expression.NativeConstructor(constructor, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.NativeField(field, tpe, loc) => Set.empty
      case Expression.NativeMethod(method, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.UserError(tpe, loc) => Set.empty
      case Expression.MatchError(tpe, loc) => Set.empty
      case Expression.SwitchError(tpe, loc) => Set.empty
    }

    val types = consts.flatMap(x => visit(x.exp)).toSet
    types.map { t =>
      val name = Symbol.freshVarSym("FnItf").toString
      val prefix = QualName(List("ca", "uwaterloo", "flix", "runtime", name))
      t -> prefix
    }.toMap
  }


  /**
    * Find enums from the given expression
    * @param e expression
    * @return
    */
  def findEnums(e: Expression): List[(Type, (String, Type))] = e match {
    case Expression.Unit => Nil
    case Expression.True => Nil
    case Expression.False => Nil
    case Expression.Char(lit) => Nil
    case Expression.Float32(lit) => Nil
    case Expression.Float64(lit) => Nil
    case Expression.Int8(lit) => Nil
    case Expression.Int16(lit) => Nil
    case Expression.Int32(lit) => Nil
    case Expression.Int64(lit) => Nil
    case Expression.BigInt(lit) => Nil
    case Expression.Str(lit) => Nil
    case Expression.LoadBool(n, o) => Nil
    case Expression.LoadInt8(b, o) => Nil
    case Expression.LoadInt16(b, o) => Nil
    case Expression.LoadInt32(b, o) => Nil
    case Expression.StoreBool(b, o, v) => Nil
    case Expression.StoreInt8(b, o, v) => Nil
    case Expression.StoreInt16(b, o, v) => Nil
    case Expression.StoreInt32(b, o, v) => Nil
    case Expression.Var(sym, tpe, loc) => Nil
    case Expression.Ref(name, tpe, loc) => Nil
    case Expression.MkClosureRef(ref, freeVars, tpe, loc) => Nil
    case Expression.ApplyRef(name, args, tpe, loc) => args.flatMap(findEnums)
    case Expression.ApplyTail(name, formals, actuals, tpe, loc) => actuals.flatMap(findEnums)
    case Expression.ApplyHook(hook, args, tpe, loc) => args.flatMap(findEnums)
    case Expression.ApplyClosure(exp, args, tpe, loc) => findEnums(exp) ++ args.flatMap(findEnums)
    case Expression.Unary(op, exp, tpe, loc) => findEnums(exp)
    case Expression.Binary(op, exp1, exp2, tpe, loc) => findEnums(exp1) ++ findEnums(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => findEnums(exp1) ++ findEnums(exp2) ++ findEnums(exp3)
    case Expression.Let(sym, exp1, exp2, tpe, loc) => findEnums(exp1) ++ findEnums(exp2)
    case Expression.Is(sym, tag, exp, loc) => findEnums(exp)
    case Expression.Tag(enum, tag, exp, tpe, loc) => List((tpe, (tag, exp.tpe))) ++ findEnums(exp)
    case Expression.Untag(sym, tag, exp, tpe, loc)  => List((exp.tpe, (tag, tpe))) ++ findEnums(exp)
    case Expression.Index(base, offset, tpe, loc) => findEnums(base)
    case Expression.Tuple(elms, tpe, loc) => elms.flatMap(findEnums).toList
    case Expression.Reference(exp, tpe, loc) => ??? // TODO
    case Expression.Dereference(exp, tpe, loc) => ??? // TODO
    case Expression.Assignment(exp1, exp2, tpe, loc) => ??? // TODO
    case Expression.Existential(params, exp, loc) => findEnums(exp)
    case Expression.Universal(params, exp, loc) => findEnums(exp)
    case Expression.NativeConstructor(constructor, args, tpe, loc) => args.flatMap(findEnums)
    case Expression.NativeField(field, tpe, loc) => Nil
    case Expression.NativeMethod(method, args, tpe, loc) => args.flatMap(findEnums)
    case Expression.UserError(tpe, loc) => Nil
    case Expression.MatchError(tpe, loc) => Nil
    case Expression.SwitchError(tpe, loc) => Nil
  }
}
