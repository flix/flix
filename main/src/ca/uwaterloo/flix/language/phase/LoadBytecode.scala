/*
 * Copyright 2015-2016 Ming-Ho Yee
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

import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.api.{Flix, Unit => UnitClass}
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Type}
import ca.uwaterloo.flix.language.phase.CodegenHelper._
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
object LoadBytecode extends Phase[ExecutableAst.Root, ExecutableAst.Root] {

  private class Loader extends ClassLoader {
    def apply(name: String, bytes: Array[Byte]): Class[_] = defineClass(name, bytes, 0, bytes.length)

    def apply(qualName: QualName, bytes: Array[Byte]): Class[_] = apply(qualName.ref.mkString("."), bytes)
  }

  /**
    * Load classes and interfaces and attach references to compiled methods in the AST.
    * The procedure of loading bytecodes is as follows:
    *
    * 1. Group constants and transform non-functions.
    * We group all constants by their prefixes to determine which methods are compiled into which classes. Also, we
    * transform all non-function constants into 0-arg functions, since codegen only compiles methods.
    * Example 1: given a root with constants A.B.C/f, A.B/g, A.B.C/h, we want to generate two classes, A.B.C
    * (containing methods f and h) and A.B (containing method g).
    * Example 2: (in pseudocode) the constant `def x = UserError` is converted to `def x() = UserError`.
    *
    * 2. Create Enum Type info
    * We create type information for each enum case. That is, given the type, the enumTypeInfo returns a map from tag of a case
    * to an `EnumCaseInfo` object which holds information such as QualifiedName, ExecutableAst case and the type of the
    * field of the enum case.
    *
    * 3. Load Tuple classes
    * We load tuple classes which are generated in TupleGen phase. if debug option is set, we dump the bytecode as well.
    *
    * 4. Load Enum interfaces
    * We load enum interfaces which are generated in EnumGen phase. if debug option is set, we dump the bytecode as well.
    *
    * 5. Load functional interfaces.
    * We load functional interfaces which are generated at CodeGen. if debug option is set, we dump the bytecode as well.
    *
    * 6. Load Enum classes
    * We load enum classes which are generated at CodeGen. if debug option is set, we dump the bytecode as well.
    *
    * 7. Load bytecodes of flix functions
    * We load classes which include flix expressions which are generated at CodeGen. if debug option is set,
    * we dump the bytecode as well. We also set the `flix` field of these classes to flix object.
    *
    * 8. Load the methods.
    * For each constant, we use reflection to get the corresponding java.lang.reflect.Method object.
    * This is actually a bit tricky. We need the rewritten lambda types (non-functions -> 0-arg functions, free
    * variables eliminated) to perform the reflection lookup, so we iterate over constantsMap. However, we want to
    * mutate the original constant from root.constants, not the one in constantsMap (which will get GC'd).
    */
  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val t = System.nanoTime()

    if (flix.options.evaluation == Evaluation.Interpreted) {
      return root.toSuccess
    }

    val loader = new Loader()

    // 1. Group constants and transform non-functions.
    val constantsMap: Map[QualName, List[ExecutableAst.Def]] = root.defs.values.map { f =>
      f.tpe match {
        case Type.Apply(Type.Arrow(l), _) => f
        case t => f.copy(tpe = Type.mkArrow(List(), t))
      }
    }.toList.groupBy(cst => FlixClassName(cst.sym.prefix))

    // 2. Create Enum Type info
    val enumTypeInfo: Map[Type, Map[String, EnumCaseInfo]] = generateEnumTypeInfoMap(root)

    // 3. Load Tuple classes
    val loadedTuples: Map[QualName, Class[_]] = root.byteCodes.tupleByteCode.map{ case (name, byteCode) =>
      if(flix.options.debug){
        dump(name, byteCode)
      }
      name -> loader(name, byteCode)
    }.toMap

    // 4. Load Enum interfaces
    val loadedEnumInterfaces : Map[EnumSym, Class[_]] = root.byteCodes.enumInterfaceByteCodes.map{ case (sym, (name, byteCode)) =>
      if(flix.options.debug){
        dump(name, byteCode)
      }
      sym -> loader(name, byteCode)
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 5. Load functional interfaces.
    val loadedInterfaces: Map[Type, Class[_]] = root.byteCodes.functionalInterfaceByteCodes.map { case (tpe, (prefix, bytecode)) =>
      if(flix.options.debug) {
        dump(prefix, bytecode)
      }
      tpe -> loader(prefix, bytecode)
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 6. Load Enum classes
    val loadedEnums = root.byteCodes.enumClassByteCodes.map{ case (sym, (prims, objs)) =>
      val loadedPrimEnums : Map[(String, Type), Class[_]] = prims.map{ case ((name, tpe), byteCode) =>
        val qualName: QualName = EnumClassName(sym, name, WrappedPrimitive(tpe))
        if(flix.options.debug){
          dump(qualName, byteCode)
        }
        (name, tpe)  -> loader(qualName, byteCode)
      }.toMap // Despite IDE highlighting, this is actually necessary.
      val loadedObjEnums : Map[String, Class[_]] = objs.map{ case (name, byteCode) =>
        val fullName = EnumClassName(sym, name, WrappedNonPrimitives(Set()))
        if(flix.options.debug){
          dump(fullName, byteCode)
        }
        name -> loader(fullName, byteCode)
      }.toMap // Despite IDE highlighting, this is actually necessary.
      sym -> (loadedPrimEnums, loadedObjEnums)
    }

    // 7. Load bytecodes of flix functions
    val loadedClasses: Map[QualName, Class[_]] = root.byteCodes.classByteCodes.map { case (prefix, bytecode) =>
      if (flix.options.debug) {
        dump(prefix, bytecode)
      }
      val clazz = loader(prefix, bytecode)
      // Set the flixObject field.
      clazz.getField(CodeGen.flixObject).set(null, flix)
      prefix -> clazz
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 8. Load the methods.
    // TODO: Here we filter laws, since the backend does not support existentials/universals, but could we fix that?
    for ((prefix, consts) <- constantsMap; const <- consts; if !const.ann.isLaw) {
      val Type.Apply(Type.Arrow(l), ts) = const.tpe
      val targs = ts.take(l - 1)

      val clazz = loadedClasses(prefix)
      val argTpes = targs.map(t => toJavaClass(t, loadedInterfaces, loadedEnumInterfaces, loadedTuples, enumTypeInfo))
      // Note: Update the original constant in root.constants, not the temporary one in constantsMap!
      root.defs(const.sym).method = clazz.getMethod(const.sym.suffix, argTpes: _*)
    }

    val e = System.nanoTime() - t
    root.copy(time = root.time.copy(loadByteCode = e)).toSuccess
  }

  private def dump(qualName: QualName, code: Array[Byte]): Unit = Files.write(Paths.get(qualName.ref.mkString("", "$", ".class")), code)

  /**
    * TODO: REMOVE THIS.
    * Convert a Flix type `tpe` into a representation of a Java type, i.e. an instance of `Class[_]`.
    * Used for reflection. Note that this method depends on the generated and loaded functional interfaces.
    */
  private def toJavaClass(tpe: Type,
                          interfaces: Map[Type, Class[_]],
                          enums: Map[EnumSym, Class[_]],
                          loadedTuples: Map[QualName, Class[_]],
                          enumTypeInfo: Map[Type, Map[String, EnumCaseInfo]]): Class[_] = tpe match {
    case Type.Var(id, kind) => classOf[java.lang.Object]
    case Type.Unit => classOf[UnitClass]
    case Type.Bool => classOf[Boolean]
    case Type.Char => classOf[Char]
    case Type.Float32 => classOf[Float]
    case Type.Float64 => classOf[Double]
    case Type.Int8 => classOf[Byte]
    case Type.Int16 => classOf[Short]
    case Type.Int32 => classOf[Int]
    case Type.Int64 => classOf[Long]
    case Type.BigInt => classOf[java.math.BigInteger]
    case Type.Str => classOf[java.lang.String]
    case Type.Native => classOf[java.lang.Object]
    case _ if tpe.isEnum && isNullOptimizable(enumTypeInfo, tpe) =>
      // If the enum is null optimizable, then we find the case which doesn't have the Unit field
      val notUnitCase = enumTypeInfo(tpe).filterNot(_._2.fieldType == Type.Unit).head
      // The type of the object will be the type of the case without Unit field
      toJavaClass(notUnitCase._2.fieldType, interfaces, enums, loadedTuples, enumTypeInfo)
    case Type.Enum(s, _) => enums(s)
    case Type.Apply(Type.Enum(s, _), _) => enums(s)
    case Type.Apply(Type.Tuple(_), lst) =>
      val clazzName = TupleClassName(lst.map(typeToWrappedType))
      loadedTuples(clazzName)
    case Type.Apply(Type.Arrow(l), _) => interfaces(tpe)
    case Type.Apply(Type.Ref, List(ts)) => getReferenceClazz(tpe)
    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
  }
}
