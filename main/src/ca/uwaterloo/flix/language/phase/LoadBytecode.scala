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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Definition, Expression}
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol, Type}
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import org.objectweb.asm.Opcodes._
object LoadBytecode extends Phase[ExecutableAst.Root, ExecutableAst.Root] {

  private class Loader extends ClassLoader {
    def apply(name: String, bytes: Array[Byte]): Class[_] = defineClass(name, bytes, 0, bytes.length)

    def apply(prefix: List[String], bytes: Array[Byte]): Class[_] = apply(prefix.mkString("."), bytes)
  }

  /**
    * Generate bytecode, load the class, and attach references to compiled methods in the AST.
    * There are a number of steps we take before and after the actual code generation.
    *
    * 1. Group constants and transform non-functions.
    * We group all constants by their prefixes to determine which methods are compiled into which classes. Also, we
    * transform all non-function constants into 0-arg functions, since codegen only compiles methods.
    * Example 1: given a root with constants A.B.C/f, A.B/g, A.B.C/h, we want to generate two classes, A.B.C
    * (containing methods f and h) and A.B (containing method g).
    * Example 2: (in pseudocode) the constant `def x = UserError` is converted to `def x() = UserError`.
    *
    * 2. Create a declarations map of names to types.
    * We need to know the type of function f in order to generate code that calls f.
    *
    * 3. Generate functional interfaces.
    * Our implementation of closures requires the lambda function to be called through an interface (which is
    * annotated with @FunctionalInterface). Instead of using functional interfaces provided by Java or Scala (which
    * are too specific or too general), we create our own.
    * In this step, we generate names for the functional interfaces, placing each interface in the package
    *    ca.uwaterloo.flix.runtime. We iterate over the entire AST to determine which function types are used in
    * MkClosureRef, remove duplicate types, and then generate names. We want the type of MkClosureRef, which is the
    * type of the closure, not the type of the lambda, since its underlying implementation method will have its
    * argument list modified for capture variables.
    * Note that this includes synthetic functions that were lambda lifted, as well as user-defined functions being
    * passed around as closures.
    * Finally, we generate bytecode for each name. We keep the types and names in an interfaces map, so that given
    * a closure's signature, we can lookup the functional interface it's called through.
    *
    * 4. Extract declared enums
    * At this stage we traverse the definitions to extract all enum types. We can't use root.enums since the type
    * of enum's field is not available there.
    *
    * 5. Create tag interface.
    * At this step we generate an interface which declare a single method `getTag()` which returns an string
    * containing the tag that each enum object represents. Every enum will implement this interface
    *
    * 6. Create an interface for each enum type.
    * We generate an interface for each enum type and each enum case will implement this interface. This interface
    * does not contain any method and it extends Tag interface
    *
    * 7. Create a class for each enum case.
    * A class will be generated for each enum case.
    * This class contains `tag` and `value` as its fields.
    * It also includes implementations of methods such as `getTag()` which returns a String containing value of `tag`,
    * `getValue()` which returns `value`, `hashCode()` which returns an Int containing hashCode of the case,
    * `toString()` which returns a String of representation of the case and `equals(o: Object)` which returns `true`
    * if the `o` is of type of enum case and the underlying value of both o and `this` is equal.
    * This class implements the enum interface of its type
    *
    * 8. Generate and load bytecode.
    * As of this step, we have grouped the constants into separate classes, transformed non-functions into 0-arg
    * functions, collected all the declarations in a map, and created and loaded functional interfaces and collected
    * them in a map.
    * Now, for each class, we generate and load the bytecode. We also initialize the static Flix field to point to
    * `this`.
    *
    * 8. Load the methods.
    * For each constant, we use reflection to get the corresponding java.lang.reflect.Method object.
    * This is actually a bit tricky. We need the rewritten lambda types (non-functions -> 0-arg functions, free
    * variables eliminated) to perform the reflection lookup, so we iterate over constantsMap. However, we want to
    * mutate the original constant from root.constants, not the one in constantsMap (which will get GC'd).
    */
  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, CompilationError] = {
    implicit val _ = flix.genSym
    val fixedPrefix = List("ca", "waterloo", "flix", "enums")

    val t = System.nanoTime()

    if (flix.options.evaluation == Evaluation.Interpreted) {
      return root.toSuccess
    }

    // Create a new classloader for each root we compile.
    val loader = new Loader()

    // 1. Group constants and transform non-functions.
    val constantsMap: Map[List[String], List[Definition.Constant]] = root.definitions.values.map { f =>
      f.tpe match {
        case Type.Apply(Type.Arrow(l), _) => f
        case t => f.copy(tpe = Type.mkArrow(List(), t))
      }
    }.toList.groupBy(_.sym.prefix)
    // TODO: Here we filter laws, since the backend does not support existentials/universals, but could we fix that?
    val constantsList: List[Definition.Constant] = constantsMap.values.flatten.toList.filterNot(_.ann.isLaw)

    // 2. Create the declarations map.
    val declarations: Map[Symbol.DefnSym, Type] = constantsList.map(f => f.sym -> f.tpe).toMap

    // 3. Generate functional interfaces.
    val interfaces: Map[Type, List[String]] = generateInterfaceNames(constantsList)
    val loadedInterfaces: Map[Type, Class[_]] = interfaces.map { case (tpe, prefix) =>
      // Use a temporary context with no functions, because the codegen needs the map of interfaces.
      val bytecode = Codegen.compileFunctionalInterface(CodegenHelper.Context(prefix, Nil, declarations,
        interfaces, Map.empty, Map.empty))(tpe)
      if (flix.options.debug) {
        dump(prefix, bytecode)
      }
      tpe -> loader(prefix, bytecode)
    }.toMap // Despite IDE highlighting, this is actually necessary.


    // 4. Extract declared enums.
    val declaredEnums: Map[Type, Set[(String, Type)]] = root.definitions.values
      .flatMap(x => findEnums(x.exp)).groupBy(x => x._1)
      .mapValues(x => x.map(y => y._2).toSet)

    // 5. Create the tag interface
    val tagInterface : Class[_] = {
      val name = fixedPrefix :+ "TagInterface"
      val bytecode = Enumgen.compileTagInterface(name)
      if (flix.options.debug) {
        dump(name, bytecode)
      }
      loader(name, bytecode)
    }

    // 6. Create an interface for each enum type
    val enumInterfaces : Map[Type, Class[_]] = declaredEnums.map{ case (tpe, _) =>
      // Get the enum symbol from the type
      val sym = tpe match {
        case Type.Apply(Type.Enum(s,_), _) => s
        case Type.Enum(s, _) => s
        case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }

      // Path to the class
      val prefix : List[String] = fixedPrefix ++ sym.namespace ++ typeToUniquePath(tpe)
      // Full name
      val fullName : List[String] = prefix ++ List("EnumInterface")
      val bytecode = Enumgen.compileEnumInterface(fullName, tagInterface)
      val enumInterface = loader(fullName, bytecode)

      if (flix.options.debug) {
        dump(fullName, bytecode)
      }
      tpe -> enumInterface
    }.toMap

    // 7. Create a class for each enum case.
    val loadedEnums : Map[Type, Map[String, Class[_]]]  = declaredEnums.map{ case (tpe, cases) =>
      //Get the enum symbol from the type
      val sym = tpe match {
        case Type.Apply(Type.Enum(s, _), _) => s
        case Type.Enum(s, _) => s
        case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
      }

      // Path to the class
      val prefix : List[String] = fixedPrefix ++ sym.namespace ++ typeToUniquePath(tpe)

      // Create a class for each enum case
      val classes : Map[String, Class[_]] = cases.map{ case (enumSubCase, fieldType) =>
        val bytecode = Enumgen.compileEnumClass(prefix :+ enumSubCase, enumInterfaces(tpe),
          CodegenHelper.descriptor(fieldType, interfaces, enumInterfaces, false), fieldType)
        if (flix.options.debug) {
          dump(prefix :+ enumSubCase, bytecode)
        }
        enumSubCase -> loader(prefix :+ enumSubCase, bytecode)
      }.toMap
      tpe -> classes
    }.toMap


    // 8. Generate and load bytecode.
    val loadedClasses: Map[List[String], Class[_]] = constantsMap.map { case (prefix, consts) =>
      val bytecode = Codegen.compile(CodegenHelper.Context(prefix, consts, declarations, interfaces, enumInterfaces, loadedEnums),
        flix.options)
      if (flix.options.debug) {
        dump(prefix, bytecode)
      }
      val clazz = loader(prefix, bytecode)
      // Set the flixObject field.
      clazz.getField(Codegen.flixObject).set(null, flix)
      prefix -> clazz
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 9. Load the methods.
    // TODO: Here we filter laws, since the backend does not support existentials/universals, but could we fix that?
    for ((prefix, consts) <- constantsMap; const <- consts; if !const.ann.isLaw) {
      val Type.Apply(Type.Arrow(l), ts) = const.tpe
      val targs = ts.take(l - 1)

      val clazz = loadedClasses(prefix)
      val argTpes = targs.map(t => toJavaClass(t, loadedInterfaces))
      // Note: Update the original constant in root.constants, not the temporary one in constantsMap!
      root.definitions(const.sym).method = clazz.getMethod(const.sym.suffix, argTpes: _*)
    }

    val e = System.nanoTime() - t
    root.copy(time = root.time.copy(codeGen = e)).toSuccess
  }

  private def dump(path: String, code: Array[Byte]): Unit = Files.write(Paths.get(path), code)

  private def dump(prefix: List[String], code: Array[Byte]): Unit = dump(prefix.mkString("", "$", ".class"), code)

  /**
    * Convert a Flix type `tpe` into a representation of a Java type, i.e. an instance of `Class[_]`.
    * Used for reflection. Note that this method depends on the generated and loaded functional interfaces.
    */
  private def toJavaClass(tpe: Type, interfaces: Map[Type, Class[_]]): Class[_] = tpe match {
    case Type.Var(id, kind) => classOf[java.lang.Object] // TODO: Assumes that generics are boxed.
    case Type.Unit => Value.Unit.getClass
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
    case Type.Enum(_, _) | Type.Apply(Type.Enum(_, _), _) => classOf[Value.Tag]
    case Type.Apply(Type.FTuple(l), _) => classOf[Array[Object]]
    case Type.Apply(Type.Arrow(l), _) => interfaces(tpe)
    case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'.")
  }

  /**
    * Generates all the names of the functional interfaces used in the Flix program.
    */
  private def generateInterfaceNames(consts: List[Definition.Constant])(implicit genSym: GenSym): Map[Type, List[String]] = {
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
      val prefix = List("ca", "uwaterloo", "flix", "runtime", name)
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
    case Expression.Is(sym, tag, exp, loc) => Nil
    case Expression.Tag(enum, tag, exp, tpe, loc) => List((tpe, (tag, exp.tpe))) ++ findEnums(exp)
    case Expression.Untag(sym, tag, exp, tpe, loc)  => Nil
    case Expression.Index(base, offset, tpe, loc) => findEnums(base)
    case Expression.Tuple(elms, tpe, loc) => elms.flatMap(findEnums).toList
    case Expression.Existential(params, exp, loc) => findEnums(exp)
    case Expression.Universal(params, exp, loc) => findEnums(exp)
    case Expression.NativeConstructor(constructor, args, tpe, loc) => args.flatMap(findEnums)
    case Expression.NativeField(field, tpe, loc) => Nil
    case Expression.NativeMethod(method, args, tpe, loc) => args.flatMap(findEnums)
    case Expression.UserError(tpe, loc) => Nil
    case Expression.MatchError(tpe, loc) => Nil
    case Expression.SwitchError(tpe, loc) => Nil
  }

  /** TODO: CHANGE THIS!
    * Type to a string wrapped in a list. This string doesn't contain prohibited characters of the type representation
    * @param tpe type
    * @return
    */
  private def typeToUniquePath(tpe: Type) : List[String] = {
    tpe.toString.filterNot {
      case '[' | ']' | ' ' | ',' | ')' | '(' => true
      case _ => false
    } :: Nil
  }
}
