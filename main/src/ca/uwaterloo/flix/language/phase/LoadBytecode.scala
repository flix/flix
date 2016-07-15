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
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Definition, Expression}
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol, Type}
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.{Evaluation, InternalCompilerException, Options}

object LoadBytecode {

  private class Loader extends ClassLoader {
    def apply(name: String, bytes: Array[Byte]): Class[_] = defineClass(name, bytes, 0, bytes.length)

    def apply(prefix: List[String], bytes: Array[Byte]): Class[_] = apply(prefix.mkString("."), bytes)
  }

  /**
    * Generate bytecode, load the class, and attach references to compiled methods in the AST.
    * There are a number of steps we take before and after the actual code generation.
    *
    * 1. Group constants and transform non-functions.
    *    We group all constants by their prefixes to determine which methods are compiled into which classes. Also, we
    *    transform all non-function constants into 0-arg functions, since codegen only compiles methods.
    *    Example 1: given a root with constants A.B.C/f, A.B/g, A.B.C/h, we want to generate two classes, A.B.C
    *    (containing methods f and h) and A.B (containing method g).
    *    Example 2: (in pseudocode) the constant `def x = UserError` is converted to `def x() = UserError`.
    *
    * 2. Create a declarations map of names to types.
    *    We need to know the type of function f in order to generate code that calls f.
    *
    * 3. Generate functional interfaces.
    *    Our implementation of closures requires the lambda function to be called through an interface (which is
    *    annotated with @FunctionalInterface). Instead of using functional interfaces provided by Java or Scala (which
    *    are too specific or too general), we create our own.
    *    In this step, we generate names for the functional interfaces, placing each interface in the package
    *    ca.uwaterloo.flix.runtime. We iterate over the entire AST to determine which function types are used in
    *    MkClosureRef, remove duplicate types, and then generate names. We want the type of MkClosureRef, which is the
    *    type of the closure, not the type of the lambda, since its underlying implementation method will have its
    *    argument list modified for capture variables.
    *    Note that this includes synthetic functions that were lambda lifted, as well as user-defined functions being
    *    passed around as closures.
    *    Finally, we generate bytecode for each name. We keep the types and names in an interfaces map, so that given
    *    a closure's signature, we can lookup the functional interface it's called through.
    *
    * 4. Generate and load bytecode.
    *    As of this step, we have grouped the constants into separate classes, transformed non-functions into 0-arg
    *    functions, collected all the declarations in a map, and created and loaded functional interfaces and collected
    *    them in a map.
    *    Now, for each class, we generate and load the bytecode. We also initialize the static Flix field to point to
    *    `this`.
    *
    * 5. Load the methods.
    *    For each constant, we use reflection to get the corresponding java.lang.reflect.Method object.
    *    This is actually a bit tricky. We need the rewritten lambda types (non-functions -> 0-arg functions, free
    *    variables eliminated) to perform the reflection lookup, so we iterate over constantsMap. However, we want to
    *    mutate the original constant from root.constants, not the one in constantsMap (which will get GC'd).
    */
  def load(flix: Flix, root: ExecutableAst.Root, options: Options)(implicit genSym: GenSym): ExecutableAst.Root = {
    val t = System.nanoTime()

    if (options.evaluation == Evaluation.Interpreted) {
      return root
    }

    // Create a new classloader for each root we compile.
    val loader = new Loader()

    // 1. Group constants and transform non-functions.
    val constantsMap: Map[List[String], List[Definition.Constant]] = root.constants.values.map { f =>
      f.tpe match {
        case _: Type.Lambda => f
        case t => f.copy(tpe = Type.Lambda(List(), t))
      }
    }.toList.groupBy(_.name.prefix)
    val constantsList: List[Definition.Constant] = constantsMap.values.flatten.toList

    // 2. Create the declarations map.
    val declarations: Map[Symbol.Resolved, Type] = constantsList.map(f => f.name -> f.tpe).toMap

    // 3. Generate functional interfaces.
    val interfaces: Map[Type, List[String]] = generateInterfaceNames(constantsList)
    val loadedInterfaces: Map[Type, Class[_]] = interfaces.map { case (tpe, prefix) =>
      // Use a temporary context with no functions, because the codegen needs the map of interfaces.
      val bytecode = Codegen.compileFunctionalInterface(Codegen.Context(prefix, List.empty, declarations, interfaces))(tpe)
      if (options.debug) {
        dump(prefix, bytecode)
      }
      tpe -> loader(prefix, bytecode)
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 4. Generate and load bytecode.
    val loadedClasses: Map[List[String], Class[_]] = constantsMap.map { case (prefix, consts) =>
      val bytecode = Codegen.compile(Codegen.Context(prefix, consts, declarations, interfaces), options)
      if (options.debug) {
        dump(prefix, bytecode)
      }
      val clazz = loader(prefix, bytecode)
      // Set the flixObject field.
      clazz.getField(Codegen.flixObject).set(null, flix)
      prefix -> clazz
    }.toMap // Despite IDE highlighting, this is actually necessary.

    // 5. Load the methods.
    for ((prefix, consts) <- constantsMap; const <- consts) {
      val clazz = loadedClasses(prefix)
      val argTpes = const.tpe.asInstanceOf[Type.Lambda].args.map(t => toJavaClass(t, loadedInterfaces))
      // Note: Update the original constant in root.constants, not the temporary one in constantsMap!
      root.constants(const.name).method = clazz.getMethod(const.name.suffix, argTpes: _*)
    }

    val e = System.nanoTime() - t
    root.copy(time = root.time.copy(codeGen = e))
  }

  private def dump(path: String, code: Array[Byte]): Unit = Files.write(Paths.get(path), code)

  private def dump(prefix: List[String], code: Array[Byte]): Unit = dump(prefix.mkString("", "$", ".class"), code)

  /**
    * Convert a Flix type `tpe` into a representation of a Java type, i.e. an instance of `Class[_]`.
    * Used for reflection. Note that this method depends on the generated and loaded functional interfaces.
    */
  private def toJavaClass(tpe: Type, interfaces: Map[Type, Class[_]]): Class[_] = tpe match {
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
    case Type.Enum(_, _) => classOf[Value.Tag]
    case Type.Tuple(elms) => classOf[Value.Tuple]
    case Type.Lambda(_, _) => interfaces(tpe)
    case Type.Parametric(_, _) => ??? // TODO: How to handle?
    case Type.FOpt(_) => ??? // TODO
    case Type.FList(_) => ??? // TODO
    case Type.FVec(_) => ??? // TODO
    case Type.FSet(_) => classOf[scala.collection.immutable.Set[AnyRef]]
    case Type.FMap(_, _) => ??? // TODO
    case Type.Predicate(_) => ??? // TODO: How to handle?
    case Type.Unresolved(_) | Type.Abs(_, _) | Type.Any => ??? // TODO: Deprecated
    case Type.Var(_) | Type.Prop => throw InternalCompilerException(s"Value of $tpe should never be compiled.")
    case Type.Tag(_, _, _) => throw InternalCompilerException(s"No corresponding JVM type for $tpe.")
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
      case Expression.Var(ident, o, tpe, loc) => Set.empty
      case Expression.Ref(name, tpe, loc) => Set.empty
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => Set(tpe)
      case Expression.ApplyRef(name, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplyHook(hook, args, tpe, loc) => args.flatMap(visit).toSet
      case Expression.ApplyClosure(exp, args, tpe, loc) => visit(exp) ++ args.flatMap(visit)
      case Expression.Unary(op, exp, tpe, loc) => visit(exp)
      case Expression.Binary(op, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => visit(exp1) ++ visit(exp2) ++ visit(exp3)
      case Expression.Let(ident, offset, exp1, exp2, tpe, loc) => visit(exp1) ++ visit(exp2)
      case Expression.CheckTag(tag, exp, loc) => visit(exp)
      case Expression.GetTagValue(tag, exp, tpe, loc) => visit(exp)
      case Expression.Tag(enum, tag, exp, tpe, loc) => visit(exp)
      case Expression.GetTupleIndex(base, offset, tpe, loc) => visit(base)
      case Expression.Tuple(elms, tpe, loc) => elms.flatMap(visit).toSet
      case Expression.CheckNil(exp, loc) => visit(exp)
      case Expression.CheckCons(exp, loc) => visit(exp)
      case Expression.FSet(elms, tpe, loc) => elms.flatMap(visit).toSet
      case Expression.Existential(params, exp, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e' at ${loc.source.format}.")
      case Expression.Universal(params, exp, loc) =>
        throw InternalCompilerException(s"Unexpected expression: '$e' at ${loc.source.format}.")
      case Expression.UserError(tpe, loc) => Set.empty
      case Expression.MatchError(tpe, loc) => Set.empty
      case Expression.SwitchError(tpe, loc) => Set.empty
    }

    val types = consts.flatMap(x => visit(x.exp)).toSet
    types.map { t =>
      val name = genSym.fresh2("FnItf").name
      val prefix = List("ca", "uwaterloo", "flix", "runtime", name)
      t -> prefix
    }.toMap
  }

}
