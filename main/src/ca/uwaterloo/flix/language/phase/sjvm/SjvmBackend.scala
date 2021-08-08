/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType, Symbol}
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualString, VirtualTerminal}
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Validation}
import flix.runtime.ProxyObject

import java.lang.reflect.InvocationTargetException
import java.nio.file.{Files, LinkOption, Path, Paths}

object SjvmBackend extends Phase[Root, CompilationResult] {
  /**
    * The directory where to place the generated class files.
    */
  val TargetDirectory: Path = Paths.get("./target/flix/")

  /**
    * Emits JVM bytecode for the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[CompilationResult, CompilationError] = flix.phase("SjvmBackend") {

    //
    // Put the AST root into implicit scope.
    //
    implicit val r: Root = root

    val allClasses: Map[JvmName, JvmClass] = flix.subphase("CodeGen") {

      if (flix.options.debug) {
        val vt = new VirtualTerminal()
        vt << "All seen expressions (a-z):" << VirtualString.Indent << VirtualString.NewLine
        val expressionStrings = root.defs.foldLeft(Set[String]()) { case (set, (_, defn)) => set union collectExpressions(defn.exp) }
        expressionStrings.toList.sorted.zipWithIndex.foreach { case (str, index) => {
          vt << str
          if (index != expressionStrings.size - 1)
            vt << VirtualString.NewLine
        }
        }
        vt << VirtualString.Dedent
        println(vt.fmt(TerminalContext.AnsiTerminal))
      }

      val functionInterfaces = GenFunctionInterfaces.gen(root.functionTypes)
      val continuationInterfaces = GenContinuationInterfaces.gen()

      //
      // Generate the main class.
      //
      val mainClass = GenMainClass.gen()

      //
      // Compute the set of namespaces in the program.
      val namespaceClasses = GenNamespaces.gen(root.namespaces)

      //
      // Generate references classes.
      //
      val refClasses = GenRefClasses.gen()
      val defClasses = GenDefClasses.gen(root.defs)

      //
      // Generate lazy classes.
      //
      //      val lazyClasses = GenLazyClasses.gen()

      //
      // Collect all the classes and interfaces together.
      //
      List(
        mainClass,
        refClasses,
        namespaceClasses,
        functionInterfaces,
        continuationInterfaces,
        defClasses
        //        lazyClasses
      ).reduce(_ ++ _)
    }

    //
    // Write each class (and interface) to disk.
    //
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.writeClassFiles && !flix.options.test) {
      flix.subphase("WriteClasses") {
        for ((jvmName, jvmClass) <- allClasses) {
          writeClass(TargetDirectory, jvmClass)
        }
      }
    }

    val loadClasses = flix.options.loadClassFiles

    if (!loadClasses) {
      //
      // Do not load any classes.
      //
      new CompilationResult(root, None, Map.empty).toSuccess
    } else {
      //
      // Loads all the generated classes into the JVM and decorates the AST.
      //
      Bootstrap.bootstrap(allClasses)

      //
      // Return the compilation result.
      //
      new CompilationResult(root, getCompiledMain(root), getCompiledDefs(root)).toSuccess
    }
  }

  private def collectExpressions(exp: ErasedAst.Expression[_ <: PType]): Set[String] = {
    val recursiveCalls: List[ErasedAst.Expression[_ <: PType]] = exp match {
      case ErasedAst.Expression.Unit(_) => Nil
      case ErasedAst.Expression.Null(_, _) => Nil
      case ErasedAst.Expression.True(_) => Nil
      case ErasedAst.Expression.False(_) => Nil
      case ErasedAst.Expression.Char(_, _) => Nil
      case ErasedAst.Expression.Float32(_, _) => Nil
      case ErasedAst.Expression.Float64(_, _) => Nil
      case ErasedAst.Expression.Int8(_, _) => Nil
      case ErasedAst.Expression.Int16(_, _) => Nil
      case ErasedAst.Expression.Int32(_, _) => Nil
      case ErasedAst.Expression.Int64(_, _) => Nil
      case ErasedAst.Expression.BigInt(_, _) => Nil
      case ErasedAst.Expression.Str(_, _) => Nil
      case ErasedAst.Expression.Var(_, _, _) => Nil
      case ErasedAst.Expression.Closure(sym, freeVars, tpe, loc) => Nil
      case ErasedAst.Expression.ApplyClo(exp, args, tpe, loc) => exp :: args
      case ErasedAst.Expression.ApplyDef(sym, args, tpe, loc) => args
      case ErasedAst.Expression.ApplyCloTail(exp, args, tpe, loc) => exp :: args
      case ErasedAst.Expression.ApplyDefTail(sym, args, tpe, loc) => args
      case ErasedAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => actuals
      case ErasedAst.Expression.Unary(sop, op, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int16Eq(exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int32Eq(exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => exp1 :: exp2 :: exp3 :: Nil
      case ErasedAst.Expression.Branch(exp, branches, tpe, loc) => branches.foldLeft(List[ErasedAst.Expression[_ <: PType]]()) { case (list, (_, exp)) => list :+ exp }
      case ErasedAst.Expression.JumpTo(sym, tpe, loc) => Nil
      case ErasedAst.Expression.Let(sym, exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Is(sym, tag, exp, loc) => exp :: Nil
      case ErasedAst.Expression.Tag(sym, tag, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Untag(sym, tag, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Index(base, offset, tpe, loc) => Nil
      case ErasedAst.Expression.Tuple(elms, tpe, loc) => elms
      case ErasedAst.Expression.RecordEmpty(tpe, loc) => Nil
      case ErasedAst.Expression.RecordSelect(exp, field, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.RecordExtend(field, value, rest, tpe, loc) => value :: rest :: Nil
      case ErasedAst.Expression.RecordRestrict(field, rest, tpe, loc) => rest :: Nil
      case ErasedAst.Expression.ArrayLit(elms, tpe, loc) => elms
      case ErasedAst.Expression.ArrayNew(elm, len, tpe, loc) => elm :: len :: Nil
      case ErasedAst.Expression.ArrayLoad(base, index, tpe, loc) => base :: index :: Nil
      case ErasedAst.Expression.ArrayStore(base, index, elm, tpe, loc) => base :: index :: elm :: Nil
      case ErasedAst.Expression.ArrayLength(base, tpe, loc) => base :: Nil
      case ErasedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => base :: beginIndex :: endIndex :: Nil
      case ErasedAst.Expression.Ref(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Deref(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Assign(exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Existential(fparam, exp, loc) => exp :: Nil
      case ErasedAst.Expression.Universal(fparam, exp, loc) => exp :: Nil
      case ErasedAst.Expression.Cast(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.TryCatch(exp, rules, tpe, loc) => exp :: rules.map(rule => rule.exp)
      case ErasedAst.Expression.InvokeConstructor(constructor, args, tpe, loc) => args
      case ErasedAst.Expression.InvokeMethod(method, exp, args, tpe, loc) => exp :: args
      case ErasedAst.Expression.InvokeStaticMethod(method, args, tpe, loc) => args
      case ErasedAst.Expression.GetField(field, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.PutField(field, exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.GetStaticField(field, tpe, loc) => Nil
      case ErasedAst.Expression.PutStaticField(field, exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.NewChannel(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.GetChannel(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.PutChannel(exp1, exp2, tpe, loc) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val baseList = rules.flatMap(rule => rule.chan :: rule.exp :: Nil)
        default match {
          case Some(value) => baseList :+ value
          case None => baseList
        }
      case ErasedAst.Expression.Spawn(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Lazy(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.Force(exp, tpe, loc) => exp :: Nil
      case ErasedAst.Expression.HoleError(sym, tpe, loc) => Nil
      case ErasedAst.Expression.MatchError(tpe, loc) => Nil
      case ErasedAst.Expression.BoxInt8(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxInt16(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxInt32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxInt64(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxChar(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxFloat32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.BoxFloat64(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt8(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt16(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxInt64(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxChar(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxFloat32(exp, loc) => exp :: Nil
      case ErasedAst.Expression.UnboxFloat64(exp, loc) => exp :: Nil
    }
    recursiveCalls.foldLeft(Set[String](exp.getClass.getSimpleName))((set, exp) => set union collectExpressions(exp))
  }


  /**
    * Optionally returns a reference to main.
    */
  private def getCompiledMain(root: Root)(implicit flix: Flix): Option[Array[String] => Int] = {
    root.defs.get(Symbol.Main) map { defn =>
      (actualArgs: Array[String]) => {
        val args: Array[AnyRef] = Array(actualArgs)
        val result = link(defn.sym, root).apply(args).getValue
        result.asInstanceOf[Integer].intValue()
      }
    }
  }

  /**
    * Returns a map from definition symbols to executable functions (backed by JVM backend).
    */
  private def getCompiledDefs(root: Root)(implicit flix: Flix): Map[Symbol.DefnSym, () => ProxyObject] = {
    root.defs.foldLeft(Map.empty[Symbol.DefnSym, () => ProxyObject]) {
      case (macc, (sym, defn)) =>
        val args: Array[AnyRef] = Array(null)
        macc + (sym -> (() => link(sym, root).apply(args)))
    }
  }

  /**
    * Returns a function object for the given definition symbol `sym`.
    */
  private def link(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], ProxyObject] = {
    (args: Array[AnyRef]) => {
      ///
      /// Retrieve the definition and its type.
      ///
      val defn = root.defs(sym)

      ///
      /// Construct the arguments array.
      ///
      val argsArray = if (args.isEmpty) Array(null) else args
      if (argsArray.length != defn.method.getParameterCount) {
        throw InternalRuntimeException(s"Expected ${defn.method.getParameterCount} arguments, but got: ${argsArray.length} for method ${defn.method.getName}.")
      }

      ///
      /// Perform the method call using reflection.
      ///
      try {
        // Call the method passing the arguments.
        val result = defn.method.invoke(null, argsArray: _*)

        // Construct a fresh proxy object.
        newProxyObj(result)
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }
  }

  /**
    * Returns a proxy object that wraps the given result value.
    */
  private def newProxyObj[T <: PType](result: AnyRef)(implicit flix: Flix): ProxyObject = {
    // Lookup the Equality method.
    val eq = null

    // Lookup the HashCode method.
    val hash = null

    // Lookup the ToString method.
    val toString = null

    // Create the proxy object.
    ProxyObject.of(result, eq, hash, toString)
  }

  /**
    * Writes the given JVM class `clazz` to a sub path under the given `prefixPath`.
    *
    * For example, if the prefix path is `/tmp/` and the class name is Foo.Bar.Baz
    * then the bytecode is written to the path `/tmp/Foo/Bar/Baz.class` provided
    * that this path either does not exist or is already a JVM class file.
    */
  private def writeClass(prefixPath: Path, clazz: JvmClass): Unit = {
    // Compute the absolute path of the class file to write.
    val path = prefixPath.resolve(clazz.name.toPath).toAbsolutePath

    // Create all parent directories (in case they don't exist).
    Files.createDirectories(path.getParent)

    // Check if the file already exists.
    if (Files.exists(path)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.")
      }

      // Check if the file is writable.
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.")
      }

      // Check that the file is empty or a class file.
      if (!(isEmpty(path) || isClassFile(path))) {
        throw InternalCompilerException(s"Refusing to overwrite non-empty, non-class file: '$path'.")
      }
    }

    // Write the bytecode.
    Files.write(path, clazz.bytecode)
  }

  /**
    * Returns `true` if the given `path` is non-empty (i.e. contains data).
    */
  private def isEmpty(path: Path): Boolean = {
    Files.size(path) == 0L
  }

  /**
    * Returns `true` if the given `path` exists and is a Java Virtual Machine class file.
    */
  private def isClassFile(path: Path): Boolean = {
    if (Files.exists(path) && Files.isReadable(path) && Files.isRegularFile(path)) {
      // Read the first four bytes of the file.
      val is = Files.newInputStream(path)
      val b1 = is.read()
      val b2 = is.read()
      val b3 = is.read()
      val b4 = is.read()
      is.close()

      // Check if the four first bytes match CAFE BABE.
      return b1 == 0xCA && b2 == 0xFE && b3 == 0xBA && b4 == 0xBE
    }
    false
  }

}
