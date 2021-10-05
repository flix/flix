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
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, PRefType, PType, RRefType, RType, Symbol}
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualString, VirtualTerminal}
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Validation}

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

    implicit val r: Root = root

    val lazyTypes = getLazyTypes(root.types)

    val refTypes = getRefTypes(root.types)

    val functionTypes = getFunctionTypes(root.types) union lazyNeededFunctions(lazyTypes)

    val closureSyms: Set[Symbol.DefnSym] = root.closures.map(ci => ci.sym)

    val nonClosureFunctions: Set[Symbol.DefnSym] = root.functions.keySet.diff(closureSyms)

    val namespaces = root.functions.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        // Collect all non-law definitions.
        val nonLaws = defs filter {
          case (sym, defn) => SjvmOps.nonLaw(defn) && !closureSyms.contains(sym)
        }
        NamespaceInfo(ns, nonLaws)
    }.toSet

    val defs = getDefs(root.functions, nonClosureFunctions)

    val tupleTypes = getTupleTypes(root.types)

    val allClasses: Map[JvmName, JvmClass] = flix.subphase("CodeGen") {

      if (flix.options.debug) {
        println(PrettyPrinter.Erased.fmtRoot(root).fmt(TerminalContext.AnsiTerminal))
      }

      if (flix.options.debug) {
        val vt = new VirtualTerminal()
        vt << "All seen expressions (a-z):" << VirtualString.Indent << VirtualString.NewLine
        val expressionStrings = root.functions.foldLeft(Set[String]()) { case (set, (_, defn)) => set union collectExpressions(defn.exp) }
        expressionStrings.toList.sorted.zipWithIndex.foreach { case (str, index) =>
          vt << str
          if (index != expressionStrings.size - 1)
            vt << VirtualString.NewLine
        }
        vt << VirtualString.Dedent
        println(vt.fmt(TerminalContext.AnsiTerminal))
      }

      // Generate Classes
      // TODO(JLS): write documentation
      // TODO(JLS): write pseudocode
      // TODO(JLS): filter the base type list to the occurring types in ex. refs
      // TODO(JLS): ParArgs as much as possible
      // TODO(JLS): Add copyright everywhere (copy over old authors from copied files
      // TODO(JLS): Check that aux methods are private
      // TODO(JLS): final/private as much as possible in generated classes

      val mainClass = GenMainClass.gen()

      val namespaceClasses = GenNamespaces.gen(namespaces)

      val continuationInterfaces = GenContinuationInterfaces.gen(functionTypes)

      val functionInterfaces = GenFunctionInterfaces.gen(functionTypes)

      val defClasses = GenDefClasses.gen(defs)

      val closureClasses = GenClosureClasses.gen(root.closures)

      val enumInterfaces = GenEnumInterfaces.gen(root.enums)

      // todo val tagClasses = GenTagClasses.gen(root.enums)

      val tupleClasses = GenTupleClasses.gen(tupleTypes)

      // todo recordInterfaces

      // todo recordEmptyClasses

      // todo recordExtendClasses

      val refClasses = GenRefClasses.gen(refTypes)

      val lazyClasses = GenLazyClasses.gen(lazyTypes)

      val unitClass = GenUnitClass.gen()

      val flixErrorClass = GenFlixErrorClass.gen()

      val rslClass = GenReifiedSourceLocationClass.gen()

      val holeErrorClass = GenHoleErrorClass.gen()

      val matchErrorClass = GenMatchErrorClass.gen()

      val globalCounterClass = GenGlobalCounterClass.gen()

      List(
        mainClass,
        namespaceClasses,
        continuationInterfaces,
        functionInterfaces,
        defClasses,
        closureClasses,
        enumInterfaces,
        tupleClasses,
        refClasses,
        lazyClasses,
        unitClass,
        flixErrorClass,
        rslClass,
        holeErrorClass,
        matchErrorClass,
        globalCounterClass
      ).reduce(_ ++ _)
    }

    //
    // Write each class (and interface) to disk.
    //
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.writeClassFiles && !flix.options.test) {
      flix.subphase("WriteClasses") {
        for ((_, jvmClass) <- allClasses) {
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
      Bootstrap.bootstrap(allClasses, closureSyms)

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
      case ErasedAst.Expression.Closure(_, _, _, _) => Nil
      case ErasedAst.Expression.ApplyClo(exp, args, _, _) => exp :: args
      case ErasedAst.Expression.ApplyDef(_, args, _, _, _) => args
      case ErasedAst.Expression.ApplyCloTail(exp, args, _, _) => exp :: args
      case ErasedAst.Expression.ApplyDefTail(_, args, _, _, _) => args
      case ErasedAst.Expression.ApplySelfTail(_, _, actuals, _, _, _) => actuals
      case ErasedAst.Expression.BoolNot(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Float32Neg(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Float64Neg(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int8Neg(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int8Not(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int16Neg(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int16Not(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int32Neg(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int32Not(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int64Neg(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Int64Not(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.BigIntNeg(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.BigIntNot(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.ObjEqNull(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.ObjNeqNull(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.BoolLogicalOp(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.BoolEquality(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.CharComparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Float32Arithmetic(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Float32Comparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Float64Arithmetic(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Float64Comparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int8Arithmetic(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int16Arithmetic(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int32Arithmetic(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int64Arithmetic(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.BigIntArithmetic(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int8Bitwise(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int16Bitwise(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int32Bitwise(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int64Bitwise(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.BigIntBitwise(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int8Comparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int16Comparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int32Comparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Int64Comparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.BigIntComparison(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.StringConcat(exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.StringEquality(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.IfThenElse(exp1, exp2, exp3, _, _) => exp1 :: exp2 :: exp3 :: Nil
      case ErasedAst.Expression.Branch(exp, branches, _, _) =>
        branches.foldLeft(List[ErasedAst.Expression[_ <: PType]](exp)) { case (list, (_, exp)) => list :+ exp }
      case ErasedAst.Expression.JumpTo(_, _, _) => Nil
      case ErasedAst.Expression.Let(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Is(_, _, exp, _) => exp :: Nil
      case ErasedAst.Expression.Tag(_, _, exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Untag(_, _, exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Index(_, _, _, _) => Nil
      case ErasedAst.Expression.Tuple(elms, _, _) => elms
      case ErasedAst.Expression.RecordEmpty(_, _) => Nil
      case ErasedAst.Expression.RecordSelect(exp, _, _, _) => exp :: Nil
      case ErasedAst.Expression.RecordExtend(_, value, rest, _, _) => value :: rest :: Nil
      case ErasedAst.Expression.RecordRestrict(_, rest, _, _) => rest :: Nil
      case ErasedAst.Expression.ArrayLit(elms, _, _) => elms
      case ErasedAst.Expression.ArrayNew(elm, len, _, _) => elm :: len :: Nil
      case ErasedAst.Expression.ArrayLoad(base, index, _, _) => base :: index :: Nil
      case ErasedAst.Expression.ArrayStore(base, index, elm, _, _) => base :: index :: elm :: Nil
      case ErasedAst.Expression.ArrayLength(base, _, _) => base :: Nil
      case ErasedAst.Expression.ArraySlice(base, beginIndex, endIndex, _, _) => base :: beginIndex :: endIndex :: Nil
      case ErasedAst.Expression.Ref(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Deref(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Assign(exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.Cast(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.TryCatch(exp, rules, _, _) => exp :: rules.map(rule => rule.exp)
      case ErasedAst.Expression.InvokeConstructor(_, args, _, _) => args
      case ErasedAst.Expression.InvokeMethod(_, exp, args, _, _) => exp :: args
      case ErasedAst.Expression.InvokeStaticMethod(_, args, _, _) => args
      case ErasedAst.Expression.GetField(_, exp, _, _) => exp :: Nil
      case ErasedAst.Expression.PutField(_, exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.GetStaticField(_, _, _) => Nil
      case ErasedAst.Expression.PutStaticField(_, exp, _, _) => exp :: Nil
      case ErasedAst.Expression.NewChannel(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.GetChannel(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.PutChannel(exp1, exp2, _, _) => exp1 :: exp2 :: Nil
      case ErasedAst.Expression.SelectChannel(rules, default, _, _) =>
        val baseList = rules.flatMap(rule => List[ErasedAst.Expression[_ <: PType]](rule.chan, rule.exp))
        default match {
          case Some(value) => baseList :+ value
          case None => baseList
        }
      case ErasedAst.Expression.Spawn(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Lazy(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.Force(exp, _, _) => exp :: Nil
      case ErasedAst.Expression.HoleError(_, _, _) => Nil
      case ErasedAst.Expression.MatchError(_, _) => Nil
      case ErasedAst.Expression.BoxBool(exp, _) => exp :: Nil
      case ErasedAst.Expression.BoxInt8(exp, _) => exp :: Nil
      case ErasedAst.Expression.BoxInt16(exp, _) => exp :: Nil
      case ErasedAst.Expression.BoxInt32(exp, _) => exp :: Nil
      case ErasedAst.Expression.BoxInt64(exp, _) => exp :: Nil
      case ErasedAst.Expression.BoxChar(exp, _) => exp :: Nil
      case ErasedAst.Expression.BoxFloat32(exp, _) => exp :: Nil
      case ErasedAst.Expression.BoxFloat64(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxBool(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxInt8(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxInt16(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxInt32(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxInt64(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxChar(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxFloat32(exp, _) => exp :: Nil
      case ErasedAst.Expression.UnboxFloat64(exp, _) => exp :: Nil
    }
    recursiveCalls.foldLeft(Set[String](exp.getClass.getSimpleName))((set, exp) => set union collectExpressions(exp))
  }

  // all function types of the program
  private def getFunctionTypes(types: Set[RType[_ <: PType]]): Set[RType[PReference[PFunction[_ <: PType]]]] = {
    def innerMatch[T <: PRefType](tpe: RRefType[T], setAcc: Set[RType[PReference[PFunction[_ <: PType]]]]): Set[RType[PReference[PFunction[_ <: PType]]]] = tpe match {
      case res@RArrow(_, _) => setAcc + res.rType.asInstanceOf[RType[PReference[PFunction[_ <: PType]]]]
      case _ => setAcc
    }

    val init = Set.empty[RType[PReference[PFunction[_ <: PType]]]]
    types.foldLeft(init) { (setAcc, rType) =>
      rType match {
        case RReference(referenceType) => innerMatch(referenceType, setAcc)
        case _ => setAcc
      }
    }
  }

  // definitions that are not laws and not closures
  private def getDefs(value: Map[Symbol.DefnSym, ErasedAst.Def[_ <: PType]], nonClosureFunctions: Set[Symbol.DefnSym]): Map[Symbol.DefnSym, ErasedAst.Def[_ <: PType]] = {
    value.filter {
      case (defnSym, defn) => SjvmOps.nonLaw(defn) && nonClosureFunctions.contains(defnSym)
    }
  }

  // erased element types in lazy values
  private def getLazyTypes(tpes: Set[RType[_ <: PType]]): Set[RType[_ <: PType]] = {
    def innerMatch[T <: PRefType](tpe: RRefType[T], setAcc: Set[RType[_ <: PType]]): Set[RType[_ <: PType]] = tpe match {
      case RLazy(elmType) => setAcc + elmType.erasedType
      case _ => setAcc
    }

    val init = Set.empty[RType[_ <: PType]]
    tpes.foldLeft(init) {
      case (setAcc, tpe) => tpe match {
        case RReference(referenceType) => innerMatch(referenceType, setAcc)
        case _ => setAcc
      }
    }
  }

  // erased element types in ref values
  private def getRefTypes(tpes: Set[RType[_ <: PType]]): Set[RType[_ <: PType]] = {
    def innerMatch[T <: PRefType](tpe: RRefType[T], setAcc: Set[RType[_ <: PType]]): Set[RType[_ <: PType]] = tpe match {
      case RRef(elmType) => setAcc + elmType.erasedType
      case _ => setAcc
    }

    val init = Set.empty[RType[_ <: PType]]
    tpes.foldLeft(init) {
      case (setAcc, tpe) => tpe match {
        case RReference(referenceType) => innerMatch(referenceType, setAcc)
        case _ => setAcc
      }
    }
  }

  // function types needed in the lazy classes
  private def lazyNeededFunctions(tpes: Set[RType[_ <: PType]]): Set[RType[PReference[PFunction[_ <: PType]]]] = {
    tpes.map(tpe => RArrow(List(RObject.rType), tpe).rType.asInstanceOf[RType[PReference[PFunction[_ <: PType]]]])
  }

  // erased tuples in the program
  private def getTupleTypes(types: Set[RType[_ <: PType]]): Set[RType[PReference[PTuple]]] = {
    def erase(lst: List[RType[_ <: PType]]): List[RType[_ <: PType]] =
      lst.map(tpe => tpe.erasedType)

    def innerMatch[T <: PRefType](tpe: RRefType[T], setAcc: Set[RType[PReference[PTuple]]]): Set[RType[PReference[PTuple]]] = tpe match {
      case RTuple(elms) => setAcc + RTuple(erase(elms)).rType.asInstanceOf[RType[PReference[PTuple]]]
      case _ => setAcc
    }

    types.foldLeft(Set[RType[PReference[PTuple]]]()) {
      case (setAcc, tpe) => tpe match {
        case RReference(referenceType) => innerMatch(referenceType, setAcc)
        case _ => setAcc
      }
    }
  }

  /**
    * Optionally returns a reference to main.
    */
  private def getCompiledMain(root: Root)(implicit flix: Flix): Option[Array[String] => Int] = {
    root.functions.get(Symbol.Main) map { defn =>
      (actualArgs: Array[String]) => {
        val args: Array[AnyRef] = Array(actualArgs)
        val result = link(defn.sym, root).apply(args)
        result.asInstanceOf[Integer].intValue()
      }
    }
  }

  /**
    * Returns a map from definition symbols to executable functions (backed by JVM backend).
    */
  private def getCompiledDefs(root: Root)(implicit flix: Flix): Map[Symbol.DefnSym, () => AnyRef] = {
    root.functions.foldLeft(Map.empty[Symbol.DefnSym, () => AnyRef]) {
      case (macc, (sym, _)) =>
        val args: Array[AnyRef] = Array(null)
        macc + (sym -> (() => link(sym, root).apply(args)))
    }
  }

  /**
    * Returns a function object for the given definition symbol `sym`.
    */
  private def link(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], AnyRef] = {
    (args: Array[AnyRef]) => {
      ///
      /// Retrieve the definition and its type.
      ///
      val defn = root.functions(sym)

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
        result
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }
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
    val path = prefixPath.resolve(clazz.name.path).toAbsolutePath

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
