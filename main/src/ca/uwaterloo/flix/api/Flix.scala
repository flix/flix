/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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

package ca.uwaterloo.flix.api

import java.nio.file.{Files, Path, Paths}

import ca.uwaterloo.flix.language.ast.Ast.Hook
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.language.{CompilationError, Compiler, GenSym}
import ca.uwaterloo.flix.runtime.quickchecker.QuickChecker
import ca.uwaterloo.flix.runtime.verifier.Verifier
import ca.uwaterloo.flix.runtime.{DeltaSolver, Model, Solver, Value}
import ca.uwaterloo.flix.util.{LocalResource, Options, StreamOps, Validation}

import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

/**
  * Main programmatic interface for Flix.
  */
class Flix {

  /**
    * A sequence of strings to parsed into Flix ASTs.
    */
  private val strings = ListBuffer.empty[String]

  /**
    * A sequence of paths to be parsed into Flix ASTs.
    */
  private val paths = ListBuffer.empty[Path]

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    */
  private val internals = List(
    "Prelude.flix" -> StreamOps.readAll(LocalResource.Library.Prelude)
  )

  /**
    * A map of hooks to JVM invokable methods.
    */
  private val hooks = mutable.Map.empty[Symbol.DefnSym, Ast.Hook]

  /**
    * The current Flix options.
    */
  private var options = Options.Default

  /**
    * The symbol generator associated with this Flix instance.
    */
  private val genSym = new GenSym()

  /**
    * Adds the given string `s` to the list of strings to be parsed.
    */
  def addStr(s: String): Flix = {
    if (s == null)
      throw new IllegalArgumentException("'s' must be non-null.")
    strings += s
    this
  }

  /**
    * Adds the given path `p` to the list of paths to be parsed.
    */
  def addPath(p: String): Flix = {
    if (p == null)
      throw new IllegalArgumentException("'p' must be non-null.")
    paths += Paths.get(p)
    this
  }

  /**
    * Adds the given path `p` to the list of paths to be parsed.
    */
  def addPath(p: Path): Flix = {
    if (p == null)
      throw new IllegalArgumentException("'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException("'p' must a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException("'p' must a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException("'p' must a readable file.")

    paths += p
    this
  }

  /**
    * Adds the given invokable `inv` with the given `name.`
    *
    * @param fqn the fully qualified name for the invokable.
    * @param tpe the Flix type of the invokable.
    * @param inv the invokable method.
    */
  def addHook(fqn: String, tpe: IType, inv: Invokable): Flix = {
    if (fqn == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (inv == null)
      throw new IllegalArgumentException("'inv' must be non-null.")
    if (tpe == null)
      throw new IllegalArgumentException("'tpe' must be non-null.")
    if (!tpe.isFunction)
      throw new IllegalArgumentException("'tpe' must be a function type.")

    val sym = Symbol.mkDefnSym(fqn)
    val hook = Ast.Hook.Safe(sym, inv, tpe.asInstanceOf[WrappedType].tpe)
    if (hooks contains sym) {
      throw new IllegalArgumentException("Hook already registered for that name.")
    }
    hooks += (sym -> hook)

    this
  }

  /**
    * Adds the given unsafe invokable `inv` with the given `name.`
    *
    * @param fqn the fully qualified name for the invokable.
    * @param tpe the Flix type of the invokable.
    * @param inv the invokable method.
    */
  def addHookUnsafe(fqn: String, tpe: IType, inv: InvokableUnsafe): Flix = {
    if (fqn == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (inv == null)
      throw new IllegalArgumentException("'inv' must be non-null.")
    if (tpe == null)
      throw new IllegalArgumentException("'tpe' must be non-null.")
    if (!tpe.isFunction)
      throw new IllegalArgumentException("'tpe' must be a function type.")

    val sym = Symbol.mkDefnSym(fqn)
    val hook = Ast.Hook.Unsafe(sym, inv, tpe.asInstanceOf[WrappedType].tpe)
    if (hooks contains sym) {
      throw new IllegalArgumentException("Hook already registered for that name.")
    }
    hooks += (sym -> hook)

    this
  }

  /**
    * Calls the invokable with the given name `name`, passing the given `args.`
    *
    * @param fqn  the fully qualified name for the invokable.
    * @param args the array of arguments passed to the invokable.
    */
  def invoke(fqn: String, args: Array[IValue]): IValue = {
    if (fqn == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (args == null)
      throw new IllegalArgumentException("'args' must be non-null.")

    val sym = Symbol.mkDefnSym(fqn)
    hooks.get(sym) match {
      case None => throw new NoSuchElementException(s"Hook '$fqn' does not exist.")
      case Some(_: Hook.Unsafe) => throw new RuntimeException(s"Trying to invoke a safe hook but '$fqn' is an unsafe hook.")
      case Some(hook: Hook.Safe) => hook.inv(args)
    }
  }

  /**
    * Calls the unsafe invokable with the given name `name`, passing the given `args.`
    *
    * @param fqn  the fully qualified name for the invokable.
    * @param args the array of arguments passed to the invokable.
    */
  def invokeUnsafe(fqn: String, args: Array[AnyRef]): AnyRef = {
    if (fqn == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (args == null)
      throw new IllegalArgumentException("'args' must be non-null.")

    val sym = Symbol.mkDefnSym(fqn)
    hooks.get(sym) match {
      case None => throw new NoSuchElementException(s"Hook '$fqn' does not exist.")
      case Some(_: Hook.Safe) => throw new RuntimeException(s"Trying to invoke an unsafe hook but '$fqn' is a safe hook.")
      case Some(hook: Hook.Unsafe) => hook.inv(args)
    }
  }

  /**
    * Sets the options used for this Flix instance.
    */
  def setOptions(opts: Options): Flix = {
    if (opts == null)
      throw new IllegalArgumentException("'opts' must be non-null.")
    options = opts
    this
  }

  /**
    * Compiles the Flix program and returns the typed ast.
    */
  def compile(): Validation[ExecutableAst.Root, CompilationError] = {
    if (strings.isEmpty && paths.isEmpty)
      throw new IllegalStateException("No input specified. Please add at least one string or path input.")

    implicit val _ = genSym

    Compiler.compile(getSourceInputs, hooks.toMap).flatMap {
      case tast =>
        if (options.documentor) {
          Documentor.document(tast)
        }
        val ast = PropertyGen.collectProperties(tast)
        val sast = Simplifier.simplify(ast)
        val lifted = Tailrec.tailrec(LambdaLift.lift(sast))
        val numbered = VarNumbering.number(lifted)
        val east = CreateExecutableAst.toExecutable(numbered)
        val compiled = LoadBytecode.load(this, east, options)
        QuickChecker.quickCheck(compiled, options) flatMap {
          r => Verifier.verify(r, options) map {
            case root => root
          }
        }
    }
  }

  /**
    * Runs the Flix fixed point solver on the program and returns the minimal model.
    */
  def solve(): Validation[Model, CompilationError] = compile().map {
    case root => new Solver(root, options).solve()
  }

  /**
    * Runs the Flix fixed point solver on the program trying to minimize the
    * number of input facts which cause some unhandled exception.
    *
    * @param path the path to write the minimized facts to.
    */
  def deltaSolve(path: Path): Validation[Unit, CompilationError] = compile().map {
    case root => DeltaSolver.solve(root, options, path)
  }

  /**
    * Returns statistics about the abstract syntax tree.
    */
  def astStats(): Validation[AstStats, CompilationError] = compile() map {
    case root => AstStats.statsOf(root)
  }

  /**
    * Returns a list of source inputs constructed
    * from the strings and paths in this builder.
    */
  private def getSourceInputs: List[SourceInput] = {
    val si1 = strings.foldLeft(List.empty[SourceInput]) {
      case (xs, s) => SourceInput.Str(s) :: xs
    }

    val si2 = paths.foldLeft(List.empty[SourceInput]) {
      case (xs, p) if p.getFileName.toString.endsWith(".flix") => SourceInput.TxtFile(p) :: xs
      case (xs, p) if p.getFileName.toString.endsWith(".flix.zip") => SourceInput.ZipFile(p) :: xs
      case (xs, p) if p.getFileName.toString.endsWith(".flix.gzip") => SourceInput.ZipFile(p) :: xs
      case (_, p) => throw new IllegalStateException(s"Unknown file type '${p.getFileName}'.")
    }

    val si3 = internals.foldLeft(List.empty[SourceInput]) {
      case (xs, (name, text)) => SourceInput.Internal(name, text) :: xs
    }

    si1 ::: si2 ::: si3
  }

  /////////////////////////////////////////////////////////////////////////////
  // Type Constructors                                                       //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the unit type.
    */
  def mkUnitType: IType = new WrappedType(Type.Unit)

  /**
    * Returns the bool type.
    */
  def mkBoolType: IType = new WrappedType(Type.Bool)

  /**
    * Returns the char type.
    */
  def mkCharType: IType = new WrappedType(Type.Char)

  /**
    * Returns the float32 type.
    */
  def mkFloat32Type: IType = new WrappedType(Type.Float32)

  /**
    * Returns the float64 type.
    */
  def mkFloat64Type: IType = new WrappedType(Type.Float64)

  /**
    * Returns the int8 type.
    */
  def mkInt8Type: IType = new WrappedType(Type.Int8)

  /**
    * Returns the int16 type.
    */
  def mkInt16Type: IType = new WrappedType(Type.Int16)

  /**
    * Returns the int32 type.
    */
  def mkInt32Type: IType = new WrappedType(Type.Int32)

  /**
    * Returns the int64 type.
    */
  def mkInt64Type: IType = new WrappedType(Type.Int64)

  /**
    * Returns the BigInt type.
    */
  def mkBigIntType: IType = new WrappedType(Type.BigInt)

  /**
    * Returns the Str type.
    */
  def mkStrType: IType = new WrappedType(Type.Str)

  /**
    * Returns the enum with the given `fqn` and `tags`.
    */
  def mkEnumType(fqn: String, tags: java.util.Map[String, IType]): IType = {
    if (fqn == null)
      throw new IllegalArgumentException("Argument 'enumName' must be non-null.")
    if (tags == null)
      throw new IllegalArgumentException("Argument 'tags' must be non-null.")

    new WrappedType(Type.Enum(Symbol.mkEnumSym(fqn), Kind.Star))
  }

  /**
    * Returns the tuple type of the given `types`.
    */
  def mkTupleType(types: Array[IType]): IType = {
    if (types == null)
      throw new IllegalArgumentException("Argument 'types' must be non-null.")

    val elms = types.toList.map(_.asInstanceOf[WrappedType].tpe)
    new WrappedType(Type.Apply(Type.FTuple(elms.length), elms))
  }

  /**
    * Returns the set type parameterized by the given type `tpe`.
    */
  def mkSetType(tpe: IType): IType = {
    if (tpe == null)
      throw new IllegalArgumentException("Argument 'tpe' must be non-null.")

    new WrappedType(Type.mkFSet(tpe.asInstanceOf[WrappedType].tpe))
  }

  /**
    * Returns the set type parameterized by the given `key` and `value` types.
    */
  def mkMapType(key: IType, value: IType): IType = {
    if (key == null)
      throw new IllegalArgumentException("Argument 'key' must be non-null.")
    if (value == null)
      throw new IllegalArgumentException("Argument 'value' must be non-null.")

    new WrappedType(Type.mkFMap(key.asInstanceOf[WrappedType].tpe, value.asInstanceOf[WrappedType].tpe))
  }

  /**
    * Returns the native type.
    */
  def mkNativeType: IType = {
    new WrappedType(Type.Native)
  }

  /**
    * Returns the function type for the function with the given argument types and return type.
    */
  def mkFunctionType(arguments: Array[IType], returnType: IType): IType = {
    if (arguments == null)
      throw new IllegalArgumentException("Argument 'arguments' must be non-null.")
    if (returnType == null)
      throw new IllegalArgumentException("Argument 'returnType' must be non-null.")

    val args = arguments.toList.map(_.asInstanceOf[WrappedType].tpe)
    val retTpe = returnType.asInstanceOf[WrappedType].tpe
    new WrappedType(Type.mkArrow(args, retTpe))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Value Constructors                                                      //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns the unit value.
    */
  def mkUnit: IValue = new WrappedValue(Value.Unit)

  /**
    * Returns the `true` bool value.
    */
  def mkTrue: IValue = new WrappedValue(Value.True)

  /**
    * Returns the `false` bool value.
    */
  def mkFalse: IValue = new WrappedValue(Value.False)

  /**
    * Returns the bool value corresponding to the given boolean.
    */
  def mkBool(b: Boolean): IValue = new WrappedValue(Value.mkBool(b))

  /**
    * Returns the char value corresponding to the given character.
    */
  def mkChar(c: Char): IValue = new WrappedValue(Value.mkChar(c))

  /**
    * Returns the float32 value corresponding to the given float.
    */
  def mkFloat32(f: Float): IValue = new WrappedValue(Value.mkFloat32(f))

  /**
    * Returns the float32 value corresponding to the given double.
    */
  def mkFloat32(d: Double): IValue = new WrappedValue(Value.mkFloat32(d))

  /**
    * Returns the float64 value corresponding to the given double.
    */
  def mkFloat64(d: Double): IValue = new WrappedValue(Value.mkFloat64(d))

  /**
    * Returns the int8 value corresponding to the given byte.
    */
  def mkInt8(b: Byte): IValue = new WrappedValue(Value.mkInt8(b))

  /**
    * Returns the int8 value corresponding to the given int.
    */
  def mkInt8(i: Int): IValue = new WrappedValue(Value.mkInt8(i))

  /**
    * Returns the int16 value corresponding to the given short.
    */
  def mkInt16(s: Short): IValue = new WrappedValue(Value.mkInt16(s))

  /**
    * Returns the int16 value corresponding to the given int.
    */
  def mkInt16(i: Int): IValue = new WrappedValue(Value.mkInt16(i))

  /**
    * Returns the int32 value corresponding to the given int.
    */
  def mkInt32(i: Int): IValue = new WrappedValue(Value.mkInt32(i))

  /**
    * Returns the int64 value corresponding to the given int.
    */
  def mkInt64(i: Int): IValue = new WrappedValue(Value.mkInt64(i))

  /**
    * Returns the int64 value corresponding to the given long.
    */
  def mkInt64(l: Long): IValue = new WrappedValue(Value.mkInt64(l))

  /**
    * Returns the BigInt value corresponding to the given int.
    */
  def mkBigInt(i: Int): IValue = new WrappedValue(Value.mkBigInt(i))

  /**
    * Returns the BigInt value corresponding to the given long.
    */
  def mkBigInt(l: Long): IValue = new WrappedValue(Value.mkBigInt(l))

  /**
    * Returns the BigInt value corresponding to the given string.
    */
  def mkBigInt(s: String): IValue = new WrappedValue(Value.mkBigInt(s))

  /**
    * Returns the BigInt value corresponding to the given java.math.BigInteger.
    */
  def mkBigInt(o: java.math.BigInteger): IValue = new WrappedValue(Value.mkBigInt(o))

  /**
    * Returns the str value corresponding to the given string.
    */
  def mkStr(s: String): IValue = {
    if (s == null)
      throw new IllegalArgumentException("Argument 's' must be non-null.")

    new WrappedValue(Value.mkStr(s))
  }

  /**
    * Returns the tag with the given `tagName` and `tagValue`.
    */
  def mkTag(tagName: String, tagValue: IValue): IValue = {
    new WrappedValue(Value.mkTag(tagName, tagValue.getUnsafeRef))
  }

  /**
    * Returns the value `None`.
    */
  def mkNone: IValue = mkTag("None", mkUnit)

  /**
    * Returns the value `Some(v)`.
    */
  def mkSome(v: IValue): IValue = {
    if (v == null)
      throw new IllegalArgumentException("Argument 'v' must be non-null.")

    mkTag("Some", v)
  }

  /**
    * Returns the value `Ok(v)`.
    */
  def mkOk(v: IValue): IValue = {
    if (v == null)
      throw new IllegalArgumentException("Argument 'v' must be non-null.")

    mkTag("Ok", v)
  }

  /**
    * Returns the value `Err(v)`.
    */
  def mkErr(v: IValue): IValue = {
    if (v == null)
      throw new IllegalArgumentException("Argument 'v' must be non-null.")

    mkTag("Err", v)
  }

  /**
    * Returns the value `Nil`.
    */
  def mkNil: IValue = mkTag("Nil", mkUnit)

  /**
    * Returns the value `v :: vs`.
    */
  def mkCons(v: IValue, vs: IValue): IValue = {
    if (v == null)
      throw new IllegalArgumentException("Argument 'v' must be non-null.")
    if (vs == null)
      throw new IllegalArgumentException("Argument 'vs' must be non-null.")

    mkTag("Cons", mkTuple(Array(v, vs)))
  }

  /**
    * Returns the tuple corresponding to the given array.
    */
  def mkTuple(tuple: Array[IValue]): IValue = {
    if (tuple == null)
      throw new IllegalArgumentException("Argument 'tuple' must be non-null.")

    new WrappedValue(Value.Tuple(tuple.map(_.getUnsafeRef)))
  }

  /**
    * Returns the opt corresponding to the given Java Optional.
    */
  def mkOpt(o: java.util.Optional[IValue]): IValue = {
    if (o == null)
      throw new IllegalArgumentException("Argument 'o' must be non-null.")

    if (!o.isPresent)
      mkNone
    else
      mkSome(o.get())
  }

  /**
    * Return the opt corresponding to the given Scala Option.
    */
  def mkOpt(o: scala.Option[IValue]): IValue = {
    if (o == null)
      throw new IllegalArgumentException("Argument 'o' must be non-null.")

    o match {
      case None => mkNone
      case Some(v) => mkSome(v)
    }
  }

  /**
    * Returns the list corresponding to the given Java array.
    */
  def mkList(l: Array[IValue]): IValue = {
    if (l == null)
      throw new IllegalArgumentException("Argument 'l' must be non-null.")

    val xs = l.foldRight(mkNil) {
      case (v, vs) => mkCons(v, vs)
    }

    new WrappedValue(xs)
  }

  /**
    * Returns the list corresponding to the given Java list.
    */
  def mkList(l: java.util.List[IValue]): IValue = {
    if (l == null)
      throw new IllegalArgumentException("Argument 'l' must be non-null.")
    import scala.collection.JavaConversions._
    mkList(l.toList.toArray)
  }

  /**
    * Returns the list corresponding to the given Scala seq.
    */
  def mkList(l: scala.Seq[IValue]): IValue = {
    if (l == null)
      throw new IllegalArgumentException("Argument 'l' must be non-null.")
    mkList(l.toArray)
  }

  /**
    * Returns the set corresponding to the given Java set.
    */
  def mkSet(s: java.util.Set[IValue]): IValue = {
    if (s == null)
      throw new IllegalArgumentException("Argument 's' must be non-null.")

    import scala.collection.JavaConversions._
    val set = s.foldLeft(immutable.Set.empty[AnyRef]) {
      case (sacc, v) => sacc + v.getUnsafeRef
    }
    new WrappedValue(Value.mkSet(set))
  }

  /**
    * Returns the set corresponding to the given Scala Set.
    */
  def mkSet(s: immutable.Set[IValue]): IValue = {
    if (s == null)
      throw new IllegalArgumentException("Argument 's' must be non-null.")

    val set = s.foldLeft(immutable.Set.empty[AnyRef]) {
      case (sacc, v) => sacc + v.getUnsafeRef
    }
    new WrappedValue(Value.mkSet(set))
  }

  /**
    * Returns the map corresponding to the given Java map.
    */
  def mkMap(m: java.util.Map[IValue, IValue]): IValue = {
    if (m == null)
      throw new IllegalArgumentException("Argument 'm' must be non-null.")

    import scala.collection.JavaConversions._
    val map = m.foldLeft(immutable.Map.empty[AnyRef, AnyRef]) {
      case (macc, (k, v)) => macc + (k.getUnsafeRef -> v.getUnsafeRef)
    }
    new WrappedValue(Value.mkMap(map))
  }

  /**
    * Returns the map corresponding to the given Scala map.
    */
  def mkMap(m: immutable.Map[IValue, IValue]): IValue = {
    if (m == null)
      throw new IllegalArgumentException("Argument 'm' must be non-null.")

    val map = m.foldLeft(immutable.Map.empty[AnyRef, AnyRef]) {
      case (macc, (k, v)) => macc + (k.getUnsafeRef -> v.getUnsafeRef)
    }
    new WrappedValue(Value.mkMap(map))
  }

  /**
    * Returns the Flix representation of the given native object `o`.
    */
  def mkNative(o: AnyRef): IValue = {
    if (o == null)
      throw new IllegalArgumentException("Argument 'o' must be non-null.")

    new WrappedValue(o)
  }

}
