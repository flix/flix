package ca.uwaterloo.flix.api

import java.nio.file.{Files, Path, Paths}

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.runtime.{Value, Invokable, Model, Solver}
import ca.uwaterloo.flix.util.{Options, Validation}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
    * A map of hooks to JVM invokable methods.
    */
  private val hooks = mutable.Map.empty[Name.Resolved, Ast.Hook]

  /**
    * The current Flix options.
    */
  private var options = Options.Default

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
    * @param name the fully qualified name for the invokable.
    * @param tpe  the Flix type of the invokable.
    * @param inv  the invokable method.
    */
  def addHook(name: String, tpe: Type.Lambda, inv: Invokable): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (inv == null)
      throw new IllegalArgumentException("'inv' must be non-null.")
    if (tpe == null)
      throw new IllegalArgumentException("'tpe' must be non-null.")

    val rname = Name.Resolved.mk(name)
    hooks.get(rname) match {
      case None => hooks += (rname -> Ast.Hook(rname, inv, tpe))
      case Some(otherHook) =>
        throw new IllegalStateException(s"Another hook already exists for the given '$name'.")
    }
    this
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
  def compile(): Validation[TypedAst.Root, FlixError] = {
    if (strings.isEmpty && paths.isEmpty)
      throw new IllegalStateException("No input specified. Please add at least one string or path input.")

    Compiler.compile(getSourceInputs, hooks.toMap)
  }

  /**
    * Solves the Flix program and returns the minimal model.
    *
    * NB: Automatically calls `compile()` thus there is no reason to do so manually.
    */
  def solve(): Validation[Model, FlixError] = {
    compile() map {
      case ast => new Solver()(Solver.SolverContext(ast, options)).solve()
    }
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

    si1 ::: si2
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
    * Returns the Str type.
    */
  def mkStrType: IType = new WrappedType(Type.Str)

  /**
    * Returns the enum with the given fully qualified `name` and tags.
    */
  def mkEnumType(name: String, tags: Array[String]): IType = {
    if (name == null) throw new IllegalArgumentException("Argument 'name' must be non-null.")
    if (tags == null) throw new IllegalArgumentException("Argument 'tags' must be non-null.")

    // TODO
    val cases = tags.foldLeft(Map.empty[String, Type.Tag]) {
      case (macc, tag) => macc + (tag -> Type.Tag(???, ???, ???))
    }

    new WrappedType(new Type.Enum(cases))
  }

  /**
    * Returns the tuple type of the given `types`.
    */
  def mkTupleType(types: Array[IType]): IType = {
    if (types == null) throw new IllegalArgumentException("Argument 'types' must be non-null.")
    val elms = types.toList.map(_.asInstanceOf[Type])
    new WrappedType(Type.Tuple(elms))
  }

  /**
    * Returns the opt type parameterized by the given type `tpe`.
    */
  def mkOptType(tpe: IType): IType = {
    if (tpe == null) throw new IllegalArgumentException("Argument 'tpe' must be non-null.")
    new WrappedType(Type.Opt(tpe.asInstanceOf[Type]))
  }

  /**
    * Returns the list type parameterized by the given type `tpe`.
    */
  def mkListType(tpe: IType): IType = {
    if (tpe == null) throw new IllegalArgumentException("Argument 'tpe' must be non-null.")
    new WrappedType(Type.Lst(tpe.asInstanceOf[Type]))
  }

  /**
    * Returns the set type parameterized by the given type `tpe`.
    */
  def mkSetType(tpe: IType): IType = {
    if (tpe == null) throw new IllegalArgumentException("Argument 'tpe' must be non-null.")
    new WrappedType(Type.Set(tpe.asInstanceOf[Type]))
  }

  /**
    * Returns the set type parameterized by the given `key` and `value` types.
    */
  def mkMapType(key: IType, value: IType): IType = {
    if (key == null) throw new IllegalArgumentException("Argument 'key' must be non-null.")
    if (value == null) throw new IllegalArgumentException("Argument 'value' must be non-null.")
    new WrappedType(Type.Map(key.asInstanceOf[Type], value.asInstanceOf[Type]))
  }

  /**
    * Returns the native type for the given fully qualified `name`.
    */
  def mkNative(name: String): IType = {
    if (name == null) throw new IllegalArgumentException("Argument 'name' must be non-null.")
    new WrappedType(Type.Native(name))
  }

  /**
    * Returns the function type for the function with the given argument types and return type.
    */
  def mkFunctionType(arguments: Array[IType], returnType: IType): IType = {
    if (arguments == null) throw new IllegalArgumentException("Argument 'arguments' must be non-null.")
    if (returnType == null) throw new IllegalArgumentException("Argument 'returnType' must be non-null.")
    val args = arguments.toList.map(_.asInstanceOf[Type])
    val retTpe = returnType.asInstanceOf[Type]
    new WrappedType(Type.Lambda(args, retTpe))
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
  def mkChar(c: Char): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the int8 value corresponding to the given byte.
    */
  def mkInt8(b: Byte): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the int8 value corresponding to the given int.
    *
    * @throws IllegalArgumentException if the int is out of bounds.
    */
  def mkInt8(i: Int): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the int16 value corresponding to the given short.
    */
  def mkInt16(s: Short): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the int16 value corresponding to the given int.
    *
    * @throws IllegalArgumentException if the int is out of bounds.
    */
  def mkInt16(i: Int): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the int32 value corresponding to the given int.
    */
  def mkInt32(i: Int): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the int64 value corresponding to the given int.
    */
  def mkInt64(i: Int): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the int64 value corresponding to the given long.
    */
  def mkInt64(l: Long): IValue = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

  /**
    * Returns the str value corresponding to the given string.
    */
  def mkStr(s: String): IValue = new WrappedValue(Value.mkStr(s))

  // TODO: Add constructor methods.


  //
  //  /**
  //    * Returns the enum with the given fully qualified `name` and tags.
  //    */
  //  def mkEnumType(name: String, tags: Array[String]): IType = {
  //    if (name == null) throw new IllegalArgumentException("Argument 'name' must be non-null.")
  //    if (tags == null) throw new IllegalArgumentException("Argument 'tags' must be non-null.")
  //
  //    // TODO
  //    val cases = tags.foldLeft(Map.empty[String, Type.Tag]) {
  //      case (macc, tag) => macc + (tag -> Type.Tag(???, ???, ???))
  //    }
  //
  //    new WrappedType(new Type.Enum(cases))
  //  }
  //
  //  /**
  //    * Returns the tuple type of the given `types`.
  //    */
  //  def mkTupleType(types: Array[IType]): IType = {
  //    if (types == null) throw new IllegalArgumentException("Argument 'types' must be non-null.")
  //    val elms = types.toList.map(_.asInstanceOf[Type])
  //    new WrappedType(Type.Tuple(elms))
  //  }
  //
  //  /**
  //    * Returns the opt type parameterized by the given type `tpe`.
  //    */
  //  def mkOptType(tpe: IType): IType = {
  //    if (tpe == null) throw new IllegalArgumentException("Argument 'tpe' must be non-null.")
  //    new WrappedType(Type.Opt(tpe.asInstanceOf[Type]))
  //  }
  //
  //  /**
  //    * Returns the list type parameterized by the given type `tpe`.
  //    */
  //  def mkListType(tpe: IType): IType = {
  //    if (tpe == null) throw new IllegalArgumentException("Argument 'tpe' must be non-null.")
  //    new WrappedType(Type.Lst(tpe.asInstanceOf[Type]))
  //  }
  //
  //  /**
  //    * Returns the set type parameterized by the given type `tpe`.
  //    */
  //  def mkSetType(tpe: IType): IType = {
  //    if (tpe == null) throw new IllegalArgumentException("Argument 'tpe' must be non-null.")
  //    new WrappedType(Type.Set(tpe.asInstanceOf[Type]))
  //  }
  //
  //  /**
  //    * Returns the set type parameterized by the given `key` and `value` types.
  //    */
  //  def mkMapType(key: IType, value: IType): IType = {
  //    if (key == null) throw new IllegalArgumentException("Argument 'key' must be non-null.")
  //    if (value == null) throw new IllegalArgumentException("Argument 'value' must be non-null.")
  //    new WrappedType(Type.Map(key.asInstanceOf[Type], value.asInstanceOf[Type]))
  //  }
  //
  //  /**
  //    * Returns the native type for the given fully qualified `name`.
  //    */
  //  def mkNative(name: String): IType = {
  //    if (name == null) throw new IllegalArgumentException("Argument 'name' must be non-null.")
  //    new WrappedType(Type.Native(name))
  //  }
  //
  //  /**
  //    * Returns the function type for the function with the given argument types and return type.
  //    */
  //  def mkFunctionType(arguments: Array[IType], returnType: IType): IType = {
  //    if (arguments == null) throw new IllegalArgumentException("Argument 'arguments' must be non-null.")
  //    if (returnType == null) throw new IllegalArgumentException("Argument 'returnType' must be non-null.")
  //    val args = arguments.toList.map(_.asInstanceOf[Type])
  //    val retTpe = returnType.asInstanceOf[Type]
  //    new WrappedType(Type.Lambda(args, retTpe))
  //  }
}
