/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.jvm.{JavaField, JavaFieldRef, JavaMethod, JavaMethodRef, JavaType}
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TypeAliasSymUse}
import ca.uwaterloo.flix.language.ast.shared.{RegionScope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Name, SourceLocation, Symbol, Type, TypeConstructor}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.{ClassDesc, MethodTypeDesc}
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/**
  * Tests how a type prints when its constructor has too few arguments, exactly the arguments it
  * takes, and too many arguments.
  *
  * Every type constructor has a table, in the order of [[TypeConstructor]]. A row `n -> s` of a
  * table says that the constructor applied to its first `n` arguments prints as `s`. Each row is a
  * test, named after the constructor, whether it is `UnderApplied`, `Applied` or `OverApplied`, and
  * `n`, e.g. `Tuple.OverApplied.3`.
  *
  * A table must have a row with too few arguments (unless the constructor takes none), a row with
  * exactly its arguments, and a row with too many. The last test of the suite fails if a type
  * constructor has no table.
  *
  * The rules that the tables pin down:
  *   - A constructor that is written with syntax of its own, such as a tuple, an arrow, a record,
  *     a relation or `and`, shows a missing argument as `?`.
  *   - A constructor that is written with a name, such as `Array` or an enum, shows the arguments
  *     it has and nothing for the ones it lacks.
  *   - Every constructor shows the arguments beyond the ones it takes in brackets after the type,
  *     e.g. `(a, b)[c]`. The one exception is an error type, which shows no arguments at all.
  */
class TestFormatTypeApplication extends AnyFunSuite {

  private val loc: SourceLocation = SourceLocation.Unknown

  private val fmt: FormatOptions = FormatOptions(FormatOptions.VarName.NameBased)

  /**
    * The classes of the type constructors that have a table.
    */
  private val tested: mutable.Set[Class[?]] = mutable.Set.empty

  /**
    * The number of type variables created so far. Gives every type variable its own id.
    */
  private var numberOfVars: Int = 0

  /**
    * The names of the type variables that a constructor is applied to, in order.
    */
  private val VarNames: List[String] = List("a", "b", "c", "d", "e", "f")

  private val a: Type = mkVar("a", Kind.Star)
  private val b: Type = mkVar("b", Kind.Star)
  private val c: Type = mkVar("c", Kind.Star)
  private val d: Type = mkVar("d", Kind.Star)
  private val e: Type = mkVar("e", Kind.Star)

  /**
    * Returns a fresh type variable with the given `name` and kind `k`.
    */
  private def mkVar(name: String, k: Kind): Type = {
    numberOfVars += 1
    Type.Var(new Symbol.KindedTypeVarSym(numberOfVars, VarText.SourceText(name), k, RegionScope.Top, loc), loc)
  }

  /**
    * Returns the type variables `a`, `b`, `c`, ... to apply a constructor of kind `k` to.
    *
    * The first variables have the kinds of the arguments that the constructor takes, so that the
    * constructor applied to exactly its arguments is well-kinded. The rest have kind `Star`.
    */
  private def mkVars(k: Kind): List[Type] = {
    val kinds = Kind.kindArgs(k)
    VarNames.zipWithIndex.map {
      case (name, i) => mkVar(name, kinds.lift(i).getOrElse(Kind.Star))
    }
  }

  /**
    * Returns the name of the type constructor `tc`, e.g. `Tuple` for `Tuple(2)`.
    */
  private def nameOf(tc: TypeConstructor): String = tc.toString.takeWhile(_ != '(')

  /**
    * Tests the type constructor `tc` applied to the first `n` of the type variables `a`, `b`, `c`,
    * ..., for every row `n -> expected` in `rows`.
    */
  private def check(tc: TypeConstructor)(rows: (Int, String)*): Unit =
    check(nameOf(tc), tc, mkVars(tc.kind))(rows.toList)

  /**
    * Tests the type constructor `tc` applied to the first `n` of the arguments `args`, for every
    * row `n -> expected` in `rows`.
    *
    * The tests are named after `name`, which tells the table from the other tables of `tc`.
    */
  private def check(name: String, tc: TypeConstructor, args: List[Type])(rows: List[(Int, String)]): Unit = {
    tested += tc.getClass
    check(name, Type.Cst(tc, loc), Kind.kindArgs(tc.kind).length, args)(rows)
  }

  /**
    * Tests the type `head`, which takes `arity` arguments, applied to the first `n` of the
    * arguments `args`, for every row `n -> expected` in `rows`.
    *
    * Requires that `rows` has a row with too few arguments (unless `arity` is zero), a row with
    * exactly `arity` arguments, and a row with too many arguments.
    */
  private def check(name: String, head: Type, arity: Int, args: List[Type])(rows: List[(Int, String)]): Unit = {
    val ns = rows.map(_._1)
    require(arity == 0 || ns.exists(_ < arity), s"The table of '$name' has no row with too few arguments.")
    require(ns.contains(arity), s"The table of '$name' has no row with exactly $arity arguments.")
    require(ns.exists(_ > arity), s"The table of '$name' has no row with too many arguments.")

    rows.foreach {
      case (n, expected) =>
        val status = if (n < arity) "UnderApplied" else if (n == arity) "Applied" else "OverApplied"
        test(s"$name.$status.$n") {
          val tpe = Type.mkApply(head, args.take(n), loc)
          val actual = Pretty.tpe(tpe)(fmt).plain
          assert(actual == expected)
        }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Primitive types                                                         //
  /////////////////////////////////////////////////////////////////////////////

  check(TypeConstructor.Void)(
    0 -> "Void",
    1 -> "Void[a]",
  )

  check(TypeConstructor.AnyType)(
    0 -> "AnyType",
    1 -> "AnyType[a]",
  )

  check(TypeConstructor.Unit)(
    0 -> "Unit",
    1 -> "Unit[a]",
  )

  check(TypeConstructor.Null)(
    0 -> "Null",
    1 -> "Null[a]",
  )

  check(TypeConstructor.Bool)(
    0 -> "Bool",
    1 -> "Bool[a]",
  )

  check(TypeConstructor.Char)(
    0 -> "Char",
    1 -> "Char[a]",
  )

  check(TypeConstructor.Float32)(
    0 -> "Float32",
    1 -> "Float32[a]",
  )

  check(TypeConstructor.Float64)(
    0 -> "Float64",
    1 -> "Float64[a]",
  )

  check(TypeConstructor.BigDecimal)(
    0 -> "BigDecimal",
    1 -> "BigDecimal[a]",
  )

  check(TypeConstructor.Int8)(
    0 -> "Int8",
    1 -> "Int8[a]",
  )

  check(TypeConstructor.Int16)(
    0 -> "Int16",
    1 -> "Int16[a]",
  )

  check(TypeConstructor.Int32)(
    0 -> "Int32",
    1 -> "Int32[a]",
    2 -> "Int32[a, b]",
  )

  check(TypeConstructor.Int64)(
    0 -> "Int64",
    1 -> "Int64[a]",
  )

  check(TypeConstructor.BigInt)(
    0 -> "BigInt",
    1 -> "BigInt[a]",
  )

  check(TypeConstructor.Str)(
    0 -> "String",
    1 -> "String[a]",
  )

  check(TypeConstructor.Regex)(
    0 -> "Regex",
    1 -> "Regex[a]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Arrows                                                                  //
  /////////////////////////////////////////////////////////////////////////////

  // The first argument of an arrow is its effect, so an arrow with one argument has an effect and
  // no types. An arrow of arity `n` takes `n` types after the effect.

  check("Arrow.Pure", TypeConstructor.Arrow(2), List(Type.Pure, a, b, c, d))(List(
    0 -> raw"? -> ? \ ?",
    1 -> "? -> ?",
    2 -> "a -> ?",
    3 -> "a -> b",
    4 -> "(a -> b)[c]",
    5 -> "(a -> b)[c, d]",
  ))

  check("Arrow.Impure", TypeConstructor.Arrow(2), List(Type.IO, a, b, c, d))(List(
    0 -> raw"? -> ? \ ?",
    1 -> raw"? -> ? \ IO",
    2 -> raw"a -> ? \ IO",
    3 -> raw"a -> b \ IO",
    4 -> raw"(a -> b \ IO)[c]",
    5 -> raw"(a -> b \ IO)[c, d]",
  ))

  check("Arrow.Arity3", TypeConstructor.Arrow(3), List(Type.IO, a, b, c, d))(List(
    0 -> raw"? -> (? -> ? \ ?)",
    1 -> raw"? -> (? -> ? \ IO)",
    2 -> raw"a -> (? -> ? \ IO)",
    3 -> raw"a -> (b -> ? \ IO)",
    4 -> raw"a -> (b -> c \ IO)",
    5 -> raw"(a -> (b -> c \ IO))[d]",
  ))

  check("Arrow.Arity4", TypeConstructor.Arrow(4), List(Type.Pure, a, b, c, d, e))(List(
    0 -> raw"? -> (? -> (? -> ? \ ?))",
    3 -> "a -> (b -> (? -> ?))",
    5 -> "a -> (b -> (c -> d))",
    6 -> "(a -> (b -> (c -> d)))[e]",
  ))

  check(TypeConstructor.ArrowWithoutEffect(2))(
    0 -> "? --> ?",
    1 -> "a --> ?",
    2 -> "a --> b",
    3 -> "(a --> b)[c]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Records                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  private val x: Name.Label = Name.Label("x", loc)

  private val closedRecordRow: Type = Type.mkRecordRowExtend(x, Type.Int32, Type.RecordRowEmpty, loc)

  private val openRecordRow: Type = Type.mkRecordRowExtend(x, Type.Int32, mkVar("r", Kind.RecordRow), loc)

  check(TypeConstructor.RecordRowEmpty)(
    0 -> "(  )",
    1 -> "(  )[a]",
  )

  check(TypeConstructor.RecordRowExtend(x))(
    0 -> "( x = ? | ? )",
    1 -> "( x = a | ? )",
    2 -> "( x = a | b )",
    3 -> "( x = a | b )[c]",
  )

  check("RecordRowExtend.Closed", TypeConstructor.RecordRowExtend(x), List(Type.Int32, Type.RecordRowEmpty, a, b))(List(
    1 -> "( x = Int32 | ? )",
    2 -> "( x = Int32 )",
    3 -> "( x = Int32 )[a]",
    4 -> "( x = Int32 )[a, b]",
  ))

  check(TypeConstructor.Record)(
    0 -> "{ ? }",
    1 -> "{ a }",
    2 -> "{ a }[b]",
  )

  check("Record.Closed", TypeConstructor.Record, List(closedRecordRow, a))(List(
    0 -> "{ ? }",
    1 -> "{ x = Int32 }",
    2 -> "{ x = Int32 }[a]",
  ))

  check("Record.Open", TypeConstructor.Record, List(openRecordRow, a))(List(
    0 -> "{ ? }",
    1 -> "{ x = Int32 | r }",
    2 -> "{ x = Int32 | r }[a]",
  ))

  /////////////////////////////////////////////////////////////////////////////
  // Schemas                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  private val p: Name.Pred = Name.Pred("P", loc)

  private val relation: Type = Type.mkRelation(List(Type.Int32, Type.Str), loc)

  private val lattice: Type = Type.mkLattice(List(Type.Str, Type.Int32), loc)

  private val closedSchemaRow: Type = Type.mkSchemaRowExtend(p, relation, Type.SchemaRowEmpty, loc)

  private val openSchemaRow: Type = Type.mkSchemaRowExtend(p, relation, mkVar("r", Kind.SchemaRow), loc)

  check(TypeConstructor.Extensible)(
    0 -> "#| ? |#",
    1 -> "#| a |#",
    2 -> "#| a |#[b]",
  )

  check("Extensible.Closed", TypeConstructor.Extensible, List(closedSchemaRow, a))(List(
    0 -> "#| ? |#",
    1 -> "#| P(Int32, String) |#",
    2 -> "#| P(Int32, String) |#[a]",
  ))

  check("Extensible.Open", TypeConstructor.Extensible, List(openSchemaRow, a))(List(
    0 -> "#| ? |#",
    1 -> "#| P(Int32, String) | r |#",
    2 -> "#| P(Int32, String) | r |#[a]",
  ))

  check(TypeConstructor.SchemaRowEmpty)(
    0 -> "#(  )",
    1 -> "#(  )[a]",
  )

  // The first argument of a schema row is the type of its predicate. A missing one is shown as a
  // predicate of one unknown term, and one that is neither a relation nor a lattice in `<` and `>`.

  check(TypeConstructor.SchemaRowExtend(p))(
    0 -> "#( P(?) | ? )",
    1 -> "#( P(<a>) | ? )",
    2 -> "#( P(<a>) | b )",
    3 -> "#( P(<a>) | b )[c]",
  )

  check("SchemaRowExtend.Relation", TypeConstructor.SchemaRowExtend(p), List(relation, Type.SchemaRowEmpty, a))(List(
    1 -> "#( P(Int32, String) | ? )",
    2 -> "#( P(Int32, String) )",
    3 -> "#( P(Int32, String) )[a]",
  ))

  check("SchemaRowExtend.Lattice", TypeConstructor.SchemaRowExtend(p), List(lattice, Type.SchemaRowEmpty, a))(List(
    1 -> "#( P(String; Int32) | ? )",
    2 -> "#( P(String; Int32) )",
    3 -> "#( P(String; Int32) )[a]",
  ))

  check(TypeConstructor.Schema)(
    0 -> "#{ ? }",
    1 -> "#{ a }",
    2 -> "#{ a }[b]",
  )

  check("Schema.Closed", TypeConstructor.Schema, List(closedSchemaRow, a))(List(
    0 -> "#{ ? }",
    1 -> "#{ P(Int32, String) }",
    2 -> "#{ P(Int32, String) }[a]",
  ))

  check("Schema.Open", TypeConstructor.Schema, List(openSchemaRow, a))(List(
    0 -> "#{ ? }",
    1 -> "#{ P(Int32, String) | r }",
    2 -> "#{ P(Int32, String) | r }[a]",
  ))

  /////////////////////////////////////////////////////////////////////////////
  // Named constructors                                                      //
  /////////////////////////////////////////////////////////////////////////////

  private val color: Symbol.RestrictableEnumSym = Symbol.mkRestrictableEnumSym(Name.RootNS, Name.Ident("Color", loc), List(Name.Ident("Red", loc), Name.Ident("Green", loc)))

  check(TypeConstructor.Sender)(
    0 -> "Sender",
    1 -> "Sender[a]",
    2 -> "Sender[a, b]",
  )

  check(TypeConstructor.Receiver)(
    0 -> "Receiver",
    1 -> "Receiver[a]",
    2 -> "Receiver[a, b]",
  )

  check(TypeConstructor.Lazy)(
    0 -> "Lazy",
    1 -> "Lazy[a]",
    2 -> "Lazy[a, b]",
  )

  check(TypeConstructor.Enum(Symbol.mkEnumSym("Map"), Kind.Star ->: Kind.Star ->: Kind.Star))(
    0 -> "Map",
    1 -> "Map[a]",
    2 -> "Map[a, b]",
    3 -> "Map[a, b, c]",
  )

  check(TypeConstructor.Struct(Symbol.mkStructSym(Name.RootNS, Name.Ident("Cell", loc)), Kind.Star ->: Kind.Eff ->: Kind.Star))(
    0 -> "Cell",
    1 -> "Cell[a]",
    2 -> "Cell[a, b]",
    3 -> "Cell[a, b, c]",
  )

  check(TypeConstructor.RestrictableEnum(color, Kind.CaseSet(color) ->: Kind.Star))(
    0 -> "Color",
    1 -> "Color[a]",
    2 -> "Color[a, b]",
  )

  check(TypeConstructor.Native(ClassDesc.of("java.util.HashMap"), 2))(
    0 -> "java.util.HashMap",
    1 -> "java.util.HashMap[a]",
    2 -> "java.util.HashMap[a, b]",
    3 -> "java.util.HashMap[a, b, c]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Java members                                                            //
  /////////////////////////////////////////////////////////////////////////////

  private val stringClass: ClassDesc = ClassDesc.of("java.lang.String")

  private val intClass: ClassDesc = ClassDesc.ofDescriptor("I")

  /**
    * The constructor `String()`.
    */
  private val javaConstructor: JavaMethod = JavaMethod(
    JavaMethodRef(stringClass, "<init>", MethodTypeDesc.ofDescriptor("()V"), isInterface = false),
    modifiers = 0,
    typeParameters = Nil,
    parameterNames = Nil,
    parameterTypes = Nil,
    returnType = JavaType.NonGeneric(ClassDesc.ofDescriptor("V")),
    isConstructor = true,
    isVarArgs = false
  )

  /**
    * The method `String.length()`.
    */
  private val javaMethod: JavaMethod = JavaMethod(
    JavaMethodRef(stringClass, "length", MethodTypeDesc.ofDescriptor("()I"), isInterface = false),
    modifiers = 0,
    typeParameters = Nil,
    parameterNames = Nil,
    parameterTypes = Nil,
    returnType = JavaType.NonGeneric(intClass),
    isConstructor = false,
    isVarArgs = false
  )

  /**
    * A field `String.hash`.
    */
  private val javaField: JavaField = JavaField(JavaFieldRef(stringClass, "hash", intClass), modifiers = 0, JavaType.NonGeneric(intClass))

  check(TypeConstructor.JvmConstructor(javaConstructor))(
    0 -> s"JvmConstructor($javaConstructor)",
    1 -> s"JvmConstructor($javaConstructor)[a]",
  )

  check(TypeConstructor.JvmMethod(javaMethod, Nil))(
    0 -> s"JvmMethod($javaMethod)",
    1 -> s"JvmMethod($javaMethod)[a]",
  )

  check(TypeConstructor.JvmField(javaField))(
    0 -> s"JvmField($javaField)",
    1 -> s"JvmField($javaField)[a]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Arrays and vectors                                                      //
  /////////////////////////////////////////////////////////////////////////////

  check(TypeConstructor.Array)(
    0 -> "Array",
    1 -> "Array[a]",
    2 -> "Array[a, b]",
    3 -> "Array[a, b, c]",
  )

  check(TypeConstructor.ArrayWithoutRegion)(
    0 -> "ArrayWithoutRegion",
    1 -> "ArrayWithoutRegion[a]",
    2 -> "ArrayWithoutRegion[a, b]",
  )

  check(TypeConstructor.Vector)(
    0 -> "Vector",
    1 -> "Vector[a]",
    2 -> "Vector[a, b]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Tuples, relations and lattices                                          //
  /////////////////////////////////////////////////////////////////////////////

  check(TypeConstructor.Tuple(2))(
    0 -> "(?, ?)",
    1 -> "(a, ?)",
    2 -> "(a, b)",
    3 -> "(a, b)[c]",
    4 -> "(a, b)[c, d]",
  )

  check("Tuple.Arity3", TypeConstructor.Tuple(3), List(a, b, c, d))(List(
    0 -> "(?, ?, ?)",
    2 -> "(a, b, ?)",
    3 -> "(a, b, c)",
    4 -> "(a, b, c)[d]",
  ))

  check(TypeConstructor.Relation(2))(
    0 -> "Relation(?, ?)",
    1 -> "Relation(a, ?)",
    2 -> "Relation(a, b)",
    3 -> "Relation(a, b)[c]",
  )

  check("Relation.Arity0", TypeConstructor.Relation(0), List(a))(List(
    0 -> "Relation()",
    1 -> "Relation()[a]",
  ))

  // The last argument of a lattice is its lattice type, which is written after a `;`.

  check(TypeConstructor.Lattice(2))(
    0 -> "Lattice(?; ?)",
    1 -> "Lattice(a; ?)",
    2 -> "Lattice(a; b)",
    3 -> "Lattice(a; b)[c]",
  )

  check("Lattice.Arity3", TypeConstructor.Lattice(3), List(a, b, c, d))(List(
    0 -> "Lattice(?, ?; ?)",
    2 -> "Lattice(a, b; ?)",
    3 -> "Lattice(a, b; c)",
    4 -> "Lattice(a, b; c)[d]",
  ))

  check("Lattice.Arity1", TypeConstructor.Lattice(1), List(a, b))(List(
    0 -> "Lattice(; ?)",
    1 -> "Lattice(; a)",
    2 -> "Lattice(; a)[b]",
  ))

  // A lattice of no types has no lattice type to show.

  check("Lattice.Arity0", TypeConstructor.Lattice(0), List(a))(List(
    0 -> "Lattice",
    1 -> "Lattice[a]",
  ))

  /////////////////////////////////////////////////////////////////////////////
  // Boolean formulas                                                        //
  /////////////////////////////////////////////////////////////////////////////

  check(TypeConstructor.True)(
    0 -> "true",
    1 -> "true[a]",
  )

  check(TypeConstructor.False)(
    0 -> "false",
    1 -> "false[a]",
  )

  check(TypeConstructor.Not)(
    0 -> "not ?",
    1 -> "not a",
    2 -> "(not a)[b]",
  )

  check(TypeConstructor.And)(
    0 -> "? and ?",
    1 -> "a and ?",
    2 -> "a and b",
    3 -> "(a and b)[c]",
  )

  check(TypeConstructor.Or)(
    0 -> "? or ?",
    1 -> "a or ?",
    2 -> "a or b",
    3 -> "(a or b)[c]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Effects                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  check(TypeConstructor.Pure)(
    0 -> "{}",
    1 -> "{}[a]",
  )

  check(TypeConstructor.Univ)(
    0 -> "Univ",
    1 -> "Univ[a]",
  )

  check(TypeConstructor.Complement)(
    0 -> "~?",
    1 -> "~a",
    2 -> "(~a)[b]",
  )

  check(TypeConstructor.Union)(
    0 -> "? + ?",
    1 -> "a + ?",
    2 -> "a + b",
    3 -> "(a + b)[c]",
  )

  check(TypeConstructor.Intersection)(
    0 -> "? & ?",
    1 -> "a & ?",
    2 -> "a & b",
    3 -> "(a & b)[c]",
  )

  check(TypeConstructor.Difference)(
    0 -> "? - ?",
    1 -> "a - ?",
    2 -> "a - b",
    3 -> "(a - b)[c]",
  )

  check(TypeConstructor.SymmetricDiff)(
    0 -> "? ⊕ ?",
    1 -> "a ⊕ ?",
    2 -> "a ⊕ b",
    3 -> "(a ⊕ b)[c]",
  )

  check(TypeConstructor.Effect(Symbol.IO, Kind.Eff))(
    0 -> "IO",
    1 -> "IO[a]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Case sets                                                               //
  /////////////////////////////////////////////////////////////////////////////

  private val red: Symbol.RestrictableCaseSym = Symbol.mkRestrictableCaseSym(color, Name.Ident("Red", loc))

  private val green: Symbol.RestrictableCaseSym = Symbol.mkRestrictableCaseSym(color, Name.Ident("Green", loc))

  check(TypeConstructor.CaseComplement(color))(
    0 -> "~?",
    1 -> "~a",
    2 -> "(~a)[b]",
  )

  check(TypeConstructor.CaseUnion(color))(
    0 -> "? + ?",
    1 -> "a + ?",
    2 -> "a + b",
    3 -> "(a + b)[c]",
  )

  check(TypeConstructor.CaseIntersection(color))(
    0 -> "? & ?",
    1 -> "a & ?",
    2 -> "a & b",
    3 -> "(a & b)[c]",
  )

  check(TypeConstructor.CaseSymmetricDiff(color))(
    0 -> "? ⊕ ?",
    1 -> "a ⊕ ?",
    2 -> "a ⊕ b",
    3 -> "(a ⊕ b)[c]",
  )

  check(TypeConstructor.CaseSet(SortedSet(red, green), color))(
    0 -> "{Green, Red}",
    1 -> "{Green, Red}[a]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Regions                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  check(TypeConstructor.Region(new Symbol.RegionSym(0, "rc", loc)))(
    0 -> "rc",
    1 -> "rc[a]",
  )

  check(TypeConstructor.RegionToStar)(
    0 -> "Region",
    1 -> "Region[a]",
    2 -> "Region[a, b]",
  )

  check(TypeConstructor.RegionWithoutRegion)(
    0 -> "RegionWithoutRegion",
    1 -> "RegionWithoutRegion[a]",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Error types                                                             //
  /////////////////////////////////////////////////////////////////////////////

  // An error type can have any kind, so its arguments say nothing and are not shown.

  check(TypeConstructor.Error(0, Kind.Star ->: Kind.Star))(
    0 -> "Error",
    1 -> "Error",
    2 -> "Error",
  )

  /////////////////////////////////////////////////////////////////////////////
  // Types that are not built from a type constructor                        //
  /////////////////////////////////////////////////////////////////////////////

  private val alias: TypeAliasSymUse = TypeAliasSymUse(Symbol.mkTypeAliasSym(Name.RootNS, Name.Ident("MyAlias", loc)), loc)

  private val assoc: AssocTypeSymUse = AssocTypeSymUse(Symbol.mkAssocTypeSym(Symbol.mkTraitSym("Collection"), Name.Ident("Elm", loc)), loc)

  check("Var", mkVar("m", Kind.Star ->: Kind.Star), arity = 1, List(a, b))(List(
    0 -> "m",
    1 -> "m[a]",
    2 -> "m[a, b]",
  ))

  check("Var.Star", mkVar("t", Kind.Star), arity = 0, List(a))(List(
    0 -> "t",
    1 -> "t[a]",
  ))

  // An alias and an associated type hold their own arguments, and show any further ones after them.

  check("Alias", Type.Alias(alias, List(Type.Int32), Type.Str, loc), arity = 0, List(a))(List(
    0 -> "MyAlias[Int32]",
    1 -> "MyAlias[Int32, a]",
  ))

  check("Alias.NoArguments", Type.Alias(alias, Nil, Type.Str, loc), arity = 0, List(a))(List(
    0 -> "MyAlias",
    1 -> "MyAlias[a]",
  ))

  check("AssocType", Type.AssocType(assoc, Type.Int32, Kind.Star, loc), arity = 0, List(a))(List(
    0 -> "Collection.Elm[Int32]",
    1 -> "Collection.Elm[Int32, a]",
  ))

  check("JvmToType", Type.JvmToType(Type.Str, loc), arity = 0, List(a))(List(
    0 -> "JvmToType(String)",
    1 -> "JvmToType(String)[a]",
  ))

  check("JvmToEff", Type.JvmToEff(Type.Str, loc), arity = 0, List(a))(List(
    0 -> "JvmToEff(String)",
    1 -> "JvmToEff(String)[a]",
  ))

  check("UnresolvedJvmType.Constructor", Type.UnresolvedJvmType(JvmMember.JvmConstructor(stringClass, List(Type.Int32)), loc), arity = 0, List(a))(List(
    0 -> "JvmUnresolvedConstructor(String, Int32)",
    1 -> "JvmUnresolvedConstructor(String, Int32)[a]",
  ))

  check("UnresolvedJvmType.Method", Type.UnresolvedJvmType(JvmMember.JvmMethod(Type.Str, Name.Ident("charAt", loc), List(Type.Int32)), loc), arity = 0, List(a))(List(
    0 -> "JvmUnresolvedMethod(String, charAt, Int32)",
    1 -> "JvmUnresolvedMethod(String, charAt, Int32)[a]",
  ))

  check("UnresolvedJvmType.Field", Type.UnresolvedJvmType(JvmMember.JvmField(loc, Type.Str, Name.Ident("hash", loc)), loc), arity = 0, List(a))(List(
    0 -> "JvmUnresolvedField(String, hash)",
    1 -> "JvmUnresolvedField(String, hash)[a]",
  ))

  check("UnresolvedJvmType.StaticMethod", Type.UnresolvedJvmType(JvmMember.JvmStaticMethod(stringClass, Name.Ident("valueOf", loc), List(Type.Int32)), loc), arity = 0, List(a))(List(
    0 -> "JvmUnresolvedStaticMethod(String, valueOf, Int32)",
    1 -> "JvmUnresolvedStaticMethod(String, valueOf, Int32)[a]",
  ))

  /////////////////////////////////////////////////////////////////////////////
  // Coverage                                                                //
  /////////////////////////////////////////////////////////////////////////////

  test("EveryTypeConstructorHasATable") {
    // The type constructors are the classes declared in `TypeConstructor` that extend it.
    val all = classOf[TypeConstructor].getDeclaredClasses.toList.filter(c => classOf[TypeConstructor].isAssignableFrom(c))
    assert(all.nonEmpty, "Unable to find the type constructors.")

    val missing = all.filterNot(tested.contains).map(_.getSimpleName.stripSuffix("$")).sorted
    assert(missing.isEmpty, s"No table for: ${missing.mkString(", ")}.")
  }

}
