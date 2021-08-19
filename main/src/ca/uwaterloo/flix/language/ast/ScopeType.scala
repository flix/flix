package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix

sealed trait ScopeType {
  def asScoped: ScopeInfo = this.withScope(Scopedness.Scoped)

  def asUnscoped: ScopeInfo = this.withScope(Scopedness.Unscoped)

//  def scopeIrrelevant(implicit flix: Flix): ScopeInfo = this.withScope(Scopedness.freshVar())

  private def withScope(sc: Scopedness): ScopeInfo = ScopeInfo(sc, this)
}

object ScopeType {
  case class Var(id: Int) extends ScopeType

  case class Cst(cst: TypeConstructor) extends ScopeType

  case class Apply(s1: ScopeInfo, s2: ScopeInfo) extends ScopeType

  case class Lambda(svar: ScopeInfo, s: ScopeInfo) extends ScopeType

  /**
    * Returns a fresh type variable of the given kind `k` and rigidity `r`.
    */
  def freshVar()(implicit flix: Flix): ScopeType.Var = {
    val id = flix.genSym.freshId()
    Var(id)
  }

  /**
    * Represents the Unit type.
    */
  val Unit: ScopeType = ScopeType.Cst(TypeConstructor.Unit)

  /**
    * Represents the Null type.
    */
  val Null: ScopeType = ScopeType.Cst(TypeConstructor.Null)

  /**
    * Represents the Bool type.
    */
  val Bool: ScopeType = ScopeType.Cst(TypeConstructor.Bool)

  /**
    * Represents the Char type.
    */
  val Char: ScopeType = ScopeType.Cst(TypeConstructor.Char)

  /**
    * Represents the Float32 type.
    */
  val Float32: ScopeType = ScopeType.Cst(TypeConstructor.Float32)

  /**
    * Represents the Float64 type.
    */
  val Float64: ScopeType = ScopeType.Cst(TypeConstructor.Float64)

  /**
    * Represents the Int8 type.
    */
  val Int8: ScopeType = ScopeType.Cst(TypeConstructor.Int8)

  /**
    * Represents the Int16 type.
    */
  val Int16: ScopeType = ScopeType.Cst(TypeConstructor.Int16)

  /**
    * Represents the Int32 type.
    */
  val Int32: ScopeType = ScopeType.Cst(TypeConstructor.Int32)

  /**
    * Represents the Int64 type.
    */
  val Int64: ScopeType = ScopeType.Cst(TypeConstructor.Int64)

  /**
    * Represents the BigInt type.
    */
  val BigInt: ScopeType = ScopeType.Cst(TypeConstructor.BigInt)

  /**
    * Represents the String type.
    */
  val Str: ScopeType = ScopeType.Cst(TypeConstructor.Str)

  /**
    * Represents the Array type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Array: ScopeType = ScopeType.Cst(TypeConstructor.Array)

  /**
    * Represents the Channel type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Channel: ScopeType = ScopeType.Cst(TypeConstructor.Channel)

  /**
    * Represents the Lazy type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Lazy: ScopeType = ScopeType.Cst(TypeConstructor.Lazy)

  /**
    * Represents the Relation type constructor.
    */
  val Relation: ScopeType = ScopeType.Cst(TypeConstructor.Relation)

  /**
    * Represents the Lattice type constructor.
    */
  val Lattice: ScopeType = ScopeType.Cst(TypeConstructor.Lattice)

  /**
    * Represents the type of an empty record.
    */
  val RecordEmpty: ScopeType = ScopeType.Cst(TypeConstructor.RecordEmpty)

  /**
    * Represents the type of an empty schema.
    */
  val SchemaEmpty: ScopeType = ScopeType.Cst(TypeConstructor.SchemaEmpty)

  /**
    * Represents the Boolean True.
    */
  val True: ScopeType = ScopeType.Cst(TypeConstructor.True)

  /**
    * Represents the Boolean False.
    */
  val False: ScopeType = ScopeType.Cst(TypeConstructor.False)

  /**
    * Represents the Pure effect. (TRUE in the Boolean algebra.)
    */
  val Pure: ScopeType = True

  /**
    * Represents the Impure effect. (FALSE in the Boolean algebra.)
    */
  val Impure: ScopeType = False

  /**
    * Represents Unscopedness. (TRUE in the Boolean algebra.)
    */
  val Unscoped: ScopeType = True

  /**
    * Represents Scopedness. (FALSE in the Boolean algebra.)
    */
  val Scoped: ScopeType = False

  /**
    * Represents the Not type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Not: ScopeType = ScopeType.Cst(TypeConstructor.Not)

  /**
    * Represents the And type constructor.
    *
    * NB: This type has kind: * -> (* -> *).
    */
  val And: ScopeType = ScopeType.Cst(TypeConstructor.And)

  /**
    * Represents the Or type constructor.
    *
    * NB: This type has kind: * -> (* -> *).
    */
  val Or: ScopeType = ScopeType.Cst(TypeConstructor.Or)

  /**
    * Returns the type `Array[tpe]` with the given optional source location `loc`.
    */
  def mkArray(elmScopeInfo: ScopeInfo): ScopeInfo = Apply(ScopeType.Cst(TypeConstructor.Array).asUnscoped, elmScopeInfo).asUnscoped

  /**
    * Returns the type `Channel[tpe]` with the given optional source location `loc`.
    */
  def mkChannel(tpe: ScopeInfo): ScopeInfo = ScopeType.Apply(ScopeType.Cst(TypeConstructor.Channel).asUnscoped, tpe).asUnscoped

  /**
    * Returns the type `Lazy[tpe]` with the given optional source location `loc`.
    */
  def mkLazy(tpe: ScopeInfo): ScopeInfo = ScopeType.Apply(ScopeType.Cst(TypeConstructor.Lazy).asUnscoped, tpe).asUnscoped

  /**
    * Returns the type `ScopedRef[tpe, lifetime]` with the given optional source location `loc`.
    */
  def mkScopedRef(tpe1: ScopeInfo, tpe2: ScopeInfo): ScopeInfo =
    ScopeType.Apply(ScopeType.Apply(ScopeType.Cst(TypeConstructor.ScopedRef).asUnscoped, tpe1).asUnscoped, tpe2).asUnscoped

  /**
    * Constructs the pure arrow type A -> B.
    */
  def mkPureArrow(a: ScopeInfo, b: ScopeInfo): ScopeInfo = mkArrowWithEffect(a, Pure.asUnscoped, b)

  /**
    * Constructs the impure arrow type A ~> B.
    */
  def mkImpureArrow(a: ScopeInfo, b: ScopeInfo): ScopeInfo = mkArrowWithEffect(a, Impure.asUnscoped, b)

  /**
    * Constructs the arrow type A -> B & e.
    */
  def mkArrowWithEffect(a: ScopeInfo, e: ScopeInfo, b: ScopeInfo): ScopeInfo = mkApply(ScopeType.Cst(TypeConstructor.Arrow(2)).asUnscoped, List(e, a, b))

  /**
    * Constructs the pure curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B.
    */
  def mkPureCurriedArrow(as: List[ScopeInfo], b: ScopeInfo): ScopeInfo = mkCurriedArrowWithEffect(as, Pure.asUnscoped, b)

  /**
    * Constructs the impure curried arrow type A_1 -> (A_2  -> ... -> A_n) ~> B.
    */
  def mkImpureCurriedArrow(as: List[ScopeInfo], b: ScopeInfo): ScopeInfo = mkCurriedArrowWithEffect(as, Impure.asUnscoped, b)

  /**
    * Constructs the curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B & e.
    */
  def mkCurriedArrowWithEffect(as: List[ScopeInfo], e: ScopeInfo, b: ScopeInfo): ScopeInfo = {
    val a = as.last
    val base = mkArrowWithEffect(a, e, b)
    as.init.foldRight(base) {
      case (arg, acc) => mkPureArrow(arg, acc)
    }
  }

  /**
    * Constructs the pure uncurried arrow type (A_1, ..., A_n) -> B.
    */
  def mkPureUncurriedArrow(as: List[ScopeInfo], b: ScopeInfo): ScopeInfo = mkUncurriedArrowWithEffect(as, Pure.asUnscoped, b)

  /**
    * Constructs the impure uncurried arrow type (A_1, ..., A_n) ~> B.
    */
  def mkImpureUncurriedArrow(as: List[ScopeInfo], b: ScopeInfo): ScopeInfo = mkUncurriedArrowWithEffect(as, Impure.asUnscoped, b)

  /**
    * Constructs the uncurried arrow type (A_1, ..., A_n) -> B & e.
    */
  def mkUncurriedArrowWithEffect(as: List[ScopeInfo], e: ScopeInfo, b: ScopeInfo): ScopeInfo = {
    val arrow = ScopeType.Apply(ScopeType.Cst(TypeConstructor.Arrow(as.length + 1)).asUnscoped, e)
    val inner = as.foldLeft(arrow: ScopeType) {
      case (acc, x) => Apply(acc.asUnscoped, x)
    }
    Apply(inner.asUnscoped, b).asUnscoped
  }

  /**
    * Constructs the apply type base[t_1, ,..., t_n].
    */
  def mkApply(base: ScopeInfo, ts: List[ScopeInfo]): ScopeInfo = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t).asUnscoped
  }

  /**
    * Returns the type `Choice[tpe, isAbsent, isPresent]`.
    */
  def mkChoice(tpe0: ScopeInfo, isAbsent: ScopeInfo, isPresent: ScopeInfo): ScopeInfo = {
    val sym = Symbol.mkEnumSym("Choice")
    val kind = Kind.Star ->: Kind.Bool ->: Kind.Bool ->: Kind.Star
    val tc = TypeConstructor.Enum(sym, kind)
    mkApply(Cst(tc).asUnscoped, List(tpe0, isAbsent, isPresent))
  }

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkEnum(sym: Symbol.EnumSym, k: Kind): ScopeInfo = ScopeType.Cst(TypeConstructor.Enum(sym, k)).asUnscoped

  /**
    * Construct the enum type `Sym[ts]`.
    */
  def mkEnum(sym: Symbol.EnumSym, ts: List[ScopeInfo]): ScopeInfo = mkApply(ScopeType.Cst(TypeConstructor.Enum(sym, Kind.mkArrow(ts.length))).asUnscoped, ts)

  /**
    * Constructs a tag type for the given `sym`, `tag`, `caseScopeType` and `resultScopeType`.
    *
    * A tag type can be understood as a "function type" from the `caseScopeType` to the `resultScopeType`.
    *
    * For example, for:
    *
    * {{{
    * enum List[a] {
    *   case Nil,
    *   case Cons(a, List[a])
    * }
    *
    * We have:
    *
    *   Nil:  Unit -> List[a]           (caseScopeType = Unit, resultScopeType = List[a])
    *   Cons: (a, List[a]) -> List[a]   (caseScopeType = (a, List[a]), resultScopeType = List[a])
    * }}}
    */
  def mkTag(sym: Symbol.EnumSym, tag: Name.Tag, caseScopeInfo: ScopeInfo, resultScopeInfo: ScopeInfo): ScopeInfo = {
    ScopeType.Apply(ScopeType.Apply(ScopeType.Cst(TypeConstructor.Tag(sym, tag)).asUnscoped, caseScopeInfo).asUnscoped, resultScopeInfo).asUnscoped
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[ScopeInfo]): ScopeInfo = {
    val init = ScopeType.Cst(TypeConstructor.Tuple(ts.length)).asUnscoped
    ts.foldLeft(init: ScopeInfo) {
      case (acc, x) => Apply(acc, x).asUnscoped
    }
  }

  /**
    * Constructs the a native type.
    */
  def mkNative(clazz: Class[_]): ScopeType = ScopeType.Cst(TypeConstructor.Native(clazz))

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordExtend(field: Name.Field, tpe: ScopeInfo, rest: ScopeInfo): ScopeInfo = {
    mkApply(ScopeType.Cst(TypeConstructor.RecordExtend(field)).asUnscoped, List(tpe, rest))
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaExtend(pred: Name.Pred, tpe: ScopeInfo, rest: ScopeInfo): ScopeInfo = {
    mkApply(ScopeType.Cst(TypeConstructor.SchemaExtend(pred)).asUnscoped, List(tpe, rest))
  }

  /**
    * Construct a relation type with the given list of type arguments `ts0`.
    */
  def mkRelation(ts0: List[ScopeInfo]): ScopeInfo = {
    val ts = ts0 match {
      case Nil => ScopeType.Unit.asUnscoped
      case x :: Nil => x
      case xs => mkTuple(xs)
    }

    Apply(Relation.asUnscoped, ts).asUnscoped
  }

  /**
    * Construct a lattice type with the given list of type arguments `ts0`.
    */
  def mkLattice(ts0: List[ScopeInfo]): ScopeInfo = {
    val ts = ts0 match {
      case Nil => ScopeType.Unit.asUnscoped
      case x :: Nil => x
      case xs => mkTuple(xs)
    }

    Apply(Lattice.asUnscoped, ts).asUnscoped
  }

  /**
    * Returns the type `Not(tpe0)`.
    */
  def mkNot(tpe0: ScopeInfo): ScopeInfo = tpe0.scopeType match {
    case ScopeType.True => ScopeType.False.asUnscoped
    case ScopeType.False => ScopeType.True.asUnscoped
    case _ => ScopeType.Apply(ScopeType.Not.asUnscoped, tpe0).asScoped
  }

  /**
    * Returns the type `And(tpe1, tpe2)`.
    */
  def mkAnd(tpe1: ScopeInfo, tpe2: ScopeInfo): ScopeInfo = (tpe1.scopeType, tpe2.scopeType) match {
    case (ScopeType.Cst(TypeConstructor.True), _) => tpe2
    case (_, ScopeType.Cst(TypeConstructor.True)) => tpe1
    case (ScopeType.Cst(TypeConstructor.False), _) => ScopeType.False.asUnscoped
    case (_, ScopeType.Cst(TypeConstructor.False)) => ScopeType.False.asUnscoped
    case _ => ScopeType.Apply(ScopeType.Apply(ScopeType.And.asUnscoped, tpe1).asUnscoped, tpe2).asUnscoped
  }

  /**
    * Returns the type `And(tpe1, And(tpe2, tpe3))`.
    */
  def mkAnd(tpe1: ScopeInfo, tpe2: ScopeInfo, tpe3: ScopeInfo): ScopeInfo = mkAnd(tpe1, mkAnd(tpe2, tpe3))

  /**
    * Returns the type `And(tpe1, And(tpe2, ...))`.
    */
  def mkAnd(tpes: List[ScopeInfo]): ScopeInfo = tpes match {
    case Nil => ScopeType.True.asUnscoped
    case x :: xs => mkAnd(x, mkAnd(xs))
  }

  /**
    * Returns the type `Or(tpe1, tpe2)`.
    */
  def mkOr(tpe1: ScopeInfo, tpe2: ScopeInfo): ScopeInfo = (tpe1.scopeType, tpe2.scopeType) match {
    case (ScopeType.Cst(TypeConstructor.True), _) => ScopeType.True.asUnscoped
    case (_, ScopeType.Cst(TypeConstructor.True)) => ScopeType.True.asUnscoped
    case (ScopeType.Cst(TypeConstructor.False), _) => tpe2
    case (_, ScopeType.Cst(TypeConstructor.False)) => tpe1
    case _ => ScopeType.Apply(ScopeType.Apply(ScopeType.Or.asUnscoped, tpe1).asUnscoped, tpe2).asUnscoped
  }

  /**
    * Returns the type `Or(tpe1, Or(tpe2, ...))`.
    */
  def mkOr(tpes: List[ScopeInfo]): ScopeInfo = tpes match {
    case Nil => ScopeType.False.asUnscoped
    case x :: xs => mkOr(x, mkOr(xs))
  }

  /**
    * Returns the type `tpe1 => tpe2`.
    */
  def mkImplies(tpe1: ScopeInfo, tpe2: ScopeInfo): ScopeInfo = ScopeType.mkOr(ScopeType.mkNot(tpe1), tpe2)

  /**
    * Returns a Boolean type that represents the equivalence of `x` and `y`.
    *
    * That is, `x == y` iff `(x /\ y) \/ (not x /\ not y)`
    */
  def mkEquiv(x: ScopeInfo, y: ScopeInfo): ScopeInfo = ScopeType.mkOr(ScopeType.mkAnd(x, y), ScopeType.mkAnd(ScopeType.mkNot(x), ScopeType.mkNot(y)))

  /**
    * Returns a Region type for the given rigid variable `l` with the given source location `loc`.
    */
  def mkRegion(l: ScopeInfo): ScopeInfo =
    ScopeType.Apply(ScopeType.Cst(TypeConstructor.Region).asUnscoped, l).asUnscoped

  /**
    * Returns a simplified (evaluated) form of the given type `tpe0`.
    *
    * Performs beta-reduction of type abstractions and applications.
    */
  def simplify(tpe0: ScopeInfo): ScopeInfo = {
    def eval(t: ScopeInfo, subst: Map[ScopeInfo, ScopeInfo]): ScopeInfo = t.scopeType match {
      case tvar: ScopeType.Var => subst.getOrElse(t, t)

      case ScopeType.Cst(_) => t

      case ScopeType.Apply(ScopeInfo(_, ScopeType.Apply(ScopeInfo(_, ScopeType.Cst(TypeConstructor.RecordExtend(field))), tpe)), rest) =>
        val t1 = eval(tpe, subst)
        val t2 = eval(rest, subst)
        ScopeType.mkRecordExtend(field, t1, t2)

      case ScopeType.Apply(ScopeInfo(_, ScopeType.Apply(ScopeInfo(_, ScopeType.Cst(TypeConstructor.SchemaExtend(pred))), tpe)), rest) =>
        val t1 = eval(tpe, subst)
        val t2 = eval(rest, subst)
        ScopeType.mkSchemaExtend(pred, t1, t2)

      case ScopeType.Lambda(tvar, tpe) => ScopeType.Lambda(tvar, eval(tpe, subst)).asUnscoped

      // TODO: Does not take variable capture into account.
      case ScopeType.Apply(tpe1, tpe2) => (eval(tpe1, subst), eval(tpe2, subst)) match {
        case (ScopeInfo(_, ScopeType.Lambda(tvar, tpe3)), t2) => eval(tpe3, subst + (tvar -> t2))
        case (t1, t2) => ScopeType.Apply(t1, t2).asUnscoped
      }
    }

    eval(tpe0, Map.empty)
  }
}
