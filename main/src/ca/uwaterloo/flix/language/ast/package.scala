package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, Input, Mutability, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatScheme}

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.immutable.SortedSet

package object ast {

  /**
    * A common super-type for control pure expressions.
    */
  sealed trait AtomicOp

  object AtomicOp {

    case class Closure(sym: Symbol.DefnSym) extends AtomicOp

    case class Unary(sop: SemanticOp.UnaryOp) extends AtomicOp

    case class Binary(sop: SemanticOp.BinaryOp) extends AtomicOp

    case class Is(sym: Symbol.CaseSym) extends AtomicOp

    case class Tag(sym: Symbol.CaseSym) extends AtomicOp

    case class Untag(sym: Symbol.CaseSym, idx: Int) extends AtomicOp

    case class Index(idx: Int) extends AtomicOp

    case object Tuple extends AtomicOp

    case class RecordSelect(label: Name.Label) extends AtomicOp

    case class RecordExtend(label: Name.Label) extends AtomicOp

    case class RecordRestrict(label: Name.Label) extends AtomicOp

    case class ExtIs(label: Name.Label) extends AtomicOp

    case class ExtTag(label: Name.Label) extends AtomicOp

    case class ExtUntag(label: Name.Label, idx: Int) extends AtomicOp

    case object ArrayLit extends AtomicOp

    case object ArrayNew extends AtomicOp

    case object ArrayLoad extends AtomicOp

    case object ArrayStore extends AtomicOp

    case object ArrayLength extends AtomicOp

    case class StructNew(sym: Symbol.StructSym, mutability: Mutability, fields: List[Symbol.StructFieldSym]) extends AtomicOp

    case class StructGet(sym: Symbol.StructFieldSym) extends AtomicOp

    case class StructPut(sym: Symbol.StructFieldSym) extends AtomicOp

    case class InstanceOf(clazz: Class[?]) extends AtomicOp

    case object Cast extends AtomicOp

    case object Unbox extends AtomicOp

    case object Box extends AtomicOp

    case class InvokeConstructor(constructor: Constructor[?]) extends AtomicOp

    case class InvokeMethod(method: Method) extends AtomicOp

    case class InvokeStaticMethod(method: Method) extends AtomicOp

    case class GetField(field: Field) extends AtomicOp

    case class PutField(field: Field) extends AtomicOp

    case class GetStaticField(field: Field) extends AtomicOp

    case class PutStaticField(field: Field) extends AtomicOp

    case object Throw extends AtomicOp

    case object Spawn extends AtomicOp

    case object Lazy extends AtomicOp

    case object Force extends AtomicOp

    case class HoleError(sym: Symbol.HoleSym) extends AtomicOp

    case object MatchError extends AtomicOp

    case class CastError(from: String, to: String) extends AtomicOp

  }

  object ChangeSet {

    /**
      * Represents a change set where everything is dirty (used for a complete re-compilation).
      */
    case object Everything extends ChangeSet

    /**
      * Represents a change set where everything in `s` is dirty (must be recompiled).
      */
    case class Dirty(s: Set[Input]) extends ChangeSet

  }

  object Kind {

    /**
      * Represents a wild kind.
      * A wild kind exists during the kinding phase, but should be eliminated before the following phase,
      * unless the kind is deemed irrelevant (e.g. the kind of a wildcard type).
      */
    case object Wild extends Kind

    /**
      * Represents the wildcard kind only matching Case Sets.
      * A wild kind exists during the kinding phase, but should be eliminated before the following phase,
      * unless the kind is deemed irrelevant (e.g. the kind of a wildcard type).
      */
    case object WildCaseSet extends Kind

    /**
      * Represents the kind of types.
      */
    case object Star extends Kind

    /**
      * Represents the kind of effect sets.
      */
    case object Eff extends Kind

    /**
      * Represents the kind of Boolean formulas
      */
    case object Bool extends Kind

    /**
      * Represents the kind of record rows.
      */
    case object RecordRow extends Kind

    /**
      * Represents the kind of schema rows.
      */
    case object SchemaRow extends Kind

    /**
      * Represents the kind of predicates.
      */
    case object Predicate extends Kind

    /**
      * Represents the kind of a Java constructor, method, or field.
      */
    case object Jvm extends Kind

    /**
      * Represents the kind of sets of restrictable enum cases.
      */
    case class CaseSet(sym: Symbol.RestrictableEnumSym) extends Kind

    /**
      * Represents the kind of type expressions `k1 -> k2`.
      */
    case class Arrow(k1: Kind, k2: Kind) extends Kind

    /**
      * Represents an error kind.
      */
    case object Error extends Kind
  }

  object Purity {

    /**
      * Represents a pure expression (i.e. an expression that cannot have
      * side effects).
      */
    case object Pure extends Purity

    /**
      * Represents an impure expression (i.e. an expression that could potentially
      * have side effects).
      */
    case object Impure extends Purity

    /**
      * Represents a control-impure expression (i.e. an expression that could
      * potentially use effects like `do Print.print()`).
      */
    case object ControlImpure extends Purity

  }

  /**
    * A common super-type that captures the rigidity of a type variable.
    */
  sealed trait Rigidity

  object Rigidity {

    /**
      * Denotes a type variable that is flexible, i.e. can be unified with other variables and types.
      */
    case object Flexible extends Rigidity

    /**
      * Denotes a type variable that is rigid, i.e. cannot be unified with anything other than itself.
      */
    case object Rigid extends Rigidity
  }

  /**
    * Environment tracking the rigidity of type variables.
    *
    * `s` holds the set of rigid variable symbols.
    * All variables not in `s` are considered flexible.
    */
  case class RigidityEnv(s: SortedSet[Symbol.KindedTypeVarSym]) {

    /**
      * Returns the rigidity of the given `sym` according to this environment.
      */
    def get(sym: Symbol.KindedTypeVarSym)(implicit scope: Scope): Rigidity = {
      if (s.contains(sym) || sym.scope.isOutside(scope)) {
        Rigidity.Rigid
      } else {
        Rigidity.Flexible
      }
    }

    /**
      * Returns true iff the given `sym` is rigid according to this environment.
      */
    def isRigid(sym: Symbol.KindedTypeVarSym)(implicit scope: Scope): Boolean = get(sym) == Rigidity.Rigid

    /**
      * Returns true iff the given `sym` is flexible according to this environment.
      */
    def isFlexible(sym: Symbol.KindedTypeVarSym)(implicit scope: Scope): Boolean = get(sym) == Rigidity.Flexible

    /**
      * Returns the flexible vars from the given list.
      */
    def getFlexibleVarsOf(tvars: List[Type.Var])(implicit scope: Scope): List[Type.Var] = tvars.filter(tvar => isFlexible(tvar.sym))

    /**
      * Marks the given `sym` as rigid in this environment.
      */
    def markRigid(sym: Symbol.KindedTypeVarSym): RigidityEnv = RigidityEnv(s + sym)

    /**
      * Merges the two rigidity environments, favoring Rigid in case of conflict.
      */
    def ++(that: RigidityEnv): RigidityEnv = RigidityEnv(this.s ++ that.s)
  }

  /**
    * Representation of polytypes.
    */
  case class Scheme(quantifiers: List[Symbol.KindedTypeVarSym], tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint], base: Type) {

    /**
      * Returns a human readable representation of the polytype.
      */
    override def toString: String = {
      FormatScheme.formatSchemeWithOptions(this, FormatOptions.Internal)
    }

  }

  object SemanticOp {
    sealed trait UnaryOp extends SemanticOp

    sealed trait BinaryOp extends SemanticOp

    /**
      * Boolean Operators.
      */
    sealed trait BoolOp extends SemanticOp

  /**
    * Char Operators.
    */
  sealed trait CharOp extends SemanticOp

  /**
    * Float32 Operators.
    */
  sealed trait Float32Op extends SemanticOp

  /**
    * Float64 Operators.
    */
  sealed trait Float64Op extends SemanticOp

  /**
    * Int8 Operators.
    */
  sealed trait Int8Op extends SemanticOp

  /**
    * Int16 Operators.
    */
  sealed trait Int16Op extends SemanticOp

  /**
    * Int32 Operators.
    */
  sealed trait Int32Op extends SemanticOp

  /**
    * Int64 Operators.
    */
  sealed trait Int64Op extends SemanticOp

  /**
    * String Operators.
    */
  sealed trait StringOp extends SemanticOp
  }

  object SimpleType {
    /**
      * Represents an uninhabited type, not an absent value like in Java.
      */
    case object Void extends SimpleType

    case object AnyType extends SimpleType

    case object Unit extends SimpleType

    case object Bool extends SimpleType

    case object Char extends SimpleType

    case object Float32 extends SimpleType

    case object Float64 extends SimpleType

    case object BigDecimal extends SimpleType

    case object Int8 extends SimpleType

    case object Int16 extends SimpleType

    case object Int32 extends SimpleType

    case object Int64 extends SimpleType

    case object BigInt extends SimpleType

    case object String extends SimpleType

    case object Regex extends SimpleType

    case object Region extends SimpleType

    case object Null extends SimpleType

    case class Array(tpe: SimpleType) extends SimpleType

    case class Lazy(tpe: SimpleType) extends SimpleType

    case class Tuple(tpes: List[SimpleType]) extends SimpleType

    case class Enum(sym: Symbol.EnumSym, targs: List[SimpleType]) extends SimpleType

    case class Struct(sym: Symbol.StructSym, targs: List[SimpleType]) extends SimpleType

    case class Arrow(targs: List[SimpleType], result: SimpleType) extends SimpleType

    case object RecordEmpty extends SimpleType

    case class RecordExtend(label: String, value: SimpleType, rest: SimpleType) extends SimpleType

    case object ExtensibleEmpty extends SimpleType

    case class ExtensibleExtend(cons: Name.Pred, tpes: List[SimpleType], rest: SimpleType) extends SimpleType

    case class Native(clazz: Class[?]) extends SimpleType
  }

  case class SourceLocation(isReal: Boolean, source: Source, sp1: SourcePosition, sp2: SourcePosition)


  /** Represents a source position. */
  case class SourcePosition(lineOneIndexed: Int, colOneIndexed: Short)

  case class Token(kind: TokenKind, src: Source, start: Int, end: Int, sp1: SourcePosition, sp2: SourcePosition)

  /**
    * Notes on naming:
    *   - Tokens are named for 'what they are' rather than 'what they represent'.
    *     So '::' is not named 'Cons' but 'ColonColon' as tokens should be oblivious to the concept of cons.
    *   - Tokens are conceptually grouped by prefix, so 'LiteralInt32' is preferred over 'Int32Literal'.
    */
  object TokenKind {

    case object Ampersand extends TokenKind

    case object AngleL extends TokenKind

    case object AngleLEqual extends TokenKind

    case object AngleR extends TokenKind

    case object AngleREqual extends TokenKind

    case object AngledEqual extends TokenKind

    case object AngledPlus extends TokenKind

    case object Annotation extends TokenKind

    case object ArrayHash extends TokenKind

    case object ArrowThickR extends TokenKind

    case object ArrowThinL extends TokenKind

    case object ArrowThinRTight extends TokenKind

    case object ArrowThinRWhitespace extends TokenKind

    case object At extends TokenKind

    case object Backslash extends TokenKind

    case object Bang extends TokenKind

    case object BangEqual extends TokenKind

    case object Bar extends TokenKind

    case object BarHash extends TokenKind

    case object BracketL extends TokenKind

    case object BracketR extends TokenKind

    case object BuiltIn extends TokenKind

    case object Caret extends TokenKind

    case object Colon extends TokenKind

    case object ColonColon extends TokenKind

    case object ColonColonColon extends TokenKind

    case object ColonMinus extends TokenKind

    case object Comma extends TokenKind

    case object CommentBlock extends TokenKind

    case object CommentDoc extends TokenKind

    case object CommentLine extends TokenKind

    case object CurlyL extends TokenKind

    case object CurlyR extends TokenKind

    case object DebugInterpolator extends TokenKind

    case object Dollar extends TokenKind

    case object Dot extends TokenKind

    case object DotDotDot extends TokenKind

    case object DotWhiteSpace extends TokenKind

    case object Equal extends TokenKind

    case object EqualEqual extends TokenKind

    case object GenericOperator extends TokenKind

    case object Hash extends TokenKind

    case object HashBar extends TokenKind

    case object HashCurlyL extends TokenKind

    case object HashParenL extends TokenKind

    case object HoleAnonymous extends TokenKind

    case object HoleNamed extends TokenKind

    case object HoleVariable extends TokenKind

    case object KeywordAlias extends TokenKind

    case object KeywordAnd extends TokenKind

    case object KeywordAs extends TokenKind

    case object KeywordCase extends TokenKind

    case object KeywordCatch extends TokenKind

    case object KeywordCheckedCast extends TokenKind

    case object KeywordCheckedECast extends TokenKind

    case object KeywordChoose extends TokenKind

    case object KeywordChooseStar extends TokenKind

    case object KeywordDef extends TokenKind

    case object KeywordDiscard extends TokenKind

    case object KeywordEMatch extends TokenKind

    case object KeywordEff extends TokenKind

    case object KeywordElse extends TokenKind

    case object KeywordEnum extends TokenKind

    case object KeywordFalse extends TokenKind

    case object KeywordFix extends TokenKind

    case object KeywordForA extends TokenKind

    case object KeywordForM extends TokenKind

    case object KeywordForall extends TokenKind

    case object KeywordForce extends TokenKind

    case object KeywordForeach extends TokenKind

    case object KeywordFrom extends TokenKind

    case object KeywordHandler extends TokenKind

    case object KeywordIf extends TokenKind

    case object KeywordImport extends TokenKind

    case object KeywordInject extends TokenKind

    case object KeywordInstance extends TokenKind

    case object KeywordInstanceOf extends TokenKind

    case object KeywordInto extends TokenKind

    case object KeywordLaw extends TokenKind

    case object KeywordLawful extends TokenKind

    case object KeywordLazy extends TokenKind

    case object KeywordLet extends TokenKind

    case object KeywordMatch extends TokenKind

    case object KeywordMod extends TokenKind

    case object KeywordMut extends TokenKind

    case object KeywordNew extends TokenKind

    case object KeywordNot extends TokenKind

    case object KeywordNull extends TokenKind

    case object KeywordOpenVariant extends TokenKind

    case object KeywordOpenVariantAs extends TokenKind

    case object KeywordOr extends TokenKind

    case object KeywordOverride extends TokenKind

    case object KeywordPQuery extends TokenKind

    case object KeywordPSolve extends TokenKind

    case object KeywordPar extends TokenKind

    case object KeywordProject extends TokenKind

    case object KeywordPub extends TokenKind

    case object KeywordQuery extends TokenKind

    case object KeywordRedef extends TokenKind

    case object KeywordRegion extends TokenKind

    case object KeywordRestrictable extends TokenKind

    case object KeywordRun extends TokenKind

    case object KeywordRvadd extends TokenKind

    case object KeywordRvand extends TokenKind

    case object KeywordRvnot extends TokenKind

    case object KeywordRvsub extends TokenKind

    case object KeywordSealed extends TokenKind

    case object KeywordSelect extends TokenKind

    case object KeywordSolve extends TokenKind

    case object KeywordSpawn extends TokenKind

    case object KeywordStaticLowercase extends TokenKind

    case object KeywordStaticUppercase extends TokenKind

    case object KeywordStruct extends TokenKind

    case object KeywordThrow extends TokenKind

    case object KeywordTrait extends TokenKind

    case object KeywordTrue extends TokenKind

    case object KeywordTry extends TokenKind

    case object KeywordType extends TokenKind

    case object KeywordTypeMatch extends TokenKind

    case object KeywordUncheckedCast extends TokenKind

    case object KeywordUniv extends TokenKind

    case object KeywordUnsafe extends TokenKind

    case object KeywordUse extends TokenKind

    case object KeywordWhere extends TokenKind

    case object KeywordWith extends TokenKind

    case object KeywordWithout extends TokenKind

    case object KeywordXor extends TokenKind

    case object KeywordXvar extends TokenKind

    case object KeywordYield extends TokenKind

    case object ListHash extends TokenKind

    case object LiteralBigDecimal extends TokenKind

    case object LiteralBigInt extends TokenKind

    case object LiteralChar extends TokenKind

    case object LiteralFloat extends TokenKind

    case object LiteralFloat32 extends TokenKind

    case object LiteralFloat64 extends TokenKind

    case object LiteralInt extends TokenKind

    case object LiteralInt16 extends TokenKind

    case object LiteralInt32 extends TokenKind

    case object LiteralInt64 extends TokenKind

    case object LiteralInt8 extends TokenKind

    case object LiteralRegex extends TokenKind

    case object LiteralString extends TokenKind

    case object LiteralStringInterpolationL extends TokenKind

    case object LiteralStringInterpolationR extends TokenKind

    case object MapHash extends TokenKind

    case object Minus extends TokenKind

    case object NameLowercase extends TokenKind

    case object NameMath extends TokenKind

    case object NameUppercase extends TokenKind

    case object ParenL extends TokenKind

    case object ParenR extends TokenKind

    case object Plus extends TokenKind

    case object Semi extends TokenKind

    case object SetHash extends TokenKind

    case object Slash extends TokenKind

    case object Star extends TokenKind

    case object Tick extends TokenKind

    case object Tilde extends TokenKind

    case object Underscore extends TokenKind

    case object VectorHash extends TokenKind

    /** A special end-of-file token. */
    case object Eof extends TokenKind

    /** A special token representing a malformed token, including the error that caused it. */
    case class Err(error: LexerError) extends TokenKind

  }

}
