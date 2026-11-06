/*
 * Copyright 2023 Herluf Baggesen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.errors.LexerError

/**
  * The kind of a [[Token]].
  *
  * @param flags a bit set of the predicates that hold for this kind, see [[TokenKind.Flags]].
  */
sealed abstract class TokenKind(private val flags: Int = 0) {
  /**
    * A string representation of the [[TokenKind]] suitable for printing.
    * For instance TokenKind.Ampersand.display gives "'&'".
    */
  def display: String = {
    this match {
      case TokenKind.Ampersand => "'&'"
      case TokenKind.AngleL => "'<'"
      case TokenKind.AngleLEqual => "'<='"
      case TokenKind.AngleR => "'>'"
      case TokenKind.AngleREqual => "'>='"
      case TokenKind.AngledEqual => "'<=>'"
      case TokenKind.AngledPlus => "'<+>'"
      case TokenKind.Annotation => "<annotation>"
      case TokenKind.ArrayHash => "'Array#'"
      case TokenKind.ArrowThickR => "'=>'"
      case TokenKind.ArrowThinL => "'<-'"
      case TokenKind.ArrowThinRTight => "'->'"
      case TokenKind.ArrowThinRWhitespace => "'->'"
      case TokenKind.At => "'@'"
      case TokenKind.Backslash => "'\\'"
      case TokenKind.Bang => "'!'"
      case TokenKind.BangEqual => "'!='"
      case TokenKind.Bar => "'|'"
      case TokenKind.BarHash => "'|#'"
      case TokenKind.BracketL => "'['"
      case TokenKind.BracketR => "']'"
      case TokenKind.BuiltIn => "<built in>"
      case TokenKind.Caret => "'^'"
      case TokenKind.Colon => "':'"
      case TokenKind.ColonColon => "'::'"
      case TokenKind.ColonColonColon => "':::'"
      case TokenKind.ColonColonTight => "'::'"
      case TokenKind.ColonMinus => "':-'"
      case TokenKind.Comma => "','"
      case TokenKind.CommentBlock => "<block comment>"
      case TokenKind.CommentDoc => "<doc comment>"
      case TokenKind.CommentLine => "<comment>"
      case TokenKind.CurlyL => "'{'"
      case TokenKind.CurlyR => "'}'"
      case TokenKind.DebugInterpolator => "<debug-interpolator>"
      case TokenKind.Dollar => "'$'"
      case TokenKind.Dot => "'.'"
      case TokenKind.DotWhiteSpace => "'. '"
      case TokenKind.Equal => "'='"
      case TokenKind.EqualEqual => "'=='"
      case TokenKind.GenericOperator => "<user-defined operator>"
      case TokenKind.Hash => "'#'"
      case TokenKind.HashBar => "'#|'"
      case TokenKind.HashCurlyL => "'#{'"
      case TokenKind.HashParenL => "'#('"
      case TokenKind.HoleAnonymous => "'???'"
      case TokenKind.HoleNamed => "<named hole>"
      case TokenKind.HoleVariable => "<variable hole>"
      case TokenKind.KeywordAlias => "'alias'"
      case TokenKind.KeywordAnd => "'and'"
      case TokenKind.KeywordAs => "'as'"
      case TokenKind.KeywordCase => "'case'"
      case TokenKind.KeywordCatch => "'catch'"
      case TokenKind.KeywordCheckedCast => "'checked_cast'"
      case TokenKind.KeywordCheckedECast => "'checked_ecast'"
      case TokenKind.KeywordChoose => "'choose'"
      case TokenKind.KeywordChooseStar => "'choose*'"
      case TokenKind.KeywordDef => "'def'"
      case TokenKind.KeywordDiscard => "'discard'"
      case TokenKind.KeywordEMatch => "'ematch'"
      case TokenKind.KeywordEff => "'eff'"
      case TokenKind.KeywordElse => "'else'"
      case TokenKind.KeywordEnum => "'enum'"
      case TokenKind.KeywordFalse => "'false'"
      case TokenKind.KeywordFix => "'fix'"
      case TokenKind.KeywordForA => "'forA'"
      case TokenKind.KeywordForM => "'forM'"
      case TokenKind.KeywordForall => "'forall'"
      case TokenKind.KeywordForce => "'force'"
      case TokenKind.KeywordForeach => "'foreach'"
      case TokenKind.KeywordFrom => "'from'"
      case TokenKind.KeywordHandler => "'handler'"
      case TokenKind.KeywordIf => "'if'"
      case TokenKind.KeywordImport => "'import'"
      case TokenKind.KeywordInject => "'inject'"
      case TokenKind.KeywordInstance => "'instance'"
      case TokenKind.KeywordInstanceOf => "'instanceof'"
      case TokenKind.KeywordInto => "'into'"
      case TokenKind.KeywordLazy => "'lazy'"
      case TokenKind.KeywordLet => "'let'"
      case TokenKind.KeywordMatch => "'match'"
      case TokenKind.KeywordMod => "'mod'"
      case TokenKind.KeywordMut => "'mut'"
      case TokenKind.KeywordNew => "'new'"
      case TokenKind.KeywordNot => "'not'"
      case TokenKind.KeywordNull => "'null'"
      case TokenKind.KeywordOpenVariant => "'open_variant'"
      case TokenKind.KeywordOpenVariantAs => "'open_variant_as'"
      case TokenKind.KeywordOr => "'or'"
      case TokenKind.KeywordPQuery => "'pquery'"
      case TokenKind.KeywordPSolve => "'psolve'"
      case TokenKind.KeywordPar => "'par'"
      case TokenKind.KeywordProject => "'project'"
      case TokenKind.KeywordPub => "'pub'"
      case TokenKind.KeywordQuery => "'query'"
      case TokenKind.KeywordRedef => "'redef'"
      case TokenKind.KeywordRegion => "'region'"
      case TokenKind.KeywordRestrictable => "'restrictable'"
      case TokenKind.KeywordRun => "'run'"
      case TokenKind.KeywordRvadd => "'rvadd'"
      case TokenKind.KeywordRvand => "'rvand'"
      case TokenKind.KeywordRvnot => "'rvnot'"
      case TokenKind.KeywordRvsub => "'rvsub'"
      case TokenKind.KeywordSealed => "'sealed'"
      case TokenKind.KeywordSelect => "'select'"
      case TokenKind.KeywordSolve => "'solve'"
      case TokenKind.KeywordSpawn => "'spawn'"
      case TokenKind.KeywordStaticLowercase => "'static'"
      case TokenKind.KeywordStaticUppercase => "'Static'"
      case TokenKind.KeywordStruct => "'struct'"
      case TokenKind.KeywordSuper => "'super'"
      case TokenKind.KeywordThrow => "'throw'"
      case TokenKind.KeywordTrait => "'trait'"
      case TokenKind.KeywordTrue => "'true'"
      case TokenKind.KeywordTry => "'try'"
      case TokenKind.KeywordType => "'type'"
      case TokenKind.KeywordUncheckedCast => "'unchecked_cast'"
      case TokenKind.KeywordUniv => "'univ'"
      case TokenKind.KeywordUnsafe => "'unsafe'"
      case TokenKind.KeywordUse => "'use'"
      case TokenKind.KeywordWhere => "'where'"
      case TokenKind.KeywordWith => "'with'"

      case TokenKind.KeywordXor => "'xor'"
      case TokenKind.KeywordXvar => "'xvar'"
      case TokenKind.KeywordYield => "'yield'"
      case TokenKind.ListHash => "'List#'"
      case TokenKind.LiteralBigDecimal => "'<digits>ff'"
      case TokenKind.LiteralBigInt => "'<digits>ii'"
      case TokenKind.LiteralChar => "<char>"
      case TokenKind.LiteralFloat => "'<digits>.<digits>'"
      case TokenKind.LiteralFloat32 => "'<digits>f32'"
      case TokenKind.LiteralFloat64 => "'<digits>f64'"
      case TokenKind.LiteralInt => "'<digits>'"
      case TokenKind.LiteralInt16 => "'<digits>i16'"
      case TokenKind.LiteralInt32 => "'<digits>i32'"
      case TokenKind.LiteralInt64 => "'<digits>i64'"
      case TokenKind.LiteralInt8 => "'<digits>i8'"
      case TokenKind.LiteralRegex => "<regex>"
      case TokenKind.LiteralString => "<string>"
      case TokenKind.LiteralStringInterpolationL => "'${'"
      case TokenKind.LiteralStringInterpolationR => "'}\"'"
      case TokenKind.MapHash => "'Map#'"
      case TokenKind.Minus => "'-'"
      case TokenKind.NameLowercase => "<name>"
      case TokenKind.NameMath => "<math name>"
      case TokenKind.NameUppercase => "<Name>"
      case TokenKind.ParenL => "'('"
      case TokenKind.ParenR => "')'"
      case TokenKind.Plus => "'+'"
      case TokenKind.Semi => "';'"
      case TokenKind.SetHash => "'Set#'"
      case TokenKind.Slash => "'/'"
      case TokenKind.Star => "'*'"
      case TokenKind.Tick => "'`'"
      case TokenKind.Tilde => "'~'"
      case TokenKind.Underscore => "'_'"
      case TokenKind.VectorHash => "'Vector#'"
      case TokenKind.Eof => "<end-of-file>"
      case TokenKind.Err(_) => "<error>"
    }
  }

  /** Returns `true` if this token is a line or block comment. */
  def isCommentNonDoc: Boolean = this match {
    case TokenKind.CommentBlock => true
    case TokenKind.CommentLine => true
    case _ => false
  }

  /** Returns `true` if this token is a doc comment. */
  def isCommentDoc: Boolean = this match {
    case TokenKind.CommentDoc => true
    case _ => false
  }

  /** Returns `true` if this token is a doc, line or block comment. */
  def isComment: Boolean = this.isCommentDoc || this.isCommentNonDoc

  /** Returns `true` if this token is a keyword. */
  def isKeyword: Boolean = (flags & TokenKind.Flags.Keyword) != 0

  /** Returns `true` if this token is a modifier (e.g. `pub`). */
  def isModifier: Boolean = this match {
    case TokenKind.KeywordMut => true
    case TokenKind.KeywordPub => true
    case TokenKind.KeywordSealed => true
    case _ => false
  }

  /** Returns `true` if this token is an operator (e.g. `+`). */
  def isOperator: Boolean = this match {
    // Arithmetic operators
    case TokenKind.Plus => true
    case TokenKind.Minus => true
    case TokenKind.Star => true
    case TokenKind.Slash => true

    // Comparison operators
    case TokenKind.AngleL => true
    case TokenKind.AngleR => true
    case TokenKind.AngledEqual => true
    case TokenKind.AngleLEqual => true
    case TokenKind.AngleREqual => true
    case TokenKind.EqualEqual => true
    case TokenKind.BangEqual => true

    // User-defined operators
    case TokenKind.GenericOperator => true

    // List operators
    case TokenKind.ColonColonColon => true

    // Note: `KeywordAnd` and `KeywordOr` are explicitly not supported due to laziness.
    // Note: `ColonColon` is not supported due to desugaring of long lists.

    case _ => false
  }

  /**
    * Returns `true` if this token can validly appear as the first token of a declaration.
    *
    * Note that a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInDecl: Boolean = this.isModifier || (this match {
    case TokenKind.Annotation => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordEff => true
    case TokenKind.KeywordEnum => true
    case TokenKind.KeywordInstance => true
    case TokenKind.KeywordMod => true
    case TokenKind.KeywordRestrictable => true
    case TokenKind.KeywordStruct => true
    case TokenKind.KeywordTrait => true
    case TokenKind.KeywordType => true
    case _ => false
  })

  /**
    * Returns `true` if this token can validly appear as the first token in a declaration within an effect.
    *
    * Note that a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInEffDecl: Boolean = this match {
    case TokenKind.Annotation => true
    case TokenKind.KeywordDef => true
    case _ if this.isModifier => true
    case _ => false
  }

  /**
    * Returns `true` if this token can validly appear as the first token in a declaration within an instance.
    *
    * Note that a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInInstanceDecl: Boolean = this match {
    case TokenKind.Annotation => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordType => true
    case _ if this.isModifier => true
    case _ => false
  }

  /**
    * Returns `true` if this token can validly appear as the first token in a declaration within a trait.
    *
    * Note that a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInTraitDecl: Boolean = this match {
    case TokenKind.Annotation => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordType => true
    case _ if this.isModifier => true
    case _ => false
  }

  /** Returns `true` if this token can validly appear as the first token of an expression. */
  def isFirstInExp: Boolean = this match {
    case TokenKind.Annotation => true
    case TokenKind.ArrayHash => true
    case TokenKind.BuiltIn => true
    case TokenKind.CurlyL => true
    case TokenKind.DebugInterpolator => true
    case TokenKind.HashBar => true
    case TokenKind.HashCurlyL => true
    case TokenKind.HashParenL => true
    case TokenKind.HoleAnonymous => true
    case TokenKind.HoleNamed => true
    case TokenKind.HoleVariable => true
    case TokenKind.KeywordCheckedCast => true
    case TokenKind.KeywordCheckedECast => true
    case TokenKind.KeywordChoose => true
    case TokenKind.KeywordChooseStar => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordDiscard => true
    case TokenKind.KeywordEMatch => true
    case TokenKind.KeywordFalse => true
    case TokenKind.KeywordForA => true
    case TokenKind.KeywordForM => true
    case TokenKind.KeywordForce => true
    case TokenKind.KeywordForeach => true
    case TokenKind.KeywordHandler => true
    case TokenKind.KeywordIf => true
    case TokenKind.KeywordImport => true
    case TokenKind.KeywordInject => true
    case TokenKind.KeywordLazy => true
    case TokenKind.KeywordLet => true
    case TokenKind.KeywordMatch => true
    case TokenKind.KeywordNew => true
    case TokenKind.KeywordNot => true
    case TokenKind.KeywordNull => true
    case TokenKind.KeywordOpenVariant => true
    case TokenKind.KeywordOpenVariantAs => true
    case TokenKind.KeywordPQuery => true
    case TokenKind.KeywordPSolve => true
    case TokenKind.KeywordPar => true
    case TokenKind.KeywordQuery => true
    case TokenKind.KeywordRegion => true
    case TokenKind.KeywordRun => true
    case TokenKind.KeywordSelect => true
    case TokenKind.KeywordSolve => true
    case TokenKind.KeywordSpawn => true
    case TokenKind.KeywordStaticUppercase => true
    case TokenKind.KeywordSuper => true
    case TokenKind.KeywordTrue => true
    case TokenKind.KeywordTry => true
    case TokenKind.KeywordUncheckedCast => true
    case TokenKind.KeywordUnsafe => true
    case TokenKind.KeywordUse => true
    case TokenKind.KeywordXvar => true
    case TokenKind.ListHash => true
    case TokenKind.LiteralBigDecimal => true
    case TokenKind.LiteralBigInt => true
    case TokenKind.LiteralChar => true
    case TokenKind.LiteralFloat => true
    case TokenKind.LiteralFloat32 => true
    case TokenKind.LiteralFloat64 => true
    case TokenKind.LiteralInt => true
    case TokenKind.LiteralInt16 => true
    case TokenKind.LiteralInt32 => true
    case TokenKind.LiteralInt64 => true
    case TokenKind.LiteralInt8 => true
    case TokenKind.LiteralRegex => true
    case TokenKind.LiteralString => true
    case TokenKind.LiteralStringInterpolationL => true
    case TokenKind.MapHash => true
    case TokenKind.Minus => true
    case TokenKind.NameLowercase => true
    case TokenKind.NameMath => true
    case TokenKind.NameUppercase => true
    case TokenKind.ParenL => true
    case TokenKind.Plus => true
    case TokenKind.SetHash => true
    case TokenKind.Underscore => true
    case TokenKind.VectorHash => true
    case _ => false
  }

  /**
    * Returns `true` if this token is not a binary operator. If such a token is encountered
    * without a preceding semicolon, we assume the semicolon was forgotten.
    *
    * Note that this list should be extended in the future with more TokenKinds.
    */
  def notBinaryOperator: Boolean = this match {
    case TokenKind.KeywordDiscard => true
    case TokenKind.KeywordForeach => true
    case TokenKind.KeywordLet     => true
    case _ => false
  }

  /**
    * Returns `true` if this token can follow a binary operator.
    *
    * Note: This is only used for error-recovery, not for parsing itself.
    * It determines whether a binary operator could be missing between two expressions.
    *
    * Returns `true` e.g. for `NameLowercase`:
    *   x y   // 'y' can follow a binary operator, so a missing operator is inferred
    *
    * Returns `false` e.g. for `CurlyL`:
    *   x { }  // '{' cannot follow a binary operator, so no inference is attempted
    */
  def canFollowBinaryOperator: Boolean = this match {
    case TokenKind.HoleAnonymous                => true
    case TokenKind.KeywordFalse                 => true
    case TokenKind.KeywordIf                    => true
    case TokenKind.KeywordMatch                 => true
    case TokenKind.KeywordNot                   => true
    case TokenKind.KeywordNull                  => true
    case TokenKind.KeywordTrue                  => true
    case TokenKind.NameLowercase                => true
    case TokenKind.NameUppercase                => true
    case TokenKind.NameMath                     => true
    case TokenKind.LiteralInt                   => true
    case TokenKind.LiteralInt8                  => true
    case TokenKind.LiteralInt16                 => true
    case TokenKind.LiteralInt32                 => true
    case TokenKind.LiteralInt64                 => true
    case TokenKind.LiteralBigInt                => true
    case TokenKind.LiteralFloat                 => true
    case TokenKind.LiteralFloat32               => true
    case TokenKind.LiteralFloat64               => true
    case TokenKind.LiteralBigDecimal            => true
    case TokenKind.LiteralChar                  => true
    case TokenKind.LiteralRegex                 => true
    case TokenKind.LiteralString                => true
    case TokenKind.LiteralStringInterpolationL  => true
    case _                                      => false
  }

  /** Returns `true` if this token can validly appear as the first token of a type. */
  def isFirstInType: Boolean = this match {
    case TokenKind.AngleL => true
    case TokenKind.CurlyL => true
    case TokenKind.HashCurlyL => true
    case TokenKind.HashParenL => true
    case TokenKind.KeywordFalse => true
    case TokenKind.KeywordNot => true
    case TokenKind.KeywordRvnot => true
    case TokenKind.KeywordStaticUppercase => true
    case TokenKind.KeywordTrue => true
    case TokenKind.KeywordUniv => true
    case TokenKind.NameLowercase => true
    case TokenKind.NameMath => true
    case TokenKind.NameUppercase => true
    case TokenKind.ParenL => true
    case TokenKind.Tilde => true
    case TokenKind.Underscore => true
    case _ => false
  }

  /** Returns `true` if this token can validly appear as the first token of a pattern. */
  def isFirstInPattern: Boolean = this match {
    case TokenKind.CurlyL => true
    case TokenKind.KeywordFalse => true
    case TokenKind.KeywordNull => true
    case TokenKind.KeywordQuery => true
    case TokenKind.KeywordTrue => true
    case TokenKind.LiteralBigDecimal => true
    case TokenKind.LiteralBigInt => true
    case TokenKind.LiteralChar => true
    case TokenKind.LiteralFloat => true
    case TokenKind.LiteralFloat32 => true
    case TokenKind.LiteralFloat64 => true
    case TokenKind.LiteralInt => true
    case TokenKind.LiteralInt16 => true
    case TokenKind.LiteralInt32 => true
    case TokenKind.LiteralInt64 => true
    case TokenKind.LiteralInt8 => true
    case TokenKind.LiteralRegex => true
    case TokenKind.LiteralString => true
    case TokenKind.Minus => true
    case TokenKind.NameLowercase => true
    case TokenKind.NameMath => true
    case TokenKind.NameUppercase => true
    case TokenKind.ParenL => true
    case TokenKind.Underscore => true
    case _ => false
  }

  /** Returns `true` if this token can validly appear as the first token of a record operation. */
  def isFirstInRecordOp: Boolean = this match {
    case TokenKind.Minus => true
    case TokenKind.NameLowercase => true
    case TokenKind.Plus => true
    case _ => false
  }

  /** Returns `true` if this token is a literal value. */
  def isLiteral: Boolean = this match {
    case TokenKind.DebugInterpolator => true
    case TokenKind.LiteralBigDecimal => true
    case TokenKind.LiteralBigInt => true
    case TokenKind.LiteralChar => true
    case TokenKind.LiteralFloat => true
    case TokenKind.LiteralFloat32 => true
    case TokenKind.LiteralFloat64 => true
    case TokenKind.LiteralInt => true
    case TokenKind.LiteralInt16 => true
    case TokenKind.LiteralInt32 => true
    case TokenKind.LiteralInt64 => true
    case TokenKind.LiteralInt8 => true
    case TokenKind.LiteralRegex => true
    case TokenKind.LiteralString => true
    case TokenKind.LiteralStringInterpolationL => true
    case TokenKind.LiteralStringInterpolationR => true
    case _ => false
  }

  /*
    * The following isRecover* functions represent sets of TokenKinds used for error-recovery in the parser.
    * When parsing sequences of expressions in a loop and an unexpected token is found,
    * a recovery means breaking the loop.
    *
    * An example would be:
    * def foo(): Unit = bar(1,
    * enum Legumes { case Beans, Chickpeas }
    * Here we recover from parsing argument expressions of the unfinished call to `bar`
    * and still discover `Legumes`, because isRecoverDecl return true for TokenKind.KeywordEnum.
    */

  /** Returns `true` if this token warrants breaking a declaration parsing loop. */
  def isRecoverInDecl: Boolean =
    this.isFirstInDecl

  /** Returns `true` if this token warrants breaking a module parsing loop. */
  def isRecoverInMod: Boolean = this match {
    case TokenKind.CurlyR => true
    case _ if this.isFirstInDecl => true
    case _ => false
  }

  /** Returns `true` if this token warrants breaking an expression parsing loop. */
  def isRecoverInExpr: Boolean = this match {
    case TokenKind.Semi => true
    case _ if this.isFirstInDecl => true
    case _ => false
  }

  /** Returns `true` if this token warrants breaking a type parsing loop. */
  def isRecoverInType: Boolean = this match {
    case TokenKind.Equal => true
    case TokenKind.Semi => true
    case _ if this.isFirstInDecl => true
    case _ => false
  }

  /** Returns `true` if this token warrants breaking a pattern parsing loop. */
  def isRecoverInPattern: Boolean = this match {
    case TokenKind.ArrowThickR => true
    case TokenKind.KeywordCase => true
    case _ if this.isFirstInDecl => true
    case _ => false
  }

  /** Returns `true` if this token warrants breaking a top-level use or import parsing loop. */
  def isRecoverInUseOrImport: Boolean = this match {
    case TokenKind.KeywordImport => true
    case TokenKind.KeywordUse => true
    case TokenKind.Semi => true
    case _ if this.isFirstInDecl => true
    case _ => false
  }

  /** Returns `true` if this token warrants breaking a function parameter parsing loop. */
  def isRecoverInParameters: Boolean = this match {
    case TokenKind.Colon => true
    case TokenKind.Equal => true
    case _ if this.isFirstInDecl => true
    case _ => false
  }

  /**
    * Returns `true` if this token is a semantic token used for syntax highlighting in LSP (Language Server Protocol).
    *
    * This code must stay in sync with [[ca.uwaterloo.flix.api.lsp.provider.SemanticTokensProvider]].
    */
  def isSemantic: Boolean =
    isKeyword || isModifier || isComment
}

/**
  * Notes on naming:
  *   - Tokens are named for 'what they are' rather than 'what they represent'.
  *     So '::' is not named 'Cons' but 'ColonColon' as tokens should be oblivious to the concept of cons.
  *   - Tokens are conceptually grouped by prefix, so 'LiteralInt32' is preferred over 'Int32Literal'.
  */
object TokenKind {

  /**
    * Bit flags carried by every token kind, one bit per predicate.
    *
    * The constants must stay `final val` without a type annotation so Scala folds them to literals.
    */
  private object Flags {
    final val Keyword = 1 << 0
  }

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

  case object ColonColonTight extends TokenKind

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

  case object KeywordAlias extends TokenKind(Flags.Keyword)

  case object KeywordAnd extends TokenKind(Flags.Keyword)

  case object KeywordAs extends TokenKind(Flags.Keyword)

  case object KeywordCase extends TokenKind(Flags.Keyword)

  case object KeywordCatch extends TokenKind(Flags.Keyword)

  case object KeywordCheckedCast extends TokenKind(Flags.Keyword)

  case object KeywordCheckedECast extends TokenKind(Flags.Keyword)

  case object KeywordChoose extends TokenKind(Flags.Keyword)

  case object KeywordChooseStar extends TokenKind(Flags.Keyword)

  case object KeywordDef extends TokenKind(Flags.Keyword)

  case object KeywordDiscard extends TokenKind(Flags.Keyword)

  case object KeywordEMatch extends TokenKind(Flags.Keyword)

  case object KeywordEff extends TokenKind(Flags.Keyword)

  case object KeywordElse extends TokenKind(Flags.Keyword)

  case object KeywordEnum extends TokenKind(Flags.Keyword)

  case object KeywordFalse extends TokenKind(Flags.Keyword)

  case object KeywordFix extends TokenKind(Flags.Keyword)

  case object KeywordForA extends TokenKind(Flags.Keyword)

  case object KeywordForM extends TokenKind(Flags.Keyword)

  case object KeywordForall extends TokenKind(Flags.Keyword)

  case object KeywordForce extends TokenKind(Flags.Keyword)

  case object KeywordForeach extends TokenKind(Flags.Keyword)

  case object KeywordFrom extends TokenKind(Flags.Keyword)

  case object KeywordHandler extends TokenKind(Flags.Keyword)

  case object KeywordIf extends TokenKind(Flags.Keyword)

  case object KeywordImport extends TokenKind(Flags.Keyword)

  case object KeywordInject extends TokenKind(Flags.Keyword)

  case object KeywordInstance extends TokenKind(Flags.Keyword)

  case object KeywordInstanceOf extends TokenKind(Flags.Keyword)

  case object KeywordInto extends TokenKind(Flags.Keyword)

  case object KeywordLazy extends TokenKind(Flags.Keyword)

  case object KeywordLet extends TokenKind(Flags.Keyword)

  case object KeywordMatch extends TokenKind(Flags.Keyword)

  case object KeywordMod extends TokenKind(Flags.Keyword)

  case object KeywordMut extends TokenKind(Flags.Keyword)

  case object KeywordNew extends TokenKind(Flags.Keyword)

  case object KeywordNot extends TokenKind(Flags.Keyword)

  case object KeywordNull extends TokenKind(Flags.Keyword)

  case object KeywordOpenVariant extends TokenKind(Flags.Keyword)

  case object KeywordOpenVariantAs extends TokenKind(Flags.Keyword)

  case object KeywordOr extends TokenKind(Flags.Keyword)

  case object KeywordPQuery extends TokenKind(Flags.Keyword)

  case object KeywordPSolve extends TokenKind(Flags.Keyword)

  case object KeywordPar extends TokenKind(Flags.Keyword)

  case object KeywordProject extends TokenKind(Flags.Keyword)

  case object KeywordPub extends TokenKind(Flags.Keyword)

  case object KeywordQuery extends TokenKind(Flags.Keyword)

  case object KeywordRedef extends TokenKind(Flags.Keyword)

  case object KeywordRegion extends TokenKind(Flags.Keyword)

  case object KeywordRestrictable extends TokenKind(Flags.Keyword)

  case object KeywordRun extends TokenKind(Flags.Keyword)

  case object KeywordRvadd extends TokenKind(Flags.Keyword)

  case object KeywordRvand extends TokenKind(Flags.Keyword)

  case object KeywordRvnot extends TokenKind(Flags.Keyword)

  case object KeywordRvsub extends TokenKind(Flags.Keyword)

  case object KeywordSealed extends TokenKind(Flags.Keyword)

  case object KeywordSelect extends TokenKind(Flags.Keyword)

  case object KeywordSolve extends TokenKind(Flags.Keyword)

  case object KeywordSpawn extends TokenKind(Flags.Keyword)

  case object KeywordStaticLowercase extends TokenKind(Flags.Keyword)

  case object KeywordStaticUppercase extends TokenKind

  case object KeywordStruct extends TokenKind(Flags.Keyword)

  case object KeywordSuper extends TokenKind(Flags.Keyword)

  case object KeywordThrow extends TokenKind(Flags.Keyword)

  case object KeywordTrait extends TokenKind(Flags.Keyword)

  case object KeywordTrue extends TokenKind(Flags.Keyword)

  case object KeywordTry extends TokenKind(Flags.Keyword)

  case object KeywordType extends TokenKind(Flags.Keyword)

  case object KeywordUncheckedCast extends TokenKind(Flags.Keyword)

  case object KeywordUniv extends TokenKind(Flags.Keyword)

  case object KeywordUnsafe extends TokenKind(Flags.Keyword)

  case object KeywordUse extends TokenKind(Flags.Keyword)

  case object KeywordWhere extends TokenKind(Flags.Keyword)

  case object KeywordWith extends TokenKind(Flags.Keyword)


  case object KeywordXor extends TokenKind(Flags.Keyword)

  case object KeywordXvar extends TokenKind(Flags.Keyword)

  case object KeywordYield extends TokenKind(Flags.Keyword)

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
