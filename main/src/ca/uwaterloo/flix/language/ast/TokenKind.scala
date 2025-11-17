/*
 * Copyright 2023 Herluf Baggesen
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
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.errors.LexerError

sealed trait TokenKind {
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
      case TokenKind.DotDotDot => "'...'"
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
      case TokenKind.KeywordLaw => "'law'"
      case TokenKind.KeywordLawful => "'lawful'"
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
      case TokenKind.KeywordOverride => "'override'"
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
      case TokenKind.KeywordThrow => "'throw'"
      case TokenKind.KeywordTrait => "'trait'"
      case TokenKind.KeywordTrue => "'true'"
      case TokenKind.KeywordTry => "'try'"
      case TokenKind.KeywordType => "'type'"
      case TokenKind.KeywordTypeMatch => "'typematch'"
      case TokenKind.KeywordUncheckedCast => "'unchecked_cast'"
      case TokenKind.KeywordUniv => "'univ'"
      case TokenKind.KeywordUnsafe => "'unsafe'"
      case TokenKind.KeywordUnsafely => "'unsafely'"
      case TokenKind.KeywordUse => "'use'"
      case TokenKind.KeywordWhere => "'where'"
      case TokenKind.KeywordWith => "'with'"
      case TokenKind.KeywordWithout => "'without'"
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

  /** Returns `true` if this token is a doc, line or block comment. */
  def isComment: Boolean = this match {
    case TokenKind.CommentDoc => true
    case _ if this.isCommentNonDoc => true
    case _ => false
  }

  /** Returns `true` if this token is a keyword. */
  def isKeyword: Boolean = this match {
    case TokenKind.KeywordAlias => true
    case TokenKind.KeywordAnd => true
    case TokenKind.KeywordAs => true
    case TokenKind.KeywordCase => true
    case TokenKind.KeywordCatch => true
    case TokenKind.KeywordCheckedCast => true
    case TokenKind.KeywordCheckedECast => true
    case TokenKind.KeywordChoose => true
    case TokenKind.KeywordChooseStar => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordDiscard => true
    case TokenKind.KeywordEMatch => true
    case TokenKind.KeywordEff => true
    case TokenKind.KeywordElse => true
    case TokenKind.KeywordEnum => true
    case TokenKind.KeywordFalse => true
    case TokenKind.KeywordFix => true
    case TokenKind.KeywordForA => true
    case TokenKind.KeywordForM => true
    case TokenKind.KeywordForall => true
    case TokenKind.KeywordForce => true
    case TokenKind.KeywordForeach => true
    case TokenKind.KeywordFrom => true
    case TokenKind.KeywordHandler => true
    case TokenKind.KeywordIf => true
    case TokenKind.KeywordImport => true
    case TokenKind.KeywordInject => true
    case TokenKind.KeywordInstance => true
    case TokenKind.KeywordInstanceOf => true
    case TokenKind.KeywordInto => true
    case TokenKind.KeywordLaw => true
    case TokenKind.KeywordLawful => true
    case TokenKind.KeywordLazy => true
    case TokenKind.KeywordLet => true
    case TokenKind.KeywordMatch => true
    case TokenKind.KeywordMod => true
    case TokenKind.KeywordNew => true
    case TokenKind.KeywordNot => true
    case TokenKind.KeywordNull => true
    case TokenKind.KeywordOpenVariant => true
    case TokenKind.KeywordOpenVariantAs => true
    case TokenKind.KeywordOr => true
    case TokenKind.KeywordOverride => true
    case TokenKind.KeywordPQuery => true
    case TokenKind.KeywordPSolve => true
    case TokenKind.KeywordPar => true
    case TokenKind.KeywordProject => true
    case TokenKind.KeywordPub => true
    case TokenKind.KeywordQuery => true
    case TokenKind.KeywordRegion => true
    case TokenKind.KeywordRestrictable => true
    case TokenKind.KeywordRun => true
    case TokenKind.KeywordRvadd => true
    case TokenKind.KeywordRvand => true
    case TokenKind.KeywordRvnot => true
    case TokenKind.KeywordRvsub => true
    case TokenKind.KeywordSealed => true
    case TokenKind.KeywordSelect => true
    case TokenKind.KeywordSolve => true
    case TokenKind.KeywordSpawn => true
    case TokenKind.KeywordStaticLowercase => true
    case TokenKind.KeywordStruct => true
    case TokenKind.KeywordThrow => true
    case TokenKind.KeywordTrait => true
    case TokenKind.KeywordTrue => true
    case TokenKind.KeywordTry => true
    case TokenKind.KeywordType => true
    case TokenKind.KeywordTypeMatch => true
    case TokenKind.KeywordUncheckedCast => true
    case TokenKind.KeywordUniv => true
    case TokenKind.KeywordUnsafe => true
    case TokenKind.KeywordUnsafely => true
    case TokenKind.KeywordUse => true
    case TokenKind.KeywordWhere => true
    case TokenKind.KeywordWith => true
    case TokenKind.KeywordWithout => true
    case TokenKind.KeywordXor => true
    case TokenKind.KeywordXvar => true
    case TokenKind.KeywordYield => true
    case _ => false
  }

  /** Returns `true` if this token is a modifier (e.g. `pub`). */
  def isModifier: Boolean = this match {
    case TokenKind.KeywordLawful => true
    case TokenKind.KeywordMut => true
    case TokenKind.KeywordOverride => true
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
    * Note that a [[TokenKind.CommentDoc]], a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInDecl: Boolean = this.isModifier || (this match {
    case TokenKind.Annotation => true
    case TokenKind.CommentDoc => true
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

  /** Returns `true` if this token can validly appear after [[TokenKind.CommentDoc]]. */
  def isDocumentable: Boolean = this match {
    case TokenKind.KeywordCase => true
    case TokenKind.KeywordLaw => true
    case _ if this.isFirstInDecl => true
    case _ => false
  }

  /**
    * Returns `true` if this token can validly appear as the first token in a declaration within an effect.
    *
    * Note that a [[TokenKind.CommentDoc]], a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInEffDecl: Boolean = this match {
    case TokenKind.Annotation => true
    case TokenKind.CommentDoc => true
    case TokenKind.KeywordDef => true
    case _ if this.isModifier => true
    case _ => false
  }

  /**
    * Returns `true` if this token can validly appear as the first token in a declaration within an instance.
    *
    * Note that a [[TokenKind.CommentDoc]], a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInInstanceDecl: Boolean = this match {
    case TokenKind.Annotation => true
    case TokenKind.CommentDoc => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordType => true
    case _ if this.isModifier => true
    case _ => false
  }

  /**
    * Returns `true` if this token can validly appear as the first token in a declaration within a trait.
    * *
    * * Note that a [[TokenKind.CommentDoc]], a modifier (see [[isModifier]]) and/or [[TokenKind.Annotation]] may lead a declaration.
    */
  def isFirstInTraitDecl: Boolean = this match {
    case TokenKind.Annotation => true
    case TokenKind.CommentDoc => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordLaw => true
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
    case TokenKind.DotDotDot => true
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
    case TokenKind.KeywordTrue => true
    case TokenKind.KeywordTry => true
    case TokenKind.KeywordTypeMatch => true
    case TokenKind.KeywordUncheckedCast => true
    case TokenKind.KeywordUnsafe => true
    case TokenKind.KeywordUnsafely => true
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

  case object KeywordUnsafely extends TokenKind

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
