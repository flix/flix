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
    * Returns the canonical text of this token, if it has a fixed lexeme.
    */
  def fixedLexeme: Option[String] = this match {
    // Punctuation and symbols
    case TokenKind.Ampersand                   => Some("&")
    case TokenKind.AngleL                      => Some("<")
    case TokenKind.AngleLEqual                 => Some("<=")
    case TokenKind.AngleR                      => Some(">")
    case TokenKind.AngleREqual                 => Some(">=")
    case TokenKind.AngledEqual                 => Some("<=>")
    case TokenKind.AngledPlus                  => Some("<+>")
    case TokenKind.ArrayHash                   => Some("Array#")
    case TokenKind.ArrowThickR                 => Some("=>")
    case TokenKind.ArrowThinL                  => Some("<-")
    case TokenKind.ArrowThinRTight             => Some("->")
    case TokenKind.ArrowThinRWhitespace        => Some("->")
    case TokenKind.At                          => Some("@")
    case TokenKind.Backslash                   => Some("\\")
    case TokenKind.Bang                        => Some("!")
    case TokenKind.BangEqual                   => Some("!=")
    case TokenKind.Bar                         => Some("|")
    case TokenKind.BarHash                     => Some("|#")
    case TokenKind.BracketL                    => Some("[")
    case TokenKind.BracketR                    => Some("]")
    case TokenKind.Caret                       => Some("^")
    case TokenKind.Colon                       => Some(":")
    case TokenKind.ColonColon                  => Some("::")
    case TokenKind.ColonColonColon             => Some(":::")
    case TokenKind.ColonMinus                  => Some(":-")
    case TokenKind.Comma                       => Some(",")
    case TokenKind.CurlyL                      => Some("{")
    case TokenKind.CurlyR                      => Some("}")
    case TokenKind.Dollar                      => Some("$")
    case TokenKind.Dot                         => Some(".")
    case TokenKind.DotWhiteSpace               => Some(". ")
    case TokenKind.Equal                       => Some("=")
    case TokenKind.EqualEqual                  => Some("==")
    case TokenKind.Hash                        => Some("#")
    case TokenKind.HashBar                     => Some("#|")
    case TokenKind.HashCurlyL                  => Some("#{")
    case TokenKind.HashParenL                  => Some("#(")
    case TokenKind.HoleAnonymous               => Some("???")
    case TokenKind.ListHash                    => Some("List#")
    case TokenKind.LiteralStringInterpolationL => Some("${")
    case TokenKind.LiteralStringInterpolationR => Some("}\"")
    case TokenKind.MapHash                     => Some("Map#")
    case TokenKind.Minus                       => Some("-")
    case TokenKind.ParenL                      => Some("(")
    case TokenKind.ParenR                      => Some(")")
    case TokenKind.Plus                        => Some("+")
    case TokenKind.Semi                        => Some(";")
    case TokenKind.SetHash                     => Some("Set#")
    case TokenKind.Slash                       => Some("/")
    case TokenKind.Star                        => Some("*")
    case TokenKind.Tick                        => Some("`")
    case TokenKind.Tilde                       => Some("~")
    case TokenKind.Underscore                  => Some("_")
    case TokenKind.VectorHash                  => Some("Vector#")

    // Keywords
    case TokenKind.KeywordAlias                => Some("alias")
    case TokenKind.KeywordAnd                  => Some("and")
    case TokenKind.KeywordAs                   => Some("as")
    case TokenKind.KeywordCase                 => Some("case")
    case TokenKind.KeywordCatch                => Some("catch")
    case TokenKind.KeywordCheckedCast          => Some("checked_cast")
    case TokenKind.KeywordCheckedECast         => Some("checked_ecast")
    case TokenKind.KeywordChoose               => Some("choose")
    case TokenKind.KeywordChooseStar           => Some("choose*")
    case TokenKind.KeywordDef                  => Some("def")
    case TokenKind.KeywordDiscard              => Some("discard")
    case TokenKind.KeywordEMatch               => Some("ematch")
    case TokenKind.KeywordEff                  => Some("eff")
    case TokenKind.KeywordElse                 => Some("else")
    case TokenKind.KeywordEnum                 => Some("enum")
    case TokenKind.KeywordFalse                => Some("false")
    case TokenKind.KeywordFix                  => Some("fix")
    case TokenKind.KeywordForA                 => Some("forA")
    case TokenKind.KeywordForM                 => Some("forM")
    case TokenKind.KeywordForall               => Some("forall")
    case TokenKind.KeywordForce                => Some("force")
    case TokenKind.KeywordForeach              => Some("foreach")
    case TokenKind.KeywordFrom                 => Some("from")
    case TokenKind.KeywordHandler              => Some("handler")
    case TokenKind.KeywordIf                   => Some("if")
    case TokenKind.KeywordImport               => Some("import")
    case TokenKind.KeywordInject               => Some("inject")
    case TokenKind.KeywordInstance             => Some("instance")
    case TokenKind.KeywordInstanceOf           => Some("instanceof")
    case TokenKind.KeywordInto                 => Some("into")
    case TokenKind.KeywordLaw                  => Some("law")
    case TokenKind.KeywordLawful               => Some("lawful")
    case TokenKind.KeywordLazy                 => Some("lazy")
    case TokenKind.KeywordLet                  => Some("let")
    case TokenKind.KeywordMatch                => Some("match")
    case TokenKind.KeywordMod                  => Some("mod")
    case TokenKind.KeywordMut                  => Some("mut")
    case TokenKind.KeywordNew                  => Some("new")
    case TokenKind.KeywordNot                  => Some("not")
    case TokenKind.KeywordNull                 => Some("null")
    case TokenKind.KeywordOpenVariant          => Some("open_variant")
    case TokenKind.KeywordOpenVariantAs        => Some("open_variant_as")
    case TokenKind.KeywordOr                   => Some("or")
    case TokenKind.KeywordOverride             => Some("override")
    case TokenKind.KeywordPQuery               => Some("pquery")
    case TokenKind.KeywordPSolve               => Some("psolve")
    case TokenKind.KeywordPar                  => Some("par")
    case TokenKind.KeywordProject              => Some("project")
    case TokenKind.KeywordPub                  => Some("pub")
    case TokenKind.KeywordQuery                => Some("query")
    case TokenKind.KeywordRedef                => Some("redef")
    case TokenKind.KeywordRegion               => Some("region")
    case TokenKind.KeywordRestrictable         => Some("restrictable")
    case TokenKind.KeywordRun                  => Some("run")
    case TokenKind.KeywordRvadd                => Some("rvadd")
    case TokenKind.KeywordRvand                => Some("rvand")
    case TokenKind.KeywordRvnot                => Some("rvnot")
    case TokenKind.KeywordRvsub                => Some("rvsub")
    case TokenKind.KeywordSealed               => Some("sealed")
    case TokenKind.KeywordSelect               => Some("select")
    case TokenKind.KeywordSolve                => Some("solve")
    case TokenKind.KeywordSpawn                => Some("spawn")
    case TokenKind.KeywordStaticLowercase      => Some("static")
    case TokenKind.KeywordStaticUppercase      => Some("Static")
    case TokenKind.KeywordStruct               => Some("struct")
    case TokenKind.KeywordSuper                => Some("super")
    case TokenKind.KeywordThrow                => Some("throw")
    case TokenKind.KeywordTrait                => Some("trait")
    case TokenKind.KeywordTrue                 => Some("true")
    case TokenKind.KeywordTry                  => Some("try")
    case TokenKind.KeywordType                 => Some("type")
    case TokenKind.KeywordUncheckedCast        => Some("unchecked_cast")
    case TokenKind.KeywordUniv                 => Some("univ")
    case TokenKind.KeywordUnsafe               => Some("unsafe")
    case TokenKind.KeywordUse                  => Some("use")
    case TokenKind.KeywordWhere                => Some("where")
    case TokenKind.KeywordWith                 => Some("with")
    case TokenKind.KeywordXor                  => Some("xor")
    case TokenKind.KeywordXvar                 => Some("xvar")
    case TokenKind.KeywordYield                => Some("yield")

    // Markers and variables that to not have a fixed lexeme
    case _                                     => None
  }

  /**
    * A string representation of the [[TokenKind]] suitable for printing.
    * For instance TokenKind.Ampersand.display gives "'&'".
    */
  def display: String = fixedLexeme match {
    case Some(s) => s"'$s'"
    case None    => this match {
      case TokenKind.Annotation        => "<annotation>"
      case TokenKind.BuiltIn           => "<built in>"
      case TokenKind.CommentBlock      => "<block comment>"
      case TokenKind.CommentDoc        => "<doc comment>"
      case TokenKind.CommentLine       => "<comment>"
      case TokenKind.DebugInterpolator => "<debug-interpolator>"
      case TokenKind.GenericOperator   => "<user-defined operator>"
      case TokenKind.HoleNamed         => "<named hole>"
      case TokenKind.HoleVariable      => "<variable hole>"
      case TokenKind.LiteralBigDecimal => "'<digits>ff'"
      case TokenKind.LiteralBigInt     => "'<digits>ii'"
      case TokenKind.LiteralChar       => "<char>"
      case TokenKind.LiteralFloat      => "'<digits>.<digits>'"
      case TokenKind.LiteralFloat32    => "'<digits>f32'"
      case TokenKind.LiteralFloat64    => "'<digits>f64'"
      case TokenKind.LiteralInt        => "'<digits>'"
      case TokenKind.LiteralInt16      => "'<digits>i16'"
      case TokenKind.LiteralInt32      => "'<digits>i32'"
      case TokenKind.LiteralInt64      => "'<digits>i64'"
      case TokenKind.LiteralInt8       => "'<digits>i8'"
      case TokenKind.LiteralRegex      => "<regex>"
      case TokenKind.LiteralString     => "<string>"
      case TokenKind.NameLowercase     => "<name>"
      case TokenKind.NameMath          => "<math name>"
      case TokenKind.NameUppercase     => "<Name>"
      case TokenKind.Eof               => "<end-of-file>"
      case TokenKind.Err(_)            => "<error>"
      case _                           => sys.error(s"display: unhandled TokenKind: $this")
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
  def isKeyword: Boolean = TokenKind.keywords.contains(this)

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

  case object KeywordSuper extends TokenKind

  case object KeywordThrow extends TokenKind

  case object KeywordTrait extends TokenKind

  case object KeywordTrue extends TokenKind

  case object KeywordTry extends TokenKind

  case object KeywordType extends TokenKind

  case object KeywordUncheckedCast extends TokenKind

  case object KeywordUniv extends TokenKind

  case object KeywordUnsafe extends TokenKind

  case object KeywordUse extends TokenKind

  case object KeywordWhere extends TokenKind

  case object KeywordWith extends TokenKind


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

  /**
    * The set of all [[TokenKind]]s that are Flix keywords.
    * Note that this is the single source of truth, therefore must be kept in sync.
    */
  val keywords: Set[TokenKind] = Set(
    KeywordAlias, KeywordAnd, KeywordAs, KeywordCase, KeywordCatch,
    KeywordCheckedCast, KeywordCheckedECast, KeywordChoose, KeywordChooseStar,
    KeywordDef, KeywordDiscard, KeywordEMatch, KeywordEff, KeywordElse,
    KeywordEnum, KeywordFalse, KeywordFix, KeywordForA, KeywordForM,
    KeywordForall, KeywordForce, KeywordForeach, KeywordFrom, KeywordHandler,
    KeywordIf, KeywordImport, KeywordInject, KeywordInstance, KeywordInstanceOf,
    KeywordInto, KeywordLaw, KeywordLawful, KeywordLazy, KeywordLet,
    KeywordMatch, KeywordMod, KeywordMut, KeywordNew, KeywordNot, KeywordNull,
    KeywordOpenVariant, KeywordOpenVariantAs, KeywordOr, KeywordOverride,
    KeywordPQuery, KeywordPSolve, KeywordPar, KeywordProject, KeywordPub,
    KeywordQuery, KeywordRedef, KeywordRegion, KeywordRestrictable, KeywordRun,
    KeywordRvadd, KeywordRvand, KeywordRvnot, KeywordRvsub, KeywordSealed,
    KeywordSelect, KeywordSolve, KeywordSpawn, KeywordStaticLowercase,
    KeywordStaticUppercase, KeywordStruct, KeywordSuper, KeywordThrow,
    KeywordTrait, KeywordTrue, KeywordTry, KeywordType, KeywordUncheckedCast,
    KeywordUniv, KeywordUnsafe, KeywordUse, KeywordWhere, KeywordWith,
    KeywordXor, KeywordXvar, KeywordYield
  )

  /**
    * Returns the set of all fixed lexemes corresponding to keywords.
    */
  val keywordLexemes: Set[String] = keywords.flatMap(_.fixedLexeme)
}
