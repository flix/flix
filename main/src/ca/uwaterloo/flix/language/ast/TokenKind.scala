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

sealed trait TokenKind

// NOTE: Tokens are named for 'what they are' rather than 'what they represent'.
// So '::' is not named 'Cons' but instead 'ColonColon' as the lexer should be oblivious to the concept of cons

// NOTE: Builtin type keywords like 'Float32' are lexed into 'Float32Keyword' whereas Float32 *literals* are lexed to 'Float32'
// This aligns naming of literals with other types such as 'String' and 'Bool'

object TokenKind {
  case object Ampersand extends TokenKind

  case object AngledEqual extends TokenKind

  case object AngledEqualEqual extends TokenKind

  case object AngledPlus extends TokenKind

  case object Annotation extends TokenKind

  case object Arrow extends TokenKind

  case object At extends TokenKind

  case object BackArrow extends TokenKind

  case object Backslash extends TokenKind

  case object Bang extends TokenKind

  case object Bar extends TokenKind

  case object BigDecimal extends TokenKind

  case object BigInt extends TokenKind

  case object BuiltIn extends TokenKind

  case object Caret extends TokenKind

  case object Char extends TokenKind

  case object Colon extends TokenKind

  case object ColonColon extends TokenKind

  case object ColonEqual extends TokenKind

  case object Comma extends TokenKind

  case object CommentBlock extends TokenKind

  case object CommentLine extends TokenKind

  case object Dollar extends TokenKind

  case object Dot extends TokenKind

  case object DotDot extends TokenKind

  case object Equal extends TokenKind

  case object EqualEqual extends TokenKind

  case object Float32 extends TokenKind

  case object Float64 extends TokenKind

  case object Hash extends TokenKind

  case object HoleAnonymous extends TokenKind

  case object HoleNamed extends TokenKind

  case object HoleVariable extends TokenKind

  case object InfixFunction extends TokenKind

  case object Int16 extends TokenKind

  case object Int32 extends TokenKind

  case object Int64 extends TokenKind

  case object Int8 extends TokenKind

  case object KeywordAbsent extends TokenKind

  case object KeywordAlias extends TokenKind

  case object KeywordAnd extends TokenKind

  case object KeywordAs extends TokenKind

  case object KeywordBigDecimal extends TokenKind

  case object KeywordBigInt extends TokenKind

  case object KeywordBool extends TokenKind

  case object KeywordCase extends TokenKind

  case object KeywordCatch extends TokenKind

  case object KeywordChan extends TokenKind

  case object KeywordChar extends TokenKind

  case object KeywordClass extends TokenKind

  case object KeywordDef extends TokenKind

  case object KeywordDeref extends TokenKind

  case object KeywordDiscard extends TokenKind

  case object KeywordElse extends TokenKind

  case object KeywordEnum extends TokenKind

  case object KeywordFalse extends TokenKind

  case object KeywordFix extends TokenKind

  case object KeywordFloat32 extends TokenKind

  case object KeywordFloat64 extends TokenKind

  case object KeywordForA extends TokenKind

  case object KeywordForce extends TokenKind

  case object KeywordForeach extends TokenKind

  case object KeywordForM extends TokenKind

  case object KeywordIf extends TokenKind

  case object KeywordImport extends TokenKind

  case object KeywordImpure extends TokenKind

  case object KeywordInline extends TokenKind

  case object KeywordInstance extends TokenKind

  case object KeywordInt16 extends TokenKind

  case object KeywordInt32 extends TokenKind

  case object KeywordInt64 extends TokenKind

  case object KeywordInt8 extends TokenKind

  case object KeywordInto extends TokenKind

  case object KeywordLat extends TokenKind

  case object KeywordLaw extends TokenKind

  case object KeywordLawful extends TokenKind

  case object KeywordLazy extends TokenKind

  case object KeywordLet extends TokenKind

  case object KeywordMatch extends TokenKind

  case object KeywordMod extends TokenKind

  case object KeywordNamespace extends TokenKind

  case object KeywordNil extends TokenKind

  case object KeywordNot extends TokenKind

  case object KeywordNull extends TokenKind

  case object KeywordObject extends TokenKind

  case object KeywordOpaque extends TokenKind

  case object KeywordOr extends TokenKind

  case object KeywordOverride extends TokenKind

  case object KeywordPar extends TokenKind

  case object KeywordPredicate extends TokenKind

  case object KeywordPresent extends TokenKind

  case object KeywordPub extends TokenKind

  case object KeywordPure extends TokenKind

  case object KeywordRead extends TokenKind

  case object KeywordRecordRow extends TokenKind

  case object KeywordRef extends TokenKind

  case object KeywordRegion extends TokenKind

  case object KeywordReify extends TokenKind

  case object KeywordReifyBool extends TokenKind

  case object KeywordReifyEff extends TokenKind

  case object KeywordReifyType extends TokenKind

  case object KeywordRel extends TokenKind

  case object KeywordRem extends TokenKind

  case object KeywordSchemaRow extends TokenKind

  case object KeywordSealed extends TokenKind

  case object KeywordSet extends TokenKind

  case object KeywordSpawn extends TokenKind

  case object KeywordStatic extends TokenKind

  case object KeywordString extends TokenKind

  case object KeywordTrue extends TokenKind

  case object KeywordType extends TokenKind

  case object KeywordTypeMatch extends TokenKind

  case object KeywordUnit extends TokenKind

  case object KeywordUppercaseRegion extends TokenKind

  case object KeywordUppercaseType extends TokenKind

  case object KeywordUse extends TokenKind

  case object KeywordWhere extends TokenKind

  case object KeywordWith extends TokenKind

  case object KeywordWrite extends TokenKind

  case object KeywordYield extends TokenKind

  case object LAngle extends TokenKind

  case object LAngleEqual extends TokenKind

  case object LBracket extends TokenKind

  case object LCurly extends TokenKind

  case object LParen extends TokenKind

  case object Minus extends TokenKind

  case object NameGreek extends TokenKind

  case object NameJava extends TokenKind

  case object NameLowercase extends TokenKind

  case object NameMath extends TokenKind

  case object NameUppercase extends TokenKind

  case object Plus extends TokenKind

  case object RAngle extends TokenKind

  case object RAngleEqual extends TokenKind

  case object RBracket extends TokenKind

  case object RCurly extends TokenKind

  case object RParen extends TokenKind

  case object Semi extends TokenKind

  case object Slash extends TokenKind

  case object Star extends TokenKind

  case object StarStar extends TokenKind

  case object String extends TokenKind

  case object TripleAmpersand extends TokenKind

  case object TripleBar extends TokenKind

  case object TripleCaret extends TokenKind

  case object TripleLAngle extends TokenKind

  case object TripleQuestionMark extends TokenKind

  case object TripleRAngle extends TokenKind

  case object TripleTilde extends TokenKind

  case object Underscore extends TokenKind

  case object UserDefinedOperator extends TokenKind

  /**
   * A special token emitted instead of halting the lexer when an error is encountered.
   * @param kind the kind of error found.
   */
  case class Err(kind: LexerErr) extends TokenKind

  /**
   * A virtual token signalling END-OF-FILE.
   */
  case object Eof extends TokenKind
}
