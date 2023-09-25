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

/* Tokens are named for 'what they are' rather than 'what they represent'.
 * So '::' is not named 'Cons' but instead 'ColonColon' as the lexer should be oblivious to the concept of cons
 *
 * Tokens belonging to some conceptual group should have the group name as prefix.
 * So 'LiteralInt32' is preferred over 'Int32Literal'
 */

object TokenKind {
  case object Ampersand extends TokenKind

  case object AngledEqual extends TokenKind

  case object AngledPlus extends TokenKind

  case object AngleL extends TokenKind

  case object AngleLEqual extends TokenKind

  case object AngleR extends TokenKind

  case object AngleREqual extends TokenKind

  case object Annotation extends TokenKind

  case object ArrayHash extends TokenKind

  case object Arrow extends TokenKind

  case object At extends TokenKind

  case object BackArrow extends TokenKind

  case object Backslash extends TokenKind

  case object Bang extends TokenKind

  case object Bar extends TokenKind

  case object BracketL extends TokenKind

  case object BracketR extends TokenKind

  case object BuiltIn extends TokenKind

  case object Caret extends TokenKind

  case object Colon extends TokenKind

  case object ColonColon extends TokenKind

  case object ColonEqual extends TokenKind

  case object Comma extends TokenKind

  case object CommentBlock extends TokenKind

  case object CommentLine extends TokenKind

  case object CurlyL extends TokenKind

  case object CurlyR extends TokenKind

  case object Dot extends TokenKind

  case object Equal extends TokenKind

  case object EqualEqual extends TokenKind

  case object Hash extends TokenKind

  case object HoleAnonymous extends TokenKind

  case object HoleNamed extends TokenKind

  case object HoleVariable extends TokenKind

  case object InfixFunction extends TokenKind

  case object KeywordAlias extends TokenKind

  case object KeywordAnd extends TokenKind

  case object KeywordAs extends TokenKind

  case object KeywordCase extends TokenKind

  case object KeywordCatch extends TokenKind

  case object KeywordCheckedCast extends TokenKind

  case object KeywordCheckedECast extends TokenKind

  case object KeywordChoose extends TokenKind

  case object KeywordClass extends TokenKind

  case object KeywordDebug extends TokenKind

  case object KeywordDef extends TokenKind

  case object KeywordDeref extends TokenKind

  case object KeywordDiscard extends TokenKind

  case object KeywordDo extends TokenKind

  case object KeywordEff extends TokenKind

  case object KeywordElse extends TokenKind

  case object KeywordEnum extends TokenKind

  case object KeywordFalse extends TokenKind

  case object KeywordFix extends TokenKind

  case object KeywordFor extends TokenKind

  case object KeywordForA extends TokenKind

  case object KeywordForall extends TokenKind

  case object KeywordForce extends TokenKind

  case object KeywordForeach extends TokenKind

  case object KeywordForM extends TokenKind

  case object KeywordFrom extends TokenKind

  case object KeywordGet extends TokenKind

  case object KeywordIf extends TokenKind

  case object KeywordImport extends TokenKind

  case object KeywordImpure extends TokenKind

  case object KeywordInject extends TokenKind

  case object KeywordInline extends TokenKind

  case object KeywordInstance extends TokenKind

  case object KeywordInto extends TokenKind

  case object KeywordLaw extends TokenKind

  case object KeywordLawful extends TokenKind

  case object KeywordLazy extends TokenKind

  case object KeywordLet extends TokenKind

  case object KeywordMasked_cast extends TokenKind

  case object KeywordMatch extends TokenKind

  case object KeywordMod extends TokenKind

  case object KeywordNew extends TokenKind

  case object KeywordNot extends TokenKind

  case object KeywordNull extends TokenKind

  case object KeywordOpen extends TokenKind

  case object KeywordOpen_as extends TokenKind

  case object KeywordOr extends TokenKind

  case object KeywordOverride extends TokenKind

  case object KeywordPar extends TokenKind

  case object KeywordProject extends TokenKind

  case object KeywordPub extends TokenKind

  case object KeywordPure extends TokenKind

  case object KeywordQuery extends TokenKind

  case object KeywordRef extends TokenKind

  case object KeywordRegion extends TokenKind

  case object KeywordRestrictable extends TokenKind

  case object KeywordResume extends TokenKind

  case object KeywordSealed extends TokenKind

  case object KeywordSelect extends TokenKind

  case object KeywordSolve extends TokenKind

  case object KeywordSpawn extends TokenKind

  case object KeywordStatic extends TokenKind

  case object KeywordTrue extends TokenKind

  case object KeywordTry extends TokenKind

  case object KeywordType extends TokenKind

  case object KeywordTypeMatch extends TokenKind

  case object KeywordUnchecked_cast extends TokenKind

  case object KeywordUse extends TokenKind

  case object KeywordWhere extends TokenKind

  case object KeywordWith extends TokenKind

  case object KeywordWithout extends TokenKind

  case object KeywordYield extends TokenKind

  case object ListHash extends TokenKind

  case object LiteralBigDecimal extends TokenKind

  case object LiteralBigInt extends TokenKind

  case object LiteralChar extends TokenKind

  case object LiteralFloat32 extends TokenKind

  case object LiteralFloat64 extends TokenKind

  case object LiteralInt64 extends TokenKind

  case object LiteralInt8 extends TokenKind

  case object LiteralString extends TokenKind

  case object MapHash extends TokenKind

  case object Minus extends TokenKind

  case object NameGreek extends TokenKind

  case object NameJava extends TokenKind

  case object NameLowerCase extends TokenKind

  case object NameMath extends TokenKind

  case object NameUpperCase extends TokenKind

  case object ParenL extends TokenKind

  case object ParenR extends TokenKind

  case object Plus extends TokenKind

  case object Semi extends TokenKind

  case object SetHash extends TokenKind

  case object Slash extends TokenKind

  case object Star extends TokenKind

  case object StarStar extends TokenKind

  case object TripleAmpersand extends TokenKind

  case object TripleAngleL extends TokenKind

  case object TripleAngleR extends TokenKind

  case object TripleBar extends TokenKind

  case object TripleCaret extends TokenKind

  case object TripleQuestionMark extends TokenKind

  case object TripleTilde extends TokenKind

  case object Underscore extends TokenKind

  case object UserDefinedOperator extends TokenKind

  case object VectorHash extends TokenKind

  /** A special token emitted instead of halting the lexer when an error is encountered.
   * @param kind the kind of error found.
   */
  case class Err(kind: TokenErrorKind) extends TokenKind

  /** A virtual token signalling END-OF-FILE.
   */
  case object Eof extends TokenKind
}
