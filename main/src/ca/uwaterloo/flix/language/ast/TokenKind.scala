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

/**
 * Tokens are named for 'what they are' rather than 'what they represent'.
 * So '::' is not named 'Cons' but instead 'ColonColon' as the lexer should be oblivious to the concept of cons
 *
 * Tokens belonging to some conceptual group should have the group name as prefix.
 * So 'LiteralInt32' is preferred over 'Int32Literal'
 */
object TokenKind {
  case object Ampersand extends TokenKind

// NOTE: Builtin type keywords like 'Float32' are lexed into 'Float32Keyword' whereas Float32 *literals* are lexed to 'Float32'
// This aligns naming of literals with other types such as 'String' and 'Bool'

object TokenKind {
  case object LParen extends TokenKind

  case object AngledPlus extends TokenKind

  case object LCurly extends TokenKind

  case object RCurly extends TokenKind

  case object LBracket extends TokenKind

  case object RBracket extends TokenKind

  case object Bang extends TokenKind

  case object Semi extends TokenKind

  case object Dot extends TokenKind

  case object DotDot extends TokenKind

  case object Comma extends TokenKind

  case object Colon extends TokenKind

  case object ColonColon extends TokenKind

  case object ColonEqual extends TokenKind

  case object Comma extends TokenKind

  case object CommentBlock extends TokenKind

  case object CommentLine extends TokenKind

  case object CurlyL extends TokenKind

  case object CurlyR extends TokenKind

  case object Dollar extends TokenKind

  case object Dot extends TokenKind

  case object Equal extends TokenKind

  case object EqualEqual extends TokenKind

  case object Hash extends TokenKind

  case object HoleAnonymous extends TokenKind

  case object HoleNamed extends TokenKind

  case object HoleVariable extends TokenKind

  case object InfixFunction extends TokenKind

  case object KeywordAbsent extends TokenKind

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

  case object KeywordMaskedCast extends TokenKind

  case object KeywordMatch extends TokenKind

  case object KeywordMod extends TokenKind

  case object KeywordNew extends TokenKind

  case object KeywordNot extends TokenKind

  case object KeywordNull extends TokenKind

  case object KeywordOpen extends TokenKind

  case object KeywordOpenAs extends TokenKind

  case object KeywordOr extends TokenKind

  case object KeywordOverride extends TokenKind

  case object KeywordPar extends TokenKind

  case object KeywordPresent extends TokenKind

  case object KeywordProject extends TokenKind

  case object KeywordPub extends TokenKind

  case object KeywordPure extends TokenKind

  case object KeywordQuery extends TokenKind

  case object KeywordRef extends TokenKind

  case object KeywordRegion extends TokenKind

  case object KeywordRelationalChoose extends TokenKind

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

  case object KeywordUncheckedCast extends TokenKind

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

  case object LiteralInt8 extends TokenKind

  case object LiteralInt16 extends TokenKind

  case object LiteralInt32 extends TokenKind

  case object LiteralInt64 extends TokenKind

  case object LiteralString extends TokenKind

  case object LiteralStringInterpolationL extends TokenKind

  case object LiteralStringInterpolationR extends TokenKind

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

  case object Minus extends TokenKind

  case object Hash extends TokenKind

  case object Dollar extends TokenKind

  case object Bar extends TokenKind

  case object Caret extends TokenKind

  case object Ampersand extends TokenKind

  case object TripleAmpersand extends TokenKind

  case object TripleLAngle extends TokenKind

  case object TripleRAngle extends TokenKind

  case object TripleQuestionMark extends TokenKind

  case object TripleCaret extends TokenKind

  case object TripleBar extends TokenKind

  case object TripleTilde extends TokenKind

  case object AngledEqual extends TokenKind

  case object AngledEqualEqual extends TokenKind

  case object AngledPlus extends TokenKind

  case object Star extends TokenKind

  case object StarStar extends TokenKind

  case object Tilde extends TokenKind

  case object TripleAmpersand extends TokenKind

  case object TripleAngleL extends TokenKind

  case object TripleAngleR extends TokenKind

  case object TripleBar extends TokenKind

  case object TripleCaret extends TokenKind

  case object TripleTilde extends TokenKind

  case object Underscore extends TokenKind

  case object LAngle extends TokenKind

  case object RAngle extends TokenKind

  case object LAngleEqual extends TokenKind

  case object RAngleEqual extends TokenKind

  case object Equal extends TokenKind

  case object EqualEqual extends TokenKind

  case object UserDefinedOperator extends TokenKind

  case object At extends TokenKind

  case object InfixFunction extends TokenKind

  case object BackArrow extends TokenKind

  case object AndKeyword extends TokenKind

  case object OrKeyword extends TokenKind

  case object ModKeyword extends TokenKind

  case object ForeachKeyword extends TokenKind

  case object ForMKeyword extends TokenKind

  case object ForAKeyword extends TokenKind

  case object NotKeyword extends TokenKind

  case object RemKeyword extends TokenKind

  case object AbsentKeyword extends TokenKind

  case object BoolKeyword extends TokenKind

  case object UnitKeyword extends TokenKind

  case object CharKeyword extends TokenKind

  case object Float32Keyword extends TokenKind

  case object Float64Keyword extends TokenKind

  case object Int8Keyword extends TokenKind

  case object Int16Keyword extends TokenKind

  case object Int32Keyword extends TokenKind

  case object Int64Keyword extends TokenKind

  case object StringKeyword extends TokenKind

  case object BigIntKeyword extends TokenKind

  case object BigDecimalKeyword extends TokenKind

  case object ImpureKeyword extends TokenKind

  case object NilKeyword extends TokenKind

  case object PredicateKeyword extends TokenKind

  case object PresentKeyword extends TokenKind

  case object PureKeyword extends TokenKind

  case object ReadKeyword extends TokenKind

  case object RecordRowKeyword extends TokenKind

  case object UppercaseRegionKeyword extends TokenKind

  case object SchemaRowKeyword extends TokenKind

  case object UppercaseTypeKeyword extends TokenKind

  case object WriteKeyword extends TokenKind

  case object AliasKeyword extends TokenKind

  case object CaseKeyword extends TokenKind

  case object CatchKeyword extends TokenKind

  case object ChanKeyword extends TokenKind

  case object ClassKeyword extends TokenKind

  case object DefKeyword extends TokenKind

  case object DerefKeyword extends TokenKind

  case object ElseKeyword extends TokenKind

  case object EnumKeyword extends TokenKind

  case object FalseKeyword extends TokenKind

  case object FixKeyword extends TokenKind

  case object ForceKeyword extends TokenKind

  case object IfKeyword extends TokenKind

  case object ImportKeyword extends TokenKind

  case object InlineKeyword extends TokenKind

  case object InstanceKeyword extends TokenKind

  case object IntoKeyword extends TokenKind

  case object LatKeyword extends TokenKind

  case object LawKeyword extends TokenKind

  case object LawfulKeyword extends TokenKind

  case object LazyKeyword extends TokenKind

  case object LetKeyword extends TokenKind

  case object MatchKeyword extends TokenKind

  case object TypeMatchKeyword extends TokenKind

  case object NamespaceKeyword extends TokenKind

  case object NullKeyword extends TokenKind

  case object OpaqueKeyword extends TokenKind

  case object OverrideKeyword extends TokenKind

  case object ParKeyword extends TokenKind

  case object YieldKeyword extends TokenKind

  case object PubKeyword extends TokenKind

  case object AsKeyword extends TokenKind

  case object RefKeyword extends TokenKind

  case object RegionKeyword extends TokenKind

  case object ReifyKeyword extends TokenKind

  case object ReifyBoolKeyword extends TokenKind

  case object ReifyEffKeyword extends TokenKind

  case object ReifyTypeKeyword extends TokenKind

  case object RelKeyword extends TokenKind

  case object SealedKeyword extends TokenKind

  case object SetKeyword extends TokenKind

  case object SpawnKeyword extends TokenKind

  case object StaticKeyword extends TokenKind

  case object TrueKeyword extends TokenKind

  case object TypeKeyword extends TokenKind

  case object UseKeyword extends TokenKind

  case object WhereKeyword extends TokenKind

  case object WithKeyword extends TokenKind

  case object DiscardKeyword extends TokenKind

  case object ObjectKeyword extends TokenKind

  case object UppercaseName extends TokenKind

  case object LowercaseName extends TokenKind

  case object MathName extends TokenKind

  case object GreekName extends TokenKind

  case object Float32 extends TokenKind

  case object Float64 extends TokenKind

  case object Int8 extends TokenKind

  case object Int16 extends TokenKind

  case object Int32 extends TokenKind

  case object Int64 extends TokenKind

  case object BigInt extends TokenKind

  case object BigDecimal extends TokenKind

  case object String extends TokenKind

  case object Char extends TokenKind

  case object Annotation extends TokenKind

  case object AnonymousHole extends TokenKind

  case object JavaName extends TokenKind

  case object BuiltIn extends TokenKind

  case object NamedHole extends TokenKind

  case object VariableHole extends TokenKind

  case object LineComment extends TokenKind

  case object BlockComment extends TokenKind

  case class Err(kind: LexerErr) extends TokenKind

  case object Eof extends TokenKind

}

sealed trait LexerErr

object LexerErr {

  case object UnexpectedChar extends LexerErr

  case object UnterminatedString extends LexerErr

  case object UnterminatedChar extends LexerErr

  case object UnterminatedInfixFunction extends LexerErr

  case object DoubleDottedNumber extends LexerErr

  case object MalformedNumber extends LexerErr

  case object BlockCommentTooDeep extends LexerErr

  case object UnterminatedBlockComment extends LexerErr

  case object UnterminatedBuiltIn extends LexerErr
}
