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

sealed trait TokenKind

// NOTE: Tokens are named for 'what they are' rather than 'what they represent'.
// So '::' is not named 'Cons' but instead 'ColonColon' as the lexer should be oblivious to the concept of cons

// NOTE: Builtin type keywords like 'Float32' are lexed into 'Float32Keyword' whereas Float32 *literals* are lexed to 'Float32'
// This aligns naming of literals with other types such as 'String' and 'Bool'

object TokenKind {
  case object LParen extends TokenKind

  case object AngledPlus extends TokenKind

  case object LCurly extends TokenKind

  case object RCurly extends TokenKind

  case object LBracket extends TokenKind

  case object RBracket extends TokenKind

  case object Eq extends TokenKind

  case object Semi extends TokenKind

  case object Dot extends TokenKind

  case object DotDot extends TokenKind

  case object Comma extends TokenKind

  case object Colon extends TokenKind

  case object ColonColon extends TokenKind

  case object ColonEqual extends TokenKind

  case object Arrow extends TokenKind

  case object Plus extends TokenKind

  case object Minus extends TokenKind

  case object Hash extends TokenKind

  case object Star extends TokenKind

  case object StarStar extends TokenKind

  case object Slash extends TokenKind

  case object LAngle extends TokenKind

  case object RAngle extends TokenKind

  case object LAngleEqual extends TokenKind

  case object RAngleEqual extends TokenKind

  case object Equal extends TokenKind

  case object EqualEqual extends TokenKind

  case object At extends TokenKind

  case object BackArrow extends TokenKind

  case object AndKeyword extends TokenKind

  case object OrKeyword extends TokenKind

  case object ModKeyword extends TokenKind

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

  case object NamespaceKeyword extends TokenKind

  case object NullKeyword extends TokenKind

  case object OpaqueKeyword extends TokenKind

  case object OverrideKeyword extends TokenKind

  case object PubKeyword extends TokenKind

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

  case object LineComment extends TokenKind

  case object BlockComment extends TokenKind

  case class Err(kind: LexerErr) extends TokenKind

  case object Eof extends TokenKind

}

sealed trait LexerErr

object LexerErr {

  case object UnexpectedSymbol extends LexerErr

  case object UnterminatedString extends LexerErr

  case object UnterminatedChar extends LexerErr

  case object DoubleDottedNumber extends LexerErr

  case object MalformedNumber extends LexerErr

  case object MalformedNumberType extends LexerErr

}
