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
    * Integer representation of the TokenKind.
    * Makes it possible to construct BitSets of TokenKinds.
    * In a BitSet, we want the smallest numeric values possible,
    * so in practice we just use a running serial number starting at 0.
    * Order does not matter.
    * When adding or removing TokenKinds, [[repr]] of later tokens kinds should be updated.
    */
  val repr: Int
}

/**
  * Tokens are named for 'what they are' rather than 'what they represent'.
  * So '::' is not named 'Cons' but instead 'ColonColon' as the lexer should be oblivious to the concept of cons
  *
  * Tokens belonging to some conceptual group should have the group name as prefix.
  * So 'LiteralInt32' is preferred over 'Int32Literal'
  */
object TokenKind {
  case object Ampersand extends TokenKind {
    val repr = 0
  }

  case object AngledEqual extends TokenKind {
    val repr = 1
  }

  case object AngledPlus extends TokenKind {
    val repr = 2
  }

  case object AngleL extends TokenKind {
    val repr = 3
  }

  case object AngleLEqual extends TokenKind {
    val repr = 4
  }

  case object AngleR extends TokenKind {
    val repr = 5
  }

  case object AngleREqual extends TokenKind {
    val repr = 6
  }

  case object Annotation extends TokenKind {
    val repr = 7
  }

  case object ArrayHash extends TokenKind {
    val repr = 8
  }

  case object ArrowThickR extends TokenKind {
    val repr = 9
  }

  case object ArrowThinL extends TokenKind {
    val repr = 10
  }

  case object ArrowThinR extends TokenKind {
    val repr = 11
  }

  case object At extends TokenKind {
    val repr = 12
  }

  case object Backslash extends TokenKind {
    val repr = 13
  }

  case object Bang extends TokenKind {
    val repr = 14
  }

  case object BangEqual extends TokenKind {
    val repr = 15
  }

  case object Bar extends TokenKind {
    val repr = 16
  }

  case object BracketL extends TokenKind {
    val repr = 17
  }

  case object BracketR extends TokenKind {
    val repr = 18
  }

  case object BuiltIn extends TokenKind {
    val repr = 19
  }

  case object Caret extends TokenKind {
    val repr = 20
  }

  case object Colon extends TokenKind {
    val repr = 21
  }

  case object ColonColon extends TokenKind {
    val repr = 22
  }

  case object ColonEqual extends TokenKind {
    val repr = 23
  }

  case object ColonMinus extends TokenKind {
    val repr = 24
  }

  case object Comma extends TokenKind {
    val repr = 25
  }

  case object CommentBlock extends TokenKind {
    val repr = 26
  }

  case object CommentLine extends TokenKind {
    val repr = 27
  }

  case object CommentDoc extends TokenKind {
    val repr = 28
  }

  case object CurlyL extends TokenKind {
    val repr = 29
  }

  case object CurlyR extends TokenKind {
    val repr = 30
  }

  case object Dollar extends TokenKind {
    val repr = 31
  }

  case object Dot extends TokenKind {
    val repr = 32
  }

  case object DotCurlyL extends TokenKind {
    val repr = 33
  }

  case object Equal extends TokenKind {
    val repr = 34
  }

  case object EqualEqual extends TokenKind {
    val repr = 35
  }

  case object Hash extends TokenKind {
    val repr = 36
  }

  case object HashCurlyL extends TokenKind {
    val repr = 37
  }

  case object HashParenL extends TokenKind {
    val repr = 38
  }

  case object HoleAnonymous extends TokenKind {
    val repr = 39
  }

  case object HoleNamed extends TokenKind {
    val repr = 40
  }

  case object HoleVariable extends TokenKind {
    val repr = 41
  }

  case object InfixFunction extends TokenKind {
    val repr = 42
  }

  case object KeywordAlias extends TokenKind {
    val repr = 43
  }

  case object KeywordAnd extends TokenKind {
    val repr = 44
  }

  case object KeywordAs extends TokenKind {
    val repr = 45
  }

  case object KeywordCase extends TokenKind {
    val repr = 46
  }

  case object KeywordCatch extends TokenKind {
    val repr = 47
  }

  case object KeywordCheckedCast extends TokenKind {
    val repr = 48
  }

  case object KeywordCheckedECast extends TokenKind {
    val repr = 49
  }

  case object KeywordChoose extends TokenKind {
    val repr = 50
  }

  case object KeywordChooseStar extends TokenKind {
    val repr = 51
  }

  case object KeywordDebug extends TokenKind {
    val repr = 52
  }

  case object KeywordDebugBang extends TokenKind {
    val repr = 53
  }

  case object KeywordDebugBangBang extends TokenKind {
    val repr = 54
  }

  case object KeywordDef extends TokenKind {
    val repr = 55
  }

  case object KeywordDeref extends TokenKind {
    val repr = 56
  }

  case object KeywordDiscard extends TokenKind {
    val repr = 57
  }

  case object KeywordDo extends TokenKind {
    val repr = 58
  }

  case object KeywordEff extends TokenKind {
    val repr = 59
  }

  case object KeywordElse extends TokenKind {
    val repr = 60
  }

  case object KeywordEnum extends TokenKind {
    val repr = 61
  }

  case object KeywordFalse extends TokenKind {
    val repr = 62
  }

  case object KeywordFix extends TokenKind {
    val repr = 63
  }

  case object KeywordForA extends TokenKind {
    val repr = 64
  }

  case object KeywordForall extends TokenKind {
    val repr = 65
  }

  case object KeywordForce extends TokenKind {
    val repr = 66
  }

  case object KeywordForeach extends TokenKind {
    val repr =67
  }

  case object KeywordForM extends TokenKind {
    val repr = 68
  }

  case object KeywordFrom extends TokenKind {
    val repr = 69
  }

  case object KeywordIf extends TokenKind {
    val repr = 70
  }

  case object KeywordImport extends TokenKind {
    val repr = 71
  }

  case object KeywordImpure extends TokenKind {
    val repr = 72
  }

  case object KeywordInject extends TokenKind {
    val repr = 73
  }

  case object KeywordInline extends TokenKind {
    val repr = 74
  }

  case object KeywordInstance extends TokenKind {
    val repr = 75
  }

  case object KeywordInstanceOf extends TokenKind {
    val repr = 76
  }

  case object KeywordInto extends TokenKind {
    val repr = 77
  }

  case object KeywordJavaGetField extends TokenKind {
    val repr = 78
  }

  case object KeywordJavaNew extends TokenKind {
    val repr = 79
  }

  case object KeywordJavaSetField extends TokenKind {
    val repr = 80
  }

  case object KeywordLaw extends TokenKind {
    val repr = 81
  }

  case object KeywordLawful extends TokenKind {
    val repr = 82
  }

  case object KeywordLazy extends TokenKind {
    val repr = 83
  }

  case object KeywordLet extends TokenKind {
    val repr = 84
  }

  case object KeywordMaskedCast extends TokenKind {
    val repr = 85
  }

  case object KeywordMatch extends TokenKind {
    val repr = 86
  }

  case object KeywordMod extends TokenKind {
    val repr = 87
  }

  case object KeywordNew extends TokenKind {
    val repr = 88
  }

  case object KeywordNot extends TokenKind {
    val repr = 89
  }

  case object KeywordNull extends TokenKind {
    val repr = 90
  }

  case object KeywordOpenVariant extends TokenKind {
    val repr = 91
  }

  case object KeywordOpenVariantAs extends TokenKind {
    val repr = 92
  }

  case object KeywordOr extends TokenKind {
    val repr = 93
  }

  case object KeywordOverride extends TokenKind {
    val repr = 94
  }

  case object KeywordPar extends TokenKind {
    val repr = 95
  }

  case object KeywordPub extends TokenKind {
    val repr = 96
  }

  case object KeywordPure extends TokenKind {
    val repr = 97
  }

  case object KeywordProject extends TokenKind {
    val repr = 98
  }

  case object KeywordQuery extends TokenKind {
    val repr = 99
  }

  case object KeywordRef extends TokenKind {
    val repr = 100
  }

  case object KeywordRegion extends TokenKind {
    val repr = 101
  }

  case object KeywordRestrictable extends TokenKind {
    val repr = 102
  }

  case object KeywordRvadd extends TokenKind {
    val repr = 103
  }

  case object KeywordRvand extends TokenKind {
    val repr = 104
  }

  case object KeywordRvnot extends TokenKind {
    val repr = 105
  }

  case object KeywordRvsub extends TokenKind {
    val repr = 106
  }

  case object KeywordSealed extends TokenKind {
    val repr = 107
  }

  case object KeywordSelect extends TokenKind {
    val repr = 108
  }

  case object KeywordSolve extends TokenKind {
    val repr = 109
  }

  case object KeywordSpawn extends TokenKind {
    val repr = 110
  }

  case object KeywordStatic extends TokenKind {
    val repr = 111
  }

  case object KeywordStaticUppercase extends TokenKind {
    val repr = 112
  }

  case object KeywordTrait extends TokenKind {
    val repr = 113
  }

  case object KeywordTrue extends TokenKind {
    val repr = 114
  }

  case object KeywordTry extends TokenKind {
    val repr = 115
  }

  case object KeywordType extends TokenKind {
    val repr = 116
  }

  case object KeywordTypeMatch extends TokenKind {
    val repr = 117
  }

  case object KeywordUncheckedCast extends TokenKind {
    val repr = 118
  }

  case object KeywordUniv extends TokenKind {
    val repr = 119
  }

  case object KeywordUse extends TokenKind {
    val repr = 120
  }

  case object KeywordWhere extends TokenKind {
    val repr = 121
  }

  case object KeywordWith extends TokenKind {
    val repr = 122
  }

  case object KeywordWithout extends TokenKind {
    val repr = 123
  }

  case object KeywordYield extends TokenKind {
    val repr = 124
  }

  case object KeywordXor extends TokenKind {
    val repr = 125
  }

  case object ListHash extends TokenKind {
    val repr = 126
  }

  case object LiteralBigDecimal extends TokenKind {
    val repr = 127
  }

  case object LiteralBigInt extends TokenKind {
    val repr = 128
  }

  case object LiteralChar extends TokenKind {
    val repr = 129
  }

  case object LiteralDebugStringL extends TokenKind {
    val repr = 130
  }

  case object LiteralDebugStringR extends TokenKind {
    val repr = 131
  }

  case object LiteralFloat32 extends TokenKind {
    val repr = 132
  }

  case object LiteralFloat64 extends TokenKind {
    val repr = 133
  }

  case object LiteralInt8 extends TokenKind {
    val repr = 134
  }

  case object LiteralInt16 extends TokenKind {
    val repr = 135
  }

  case object LiteralInt32 extends TokenKind {
    val repr = 136
  }

  case object LiteralInt64 extends TokenKind {
    val repr = 137
  }

  case object LiteralRegex extends TokenKind {
    val repr = 138
  }

  case object LiteralString extends TokenKind {
    val repr = 139
  }

  case object LiteralStringInterpolationL extends TokenKind {
    val repr = 140
  }

  case object LiteralStringInterpolationR extends TokenKind {
    val repr = 141
  }

  case object MapHash extends TokenKind {
    val repr = 142
  }

  case object Minus extends TokenKind {
    val repr = 143
  }

  case object NameGreek extends TokenKind {
    val repr = 144
  }

  case object NameJava extends TokenKind {
    val repr = 145
  }

  case object NameLowerCase extends TokenKind {
    val repr = 146
  }

  case object NameMath extends TokenKind {
    val repr = 147
  }

  case object NameUpperCase extends TokenKind {
    val repr = 148
  }

  case object ParenL extends TokenKind {
    val repr = 149
  }

  case object ParenR extends TokenKind {
    val repr = 150
  }

  case object Plus extends TokenKind {
    val repr = 151
  }

  case object Semi extends TokenKind {
    val repr = 152
  }

  case object SetHash extends TokenKind {
    val repr = 153
  }

  case object Slash extends TokenKind {
    val repr = 154
  }

  case object Star extends TokenKind {
    val repr = 155
  }

  case object StarStar extends TokenKind {
    val repr = 156
  }

  case object Tilde extends TokenKind {
    val repr = 157
  }

  case object TripleAmpersand extends TokenKind {
    val repr = 158
  }

  case object TripleAngleL extends TokenKind {
    val repr = 159
  }

  case object TripleAngleR extends TokenKind {
    val repr = 160
  }

  case object TripleBar extends TokenKind {
    val repr = 161
  }

  case object TripleCaret extends TokenKind {
    val repr = 162
  }

  case object TripleColon extends TokenKind {
    val repr = 163
  }

  case object TripleTilde extends TokenKind {
    val repr = 164
  }

  case object Underscore extends TokenKind {
    val repr = 165
  }

  case object UserDefinedOperator extends TokenKind {
    val repr = 166
  }

  case object VectorHash extends TokenKind {
    val repr = 167
  }

  /** A special token emitted instead of halting the lexer when an error is encountered.
    *
    * @param error the actual error related to this token
    */
  case class Err(error: LexerError) extends TokenKind {
    val repr = 168
  }

  /** A virtual token signalling END-OF-FILE.
    */
  case object Eof extends TokenKind {
    val repr = 169
  }
}
