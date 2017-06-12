/*
 * Copyright 2017 Liam Palmer
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

/////////////////////////////////////////////////////////////////////////////
// constantFoldPlus                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def constantFoldPlus01: Bool = (1.03f32 + 2.48f32) `assertEq!` 3.51f32

@test
def constantFoldPlus02: Bool = (1.03f64 + 2.48f64) `assertEq!` 3.51f64

@test
def constantFoldPlus03: Bool = (1i8 + 2i8) `assertEq!` 3i8

@test
def constantFoldPlus04: Bool = (1i16 + 2i16) `assertEq!` 3i16

@test
def constantFoldPlus05: Bool = (1 + 2) `assertEq!` 3

@test
def constantFoldPlus06: Bool = (1i64 + 2i64) `assertEq!` 3i64

@test
def constantFoldPlus07: Bool = (1ii + 2ii) `assertEq!` 3ii

/////////////////////////////////////////////////////////////////////////////
// constantFoldMinus                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def constantFoldMinus01: Bool = (1.03f32 - 2.48f32) `assertEq!` -1.45f32

@test
def constantFoldMinus02: Bool = (1.03f64 - 2.48f64) `assertEq!` -1.45f64

@test
def constantFoldMinus03: Bool = (1i8 - 2i8) `assertEq!` -1i8

@test
def constantFoldMinus04: Bool = (1i16 - 2i16) `assertEq!` -1i16

@test
def constantFoldMinus05: Bool = (1 - 2) `assertEq!` -1

@test
def constantFoldMinus06: Bool = (1i64 - 2i64) `assertEq!` -1i64

@test
def constantFoldMinus07: Bool = (1ii - 2ii) `assertEq!` -1ii

/////////////////////////////////////////////////////////////////////////////
// constantFoldTimes                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def constantFoldTimes01: Bool = (2.5f32 * 3.5f32) `assertEq!` 8.75f32

@test
def constantFoldTimes02: Bool = (2.5f64 * 3.5f64) `assertEq!` 8.75f64

@test
def constantFoldTimes03: Bool = (3i8 * 2i8) `assertEq!` 6i8

@test
def constantFoldTimes04: Bool = (3i16 * 2i16) `assertEq!` 6i16

@test
def constantFoldTimes05: Bool = (3 * 2) `assertEq!` 6

@test
def constantFoldTimes06: Bool = (3i64 * 2i64) `assertEq!` 6i64

@test
def constantFoldTimes07: Bool = (3ii * 2ii) `assertEq!` 6ii

/////////////////////////////////////////////////////////////////////////////
// constantFoldDivide                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def constantFoldDivide01: Bool = (8.75f32 / 3.5f32) `assertEq!` 2.5f32

@test
def constantFoldDivide02: Bool = (8.75f64 / 3.5f64) `assertEq!` 2.5f64

@test
def constantFoldDivide03: Bool = (6i8 / 3i8) `assertEq!` 2i8

@test
def constantFoldDivide04: Bool = (6i16 / 3i16) `assertEq!` 2i16

@test
def constantFoldDivide05: Bool = (6 / 3) `assertEq!` 2

@test
def constantFoldDivide06: Bool = (6i64 / 3i64) `assertEq!` 2i64

@test
def constantFoldDivide07: Bool = (6ii / 3ii) `assertEq!` 2ii

/////////////////////////////////////////////////////////////////////////////
// constantFoldEqual                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def constantFoldEqual01: Bool = assert!(1.03f32 == 1.03f32)

@test
def constantFoldEqual02: Bool = assertNot!(1.03f32 == 1.04f32)

@test
def constantFoldEqual03: Bool = assert!(1.03f64 == 1.03f64)

@test
def constantFoldEqual04: Bool = assertNot!(1.03f64 == 1.04f64)

@test
def constantFoldEqual05: Bool = assert!(1i8 == 1i8)

@test
def constantFoldEqual06: Bool = assertNot!(1i8 == 2i8)

@test
def constantFoldEqual07: Bool = assert!(1i16 == 1i16)

@test
def constantFoldEqual08: Bool = assertNot!(1i16 == 2i16)

@test
def constantFoldEqual09: Bool = assert!(1 == 1)

@test
def constantFoldEqual10: Bool = assertNot!(1 == 2)

@test
def constantFoldEqual11: Bool = assert!(1i64 == 1i64)

@test
def constantFoldEqual12: Bool = assertNot!(1i64 == 2i64)

@test
def constantFoldEqual13: Bool = assert!(1ii == 1ii)

@test
def constantFoldEqual14: Bool = assertNot!(1ii == 2ii)

@test
def constantFoldEqual15: Bool = assert!('h' == 'h')

@test
def constantFoldEqual16: Bool = assertNot!('h' == 'd')

@test
def constantFoldEqual17: Bool = assert!("hello" == "hello")

@test
def constantFoldEqual18: Bool = assertNot!("hello" == "helld")

/////////////////////////////////////////////////////////////////////////////
// constantFoldNotEqual                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def constantFoldNotEqual01: Bool = assertNot!(1.03f32 != 1.03f32)

@test
def constantFoldNotEqual02: Bool = assert!(1.03f32 != 1.04f32)

@test
def constantFoldNotEqual03: Bool = assertNot!(1.03f64 != 1.03f64)

@test
def constantFoldNotEqual04: Bool = assert!(1.03f64 != 1.04f64)

@test
def constantFoldNotEqual05: Bool = assertNot!(1i8 != 1i8)

@test
def constantFoldNotEqual06: Bool = assert!(1i8 != 2i8)

@test
def constantFoldNotEqual07: Bool = assertNot!(1i16 != 1i16)

@test
def constantFoldNotEqual08: Bool = assert!(1i16 != 2i16)

@test
def constantFoldNotEqual09: Bool = assertNot!(1 != 1)

@test
def constantFoldNotEqual10: Bool = assert!(1 != 2)

@test
def constantFoldNotEqual11: Bool = assertNot!(1i64 != 1i64)

@test
def constantFoldNotEqual12: Bool = assert!(1i64 != 2i64)

@test
def constantFoldNotEqual13: Bool = assertNot!(1ii != 1ii)

@test
def constantFoldNotEqual14: Bool = assert!(1ii != 2ii)

@test
def constantFoldNotEqual15: Bool = assertNot!('h' != 'h')

@test
def constantFoldNotEqual16: Bool = assert!('h' != 'd')

@test
def constantFoldNotEqual17: Bool = assertNot!("hello" != "hello")

@test
def constantFoldNotEqual18: Bool = assert!("hello" != "helld")