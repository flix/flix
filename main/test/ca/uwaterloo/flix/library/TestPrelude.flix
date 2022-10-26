/*
 * Copyright 2020 Magnus Madsen
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
// identity                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def identity01(): Bool = identity(5) == 5

@test
def identity02(): Bool = identity(true) == true

@test
def identity03(): Bool = identity(6ii) == 6ii

/////////////////////////////////////////////////////////////////////////////
// flip                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def flip01(): Bool =
    let f = flip((x, y) -> x - y);
    f(5, 4) == -1

@test
def flip02(): Bool =
    let f = flip((x, y) -> x - y);
    f(4, 5) == 1

@test
def flip03(): Bool =
    let f = flip((x, y) -> x / y);
    f(10, 3) == 0

@test
def flip04(): Bool =
    let f = flip((x, y) -> x / y);
    f(3, 10) == 3

/////////////////////////////////////////////////////////////////////////////
// fst                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def fst01(): Bool = fst((5, 0.0f32)) == 5

@test
def fst02(): Bool = fst((0.0f32, 5)) == 0.0f32

/////////////////////////////////////////////////////////////////////////////
// snd                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def snd01(): Bool = snd((5, 0.0f32)) == 0.0f32

@test
def snd02(): Bool = snd((0.0f32, 5)) == 5

/////////////////////////////////////////////////////////////////////////////
// swap                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def swap01(): Bool = swap((5, 0.0f32)) == (0.0f32, 5)

@test
def swap02(): Bool = swap((0.0f32, 5)) == (5, 0.0f32)

/////////////////////////////////////////////////////////////////////////////
// Function Composition                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def forwardComposition01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f >> g;
        h(0) == 2 // (0 + 1) * 2

@test
def forwardPipe01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = 0 |> f |> g;
        r == 2 // (0 + 1) * 2

@test
def forwardPipe02(): Bool =
    let f = match (x, y) -> x + y;
    let r = (1, 2) |> f;
        r == 3

/////////////////////////////////////////////////////////////////////////////
// ||> (doublePipe)                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def doublePipe01(): Bool = ((1, 2) ||> Add.add) == 3

@test
def doublePipe02(): Bool = (("a", "b") ||> Add.add) == "ab"

@test
def doublePipe03(): Bool = (("abc", "def") ||> String.concat) == "abcdef"

@test
def doublePipe04(): Bool =
    ((1 :: 2 :: 3 :: Nil, 4 :: 5 :: Nil) ||> List.append) == 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@test
def doublePipe05(): Bool = ((0, 5) ||> List.range) == 0 :: 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// !> (imperativePipe)                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def imperativePipe01(): Bool = region r {
    let m: MutList[Int32, _] = new MutList(r);
    let f = (x) -> MutList.push!(x, m);
    (4 !> f) == 4 and not MutList.isEmpty(m)
}

@test
def imperativePipe02(): Bool = region r {
    let m: MutList[Int32, _] = new MutList(r);
    let f = (x) -> MutList.push!(x, m);

    let a = 4 !> f;
    let b = 5 !> f;
    let c = 7 !> f;
    a == 4 and b == 5 and c == 7 and MutList.length(m) == 3
}

@test
def imperativePipe03(): Bool = region r {
    let a = ref 8 @ r;
    let f = (x) -> a := x;
    (2 !> f) == 2 and deref a == 2
}

@test
def imperativePipe04(): Bool = region r {
    let a = ref 8 @ r;
    let f = (x) -> a := x;
    (2 !> f) == 2 and (4 !> f) == 4 and deref a == 4
}

/////////////////////////////////////////////////////////////////////////////
// stringify                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def testUnitStringify01(): Bool = stringify(()) == "()"


@test
def testBoolStringify01(): Bool = stringify(true) == "true"

@test
def testBoolStringify02(): Bool = stringify(false) == "false"


@test
def testCharStringify01(): Bool = stringify('a') == "\'a\'"

@test
def testCharStringify02(): Bool = stringify('\u2194') == "\'\u2194\'"

@test
def testCharStringify03(): Bool = stringify('\t') == "\'\\t\'"


@test
def testFloat32Stringify01(): Bool = stringify(21.42f32) == "21.42f32"


@test
def testInt8Stringify01(): Bool = stringify(42i8) == "42i8"


@test
def testInt16Stringify01(): Bool = stringify(42i16) == "42i16"


@test
def testInt32Stringify01(): Bool = stringify(42i32) == "42"


@test
def testInt64Stringify01(): Bool = stringify(42i64) == "42i64"


@test
def testBigIntStringify01(): Bool = stringify(42ii) == "42ii"


@test
def testStringStringify01(): Bool = stringify("Hello World!") == "\"Hello World!\""

@test
def testStringStringify02(): Bool = stringify("one\ttwo") == "\"one\\ttwo\""

@test
def testStringStringify03(): Bool = stringify("\u0000") == "\"\u0000\""

@test
def testStringStringify05(): Bool = stringify("\n\r\\\"\'\t\${\%{\u1234\u0000") ==
    "\"" + "\\n" + "\\r" + "\\\\" + "\\\"" + "\\\'" + "\\t" + "\${" + "\%{" + "\u1234" + "\u0000" + "\""

@test
def testArrayOfUnitStringify01(): Bool \ IO = stringify([(), ()]) == "[(), ()]"


@test
def testArrayOfBoolStringify01(): Bool \ IO = stringify([true, false]) == "[true, false]"


@test
def testArrayOfCharStringify01(): Bool \ IO = stringify(['a', 'b', 'c']) == "[a, b, c]"


@test
def testArrayOfFloat32Stringify01(): Bool \ IO = stringify([21.42f32, 98.6f32]) == "[21.42, 98.6]"


@test
def testArrayOfInt8Stringify01(): Bool \ IO = stringify([42i8, 43i8]) == "[42, 43]"


@test
def testArrayOfInt16Stringify01(): Bool \ IO = stringify([42i16, 43i16]) == "[42, 43]"


@test
def testArrayOfInt32Stringify01(): Bool \ IO = stringify([42i32, 43i32]) == "[42, 43]"


@test
def testArrayOfInt64Stringify01(): Bool \ IO = stringify([42i64, 43i64]) == "[42, 43]"


@test
def testArrayOfBigIntStringify01(): Bool \ IO = stringify([42ii, 43ii]) == "[42, 43]"


@test
def testArrayOfStringStringify01(): Bool \ IO = stringify(["Hello", "World!"]) == "[Hello, World!]"


@test
def testArrayOfArrayOfIntStringify01(): Bool \ IO = stringify([[1, 2], [3, 4]]) == "[[1, 2], [3, 4]]"


@test
def testArrayOfArrayOfArrayOfIntStringify01(): Bool \ IO = stringify([[[1, 2], [3, 4]], [[5, 6], [7, 8]]]) == "[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]"


@test
def testTupleStringify01(): Bool = stringify((true, 'a', "Hello World!")) == "(true, a, Hello World!)"

@test
def testTupleStringify02(): Bool = stringify((false, 2, ())) == "(false, 2, ())"


pub enum Suit {
    case Hearts, Diamonds, Clubs, Spades
}

pub enum Rank {
    case N(Int32), Jack, Queen, King, Ace
}

pub enum Card {
    case Card(Suit, Rank)
}

@test
def testEnumStringify01(): Bool = stringify(Card(Hearts, N(5)))  == "Card(Hearts, N(5))"


pub enum Poly[a, b] {
    case NoneOfThem,
    case This(a)
    case That(b)
    case Both(a, b)
}

@test
def testPolyStringify01(): Bool = stringify(Poly.NoneOfThem) == "NoneOfThem"

@test
def testPolyStringify02(): Bool = stringify(This(123)) == "This(123)"

@test
def testPolyStringify03(): Bool = stringify(That("Hello World!")) == "That(Hello World!)"

@test
def testPolyStringify04(): Bool = stringify(Both(123, "Hello World!")) == "Both(123, Hello World!)"

@test
def testRecordStringify01(): Bool = stringify({field = (1, 2)}) == "{field = (1, 2)}"

@test
def testRecordStringify02(): Bool = {
    let s = stringify({something = (), other = true});
    s == "{something = (), other = true}" or s == "{other = true, something = ()}"
}

@test
def testRecordStringify03(): Bool = {
    let s = stringify({something = {}, other = {single = 8}});
    s == "{something = {}, other = {single = 8}}" or s == "{other = {single = 8}, something = {}}"
}

@test
def testFunctionStringify01(): Bool = {
    def f(x: Int32): Int32 = x;
    let s = stringify(f);
    s == "Int32 -> Int32"
}

def toIntFunction(x: a, g: a -> Int32): Int32 = g(x)

@test
def testFunctionStringify02(): Bool = {
    let s = stringify(toIntFunction(true));
    s == "Obj -> Int32"
}

def thunkFunction(x: a): String -> a = _ -> x

@test
def testFunctionStringify03(): Bool = {
    let s = stringify((thunkFunction(42), thunkFunction('a')));
    s == "(Obj -> Int32, Obj -> Char)"
}

// @test
// def testNullStringify01(): Bool = {
//     let s = stringify(null);
//     s == "null"
// }

// @test
// def testNullStringify02(): Bool = {
//     let s = stringify(null as String);
//     s == "null"
// }

// @test
// def testNullStringify03(): Bool = {
//     let s = stringify(null as BigInt);
//     s == "null"
// }

@test
def testNullStringify04(): Bool = {
    let s = stringify(This(null as String));
    s == "This"
}

@test
def testNullStringify05(): Bool = {
    let s = stringify((null, 42));
    s == "(null, 42)"
}

@test
def testNullStringify06(): Bool = region r {
    let s = stringify([null as String, "42"] @ r);
    s == "[null, 42]"
}

@test
def testNullStringify07(): Bool = {
    let s = stringify({field = null as String});
    s == "{field = null}"
}
