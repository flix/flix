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
@Test
def identity01(): Bool = identity(5) == 5

@Test
def identity02(): Bool = identity(true) == true

@Test
def identity03(): Bool = identity(6ii) == 6ii

/////////////////////////////////////////////////////////////////////////////
// flip                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def flip01(): Bool =
    let f = flip((x, y) -> x - y);
    f(5, 4) == -1

@Test
def flip02(): Bool =
    let f = flip((x, y) -> x - y);
    f(4, 5) == 1

@Test
def flip03(): Bool =
    let f = flip((x, y) -> x / y);
    f(10, 3) == 0

@Test
def flip04(): Bool =
    let f = flip((x, y) -> x / y);
    f(3, 10) == 3

/////////////////////////////////////////////////////////////////////////////
// fst                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def fst01(): Bool = fst((5, 0.0f32)) == 5

@Test
def fst02(): Bool = fst((0.0f32, 5)) == 0.0f32

/////////////////////////////////////////////////////////////////////////////
// snd                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def snd01(): Bool = snd((5, 0.0f32)) == 0.0f32

@Test
def snd02(): Bool = snd((0.0f32, 5)) == 5

/////////////////////////////////////////////////////////////////////////////
// swap                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def swap01(): Bool = swap((5, 0.0f32)) == (0.0f32, 5)

@Test
def swap02(): Bool = swap((0.0f32, 5)) == (5, 0.0f32)

/////////////////////////////////////////////////////////////////////////////
// Function Composition                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def forwardComposition01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f >> g;
        h(0) == 2 // (0 + 1) * 2

@Test
def forwardPipe01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = 0 |> f |> g;
        r == 2 // (0 + 1) * 2

@Test
def forwardPipe02(): Bool =
    let f = match (x, y) -> x + y;
    let r = (1, 2) |> f;
        r == 3

/////////////////////////////////////////////////////////////////////////////
// ||> (doublePipe)                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def doublePipe01(): Bool = ((1, 2) ||> Add.add) == 3

@Test
def doublePipe02(): Bool = (("a", "b") ||> Add.add) == "ab"

@Test
def doublePipe03(): Bool = (("abc", "def") ||> String.concat) == "abcdef"

@Test
def doublePipe04(): Bool =
    ((1 :: 2 :: 3 :: Nil, 4 :: 5 :: Nil) ||> List.append) == 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@Test
def doublePipe05(): Bool = ((0, 5) ||> List.range) == 0 :: 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// !> (imperativePipe)                                                     //
/////////////////////////////////////////////////////////////////////////////

@Test
def imperativePipe01(): Bool = region rc {
    let m: MutList[Int32, _] = MutList.new(rc);
    let f = (x) -> MutList.push!(x, m);
    (4 !> f) == 4 and not MutList.isEmpty(m)
}

@Test
def imperativePipe02(): Bool = region rc {
    let m: MutList[Int32, _] = MutList.new(rc);
    let f = (x) -> MutList.push!(x, m);

    let a = 4 !> f;
    let b = 5 !> f;
    let c = 7 !> f;
    a == 4 and b == 5 and c == 7 and MutList.length(m) == 3
}

@Test
def imperativePipe03(): Bool = region rc {
    let a = ref 8 @ rc;
    let f = (x) -> a := x;
    (2 !> f) == 2 and deref a == 2
}

@Test
def imperativePipe04(): Bool = region rc {
    let a = ref 8 @ rc;
    let f = (x) -> a := x;
    (2 !> f) == 2 and (4 !> f) == 4 and deref a == 4
}
