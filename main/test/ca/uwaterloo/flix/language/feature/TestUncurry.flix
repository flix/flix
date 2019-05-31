/*
 * Copyright 2017 Jason Mittertreiner
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

def add3(x: Int): Int -> Int -> Int = {
    y -> (z -> x + y + z)
}

def add2(x: Int): Int -> Int = add3(3)(x)
def add1a(x: Int): Int = add2(4)(x)
def add1b(x: Int): Int = add3(3)(4)(x)

@test
def testSimple01(): Bool =
    add3(3)(4)(5) `assertEq!` 12

@test
def testSimple02(): Bool =
    add2(4)(5) `assertEq!` 12

@test
def testSimple03(): Bool =
    add1a(5) `assertEq!` 12

@test
def testSimple04(): Bool =
    add1b(5) `assertEq!` 12


def sub(x: Int): Int -> Int = y -> (x - y)

@test
def testMulti01(): Bool = add3(sub(3)(4))(4)(5) `assertEq!` 8


@test
def testMap01(): Bool =  {
  let lst0: List[Int] = 1 :: 2 :: 3 :: 4 :: Nil;
  let lst1: List[Int] = List.map(add1a, lst0);
  List.fold((x, y) -> x + y, 0, lst1) `assertEq!` 38
}

@test
def testMap02(): Bool = {
  let lst0: List[Int32] = 1 :: 2 :: 3 :: 4 :: Nil;
  let lst1: List[Int32] = List.map(add3(4)(5), lst0);
  List.fold((x, y) -> x + y, 0, lst1) `assertEq!` 46
}

def id(x: (Int32) -> Int32): (Int32) -> Int32 = x

@test
def testReturnFunc01(): Bool = {
    id(x -> x)(3) `assertEq!` 3
}

@test
def testLambdas01(): Bool = {
    let foo = i -> (w -> x -> y -> z -> w + x + y + z + i)(2)(3)(4)(5);
    foo(1)  `assertEq!` 15
}

@test
def testLambdas02(): Bool = {
    let foo = i -> (w -> x -> y -> z -> w + x + y + z + i)(3)(4)(5);
    foo(1)(2)  `assertEq!` 15
}

@test
def testLambdas03(): Bool = {
    let foo = i -> (w -> x -> y -> z -> w + x + y + z + i)(4)(5);
    foo(1)(2)(3)  `assertEq!` 15
}

@test
def testLambdas04(): Bool = {
    let foo = i -> (w -> x -> y -> z -> w + x + y + z + i)(5);
    foo(1)(2)(3)(4)  `assertEq!` 15
}

@test
def testLambdas05(): Bool = {
    let foo = i -> (w -> x -> y -> z -> w + x + y + z + i);
    foo(1)(2)(3)(4)(5)  `assertEq!` 15
}

@test
def testZeroArgs01(): Bool = {
  let inc4: Int32 -> Int32 = x -> x + 4;
  let add4: Int32 -> Int32 = i -> inc4(i);
  add4(4) `assertEq!` 8
}

@test
def nestedDef01(): Bool = {
  let id: Int32 -> Int32 = i -> {
    let nested = _ -> _ -> i;
    nested(3)(3)
  };

  id(4) `assertEq!` 4
}

