/*
 * Copyright 2017 Magnus Madsen
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

/*
 * Inspired by https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax.
 */
@test
def testUCS01(): Bool = inc(42) == 43

@test
def testUCS02(): Bool = 42.inc() == 43

@test
def testUCS03(): Bool = add(21, 42) == 63

@test
def testUCS04(): Bool = 21.add(42) == 63

@test
def testUCS05(): Bool = sum(1, 2, 3) == 6

@test
def testUCS06(): Bool = 1.sum(2, 3) == 6

@test
def testUCS07(): Bool = isEmpty(Nil) == true

@test
def testUCS08(): Bool = Nil.isEmpty() == true

@test
def testUCS09(): Bool = find(x -> x == 42, 1 :: 2 :: Nil) == None

@test
def testUCS10(): Bool = (x -> x == 42).find(1 :: 2 :: Nil) == None

@test
def testUCS11(): Bool = repeat(42, 3) == 42 :: 42 :: 42 :: Nil

@test
def testUCS12(): Bool = 42.repeat(3) == 42 :: 42 :: 42 :: Nil

@test
def testUCS13(): Bool = map(x -> x + 1, 1 :: 2 :: 3 :: Nil) == 2 :: 3 :: 4 :: Nil

@test
def testUCS14(): Bool = (x -> x + 1).map(1 :: 2 :: 3 :: Nil) == 2 :: 3 :: 4 :: Nil

def inc(x: Int): Int = x + 1
def add(x: Int, y: Int): Int = x + y
def sum(x: Int, y: Int, z: Int): Int = x + y + z

def isEmpty[a](xs: List[a]): Bool = match xs {
    case Nil => true
    case _ => false
}

def find[a](f: a -> Bool, xs: List[a]): Option[a] = List.findLeft(f, xs)

def repeat[a](a: a, n: Int32): List[a] = List.repeat(a, n)

def map[a,b](f: a -> b, xs: List[a]): List[b] = List.map(f, xs)

