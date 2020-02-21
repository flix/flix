/*
 * Copyright 2019 Magnus Madsen
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
@test
def testNew01(): Bool & Impure = Array.new(42, 10)[0] == 42

@test
def testNew02(): Bool & Impure = Array.new(42, 10)[5] == 42

@test
def testNew03(): Bool & Impure = Array.new(42, 10)[9] == 42

@test
def testGet01(): Bool & Impure = Array.get([1, 2, 3], 0) == 1

@test
def testGet02(): Bool & Impure = Array.get([1, 2, 3], 1) == 2

@test
def testGet03(): Bool & Impure = Array.get([1, 2, 3], 2) == 3

@test
def testPut01(): Bool & Impure = Array.put([1], 0, 42)[0] == 42

@test
def testPut02(): Bool & Impure = Array.put([1, 2], 1, 42)[1] == 42

@test
def testPut03(): Bool & Impure = Array.put([1, 2, 3], 2, 42)[2] == 42

@test
def testLength01(): Bool & Impure = Array.length([]) == 0

@test
def testLength02(): Bool & Impure = Array.length([1]) == 1

@test
def testLength03(): Bool & Impure = Array.length([1, 2, 3]) == 3

@test
def testSlice01(): Bool & Impure = Array.slice([1, 2, 3], 0, 1)[0] == 1

@test
def testSlice02(): Bool & Impure = Array.slice([1, 2, 3], 1, 2)[0] == 2

@test
def testSlice03(): Bool & Impure = Array.slice([1, 2, 3], 2, 3)[0] == 3

@test
def testToList01(): Bool & Impure = Array.toList([]) == Nil

@test
def testToList02(): Bool & Impure = Array.toList([1]) == 1 :: Nil

@test
def testToList03(): Bool & Impure = Array.toList([1, 2]) == 1 :: 2 :: Nil
