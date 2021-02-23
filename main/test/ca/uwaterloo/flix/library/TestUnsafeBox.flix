/*
 *  Copyright 2021 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace TestUnsafeBox {

    use UnsafeBox.{box, lift1, lift2};

    @test
    def testBox01(): Bool = box("abc") == box("abc")

    @test
    def testBox02(): Bool = box(1 :: 2 :: 3 :: Nil) == box(1 :: 2 :: 3 :: Nil)

    @test
    def testBox03(): Bool = box(None: Option[Int32]) == box(None: Option[Int32])

    @test
    def testBox04(): Bool = box(Some(42): Option[Int32]) == box(Some(42): Option[Int32])

    @test
    def testBox05(): Bool = box(None: Option[Int32]) == box(None: Option[Int32])

    // TODO: Why are the type annotations required below?

    @test
    def testLift1_01(): Bool = lift1(s -> String.toUpperCase(s))(box("hello")) == box("HELLO")

    @test
    def testLift1_02(): Bool = lift1(l -> List.reverse(l: List[Int32]))(box(1 :: 2 :: 3 :: Nil)) == box(3 :: 2 :: 1 :: Nil)

    @test
    def testLift2_01(): Bool = lift2((x: String, y: String) -> (y, x))(box("hello"), box("world")) == box(("world", "hello"))

}
