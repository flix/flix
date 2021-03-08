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
namespace TestChoice {

    use Choice.{assertAbsent, assertPresent, isAbsent, isPresent, unbox, getWithDefault, withDefault, map, invert};

    @test
    def assertAbsent01(): Bool = assertAbsent(Absent) == ()

    @test
    def assertPresent01(): Bool = assertPresent(Present(123)) == ()

    @test
    def isAbsent01(): Bool = isAbsent(Absent)

    @test
    def isAbsent02(): Bool = not isAbsent(Present(123))

    @test
    def isPresent01(): Bool = isPresent(Present(123))

    @test
    def isPresent02(): Bool = not isPresent(Absent)

    @test
    def unbox01(): Bool = unbox(Present(123)) == 123

    @test
    def unbox02(): Bool = unbox(Present("Hello World")) == "Hello World"

    @test
    def getWithDefault01(): Bool = getWithDefault(Absent, 42) == 42

    @test
    def getWithDefault02(): Bool = getWithDefault(Present(21), 42) == 21

    @test
    def testWithDefault01(): Bool = unbox(withDefault(Present(123), Present(456))) == 123

    @test
    def testWithDefault02(): Bool = unbox(withDefault(Absent, Present(456))) == 456

    @test
    def testWithDefault03(): Bool = unbox(withDefault(Present(123), Present(456))) == 123

    @test
    def map01(): Bool = isAbsent(map(x -> x, Absent))

    @test
    def map02(): Bool = isPresent(map(x -> x, Present(123)))

    @test
    def map03(): Bool = unbox(map(x -> x * 2, Present(42))) == 84

    @test
    def invert01(): Bool = unbox(invert(Absent, 42)) == 42

    @test
    def invert02(): Bool = isAbsent(invert(Present(42), 84))

    @test
    def invert03(): Bool = isAbsent(invert(invert(Absent, 42), 84))

    @test
    def invert04(): Bool = isPresent(invert(invert(invert(Absent, 21), 42), 84))

}
