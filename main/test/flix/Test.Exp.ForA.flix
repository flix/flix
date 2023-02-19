/*
 *  Copyright 2023 Jakob Schneider Villumsen
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

namespace TestForA {

    @test
    def testForA01(): List[String] =
        forA (
            x <- s("Hello")
        ) yield {
            "${x} world"
        }

    @test
    def testForA02(): Bool =
        let res = forA (
            x <- s("Hello")
        ) yield {
            "${x} world"
        };
        res == "Hello world" :: Nil

    @test
    def testForA03(): List[String] =
        forA (
            x <- s("Hello");
            y <- s("world")
        ) yield {
            "${x} ${y}"
        }

    @test
    def testForA04(): Bool =
        let res = forA (
            x <- s("Hello");
            y <- s("world")
        ) yield {
            "${x} ${y}"
        };
        res == "Hello world" :: Nil

    def s(a: a): List[a] =
        a :: Nil

}
