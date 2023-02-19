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
            x <- "Hello" :: Nil
        ) yield {
            "${x} world"
        }

    @test
    def testForA02(): Bool =
        let res = forA (
            x <- "Hello" :: Nil
        ) yield {
            "${x} world"
        };
        res == "Hello world" :: Nil

    @test
    def testForA03(): List[String] =
        forA (
            x <- "Hello" :: Nil;
            y <- "world" :: Nil
        ) yield {
            "${x} ${y}"
        }

    @test
    def testForA04(): Bool =
        let res = forA (
            x <- "Hello" :: Nil;
            y <- "world" :: Nil
        ) yield {
            "${x} ${y}"
        };
        res == "Hello world" :: Nil

    @test
    def testForA05(): Bool =
        let res = forA (
            (a, b) <- (1, 2) :: Nil;
            (c, d) <- (3, 4) :: Nil
        ) yield (a + b) * c + d;
        res == ((1 + 2) * 3 + 4) :: Nil

    @test
    def testForA06(): Bool =
        let res = forA (
            x <- "Hello" :: Nil;
            y <- "world" :: "a" :: "b" :: "c" :: Nil
        ) yield {
            "${x} ${y}"
        };
        res == "Hello world" :: "Hello a" :: "Hello b" :: "Hello c" :: Nil

    @test
    def testForA07(): Bool =
        let res = forA (
            x <- "1" :: "2" :: "3" :: Nil;
            y <- "a" :: "b" :: "c" :: Nil
        ) yield {
            "${x} ${y}"
        };
        res == "1 a" :: "1 b" :: "1 c" ::
               "2 a" :: "2 b" :: "2 c" ::
               "3 a" :: "3 b" :: "3 c" :: Nil

}
