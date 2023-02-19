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

    @test
    def testForA08(): Bool =
        let res = forA (
            (a, b) <- ("1", "1") :: ("2", "2") :: ("3", "3") :: Nil;
            x <- "a" :: "b" :: "c" :: Nil
        ) yield {
            "${a}${x}${b}"
        };
        res == "1a1" :: "1b1" :: "1c1" ::
               "2a2" :: "2b2" :: "2c2" ::
               "3a3" :: "3b3" :: "3c3" :: Nil

    @test
    def testForA09(): Bool =
        let res = forA (
            x <- "1" :: "2" :: "3" :: Nil;
            y <- "a" :: "b" :: "c" :: Nil;
            z <- "x" :: "y" :: "z" :: Nil
        ) yield {
            "${x}${y}${z}"
        };
        res == "1ax" :: "1ay" :: "1az" ::
               "1bx" :: "1by" :: "1bz" ::
               "1cx" :: "1cy" :: "1cz" ::
               "2ax" :: "2ay" :: "2az" ::
               "2bx" :: "2by" :: "2bz" ::
               "2cx" :: "2cy" :: "2cz" ::
               "3ax" :: "3ay" :: "3az" ::
               "3bx" :: "3by" :: "3bz" ::
               "3cx" :: "3cy" :: "3cz" :: Nil

    @test
    def testForA10(): Bool =
        let res = forA (
            x <- "1" :: "2" :: "3" :: Nil;
            y <- "a" :: "b" :: "c" :: Nil
        ) yield {
            forA (
                z <- "x" :: "y" :: "z" :: Nil
            ) yield "${x}${y}${z}"
        };
        res == ("1ax" :: "1ay" :: "1az" :: Nil) ::
               ("1bx" :: "1by" :: "1bz" :: Nil) ::
               ("1cx" :: "1cy" :: "1cz" :: Nil) ::
               ("2ax" :: "2ay" :: "2az" :: Nil) ::
               ("2bx" :: "2by" :: "2bz" :: Nil) ::
               ("2cx" :: "2cy" :: "2cz" :: Nil) ::
               ("3ax" :: "3ay" :: "3az" :: Nil) ::
               ("3bx" :: "3by" :: "3bz" :: Nil) ::
               ("3cx" :: "3cy" :: "3cz" :: Nil) :: Nil

}
