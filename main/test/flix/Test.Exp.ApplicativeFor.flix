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

mod Test.Exp.ApplicativeFor {

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testApplicativeFor01(): List[String] =
        forA (
            x <- "Hello" :: Nil
        ) yield {
            "${x} world"
        }

    @test
    def testApplicativeFor02(): Bool =
        let res = forA (
            x <- "Hello" :: Nil
        ) yield {
            "${x} world"
        };
        res == "Hello world" :: Nil

    @test
    def testApplicativeFor03(): List[String] =
        forA (
            x <- "Hello" :: Nil;
            y <- "world" :: Nil
        ) yield {
            "${x} ${y}"
        }

    @test
    def testApplicativeFor04(): Bool =
        let res = forA (
            x <- "Hello" :: Nil;
            y <- "world" :: Nil
        ) yield {
            "${x} ${y}"
        };
        res == "Hello world" :: Nil

    @test
    def testApplicativeFor05(): Bool =
        let res = forA (
            (a, b) <- (1, 2) :: Nil;
            (c, d) <- (3, 4) :: Nil
        ) yield (a + b) * c + d;
        res == ((1 + 2) * 3 + 4) :: Nil

    @test
    def testApplicativeFor06(): Bool =
        let res = forA (
            x <- "Hello" :: Nil;
            y <- "world" :: "a" :: "b" :: "c" :: Nil
        ) yield {
            "${x} ${y}"
        };
        res == "Hello world" :: "Hello a" :: "Hello b" :: "Hello c" :: Nil

    @test
    def testApplicativeFor07(): Bool =
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
    def testApplicativeFor08(): Bool =
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
    def testApplicativeFor09(): Bool =
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
    def testApplicativeFor10(): Bool =
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


    /////////////////////////////////////////////////////////////////////////////
    // Validation                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testApplicativeFor11(): Bool =
        let res = forA (
            x <- s("Hello"): Validation[Int32, String]
        ) yield {
            "${x} world"
        };
        res == s("Hello world")

    @test
    def testApplicativeFor12(): Bool =
        let res = forA (
            x <- s("Hello"): Validation[Int32, String];
            y <- s("world")
        ) yield {
            "${x} ${y}"
        };
        res == s("Hello world")

    @test
    def testApplicativeFor13(): Bool =
        let res = forA (
            (a, b) <- s((1, 2)): Validation[String, (Int32, Int32)];
            (c, d) <- s((3, 4))
        ) yield (a + b) * c + d;
        res == s((1 + 2) * 3 + 4)

    @test
    def testApplicativeFor14(): Bool =
        let res = forA (
            (a, b) <- s((1, 2)): Validation[String, (Int32, Int32)];
            (c, d) <- f("err")
        ) yield (a + b) * c + d;
        res == f("err")

    @test
    def testApplicativeFor15(): Bool =
        let res = forA (
            (a, b) <- s((1, 2)): Validation[String, (Int32, Int32)];
            (c, d) <- f("err1");
            (_, _) <- f("err2")
        ) yield (a + b) * c + d;
        res == fs("err1" :: "err2" :: Nil)

    @test
    def testApplicativeFor16(): Bool =
        let res = forA (
            a <- s("a"): Validation[Bool, String];
            b <- s("1");
            c <- s("x")
        ) yield "${a}${b}${c}";
        res == s("a1x")

    @test
    def testApplicativeFor17(): Bool =
        let res = forA (
            a <- s("a"): Validation[Bool, String];
            b <- s("1")
        ) yield {
            forA (
                c <- s("x"): Validation[Int32, String]
            ) yield "${a}${b}${c}"
        };
        res == s(s("a1x"))

    @test
    def testApplicativeFor18(): Bool =
        let res = forA (
            (a, b) <- s((1, 2)): Validation[String, (Int32, Int32)];
            (c, d) <- f("err1");
            _ <- f("err2");
            _ <- f("err3");
            _ <- f("err4");
            _ <- f("err5");
            _ <- f("err6");
            _ <- f("err7");
            _ <- f("err8");
            _ <- f("err9");
            _ <- f("err10");
            _ <- f("err11");
            _ <- f("err12")
        ) yield (a + b) * c + d;
        res == fs("err1" :: "err2" :: "err3" :: "err4" :: "err5" :: "err6" :: "err7" :: "err8" :: "err9" :: "err10" :: "err11" :: "err12" :: Nil)

    ///
    /// Returns a `Validation.Success` containing element `t`.
    ///
    def s(t: t): Validation[e, t] =
        Validation.Success(t)

    ///
    /// Returns a `Validation.Failure` containing element `e`.
    ///
    def f(e: e): Validation[e, t] =
        Validation.Failure(Nec.singleton(e))

    ///
    /// Returns a `Validation.Failure` containing element all elements of `es`.
    /// `es` must be non-empty!
    ///
    def fs(es: List[e]): Validation[e, t] = match es {
        case Nil     => bug!("Called with empty list")
        case x :: xs => List.foldLeft(Nec.snoc, Nec.singleton(x), xs) |> Validation.Failure
    }

}
