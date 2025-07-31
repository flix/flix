/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

mod Test.Exp.Fixpoint.Application {

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 1 argument                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationOne01(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            R(x, x + 1) :- P(x).
        };
        let res = tuples |> Vector.map(x -> (x, x + 1));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationOne02(): Bool = {
        let n = 50;
        let res = Vector.range(0, n + 1);
        let r = #{
            Succ(0).
            Succ(x + 1) :- Succ(x), if(x < n)
        };
        res `Assert.eq` query r select x from Succ(x)
    }

    @Test
    def testApplicationOne03(): Bool = {
        let g = x -> {
            let fizz = if (Int32.modulo(x, 3) == 0) "Fizz" else "";
            let buzz = if (Int32.modulo(x, 5) == 0) "Buzz" else "";
            let out = fizz + buzz;
            if (out == "")
                Int32.toString(x)
            else
                out
        };
        let tuples = Vector.range(1, 21);
        let f = inject tuples into Nat/1;
        let r = #{
            FizzBuzz(x, g(x)) :- Nat(x).
        };
        let res = Vector#{
            "1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz",
            "11", "Fizz", "13", "14", "FizzBuzz", "16", "17", "Fizz", "19", "Buzz"
        };
        res `Assert.eq`
        query f, r select (x, y) from FizzBuzz(x, y) |>
            Vector.unzip |> snd
    }

    @Test
    def testApplicationOne04(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            R(x + 1, x) :- P(x).
        };
        let res = tuples |> Vector.map(x -> (x + 1, x));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationOne05(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            R(x + 1, x + 1) :- P(x).
        };
        let res = tuples |> Vector.map(x -> (x + 1, x + 1));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 2 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationTwo01(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            R(x, x + y) :- P(x), Q(y).
        };
        let res = tuples |> Vector.map(x -> (x, x + 1));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationTwo02(): Bool = {
        let n = 20;
        let res = Vector#{0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20};
        let r = #{
            Two(2).
            SuccTwo(0).
            SuccTwo(x + y) :- SuccTwo(x), Two(y), if(x < n)
        };
        res `Assert.eq` query r select x from SuccTwo(x)
    }

    @Test
    def testApplicationTwo03(): Bool = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            R(a, a + b) :- P(a), Q(b).
        };
        let res = tuples |> Vector.map(x -> (x, x + "1")) |> Vector.sort;
        Assert.eq(res, query f, r select (x, y) from R(x, y))
    }

    @Test
    def testApplicationTwo04(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            R(x + y, x) :- P(x), Q(y).
        };
        let res = tuples |> Vector.map(x -> (x + 1, x));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationTwo05(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            R(x + y, x + y) :- P(x), Q(y).
        };
        let res = tuples |> Vector.map(x -> (x + 1, x + 1));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 3 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationThree01(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            R(x, x + y + z) :- P(x), Q(y), S(z).
        };
        let res = tuples |> Vector.map(x -> (x, x + 1 + 2));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationThree02(): Bool = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            S("2").
            R(a, a + b + c) :- P(a), Q(b), S(c).
        };
        let res = tuples |> Vector.map(x -> (x, x + "1" + "2")) |> Vector.sort;
        Assert.eq(res, query f, r select (x, y) from R(x, y))
    }

    @Test
    def testApplicationThree03(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            R(x + y + z, x) :- P(x), Q(y), S(z).
        };
        let res = tuples |> Vector.map(x -> (x + 1 + 2, x));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationThree04(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            R(x + y + z, x + y + z) :- P(x), Q(y), S(z).
        };
        let res = tuples |> Vector.map(x -> (x + 1 + 2, x + 1 + 2));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 4 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationFour01(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            R(a, a + b + c + d) :- P(a), Q(b), S(c), T(d).
        };
        let res = tuples |> Vector.map(x -> (x, x + 1 + 2 + 3));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationFour02(): Bool = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            S("2").
            T("3").
            R(a, a + b + c + d) :- P(a), Q(b), S(c), T(d).
        };
        let res = tuples |> Vector.map(x -> (x, x + "1" + "2" + "3")) |> Vector.sort;
        Assert.eq(res, query f, r select (x, y) from R(x, y))
    }

    @Test
    def testApplicationFour03(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            R(a + b + c + d, a) :- P(a), Q(b), S(c), T(d).
        };
        let res = tuples |> Vector.map(x -> (x + 1 + 2 + 3, x));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationFour04(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            R(a + b + c + d, a + b + c + d) :- P(a), Q(b), S(c), T(d).
        };
        let res = tuples |> Vector.map(x -> (x + 1 + 2 + 3, x + 1 + 2 + 3));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 5 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationFive01(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            U(4).
            R(a, a + b + c + d + e) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let res = tuples |> Vector.map(x -> (x, x + 1 + 2 + 3 + 4));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationFive02(): Bool = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            S("2").
            T("3").
            U("4").
            R(a, a + b + c + d + e) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let res = tuples |> Vector.map(x -> (x, x + "1" + "2" + "3" + "4")) |> Vector.sort;
        Assert.eq(res, query f, r select (x, y) from R(x, y))
    }

    @Test
    def testApplicationFive03(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            U(4).
            R(a + b + c + d + e, a) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let res = tuples |> Vector.map(x -> (x + 1 + 2 + 3 + 4, x));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

    @Test
    def testApplicationFive04(): Bool = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            U(4).
            R(a + b + c + d + e, a + b + c + d + e) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let res = tuples |> Vector.map(x -> (x + 1 + 2 + 3 + 4, x + 1 + 2 + 3 + 4));
        res `Assert.eq` query f, r select (x, y) from R(x, y)
    }

}
