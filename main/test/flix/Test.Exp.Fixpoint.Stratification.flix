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

mod Test.Exp.Fixpoint.Stratification {

    @Test
    def testStratification01(): Bool = {
        let pr = #{
            A1(1).
            B1(x) :- A1(x).
            C1(x) :- B1(x).
            D1(x) :- C1(x).
        };
        let mm = solve pr;
        Vector#{1} `Assert.eq` query mm select (x) from A1(x) and
        Vector#{1} `Assert.eq` query mm select (x) from B1(x) and
        Vector#{1} `Assert.eq` query mm select (x) from C1(x) and
        Vector#{1} `Assert.eq` query mm select (x) from D1(x)

    }

    @Test
    def testStratification02(): Bool = {
        let pr = #{
            A2(1).
            B2(x) :- A2(x).
            C2(x + y) :- A2(x), B2(y).
            D2(x + y + z) :- A2(x), B2(y), C2(z).
        };
        let mm = solve pr;
        Vector#{1} `Assert.eq` query mm select (x) from A2(x) and
        Vector#{1} `Assert.eq` query mm select (x) from B2(x) and
        Vector#{1 + 1} `Assert.eq` query mm select (x) from C2(x) and
        Vector#{1 + 1 + 2} `Assert.eq` query mm select (x) from D2(x)
    }

    @Test
    def testStratification03(): Bool = {
        let pr = #{
            A(1, 2).
            A(2, 3).
            A(3, 4).
            B(x, 4) :- A(1, x).
            C(x, y) :- A(2, x), B(2, y).
            D(9) :- A(_, 3), C(_, 4).
        };
        let mm = solve pr;
        Vector#{(1, 2), (2, 3), (3, 4)} `Assert.eq` query mm select (x, y) from A(x, y) and
        Vector#{(2, 4)} `Assert.eq` query mm select (x, y) from B(x, y) and
        Vector#{(3, 4)} `Assert.eq` query mm select (x, y) from C(x, y) and
        Vector#{9} `Assert.eq` query mm select (x) from D(x)
    }

    @Test
    def testStratification04(): Bool = {
        let pr = #{
            A(1, 2).
            A(2, 3).
            A(3, 4).
            B(x, 4) :- A(1, x).
            C(x, y) :- B(2, y), A(2, x).
            D(9) :- C(_, 4), A(_, 3).
        };
        let mm = solve pr;
        Vector#{(1, 2), (2, 3), (3, 4)} `Assert.eq` query mm select (x, y) from A(x, y) and
        Vector#{(2, 4)} `Assert.eq` query mm select (x, y) from B(x, y) and
        Vector#{(3, 4)} `Assert.eq` query mm select (x, y) from C(x, y) and
        Vector#{9} `Assert.eq` query mm select (x) from D(x)
    }

    @Test
    def testStratification05(): Bool = {
        let pr = #{
            A("1", "2").
            A("2", "3").
            A("3", "4").
            B(x, "4") :- A("1", x).
            C(x, y) :- B("2", y), A("2", x).
            D("9") :- C(_, "4"), A(_, "3").
        };
        let mm = solve pr;
        Vector#{("1", "2"), ("2", "3"), ("3", "4")} `Assert.eq` query mm select (x, y) from A(x, y) and
        Vector#{("2", "4")} `Assert.eq` query mm select (x, y) from B(x, y) and
        Vector#{("3", "4")} `Assert.eq` query mm select (x, y) from C(x, y) and
        Vector#{"9"} `Assert.eq` query mm select (x) from D(x)
    }

    @Test
    def testStratification06(): Bool = {
        let pr = #{
            A(1).
            A(2).
            B(3).
            B(x) :- A(x).
            C(x) :- B(x), not A(x).
        };
        let mm = solve pr;
        Vector#{1, 2} `Assert.eq` query mm select (x) from A(x) and
        Vector#{1, 2, 3} `Assert.eq` query mm select (x) from B(x) and
        Vector#{3} `Assert.eq` query mm select (x) from C(x)
    }

    @Test
    def testStratification07(): Bool = {
        let pr = #{
            A(1).
            A(2).
            B(3).
            B(x) :- A(x).
            C(x) :- fix B(x), not A(x).
        };
        let mm = solve pr;
        Vector#{1, 2} `Assert.eq` query mm select (x) from A(x) and
        Vector#{1, 2, 3} `Assert.eq` query mm select (x) from B(x) and
        Vector#{3} `Assert.eq` query mm select (x) from C(x)
    }

    @Test
    def testStratification08(): Bool = {
        let pr = #{
            A(1).
            A(2).
            B(3).
            B(x) :- fix A(x).
            C(x) :- B(x), not A(x).
        };
        let mm = solve pr;
        Vector#{1, 2} `Assert.eq` query mm select (x) from A(x) and
        Vector#{1, 2, 3} `Assert.eq` query mm select (x) from B(x) and
        Vector#{3} `Assert.eq` query mm select (x) from C(x)
    }

    @Test
    def testStratification09(): Bool = {
        let pr = #{
            A("1").
            A("2").
            B("3").
            B(x) :- A(x).
            C(x) :- B(x), not A(x).
        };
        let mm = solve pr;
        Vector#{"1", "2"} `Assert.eq` query mm select (x) from A(x) and
        Vector#{"1", "2", "3"} `Assert.eq` query mm select (x) from B(x) and
        Vector#{"3"} `Assert.eq` query mm select (x) from C(x)
    }
}
