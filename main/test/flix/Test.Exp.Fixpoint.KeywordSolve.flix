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

mod Test.Exp.Fixpoint.KeywordSolve {

    /////////////////////////////////////////////////////////////////////////////
    // Test that `solve` does not mutate the original program being solved     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSolve01(): Bool = {
        let db1 = solve #{
            ASolve(42).
        };
        let db2 = #{
            ASolve(-42).
        };
        let combined = solve db1, db2;
        let mm1 = query db1 select x from ASolve(x);
        let mm2 = query combined select x from ASolve(x);
        mm1 `Assert.eq` Vector#{42} and mm2 `Assert.eq` Vector#{-42, 42}
    }

    @Test
    def testSolve02(): Bool = {
        let db1 = solve #{
            ASolve(42).
        };
        let db2 = solve #{
            ASolve(-42).
        };
        let combined = solve db1, db2;
        let mm1 = query db1 select x from ASolve(x);
        let mm2 = query combined select x from ASolve(x);
        let mm3 = query db2 select x from ASolve(x);
        mm1 `Assert.eq` Vector#{42} and mm2 `Assert.eq` Vector#{-42, 42} and mm3 `Assert.eq` Vector#{-42}
    }

    @Test
    def testSolve03(): Bool = {
        let db1 = solve #{
            BSolve(1).
            BSolve(2).
            ASolve(x) :- BSolve(x).
        };
        let db2 = #{
            ASolve(-x) :- BSolve(x).
        };
        let combined = solve db1, db2;
        let mm1 = query db1 select x from ASolve(x);
        let mm2 = query combined select x from ASolve(x);
        mm1 `Assert.eq` Vector#{1, 2} and mm2 `Assert.eq` Vector#{-2, -1, 1, 2}
    }

    @Test
    def testSolve04(): Bool = {
        let db1 = #{
            ASolve(1).
            ASolve(x) :- BSolve(x).
        };
        let db2 = #{
            BSolve(2).
            BSolve(x) :- ASolve(x).
        };
        let combined = solve db1, db2;
        let db1Solved = solve db1;
        let db2Solved = solve db2;
        let mm11 = query db1Solved select x from ASolve(x);
        let mm12 = query db1Solved select x from BSolve(x);
        let mm21 = query db2Solved select x from ASolve(x);
        let mm22 = query db2Solved select x from BSolve(x);
        let mm31 = query combined select x from ASolve(x);
        let mm32 = query combined select x from BSolve(x);
        mm11 `Assert.eq` Vector#{1} and mm12 `Assert.eq` Vector.empty()
            and mm21 `Assert.eq` Vector.empty() and mm22 `Assert.eq` Vector#{2}
            and mm31 `Assert.eq` Vector#{1, 2} and mm32 `Assert.eq` Vector#{1, 2}
    }

}
