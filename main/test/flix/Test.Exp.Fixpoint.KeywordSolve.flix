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
    pub def testSolve01(): Bool = {
        let solved = solve #{
            ASolve01(42).
        };
        let otherProg = #{
            ASolve01(-42).
        };
        let k = solve solved, otherProg;
        let res1 = query solved select x from ASolve01(x);
        let res2 = query k select x from ASolve01(x);
        res1 `Assert.eq` Vector#{42} and res2 `Assert.eq` Vector#{-42, 42}
    }

    @Test
    pub def testSolve02(): Bool = {
        let solved = solve #{
            ASolve02(42).
        };
        let otherProg = solve #{
            ASolve02(-42).
        };
        let k = solve solved, otherProg;
        let res1 = query solved select x from ASolve02(x);
        let res2 = query k select x from ASolve02(x);
        let res3 = query otherProg select x from ASolve02(x);
        res1 `Assert.eq` Vector#{42} and res2 `Assert.eq` Vector#{-42, 42} and res3 `Assert.eq` Vector#{-42}
    }

    @Test
    pub def testSolve03(): Bool = {
        let solved = solve #{
            BSolve03(1).
            BSolve03(2).
            ASolve03(x) :- BSolve03(x).
        };
        let otherProg = #{
            ASolve03(-x) :- BSolve03(x).
        };
        let k = solve solved, otherProg;
        let res1 = query solved select x from ASolve03(x);
        let res2 = query k select x from ASolve03(x);
        res1 `Assert.eq` Vector#{1, 2} and res2 `Assert.eq` Vector#{-2, -1, 1, 2}
    }

}
