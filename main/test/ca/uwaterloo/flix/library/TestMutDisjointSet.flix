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

mod TestMutDisjointSet {

    /////////////////////////////////////////////////////////////////////////////
    // makeSet                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def makeSet01(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        foreach (x <- List.range(0, 10)) {
            MutDisjointSet.makeSet(x, s)
        };
        let actual =
        List.range(0, 10)
        |> List.map(s |> flip(MutDisjointSet.contains))
        |> List.foldLeft((a, b) -> a and b, true);
        Assert.eq(true, actual)
    }

}
