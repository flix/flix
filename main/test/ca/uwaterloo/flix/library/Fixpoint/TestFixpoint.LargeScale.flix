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

mod TestFixpoint.LargeScale {

    @Test
    def testLargeScale01(): Bool = {
        // This program produces 1 million Path facts
        let tuples = Vector.range(0, 1000) |> Vector.map(x -> (x, x + 1));
        let f = inject tuples into Edge/2;
        let r = #{
            Edge(1000, 0).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let q = query f, r select (x, y) from Path(x, y) where y == x + 1;
        q == tuples
    }

}
