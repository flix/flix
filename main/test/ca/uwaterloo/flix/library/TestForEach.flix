/*
 *  Copyright 2025 Ry Wiese
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

mod TestForEach {
    use Int32.modulo
    use ForEach.forEach
    use ForEach.withFilter
    use ForEach.withIndex
    use ForEach.withMap
    use ForEach.withZip

    /////////////////////////////////////////////////////////////////////////////
    // withIndex                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def withIndex01(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach((i, x) <- withIndex(Nil)) {
            results |> MutList.push((i, x))
        };
        results |> MutList.isEmpty
    }

    @test
    pub def withIndex02(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach((i, x) <- withIndex(2 :: 4 :: 6 :: 8 :: Nil)) {
            results |> MutList.push((i, x))
        };
        results |> MutList.toList == (0, 2) :: (1, 4) :: (2, 6) :: (3, 8) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // withFilter                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def withFilter01(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withFilter(x -> x `modulo` 2 == 0, Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.isEmpty
    }

    @test
    pub def withFilter02(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withFilter(x -> x `modulo` 2 == 0, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == 0 :: 2 :: 4 :: 6 :: 8 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // withMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def withMap01(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withMap(x -> x * 2, Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.isEmpty
    }

    @test
    pub def withMap02(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withMap(x -> x * 2, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == 0 :: 2 :: 4 :: 6 :: 8 :: 10 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // withZip                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def withZip01(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(Nil, Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.isEmpty
    }

    @test
    pub def withZip02(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: 4 :: 5 :: Nil, Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.isEmpty
    }

    @test
    pub def withZip03(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(Nil, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.isEmpty
    }

    @test
    pub def withZip04(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: 4 :: 5 :: Nil, "one" :: "two" :: "three" :: Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == (1, "one") :: (2, "two") :: (3, "three") :: Nil
    }

    @test
    pub def withZip05(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: Nil, "one" :: "two" :: "three" :: "four" :: "five" :: Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == (1, "one") :: (2, "two") :: (3, "three") :: Nil
    }

    @test
    pub def withZip06(): Bool = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: 4 :: 5 :: Nil, "one" :: "two" :: "three" :: "four" :: "five" :: Nil)) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == (1, "one") :: (2, "two") :: (3, "three") :: (4, "four") :: (5, "five") :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // composite                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def composite01(): Bool = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)
            |> withFilter(x -> x `modulo` 2 == 0)
            |> withIndex;
        foreach((i, x) <- list) {
            results |> MutList.push((i, x))
        };
        results |> MutList.toList == (0, 0) :: (1, 2) :: (2, 4) :: (3, 6) :: (4, 8) :: Nil
    }

    @test
    pub def composite02(): Bool = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)
            |> withIndex
            |> withFilter(pair -> {
                let (_, x) = pair;
                x `modulo` 2 == 0
            });
        foreach((i, x) <- list) {
            results |> MutList.push((i, x))
        };
        results |> MutList.toList == (0, 0) :: (2, 2) :: (4, 4) :: (6, 6) :: (8, 8) :: Nil
    }

    @test
    pub def composite03(): Bool = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)
            |> withMap(x -> x * 2)
            |> withIndex;
        foreach((i, x) <- list) {
            results |> MutList.push((i, x))
        };
        results |> MutList.toList == (0, 0) :: (1, 2) :: (2, 4) :: (3, 6) :: (4, 8) :: (5, 10) :: Nil
    }

    @test
    pub def composite04(): Bool = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)
            |> withIndex
            |> withMap(pair -> {
                let (i, x) = pair;
                i * x
            });
        foreach(x <- list) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == 0 :: 1 :: 4 :: 9 :: 16 :: 25 :: Nil
    }

    @test
    pub def composite05(): Bool = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)
            |> withMap(x -> x * 2)
            |> withFilter(x -> x `modulo` 3 == 0);
        foreach(x <- list) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == 0 :: 6 :: Nil
    }

    @test
    pub def composite06(): Bool = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)
            |> withFilter(x -> x `modulo` 2 == 0)
            |> withMap(x -> x * 3);
        foreach(x <- list) {
            results |> MutList.push(x)
        };
        results |> MutList.toList == 0 :: 6 :: 12 :: 18 :: 24 :: Nil
    }

    @test
    pub def composite07(): Bool = region rc {
        let results = MutList.empty(rc);
        let list = withIndex(withZip('0' :: '1' :: '2' :: '3' :: '4' :: '5' :: Nil, "zero" :: "one" :: "two" :: "three" :: "four" :: "five" :: Nil));
        foreach((i, (c, s)) <- list) {
            results |> MutList.push((i, c, s))
        };
        results |> MutList.toList == (0, '0', "zero") :: (1, '1', "one") :: (2, '2', "two") :: (3, '3', "three") :: (4, '4', "four") :: (5, '5', "five") :: Nil
    }

}
