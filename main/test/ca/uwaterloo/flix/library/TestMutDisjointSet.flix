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

    @Test
    def makeSet02(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        foreach (x <- List.range(0, 10)) {
            MutDisjointSet.makeSet(x, s)
        };
        let MutDisjointSet.MutDisjointSet({ forest | _ }) = s;
        let actual = MutMap.size(forest);
        Assert.eq(10, actual)
    }


    /////////////////////////////////////////////////////////////////////////////
    // makeSets                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def makeSets01(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        let actual =
        List.range(0, 10)
        |> List.map(s |> flip(MutDisjointSet.contains))
        |> List.foldLeft((a, b) -> a and b, true);
        Assert.eq(true, actual)
    }

    @Test
    def makeSets02(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        let MutDisjointSet.MutDisjointSet({ forest | _ }) = s;
        let actual = MutMap.size(forest);
        Assert.eq(10, actual)
    }

    @Test
    def makeSets03(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets("a" :: "b" :: "c" :: Nil, s);
        let MutDisjointSet.MutDisjointSet({ forest | _ }) = s;
        let actual = MutMap.size(forest);
        Assert.eq(3, actual)
    }


    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(-10, 20), s);
        let actual = foreach (
            x <- List.range(-10, 20)
        ) yield MutDisjointSet.find(x, s);
        Assert.eq(List.range(-10, 20) |> List.map(Some), actual)
    }

    @Test
    def find02(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets("x" :: "y" :: "z" :: Nil, s);
        let actual = foreach (
            x <- "x" :: "y" :: "z" :: Nil
        ) yield MutDisjointSet.find(x, s);
        Assert.eq(("x" :: "y" :: "z" :: Nil) |> List.map(Some), actual)
    }

    @Test
    def find03(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(2, 3, s);
        MutDisjointSet.union(4, 5, s);
        MutDisjointSet.union(6, 7, s);
        MutDisjointSet.union(8, 9, s);
        let actual = foreach (
            x <- 0 :: 2 :: 4 :: 6 :: 8 :: Nil
        ) yield MutDisjointSet.find(x, s);
        Assert.eq((1 :: 3 :: 5 :: 7 :: 9 :: Nil) |> List.map(Some), actual)
    }

    @Test
    def find04(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(2, 3, s);
        MutDisjointSet.union(4, 5, s);
        MutDisjointSet.union(6, 7, s);
        MutDisjointSet.union(8, 9, s);
        let actual = foreach (
            x <- 1 :: 3 :: 5 :: 7 :: 9 :: Nil
        ) yield MutDisjointSet.find(x, s);
        Assert.eq((1 :: 3 :: 5 :: 7 :: 9 :: Nil) |> List.map(Some), actual)
    }

    @Test
    def find05(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets("a" :: "b" :: "c" :: "d" :: Nil, s);
        let actual = MutDisjointSet.find("A", s);
        Assert.eq(None, actual)
    }


    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def union01(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        let actual = MutDisjointSet.find(0, s);
        Assert.eq(Some(1), actual)
    }

    @Test
    def union02(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(0, 1, s);
        let actual = MutDisjointSet.find(0, s);
        Assert.eq(Some(1), actual)
    }

    @Test
    def union03(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(1, 1, s);
        let actual = MutDisjointSet.find(1, s);
        Assert.eq(Some(1), actual)
    }

    @Test
    def union04(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(1, 1, s);
        let actual = MutDisjointSet.find(1, s);
        Assert.eq(Some(1), actual)
    }

    @Test
    def union05(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(1, 1, s);
        MutDisjointSet.union(1, 0, s);
        let actual = MutDisjointSet.find(1, s);
        Assert.eq(Some(1), actual)
    }

    @Test
    def union06(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(1, 1, s);
        MutDisjointSet.union(1, 0, s);
        let actual = MutDisjointSet.find(0, s);
        Assert.eq(Some(1), actual)
    }

    @Test
    def union07(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(1, 0, s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(1, 1, s);
        let actual = MutDisjointSet.find(0, s);
        Assert.eq(Some(0), actual)
    }

    @Test
    def union08(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(1, 0, s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(1, 1, s);
        let actual = MutDisjointSet.find(1, s);
        Assert.eq(Some(0), actual)
    }

    @Test
    def union09(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(0, 1, s);
        MutDisjointSet.union(1, 3, s);
        let actual = MutDisjointSet.find(3, s);
        Assert.eq(Some(1), actual)
    }

    @Test
    def union10(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets("a" :: "b" :: "c" :: "d" :: Nil, s);
        MutDisjointSet.union("c", "d", s);
        let actual = MutDisjointSet.find("a", s);
        Assert.eq(Some("a"), actual)
    }

    @Test
    def union11(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets("a" :: "b" :: "c" :: "d" :: Nil, s);
        MutDisjointSet.union("c", "d", s);
        MutDisjointSet.union("b", "c", s);
        MutDisjointSet.union("a", "b", s);
        let actual = MutDisjointSet.find("a", s);
        Assert.eq(Some("d"), actual)
    }

    @Test
    def union12(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        let actual = MutDisjointSet.find(9, s);
        Assert.eq(Some(4), actual)
    }

    @Test
    def union13(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        MutDisjointSet.union(8, 1, s);
        let actual = MutDisjointSet.find(9, s);
        Assert.eq(Some(4), actual)
    }

    @Test
    def union14(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        MutDisjointSet.union(8, 1, s);
        MutDisjointSet.union(8, 9, s);
        let actual = MutDisjointSet.find(8, s);
        Assert.eq(Some(4), actual)
    }

    @Test
    def union15(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        MutDisjointSet.union(8, 1, s);
        MutDisjointSet.union(2, 3, s);
        MutDisjointSet.union(2, 8, s);
        MutDisjointSet.union(8, 9, s);
        let actual = MutDisjointSet.find(4, s);
        Assert.eq(Some(1), actual)
    }


    /////////////////////////////////////////////////////////////////////////////
    // equivalent                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def equivalent01(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(-10, 20), s);
        let actual = MutDisjointSet.equivalent(-10, -10, s);
        Assert.eq(true, actual)
    }

    @Test
    def equivalent02(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(-10, 20), s);
        let actual = MutDisjointSet.equivalent(-10, 10, s);
        Assert.eq(false, actual)
    }

    @Test
    def equivalent03(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(-10, 20), s);
        let actual = MutDisjointSet.equivalent(-10, 200, s);
        Assert.eq(false, actual)
    }

    @Test
    def equivalent04(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        let actual = MutDisjointSet.equivalent(7, 9, s);
        Assert.eq(true, actual)
    }

    @Test
    def equivalent05(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        let actual = MutDisjointSet.equivalent(7, 8, s);
        Assert.eq(false, actual)
    }

    @Test
    def equivalent06(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        MutDisjointSet.union(8, 1, s);
        let actual = MutDisjointSet.equivalent(7, 8, s);
        Assert.eq(false, actual)
    }

    @Test
    def equivalent07(): Bool = region rc {
        let s = MutDisjointSet.new(rc);
        MutDisjointSet.makeSets(List.range(0, 10), s);
        MutDisjointSet.union(7, 4, s);
        MutDisjointSet.union(7, 9, s);
        MutDisjointSet.union(8, 1, s);
        MutDisjointSet.union(8, 7, s);
        let actual = MutDisjointSet.equivalent(7, 8, s);
        Assert.eq(true, actual)
    }

}
