/*
 * Copyright 2019 Magnus Madsen, Stephen Tetley
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

mod TestMutHeap {

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNew01(): Bool = region rc {
        let MutHeap(_, arr, _) = MutHeap.new(rc);
        Array.length(deref arr) == 8
    }

    @Test
    def testNew02(): Bool = region rc {
        let MutHeap(_, _, s) = MutHeap.new(rc);
        deref s == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSize01(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.size(h) == 0
    }

    @Test
    def testSize02(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, "a" :: "b" :: "c" :: "d" :: "e" :: Nil);
        MutHeap.size(h) == 5
    }

    @Test
    def testSize03(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutHeap.delete(h, 4);
        MutHeap.size(h) == 7
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsEmpty01(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.isEmpty(h)
    }

    @Test
    def testIsEmpty02(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.insert(h, "a");
        not MutHeap.isEmpty(h)
    }

    @Test
    def testIsEmpty03(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 2 :: 3 :: Nil);
        MutHeap.delete(h, 1);
        MutHeap.delete(h, 2);
        MutHeap.delete(h, 3);
        MutHeap.isEmpty(h)
    }

    /////////////////////////////////////////////////////////////////////////////
    // peek                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPeek01(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.insert(h, 3);
        MutHeap.delete(h, 3);
        MutHeap.peek(h) == None
    }

    @Test
    def testPeek02(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.insert(h, 1);
        MutHeap.peek(h) == Some(1)
    }

    @Test
    def testPeek03(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 9 :: 2 :: 8 :: 3 :: 7 :: Nil);
        MutHeap.delete(h, 9);
        MutHeap.peek(h) == Some(8)
    }

    @Test
    def testPeek04(): Bool = region rc {
        let h = MutHeap.new(rc) : MutHeap[Int32, rc];
        MutHeap.peek(h) == None
    }

    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPop01(): Bool = region rc {
        let h = MutHeap.new(rc) : MutHeap[Int32, rc];
        MutHeap.pop(h);
        MutHeap.size(h) == 0
    }

    @Test
    def testPop02(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutHeap.pop(h);
        MutHeap.pop(h);
        MutHeap.size(h) == 3
    }

    @Test
    def testPop03(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 2 :: 3 :: Nil);
        MutHeap.pop(h);
        MutHeap.pop(h);
        MutHeap.pop(h);
        MutHeap.size(h) == 0
    }

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testInsert01(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.insert(h, "a");
        MutHeap.size(h) == 1
    }

    @Test
    def testInsert02(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.insert(h, "a");
        MutHeap.insert(h, "b");
        MutHeap.insert(h, "c");
        MutHeap.insert(h, "a");
        MutHeap.insert(h, "a");
        MutHeap.peek(h) == Some("c")
    }

    @Test
    def testInsert03(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 3 :: 4 :: 5 :: 6 :: 10 :: 5 :: 7 :: 14 :: 20 :: Nil);
        MutHeap.peek(h) == Some(20)
    }

    /////////////////////////////////////////////////////////////////////////////
    // delete                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testDelete01(): Bool = region rc {
        let h = MutHeap.new(rc) : MutHeap[Int32, rc];
        MutHeap.delete(h, 3);
        MutHeap.isEmpty(h)
    }

    @Test
    def testDelete02(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutHeap.delete(h, 5);
        MutHeap.delete(h, 3);
        MutHeap.peek(h) == Some(4)
    }

    @Test
    def testDelete03(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        MutHeap.delete(h, 5);
        MutHeap.delete(h, 5);
        MutHeap.delete(h, 5);
        MutHeap.size(h) == 4
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToString01(): Bool = region rc {
        let h = MutHeap.new(rc) : MutHeap[Char, rc];
        MutHeap.toString(h) == "MutHeap {}"
    }

    @Test
    def testToString02(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.insert(h, 3);
        MutHeap.toString(h) == "MutHeap {3}"
    }

    @Test
    def testToString03(): Bool = region rc {
        let h = MutHeap.new(rc);
        MutHeap.readFromList(h, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        MutHeap.delete(h, 1);
        MutHeap.delete(h, 2);
        MutHeap.toString(h) == "MutHeap {8, 7, 6, 4, 3, 5}"
    }

}
