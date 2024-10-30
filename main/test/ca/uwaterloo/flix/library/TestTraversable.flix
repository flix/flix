/*
 * Copyright 2021 Stephen Tetley
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

mod TestTraversable {

    use Traversable.{sequence, traverse, mapAccumLeft}


    /////////////////////////////////////////////////////////////////////////////
    // for                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    // Just test `for` on List - `for` will work for every Traversable with a
    // properly defined `traverse` method.

    @test
    def for01(): Bool =
        let xs: List[Int32] = Nil;
        Traversable.for(xs, x -> Some(x+1)) == Some(Nil)

    @test
    def for02(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        Traversable.for(xs, x -> Some(x+1)) == Some(2 :: Nil)

    @test
    def for03(): Bool =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        Traversable.for(xs, x -> Some(x+1)) == Some(2 :: 3 :: Nil)

    @test
    def for04(): Bool =
        let xs: List[Int32] = Nil;
        Traversable.for(xs, _ -> (None: Option[Int32])) == Some(Nil)

    @test
    def for05(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        Traversable.for(xs, _ -> (None: Option[Int32])) == None

    @test
    def for06(): Bool =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        Traversable.for(xs, _ -> (None: Option[Int32])) == None

    /////////////////////////////////////////////////////////////////////////////
    // sequenceOption                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequenceOption01(): Bool =
        let o: Option[Option[Int32]] = None;
        sequence(o) == Some(None)

    @test
    def sequenceOption02(): Bool =
        let o: Option[Option[Int32]] = Some(None);
        sequence(o) == None

    @test
    def sequenceOption03(): Bool =
        let o: Option[Option[Int32]] = Some(Some(1));
        sequence(o) == Some(Some(1))

    /////////////////////////////////////////////////////////////////////////////
    // traverseOption                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseOption01(): Bool =
        let o: Option[Int32] = None;
        traverse(x -> Some(x+1), o) == Some(None)

    @test
    def traverseOption02(): Bool =
        let o: Option[Int32] = Some(1);
        traverse(x -> Some(x+1), o) == Some(Some(2))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftOption                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeftOption01(): Bool =
        let o: Option[Int32] = None;
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, o) == (10, None)

    @test
    def mapAccumLeftOption02(): Bool =
        let o: Option[Int32] = Some(1);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, o) == (11, Some(11))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequenceList01(): Bool =
        let xs: List[Option[Int32]] = Nil;
        sequence(xs) == Some(Nil)

    @test
    def sequenceList02(): Bool =
        let xs: List[Option[Int32]] = None :: Nil;
        sequence(xs) == None

    @test
    def sequenceList03(): Bool =
        let xs = Some(1) :: Nil;
        sequence(xs) == Some(1 :: Nil)

    @test
    def sequenceList04(): Bool =
        let xs = Some(1) :: None :: Nil;
        sequence(xs) == None

    @test
    def sequenceList05(): Bool =
        let xs = Some(1) :: Some(2) :: Nil;
        sequence(xs) == Some(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // traverseList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseList01(): Bool =
        let xs: List[Int32] = Nil;
        traverse(x -> Some(x+1), xs) == Some(Nil)

    @test
    def traverseList02(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        traverse(x -> Some(x+1), xs) == Some(2 :: Nil)

    @test
    def traverseList03(): Bool =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        traverse(x -> Some(x+1), xs) == Some(2 :: 3 :: Nil)

    @test
    def traverseList04(): Bool =
        let xs: List[Int32] = Nil;
        traverse(_ -> (None: Option[Int32]), xs) == Some(Nil)

    @test
    def traverseList05(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        traverse(_ -> (None: Option[Int32]), xs) == None

    @test
    def traverseList06(): Bool =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        traverse(_ -> (None: Option[Int32]), xs) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftList                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeftList01(): Bool =
        let xs: List[Int32] = Nil;
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (10, Nil)

    @test
    def mapAccumLeftList02(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (11, 11 :: Nil)

    @test
    def mapAccumLeftList03(): Bool =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (12, 11 :: 13 :: Nil)

    @test
    def mapAccumLeftList04(): Bool =
        let xs: List[Int32] = 1 :: 2 :: 3 :: Nil;
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (13, 11 :: 13 :: 15 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sequenceNel.Nel                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequenceNel01(): Bool =
        let xs: Nel[Option[Int32]] = Nel.singleton(None);
        sequence(xs) == None

    @test
    def sequenceNel02(): Bool =
        let xs: Nel[Option[Int32]] = Nel.singleton(Some(1));
        sequence(xs) == Some(Nel.singleton(1))

    @test
    def sequenceNel03(): Bool =
        let xs = Nel.Nel(Some(1), Some(2) :: Nil);
        sequence(xs) == Some(Nel.Nel(1, 2 :: Nil))

    @test
    def sequenceNel04(): Bool =
        let xs = Nel.Nel(Some(1), Some(2) :: Some(3) :: Nil);
        sequence(xs) == Some(Nel.Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverseNel.Nel                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseNel01(): Bool =
        let xs: Nel[Int32] = Nel.Nel(1, Nil);
        traverse(x -> Some(x+1), xs) == Some(Nel.Nel(2, Nil))

    @test
    def traverseNel02(): Bool =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: Nil);
        traverse(x -> Some(x+1), xs) == Some(Nel.Nel(2, 3 :: Nil))

    @test
    def traverseNel03(): Bool =
        let xs: Nel[Int32] = Nel.Nel(1, Nil);
        traverse(_ -> (None: Option[Int32]), xs) == None

    @test
    def traverseNel04(): Bool =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: Nil);
        traverse(_ -> (None: Option[Int32]), xs) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftNel.Nel                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeftNel01(): Bool =
        let xs: Nel[Int32] = Nel.Nel(1, Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (11, Nel.Nel(11, Nil))

    @test
    def mapAccumLeftNel02(): Bool =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (12, Nel.Nel(11, 13 :: Nil))

    @test
    def mapAccumLeftNel03(): Bool =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: 3 :: Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (13, Nel.Nel(11, 13 :: 15 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceChain                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequenceChain01(): Bool =
        let xs: Chain[Option[Int32]] = Chain.empty();
        sequence(xs) == Some(Chain.empty())

    @test
    def sequenceChain02(): Bool =
        let xs: Chain[Option[Int32]] = Chain.singleton(None);
        sequence(xs) == None

    @test
    def sequenceChain03(): Bool =
        let xs = Chain.singleton(Some(1));
        sequence(xs) == Some(Chain.singleton(1))

    @test
    def sequenceChain04(): Bool =
        let xs = List.toChain(Some(1) :: None :: Nil);
        sequence(xs) == None

    @test
    def sequenceChain05(): Bool =
        let xs = List.toChain(Some(1) :: Some(2) :: Nil);
        sequence(xs) == Some(List.toChain(1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverseChain                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseChain01(): Bool =
        let xs: Chain[Int32] = Chain.empty();
        traverse(x -> Some(x+1), xs) == Some(Chain.empty())

    @test
    def traverseChain02(): Bool =
        let xs: Chain[Int32] = Chain.singleton(1);
        traverse(x -> Some(x+1), xs) == Some(Chain.singleton(2))

    @test
    def traverseChain03(): Bool =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: Nil);
        traverse(x -> Some(x+1), xs) == Some(List.toChain(2 :: 3 :: Nil))

    @test
    def traverseChain04(): Bool =
        let xs: Chain[Int32] = Chain.empty();
        traverse(_ -> (None: Option[Int32]), xs) == Some(Chain.empty())

    @test
    def traverseChain05(): Bool =
        let xs: Chain[Int32] = Chain.singleton(1);
        traverse(_ -> (None: Option[Int32]), xs) == None

    @test
    def traverseChain06(): Bool =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: Nil);
        traverse(_ -> (None: Option[Int32]), xs) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftChain                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeftChain01(): Bool =
        let xs: Chain[Int32] = Chain.empty();
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (10, Chain.empty())

    @test
    def mapAccumLeftChain02(): Bool =
        let xs: Chain[Int32] = Chain.singleton(1);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (11, Chain.singleton(11))

    @test
    def mapAccumLeftChain03(): Bool =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (12, List.toChain(11 :: 13 :: Nil))

    @test
    def mapAccumLeftChain04(): Bool =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: 3 :: Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (13, List.toChain(11 :: 13 :: 15 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceMap                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequenceMap01(): Bool =
        let m: Map[Int32, Option[Int32]] = Map#{};
        sequence(m) == Some(Map#{})

    @test
    def sequenceMap02(): Bool =
        let m: Map[Int32, Option[Int32]] = Map#{1 => None};
        sequence(m) == None

    @test
    def sequenceMap03(): Bool =
        let m = Map#{1 => Some(1)};
        sequence(m) == Some(Map#{1 => 1})

    @test
    def sequenceMap04(): Bool =
        let m = Map#{1 => Some(1), 2 => None};
        sequence(m) == None

    @test
    def sequenceMap05(): Bool =
        let m = Map#{1 => Some(1), 2 => Some(2)};
        sequence(m) == Some(Map#{1 => 1, 2 => 2})

    /////////////////////////////////////////////////////////////////////////////
    // traverseMap                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseMap01(): Bool =
        let m: Map[Int32, Int32] = Map#{};
        traverse(x -> Some(x+1), m) == Some(Map#{})

    @test
    def traverseMap02(): Bool =
        let m: Map[Int32, Int32] = Map#{1 => 1};
        traverse(x -> Some(x+1), m) == Some(Map#{1 => 2})

    @test
    def traverseMap03(): Bool =
        let m: Map[Int32, Int32] = Map#{1 => 1, 2 => 2};
        traverse(x -> Some(x+1), m) == Some(Map#{1 => 2, 2 => 3})

    @test
    def traverseMap04(): Bool =
        let m: Map[Int32, Int32] = Map#{};
        traverse(_ -> (None: Option[Int32]), m) == Some(Map#{})

    @test
    def traverseMap05(): Bool =
        let m: Map[Int32, Int32] = Map#{1 => 1};
        traverse(_ -> (None: Option[Int32]), m) == None

    @test
    def traverseMap06(): Bool =
        let m: Map[Int32, Int32] = Map#{1 => 1, 2 => 2};
        traverse(_ -> (None: Option[Int32]), m) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeftMap01(): Bool =
        let m: Map[Int32, Int32] = Map#{};
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (10, Map#{})

    @test
    def mapAccumLeftMap02(): Bool =
        let m: Map[Int32, Int32] = Map#{1 => 1};
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (11, Map#{1 => 11})

    @test
    def mapAccumLeftMap03(): Bool =
        let m: Map[Int32, Int32]  = Map#{1 => 1, 2 => 2};
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (12, Map#{1 => 11, 2 => 13})

    @test
    def mapAccumLeftMap04(): Bool =
        let m: Map[Int32, Int32]  = Map#{1 => 1, 2 => 2, 3 => 3};
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (13, Map#{1 => 11, 2 => 13, 3 => 15})

    /////////////////////////////////////////////////////////////////////////////
    /// Helper for RedBlackTree                                                //
    /////////////////////////////////////////////////////////////////////////////

    def toRedBlackTree(xs: List[(k, v)]): RedBlackTree[k, v] with Order[k] =
        List.foldLeft((acc, kv) -> {let (k,v) = kv; RedBlackTree.insert(k, v, acc)}, RedBlackTree.empty(), xs)

    /////////////////////////////////////////////////////////////////////////////
    // sequenceRedBlackTree                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequenceRedBlackTree01(): Bool =
        let t: RedBlackTree[Int32, Option[Int32]] = RedBlackTree.empty();
        sequence(t) == Some(RedBlackTree.empty())

    @test
    def sequenceRedBlackTree02(): Bool =
        let t: RedBlackTree[Int32, Option[Int32]] = toRedBlackTree((1, None) :: Nil);
        sequence(t) == None

    @test
    def sequenceRedBlackTree03(): Bool =
        let t = toRedBlackTree((1, Some(1)) :: Nil);
        sequence(t) == Some(toRedBlackTree((1, 1) :: Nil))

    @test
    def sequenceRedBlackTree04(): Bool =
        let t = toRedBlackTree((1, Some(1)) :: (2, None) :: Nil);
        sequence(t) == None

    @test
    def sequenceRedBlackTree05(): Bool =
        let t = toRedBlackTree((1, Some(1)) :: (2, Some(2)) :: Nil);
        sequence(t) == Some(toRedBlackTree((1, 1) :: (2, 2) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverseRedBlackTree                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseRedBlackTree01(): Bool =
        let t: RedBlackTree[Int32, Int32] = RedBlackTree.empty();
        traverse(x -> Some(x+1), t) == Some(RedBlackTree.empty())

    @test
    def traverseRedBlackTree02(): Bool =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: Nil);
        traverse(x -> Some(x+1), t) == Some(toRedBlackTree((1, 2) :: Nil))

    @test
    def traverseRedBlackTree03(): Bool =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        traverse(x -> Some(x+1), t) == Some(toRedBlackTree((1, 2) :: (2, 3) :: Nil))

    @test
    def traverseRedBlackTree04(): Bool =
        let t: RedBlackTree[Int32, Int32] = RedBlackTree.empty();
        traverse(_ -> (None: Option[Int32]), t) == Some(RedBlackTree.empty())

    @test
    def traverseRedBlackTree05(): Bool =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1):: Nil);
        traverse(_ -> (None: Option[Int32]), t) == None

    @test
    def traverseRedBlackTree06(): Bool =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        traverse(_ -> (None: Option[Int32]), t) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftRedBlackTree                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeftRedBlackTree01(): Bool =
        let m: RedBlackTree[Int32, Int32] = RedBlackTree.empty();
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (10, RedBlackTree.empty())

    @test
    def mapAccumLeftRedBlackTree02(): Bool =
        let m: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (11, toRedBlackTree((1, 11) :: Nil))

    @test
    def mapAccumLeftRedBlackTree03(): Bool =
        let m: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (12, toRedBlackTree((1, 11) :: (2, 13) :: Nil))

    @test
    def mapAccumLeftRedBlackTree04(): Bool =
        let m: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: (3, 3) :: Nil);
        mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (13, toRedBlackTree((1, 11) :: (2, 13) :: (3, 15) :: Nil))

}
