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

namespace TestTraversable {

    /////////////////////////////////////////////////////////////////////////////
    // sequence_Option                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence_Option01(): Bool =
        let o: Option[Option[Int]] = None;
        Traversable.sequence(o) == Some(None)

    @test
    def sequence_Option02(): Bool =
        let o: Option[Option[Int]] = Some(None);
        Traversable.sequence(o) == None

    @test
    def sequence_Option03(): Bool =
        let o: Option[Option[Int]] = Some(Some(1));
        Traversable.sequence(o) == Some(Some(1))

    /////////////////////////////////////////////////////////////////////////////
    // traverse_Option                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse_Option01(): Bool =
        let o: Option[Int] = None;
        Traversable.traverse(x -> Some(x+1), o) == Some(None)

    @test
    def traverse_Option02(): Bool =
        let o: Option[Int] = Some(1);
        Traversable.traverse(x -> Some(x+1), o) == Some(Some(2))


    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft_Option                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft_Option01(): Bool =
        let o: Option[Int] = None;
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, o) == (10, None)

    @test
    def mapAccumLeft_Option02(): Bool =
        let o: Option[Int] = Some(1);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, o) == (11, Some(11))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight_Option                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight_Option01(): Bool =
        let o: Option[Int] = None;
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, o) == (10, None)

    @test
    def mapAccumRight_Option02(): Bool =
        let o: Option[Int] = Some(1);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, o) == (11, Some(11))


    /////////////////////////////////////////////////////////////////////////////
    // sequence_List                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence_List01(): Bool =
        let xs: List[Option[Int]] = Nil;
        Traversable.sequence(xs) == Some(Nil)

    @test
    def sequence_List02(): Bool =
        let xs: List[Option[Int]] = None :: Nil;
        Traversable.sequence(xs) == None

    @test
    def sequence_List03(): Bool =
        let xs = Some(1) :: Nil;
        Traversable.sequence(xs) == Some(1 :: Nil)

    @test
    def sequence_List04(): Bool =
        let xs = Some(1) :: None :: Nil;
        Traversable.sequence(xs) == None

    @test
    def sequence_List05(): Bool =
        let xs = Some(1) :: Some(2) :: Nil;
        Traversable.sequence(xs) == Some(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // traverse_List                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse_List01(): Bool =
        let xs: List[Int] = Nil;
        Traversable.traverse(x -> Some(x+1), xs) == Some(Nil)

    @test
    def traverse_List02(): Bool =
        let xs: List[Int] = 1 :: Nil;
        Traversable.traverse(x -> Some(x+1), xs) == Some(2 :: Nil)

    @test
    def traverse_List03(): Bool =
        let xs: List[Int] = 1 :: 2 :: Nil;
        Traversable.traverse(x -> Some(x+1), xs) == Some(2 :: 3 :: Nil)

    @test
    def traverse_List04(): Bool =
        let xs: List[Int] = Nil;
        Traversable.traverse(_ -> None, xs) == Some(Nil)

    @test
    def traverse_List05(): Bool =
        let xs: List[Int] = 1 :: Nil;
        Traversable.traverse(_ -> None, xs) == None

    @test
    def traverse_List06(): Bool =
        let xs: List[Int] = 1 :: 2 :: Nil;
        Traversable.traverse(_ -> None, xs) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft_List                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft_List01(): Bool =
        let xs: List[Int] = Nil;
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (10, Nil)

    @test
    def mapAccumLeft_List02(): Bool =
        let xs: List[Int] = 1 :: Nil;
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (11, 11 :: Nil)

    @test
    def mapAccumLeft_List03(): Bool =
        let xs: List[Int] = 1 :: 2 :: Nil;
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (12, 11 :: 13 :: Nil)

    @test
    def mapAccumLeft_List04(): Bool =
        let xs: List[Int] = 1 :: 2 :: 3 :: Nil;
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (13, 11 :: 13 :: 15 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight_List                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight_List01(): Bool =
        let xs: List[Int] = Nil;
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (10, Nil)

    @test
    def mapAccumRight_List02(): Bool =
        let xs: List[Int] = 1 :: Nil;
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (11, 11 :: Nil)

    @test
    def mapAccumRight_List03(): Bool =
        let xs: List[Int] = 1 :: 2 :: Nil;
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (12, 12 :: 12 :: Nil)

    @test
    def mapAccumRight_List04(): Bool =
        let xs: List[Int] = 1 :: 2 :: 3 :: Nil;
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (13, 13 :: 13 :: 13 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sequence_Nel                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence_Nel01(): Bool =
        let xs: Nel[Option[Int]] = Nel.singleton(None);
        Traversable.sequence(xs) == None

    @test
    def sequence_Nel02(): Bool =
        let xs: Nel[Option[Int]] = Nel.singleton(Some(1));
        Traversable.sequence(xs) == Some(Nel.singleton(1))

    @test
    def sequence_Nel03(): Bool =
        let xs = Nel(Some(1), Some(2) :: Nil);
        Traversable.sequence(xs) == Some(Nel(1, 2 :: Nil))

    @test
    def sequence_Nel04(): Bool =
        let xs = Nel(Some(1), Some(2) :: Some(3) :: Nil);
        Traversable.sequence(xs) == Some(Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse_Nel                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse_Nel01(): Bool =
        let xs: Nel[Int] = Nel(1, Nil);
        Traversable.traverse(x -> Some(x+1), xs) == Some(Nel(2, Nil))

    @test
    def traverse_Nel02(): Bool =
        let xs: Nel[Int] = Nel(1, 2 :: Nil);
        Traversable.traverse(x -> Some(x+1), xs) == Some(Nel(2, 3 :: Nil))

    @test
    def traverse_Nel03(): Bool =
        let xs: Nel[Int] = Nel(1, Nil);
        Traversable.traverse(_ -> None, xs) == None

    @test
    def traverse_Nel04(): Bool =
        let xs: Nel[Int] = Nel(1, 2 :: Nil);
        Traversable.traverse(_ -> None, xs) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft_Nel                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft_Nel01(): Bool =
        let xs: Nel[Int] = Nel(1, Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (11, Nel(11, Nil))

    @test
    def mapAccumLeft_Nel02(): Bool =
        let xs: Nel[Int] = Nel(1, 2 :: Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (12, Nel(11, 13 :: Nil))

    @test
    def mapAccumLeft_Nel03(): Bool =
        let xs: Nel[Int] = Nel(1, 2 :: 3 :: Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (13, Nel(11, 13 :: 15 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight_Nel                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight_Nel01(): Bool =
        let xs: Nel[Int] = Nel(1, Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (11, Nel(11, Nil))

    @test
    def mapAccumRight_Nel02(): Bool =
        let xs: Nel[Int] = Nel(1, 2 :: Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (12, Nel(12, 12 :: Nil))

    @test
    def mapAccumRight_Nel03(): Bool =
        let xs: Nel[Int] = Nel(1, 2 :: 3 :: Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (13, Nel(13, 13 :: 13 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sequence_Chain                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence_Chain01(): Bool =
        let xs: Chain[Option[Int]] = Chain.empty();
        Traversable.sequence(xs) == Some(Chain.empty())

    @test
    def sequence_Chain02(): Bool =
        let xs: Chain[Option[Int]] = Chain.singleton(None);
        Traversable.sequence(xs) == None

    @test
    def sequence_Chain03(): Bool =
        let xs = Chain.singleton(Some(1));
        Traversable.sequence(xs) == Some(Chain.singleton(1))

    @test
    def sequence_Chain04(): Bool =
        let xs = List.toChain(Some(1) :: None :: Nil);
        Traversable.sequence(xs) == None

    @test
    def sequence_Chain05(): Bool =
        let xs = List.toChain(Some(1) :: Some(2) :: Nil);
        Traversable.sequence(xs) == Some(List.toChain(1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse_Chain                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse_Chain01(): Bool =
        let xs: Chain[Int] = Chain.empty();
        Traversable.traverse(x -> Some(x+1), xs) == Some(Chain.empty())

    @test
    def traverse_Chain02(): Bool =
        let xs: Chain[Int] = Chain.singleton(1);
        Traversable.traverse(x -> Some(x+1), xs) == Some(Chain.singleton(2))

    @test
    def traverse_Chain03(): Bool =
        let xs: Chain[Int] = List.toChain(1 :: 2 :: Nil);
        Traversable.traverse(x -> Some(x+1), xs) == Some(List.toChain(2 :: 3 :: Nil))

    @test
    def traverse_Chain04(): Bool =
        let xs: Chain[Int] = Chain.empty();
        Traversable.traverse(_ -> None, xs) == Some(Chain.empty())

    @test
    def traverse_Chain05(): Bool =
        let xs: Chain[Int] = Chain.singleton(1);
        Traversable.traverse(_ -> None, xs) == None

    @test
    def traverse_Chain06(): Bool =
        let xs: Chain[Int] = List.toChain(1 :: 2 :: Nil);
        Traversable.traverse(_ -> None, xs) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft_Chain                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft_Chain01(): Bool =
        let xs: Chain[Int] = Chain.empty();
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (10, Chain.empty())

    @test
    def mapAccumLeft_Chain02(): Bool =
        let xs: Chain[Int] = Chain.singleton(1);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (11, Chain.singleton(11))

    @test
    def mapAccumLeft_Chain03(): Bool =
        let xs: Chain[Int] = List.toChain(1 :: 2 :: Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (12, List.toChain(11 :: 13 :: Nil))

    @test
    def mapAccumLeft_Chain04(): Bool =
        let xs: Chain[Int] = List.toChain(1 :: 2 :: 3 :: Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs) == (13, List.toChain(11 :: 13 :: 15 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight_Chain                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight_Chain01(): Bool =
        let xs: Chain[Int] = Chain.empty();
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (10, Chain.empty())

    @test
    def mapAccumRight_Chain02(): Bool =
        let xs: Chain[Int] = Chain.singleton(1);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (11, Chain.singleton(11))

    @test
    def mapAccumRight_Chain03(): Bool =
        let xs: Chain[Int] = List.toChain(1 :: 2 :: Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (12, List.toChain(12 :: 12 :: Nil))

    @test
    def mapAccumRight_Chain04(): Bool =
        let xs: Chain[Int] = List.toChain(1 :: 2 :: 3 :: Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, xs) == (13, List.toChain(13 :: 13 :: 13 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sequence_Map                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence_Map01(): Bool =
        let m: Map[Int, Option[Int]] = Map#{};
        Traversable.sequence(m) == Some(Map#{})

    @test
    def sequence_Map02(): Bool =
        let m: Map[Int, Option[Int]] = Map#{1 -> None};
        Traversable.sequence(m) == None

    @test
    def sequence_Map03(): Bool =
        let m = Map#{1 -> Some(1)};
        Traversable.sequence(m) == Some(Map#{1 -> 1})

    @test
    def sequence_Map04(): Bool =
        let m = Map#{1 -> Some(1), 2 -> None};
        Traversable.sequence(m) == None

    @test
    def sequence_Map05(): Bool =
        let m = Map#{1 -> Some(1), 2 -> Some(2)};
        Traversable.sequence(m) == Some(Map#{1 -> 1, 2 -> 2})

    /////////////////////////////////////////////////////////////////////////////
    // traverse_Map                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse_Map01(): Bool =
        let m: Map[Int, Int] = Map#{};
        Traversable.traverse(x -> Some(x+1), m) == Some(Map#{})

    @test
    def traverse_Map02(): Bool =
        let m: Map[Int, Int] = Map#{1 -> 1};
        Traversable.traverse(x -> Some(x+1), m) == Some(Map#{1 -> 2})

    @test
    def traverse_Map03(): Bool =
        let m: Map[Int, Int] = Map#{1 -> 1, 2 -> 2};
        Traversable.traverse(x -> Some(x+1), m) == Some(Map#{1 -> 2, 2 -> 3})

    @test
    def traverse_Map04(): Bool =
        let m: Map[Int, Int] = Map#{};
        Traversable.traverse(_ -> None, m) == Some(Map#{})

    @test
    def traverse_Map05(): Bool =
        let m: Map[Int, Int] = Map#{1 -> 1};
        Traversable.traverse(_ -> None, m) == None

    @test
    def traverse_Map06(): Bool =
        let m: Map[Int, Int] = Map#{1 -> 1, 2 -> 2};
        Traversable.traverse(_ -> None, m) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft_Map                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft_Map01(): Bool =
        let m: Map[Int, Int] = Map#{};
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (10, Map#{})

    @test
    def mapAccumLeft_Map02(): Bool =
        let m: Map[Int, Int] = Map#{1 -> 1};
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (11, Map#{1 -> 11})

    @test
    def mapAccumLeft_Map03(): Bool =
        let m: Map[Int, Int]  = Map#{1 -> 1, 2 -> 2};
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (12, Map#{1 -> 11, 2 -> 13})

    @test
    def mapAccumLeft_Map04(): Bool =
        let m: Map[Int, Int]  = Map#{1 -> 1, 2 -> 2, 3 -> 3};
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (13, Map#{1 -> 11, 2 -> 13, 3 -> 15})

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight_Map                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight_Map01(): Bool =
        let m: Map[Int, Int] = Map#{};
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (10, Map#{})

    @test
    def mapAccumRight_Map02(): Bool =
        let m: Map[Int, Int] = Map#{1 -> 1};
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (11, Map#{1 -> 11})

    @test
    def mapAccumRight_Map03(): Bool =
        let m: Map[Int, Int]  = Map#{1 -> 1, 2 -> 2};
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (12, Map#{1 -> 12, 2 -> 12})

    @test
    def mapAccumRight_Map04(): Bool =
        let m: Map[Int, Int]  = Map#{1 -> 1, 2 -> 2, 3 -> 3};
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (13, Map#{1 -> 13, 2 -> 13, 3 -> 13})

    /////////////////////////////////////////////////////////////////////////////
    /// Helper for RedBlackTree                                                //
    /////////////////////////////////////////////////////////////////////////////

    def toRedBlackTree(xs: List[(k, v)]): RedBlackTree.RedBlackTree[k, v] with Order[k] =
        List.foldLeft((acc, kv) -> {let (k,v) = kv; RedBlackTree.insert(k, v, acc)}, RedBlackTree.empty(), xs)

    /////////////////////////////////////////////////////////////////////////////
    // sequence_RedBlackTree                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence_RedBlackTree01(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Option[Int]] = RedBlackTree.empty();
        Traversable.sequence(t) == Some(RedBlackTree.empty())

    @test
    def sequence_RedBlackTree02(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Option[Int]] = toRedBlackTree((1, None) :: Nil);
        Traversable.sequence(t) == None

    @test
    def sequence_RedBlackTree03(): Bool =
        let t = toRedBlackTree((1, Some(1)) :: Nil);
        Traversable.sequence(t) == Some(toRedBlackTree((1, 1) :: Nil))

    @test
    def sequence_RedBlackTree04(): Bool =
        let t = toRedBlackTree((1, Some(1)) :: (2, None) :: Nil);
        Traversable.sequence(t) == None

    @test
    def sequence_RedBlackTree05(): Bool =
        let t = toRedBlackTree((1, Some(1)) :: (2, Some(2)) :: Nil);
        Traversable.sequence(t) == Some(toRedBlackTree((1, 1) :: (2, 2) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse_RedBlackTree                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse_RedBlackTree01(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Int] = RedBlackTree.empty();
        Traversable.traverse(x -> Some(x+1), t) == Some(RedBlackTree.empty())

    @test
    def traverse_RedBlackTree02(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: Nil);
        Traversable.traverse(x -> Some(x+1), t) == Some(toRedBlackTree((1, 2) :: Nil))

    @test
    def traverse_RedBlackTree03(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        Traversable.traverse(x -> Some(x+1), t) == Some(toRedBlackTree((1, 2) :: (2, 3) :: Nil))

    @test
    def traverse_RedBlackTree04(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Int] = RedBlackTree.empty();
        Traversable.traverse(_ -> None, t) == Some(RedBlackTree.empty())

    @test
    def traverse_RedBlackTree05(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1):: Nil);
        Traversable.traverse(_ -> None, t) == None

    @test
    def traverse_RedBlackTree06(): Bool =
        let t: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        Traversable.traverse(_ -> None, t) == None

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft_RedBlackTree                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft_RedBlackTree01(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = RedBlackTree.empty();
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (10, RedBlackTree.empty())

    @test
    def mapAccumLeft_RedBlackTree02(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (11, toRedBlackTree((1, 11) :: Nil))

    @test
    def mapAccumLeft_RedBlackTree03(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (12, toRedBlackTree((1, 11) :: (2, 13) :: Nil))

    @test
    def mapAccumLeft_RedBlackTree04(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: (2, 2) :: (3, 3) :: Nil);
        Traversable.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m) == (13, toRedBlackTree((1, 11) :: (2, 13) :: (3, 15) :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight_RedBlackTree                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight_RedBlackTree01(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = RedBlackTree.empty();
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (10, RedBlackTree.empty())

    @test
    def mapAccumRight_RedBlackTree02(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (11, toRedBlackTree((1, 11) :: Nil))

    @test
    def mapAccumRight_RedBlackTree03(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (12, toRedBlackTree((1, 12) :: (2, 12) :: Nil))

    @test
    def mapAccumRight_RedBlackTree04(): Bool =
        let m: RedBlackTree.RedBlackTree[Int, Int] = toRedBlackTree((1, 1) :: (2, 2) :: (3, 3) :: Nil);
        Traversable.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, m) == (13, toRedBlackTree((1, 13) :: (2, 13) :: (3, 13) :: Nil))



}
