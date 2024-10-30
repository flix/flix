/*
 * Copyright 2024 Stephen Tetley
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

mod TestCollectable {

    /////////////////////////////////////////////////////////////////////////////
    // Chain instance                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def collectChain01(): Bool = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        (Collectable.collect(iter) : Chain[Int32]) == Chain.empty()
    }

    @test
    def collectChain02(): Bool = region rc {
        let iter = Iterator.singleton(rc, 1);
        Collectable.collect(iter) == Chain.singleton(1)
    }

    @test
    def collectChain03(): Bool = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        Collectable.collect(iter) == List.toChain(List#{1, 2, 3})
    }

    @test
    def collectChain04(): Bool = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        Collectable.collect(iter) == List.toChain(List#{'C', 'B', 'A'})
    }

    /////////////////////////////////////////////////////////////////////////////
    // List instance                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def collectList01(): Bool = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        (Collectable.collect(iter) : List[Int32]) == Nil
    }

    @test
    def collectList02(): Bool = region rc {
        let iter = Iterator.singleton(rc, 1);
        Collectable.collect(iter) == List#{1}
    }

    @test
    def collectList03(): Bool = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        Collectable.collect(iter) == List#{1, 2, 3}
    }

    @test
    def collectList04(): Bool = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        Collectable.collect(iter) == List#{'C', 'B', 'A'}
    }

    /////////////////////////////////////////////////////////////////////////////
    // Set instance                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def collectSet01(): Bool = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        (Collectable.collect(iter) : Set[Int32]) == Set.empty()
    }

    @test
    def collectSet02(): Bool = region rc {
        let iter = Iterator.singleton(rc, 1);
        Collectable.collect(iter) == Set#{1}
    }

    @test
    def collectSet03(): Bool = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        Collectable.collect(iter) == Set#{1, 2, 3}
    }

    @test
    def collectSet04(): Bool = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        Collectable.collect(iter) == Set#{'A', 'B', 'C'}
    }

    /////////////////////////////////////////////////////////////////////////////
    // Set instance                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def collectVector01(): Bool = region rc {
        let iter: Iterator[Int32, rc, rc] = Iterator.empty(rc);
        (Collectable.collect(iter) : Vector[Int32]) == Vector#{}
    }

    @test
    def collectVector02(): Bool = region rc {
        let iter = Iterator.singleton(rc, 1);
        Collectable.collect(iter) == Vector#{1}
    }

    @test
    def collectVector03(): Bool = region rc {
        let iter = List.iterator(rc, List#{1, 2, 3});
        Collectable.collect(iter) == Vector#{1, 2, 3}
    }

    @test
    def collectVector04(): Bool = region rc {
        let iter = List.iterator(rc, List#{'C', 'B', 'A'});
        Collectable.collect(iter) == Vector#{'C', 'B', 'A'}
    }

}
