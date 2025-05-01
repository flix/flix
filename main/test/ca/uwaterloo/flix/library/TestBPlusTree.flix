mod TestBPlusTree {
    import java.lang.Object
    import java.util.Objects
    use Array.sameElements
    use BPlusTree.Node
    def listToMap(l: List[(k, v)]): Map[k, v] with Order[k] =
        List.foldLeft(acc -> p -> let (x, y) = p; Map.insert(x, y, acc), Map#{}, l)

    /////////////////////////////////////////////////////////////////////////////
    // Test concurrency                                                        //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Calls `f` `callNum` times and returns the and of the result.
    ///
    def repeatN(f: Unit -> Bool \ ef, callNum: Int64): Bool \ ef = 
        def repeatNInternal(callNumInternal: Int64, res: Bool): Bool \ ef = {
            if (callNumInternal <= 0i64) res else {
                // If the call to f is inlined Flix crashes for some reason.
                let f_ = f();
                repeatNInternal(callNumInternal - 1i64, res and f_)
            }
        };
        repeatNInternal(callNum, true)

    def totalInsertNum(): Int64 = 300_000i64

    ///
    /// `gen` and `inserted` are the functions that generate the inserted values.
    /// If `inserted` is Some we check that they have been inserted.
    /// For both `gen` and `inserted` every element of the vector will be called `numInserts` times and, respectively, be inserted or test membership.
    ///
    def concurrencyWithGenerators(gen: Vector[Unit -> (a, b) \ ef1], numInserts: Int64, inserted: Option[Vector[Unit -> (a, b) \ ef2]]): Bool \ ef2 + IO + NonDet - Random with Order[a], Eq[b] = region rc {
        let tree: BPlusTree[a, b, rc]  = BPlusTree.emptyWithOrder(rc, 10); // Small order to get more splits.
        let seeds = Random.runWithIO(() -> Vector.map(_ -> Random.randomInt64(), gen));
        let _: Unit = region rc2 {
            Vector.forEach(inputt -> {
                let (f, seed) = inputt;
                // Have to remove the effect ef1 and rc as spawn does not accept them
                spawn unchecked_cast(() -> {
                    Random.runWithSeed(seed, () -> repeatN(() -> {
                        let (x, y) = f();
                        BPlusTree.insert(x, y, tree);
                        true
                    }, numInserts)); ()
                } as Unit -> Unit \ IO)() @ rc2}
            , Vector.zip(gen, seeds))
        };
        BPlusTree.assertTreeInvariant(tree)
         and match inserted {
            case None => true
            case Some(shouldBeIn) =>
            Vector.forAll(inputt -> {
                let (f, seed) = inputt;
                Random.runWithSeed(seed, () -> repeatN(() -> {
                    let (x, _) = f();
                    BPlusTree.memberOf(x, tree)
                }, numInserts))}
            , Vector.zip(shouldBeIn, seeds))
        }
    }

    ///
    /// `gen` and `inserted` are the functions that generate the inserted values.
    /// If `inserted` is Some we check that they have been inserted.
    /// For both `gen` and `inserted` every element of the vector will be called `numInserts` times and, respectively, be inserted or test membership
    ///
    def concurrencyWithGeneratorsAndOccasionalTests(gen: Vector[Unit -> (a, b) \ ef1], numInserts: Int64, inserted: Vector[Unit -> (a, b) \ ef2], readEvery: Int64): Bool \ IO + NonDet - Random with Order[a], Eq[b] = region rc {
        let tree: BPlusTree[a, b, rc]  = BPlusTree.emptyWithOrder(rc, 10); // Small order to get more splits.
        let seeds = Random.runWithIO(() -> Vector.map(_ -> Random.randomInt64(), gen));
        let resultVec = Vector.map(_ -> Ref.fresh(rc, false), Vector.range(0, Vector.length(gen)));
        let insertWithSeed = Vector.map(match (seed, insert) -> Random.handleWithSeed(seed, insert), Vector.zip(seeds, inserted));
        let _: Unit = region rc2 {
            Vector.forEach(inputt -> {
                let (((f, seed), insertedFunc), resultRef) = inputt;
                // Have to remove the effect ef1 and rc as spawn does not accept them
                spawn unchecked_cast(() -> {
                    let countRef = Ref.fresh(rc, 0i64);
                    Random.runWithSeed(seed, () -> Ref.put(repeatN(() -> {
                        let (x, y) = f();
                        BPlusTree.insert(x, y, tree);
                        let cur = Ref.get(countRef);
                        if (cur >= readEvery) {
                            let (x_, _) = insertedFunc();
                            Ref.put(0i64, countRef);
                            BPlusTree.memberOf(x_, tree)
                        } else {
                            Ref.put(cur + 1i64, countRef);
                            true
                        }
                    }, numInserts), resultRef))
                } as Unit -> Unit \ IO)() @ rc2
                }
            , Vector.zip(Vector.zip(Vector.zip(gen, seeds), insertWithSeed), resultVec))
        };
        Vector.forAll(x -> Ref.get(x), resultVec)
        // res
    }

    ///
    /// Run concurrencyWithGenerators with random values as inserts.
    ///
    def concurrencyRandomInserts(numThreads: Int32, totalInserts: Int64): Bool \ IO + NonDet =
        let generators = Vector.map(_ -> () -> (Random.randomInt64(), Random.randomInt32()), Vector.range(0, numThreads));
        let insertions = Vector.map(_ -> () -> (Random.randomInt64(), Random.randomInt32()), Vector.range(0, numThreads));
        concurrencyWithGenerators(generators, totalInserts / Int32.toInt64(numThreads), Some(insertions))

    ///
    /// Run concurrencyWithGenerators with random values between 0 (inclusive) and `rangeSize` (exlusive) as inserts.
    ///
    def concurrencyRandomInsertsClamped(numThreads: Int32, totalInserts: Int64, rangeSize: Int64): Bool \ IO + NonDet =
        let generators = Vector.map(_ -> () -> (Int64.modulo(Random.randomInt64(), rangeSize), Random.randomInt32()), Vector.range(0, numThreads));
        let insertions = Vector.map(_ -> () -> (Int64.modulo(Random.randomInt64(), rangeSize), Random.randomInt32()), Vector.range(0, numThreads));
        concurrencyWithGenerators(generators, totalInserts / Int32.toInt64(numThreads), Some(insertions))

    ///
    /// Run concurrencyWithGenerators with [1, 2, ..., `numInserts`] as inserts for all threads.
    ///
    def concurrencyLockStep(numThreads: Int32, totalInserts: Int64): Bool \ IO + NonDet = region rc {
        let range = Vector.range(0, numThreads);
        let generators = Vector.map(reff -> () -> {let next = Ref.get(reff); Ref.put(next + 1i64, reff); (next, next)}, Vector.map(_ -> Ref.fresh(rc, 0i64), range));
        let insertions = Vector.map(reff -> () -> {let next = Ref.get(reff); Ref.put(next + 1i64, reff); (next, next)}, Vector.map(_ -> Ref.fresh(rc, 0i64), range));
        concurrencyWithGenerators(generators, totalInserts, Some(insertions))
    }

        ///
    /// Run concurrencyWithGeneratorsAndOccasionalTests with random values as inserts.
    ///
    def concurrencyRandomInsertsIntermittentMemberOf(numThreads: Int32, totalInserts: Int64, readEvery: Int64): Bool \ IO + NonDet =
        let generators = Vector.map(_ -> () -> (Random.randomInt64(), Random.randomInt32()), Vector.range(0, numThreads));
        let insertions = Vector.map(_ -> () -> (Random.randomInt64(), Random.randomInt32()), Vector.range(0, numThreads));
        concurrencyWithGeneratorsAndOccasionalTests(generators, totalInserts / Int32.toInt64(numThreads), insertions, readEvery)

    ///
    /// Run concurrencyWithGeneratorsAndOccasionalTests with random values between 0 (inclusive) and `rangeSize` (exlusive) as inserts.
    ///
    def concurrencyRandomInsertsClampedIntermittentMemberOf(numThreads: Int32, totalInserts: Int64, rangeSize: Int64, readEvery: Int64): Bool \ IO + NonDet =
        let generators = Vector.map(_ -> () -> (Int64.modulo(Random.randomInt64(), rangeSize), Random.randomInt32()), Vector.range(0, numThreads));
        let insertions = Vector.map(_ -> () -> (Int64.modulo(Random.randomInt64(), rangeSize), Random.randomInt32()), Vector.range(0, numThreads));
        concurrencyWithGeneratorsAndOccasionalTests(generators, totalInserts / Int32.toInt64(numThreads), insertions, readEvery)

    ///
    /// Run concurrencyWithGeneratorsAndOccasionalTests with [1, 2, ..., `numInserts`] as inserts for all threads.
    ///
    def concurrencyLockStepIntermittentMemberOf(numThreads: Int32, totalInserts: Int64, readEvery: Int64): Bool \ IO + NonDet = region rc {
        let range = Vector.range(0, numThreads);
        let generators = Vector.map(reff -> () -> {let next = Ref.get(reff); Ref.put(next + 1i64, reff); (next, next)}, Vector.map(_ -> Ref.fresh(rc, 0i64), range));
        let insertions = Vector.map(reff -> () -> {let next = Ref.get(reff); Ref.put(next + 1i64, reff); (next, next)}, Vector.map(_ -> Ref.fresh(rc, 0i64), range));
        concurrencyWithGeneratorsAndOccasionalTests(generators, totalInserts, insertions, readEvery)
    }



    // Large concurrent insertion test-cases
    @test
    def concurrency01(): Bool \ IO + NonDet = concurrencyRandomInserts(10, totalInsertNum())

    @test
    def concurrency02(): Bool \ IO + NonDet = concurrencyRandomInserts(20, totalInsertNum())

    @test
    def concurrency03(): Bool \ IO + NonDet = concurrencyRandomInserts(30, totalInsertNum())

    @test
    def concurrency04(): Bool \ IO + NonDet = concurrencyRandomInsertsClamped(10, totalInsertNum(), 10_000i64)

    @test
    def concurrency05(): Bool \ IO + NonDet = concurrencyRandomInsertsClamped(30, totalInsertNum(), 10_000i64)

    @test
    def concurrency06(): Bool \ IO + NonDet = concurrencyRandomInsertsClamped(20, totalInsertNum(), 10_000i64)

    @test
    def concurrency07(): Bool \ IO + NonDet = concurrencyRandomInsertsClamped(40, totalInsertNum(), 100_000i64)

    @test
    def concurrency08(): Bool \ IO + NonDet = concurrencyLockStep(10, totalInsertNum())

    @test
    def concurrency09(): Bool \ IO + NonDet = concurrencyLockStep(30, totalInsertNum())

    @test
    def concurrency10(): Bool \ IO + NonDet = concurrencyRandomInsertsIntermittentMemberOf(10, totalInsertNum(), 10i64)

    @test
    def concurrency11(): Bool \ IO + NonDet = concurrencyRandomInsertsIntermittentMemberOf(20, totalInsertNum(), 10i64)

    @test
    def concurrency12(): Bool \ IO + NonDet = concurrencyRandomInsertsIntermittentMemberOf(30, totalInsertNum(), 10i64)

    @test
    def concurrency13(): Bool \ IO + NonDet = concurrencyRandomInsertsClampedIntermittentMemberOf(10, totalInsertNum(), 10_000i64, 10i64)

    @test
    def concurrency14(): Bool \ IO + NonDet = concurrencyRandomInsertsClampedIntermittentMemberOf(30, totalInsertNum(), 10_000i64, 10i64)

    @test
    def concurrency15(): Bool \ IO + NonDet = concurrencyRandomInsertsClampedIntermittentMemberOf(20, totalInsertNum(), 10_000i64, 10i64)

    @test
    def concurrency16(): Bool \ IO + NonDet = concurrencyRandomInsertsClampedIntermittentMemberOf(40, totalInsertNum(), 100_000i64, 10i64)

    @test
    def concurrency17(): Bool \ IO + NonDet = concurrencyLockStepIntermittentMemberOf(10, totalInsertNum(), 10i64)

    @test
    def concurrency18(): Bool \ IO + NonDet = concurrencyLockStepIntermittentMemberOf(30, totalInsertNum(), 10i64)

    /////////////////////////////////////////////////////////////////////////////
    // containsPair                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def containsPair01(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        not BPlusTree.containsPair(0, 0, tree) and
            not BPlusTree.containsPair(0, 1, tree) and
            not BPlusTree.containsPair(1, 0, tree)
    }
    @test
    def containsPair02(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(1, 2, tree);
        BPlusTree.containsPair(1, 2, tree) and
            not BPlusTree.containsPair(2, 1, tree) and
            not BPlusTree.containsPair(1, 3, tree)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.forEach(f, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def forEach02(): Bool = region rc {
        let tree = rangeQueryMultipleValues(rc);
        let list = MutList.empty(rc);
        let f = k -> v -> MutList.push((k, v), list);
        BPlusTree.forEach(f, tree);
        let res = Vector#{(0, 12), (1, 12), (2, 13), (3, 14)};
        MutList.toVector(list) `Vector.equals` res
    }

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getWithDefault01(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithOrder(rc, 3);
        42 == BPlusTree.getWithDefault(0, 42, tree)
    }

    @test
    def getWithDefault02(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(0, 10, tree);
        10 == BPlusTree.getWithDefault(0, 42, tree)
    }

    @test
    def getWithDefault03(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(0, 10, tree);
        BPlusTree.insert(1, 11, tree);
        BPlusTree.insert(2, 12, tree);
        BPlusTree.insert(3, 13, tree);
        BPlusTree.insert(4, 14, tree);
        BPlusTree.insert(5, 15, tree);
        BPlusTree.insert(6, 16, tree);
        10 == BPlusTree.getWithDefault(0, 42, tree)
        and 11 == BPlusTree.getWithDefault(1, 42, tree)
        and 12 == BPlusTree.getWithDefault(2, 42, tree)
        and 13 == BPlusTree.getWithDefault(3, 42, tree)
        and 14 == BPlusTree.getWithDefault(4, 42, tree)
        and 15 == BPlusTree.getWithDefault(5, 42, tree)
        and 16 == BPlusTree.getWithDefault(6, 42, tree)
        and 42 == BPlusTree.getWithDefault(-1, 42, tree)
        and 42 == BPlusTree.getWithDefault(-2, 42, tree)
        and 42 == BPlusTree.getWithDefault(7, 42, tree)
        and 42 == BPlusTree.getWithDefault(8, 42, tree)
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.isEmpty(tree)
    }

    @test
    def isEmpty02(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(1, 2, tree);
        not BPlusTree.isEmpty(tree)
    }

    @test
    def isEmpty03(): Bool = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithOrder(rc, 3);
        let empty0 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(1, 2, tree);
        let empty1 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(2, 3, tree);
        let empty2 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(3, 4, tree);
        let empty3 = BPlusTree.isEmpty(tree);
        BPlusTree.insert(4, 5, tree);
        let empty4 = BPlusTree.isEmpty(tree);
        empty0 and not empty1 and not empty2 and not empty3 and not empty4
    }

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert01(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(4, 2, tree);
        BPlusTree.containsPair(4, 2 ,tree) and BPlusTree.assertTreeInvariant(tree)
    }

    @test
    def insert02(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(1, 10, tree);
        BPlusTree.insert(2, 11, tree);
        BPlusTree.insert(3, 12, tree);
        BPlusTree.containsPair(1, 10, tree) and
            BPlusTree.containsPair(2, 11, tree) and
            BPlusTree.containsPair(3, 12, tree) and
            BPlusTree.assertTreeInvariant(tree)
    }

    @test
    def insert03(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(6, 15, tree);
        BPlusTree.insert(5, 14, tree);
        BPlusTree.insert(4, 13, tree);
        BPlusTree.insert(3, 12, tree);
        BPlusTree.insert(2, 11, tree);
        BPlusTree.insert(1, 10, tree);
        BPlusTree.containsPair(1, 10, tree) and
            BPlusTree.containsPair(2, 11, tree) and
            BPlusTree.containsPair(3, 12, tree) and
            BPlusTree.containsPair(4, 13, tree) and
            BPlusTree.containsPair(5, 14, tree) and
            BPlusTree.containsPair(6, 15, tree) and
            BPlusTree.assertTreeInvariant(tree)
    }

    @test
    def insert04(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 5);
        let pairs = List.map(p -> {
            let (x, y) = p;
            (Int32.modulo(x * 1000, 179), Int32.modulo(y * 1003, 109))
        }, List.zip(List.range(1, 10_000), List.reverse(List.range(1, 10_000))));
        List.forEach(p -> {
            let (x, y) = p;
            BPlusTree.insert(x, y, tree)
        }, pairs);
        Map.forAll(x -> y -> {
            BPlusTree.containsPair(x, y, tree)
        }, listToMap(pairs)) and BPlusTree.assertTreeInvariant(tree)
    }

    @test
    def insert05(): Bool = region rc {
        List.forAll(order -> {
            let tree = BPlusTree.emptyWithOrder(rc, order);
            let pairs = List.map(p -> {
                let (x, y) = p;
                (Int32.modulo(x * 1000, 179), Int32.modulo(y * 1003, 109))
            }, List.zip(List.range(1, 100_000), List.reverse(List.range(1, 100_000))));
            List.forEach(p -> {
                let (x, y) = p;
                BPlusTree.insert(x, y, tree)
            }, pairs);
            Map.forAll((x, y) -> {
                BPlusTree.containsPair(x, y, tree)
            }, listToMap(pairs)) and BPlusTree.assertTreeInvariant(tree)
        }, List.range(3, 10))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(2, 3, tree);
        BPlusTree.memberOf(2, tree)
    }


    @test
    def memberOf02(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        not BPlusTree.memberOf(0, tree) and
            not BPlusTree.memberOf(1, tree) and
            not BPlusTree.memberOf(-1, tree)
    }

    @test
    def memberOf03(): Bool = region rc {
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(1, 2, tree);
        BPlusTree.memberOf(1, tree) and
            not BPlusTree.memberOf(2, tree) and
            not BPlusTree.memberOf(0, tree)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rangeQueryWith                                                          //
    /////////////////////////////////////////////////////////////////////////////

    def rangeQueryDefaultTree(rc: Region[r]): BPlusTree[Int32, Int32, r] \ r =
        let tree: BPlusTree[Int32, Int32, r] = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(0, 10, tree);
        BPlusTree.insert(1, 11, tree);
        BPlusTree.insert(2, 12, tree);
        BPlusTree.insert(3, 13, tree);
        BPlusTree.insert(4, 14, tree);
        BPlusTree.insert(5, 15, tree);
        tree

    def rangeQueryMultipleValues(rc: Region[r]): BPlusTree[Int32, Int32, r] \ r =
        let tree = BPlusTree.emptyWithOrder(rc, 3);
        BPlusTree.insert(0, 10, tree);
        BPlusTree.insert(0, 11, tree);
        BPlusTree.insert(0, 12, tree);
        BPlusTree.insert(1, 11, tree);
        BPlusTree.insert(1, 12, tree);
        BPlusTree.insert(2, 13, tree);
        BPlusTree.insert(3, 14, tree);
        tree

    @test
    def rangeQueryWith01(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 0, 5, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith02(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, 5, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith03(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, 7, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith04(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 1, 1, tree);
        let res = Vector#{1};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith05(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 1, 3, tree);
        let res = Vector#{1, 2, 3};
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith06(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, -2, tree);
        let res = Vector#{ };
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith07(): Bool = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 7, 7, tree);
        let res = Vector#{ };
        MutList.toVector(list) `Vector.equals` res
    }

    @test
    def rangeQueryWith08(): Bool = region rc {
        let tree = rangeQueryMultipleValues(rc);
        let list = MutList.empty(rc);
        let f = k -> v -> MutList.push((k, v), list);
        BPlusTree.rangeQueryWith(f, 0, 3, tree);
        let res = Vector#{(0, 12), (1, 12), (2, 13), (3, 14)};
        MutList.toVector(list) `Vector.equals` res
    }
}
