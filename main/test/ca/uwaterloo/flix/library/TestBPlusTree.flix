mod TestBPlusTree {
    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, fail};
    import java.lang.Object
    import java.util.Objects
    use Array.sameElements
    use BPlusTree.Node

    def listToMap(l: List[(k, v)]): Map[k, v] with Order[k] =
        List.foldLeft(acc -> p -> let (x, y) = p; Map.insert(x, y, acc), Map#{}, l)

    def preferSome(l: Option[Int32], r: Option[Int32]): Option[Int32] = match l {
        case None => r
        case Some(_) => l
    }

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

    def totalInsertNum(): Int64 = 100_000i64

    def totalInsertNumInt32(): Int32 = 100_000

    ///
    /// `gen` and `inserted` are the functions that generate the inserted values.
    /// If `inserted` is Some we check that they have been inserted.
    /// For both `gen` and `inserted` every element of the vector will be called `numInserts` times and, respectively, be inserted or test membership.
    ///
    def concurrencyWithGenerators(gen: Vector[Unit -> (a, b) \ ef1], numInserts: Int64, inserted: Option[Vector[Unit -> (a, b) \ ef2]]): Bool \ ef2 + IO + NonDet - Random with Order[a], Eq[b] = region rc {
        let tree: BPlusTree[a, b, rc] = BPlusTree.emptyWithArity(rc, 10); // Small arity to get more splits.
        let seeds = Random.runWithIO(() -> Vector.map(_ -> Random.randomInt64(), gen));
        let _: Unit = region rc2 {
            Vector.forEach(inputt -> {
                let (f, seed) = inputt;
                // Have to remove the effect ef1 and rc as spawn does not accept them
                spawn unchecked_cast(() -> {
                    discard Random.runWithSeed(seed, () -> repeatN(() -> {
                        let (x, y) = f();
                        BPlusTree.put(x, y, tree);
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
        let tree: BPlusTree[a, b, rc] = BPlusTree.emptyWithArity(rc, 10); // Small arity to get more splits.
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
                        BPlusTree.put(x, y, tree);
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
    @Test
    def concurrency01(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInserts(10, totalInsertNum()))

    @Test
    def concurrency02(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInserts(30, totalInsertNum()))

    @Test
    def concurrency03(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInsertsClamped(10, totalInsertNum(), 10_000i64))

    @Test
    def concurrency04(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInsertsClamped(30, totalInsertNum(), 40_000i64))

    @Test
    def concurrency05(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyLockStep(10, totalInsertNum()))

    @Test
    def concurrency06(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyLockStep(30, totalInsertNum()))

    @Test
    def concurrency07(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInsertsIntermittentMemberOf(10, totalInsertNum(), 10i64))

    @Test
    def concurrency08(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInsertsIntermittentMemberOf(30, totalInsertNum(), 10i64))

    @Test
    def concurrency09(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInsertsClampedIntermittentMemberOf(10, totalInsertNum(), 10_000i64, 10i64))

    @Test
    def concurrency10(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyRandomInsertsClampedIntermittentMemberOf(30, totalInsertNum(), 40_000i64, 10i64))

    @Test
    def concurrency11(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyLockStepIntermittentMemberOf(10, totalInsertNum(), 10i64))

    @Test
    def concurrency12(): Unit \ Assert + IO + NonDet =
        assertTrue(concurrencyLockStepIntermittentMemberOf(30, totalInsertNum(), 10i64))

    /////////////////////////////////////////////////////////////////////////////
    // memberOfPair                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOfPair01(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        assertFalse(BPlusTree.memberOfPair(0, 0, tree));
        assertFalse(BPlusTree.memberOfPair(0, 1, tree));
        assertFalse(BPlusTree.memberOfPair(1, 0, tree))
    }
    @Test
    def memberOfPair02(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 2, tree);
        assertTrue(BPlusTree.memberOfPair(1, 2, tree));
        assertFalse(BPlusTree.memberOfPair(2, 1, tree));
        assertFalse(BPlusTree.memberOfPair(1, 3, tree))
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.forEach(f, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let tree = rangeQueryMultipleValues(rc);
        let list = MutList.empty(rc);
        let f = k -> v -> MutList.push((k, v), list);
        BPlusTree.forEach(f, tree);
        let res = Vector#{(0, 12), (1, 12), (2, 13), (3, 14)};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foreach01(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = kv -> MutList.push(fst(kv), list);
        foreach (t <- tree) f(t);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def foreach02(): Unit \ Assert = region rc {
        let tree = rangeQueryMultipleValues(rc);
        let list = MutList.empty(rc);
        let f = kv -> MutList.push(kv, list);
        foreach(t <- tree) f(t);
        let res = Vector#{(0, 12), (1, 12), (2, 13), (3, 14)};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def foldLeft01(): Unit \ Assert = region rc{
        let t = BPlusTree.empty(rc);
        BPlusTree.put(1, "a", t);
        BPlusTree.put(2, "b", t);
        BPlusTree.put(3, "c", t);
        let res = BPlusTree.foldLeft(acc -> v -> acc + v, "", t);
        Assert.assertEq(expected = "abc", res)
    }

    @Test
    pub def foldLeft02(): Unit \ Assert = region rc{
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 6, t);
        BPlusTree.put(2, 3, t);
        BPlusTree.put(3, 5, t);
        BPlusTree.put(4, 8, t);
        BPlusTree.put(5, 9, t);
        BPlusTree.put(6, 11, t);
        BPlusTree.put(7, 15, t);
        BPlusTree.put(8, 12, t);
        BPlusTree.put(9, 2, t);
        BPlusTree.put(10, -1, t);
        let res = BPlusTree.foldLeft(acc -> v -> v :: acc, List.empty(), t);
        Assert.assertEq(expected = List#{-1, 2, 12, 15, 11, 9, 8, 5, 3, 6}, res)
    }

    @Test
    pub def foldLeft03(): Unit \ Assert = region rc{
        let t: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        let res = BPlusTree.foldLeft(acc -> v -> v :: acc, List.empty(), t);
        Assert.assertEq(expected = List.empty(), res)
    }

    @Test
    pub def foldLeft04(): Unit \ Assert = region rc{
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 1, t);
        BPlusTree.put(2, 2, t);
        BPlusTree.put(3, 3, t);
        let res = BPlusTree.foldLeft(acc -> v -> "(${acc} + ${v})", "0", t);
        Assert.assertEq(expected = "(((0 + 1) + 2) + 3)", res)
    }

    @Test
    pub def foldLeft05(): Unit \ Assert = region rc{
        let t: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        let list = MutList.empty(rc);
        let res = (0, t) ||> BPlusTree.foldLeft(acc -> v -> {
            MutList.push(v, list);
            v + acc
        });
        Assert.assertEq(expected = 0, res);
        Assert.assertTrue(MutList.isEmpty(list))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def foldLeftWithKey01(): Unit \ Assert = region rc{
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(-1, 1, t);
        BPlusTree.put(5, 2, t);
        BPlusTree.put(8, 3, t);
        BPlusTree.put(13, 3, t);
        BPlusTree.put(-6, 3, t);
        let res = BPlusTree.foldLeftWithKey(acc -> k -> _ -> Set.insert(k, acc), Set.empty(), t);
        Assert.assertEq(expected = BPlusTree.keysOf(t), res)
    }

    @Test
    pub def foldLeftWithKey02(): Unit \ Assert = region rc{
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, "a", t);
        BPlusTree.put(2, "b", t);
        BPlusTree.put(3, "c", t);
        BPlusTree.put(4, "d", t);
        BPlusTree.put(5, "e", t);
        let res = BPlusTree.foldLeftWithKey(acc -> _ -> v -> v :: acc, List.empty(), t);
        Assert.assertEq(expected = "e" :: "d" :: "c" :: "b" :: "a" :: Nil, res)
    }

    @Test
    pub def foldLeftWithKey03(): Unit \ Assert = region rc{
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, "a", t);
        BPlusTree.put(2, "b", t);
        BPlusTree.put(3, "c", t);
        BPlusTree.put(4, "d", t);
        BPlusTree.put(5, "e", t);
        let res = BPlusTree.foldLeftWithKey(acc -> k -> v -> acc + "${(k, v)}", "", t);
        Assert.assertEq(expected = "(1, a)(2, b)(3, c)(4, d)(5, e)", res)
    }


    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getWithDefault01(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        assertEq(expected = 42, BPlusTree.getWithDefault(0, 42, tree))
    }

    @Test
    def getWithDefault02(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 10, tree);
        assertEq(expected = 10, BPlusTree.getWithDefault(0, 42, tree))
    }

    @Test
    def getWithDefault03(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 10, tree);
        BPlusTree.put(1, 11, tree);
        BPlusTree.put(2, 12, tree);
        BPlusTree.put(3, 13, tree);
        BPlusTree.put(4, 14, tree);
        BPlusTree.put(5, 15, tree);
        BPlusTree.put(6, 16, tree);
        assertEq(expected = 10, BPlusTree.getWithDefault(0, 42, tree));
        assertEq(expected = 11, BPlusTree.getWithDefault(1, 42, tree));
        assertEq(expected = 12, BPlusTree.getWithDefault(2, 42, tree));
        assertEq(expected = 13, BPlusTree.getWithDefault(3, 42, tree));
        assertEq(expected = 14, BPlusTree.getWithDefault(4, 42, tree));
        assertEq(expected = 15, BPlusTree.getWithDefault(5, 42, tree));
        assertEq(expected = 16, BPlusTree.getWithDefault(6, 42, tree));
        assertEq(expected = 42, BPlusTree.getWithDefault(-1, 42, tree));
        assertEq(expected = 42, BPlusTree.getWithDefault(-2, 42, tree));
        assertEq(expected = 42, BPlusTree.getWithDefault(7, 42, tree));
        assertEq(expected = 42, BPlusTree.getWithDefault(8, 42, tree))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        assertTrue(BPlusTree.isEmpty(tree))
    }

    @Test
    def isEmpty02(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 2, tree);
        assertFalse(BPlusTree.isEmpty(tree))
    }

    @Test
    def isEmpty03(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        let empty0 = BPlusTree.isEmpty(tree);
        BPlusTree.put(1, 2, tree);
        let empty1 = BPlusTree.isEmpty(tree);
        BPlusTree.put(2, 3, tree);
        let empty2 = BPlusTree.isEmpty(tree);
        BPlusTree.put(3, 4, tree);
        let empty3 = BPlusTree.isEmpty(tree);
        BPlusTree.put(4, 5, tree);
        let empty4 = BPlusTree.isEmpty(tree);
        assertTrue(empty0);
        assertFalse(empty1);
        assertFalse(empty2);
        assertFalse(empty3);
        assertFalse(empty4)
    }

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def keysOf01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(2, 5, t);
        BPlusTree.put(1, 4, t);
        BPlusTree.put(3, 6, t);
        let res = BPlusTree.keysOf(t);
        Assert.assertEq(expected = Set#{1, 2, 3}, res)
    }

    @Test
    pub def keysOf02(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 11, t);
        BPlusTree.put(5, 15, t);
        BPlusTree.put(3, 13, t);
        BPlusTree.put(6, 16, t);
        BPlusTree.put(2, 12, t);
        BPlusTree.put(4, 14, t);
        let res = BPlusTree.keysOf(t);
        Assert.assertEq(expected = Set#{1, 2, 3, 4, 5, 6}, res)
    }

    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def valuesOf01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(3, 5, t);
        BPlusTree.put(1, 7, t);
        BPlusTree.put(2, 6, t);
        let res = BPlusTree.valuesOf(t);
        Assert.assertEq(expected = List#{7, 6, 5}, res)
    }

    @Test
    pub def valuesOf02(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 7, t);
        BPlusTree.put(6, 5, t);
        BPlusTree.put(2, 6, t);
        BPlusTree.put(5, 3, t);
        BPlusTree.put(3, 1, t);
        BPlusTree.put(4, 5, t);
        let res = BPlusTree.valuesOf(t);
        Assert.assertEq(expected = List#{7, 6, 1, 5, 3, 5}, res)
    }

    /////////////////////////////////////////////////////////////////////////////
    // parForEach                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parForEach01(): Unit \ Assert + IO + NonDet = region rc {
        Random.runWithIO(() -> {
            let treeFrom = BPlusTree.empty(rc);
            let treeTo = BPlusTree.empty(rc);
            List.range(0, 50_000) |> List.forEach(_ -> {
                let x = Random.randomInt64();
                let y = Random.randomInt64();
                BPlusTree.put(x, y, treeFrom)
            });
            BPlusTree.parForEach(x -> y -> {
                BPlusTree.put(x, y, treeTo)
            }, treeFrom);
            assertTrue(List.toVector(BPlusTree.toList(treeFrom)) `Vector.equals` List.toVector(BPlusTree.toList(treeTo)))
        })
    }

    /////////////////////////////////////////////////////////////////////////////
    // parForEachWhen                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def parForEachWhen01(): Unit \ Assert + IO + NonDet = region rc {
        Random.runWithIO(() -> {
            let treeFrom = BPlusTree.empty(rc);
            let treeTo = BPlusTree.empty(rc);
            List.range(0, 50_000) |> List.forEach(_ -> {
                let x = Random.randomInt64();
                let y = Random.randomInt64();
                BPlusTree.put(x, y, treeFrom)
            });
            let f1 = x -> y -> BPlusTree.put(x, y, treeTo);
            let f2 = x -> y -> BPlusTree.put(x, y, treeTo);
            BPlusTree.parForEachWhen(f1, f2, 1000, treeFrom);
            assertTrue(List.toVector(BPlusTree.toList(treeFrom)) `Vector.equals` List.toVector(BPlusTree.toList(treeTo)))
        })
    }

    @Test
    def parForEachWhen02(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.parForEachWhen(f, f, 1000, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def parForEachWhen03(): Unit \ Assert = region rc {
        let tree = rangeQueryMultipleValues(rc);
        let list = MutList.empty(rc);
        let f = k -> v -> MutList.push((k, v), list);
        BPlusTree.parForEachWhen(f, f, 1000, tree);
        let res = Vector#{(0, 12), (1, 12), (2, 13), (3, 14)};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def parForEachWhen04(): Unit \ Assert = region rc {
        let values = List.range(0, 10_000) |> List.map(i -> (i, i+1));
        let tree = BPlusTree.empty(rc);
        foreach((k, v) <- values) {
            BPlusTree.put(k, v, tree)
        };
        let sequentialCollector = MutList.empty(rc);
        let parallelCollector = BPlusTree.empty(rc);
        let f1 = k -> v -> MutList.push((k, v), sequentialCollector);
        let f2 = k -> v -> BPlusTree.put(k, v, parallelCollector);
        BPlusTree.parForEachWhen(f1, f2, 10_000, tree);
        assertTrue(MutList.isEmpty(sequentialCollector));
        assertEq(expected = values, BPlusTree.toList(parallelCollector))
    }

    @Test
    def parForEachWhen05(): Unit \ Assert = region rc {
        let values = List.range(0, 9_999) |> List.map(i -> (i, i+1));
        let tree = BPlusTree.empty(rc);
        foreach((k, v) <- values) {
            BPlusTree.put(k, v, tree)
        };
        let sequentialCollector = MutList.empty(rc);
        let parallelCollector = BPlusTree.empty(rc);
        let f1 = k -> v -> MutList.push((k, v), sequentialCollector);
        let f2 = k -> v -> BPlusTree.put(k, v, parallelCollector);
        BPlusTree.parForEachWhen(f1, f2, 10_000, tree);
        assertEq(expected = values, MutList.toList(sequentialCollector));
        assertTrue(BPlusTree.isEmpty(parallelCollector))
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def put01(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(4, 2, tree);
        assertTrue(BPlusTree.memberOfPair(4, 2 ,tree));
        assertTrue(BPlusTree.assertTreeInvariant(tree))
    }

    @Test
    def put02(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 10, tree);
        BPlusTree.put(2, 11, tree);
        BPlusTree.put(3, 12, tree);
        assertTrue(BPlusTree.memberOfPair(1, 10, tree));
        assertTrue(BPlusTree.memberOfPair(2, 11, tree));
        assertTrue(BPlusTree.memberOfPair(3, 12, tree));
        assertTrue(BPlusTree.assertTreeInvariant(tree))
    }

    @Test
    def put03(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(6, 15, tree);
        BPlusTree.put(5, 14, tree);
        BPlusTree.put(4, 13, tree);
        BPlusTree.put(3, 12, tree);
        BPlusTree.put(2, 11, tree);
        BPlusTree.put(1, 10, tree);
        assertTrue(BPlusTree.memberOfPair(1, 10, tree));
        assertTrue(BPlusTree.memberOfPair(2, 11, tree));
        assertTrue(BPlusTree.memberOfPair(3, 12, tree));
        assertTrue(BPlusTree.memberOfPair(4, 13, tree));
        assertTrue(BPlusTree.memberOfPair(5, 14, tree));
        assertTrue(BPlusTree.memberOfPair(6, 15, tree));
        assertTrue(BPlusTree.assertTreeInvariant(tree))
    }

    @Test
    def put04(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 5);
        let pairs = List.map(p -> {
            let (x, y) = p;
            (Int32.modulo(x * 1000, 179), Int32.modulo(y * 1003, 109))
        }, List.zip(List.range(1, 10_000), List.reverse(List.range(1, 10_000))));
        List.forEach(p -> {
            let (x, y) = p;
            BPlusTree.put(x, y, tree)
        }, pairs);
        assertTrue(Map.forAll(x -> y -> {
            BPlusTree.memberOfPair(x, y, tree)
        }, listToMap(pairs)));
        assertTrue(BPlusTree.assertTreeInvariant(tree))
    }

    @Test
    def put05(): Unit \ Assert = region rc {
        let pairs = List.map(p -> {
            let (x, y) = p;
            (Int32.modulo(x * 1000, 179), Int32.modulo(y * 1003, 109))
        }, List.zip(List.range(1, 50_000), List.reverse(List.range(1, 50_000))));
        let pairsAsMap = listToMap(pairs);
        assertTrue(List.forAll(arity -> {
            let tree = BPlusTree.emptyWithArity(rc, arity);
            List.forEach(p -> {
                let (x, y) = p;
                BPlusTree.put(x, y, tree)
            }, pairs);
            Map.forAll((x, y) -> {
                BPlusTree.memberOfPair(x, y, tree)
            }, pairsAsMap) and BPlusTree.assertTreeInvariant(tree)
        }, List.range(3, 10)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def get01(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        assertTrue(BPlusTree.get(0, tree) |> Option.isEmpty)
    }

    @Test
    def get02(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(0, 1, tree);
        match BPlusTree.get(0, tree) {
            case Some(v) => assertEq(expected = 1, v)
            case None => fail("Expected Some(1), got None")
        }
    }

    @Test
    def get03(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(0, 1, tree);
        assertTrue(BPlusTree.get(1, tree) |> Option.isEmpty)
    }

    /////////////////////////////////////////////////////////////////////////////
    // getKeyAndValue                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getKeyAndValue01(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        assertTrue(BPlusTree.getKeyAndValue(0, tree) |> Option.isEmpty)
    }

    @Test
    def getKeyAndValue02(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(0, 1, tree);
        match BPlusTree.getKeyAndValue(0, tree) {
            case Some(v) => assertEq(expected = (0, 1), v)
            case None => fail("Expected Some((0, 1)), got None")
        }
    }

    @Test
    def getKeyAndValue03(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(0, 1, tree);
        assertTrue(BPlusTree.getKeyAndValue(1, tree) |> Option.isEmpty)
    }

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def plusOne(v: Int32): Int32 = v + 1

    @Test
    def adjust01(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 2, t);
        BPlusTree.adjust(plusOne, 1, t);
        Assert.assertEq(expected = Some(3), BPlusTree.get(1, t))
    }

    @Test
    def adjust02(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 2, t);
        BPlusTree.adjust(identity, 1, t);
        Assert.assertEq(expected = Some(2), BPlusTree.get(1, t))
    }

    @Test
    def adjust03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, "foo", t);
        BPlusTree.adjust(String.toUpperCase, 1, t);
        Assert.assertEq(expected = Some("FOO"), BPlusTree.get(1, t))
    }

    @Test
    def adjust04(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.adjust(plusOne, 1, t);
        Assert.assertNone(BPlusTree.get(1, t))
    }

    @Test
    def adjust05(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(2, 2, t);
        BPlusTree.put(5, 2, t);
        BPlusTree.put(6, 2, t);
        BPlusTree.put(-1, 2, t);
        BPlusTree.put(-5, 2, t);
        BPlusTree.put(20, 2, t);
        BPlusTree.adjust(plusOne, 6, t);
        Assert.assertEq(expected = Some(3), BPlusTree.get(6, t))
    }

    @Test
    def adjust06(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 300))
            BPlusTree.put(k, k * 100, t);
        BPlusTree.adjust(plusOne, 150, t);
        Assert.assertEq(expected = Some(15001), BPlusTree.get(150, t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    def addKeyToValue(k: Int32, v: Int32): Int32 = k + v

    @Test
    def adjustWithKey01(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(5, 12, t);
        BPlusTree.adjustWithKey(addKeyToValue, 5, t);
        Assert.assertEq(expected = Some(17), BPlusTree.get(5, t))
    }

    @Test
    def adjustWithKey02(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(5, 12, t);
        BPlusTree.adjustWithKey(addKeyToValue, -3, t);
        Assert.assertEq(expected = Some(12), BPlusTree.get(5, t));
        Assert.assertNone(BPlusTree.get(-3, t))
    }

    @Test
    def adjustWithKey03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 300))
            BPlusTree.put(k, k * 100, t);
        BPlusTree.adjustWithKey(addKeyToValue, 150, t);
        Assert.assertEq(expected = Some(15150), BPlusTree.get(150, t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // computeIfAbsent                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def computeIfAbsent01(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        discard BPlusTree.computeIfAbsent(() -> 2, 1, tree);
        assertTrue(BPlusTree.memberOfPair(1, 2, tree))
    }

    @Test
    def computeIfAbsent02(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(1, 3, tree);
        discard BPlusTree.computeIfAbsent(() -> 2, 1, tree);
        assertTrue(BPlusTree.memberOfPair(1, 3, tree));
        assertFalse(BPlusTree.memberOfPair(1, 2, tree))
    }

    @Test
    def computeIfAbsent03(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        let ref = Ref.fresh(rc, false);
        let effectfulFunction = () -> {
            Ref.put(true, ref);
            2
        };
        discard BPlusTree.computeIfAbsent(effectfulFunction, 1, tree);
        assertTrue(BPlusTree.memberOfPair(1, 2, tree));
        assertTrue(Ref.get(ref))
    }

    @Test
    def computeIfAbsent04(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        let ref = Ref.fresh(rc, true);
        let effectfulFunction = () -> {
            Ref.put(false, ref);
            2
        };
        BPlusTree.put(1, 3, tree);
        discard BPlusTree.computeIfAbsent(effectfulFunction, 1, tree);
        assertTrue(BPlusTree.memberOfPair(1, 3, tree));
        assertTrue(Ref.get(ref))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(2, 3, tree);
        assertTrue(BPlusTree.memberOf(2, tree))
    }

    @Test
    def memberOf02(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        assertFalse(BPlusTree.memberOf(0, tree));
        assertFalse(BPlusTree.memberOf(1, tree));
        assertFalse(BPlusTree.memberOf(-1, tree))
    }

    @Test
    def memberOf03(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 2, tree);
        assertTrue(BPlusTree.memberOf(1, tree));
        assertFalse(BPlusTree.memberOf(2, tree));
        assertFalse(BPlusTree.memberOf(0, tree))
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////
    
    @Test
    def joinValues01(): Unit \ Assert = region rc {
        let t: BPlusTree[_, Int32, _] = BPlusTree.empty(rc);
        Assert.assertEq(expected = "", BPlusTree.joinValues(", ", t))
    }

    @Test
    def joinValues02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(2, "b", t);
        BPlusTree.put(1, "a", t);
        BPlusTree.put(3, "c", t);

        Assert.assertEq(expected = "a, b, c", BPlusTree.joinValues(", ", t))
    }

    @Test
    def joinValues03(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(10, "x", t);

        Assert.assertEq(expected = "x", BPlusTree.joinValues(" | ", t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Unit \ Assert = region rc {
        let t: BPlusTree[Int32, _, _] = BPlusTree.empty(rc);
        Assert.assertEq(expected = "", BPlusTree.joinKeys(", ", t))
    }

    @Test
    def joinKeys02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(3, "c", t);
        BPlusTree.put(1, "a", t);
        BPlusTree.put(2, "b", t);

        Assert.assertEq(expected = "1, 2, 3", BPlusTree.joinKeys(", ", t))
    }

    @Test
    def joinKeys03(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(42, "x", t);

        Assert.assertEq(expected = "42", BPlusTree.joinKeys(" | ", t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        let t: BPlusTree[String, String, _] = BPlusTree.empty(rc);
        let f = k -> v -> "${k}:${v}";

        Assert.assertEq(expected = "", BPlusTree.joinWith(f, ", ", t))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(2, "b", t);
        BPlusTree.put(1, "a", t);
        BPlusTree.put(3, "c", t);

        let f = k -> v -> "${k}=>${v}";

        Assert.assertEq(expected = "1=>a, 2=>b, 3=>c", BPlusTree.joinWith(f, ", ", t))
    }

    @Test
    pub def joinWith03(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(7, 9, t);

        let f = k -> v -> "${k * v}";

        Assert.assertEq(expected = "63", BPlusTree.joinWith(f, "; ", t))
    }

    @Test
    def joinWith04(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 5);
        foreach (i <- Range.Range(0, 100)) {
            BPlusTree.put(i, "${i}", t)
        };

        let f = k -> v -> "${k}=${v}";

        let keys = List.range(0, 100);
        let expected = List.join(", ", List.map(i -> "${i}=${i}", keys));

        Assert.assertEq(expected = expected, BPlusTree.joinWith(f, ", ", t))
    }


    /////////////////////////////////////////////////////////////////////////////
    // minimumKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKey01(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        assertEq(expected = Option.None, BPlusTree.minimumKey(tree))
    }

    @Test
    def minimumKey02(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 3, tree);
        assertEq(expected = Option.Some((1, 3)), BPlusTree.minimumKey(tree))
    }

    @Test
    def minimumKey03(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 3, tree);
        BPlusTree.put(2, 4, tree);
        BPlusTree.put(1, 9, tree);
        BPlusTree.put(3, -1, tree);
        BPlusTree.put(4, 6, tree);
        assertEq(expected = Option.Some((1, 9)), BPlusTree.minimumKey(tree))
    }

    @Test
    def minimumKey04(): Unit \ Assert = region rc {
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 3, tree);
        BPlusTree.put(2, -12, tree);
        BPlusTree.put(0, -5, tree);
        BPlusTree.put(-1, -4, tree);
        BPlusTree.put(4, -3, tree);
        BPlusTree.put(5, -6, tree);
        assertEq(expected = Option.Some((-1, -4)), BPlusTree.minimumKey(tree))
    }

    /////////////////////////////////////////////////////////////////////////////
    // putIf                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def putIf01(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(1, 2, tree);
        BPlusTree.putIf(_ -> _ -> _ -> _ -> true, 3, 4, tree);
        assertTrue(BPlusTree.memberOfPair(1, 2, tree));
        assertTrue(BPlusTree.memberOfPair(3, 4, tree));
        assertFalse(BPlusTree.memberOfPair(1, 4, tree))
    }

    @Test
    def putIf02(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(1, 2, tree);
        BPlusTree.putIf(k1 -> _ -> k2 -> _ -> k1 == k2, 1, 4, tree);
        assertTrue(BPlusTree.memberOfPair(1, 4, tree))
    }

    @Test
    def putIf03(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(1, 2, tree);
        BPlusTree.putIf(_ -> v1 -> _ -> _ -> v1 == 4, 1, 4, tree);
        assertTrue(BPlusTree.memberOfPair(1, 4, tree))
    }

    @Test
    def putIf04(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(1, 2, tree);
        BPlusTree.putIf(_ -> _ -> _ -> v2 -> v2 == 4, 1, 4, tree);
        assertTrue(BPlusTree.memberOfPair(1, 2, tree))
    }

    enum PutIfEnum {
        case PutIfEnum(Int32, Int32)
    }

    instance Eq[PutIfEnum] {
        ///
        /// Returns `true` if and only if `x` is equal to `y`.
        ///
        pub def eq(x: PutIfEnum, y: PutIfEnum): Bool = match (x, y) {
            case (PutIfEnum.PutIfEnum(x1, _), PutIfEnum.PutIfEnum(x2, _)) => x1 == x2
        }
    }

    instance Order[PutIfEnum] {
        ///
        /// Returns `true` if and only if `x` is equal to `y`.
        ///
        pub def compare(x: PutIfEnum, y: PutIfEnum): Comparison = match (x, y) {
            case (PutIfEnum.PutIfEnum(x1, _), PutIfEnum.PutIfEnum(x2, _)) => x1 <=> x2
        }
    }

    @Test
    def putIf05(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        let key1 = PutIfEnum.PutIfEnum(1, 2);
        let key2 = PutIfEnum.PutIfEnum(1, 3);
        BPlusTree.put(key1, 2, tree);
        BPlusTree.putIf(k1 -> _ -> _ -> _ -> match k1 {
            case PutIfEnum.PutIfEnum(_, y) => y == 3
        }, key2, 4, tree);
        assertTrue(BPlusTree.memberOfPair(key1, 4, tree))
    }

    @Test
    def putIf06(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        let key1 = PutIfEnum.PutIfEnum(1, 2);
        let key2 = PutIfEnum.PutIfEnum(1, 3);
        BPlusTree.put(key1, 2, tree);
        BPlusTree.putIf(_ -> _ -> k2 -> _ -> match k2 {
            case PutIfEnum.PutIfEnum(_, y) => y == 3
        }, key2, 4, tree);
        assertTrue(BPlusTree.memberOfPair(key1, 2, tree))
    }

    def putIfPreferSome(_: PutIfEnum, l: Option[Int32], _: PutIfEnum, _: Option[Int32]): Bool = match l {
        case None => false
        case Some(_) => true
    }

    @Test
    pub def putIf07(): Unit \ Assert + IO = region rc {
        let tree = BPlusTree.empty(rc);
        let _: Unit = region rc2 {
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(i ->
                        BPlusTree.putIf(putIfPreferSome, PutIfEnum.PutIfEnum(i, 1), None, tree)
                    ): _ \ rc) as _ \ IO) @ rc2);
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(i ->
                        BPlusTree.putIf(putIfPreferSome, PutIfEnum.PutIfEnum(i, -1), Some(1), tree)
            ): _ \ rc) as _ \ IO) @ rc2)
        };
        assertTrue(List.range(0, totalInsertNumInt32()) |> List.forAll(i -> Some((PutIfEnum.PutIfEnum(i, -1), Some(1))) == BPlusTree.getKeyAndValue(PutIfEnum.PutIfEnum(i, 1), tree)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // putWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def putWith01(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(1, 2, tree);
        BPlusTree.putWith(v1 -> v2 -> v1 + v2, 1, 3, tree);
        assertTrue(BPlusTree.memberOfPair(1, 5, tree));
        assertFalse(BPlusTree.memberOfPair(1, 2, tree));
        assertFalse(BPlusTree.memberOfPair(1, 3, tree))
    }

    @Test
    def putWith02(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.put(1, 2, tree);
        BPlusTree.putWith(v1 -> v2 -> v1 - v2, 1, 1, tree);
        assertTrue(BPlusTree.memberOfPair(1, -1, tree));
        assertFalse(BPlusTree.memberOfPair(1, 2, tree));
        assertFalse(BPlusTree.memberOfPair(1, 1, tree))
    }

    @Test
    def putWith03(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        BPlusTree.putWith(v1 -> v2 -> v1 - v2, 1, 2, tree);
        assertTrue(BPlusTree.memberOfPair(1, 2, tree))
    }

    @Test
    pub def putWith04(): Unit \ Assert + IO = region rc {
        let tree = BPlusTree.empty(rc);
        let _: Unit = region rc2 {
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(i ->
                        BPlusTree.putWith(preferSome, i, None, tree)
                    ): _ \ rc) as _ \ IO) @ rc2);
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(i ->
                        BPlusTree.putWith(preferSome, i, Some(1), tree)
            ): _ \ rc) as _ \ IO) @ rc2)
        };
        assertTrue(List.range(0, totalInsertNumInt32()) |> List.forAll(i -> BPlusTree.memberOfPair(i, Some(1), tree)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        assertEq(expected = 0, BPlusTree.size(tree))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        assertEq(expected = 1, BPlusTree.size(tree))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        BPlusTree.put(0, 0, tree);
        assertEq(expected = 1, BPlusTree.size(tree))
    }

    @Test
    def size04(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        BPlusTree.put(0, 1, tree);
        assertEq(expected = 1, BPlusTree.size(tree))
    }

    @Test
    def size05(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        BPlusTree.put(0, 1, tree);
        BPlusTree.put(1, 1, tree);
        assertEq(expected = 2, BPlusTree.size(tree))
    }

    @Test
    def size06(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        discard BPlusTree.computeIfAbsent(() -> 3, 0, tree);
        assertEq(expected = 1, BPlusTree.size(tree))
    }

    @Test
    def size07(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        discard BPlusTree.computeIfAbsent(() -> 3, 1, tree);
        assertEq(expected = 2, BPlusTree.size(tree))
    }

    @Test
    def size09(): Unit \ Assert = region rc {
        let tree1: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree1);
        BPlusTree.put(1, 1, tree1);
        BPlusTree.put(2, 2, tree1);
        BPlusTree.put(3, 3, tree1);
        BPlusTree.put(4, 4, tree1);
        BPlusTree.put(5, 5, tree1);
        let tree2: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(4, 4, tree2);
        BPlusTree.put(5, 5, tree2);
        BPlusTree.put(6, 6, tree2);
        BPlusTree.put(7, 7, tree2);
        BPlusTree.put(8, 8, tree2);
        BPlusTree.put(9, 9, tree2);
        BPlusTree.merge(tree2, tree1);
        assertEq(expected = 10, BPlusTree.size(tree1))
    }

    @Test
    def size10(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        BPlusTree.putWith(_ -> _ -> 0, 0, 0, tree);
        assertEq(expected = 1, BPlusTree.size(tree))
    }

    @Test
    def size11(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 0, tree);
        BPlusTree.putWith(_ -> _ -> 0, 1, 0, tree);
        assertEq(expected = 2, BPlusTree.size(tree))
    }

    @Test
    def size12(): Unit \ Assert + IO + NonDet = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.empty(rc);
        let _: Unit = region rc2 {
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(_ ->
                        Random.runWithIO(() -> BPlusTree.put(Random.randomInt32(), Random.randomInt32(), tree))
            ): _ \ rc + NonDet + IO) as _ \ IO + NonDet) @ rc2);
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(_ ->
                        Random.runWithIO(() -> BPlusTree.put(Random.randomInt32(), Random.randomInt32(), tree))
            ): _ \ rc + NonDet + IO) as _ \ IO + NonDet) @ rc2);
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(_ ->
                        Random.runWithIO(() -> BPlusTree.put(Random.randomInt32(), Random.randomInt32(), tree))
            ): _ \ rc + NonDet + IO) as _ \ IO + NonDet) @ rc2);
            (spawn unchecked_cast((
                List.range(0, totalInsertNumInt32()) |>
                    List.forEach(_ ->
                        Random.runWithIO(() -> BPlusTree.put(Random.randomInt32(), Random.randomInt32(), tree))
            ): _ \ rc + NonDet + IO) as _ \ IO + NonDet) @ rc2)
        };
        assertEq(expected = BPlusTree.size(tree), BPlusTree.toList(tree) |> List.size)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        let tree: BPlusTree[Int32, Int32, rc] = BPlusTree.empty(rc);
        assertEq(expected = List.empty(), BPlusTree.toList(tree))
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        let pairs = (1, 12) :: (2, 23) :: (3, 34) :: (4, 45) :: Nil;
        List.forEach(match (k, v) -> BPlusTree.put(k, v, tree), pairs);
        assertEq(expected = pairs, BPlusTree.toList(tree))
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        let toBeOverridden = (1, 2) :: (2, 4) :: Nil;
        let finalValues = (1, 3) :: (2, 5) :: Nil;
        List.forEach(match (k, v) -> BPlusTree.put(k, v, tree), toBeOverridden);
        List.forEach(match (k, v) -> BPlusTree.put(k, v, tree), finalValues);
        assertEq(expected = finalValues, BPlusTree.toList(tree))
    }

    @Test
    def toList04(): Unit \ Assert = region rc {
        let tree = BPlusTree.empty(rc);
        let pairs = (1, 2) :: (2, 4) :: (1, 3) :: (2, 5) :: Nil;
        List.forEach(match (k, v) -> BPlusTree.put(k, v, tree), pairs);
        let map = List.foldLeft(acc -> match (key, val) -> Map.insert(key, val, acc), Map#{}, pairs);
        let finalPairs = Map.toList(map);
        assertEq(expected = List.sort(finalPairs), BPlusTree.toList(tree))
    }

    @Test
    def toList05(): Unit \ Assert + NonDet + IO = region rc {
        let tree = BPlusTree.empty(rc);
        let getRandomPairs = () -> {
            List.range(0, 10_000) |> List.map(_ -> (Random.randomInt32(), Random.randomInt32()))
        };
        let pairs = Random.handle(getRandomPairs)();
        List.forEach(match (k, v) -> BPlusTree.put(k, v, tree), pairs);
        let map = List.foldLeft(acc -> match (key, val) -> Map.insert(key, val, acc), Map#{}, pairs);
        let finalPairs = Map.toList(map);
        assertEq(expected = finalPairs, BPlusTree.toList(tree))
    }

    /////////////////////////////////////////////////////////////////////////////
    // rangeQueryWith                                                          //
    /////////////////////////////////////////////////////////////////////////////

    def rangeQueryDefaultTree(rc: Region[r]): BPlusTree[Int32, Int32, r] \ r =
        let tree: BPlusTree[Int32, Int32, r] = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 10, tree);
        BPlusTree.put(1, 11, tree);
        BPlusTree.put(2, 12, tree);
        BPlusTree.put(3, 13, tree);
        BPlusTree.put(4, 14, tree);
        BPlusTree.put(5, 15, tree);
        tree

    def rangeQueryMultipleValues(rc: Region[r]): BPlusTree[Int32, Int32, r] \ r =
        let tree = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(0, 10, tree);
        BPlusTree.put(0, 11, tree);
        BPlusTree.put(0, 12, tree);
        BPlusTree.put(1, 11, tree);
        BPlusTree.put(1, 12, tree);
        BPlusTree.put(2, 13, tree);
        BPlusTree.put(3, 14, tree);
        tree

    @Test
    def rangeQueryWith01(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 0, 5, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def rangeQueryWith02(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, 5, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def rangeQueryWith03(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, 7, tree);
        let res = Vector#{0, 1, 2, 3, 4, 5};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def rangeQueryWith04(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 1, 1, tree);
        let res = Vector#{1};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def rangeQueryWith05(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 1, 3, tree);
        let res = Vector#{1, 2, 3};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def rangeQueryWith06(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, -2, -2, tree);
        let res = Vector#{ };
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def rangeQueryWith07(): Unit \ Assert = region rc {
        let tree = rangeQueryDefaultTree(rc);
        let list = MutList.empty(rc);
        let f = k -> _ -> MutList.push(k, list);
        BPlusTree.rangeQueryWith(f, 7, 7, tree);
        let res = Vector#{ };
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    @Test
    def rangeQueryWith08(): Unit \ Assert = region rc {
        let tree = rangeQueryMultipleValues(rc);
        let list = MutList.empty(rc);
        let f = k -> v -> MutList.push((k, v), list);
        BPlusTree.rangeQueryWith(f, 0, 3, tree);
        let res = Vector#{(0, 12), (1, 12), (2, 13), (3, 14)};
        assertTrue(MutList.toVector(list) `Vector.equals` res)
    }

    /////////////////////////////////////////////////////////////////////////////
    // find / findLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertNone(BPlusTree.find(_ -> _ -> false, t))
    }

    @Test
    def find02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertNone(BPlusTree.find(_ -> _ -> true, t))
    }

    @Test
    def find03(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(10, 20);
        Assert.assertEq(expected = Some((10, 20)), BPlusTree.find(k -> _ -> k == 10, t))
    }

    @Test
    def find04(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(10, 20);
        Assert.assertNone(BPlusTree.find(k -> _ -> k == 0, t))
    }

    @Test
    def find05(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, k + 1, t);
        Assert.assertEq(expected = Some((50, 51)), BPlusTree.find(k -> _ -> k == 50, t))
    }

    @Test
    def find06(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, k + 1, t);
        Assert.assertNone(BPlusTree.find(k -> v -> k > v, t))
    }

    @Test
    def find07(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, k + 1, t);
        Assert.assertEq(expected = Some((0, 1)), BPlusTree.find(k -> v -> k < v, t))
    }


    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertFalse(BPlusTree.nonEmpty(t))
    }

    @Test
    def nonEmpty02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(1, 10);
        Assert.assertTrue(BPlusTree.nonEmpty(t))
    }

    @Test
    def nonEmpty03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, (), t);
        Assert.assertTrue(BPlusTree.nonEmpty(t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertTrue(BPlusTree.forAll(_ -> _ -> false, t))
    }

    @Test
    def forAll02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertTrue(BPlusTree.forAll(_ -> _ -> true, t))
    }

    @Test
    def forAll03(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(4, 2);
        Assert.assertTrue(BPlusTree.forAll(k -> v -> k > v, t))
    }

    @Test
    def forAll04(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, (), t);
        Assert.assertTrue(BPlusTree.forAll(k -> _ -> k < 100, t))
    }

    @Test
    def forAll05(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, (), t);
        BPlusTree.put(105, (), t);
        Assert.assertFalse(BPlusTree.forAll(k -> _ -> k < 105, t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertFalse(BPlusTree.exists(_ -> _ -> true, t))
    }

    @Test
    def exists02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertFalse(BPlusTree.exists(_ -> _ -> false, t))
    }


    @Test
    def exists03(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(10, 5);
        Assert.assertFalse(BPlusTree.exists(_ -> _ -> false, t))
    }

    @Test
    def exists04(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(10, 5);
        Assert.assertTrue(BPlusTree.exists(k -> v -> k + v == 15, t))
    }

    @Test
    def exists05(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(10, 5);
        Assert.assertFalse(BPlusTree.exists(k -> v -> k + v == 0, t))
    }

    @Test
    def exists06(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc) !> BPlusTree.put(10, 5) !> BPlusTree.put(20, 5);
        Assert.assertTrue(BPlusTree.exists(_ -> v -> v == 5, t))
    }

    @Test
    def exists07(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, (), t);
        Assert.assertTrue(BPlusTree.exists(k -> _ -> k == 99, t))
    }

    @Test
    def exists08(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (k <- Range.Range(0, 100))
            BPlusTree.put(k, (), t);
        let f = k -> _ -> k == 99;
        Assert.assertEq(expected = not BPlusTree.forAll(k -> v -> not f(k, v), t), BPlusTree.exists(f, t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def iterator01(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(2, 20, t);
        BPlusTree.put(1, 10, t);
        BPlusTree.put(3, 30, t);
        let it = BPlusTree.iterator(rc, t);

        Assert.assertEq(expected = Vector#{(1,10), (2,20), (3,30)}, Iterator.toVector(it))
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let t: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        let it = BPlusTree.iterator(rc, t);
        Assert.assertEq(expected = Vector.empty(), Iterator.toVector(it))
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (i <- Range.Range(1, 11)) {
            BPlusTree.put(i, i * 2, t)
        };

        let it = BPlusTree.iterator(rc, t);

        Assert.assertEq(
            expected = Vector#{(1, 2), (2, 4), (3, 6), (4, 8), (5, 10), (6, 12), (7, 14), (8, 16), (9, 18), (10, 20)},
            Iterator.toVector(it)
        )
    }

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 10, t);
        BPlusTree.put(2, 20, t);

        let t2 = BPlusTree.map(rc, v -> v + 1, t);
        Assert.assertEq(expected = Map#{1 => 11, 2 => 21}, BPlusTree.toMap(t2))
    }

    @Test
    def map02(): Unit \ Assert = region rc {
        let t: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        let t2 = BPlusTree.map(rc, v -> v * 10, t);
        Assert.assertTrue(BPlusTree.isEmpty(t2))
    }

    @Test
    def map03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(10, 999, t);
        BPlusTree.put(3, 501, t);
        BPlusTree.put(7, 1234, t);
        BPlusTree.put(1, 222, t);
        BPlusTree.put(6, 777, t);
        BPlusTree.put(2, 888, t);
        BPlusTree.put(9, 42, t);
        BPlusTree.put(4, 1001, t);

        let t2 = BPlusTree.map(rc, v -> Int32.modulo(v, 2), t);

        let expected = Map#{
            1 => Int32.modulo(222, 2),
            2 => Int32.modulo(888, 2),
            3 => Int32.modulo(501, 2),
            4 => Int32.modulo(1001, 2),
            6 => Int32.modulo(777, 2),
            7 => Int32.modulo(1234, 2),
            9 => Int32.modulo(42, 2),
            10 => Int32.modulo(999, 2)
        };

        Assert.assertEq(expected = expected, BPlusTree.toMap(t2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(1, 2, t);
        BPlusTree.put(3, 4, t);

        let t2 = BPlusTree.mapWithKey(rc, k -> v -> k + v, t);
        Assert.assertEq(expected = Map#{1 => 3, 3 => 7}, BPlusTree.toMap(t2))
    }

    @Test
    def mapWithKey02(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        let t2 = BPlusTree.mapWithKey(rc, k -> v -> k * 100 + v, t);
        Assert.assertTrue(BPlusTree.isEmpty(t2))
    }

    @Test
    def mapWithKey03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 4);
        foreach (i <- Range.Range(1, 16))
            BPlusTree.put(i, (), t);
        
        def fizzBuzz(n, _) = {
            let divBy3 = Int32.modulo(n, 3) == 0;
            let divBy5 = Int32.modulo(n, 5) == 0;
            if (divBy3 and divBy5) "FizzBuzz"
            else if (divBy3) "Fizz"
            else if (divBy5) "Buzz"
            else Int32.toString(n)
        };

        let t2 = BPlusTree.mapWithKey(rc, fizzBuzz, t);
        let expected = Map#{
            1 => "1", 2 => "2", 3 => "Fizz", 4 => "4", 5 => "Buzz", 6 => "Fizz", 7 => "7", 8 => "8",
            9 => "Fizz", 10 => "Buzz", 11 => "11", 12 => "Fizz", 13 => "13", 14 => "14", 15 => "FizzBuzz"
        };
        Assert.assertEq(expected = expected, BPlusTree.toMap(t2))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        BPlusTree.put(2, "hej", t);
        BPlusTree.put(1, "med", t);

        Assert.assertEq(expected = Map#{1 => "med", 2 => "hej"}, BPlusTree.toMap(t))
    }

    @Test
    def toMap02(): Unit \ Assert = region rc {
        let t: BPlusTree[Int32, Int32, rc] = BPlusTree.emptyWithArity(rc, 3);
        Assert.assertEq(expected = Map.empty(), BPlusTree.toMap(t))
    }

    @Test
    def toMap03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (i <- Range.Range(1, 7))
            BPlusTree.put(i, i*10, t);
            
        Assert.assertEq(
            expected = Map#{1 => 10, 2 => 20, 3 => 30, 4 => 40, 5 => 50, 6 => 60 }, 
            BPlusTree.toMap(t)
        )
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(1, "x",t);
        BPlusTree.put(2, "y",t);
        Assert.assertEq(expected = Set#{(1, "x"), (2, "y")}, BPlusTree.toSet(t))
    }

    @Test
    def toSet02(): Unit \ Assert = region rc {
        let t: BPlusTree[Int32, Int32, rc] = BPlusTree.empty(rc);
        Assert.assertEq(expected = Set.empty(), BPlusTree.toSet(t))
    }

    @Test
    def toSet03(): Unit \ Assert = region rc {
        let t = BPlusTree.emptyWithArity(rc, 3);
        foreach (i <- Range.Range(1, 5)) {
            BPlusTree.put(i, i*2, t)
        };
        Assert.assertEq(expected = Set#{(1,2),(2,4),(3,6),(4,8)}, BPlusTree.toSet(t))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        BPlusTree.put(3, 30, t);
        BPlusTree.put(1, 10, t);
        BPlusTree.put(2, 20, t);

        Assert.assertEq(expected = Vector#{(1,10), (2,20), (3,30)}, BPlusTree.toVector(t))
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let t = BPlusTree.empty(rc);
        Assert.assertTrue(Vector.isEmpty(BPlusTree.toVector(t)))
    }

}
