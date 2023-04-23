mod Test.Exp.Regions {

    // Uses references and regions (e.g. could just duplicate most test cases for both).
    // Test cases that use 0, 1, 2, and 3 nested regions.
    // Test cases that use 0, 1, 2, 3 data structures nested inside each other (e.g. array inside array inside array).
    // Test cases that mix both multiple regions and multiple data structures.

    @test
    def testRegions1Region01(): Bool =
        region r {
            let a = Array.repeat(r, 8, 1);
            Array.get(0, a) == 1
        }

    @test
    def testRegions1Region02(): Bool =
        region r {
            let l = MutList.new(r);
            MutList.push!(1, l);
            MutList.pop!(l) == Some(1)
        }

    @test
    def testRegions1Region03(): Bool =
        region r {
            let m = MutMap.new(r);
            MutMap.put!(1, 1, m);
            MutMap.get(1, m) == Some(1)
        }

    @test
    def testRegions1Region1Nested01(): Bool =
        region r {
            let a = Array.repeat(r, 8, MutMap.new(r));
            MutMap.put!(1, 1, Array.get(0, a));
            MutMap.get(1, Array.get(0, a)) == Some(1)
        }

    @test
    def testRegions1Region1Nested02(): Bool =
        region r {
            let l = MutList.new(r);
            MutList.push!(Array.repeat(r, 8, 1), l);
            match MutList.pop!(l) {
                case Some(a) => a `Array.sameElements` Array.repeat(r, 8, 1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region1Nested03(): Bool =
        region r {
            let m = MutMap.new(r);
            let l = MutList.new(r);
            MutList.push!(1, l);
            MutMap.put!(1, l, m);
            match MutMap.get(1, m) {
                case Some(x) => x `MutList.sameElements` l
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region1Nested04(): Bool =
        region r {
            let a = Array.repeat(r, 8, MutMap.new(r));
            MutMap.put!(1, 1, Array.get(0, a));
            MutMap.get(1, Array.get(0, a)) == Some(1)
        }

    @test
    def testRegions1Region1Nested05(): Bool =
        region r {
            let l = MutList.new(r);
            MutList.push!(Array.repeat(r, 8, 1), l);
            match MutList.pop!(l) {
                case Some(a) => a `Array.sameElements` Array.repeat(r, 8, 1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region1Nested06(): Bool =
        region r {
            let m = MutMap.new(r);
            let l = MutList.new(r);
            MutList.push!(1, l);
            MutMap.put!(1, l, m);
            match MutMap.get(1, m) {
                case Some(x) => x `MutList.sameElements` l
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2Nested01(): Bool =
        region r {
            let a = Array.repeat(r, 8, MutMap.new(r));
            let l = MutList.new(r);
            MutList.push!(1, l);
            MutMap.put!(1, l, Array.get(0, a));
            match MutMap.get(1, Array.get(0, a)) {
                case Some(k) => MutList.pop!(k) == Some(1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2Nested02(): Bool =
        region r {
            let l = MutList.new(r);
            let a = Array.repeat(r, 8, MutMap.new(r));
            MutMap.put!(1, 1, Array.get(0, a));
            MutList.push!(a, l);
            match MutList.pop!(l) {
                case Some(b) => MutMap.get(1, Array.get(0, b)) == Some(1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2Nested03(): Bool =
        region r {
            let m = MutMap.new(r);
            let l = MutList.new(r);
            MutList.push!(Array.repeat(r, 8, 1), l);
            MutMap.put!(1, l, m);
            match MutMap.get(1, m) {
                case Some(k) => match MutList.pop!(k) {
                    case Some(a) => Array.get(0, a) == 1
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3Nested01(): Bool =
        region r {
            let a = Array.repeat(r, 8, MutMap.new(r));
            let l = MutList.new(r);
            MutList.push!(Array.repeat(r, 8, 1), l);
            MutMap.put!(1, l, Array.get(0, a));
            match MutMap.get(1, Array.get(0, a)) {
                case Some(k) => match MutList.pop!(k) {
                    case Some(b) => Array.get(0, b) == 1
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3Nested02(): Bool =
        region r {
            let l = MutList.new(r);
            let a = Array.repeat(r, 8, MutMap.new(r));
            let l1 = MutList.new(r);
            MutList.push!(1, l1);
            MutMap.put!(1, l1, Array.get(0, a));
            MutList.push!(a, l);
            match MutList.pop!(l) {
                case Some(b) => match MutMap.get(1, Array.get(0, b)) {
                    case Some(k) => MutList.pop!(k) == Some(1)
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3Nested03(): Bool =
        region r {
            let m = MutMap.new(r);
            let l = MutList.new(r);
            let a = Array.repeat(r, 8, MutMap.new(r));
            MutMap.put!(1, 1, Array.get(0, a));
            MutList.push!(a, l);
            MutMap.put!(1, l, m);
            match MutMap.get(1, m) {
                case Some(k) => match MutList.pop!(k) {
                    case Some(b) => MutMap.get(1, Array.get(0, b)) == Some(1)
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions2Regions01(): Bool =
        region r1 {
            let a1 = Array.repeat(r1, 8, 1);
            Array.get(0, a1) == 1 and 1 ==
            region r2 {
                let a2 = Array.repeat(r2, 8, 1);
                Array.get(0, a2)
            }
        }

    @test
    def testRegions2Regions02(): Bool =
        region r1 {
            let l1 = MutList.new(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = MutList.new(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions03(): Bool =
        region r1 {
            let m1 = MutMap.new(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = MutMap.new(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions04(): Bool =
        region r1 {
            let a1 = Array.repeat(r1, 8, 1);
            Array.get(0, a1) == 1 and 1 ==
            region r2 {
                let a2 = Array.repeat(r2, 8, 1);
                Array.get(0, a2)
            }
        }

    @test
    def testRegions2Regions05(): Bool =
        region r1 {
            let l1 = MutList.new(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = MutList.new(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions06(): Bool =
        region r1 {
            let m1 = MutMap.new(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = MutMap.new(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions07(): Bool =
        region r1 {
            let a1 = Array.repeat(r1, 8, 1);
            Array.get(0, a1) == 1 and 1 ==
            region r2 {
                let a2 = Array.repeat(r2, 8, 1);
                Array.get(0, a2)
            }
        }

    @test
    def testRegions2Regions08(): Bool =
        region r1 {
            let l1 = MutList.new(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = MutList.new(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions09(): Bool =
        region r1 {
            let m1 = MutMap.new(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = MutMap.new(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions10(): Bool =
        region r1 {
            let a1 = Array.repeat(r1, 8, 1);
            Array.get(0, a1) == 1 and 1 ==
            region r2 {
                let a2 = Array.repeat(r2, 8, 1);
                Array.get(0, a2)
            }
        }

    @test
    def testRegions2Regions11(): Bool =
        region r1 {
            let l1 = MutList.new(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = MutList.new(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions12(): Bool =
        region r1 {
            let m1 = MutMap.new(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = MutMap.new(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions13(): Bool =
        region r1 {
            let a1 = Array.repeat(r1, 8, 1);
            region r2 {
                let a2 = Array.repeat(r2, 8, 1);
                Array.get(0, a1) == 1 and 1 == Array.get(0, a2)
            }
        }

    @test
    def testRegions2Regions14(): Bool =
        region r1 {
            let l1 = MutList.new(r1);

            region r2 {
                let l2 = MutList.new(r2);
                MutList.push!(1, l1);
                MutList.push!(1, l2);
                MutList.pop!(l1) == Some(1) and Some(1) == MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions15(): Bool =
        region r1 {
            let m1 = MutMap.new(r1);
            region r2 {
                let m2 = MutMap.new(r2);
                MutMap.put!(1, 1, m1);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m1) == Some(1) and Some(1) == MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions1Nested01(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutMap[Int32, Int32, r2], r1] = Array.repeat(r1, 8, MutMap.new(r2));
                MutMap.put!(1, 1, Array.get(0, a));
                MutMap.get(1, Array.get(0, a)) == Some(1)
            }
        }

    @test
    def testRegions2Regions1Nested02(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[Int32, r2], r1] = MutList.new(r1);
                MutList.push!(Array.repeat(r2, 8, 1), l);
                match MutList.pop!(l) {
                    case Some(a) => Array.get(0, a) == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions1Nested03(): Bool =
        region r1 {
            region r2 {
                let m: MutMap[Int32, MutList[Int32, r2], r1] = MutMap.new(r1);
                let l = MutList.new(r2);
                MutList.push!(1, l);
                MutMap.put!(1, l, m);
                match MutMap.get(1, m) {
                    case Some(k) => MutList.pop!(k) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions1Nested04(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutMap[Int32, Int32, r1], r2] = Array.repeat(r2, 8, MutMap.new(r1));
                MutMap.put!(1, 1, Array.get(0, a));
                MutMap.get(1, Array.get(0, a)) == Some(1)
            }
        }

    @test
    def testRegions2Regions1Nested05(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[Int32, r1], r2] = MutList.new(r2);
                MutList.push!(Array.repeat(r1, 8, 1), l);
                match MutList.pop!(l) {
                    case Some(a) => Array.get(0, a) == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions1Nested06(): Bool =
        region r1 {
            let l = MutList.new(r1);
            region r2 {
                let m: MutMap[Int32, MutList[Int32, r1], r2] = MutMap.new(r2);
                MutList.push!(1, l);
                MutMap.put!(1, l, m);
                match MutMap.get(1, m) {
                    case Some(k) => MutList.nth(0, k) == Some(1)
                    case _       => unreachable!()
                }
            } and MutList.nth(0, l) == Some(1)
        }

    @test
    def testRegions2Regions2Nested01(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutMap[Int32, MutList[Int32, r1], r2], r2] = Array.repeat(r2, 8, MutMap.new(r2));
                let l = MutList.new(r1);
                MutList.push!(1, l);
                MutMap.put!(1, l, Array.get(0, a));
                match MutMap.get(1, Array.get(0, a)) {
                    case Some(k) => MutList.nth(0, k) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions2Nested02(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[MutMap[Int32, Int32, r2], r1], r2] = MutList.new(r2);
                let a = Array.repeat(r1, 8, MutMap.new(r2));
                MutList.push!(a, l);
                MutMap.put!(1, 1, Array.get(0, a)); // Mutate a[0], the MutList `l` above should also be updated by this
                match MutList.pop!(l) {
                    case Some(b) => MutMap.get(1, Array.get(0, b)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions2Nested03(): Bool =
        region r1 {
            region r2 {
                let m: MutMap[Int32, MutList[Array[Int32, r2], r2], r1] = MutMap.new(r1);
                let l = MutList.new(r2);
                MutList.push!(Array.repeat(r2, 8, 1), l);
                MutMap.put!(1, l, m);
                match MutMap.get(1, m) {
                    case Some(k) => MutList.nth(0, k) |> Option.flatMap(Array.nth(0)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3Nested01(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutMap[Int32, MutList[Array[Int32, r1], r1], r2], r2] = Array.repeat(r2, 8, MutMap.new(r2));
                let l = MutList.new(r1);
                MutList.push!(Array.repeat(r1, 8, 1), l);
                MutMap.put!(1, l, Array.get(0, a));
                match MutMap.get(1, Array.get(0, a)) {
                    case Some(k) => MutList.nth(0, k) |> Option.flatMap(Array.nth(0)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3Nested02(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[MutMap[Int32, MutList[Int32, r2], r2], r1], r2] = MutList.new(r2);
                let a = Array.repeat(r1, 8, MutMap.new(r2));
                MutList.push!(a, l);
                let l2 = MutList.new(r2);
                MutMap.put!(1, l2, Array.get(0, a)); // Mutate a[0], the MutList `l` above should also be updated by this
                MutList.push!(1, l2);
                match MutList.pop!(l) {
                    case Some(b) => match MutMap.get(1, Array.get(0, b)) {
                        case Some(k) => MutList.nth(0, k) == Some(1)
                        case _       => unreachable!()
                    }
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3Nested03(): Bool =
        region r1 {
            region r2 {
                let m: MutMap[Int32, MutList[Array[MutMap[Int32, Int32, r2], r2], r1], r1] = MutMap.new(r1);
                let l = MutList.new(r1);
                let a = Array.repeat(r2, 8, MutMap.new(r2));
                MutList.push!(a, l);
                MutMap.put!(1, 1, Array.get(0, a)); // Mutate the innermost nested MutMap
                MutMap.put!(1, l, m);
                match MutMap.get(1, m) {
                    case Some(k) => MutList.nth(0, k) |> Option.flatMap(Array.nth(0))
                                                      |> Option.flatMap(MutMap.get(1)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions3Regions01(): Bool =
        region r1 {
            let a = Array.repeat(r1, 8, 1);
            region r2 {
                let l = MutList.new(r2);
                region _ {
                    let m = MutMap.new(r1);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    Array.get(0, a) == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions02(): Bool =
        region r1 {
            let a = Array.repeat(r1, 8, 1);
            region r2 {
                let l = MutList.new(r2);
                region r3 {
                    let m = MutMap.new(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    Array.get(0, a) == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions03(): Bool =
        region r1 {
            let a = Array.repeat(r1, 8, 1);
            region _ {
                let l = MutList.new(r1);
                region r3 {
                    let m = MutMap.new(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    Array.get(0, a) == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions04(): Bool =
        region r1 {
            let a = Array.repeat(r1, 8, 1);
            region r2 {
                let l = MutList.new(r2);
                region r3 {
                    let m = MutMap.new(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    Array.get(0, a) == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions1Nested01(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutList[Int32, r1], r2] = Array.repeat(r2, 8, MutList.new(r1));
                region _ {
                    MutList.push!(1, Array.get(0, a));
                    MutList.pop!(Array.get(0, a)) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions1Nested02(): Bool =
        region r1 {
            region r2 {
                let l: MutList[MutMap[Int32, Int32, r1], r2] = MutList.new(r2);
                region _ {
                    let m = MutMap.new(r1);
                    MutList.push!(m, l);
                    MutMap.put!(1, 1, m);
                    match MutList.pop!(l) {
                        case Some(n) => MutMap.get(1, n) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions1Nested03(): Bool =
        region _ {
            region r2 {
                region r3 {
                    let m: MutMap[Int32, MutList[Int32, r3], r2] = MutMap.new(r2);
                    let l = MutList.new(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, l, m);
                    match MutMap.get(1, m) {
                        case Some(k) => MutList.pop!(k) == Some(1)
                        case _       => unreachable!()
                    } and MutList.pop!(l) == None
                }
            }
        }

    @test
    def testRegions3Regions2Nested01(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let a: Array[MutMap[Int32, MutList[Int32, r3], r2], r1] = Array.new(r1, 8);
                    let m = MutMap.new(r2);
                    let l = MutList.new(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, l, m);
                    Array.put(m, 0, a);
                    match MutMap.get(1, Array.get(0, a)) {
                        case Some(k) => MutList.pop!(k) == Some(1)
                        case _       => unreachable!()
                    } and MutList.pop!(l) == None
                }
            }
        }

    @test
    def testRegions3Regions2Nested02(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let l: MutList[Array[MutMap[Int32, Int32, r1], r2], r3] = MutList.new(r3);
                    let a = Array.repeat(r2, 8, MutMap.new(r1));
                    MutList.push!(a, l);
                    MutMap.put!(1, 1, Array.get(0, a));
                    match MutList.pop!(l) {
                        case Some(b) => MutMap.get(1, Array.get(0, b)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions2Nested03(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let m: MutMap[Int32, MutList[Array[Int32, r1], r3], r2] = MutMap.new(r2);
                    let l = MutList.new(r3);
                    MutMap.put!(1, l, m);
                    MutList.push!(Array.repeat(r1, 8, 1), l);
                    match MutMap.get(1, m) {
                        case Some(k) => MutList.nth(0, k) |> Option.flatMap(Array.nth(0)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions3Nested01(): Bool =
        region r1 {
            let l = MutList.new(r1);
            region r2 {
                region r3 {
                    let a: Array[MutMap[Int32, MutList[Array[Int32, r1], r1], r2], r3] = Array.repeat(r3, 8, MutMap.new(r2));
                    MutMap.put!(1, l, Array.get(0, a));
                    MutList.push!(Array.repeat(r1, 8, 1), l);
                    match MutMap.get(1, Array.get(0, a)) {
                        case Some(k) => MutList.nth(0, k) |> Option.flatMap(Array.nth(0)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions3Nested02(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let l: MutList[Array[MutMap[Int32, MutList[Int32, r2], r3], r1], r2] = MutList.new(r2);
                    let a = Array.repeat(r1, 8, MutMap.new(r3));
                    let l2 = MutList.new(r2);
                    MutList.push!(a, l);
                    MutMap.put!(1, l2, Array.get(0, a));
                    MutList.push!(1, l2);
                    match MutList.pop!(l) {
                        case Some(b) => match MutMap.get(1, Array.get(0, b)) {
                            case Some(k) => MutList.pop!(k) == Some(1)
                            case _       => unreachable!()
                        }
                        case _       => unreachable!()
                    } and MutList.pop!(l2) == None
                }
            }
        }

    @test
    def testRegions3Regions3Nested03(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let m: MutMap[Int32, MutList[Array[MutMap[Int32, Int32, r1], r3], r1], r2] = MutMap.new(r2);
                    let l = MutList.new(r1);
                    let a = Array.repeat(r3, 8, MutMap.new(r1));
                    MutMap.put!(1, l, m);
                    MutList.push!(a, l);
                    MutMap.put!(1, 1, Array.get(0, a));
                    match MutMap.get(1, m) {
                        case Some(k) => MutList.nth(0, k) |> Option.flatMap(Array.nth(0)) |> Option.flatMap(MutMap.get(1)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions1RegionReferences03(): Bool =
        region r {
            let a = Array.repeat(r, 8, ref 1 @ r);
            deref Array.get(0, a) == 1
        }

    @test
    def testRegions1RegionReferences04(): Bool =
        region r {
            let l = ref MutList.new(r) @ r;
            MutList.push!(1, deref l);
            MutList.pop!(deref l) == Some(1)
        }

    @test
    def testRegions1RegionReferences05(): Bool =
        region r {
            let m = ref MutMap.new(r) @ r;
            MutMap.put!(1, 1, deref m);
            MutMap.get(1, deref m) == Some(1)
        }

    @test
    def testRegions1Region1NestedReferences01(): Bool =
        region r {
            let a = ref Array.repeat(r, 8, MutMap.new(r)) @ r;
            MutMap.put!(1, 1, Array.get(0, deref a));
            MutMap.get(1, Array.get(0, deref a)) == Some(1)
        }

    @test
    def testRegions1Region1NestedReferences02(): Bool =
        region r {
            let l = ref MutList.new(r) @ r;
            MutList.push!(ref (Array.repeat(r, 8, 1)) @ r, deref l);
            match MutList.pop!(deref l) {
                case Some(a) => (deref a) `Array.sameElements` Array.repeat(r, 8, 1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region1NestedReferences03(): Bool =
        region r {
            let m = MutMap.new(r);
            let l = ref MutList.new(r) @ r;
            MutList.push!(1, deref l);
            MutMap.put!(1, l, m);
            match MutMap.get(1, m) {
                case Some(x) => deref x `MutList.sameElements` deref l
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region1NestedReferences04(): Bool =
        region r {
            let a = ref Array.repeat(r, 8, ref MutMap.new(r) @ r) @ r;
            MutMap.put!(1, 1, deref (Array.get(0, deref a)));
            MutMap.get(1, deref (Array.get(0, deref a))) == Some(1)
        }

    @test
    def testRegions1Region1NestedReferences05(): Bool =
        region r {
            let l = ref MutList.new(r) @ r;
            MutList.push!(Array.repeat(r, 8, 1), deref l);
            match MutList.pop!(deref l) {
                case Some(a) => a `Array.sameElements` Array.repeat(r, 8, 1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2NestedReferences01(): Bool =
        region r {
            let a = ref Array.repeat(r, 8, ref MutMap.new(r) @ r) @ r;
            let l = ref MutList.new(r) @ r;
            MutList.push!(1, deref l);
            MutMap.put!(1, l, deref Array.get(0, deref a));
            match MutMap.get(1, deref Array.get(0, deref a)) {
                case Some(k) => MutList.pop!(deref k) == Some(1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2NestedReferences02(): Bool =
        region r {
            let l = ref MutList.new(r) @ r;
            let a = ref Array.repeat(r, 8, ref MutMap.new(r) @ r) @ r;
            MutMap.put!(1, 1, deref Array.get(0, deref a));
            MutList.push!(a, deref l);
            match MutList.pop!(deref l) {
                case Some(b) => MutMap.get(1, deref Array.get(0, deref b)) == Some(1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2NestedReferences03(): Bool =
        region r {
            let m = ref MutMap.new(r) @ r;
            let l = ref MutList.new(r) @ r;
            MutList.push!(ref Array.repeat(r, 8, 1) @ r, deref l);
            MutMap.put!(1, l, deref m);
            match MutMap.get(1, deref m) {
                case Some(k) => match MutList.pop!(deref k) {
                    case Some(a) => Array.get(0, deref a) == 1
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3NestedReferences01(): Bool =
        region r {
            let a = ref Array.repeat(r, 8, ref MutMap.new(r) @ r) @ r;
            let l = ref MutList.new(r) @ r;
            MutList.push!(ref Array.repeat(r, 8, 1) @ r, deref l);
            MutMap.put!(1, l, deref Array.get(0, deref a));
            match MutMap.get(1, deref Array.get(0, deref a)) {
                case Some(k) => match MutList.pop!(deref k) {
                    case Some(b) => Array.get(0, deref b) == 1
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3NestedReferences02(): Bool =
        region r {
            let l = ref MutList.new(r) @ r;
            let a = ref Array.repeat(r, 8, ref MutMap.new(r) @ r) @ r;
            let l1 = ref MutList.new(r) @ r;
            MutList.push!(1, deref l1);
            MutMap.put!(1, l1, deref Array.get(0, deref a));
            MutList.push!(a, deref l);
            match MutList.pop!(deref l) {
                case Some(b) => match MutMap.get(1, deref Array.get(0, deref b)) {
                    case Some(k) => MutList.pop!(deref k) == Some(1)
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3NestedReferences03(): Bool =
        region r {
            let m = ref MutMap.new(r) @ r;
            let l = ref MutList.new(r) @ r;
            let a = ref Array.repeat(r, 8, ref MutMap.new(r) @ r) @ r;
            MutMap.put!(1, 1, deref Array.get(0, deref a));
            MutList.push!(a, deref l);
            MutMap.put!(1, l, deref m);
            match MutMap.get(1, deref m) {
                case Some(k) => match MutList.pop!(deref k) {
                    case Some(b) => MutMap.get(1, deref Array.get(0, deref b)) == Some(1)
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions2RegionsReferences01(): Bool =
        region r1 {
            let a1 = ref Array.repeat(r1, 8, 1) @ r1;
            Array.get(0, deref a1) == 1 and 1 ==
            region r2 {
                let a2 = ref Array.repeat(r2, 8, 1) @ r1;
                Array.get(0, deref a2)
            }
        }

    @test
    def testRegions2RegionsReferences02(): Bool =
        region r1 {
            let l1 = ref MutList.new(r1) @ r1;
            MutList.push!(1, deref l1);
            MutList.pop!(deref l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = ref MutList.new(r2) @ r2;
                MutList.push!(1, deref l2);
                MutList.pop!(deref l2)
            }
        }

    @test
    def testRegions2RegionsReferences03(): Bool =
        region r1 {
            region r2 {
                let l1 = ref MutList.new(r1) @ r1;
                let l2 = ref MutList.new(r2) @ r2;
                MutList.push!(1, deref l1);
                MutList.push!(1, deref l2);
                MutList.pop!(deref l1) == Some(1) and Some(1) == MutList.pop!(deref l2)
            }
        }

    @test
    def testRegions2RegionsReferences04(): Bool =
        region r1 {
            region r2 {
                let m1 = ref MutMap.new(r1) @ r2;
                let m2 = ref MutMap.new(r2) @ r1;
                MutMap.put!(1, 1, deref m1);
                MutMap.put!(1, 1, deref m2);
                MutMap.get(1, deref m1) == Some(1) and Some(1) == MutMap.get(1, deref m2)
            }
        }

    @test
    def testRegions2RegionsReferences05(): Bool =
        region r1 {
            region r2 {
                let a1 = ref Array.repeat(r1, 8, 1) @ r2;
                let a2 = ref Array.repeat(r2, 8, 1) @ r2;
                Array.get(0, deref a1) == 1 and 1 == Array.get(0, deref a2)
            }
        }

    @test
    def testRegions2RegionsReferences06(): Bool =
        region r1 {
            region r2 {
                let l1 = ref MutList.new(r1) @ r1;
                let l2 = ref MutList.new(r2) @ r1;
                MutList.push!(1, deref l1);
                MutList.push!(1, deref l2);
                MutList.pop!(deref l1) == Some(1) and Some(1) == MutList.pop!(deref l2)
            }
        }

    @test
    def testRegions2Regions1NestedReferences01(): Bool =
        region r1 {
            region r2 {
                let a: Ref[Array[Ref[MutMap[Int32, Int32, r2], r1], r1], r2] = ref Array.repeat(r1, 8, ref MutMap.new(r2) @ r1) @ r2;
                MutMap.put!(1, 1, deref Array.get(0, deref a));
                MutMap.get(1, deref Array.get(0, deref a)) == Some(1)
            }
        }

    @test
    def testRegions2Regions1NestedReferences02(): Bool =
        region r1 {
            region r2 {
                let l: Ref[MutList[Ref[Array[Int32, r2], r1], r1], r1] = ref MutList.new(r1) @ r1;
                MutList.push!(ref Array.repeat(r2, 8, 1) @ r1, deref l);
                match MutList.pop!(deref l) {
                    case Some(a) => Array.get(0, deref a) == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions1NestedReferences03(): Bool =
        region r1 {
            region r2 {
                let m: Ref[Ref[MutMap[Int32, Ref[MutList[Int32, r2], r1], r1], r1], r2] = ref ref MutMap.new(r1) @ r1 @ r2;
                let l = ref MutList.new(r2) @ r1;
                MutList.push!(1, deref l);
                MutMap.put!(1, l, deref deref m);
                match MutMap.get(1, deref deref m) {
                    case Some(k) => MutList.pop!(deref k) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions1NestedReferences04(): Bool =
        region r1 {
            region r2 {
                let a: Ref[Array[Ref[MutMap[Int32, Int32, r1], r2], r2], r2] = ref Array.repeat(r2, 8, ref MutMap.new(r1) @ r2) @ r2;
                MutMap.put!(1, 1, deref Array.get(0, deref a));
                MutMap.get(1, deref Array.get(0, deref a)) == Some(1)
            }
        }

    @test
    def testRegions2Regions2NestedReferences01(): Bool =
        region r1 {
            region r2 {
                let a: Ref[Array[Ref[MutMap[Int32, Ref[MutList[Int32, r1], r1], r2], r2], r2], r2] = ref Array.repeat(r2, 8, ref MutMap.new(r2) @ r2) @ r2;
                let l = ref MutList.new(r1) @ r1;
                MutList.push!(1, deref l);
                MutMap.put!(1, l, deref Array.get(0, deref a));
                match MutMap.get(1, deref Array.get(0, deref a)) {
                    case Some(k) => MutList.nth(0, deref k) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions2NestedReferences02(): Bool =
        region r1 {
            region r2 {
                let l: Ref[MutList[Ref[Array[Ref[MutMap[Int32, Int32, r2], r1], r2], r1], r2], r1] = ref MutList.new(r2) @ r1;
                let a = ref Array.repeat(r2, 8, ref MutMap.new(r2) @ r1) @ r1;
                MutList.push!(a, deref l);
                MutMap.put!(1, 1, deref Array.get(0, deref a)); // Mutate a[0], the MutList `l` above should also be updated by this
                match MutList.pop!(deref l) {
                    case Some(b) => MutMap.get(1, deref Array.get(0, deref b)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions2NestedReferences03(): Bool =
        region r1 {
            region r2 {
                let m: Ref[MutMap[Int32, Ref[MutList[Ref[Array[Int32, r2], r2], r1], r2], r1], r2] = ref MutMap.new(r1) @ r2;
                let l = ref MutList.new(r1) @ r2;
                MutList.push!(ref Array.repeat(r2, 8, 1) @ r2, deref l);
                MutMap.put!(1, l, deref m);
                match MutMap.get(1, deref m) {
                    case Some(k) => MutList.nth(0, deref k) |> Option.flatMap(arr -> Array.nth(0, deref arr)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3NestedReferences01(): Bool =
        region r1 {
            region r2 {
                let a: Ref[Array[Ref[MutMap[Int32, Ref[MutList[Ref[Array[Int32, r1], r1], r1], r1], r2], r2], r2], r2] = ref Array.repeat(r2, 8, ref MutMap.new(r2) @ r2) @ r2;
                let l = ref MutList.new(r1) @ r1;
                MutList.push!(ref Array.repeat(r1, 8, 1) @ r1, deref l);
                MutMap.put!(1, l, deref Array.get(0, deref a));
                match MutMap.get(1, deref Array.get(0, deref a)) {
                    case Some(k) => MutList.nth(0, deref k) |> Option.flatMap(arr -> Array.nth(0, deref arr)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3NestedReferences02(): Bool =
        region r1 {
            region r2 {
                let l: Ref[MutList[Ref[Array[Ref[MutMap[Int32, Ref[MutList[Int32, r2], r1], r2], r1], r1], r2], r2], r1] = ref MutList.new(r2) @ r1;
                let a = ref Array.repeat(r1, 8, ref MutMap.new(r2) @ r1) @ r2;
                MutList.push!(a, deref l);
                let l2 = ref MutList.new(r2) @ r1;
                MutMap.put!(1, l2, deref Array.get(0, deref a)); // Mutate a[0], the MutList `l` above should also be updated by this
                MutList.push!(1, deref l2);
                match MutList.pop!(deref l) {
                    case Some(b) => match MutMap.get(1, deref Array.get(0, deref b)) {
                        case Some(k) => MutList.nth(0, deref k) == Some(1)
                        case _       => unreachable!()
                    }
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3NestedReferences03(): Bool =
        region r1 {
            region r2 {
                let m: Ref[MutMap[Int32, Ref[MutList[Ref[Array[Ref[MutMap[Int32, Int32, r2], r1], r2], r1], r1], r2], r1], r2] = ref MutMap.new(r1) @ r2;
                let l = ref MutList.new(r1) @ r2;
                let a = ref Array.repeat(r2, 8, ref MutMap.new(r2) @ r1) @ r1;
                MutList.push!(a, deref l);
                MutMap.put!(1, 1, deref Array.get(0, deref a)); // Mutate the innermost nested MutMap
                MutMap.put!(1, l, deref m);
                match MutMap.get(1, deref m) {
                    case Some(k) => MutList.nth(0, deref k) |> Option.flatMap(arr -> Array.nth(0, deref arr))
                                                            |> Option.flatMap(m1 -> MutMap.get(1, deref m1)) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions3RegionsReferences01(): Bool =
        region r1 {
            let a = ref Array.repeat(r1, 8, 1) @ r1;
            region r2 {
                let l = ref MutList.new(r2) @ r1;
                region _ {
                    let m = ref MutMap.new(r2) @ r2;
                    MutList.push!(1, deref l);
                    MutMap.put!(1, 1, deref m);
                    Array.get(0, deref a) == 1 and MutList.pop!(deref l) == Some(1) and MutMap.get(1, deref m) == Some(1)
                }
            }
        }

    @test
    def testRegions3RegionsReferences02(): Bool =
        region r1 {
            let a = ref Array.repeat(r1, 8, 1) @ r1;
            region r2 {
                let l = ref MutList.new(r2) @ r2;
                region r3 {
                    let m = ref MutMap.new(r3) @ r3;
                    MutList.push!(1, deref l);
                    MutMap.put!(1, 1, deref m);
                    Array.get(0, deref a) == 1 and MutList.pop!(deref l) == Some(1) and MutMap.get(1, deref m) == Some(1)
                }
            }
        }

    @test
    def testRegions3RegionsReferences03(): Bool =
        region r1 {
            let a = ref Array.repeat(r1, 8, 1) @ r1;
            region r2 {
                let l = ref MutList.new(r2) @ r1;
                region r3 {
                    let m = ref MutMap.new(r3) @ r1;
                    MutList.push!(1, deref l);
                    MutMap.put!(1, 1, deref m);
                    Array.get(0, deref a) == 1 and MutList.pop!(deref l) == Some(1) and MutMap.get(1, deref m) == Some(1)
                }
            }
        }

    @test
    def testRegions3RegionsReferences04(): Bool =
        region r1 {
            let a = ref Array.repeat(r1, 8, 1) @ r1;
            region r2 {
                let l = ref MutList.new(r2) @ r2;
                region r3 {
                    let m = ref MutMap.new(r3) @ r3;
                    MutList.push!(1, deref l);
                    MutMap.put!(1, 1, deref m);
                    Array.get(0, deref a) == 1 and MutList.pop!(deref l) == Some(1) and MutMap.get(1, deref m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions1NestedReferences01(): Bool =
        region r1 {
            region r2 {
                let a: Ref[Array[Ref[MutList[Int32, r1], r2], r2], r2] = ref Array.repeat(r2, 8, ref MutList.new(r1) @ r2) @ r2;
                region _ {
                    MutList.push!(1, deref Array.get(0, deref a));
                    MutList.pop!(deref Array.get(0, deref a)) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions1NestedReferences02(): Bool =
        region r1 {
            region r2 {
                let l: Ref[MutList[Ref[MutMap[Int32, Int32, r1], r2], r2], r1] = ref MutList.new(r2) @ r1;
                region _ {
                    let m = ref MutMap.new(r1) @ r2;
                    MutList.push!(m, deref l);
                    MutMap.put!(1, 1, deref m);
                    match MutList.pop!(deref l) {
                        case Some(n) => MutMap.get(1, deref n) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions1NestedReferences03(): Bool =
        region _ {
            region r2 {
                region r3 {
                    let m: Ref[MutMap[Int32, Ref[MutList[Int32, r3], r3], r2], r2] = ref MutMap.new(r2) @ r2;
                    let l = ref MutList.new(r3) @ r3;
                    MutList.push!(1, deref l);
                    MutMap.put!(1, l, deref m);
                    match MutMap.get(1, deref m) {
                        case Some(k) => MutList.pop!(deref k) == Some(1)
                        case _       => unreachable!()
                    } and MutList.pop!(deref l) == None
                }
            }
        }

    @test
    def testRegions3Regions2NestedReferences01(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let a: Ref[Array[Ref[MutMap[Int32, Ref[MutList[Int32, r3], r1], r2], r3], r1], r2] = ref Array.new(r1, 8) @ r2;
                    let m = ref MutMap.new(r2) @ r3;
                    let l = ref MutList.new(r3) @ r1;
                    MutList.push!(1, deref l);
                    MutMap.put!(1, l, deref m);
                    Array.put(m, 0, deref a);
                    match MutMap.get(1, deref Array.get(0, deref a)) {
                        case Some(k) => MutList.pop!(deref k) == Some(1)
                        case _       => unreachable!()
                    } and MutList.pop!(deref l) == None
                }
            }
        }

    @test
    def testRegions3Regions2NestedReferences02(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let l: Ref[MutList[Ref[Array[Ref[MutMap[Int32, Int32, r1], r3], r2], r1], r3], r2] = ref MutList.new(r3) @ r2;
                    let a = ref Array.repeat(r2, 8, ref MutMap.new(r1) @ r3) @ r1;
                    MutList.push!(a, deref l);
                    MutMap.put!(1, 1, deref Array.get(0, deref a));
                    match MutList.pop!(deref l) {
                        case Some(b) => MutMap.get(1, deref Array.get(0, deref b)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions2NestedReferences03(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let m: Ref[MutMap[Int32, Ref[MutList[Ref[Array[Int32, r1], r2], r3], r1], r2], r3] = ref MutMap.new(r2) @ r3;
                    let l = ref MutList.new(r3) @ r1;
                    MutMap.put!(1, l, deref m);
                    MutList.push!(ref Array.repeat(r1, 8, 1) @ r2, deref l);
                    match MutMap.get(1, deref m) {
                        case Some(k) => MutList.nth(0, deref k) |> Option.flatMap(arr -> Array.nth(0, deref arr)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions3NestedReferences01(): Bool =
        region r1 {
            region r2 {
                let l = ref MutList.new(r1) @ r2;
                region r3 {
                    let a: Ref[Array[Ref[MutMap[Int32, Ref[MutList[Ref[Array[Int32, r1], r1], r1], r2], r2], r3], r3], r1] = ref Array.repeat(r3, 8, ref MutMap.new(r2) @ r3) @ r1;
                    MutMap.put!(1, l, deref Array.get(0, deref a));
                    MutList.push!(ref Array.repeat(r1, 8, 1) @ r1, deref l);
                    match MutMap.get(1, deref Array.get(0, deref a)) {
                        case Some(k) => MutList.nth(0, deref k) |> Option.flatMap(arr -> Array.nth(0, deref arr)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions3NestedReferences02(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let l: Ref[MutList[Ref[Array[Ref[MutMap[Int32, Ref[MutList[Int32, r2], r2], r3], r3], r1], r1], r2], r2] = ref MutList.new(r2) @ r2;
                    let a = ref Array.repeat(r1, 8, ref MutMap.new(r3) @ r3) @ r1;
                    let l2 = ref MutList.new(r2) @ r2;
                    MutList.push!(a, deref l);
                    MutMap.put!(1, l2, deref Array.get(0, deref a));
                    MutList.push!(1, deref l2);
                    match MutList.pop!(deref l) {
                        case Some(b) => match MutMap.get(1, deref Array.get(0, deref b)) {
                            case Some(k) => MutList.pop!(deref k) == Some(1)
                            case _       => unreachable!()
                        }
                        case _       => unreachable!()
                    } and MutList.pop!(deref l2) == None
                }
            }
        }

    @test
    def testRegions3Regions3NestedReferences03(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let m: Ref[MutMap[Int32, Ref[MutList[Ref[Array[Ref[MutMap[Int32, Int32, r1], r3], r3], r2], r1], r1], r2], r1] = ref MutMap.new(r2) @ r1;
                    let l = ref MutList.new(r1) @ r1;
                    let a = ref Array.repeat(r3, 8, ref MutMap.new(r1) @ r3) @ r2;
                    MutMap.put!(1, l, deref m);
                    MutList.push!(a, deref l);
                    MutMap.put!(1, 1, deref Array.get(0, deref a));
                    match MutMap.get(1, deref m) {
                        case Some(k) => MutList.nth(0, deref k) |> Option.flatMap(arr -> Array.nth(0, deref arr))
                                                                |> Option.flatMap(m1 -> MutMap.get(1, deref m1)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions3Regions3NestedReferences04(): Bool =
        region r1 {
            region r2 {
                region r3 {
                    let m: Ref[Ref[Ref[Ref[MutMap[Int32, Ref[MutList[Ref[Array[Ref[Ref[Ref[Ref[MutMap[Int32, Int32, r1], r3], r2], r1], r3], r3], r2], r1], r1], r2], r1], r3], r2], r3] =
                        ref ref ref ref MutMap.new(r2) @ r1 @ r3 @ r2 @ r3;
                    let l = ref MutList.new(r1) @ r1;
                    let a = ref Array.repeat(r3, 8, ref ref ref ref MutMap.new(r1) @ r3 @ r2 @ r1 @ r3) @ r2;
                    MutMap.put!(1, l, deref deref deref deref m);
                    MutList.push!(a, deref l);
                    MutMap.put!(1, 1, deref deref deref deref Array.get(0, deref a));
                    match MutMap.get(1, deref deref deref deref m) {
                        case Some(k) => MutList.nth(0, deref k) |> Option.flatMap(arr -> Array.nth(0, deref arr))
                                                                |> Option.flatMap(m1 -> MutMap.get(1, deref deref deref deref m1)) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegionExit01(): Bool \ IO = region r1 {
        let x  = ref false @ r1;
        region r2 {
            spawn { Thread.sleep(Time.Duration.fromMilliSeconds(10)); x := true } @ r2
        };
        deref x
    }

    @test
    def testRegionExit02(): Bool \ IO = region r1 {
        let x = ref false @ r1;
        region r2 {
            $SCOPE_EXIT$(() -> x := true, r2)
        };
        deref x
    }
}
