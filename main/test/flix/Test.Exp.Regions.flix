namespace Test/Exp/Regions {

    // Uses references and regions (e.g. could just duplicate most test cases for both).
    // Test cases that use 0, 1, 2, and 3 nested regions.
    // Test cases that use 0, 1, 2, 3 data structures nested inside each other (e.g. array inside array inside array).
    // Test cases that mix both multiple regions and multiple data structures.

    @test
    def testRegions0Regions01(): Array[Int32, Static] & Impure =
        []

    @test
    def testRegions0Regions02(): MutList[Int32, Static] & Impure =
        new MutList(Static)

    @test
    def testRegions0Regions03(): MutMap[Int32, Int32, Static] & Impure =
        new MutMap(Static)

    @test
    def testRegions0Regions1Nested01(): Array[Array[Int32, Static], Static] & Impure =
        []

    @test
    def testRegions0Regions1Nested02(): MutList[MutList[Int32, Static], Static] & Impure =
        new MutList(Static)

    @test
    def testRegions0Regions1Nested03(): MutMap[Int32, MutMap[Int32, Int32, Static], Static] & Impure =
        new MutMap(Static)

    @test
    def testRegions0Regions1Nested04(): Array[MutMap[Int32, Int32, Static], Static] & Impure =
        []

    @test
    def testRegions0Regions1Nested05(): MutList[Array[Int32, Static], Static] & Impure =
        new MutList(Static)

    @test
    def testRegions0Regions1Nested06(): MutMap[Int32, MutList[Int32, Static], Static] & Impure =
        new MutMap(Static)

    @test
    def testRegions0Regions1Nested07(): Array[MutList[Int32, Static], Static] & Impure =
        []

    @test
    def testRegions0Regions1Nested08(): MutList[MutMap[Int32, Int32, Static], Static] & Impure =
        new MutList(Static)

    @test
    def testRegions0Regions1Nested09(): MutMap[Int32, Array[Int32, Static], Static] & Impure =
        new MutMap(Static)

    @test
    def testRegions0Regions2Nested01(): Array[MutMap[Int32, MutList[Int32, Static], Static], Static] & Impure =
        []

    @test
    def testRegions0Regions2Nested02(): MutList[Array[MutMap[Int32, Int32, Static], Static], Static] & Impure =
        new MutList(Static)

    @test
    def testRegions0Regions2Nested03(): MutMap[Int32, MutList[Array[Int32, Static], Static], Static] & Impure =
        new MutMap(Static)

    @test
    def testRegions0Regions3Nested01(): Array[MutMap[Int32, MutList[Array[Int32, Static], Static], Static], Static] & Impure =
        []

    @test
    def testRegions0Regions3Nested02(): MutList[Array[MutMap[Int32, MutList[Int32, Static], Static], Static], Static] & Impure =
        new MutList(Static)

    @test
    def testRegions0Regions3Nested03(): MutMap[Int32, MutList[Array[MutMap[Int32, Int32, Static], Static], Static], Static] & Impure =
        new MutMap(Static)

    @test
    def testRegions1Region01(): Bool =
        region r {
            let a = [1; 8];
            a[0] == 1
        }

    @test
    def testRegions1Region02(): Bool =
        region r {
            let l = new MutList(r);
            MutList.push!(1, l);
            MutList.pop!(l) == Some(1)
        }

    @test
    def testRegions1Region03(): Bool =
        region r {
            let m = new MutMap(r);
            MutMap.put!(1, 1, m);
            MutMap.get(1, m) == Some(1)
        }

    @test
    def testRegions1Region1Nested01(): Bool & Impure =
        region r {
            let a = [new MutMap(Static); 8];
            MutMap.put!(1, 1, a[0]);
            MutMap.get(1, a[0]) == Some(1)
        }

    @test
    def testRegions1Region1Nested02(): Bool & Impure =
        region r {
            let l = new MutList(r);
            MutList.push!([1; 8] @ Static, l);
            match MutList.pop!(l) {
                case Some(a) => a `Array.sameElements` [1; 8]
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region1Nested03(): Bool & Impure =
        region r {
            let m = new MutMap(r);
            let l = new MutList(Static);
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
            let a = [new MutMap(r); 8];
            MutMap.put!(1, 1, a[0]);
            MutMap.get(1, a[0]) == Some(1)
        }

    @test
    def testRegions1Region1Nested05(): Bool =
        region r {
            let l = new MutList(r);
            MutList.push!([1; 8] @ r, l);
            match MutList.pop!(l) {
                case Some(a) => a `Array.sameElements` [1; 8]
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region1Nested06(): Bool =
        region r {
            let m = new MutMap(r);
            let l = new MutList(r);
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
            let a = [new MutMap(r); 8];
            let l = new MutList(r);
            MutList.push!(1, l);
            MutMap.put!(1, l, a[0]);
            match MutMap.get(1, a[0]) {
                case Some(k) => MutList.pop!(k) == Some(1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2Nested02(): Bool =
        region r {
            let l = new MutList(r);
            let a = [new MutMap(r); 8] @ r;
            MutMap.put!(1, 1, a[0]);
            MutList.push!(a, l);
            match MutList.pop!(l) {
                case Some(b) => MutMap.get(1, b[0]) == Some(1)
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region2Nested03(): Bool =
        region r {
            let m = new MutMap(r);
            let l = new MutList(r);
            MutList.push!([1; 8] @ r, l);
            MutMap.put!(1, l, m);
            match MutMap.get(1, m) {
                case Some(k) => match MutList.pop!(k) {
                    case Some(a) => a[0] == 1
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3Nested01(): Bool =
        region r {
            let a = [new MutMap(r); 8];
            let l = new MutList(r);
            MutList.push!([1; 8] @ r, l);
            MutMap.put!(1, l, a[0]);
            match MutMap.get(1, a[0]) {
                case Some(k) => match MutList.pop!(k) {
                    case Some(b) => b[0] == 1
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3Nested02(): Bool =
        region r {
            let l = new MutList(r);
            let a = [new MutMap(r); 8] @ r;
            let l1 = new MutList(r);
            MutList.push!(1, l1);
            MutMap.put!(1, l1, a[0]);
            MutList.push!(a, l);
            match MutList.pop!(l) {
                case Some(b) => match MutMap.get(1, b[0]) {
                    case Some(k) => MutList.pop!(k) == Some(1)
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions1Region3Nested03(): Bool =
        region r {
            let m = new MutMap(r);
            let l = new MutList(r);
            let a = [new MutMap(r); 8] @ r;
            MutMap.put!(1, 1, a[0]);
            MutList.push!(a, l);
            MutMap.put!(1, l, m);
            match MutMap.get(1, m) {
                case Some(k) => match MutList.pop!(k) {
                    case Some(b) => MutMap.get(1, b[0]) == Some(1)
                    case _       => unreachable!()
                }
                case _       => unreachable!()
            }
        }

    @test
    def testRegions2Regions01(): Bool & Impure =
        region _ {
            let a1 = [1; 8] @ Static;
            a1[0] == 1 and 1 ==
            region _ {
                let a2 = [1; 8] @ Static;
                a2[0]
            }
        }

    @test
    def testRegions2Regions02(): Bool & Impure =
        region _ {
            let l1 = new MutList(Static);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region _ {
                let l2 = new MutList(Static);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions03(): Bool & Impure =
        region _ {
            let m1 = new MutMap(Static);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region _ {
                let m2 = new MutMap(Static);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions04(): Bool & Impure =
        region r1 {
            let a1 = [1; 8] @ r1;
            a1[0] == 1 and 1 ==
            region _ {
                let a2 = [1; 8] @ Static;
                a2[0]
            }
        }

    @test
    def testRegions2Regions05(): Bool & Impure =
        region r1 {
            let l1 = new MutList(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region _ {
                let l2 = new MutList(Static);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions06(): Bool & Impure =
        region r1 {
            let m1 = new MutMap(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region _ {
                let m2 = new MutMap(Static);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions07(): Bool & Impure =
        region _ {
            let a1 = [1; 8] @ Static;
            a1[0] == 1 and 1 ==
            region r2 {
                let a2 = [1; 8] @ r2;
                a2[0]
            }
        }

    @test
    def testRegions2Regions08(): Bool & Impure =
        region _ {
            let l1 = new MutList(Static);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = new MutList(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions09(): Bool & Impure =
        region _ {
            let m1 = new MutMap(Static);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = new MutMap(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions10(): Bool =
        region r1 {
            let a1 = [1; 8] @ r1;
            a1[0] == 1 and 1 ==
            region r2 {
                let a2 = [1; 8] @ r2;
                a2[0]
            }
        }

    @test
    def testRegions2Regions11(): Bool =
        region r1 {
            let l1 = new MutList(r1);
            MutList.push!(1, l1);
            MutList.pop!(l1) == Some(1) and Some(1) ==
            region r2 {
                let l2 = new MutList(r2);
                MutList.push!(1, l2);
                MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions12(): Bool =
        region r1 {
            let m1 = new MutMap(r1);
            MutMap.put!(1, 1, m1);
            MutMap.get(1, m1) == Some(1) and Some(1) ==
            region r2 {
                let m2 = new MutMap(r2);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions13(): Bool =
        region r1 {
            let a1 = [1; 8] @ r1;
            region r2 {
                let a2 = [1; 8] @ r2;
                a1[0] == 1 and 1 == a2[0]
            }
        }

    @test
    def testRegions2Regions14(): Bool =
        region r1 {
            let l1 = new MutList(r1);

            region r2 {
                let l2 = new MutList(r2);
                MutList.push!(1, l1);
                MutList.push!(1, l2);
                MutList.pop!(l1) == Some(1) and Some(1) == MutList.pop!(l2)
            }
        }

    @test
    def testRegions2Regions15(): Bool =
        region r1 {
            let m1 = new MutMap(r1);
            region r2 {
                let m2 = new MutMap(r2);
                MutMap.put!(1, 1, m1);
                MutMap.put!(1, 1, m2);
                MutMap.get(1, m1) == Some(1) and Some(1) == MutMap.get(1, m2)
            }
        }

    @test
    def testRegions2Regions1Nested01(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutMap[Int32, Int32, r2], r1] = [new MutMap(r2); 8] @ r1;
                MutMap.put!(1, 1, a[0]);
                MutMap.get(1, a[0]) == Some(1)
            }
        }

    @test
    def testRegions2Regions1Nested02(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[Int32, r2], r1] = new MutList(r1);
                MutList.push!([1; 8] @ r2, l);
                match MutList.pop!(l) {
                    case Some(a) => a[0] == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions1Nested03(): Bool =
        region r1 {
            region r2 {
                let m: MutMap[Int32, MutList[Int32, r2], r1] = new MutMap(r1);
                let l = new MutList(r2);
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
                let a: Array[MutMap[Int32, Int32, r1], r2] = [new MutMap(r1); 8] @ r2;
                MutMap.put!(1, 1, a[0]);
                MutMap.get(1, a[0]) == Some(1)
            }
        }

    @test
    def testRegions2Regions1Nested05(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[Int32, r1], r2] = new MutList(r2);
                MutList.push!([1; 8] @ r1, l);
                match MutList.pop!(l) {
                    case Some(a) => a[0] == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions1Nested06(): Bool =
        region r1 {
            let l = new MutList(r1);
            region r2 {
                let m: MutMap[Int32, MutList[Int32, r1], r2] = new MutMap(r2);
                MutList.push!(1, l);
                MutMap.put!(1, l, m);
                match MutMap.get(1, m) {
                    case Some(k) => MutList.get(0, k) == 1
                    case _       => unreachable!()
                }
            } and MutList.get(0, l) == 1
        }

    @test
    def testRegions2Regions2Nested01(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutMap[Int32, MutList[Int32, r1], r2], r2] = [new MutMap(r2); 8] @ r2;
                let l = new MutList(r1);
                MutList.push!(1, l);
                MutMap.put!(1, l, a[0]);
                match MutMap.get(1, a[0]) {
                    case Some(k) => MutList.get(0, k) == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions2Nested02(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[MutMap[Int32, Int32, r2], r1], r2] = new MutList(r2);
                let a = [new MutMap(r2); 8] @ r1;
                MutList.push!(a, l);
                MutMap.put!(1, 1, a[0]); // Mutate a[0], the MutList `l` above should also be updated by this
                match MutList.pop!(l) {
                    case Some(b) => MutMap.get(1, b[0]) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions2Nested03(): Bool =
        region r1 {
            region r2 {
                let m: MutMap[Int32, MutList[Array[Int32, r2], r2], r1] = new MutMap(r1);
                let l = new MutList(r2);
                MutList.push!([1; 8] @ r2, l);
                MutMap.put!(1, l, m);
                match MutMap.get(1, m) {
                    case Some(k) => (MutList.get(0, k))[0] == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3Nested01(): Bool =
        region r1 {
            region r2 {
                let a: Array[MutMap[Int32, MutList[Array[Int32, r1], r1], r2], r2] = [new MutMap(r2); 8] @ r2;
                let l = new MutList(r1);
                MutList.push!([1; 8] @ r1, l);
                MutMap.put!(1, l, a[0]);
                match MutMap.get(1, a[0]) {
                    case Some(k) => (MutList.get(0, k))[0] == 1
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions2Regions3Nested02(): Bool =
        region r1 {
            region r2 {
                let l: MutList[Array[MutMap[Int32, MutList[Int32, r2], r2], r1], r2] = new MutList(r2);
                let a = [new MutMap(r2); 8] @ r1;
                MutList.push!(a, l);
                let l2 = new MutList(r2);
                MutMap.put!(1, l2, a[0]); // Mutate a[0], the MutList `l` above should also be updated by this
                MutList.push!(1, l2);
                match MutList.pop!(l) {
                    case Some(b) => match MutMap.get(1, b[0]) {
                        case Some(k) => MutList.get(0, k) == 1
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
                let m: MutMap[Int32, MutList[Array[MutMap[Int32, Int32, r2], r2], r1], r1] = new MutMap(r1);
                let l = new MutList(r1);
                let a = [new MutMap(r2); 8] @ r2;
                MutList.push!(a, l);
                MutMap.put!(1, 1, a[0]); // Mutate the innermost nested MutMap
                MutMap.put!(1, l, m);
                match MutMap.get(1, m) {
                    case Some(k) => (MutList.get(0, k))[0] |> MutMap.get(1) == Some(1)
                    case _       => unreachable!()
                }
            }
        }

    @test
    def testRegions3Regions01(): Bool & Impure =
        region r1 {
            let a = [1; 8] @ r1;
            region r2 {
                let l = new MutList(r2);
                region _ {
                    let m = new MutMap(Static);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    a[0] == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions02(): Bool & Impure =
        region _ {
            let a = [1; 8] @ Static;
            region r2 {
                let l = new MutList(r2);
                region r3 {
                    let m = new MutMap(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    a[0] == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions03(): Bool & Impure =
        region r1 {
            let a = [1; 8] @ r1;
            region _ {
                let l = new MutList(Static);
                region r3 {
                    let m = new MutMap(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    a[0] == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions04(): Bool =
        region r1 {
            let a = [1; 8] @ r1;
            region r2 {
                let l = new MutList(r2);
                region r3 {
                    let m = new MutMap(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, 1, m);
                    a[0] == 1 and MutList.pop!(l) == Some(1) and MutMap.get(1, m) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions1Nested01(): Bool & Impure =
        region _ {
            region r2 {
                let a: Array[MutList[Int32, Static], r2] = [new MutList(Static); 8] @ r2;
                region _ {
                    MutList.push!(1, a[0]);
                    MutList.pop!(a[0]) == Some(1)
                }
            }
        }

    @test
    def testRegions3Regions1Nested02(): Bool & Impure =
        region r1 {
            region _ {
                let l: MutList[MutMap[Int32, Int32, r1], Static] = new MutList(Static);
                region _ {
                    let m = new MutMap(r1);
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
                    let m: MutMap[Int32, MutList[Int32, r3], r2] = new MutMap(r2);
                    let l = new MutList(r3);
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
    def testRegions3Regions2Nested01(): Bool & Impure =
        region _ {
            region r2 {
                region r3 {
                    let a: Array[MutMap[Int32, MutList[Int32, r3], r2], Static] = [$DEFAULT$; 8] @ Static;
                    let m = new MutMap(r2);
                    let l = new MutList(r3);
                    MutList.push!(1, l);
                    MutMap.put!(1, l, m);
                    a[0] = m;
                    match MutMap.get(1, a[0]) {
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
                    let l: MutList[Array[MutMap[Int32, Int32, r1], r2], r3] = new MutList(r3);
                    let a = [new MutMap(r1); 8] @ r2;
                    MutList.push!(a, l);
                    MutMap.put!(1, 1, a[0]);
                    match MutList.pop!(l) {
                        case Some(b) => MutMap.get(1, b[0]) == Some(1)
                        case _       => unreachable!()
                    }
                }
            }
        }

    @test
    def testRegions0RegionsReferences01(): Ref[Array[Int32, Static], Static] & Impure =
        ref []

    @test
    def testRegions0RegionsReferences02(): Ref[MutList[Int32, Static], Static] & Impure =
        ref new MutList(Static)

    @test
    def testRegions0RegionsReferences03(): Ref[MutMap[Int32, Int32, Static], Static] & Impure =
        ref new MutMap(Static)

}
