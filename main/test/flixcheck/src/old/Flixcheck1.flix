mod FlixCheck1 {
    pub type alias Rand = Int32

    pub enum Gen[a](Int32 -> Rand -> a)

    ///
    /// Applies a generator `gen` to a random number `r` and returns the generated value.
    ///
    /// `gen` the generator to apply.
    ///
    /// `r` the random number.
    ///
    /// returns the generated value.
    ///
    pub def applyGen(gen: Gen[a], size: Int32, r: Rand): a =
        match gen {
            case Gen.Gen(f) => f(size, r)
        }

    pub def sized(f: Int32 -> Gen[a]): Gen[a] =
    Gen.Gen(size -> rand ->
        applyGen(f(size), size, rand)
    )

    pub def resize(size: Int32, gen: Gen[a]): Gen[a] =
        Gen.Gen(_ -> rand ->
            applyGen(gen, size, rand)
        )

    pub def rand(): Gen[Rand] =
        Gen.Gen(_ -> rand -> rand)

    pub def promote(f: a -> Gen[b]): Gen[a -> b] =
        Gen.Gen(size -> rand -> 
            a -> let gen = f(a);
            applyGen(gen, size, rand)
        )

    pub def variant(v: Int32, gen: Gen[a]): Gen[a] =
        Gen.Gen(size -> rand ->
            let Gen.Gen(f) = gen;
            f(size, rands(rand, Int32.abs(v) + 1))    
        )

    pub def rands(rand: Rand, v: Int32): Rand =
        let (r1, r2) = splitRand(rand);
        match v {
            case 0 => r1
            case n => rands(r2, n - 1)
        }

    /// Returns a generator that generates the value `a`.
    pub def return(x: a): Gen[a] = 
        Gen.Gen(_ -> _ -> x)


    pub def elements(xs: Nel[a]): Gen[a] =
        Gen.Gen(size -> rand -> 
            let vec = Nel.toVector(xs);
            let vecSize = Vector.length(vec);
            Vector.get(applyGen(chooseInt(0, vecSize - 1), size, rand), vec)
        )

    pub def oneOf(xs: Nel[Gen[a]]): Gen[a] =
        Gen.Gen(size -> rand -> 
            let (r1, r2) = splitRand(rand);
            let vec = Nel.toVector(xs);
            let vecSize = Vector.length(vec);
            applyGen(Vector.get(applyGen(chooseInt(0, vecSize - 1), size, r1), vec), size, r2)
        )

    pub def frequency(xs: Nel[(Int32, Gen[a])]): Gen[a] =
        Gen.Gen(size -> rand ->
            let (r1, r2) = splitRand(rand);
            let total = Nel.foldLeft((acc, x) -> acc + fst(x), 0, xs);
            applyGen(pick(applyGen(chooseInt(1, total), size, r1), Nel.toList(xs)), size, r2)
        )
        
    pub def pick(n: Int32, xs: List[(Int32, Gen[a])]): Gen[a] =
        match List.head(xs) {
            case None => unreachable!(),
            case Some((k, gen)) if (n <= k) => 
                gen,
            case Some((k, _)) =>
                pick(n-k, List.drop(1, xs))
        }

    /// Generates a random pair of values using two generators.
    pub def randomPair(aGen: Gen[a], bGen: Gen[b]): Gen[(a, b)] =
        Gen.Gen(size -> rand -> 
            let (r1, r2) = splitRand(rand);
            (applyGen(aGen, size, r1), applyGen(bGen, size, r2))
        )

    /// Generates a random triple of values using three generators.
    pub def randomTriple(aGen: Gen[a], bGen: Gen[b], cGen: Gen[c]): Gen[(a, b, c)] =
        Gen.Gen(size -> rand -> 
            let (r1, r2) = splitRand(rand);
            let (r3, r4) = splitRand(r1);
            (applyGen(aGen, size, r2), applyGen(bGen, size, r3), applyGen(cGen, size, r4))
        )

    /// Generates a random quadrouple of values usiflix> :eval main()ng four generators.
    pub def randomQuadruple(aGen: Gen[a], bGen: Gen[b], cGen: Gen[c], dGen: Gen[d]): Gen[(a, b, c, d)] =
        Gen.Gen(size -> rand -> 
            let (r1, r2) = splitRand(rand);
            let (r3, r4) = splitRand(r1);
            let (r5, r6) = splitRand(r2);
            (applyGen(aGen, size, r3), applyGen(bGen, size, r4), applyGen(cGen, size, r5), applyGen(dGen, size, r6))
        )
        
    /// Should produce a generator of a list of values with average length 2
    pub def randomList(aGen: Gen[a]): Gen[List[a]] =
        Gen.Gen(size -> rand ->
            let (r1, r2) = splitRand(rand);
            let (r3, r4) = splitRand(r1);
            match applyGen(chooseBool(), size, r2) {
                case true => applyGen(aGen, size, r3) :: applyGen(randomList(aGen), size, r4),
                case false => Nil
            }
        )

    /// Should generate a list with length in the interval [0, length]
    pub def randomListSized(aGen: Gen[a], maxLength: Int32): Gen[List[a]] =
        Gen.Gen(size -> rand -> 
            let (r1, r2) = splitRand(rand);
            let length = applyGen(chooseInt(0, maxLength), size, r1);
            randomListHelper(aGen, length, size, r2)
        )

    /// Helper function for generating a list with a specific length
    pub def randomListHelper(aGen: Gen[a], length: Int32, size: Int32, rand: Rand): List[a] =
        let (r1, r2) = splitRand(rand);
        if (length <= 0) 
            Nil
        else
            applyGen(aGen, size, r1) :: randomListHelper(aGen, length-1, size, r2)

    /// Should generate a list using average length for controlling the generated list size for a better distribution for testing
    pub def randomListSizedAvg(aGen: Gen[a], avgLength: Int32, bound: Int32): Gen[List[a]] =
        Gen.Gen(size -> rand ->
            let (r1, r2) = splitRand(rand);
            let (r3, r4) = splitRand(r1);
            if (bound <= 0) 
                Nil
            else
                match applyGen(chooseInt(0, avgLength), size, r2) {
                    case 0 => Nil,
                    case _ => applyGen(aGen, size, r3) :: applyGen(randomListSizedAvg(aGen, avgLength, bound-1), size, r4)
                }
        )

    /// Should generate a non-empty list with length in the interval [1, length]
    pub def randomNelSized(aGen: Gen[a], maxLength: Int32): Gen[Nel[a]] =
        Gen.Gen(size -> rand ->
            let (r1, r2) = splitRand(rand);
            let listGen = randomListSized(aGen, maxLength - 1);
            let aList = applyGen(listGen, size, r1);
            Nel.Nel(applyGen(aGen, size, r2), aList)
        )

    /// Should generate a chain with length in the interval [0, length]
    pub def randomChainSized(aGen: Gen[a], maxLength: Int32): Gen[Chain[a]] =
        Gen.Gen(size -> rand -> 
            let listGen = randomListSized(aGen, maxLength);
            let l = applyGen(listGen, size, rand);
            List.toChain(l)
        )

    /// Should generate a non-empty chain with length in the interval [1, length]
    pub def randomNecSized(aGen: Gen[a], maxLength: Int32): Gen[Nec[a]] =
        Gen.Gen(size -> rand -> 
            let (r1, r2) = splitRand(rand);
            let listGen = randomListSized(aGen, maxLength - 1);
            let aList = applyGen(listGen, size, r1);
            let necHead = Nec.NecOne(applyGen(aGen, size, r2));
            match List.toNec(aList) {
                case None => necHead,
                case Some(necTail) => Nec.Nec(necHead, necTail)
            }
        )

    /// Should generate a vector with length in the interval [0, length]
    pub def randomVectorSized(aGen: Gen[a], maxLength: Int32): Gen[Vector[a]] =
        Gen.Gen(size -> rand ->
            let listGen = randomListSized(aGen, maxLength);
            let aList = applyGen(listGen, size, rand);
            List.toVector(aList)
        )

    /// Should generate Some value or None with equal probability 
    pub def optionGenerator(aGen: Gen[a]): Gen[Option[a]] =
        Gen.Gen(size -> rand ->
            let (r1, r2) = splitRand(rand);
            match applyGen(chooseBool(), size, r1) {
                case true => Some(applyGen(aGen, size, r2)),
                case false => None
            }
        )

    /// Should generate Ok value or Err value with equal probability
    pub def resultGenerator(eGen: Gen[e], tGen: Gen[t]): Gen[Result[e, t]] =
        Gen.Gen(size -> rand ->
            let (r1, r2) = splitRand(rand);
            match applyGen(chooseBool(), size, r1) {
                case true => Ok(applyGen(tGen, size, r2)),
                case false => Err(applyGen(eGen, size, r2))
            }
        )
        
    /// Returns a Generator that uniformly generates values in the interval [lower, upper]
    pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] = 
        Gen.Gen(_ -> rand -> Int32.abs(Int32.remainder(rand, (upper + 1) - lower)) + lower)

    /// Returns a generator that uniformly generates a boolean value.
    pub def chooseBool(): Gen[Bool] =
        elements(Nel.Nel(true, false :: Nil))

    /// TODO: Implement a better float generator
    pub def chooseFloat(lower: Int32, upper: Int32): Gen[Float32] =
        Gen.Gen(size -> rand -> 
            let (r1, r2) = splitRand(rand);
            let intGen = chooseInt(lower, upper - 1);
            let quotientGen = chooseInt(0, Int32.maxValue());

            let quotient = Int32.toFloat32(applyGen(quotientGen, size, r1));
            let fraction: Float32 = quotient/Int32.toFloat32(Int32.maxValue());

            Int32.toFloat32(applyGen(intGen, size, r2)) + fraction
        )


    /// Returns a generator that uniformly generates a lower case character.
    pub def chooseLowerChar(): Gen[Char] =
        elements(Nel.Nel('a', 
        'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 
        'i' :: 'j' :: 'k' :: 'l' :: 'm' :: 'n' :: 'o' :: 
        'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 
        'w' :: 'x' :: 'y' :: 'z' :: Nil))


    /// Returns a generator that uniformly generates an upper case character.
    pub def chooseUpperChar(): Gen[Char] =
        elements(Nel.Nel('A', 
        'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' ::
        'I' :: 'J' :: 'K' :: 'L' :: 'M' :: 'N' :: 'O' ::
        'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' ::
        'W' :: 'X' :: 'Y' :: 'Z' :: Nil))


    // Arbitrary trait
    pub trait Arbitrary[a] {
        pub def arbitrary(): Gen[a]
    }

    // Arbitrary instances
    instance Arbitrary[Unit] {
        pub def arbitrary(): Gen[Unit] =
            return()
    }

    instance Arbitrary[Int32] {
        pub def arbitrary(): Gen[Int32] =
            sized(size -> chooseInt(-size, size))
    }

    instance Arbitrary[Bool] {
        pub def arbitrary(): Gen[Bool] =
            chooseBool()
    }

    instance Arbitrary[Char] {
        pub def arbitrary(): Gen[Char] = 
            chooseLowerChar()
    }

    instance Arbitrary[Float32] {
        pub def arbitrary(): Gen[Float32] = 
            sized(size -> chooseFloat(-size, size))
    }

    instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
        pub def arbitrary(): Gen[(a, b)] =
            randomPair(Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }

    instance Arbitrary[(a, b, c)] with Arbitrary[a], Arbitrary[b], Arbitrary[c] {
        pub def arbitrary(): Gen[(a, b, c)] =
            randomTriple(Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }

    instance Arbitrary[(a, b, c, d)] with Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d] {
        pub def arbitrary(): Gen[(a, b, c, d)] =
            randomQuadruple(Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }

    instance Arbitrary[List[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[List[a]] =
            sized(size -> randomListSized(Arbitrary.arbitrary(), size))
    }

    instance Arbitrary[Nel[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Nel[a]] =
            sized(size -> randomNelSized(Arbitrary.arbitrary(), size))
    }

    instance Arbitrary[Chain[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Chain[a]] =
            sized(size -> randomChainSized(Arbitrary.arbitrary(), size))
    }

    instance Arbitrary[Nec[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Nec[a]] =
            sized(size -> randomNecSized(Arbitrary.arbitrary(), size))
    }

    instance Arbitrary[Vector[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Vector[a]] =
            sized(size -> randomVectorSized(Arbitrary.arbitrary(), size))
    }

    instance Arbitrary[Option[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Option[a]] = 
            optionGenerator(Arbitrary.arbitrary())
    }

    instance Arbitrary[Result[e, t]] with Arbitrary[e], Arbitrary[t] {
        pub def arbitrary(): Gen[Result[e, t]] =
            resultGenerator(Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }

}