
mod Monadic {
    use Monad.{>>=}

    pub type alias Rand = Int32

    pub enum Gen[a](Int32 -> Rand -> a \ IO)

    // pub def applyGen(gen: Gen[a], size: Int32, rand: Rand): a = 
    //     let Gen.Gen(g) = gen;
    //     checked_ecast(unchecked_cast(g(size, rand) as _ \ IO))
    //     // g(size, rand)

    instance Functor[Gen] {
        pub def map(f: a -> b \ ef, gen: Gen[a]): Gen[b] \ ef =
            let Gen.Gen(g) = gen;
            checked_ecast(Gen.Gen(size -> rand -> 
                unchecked_cast(f(g(size, rand)) as _ \ IO)))
    }

    instance Applicative[Gen] {
        pub def ap(fGen: Gen[a -> b \ ef], aGen: Gen[a]): Gen[b] \ ef = 
            checked_ecast(Gen.Gen(size -> rand ->
                let (r1, r2) = splitRand(rand); 
                let Gen.Gen(g) = aGen;
                let Gen.Gen(func) = fGen;
                unchecked_cast(func(size, r1)(g(size, r2)) as _ \ IO)))
    
        pub def point(x: a): Gen[a] = 
            Gen.Gen(_ -> _ -> unchecked_cast(x as _ \ IO))
    
    }

    instance Monad[Gen] {
        pub def flatMap(f: a -> Gen[b] \ ef, aGen: Gen[a]): Gen[b] \ ef =             
            checked_ecast(Gen.Gen(size -> rand -> 
                let (r1, r2) = splitRand(rand);
                let Gen.Gen(g) = aGen;
                unchecked_cast(
                    let Gen.Gen(func) = f(g(size, r1));
                    func(size, r2) as _ \ IO)))

    }
    
    /// Returns a generator that generates a value of type `a` in the interval size defines.
    pub def sized(f: Int32 -> Gen[a]): Gen[a] = 
        Gen.Gen(size -> rand -> 
            let Gen.Gen(g) = f(size);
            g(size, rand)
        )

    /// Makes sure that the generator uses the specified size.
    /// 
    /// Takes as input size and generator of type `a`.     
    ///
    /// Returns a generator that generates a value of type `a`.
    pub def resize(size: Int32, gen: Gen[a]): Gen[a] = 
        Gen.Gen(_ -> rand -> 
            let Gen.Gen(g) = gen;
            g(size, rand)
        )

    pub def rand(): Gen[Rand] = 
        Gen.Gen(_ -> rand -> unchecked_cast(rand as _ \ IO))

    // pub def promote(f: a -> Gen[b]): Gen[a -> b] = 
    //     Gen.Gen(size -> rand ->
    //         a -> let gen = f(a);
    //         let Gen.Gen(g) = gen;
    //         g(size, rand)
    //     )
    
    /// Returns a generator that generates a value of type `a` from the given non-empty list of generators.
    pub def elements(xs: Nel[a]): Gen[a] =
        let vec = Nel.toVector(xs);
        Functor.map(index -> Vector.get(index, vec), chooseInt(0, Vector.length(vec) - 1))

    /// Returns a generator that generates a list of values of type `a` of length `n`.
    ///
    /// `n` is the length of the list.
    pub def vector(n: Int32): Gen[List[a]] with Arbitrary[a] =
        let gens: List[Gen[a]] = List.map(_ -> Arbitrary.arbitrary(), List.range(0, n));
        List.sequence(gens)

    /// Returns a generator that generates a list of length `length` with values of type `a`.
    pub def sizedListOf(length: Int32, gen: Gen[a]): Gen[List[a]] =
        let gens: List[Gen[a]] = List.map(_ -> gen, List.range(0, length));
        List.sequence(gens)

    /// Returns a generator that generates a value of type `a` from the given non-empty list of generators.
    pub def oneOf(gens: Nel[Gen[a]]): Gen[a] =
        elements(gens) >>= (x -> x)

    /// Returns a randomly generated value of type `a` based on the given frequencies.
    ///
    /// `xs`: A non-empty list of pairs, where each pair consists of an integer frequency and a generator for values of type `a`.
    ///
    /// Returns a generator of type `a`.
    pub def frequency(xs: Nel[(Int32, Gen[a])]): Gen[a] =
        let total = Nel.foldLeft((acc, x) -> acc + fst(x), 0, xs);
        chooseInt(1, total) >>= (n -> pick(n, xs))

    /// Returns a randomly generated value of type `a` based on the given frequency distribution.
    ///
    /// `n` is the number used to select a generator based on its frequency.
    ///
    /// `nel` is the non-empty list of tuples containing frequencies and generators.
    ///
    /// Returns a randomly generated value of type `a`.
    pub def pick(n: Int32, nel: Nel[(Int32, Gen[a])]): Gen[a] =
        match nel {
            case Nel.Nel((k, gen), _) if (n <= k) => 
                gen,
            case Nel.Nel((k, _), y :: ys) =>
                pick(n-k, Nel.Nel(y, ys))
            case Nel.Nel((_, _), Nil) =>
                // This should never happen as the sum of all frequencies are equal to n.
                unreachable!()
        }

    /// Returns a Generator that uniformly generates values in the interval [lower, upper]
    pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] =
        let f = rand -> (Int32.abs(Int32.remainder(rand, (upper + 1) - lower)) + lower);
        Functor.map(f, rand())

    /// Returns a generator that uniformly generates a boolean value.
    pub def chooseBool(): Gen[Bool] =
        elements(Nel.Nel(true, false :: Nil))

    /// Returns a generator that uniformly generates a float value in the interval [lower, upper]
    pub def chooseFloat(lower: Int32, upper: Int32): Gen[Float32] =
        let func = (int, quotient) -> (
            let number = Int32.toFloat32(int);
            let fraction = Int32.toFloat32(quotient)/Int32.toFloat32(Int32.maxValue());
            number + fraction
        );
        Applicative.map2(func, chooseInt(lower, upper - 1), chooseInt(0, Int32.maxValue()))

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
            Applicative.point()
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
            chooseUpperChar()
    }

    instance Arbitrary[Float32] {
        pub def arbitrary(): Gen[Float32] = 
            sized(size -> chooseFloat(-size, size))
    }

    instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
        pub def arbitrary(): Gen[(a, b)] =
            Applicative.map2((x, y) -> (x, y), Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }

    instance Arbitrary[(a, b, c)] with Arbitrary[a], Arbitrary[b], Arbitrary[c] {
        pub def arbitrary(): Gen[(a, b, c)] =
            Applicative.map3((x, y, z) -> (x, y, z), Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }

    instance Arbitrary[(a, b, c, d)] with Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d] {
        pub def arbitrary(): Gen[(a, b, c, d)] =
            Applicative.map4((w, x, y, z) -> (w, x, y, z), Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }

    instance Arbitrary[List[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[List[a]] =
            sized(size ->
                chooseInt(0, size) >>= vector
            )
    }

    instance Arbitrary[Nel[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Nel[a]] =
            sized(size ->
                Applicative.map2((x, xs) -> Nel.Nel(x, xs), Arbitrary.arbitrary(), vector(size - 1))
            )
    }

    instance Arbitrary[Chain[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Chain[a]] =
            sized(size ->
                Functor.map(List.toChain, vector(size))
            )
    }

    instance Arbitrary[Nec[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Nec[a]] =
            let func = (x, xs) -> 
                match List.toNec(xs) {
                    case None => Nec.singleton(x),
                    case Some(tail) => Nec.append(Nec.singleton(x), tail)
            };
            sized(size -> Applicative.map2(func, Arbitrary.arbitrary(), vector(size - 1)))
    }

    instance Arbitrary[Vector[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Vector[a]] =
            sized(size -> 
                Functor.map(List.toVector, vector(size))
            )
    }

    instance Arbitrary[Option[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Option[a]] = 
            let func = (bool, arb) ->
                match bool {
                    case true => Some(arb),
                    case false => None
                };
            Applicative.map2(func, chooseBool(), Arbitrary.arbitrary())
    }

    instance Arbitrary[Result[e, t]] with Arbitrary[e], Arbitrary[t] {
        pub def arbitrary(): Gen[Result[e, t]] =
            let func = (bool, eGen, tGen) ->
                match bool {
                    case true => Ok(tGen),
                    case false => Err(eGen)
                };
            Applicative.map3(func, chooseBool(), Arbitrary.arbitrary(), Arbitrary.arbitrary())
    }
}