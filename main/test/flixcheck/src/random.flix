mod Prng {
    type alias Prng = {value = Int64, genState = Int64}

    pub def xor(a: Int64, b: Int64): Int64 = 
        Int64.bitwiseXor(a, b)

    pub def shiftL(a: Int64, b: Int32): Int64 = 
        Int64.leftShift(a, b)
    
    pub def shiftR(a: Int64, b: Int32): Int64 = 
        Int64.rightShift(a, b)

    pub def init(seed: Int64): Prng = 
        let s0 = 1812433253i64 * (seed `xor` (seed `shiftR` 30)) + 1i64;
        let s1 = 1812433253i64 * (s0 `xor` (s0 `shiftR` 30)) + 2i64;
        {value = s0, genState = s1}

    pub def step(r: Prng): Prng =
        let s0 = r.value;
        let s1 = r.genState;
        let newState = s0 `xor` (s0 `shiftL` 23);
        let newValue = newState `xor` s1 `xor` (newState `shiftR` 17) `xor` (s1 `shiftR` 26);
        {value = newValue, genState = newState}


    pub def testmain(): Unit \ IO =
        let g = init(3748374974327i64);
        let g1 = step(g);
        let g2 = step(g1);
        let g3 = step(g2);
        let g4 = step(g3);
        println(Int64.toFloat64(g.value)/Int64.toFloat64(Int64.maxValue()));
        println(Int64.toFloat64(g1.value)/Int64.toFloat64(Int64.maxValue()));
        println(Int64.toFloat64(g2.value)/Int64.toFloat64(Int64.maxValue()));
        println(Int64.toFloat64(g3.value)/Int64.toFloat64(Int64.maxValue()));
        println(Int64.toFloat64(g4.value)/Int64.toFloat64(Int64.maxValue()))
}



