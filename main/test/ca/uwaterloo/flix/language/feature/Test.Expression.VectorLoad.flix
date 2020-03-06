@test
def testVectorLoad01(): Int & Impure = let x = [|1|]; x[|0|]

@test
def testVectorLoad02(): Int & Impure = let x = [|1; 1|]; x[|0|]

@test
def testVectorLoad03(): Int & Impure = let x = [|1; 2|]; x[|1|]

@test
def testVectorLoad04(): Int & Impure = let x = [|1; 3|]; x[|2|]

@test
def testVectorLoad05(): Int & Impure = let x = [|1; 4|]; x[|3|]

@test
def testVectorLoad06(): Int & Impure = let x = [|1; 20|]; x[|19|]

@test
def testVectorLoad07(): Bool & Impure = let x = [|true|]; x[|0|]

@test
def testVectorLoad08(): Bool & Impure = let x = [|false; 1|]; !x[|0|]

@test
def testVectorLoad09(): Bool & Impure = let x = [|true; 2|]; x[|1|]

@test
def testVectorLoad10(): Bool & Impure = let x = [|false; 3|]; !x[|2|]

@test
def testVectorLoad11(): Bool & Impure = let x = [|true; 5|]; x[|3|]

@test
def testVectorLoad12(): Char & Impure = let x = [|'c'; 1|]; x[|0|]

@test
def testVectorLoad13(): Char & Impure = let x = [|'a'; 2|]; x[|1|]

@test
def testVectorLoad14(): Char & Impure = let x = [|'c'; 3|]; x[|2|]

@test
def testVectorLoad15(): Char & Impure = let x = [|'a'; 20|]; x[|19|]

@test
def testVectorLoad16(): Int8 & Impure = let x = [|1i8; 1|]; x[|0|]

@test
def testVectorLoad17(): Int8 & Impure = let x = [|1i8; 3|]; x [|2|]

@test
def testVectorLoad18(): Int8 & Impure = let x = [|4i8; 4|]; x[|3|]

@test
def testVectorLoad19(): Int16 & Impure = let x = [|1i16; 1|]; x[|0|]

@test
def testVectorLoad20(): Int16 & Impure = let x = [|3i16; 3|]; x [|1|]

@test
def testVectorLoad21(): Int16 & Impure = let x = [|4i16; 4|]; x[|2|]

@test
def testVectorLoad22(): Int64 & Impure = let x = [|1i64; 1|]; x[|0|]

@test
def testVectorLoad23(): Int64 & Impure = let x = [|3i64; 3|]; x[|1|]

@test
def testVectorLoad24(): Int64 & Impure = let x = [|4i64; 4|]; x[|3|]

@test
def testVectorLoad25(): Float32 & Impure = let x = [|1.0f32; 1|]; x[|0|]

@test
def testVectorLoad26(): Float32 & Impure = let x = [|3.0f32; 3|]; x[|2|]

@test
def testVectorLoad27(): Float32 & Impure = let x = [|3.0f32; 4|]; x[|0|]

@test
def testVectorLoad28(): Float64 & Impure = let x = [|1.0f64; 1|]; x[|0|]

@test
def testVectorLoad29(): Float64 & Impure = let x = [|3.0f64; 3|]; x[|2|]

@test
def testVectorLoad30(): Float64 & Impure = let x = [|4.0f64; 4|]; x[|3|]

@test
def testVectorLoad31(): Str & Impure = let x = [|"Hello"; 1|]; x[|0|]

@test
def testVectorLoad32(): Str & Impure = let x = [|"World"; 2|]; x[|1|]

@test
def testVectorLoad33(): Str & Impure = let x = [|"World"; 3|]; x[|2|]

@test
def testVectorLoad34(): Vector[Int, 3] & Impure = let x = [|[|1, 2, 3|]; 1|]; x[|0|]

@test
def testVectorLoad35(): Vector[Int, 2] & Impure = let x = [|[|1, 2|]; 2|]; x[|1|]

@test
def testVectorLoad36(): Vector[Int, 3] & Impure = let x = [|[|1, 2, 3|]; 3|]; x[|2|]

@test
def testVectorLoad37(): Vector[Int, 2] & Impure = let x = [|[|2 ; 2|]; 2|]; x[|1|]

@test
def testVectorLoad38(): List[Int] & Impure = let x = [|(1 :: Nil), (2 :: Nil)|]; x[|0|]

@test
def testVectorLoad39(): Int & Impure = let x = [|[1, 2], [3, 4]|]; x[|0|][0]

// TODO
//@test
//def testVectorLoad40(): (Int, Int) & Impure = let x = [|(1, 2), (3, 4)|]; x[|0|]

@test
def testVectorLoad41(): Option[Int] & Impure = let x = [|[Some(1), Some(2), Some(3)], [Some(4), Some(5), None, None]|]; x[|1|][2]

