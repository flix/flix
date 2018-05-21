@test
def testVectorLength01(): Int = let x = [||]; length[|x|]

@test
def testVectorLength02(): Int = let x = [|1|]; length[|x|]

@test
def testVectorLength03(): Int = let x = [|1, 2, 3|]; length[|x|]

@test
def testVectorLength04(): Int = let x = [|1, 2, 3, 4|]; length[|x|]

@test
def testVectorLength05(): Int = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20|]; length[|x|]

@test
def testVectorLength06(): Int = let x = [|1, 2|]; length[|x|]

@test
def testVectorLength07(): Int = let x = [|true|]; length[|x|]

@test
def testVectorLength08(): Int = let x = [|false|]; length[|x|]

@test
def testVectorLength09(): Int = let x = [|true, false|]; length[|x|]

@test
def testVectorLength10(): Int = let x = [|false, true, false|]; length[|x|]

@test
def testVectorLength11(): Int = let x = [|false, false, false, false, true|]; length[|x|]

@test
def testVectorLength12(): Int = let x = [|'c'|]; length[|x|]

@test
def testVectorLength13(): Int = let x = [|'a', 'b'|]; length[|x|]

@test
def testVectorLength14(): Int = let x = [|'a', 'b', 'c'|]; length[|x|]

@test
def testVectorLength15(): Int = let x = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]; length[|x|]

@test
def testVectorLength16(): Int = let x = [|1i8|]; length[|x|]

@test
def testVectorLength17(): Int = let x = [|1i8, 2i8, 3i8|]; length[|x|]

@test
def testVectorLength18(): Int = let x = [|1i8, 2i8, 3i8, 4i8|]; length[|x|]

@test
def testVectorLength19(): Int = let x = [|1i16|]; length[|x|]

@test
def testVectorLength20(): Int = let x = [|1i16, 2i16, 3i16|]; length[|x|]

@test
def testVectorLength21(): Int = let x = [|1i16, 2i16, 3i16, 4i16|]; length[|x|]

@test
def testVectorLength22(): Int = let x = [|1i64|]; length[|x|]

@test
def testVectorLength23(): Int = let x = [|1i64, 2i64, 3i64|]; length[|x|]

@test
def testVectorLength24(): Int = let x = [|1i64, 2i64, 3i64, 4i64|]; length[|x|]

@test
def testVectorLength25(): Int = let x = [|1.0f32|]; length[|x|]

@test
def testVectorLength26(): Int = let x = [|1.0f32, 2.0f32, 3.0f32|]; length[|x|]

@test
def testVectorLength27(): Int = let x = [|1.0f32, 2.0f32, 3.0f32, 4.0f32|]; length[|x|]

@test
def testVectorLength28(): Int = let x = [|1.0f64|]; length[|x|]

@test
def testVectorLength29(): Int = let x = [|1.0f64, 2.0f64, 3.0f64|]; length[|x|]

@test
def testVectorLength30(): Int = let x = [|1.0f64, 2.0f64, 3.0f64, 4.0f64|]; length[|x|]

@test
def testVectorLength31(): Int = let x = [|"Hello"|]; length[|x|]

@test
def testVectorLength32(): Int = let x = [|"Hello", "World"|]; length[|x|]

@test
def testVectorLength33(): Int = let x = [|"World", "Hello", "World"|]; length[|x|]

@test
def testVectorLength34(): Int = let x = [|[|1, 2, 3|]|]; length[|x|]

@test
def testVectorLength35(): Int = let x = [|[|1, 2|], [|1, 2|]|]; length[|x|]

@test
def testVectorLength36(): Int = let x = [|[|1, 2, 3, 4, 5, 6, 7, 8|], [|1, 2, 3, 4, 5, 6, 7, 8|]|]; length[|x|]

@test
def testVectorLength37(): Int = let x = [|[|1, 2, 3, 4, 5|], [|2, 2, 2, 2, 2|]|]; length[|x|]

@test
def testVectorLength38(): Int = let x = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]; length[|x|]

@test
def testVectorLength39(): Int = let x = [|(1, 2), (3, 4)|]; length[|x|]

@test
def testVectorLength40(): Int = let x = [|[1, 2], [3, 4]|]; length[|x|]

@test
def testVectorLength41(): Int = let x = [|[1 :: Nil], [3 :: 4 :: 5 :: Nil]|]; length[|x|]

@test
def testVectorLength42(): Int = let x = [|Some(1), Some(2), Some(3)|]; length[|x|]
