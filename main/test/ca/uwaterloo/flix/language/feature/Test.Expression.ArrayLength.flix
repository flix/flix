@test
def testArrayLength01(): Int = let x = []; length[x]

@test
def testArrayLength02(): Int = let x = [1]; length[x]

@test
def testArrayLength03(): Int = let x = [1, 2, 3]; length[x]

@test
def testArrayLength04(): Int = let x = [1, 2, 3, 4]; length[x]

@test
def testArrayLength05(): Int = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; length[x]

@test
def testArrayLength06(): Int = let x = [1, 2]; length[x]

@test
def testArrayLength07(): Int = let x = [true]; length[x]

@test
def testArrayLength08(): Int = let x = [false]; length[x]

@test
def testArrayLength09(): Int = let x = [true, false]; length[x]

@test
def testArrayLength10(): Int = let x = [false, true, false]; length[x]

@test
def testArrayLength11(): Int = let x = [false, false, false, false, true]; length[x]

@test
def testArrayLength12(): Int = let x = ['c']; length[x]

@test
def testArrayLength13(): Int = let x = ['a', 'b']; length[x]

@test
def testArrayLength14(): Int = let x = ['a', 'b', 'c']; length[x]

@test
def testArrayLength15(): Int = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; length[x]

@test
def testArrayLength16(): Int = let x = [1i8]; length[x]

@test
def testArrayLength17(): Int = let x = [1i8, 2i8, 3i8]; length[x]

@test
def testArrayLength18(): Int = let x = [1i8, 2i8, 3i8, 4i8]; length[x]

@test
def testArrayLength19(): Int = let x = [1i16]; length[x]

@test
def testArrayLength20(): Int = let x = [1i16, 2i16, 3i16]; length[x]

@test
def testArrayLength21(): Int = let x = [1i16, 2i16, 3i16, 4i16]; length[x]

@test
def testArrayLength22(): Int = let x = [1i64]; length[x]

@test
def testArrayLength23(): Int = let x = [1i64, 2i64, 3i64]; length[x]

@test
def testArrayLength24(): Int = let x = [1i64, 2i64, 3i64, 4i64]; length[x]

@test
def testArrayLength25(): Int = let x = [1.0f32]; length[x]

@test
def testArrayLength26(): Int = let x = [1.0f32, 2.0f32, 3.0f32]; length[x]

@test
def testArrayLength27(): Int = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; length[x]

@test
def testArrayLength28(): Int = let x = [1.0f64]; length[x]

@test
def testArrayLength29(): Int = let x = [1.0f64, 2.0f64, 3.0f64]; length[x]

@test
def testArrayLength30(): Int = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; length[x]

@test
def testArrayLength31(): Int = let x = ["Hello"]; length[x]

@test
def testArrayLength32(): Int = let x = ["Hello", "World"]; length[x]

@test
def testArrayLength33(): Int = let x = ["World", "Hello", "World"]; length[x]

@test
def testArrayLength34(): Int = let x = [[1, 2, 3]]; length[x]

@test
def testArrayLength35(): Int = let x = [[1, 2], [1, 2]]; length[x]

@test
def testArrayLength36(): Int = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; length[x]

@test
def testArrayLength37(): Int = let x = [[1, 2, 3, 4, 5], [2]]; length[x]

@test
def testArrayLength38(): Int = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; length[x]

@test
def testArrayLength39(): Int = let x = [[|1, 2|], [|3, 4|]]; length[x]

@test
def testArrayLength40(): Int = let x = [[None, Some(1), None], [Some(2), Some(45), Some(9), None]]; length[x]

@test
def testArrayLength41(): Int = let x = [(1, 2), (3, 4)]; length[x]

def testArrayLength42(): Int = let x = [[1 :: Nil], [3 :: 4 :: 5 :: Nil]]; length[x]

