//@test
//def testVectorLit01(): [|Unit|] = [| |]

@test
def testVectorLit01(): [|Int|] = [|1|]

@test
def testVectorLit02(): [|Int, Int|] = [|1, 2|]

@test
def testVectorLit03(): [|Bool, Bool, Bool|] = [|true, true, false|]

@test
def testVectorLit04(): [|Str, Str, Str, Str|] = [|"One", "two", "three", "four"|]

@test
def testVectorLit05(): [|Char, Char, Char, Char, Char|] = [|'a', 'b', 'c', 'd', 'e'|]

@test
def testVectorLit06(): [|[|Int|]|] = [|[|1|]|]

@test
def testVectorLit07(): [|[|Int, Int|]|] = [|[|1, 2|]|]

@test
def testVectorLit08(): [|[|Int, Int|][|Int, Int|]|] = [|[|1, 2|][|1, 2|]|]