//@test
//def testVectorLit01(): [|Unit|] = [| |]

@test
def testVectorLit01(): [|Int, 1|] = [|1|]

@test
def testVectorLit02(): [|Int, 2|] = [|1, 2|]

@test
def testVectorLit03(): [|Bool, 3|] = [|true, true, false|]

@test
def testVectorLit04(): [|Str, 4|] = [|"One", "two", "three", "four"|]

@test
def testVectorLit05(): [|Char, 5|] = [|'a', 'b', 'c', 'd', 'e'|]

@test
def testVectorLit06(): [|[|Int, 1|], 1|] = [|[|1|]|]

@test
def testVectorLit07(): [|[|Int, 2|], 1|] = [|[|1, 2|]|]

//@test
//def testVectorLit08(): [|[|Int, Int|]|] = [|[|1, 2|][|1, 2|]|]