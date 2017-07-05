@test
def testBigInt_01(): BigInt = 0ii

@test
def testBigInt_02(): BigInt = 1ii

@test
def testBigInt_03(): BigInt = 10ii

// One larger than Long.MaxValue
@test
def testBigInt_04(): BigInt = 9223372036854775808ii

// One smaller than Long.MinValue
@test
def testBigInt_05(): BigInt = -9223372036854775809ii
