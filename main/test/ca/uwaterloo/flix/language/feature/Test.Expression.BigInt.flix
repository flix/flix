@test
def testBigInt01(): BigInt = 0ii

@test
def testBigInt02(): BigInt = 1ii

@test
def testBigInt03(): BigInt = 10ii

// One larger than Long.MaxValue
@test
def testBigInt04(): BigInt = 9223372036854775808ii

// One smaller than Long.MinValue
@test
def testBigInt05(): BigInt = -9223372036854775809ii
