/*
 * Note that there are specific bytecode instructions for the constants 0l and 1l.
 */

@test
def testInt64_01(): Int64 = -254454121542i64

@test
def testInt64_02(): Int64 = 45641198784545i64

@test
def testInt64_03(): Int64 = -2147483649i64

@test
def testInt64_04(): Int64 = 2147483648i64

@test
def testInt64_05(): Int64 = 9223372036854775807i64

@test
def testInt64_06(): Int64 = -9223372036854775808i64

@test
def testInt64_07(): Int64 = 0i64

@test
def testInt64_08(): Int64 = 1i64

@test
def testInt64_09(): Int64 = 1_0i64

@test
def testInt64_10(): Int64 = 1_000i64

@test
def testInt64_11(): Int64 = 42_000i64

@test
def testInt64_12(): Int64 = 1_000_000i64

@test
def testInt64_13(): Int64 = 42_000_000i64

@test
def testInt64_14(): Int64 = 1_000_000_000i64

@test
def testInt64_15(): Int64 = 42_000_000_000i64

@test
def testInt64_16(): Int64 = 1_000_000_000_000_000i64

@test
def testInt64_17(): Int64 = 42_000_000_000_000_000i64

@test
def testInt64_18(): Int64 = 1_000_000_000_000_000_000i64
