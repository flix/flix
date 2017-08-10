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