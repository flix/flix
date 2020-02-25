// TODO: More tests when we get the syntax for exponents. More tests when we have standard library (NaN, +/infinity).
// See JLS 3.10.2:
//   The largest positive finite literal of type float is 3.4028235e38f.
//   The smallest positive finite non-zero literal of type float is 1.40e-45f.
//   The largest positive finite literal of type double is 1.7976931348623157e308.
//   The smallest positive finite non-zero literal of type double is 4.9e-324.

@test
def testFloat01(): Float = 0.0

@test
def testFloat02(): Float = -0.0

@test
def testFloat03(): Float = 4.2

@test
def testFloat04(): Float = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

@test
def testFloat05(): Float = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

@test
def testFloat06(): Float = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0

@test
def testFloat07(): Float = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001

@test
def testFloat08(): Float = 1_0.0_0

@test
def testFloat09(): Float = 1_000.123_456_789

@test
def testFloat10(): Float = 42_000.0

@test
def testFloat11(): Float = 1_000_000_000_000_000_000_000_000_000.0

@test
def testFloat12(): Float = 0.000_000_000_000_000_000_000_000_000_1