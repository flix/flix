/*
 * Note that there are specific bytecode instructions for the constants -1 to 5, inclusive.
 */

@test
def testInt01(): Int = 0

@test
def testInt02(): Int = -1

@test
def testInt03(): Int = 1

@test
def testInt04(): Int = 5

@test
def testInt05(): Int = -254542

@test
def testInt06(): Int = 45649878

@test
def testInt07(): Int = 2147483647

@test
def testInt08(): Int = -2147483648

@test
def testInt09(): Int = 1_0

@test
def testInt10(): Int = 1_000

@test
def testInt11(): Int = 42_000

@test
def testInt12(): Int = 1_000_000

@test
def testInt13(): Int = 42_000_000

@test
def testInt14(): Int = 1_000_000_000

@test
def testInt15(): Int = 0x1ABCDEF9

@test
def testInt16(): Int = 0x1A_BC_DE_F9

@test
def testInt17(): Int = 0x0

@test
def testInt18(): Int = 0x1

@test
def testInt19(): Int = 0x1abcdef9

@test
def testInt20(): Int = 0x1a_bc_de_f9
