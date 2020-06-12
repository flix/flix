/*
 * Note that there is a specific bytecode instruction (BIPUSH) for pushing bytes
 * (that aren't handled by the -1 to 5 constant instructions).
 */

@test
def testInt8_01(): Int8 = -105i8

@test
def testInt8_02(): Int8 = 121i8

@test
def testInt8_03(): Int8 = -2i8

@test
def testInt8_04(): Int8 = 6i8

@test
def testInt8_05(): Int8 = 127i8

@test
def testInt8_06(): Int8 = -128i8

@test
def testInt8_07(): Int8 = 1_0i8

@test
def testInt8_08(): Int8 = 1_00i8

@test
def testInt8_09(): Int8 = 0x1Ai8

@test
def testInt8_10(): Int8 = 0x1_Ai8

@test
def testInt8_11(): Int8 = 0x0i8

@test
def testInt8_12(): Int8 = 0x1i8

@test
def testInt8_13(): Int8 = 0x1ai8

@test
def testInt8_14(): Int8 = 0x1_ai8
