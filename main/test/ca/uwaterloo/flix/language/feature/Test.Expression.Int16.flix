/*
 * Note that there is a specific bytecode instruction (SIPUSH) for pushing shorts (that aren't handled by BIPUSH).
 */

@test
def testInt16_01(): Int16 = -5320i16

@test
def testInt16_02(): Int16 = 4568i16

@test
def testInt16_03(): Int16 = -129i16

@test
def testInt16_04(): Int16 = 128i16

@test
def testInt16_05(): Int16 = 32767i16

@test
def testInt16_06(): Int16 = -32768i16
