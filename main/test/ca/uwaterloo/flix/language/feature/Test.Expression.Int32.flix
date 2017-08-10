/*
 * Larger int constants need to be loaded with LDC.
 */

@test
def testInt32_01(): Int32 = -254542i32

@test
def testInt32_02(): Int32 = 45649878i32

@test
def testInt32_03(): Int32 = -32769i32

@test
def testInt32_04(): Int32 = 32768i32

@test
def testInt32_05(): Int32 = 2147483647i32

@test
def testInt32_06(): Int32 = -2147483648i32