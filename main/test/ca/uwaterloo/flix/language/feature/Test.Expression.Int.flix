/*
 * Note that there are specific bytecode instructions for the constants -1 to 5, inclusive.
 */

@test
def testInt_01(): Int = 0

@test
def testInt_02(): Int = -1

@test
def testInt_03(): Int = 1

@test
def testInt_04(): Int = 5

@test
def testInt_05(): Int = -254542

@test
def testInt_06(): Int = 45649878

@test
def testInt_07(): Int = 2147483647

@test
def testInt_08(): Int = -2147483648
