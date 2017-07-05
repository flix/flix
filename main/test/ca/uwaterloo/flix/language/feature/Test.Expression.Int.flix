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
