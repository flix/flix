@test
def testSpawn01(): Unit = spawn ()

@test
def testSpawn02(): Unit = spawn true

@test
def testSpawn03(): Unit = spawn 'c'

@test
def testSpawn04(): Unit = spawn 4.2

@test
def testSpawn05(): Unit = spawn 42

@test
def testSpawn06(): Unit = spawn 42ii

@test
def testSpawn07(): Unit = spawn "42"